open Mina_base
open Core
open Async
open Cache_lib
open Mina_transition
open Network_peer

let build_subtrees_of_breadcrumbs ~logger ~precomputed_values ~verifier
    ~trust_system ~frontier ~initial_hash subtrees_of_enveloped_transitions =
  let missing_parent_msg =
    Printf.sprintf
      "Transition frontier already garbage-collected the parent of %s"
      (Mina_base.State_hash.to_base58_check initial_hash)
  in
  (* If the breadcrumb we are targeting is removed from the transition
   * frontier while we're catching up, it means this path is not on the
   * critical path that has been chosen in the frontier. As such, we should
   * drop it on the floor. *)
  let breadcrumb_if_present logger =
    match Transition_frontier.find frontier initial_hash with
    | None ->
        [%log error]
          ~metadata:
            [ ("state_hash", Mina_base.State_hash.to_yojson initial_hash)
            ; ( "transition_hashes"
              , `List
                  (List.map subtrees_of_enveloped_transitions
                     ~f:(fun subtree ->
                       Rose_tree.to_yojson
                         (fun enveloped_transitions ->
                           Cached.peek enveloped_transitions
                           |> Envelope.Incoming.data
                           |> External_transition.Initial_validated.state_hash
                           |> Mina_base.State_hash.to_yojson )
                         subtree )) ) ]
          "Transition frontier already garbage-collected the parent of \
           $state_hash" ;
        Or_error.error_string missing_parent_msg
    | Some breadcrumb ->
        Or_error.return breadcrumb
  in
  Deferred.Or_error.List.map subtrees_of_enveloped_transitions
    ~f:(fun subtree_of_enveloped_transitions ->
      let open Deferred.Or_error.Let_syntax in
      let%bind init_breadcrumb =
        breadcrumb_if_present
          (Logger.extend logger
             [("Check", `String "Before creating breadcrumb")])
        |> Deferred.return
      in
      Rose_tree.Deferred.Or_error.fold_map_over_subtrees
        subtree_of_enveloped_transitions ~init:(Cached.pure init_breadcrumb)
        ~f:(fun cached_parent
           (Rose_tree.T (cached_enveloped_transition, _) as subtree)
           ->
          let open Deferred.Let_syntax in
          let%map cached_result =
            Cached.transform cached_enveloped_transition
              ~f:(fun enveloped_transition ->
                let open Deferred.Or_error.Let_syntax in
                let transition_with_initial_validation =
                  Envelope.Incoming.data enveloped_transition
                in
                let transition_receipt_time = Some (Time.now ()) in
                let transition_with_hash, _ =
                  transition_with_initial_validation
                in
                let mostly_validated_transition =
                  (* TODO: handle this edge case more gracefully *)
                  (* since we are building a disconnected subtree of breadcrumbs,
                   * we skip this step in validation *)
                  External_transition.skip_frontier_dependencies_validation
                    `This_transition_belongs_to_a_detached_subtree
                    transition_with_initial_validation
                in
                let sender = Envelope.Incoming.sender enveloped_transition in
                let parent = Cached.peek cached_parent in
                let expected_parent_hash =
                  Transition_frontier.Breadcrumb.state_hash parent
                in
                let actual_parent_hash =
                  transition_with_hash |> With_hash.data
                  |> External_transition.parent_hash
                in
                let%bind () =
                  Deferred.return
                    (Result.ok_if_true
                       (State_hash.equal actual_parent_hash
                          expected_parent_hash)
                       ~error:
                         (Error.of_string
                            "Previous external transition hash does not equal \
                             to current external transition's parent hash"))
                in
                let open Deferred.Let_syntax in
                match%bind
                  O1trace.trace_recurring "Breadcrumb.build" (fun () ->
                      Deferred.Or_error.try_with ~here:[%here] (fun () ->
                          Transition_frontier.Breadcrumb.build ~logger
                            ~precomputed_values ~verifier ~trust_system ~parent
                            ~transition:mostly_validated_transition
                            ~sender:(Some sender) ~transition_receipt_time ()
                      ) )
                with
                | Error _ ->
                    Deferred.return @@ Or_error.error_string missing_parent_msg
                | Ok result -> (
                  match result with
                  | Ok new_breadcrumb ->
                      let open Result.Let_syntax in
                      Mina_metrics.(
                        Counter.inc_one
                          Transition_frontier_controller
                          .breadcrumbs_built_by_builder) ;
                      Deferred.return
                        (let%map (_ : Transition_frontier.Breadcrumb.t) =
                           breadcrumb_if_present
                             (Logger.extend logger
                                [("Check", `String "After creating breadcrumb")])
                         in
                         new_breadcrumb)
                  | Error err -> (
                      (* propagate bans through subtree *)
                      let subtree_nodes = Rose_tree.flatten subtree in
                      let ip_address_set =
                        let sender_from_tree_node node =
                          Envelope.Incoming.sender (Cached.peek node)
                        in
                        List.fold subtree_nodes
                          ~init:(Set.empty (module Network_peer.Peer))
                          ~f:(fun inet_addrs node ->
                            match sender_from_tree_node node with
                            | Local ->
                                failwith
                                  "build_subtrees_of_breadcrumbs: sender of \
                                   external transition should not be Local"
                            | Remote peer ->
                                Set.add inet_addrs peer )
                      in
                      let ip_addresses = Set.to_list ip_address_set in
                      let trust_system_record_invalid msg error =
                        let%map () =
                          Deferred.List.iter ip_addresses ~f:(fun ip_addr ->
                              Trust_system.record trust_system logger ip_addr
                                ( Trust_system.Actions
                                  .Gossiped_invalid_transition
                                , Some (msg, []) ) )
                        in
                        Error error
                      in
                      match err with
                      | `Invalid_staged_ledger_hash error ->
                          trust_system_record_invalid
                            "invalid staged ledger hash" error
                      | `Invalid_staged_ledger_diff error ->
                          trust_system_record_invalid
                            "invalid staged ledger diff" error
                      | `Fatal_error exn ->
                          Deferred.return (Or_error.of_exn exn) ) ) )
            |> Cached.sequence_deferred
          in
          Cached.sequence_result cached_result ) )
