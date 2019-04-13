open Protocols.Coda_pow
open Coda_base
open Core
open Async
open Cache_lib

module Make (Inputs : Inputs.With_unprocessed_transition_cache.S) :
  Breadcrumb_builder_intf
  with type state_hash := State_hash.t
  with type external_transition_verified :=
              Inputs.External_transition.Verified.t
  with type transition_frontier := Inputs.Transition_frontier.t
  with type transition_frontier_breadcrumb :=
              Inputs.Transition_frontier.Breadcrumb.t = struct
  open Inputs

  let build_subtrees_of_breadcrumbs ~logger ~frontier ~initial_hash
      subtrees_of_transitions =
    let breadcrumb_if_present () =
      match Transition_frontier.find frontier initial_hash with
      | None ->
          let msg =
            Printf.sprintf
              !"Transition frontier garbage already collected the parent on \
                %{sexp: Coda_base.State_hash.t}"
              initial_hash
          in
          Logger.error logger ~module_:__MODULE__ ~location:__LOC__ !"%s" msg ;
          Or_error.error_string msg
      | Some breadcrumb -> Or_error.return breadcrumb
    in
    Deferred.Or_error.List.map subtrees_of_transitions
      ~f:(fun subtree_of_transitions ->
        let open Deferred.Or_error.Let_syntax in
        let%map subtree_of_constructions =
          Rose_tree.Deferred.Or_error.fold_map subtree_of_transitions
            ~init:(Cached.pure `Initial)
            ~f:(fun cached_parent_or_initial cached_transition ->
              let open Deferred.Let_syntax in
              let%map cached_result =
                Cached.transform cached_transition ~f:(fun transition ->
                    let open Deferred.Or_error.Let_syntax in
                    let parent_or_initial =
                      Cached.peek cached_parent_or_initial
                    in
                    let%bind well_formed_parent =
                      match parent_or_initial with
                      | `Initial -> breadcrumb_if_present () |> Deferred.return
                      | `Constructed parent -> Deferred.Or_error.return parent
                    in
                    let expected_parent_hash =
                      Transition_frontier.Breadcrumb.transition_with_hash
                        well_formed_parent
                      |> With_hash.hash
                    in
                    let actual_parent_hash =
                      transition |> With_hash.data
                      |> External_transition.Verified.protocol_state
                      |> External_transition.Protocol_state.previous_state_hash
                    in
                    let%bind () =
                      Deferred.return
                        (Result.ok_if_true
                           (State_hash.equal actual_parent_hash
                              expected_parent_hash)
                           ~error:
                             (Error.of_string
                                "Previous external transition hash does not \
                                 equal to current external transition's \
                                 parent hash"))
                    in
                    let open Deferred.Let_syntax in
                    match%map
                      Transition_frontier.Breadcrumb.build ~logger
                        ~parent:well_formed_parent
                        ~transition_with_hash:transition
                    with
                    | Ok new_breadcrumb ->
                        let open Result.Let_syntax in
                        let%map _ : Transition_frontier.Breadcrumb.t =
                          breadcrumb_if_present ()
                        in
                        `Constructed new_breadcrumb
                    | Error (`Fatal_error exn) -> Or_error.of_exn exn
                    | Error (`Validation_error error) -> Error error )
                |> Cached.sequence_deferred
              in
              Cached.sequence_result cached_result )
        in
        Rose_tree.map subtree_of_constructions ~f:(fun construction ->
            Cached.transform construction ~f:(function
              | `Initial -> failwith "impossible"
              | `Constructed breadcrumb -> breadcrumb ) ) )
end
