(* Only show stdout for failed inline tests. *)
open Inline_test_quiet_logs
open Core
open Async
open Cache_lib
open Pipe_lib
open Mina_base
open Mina_transition
open Network_peer

(** [Ledger_catchup] is a procedure that connects a foreign external transition
    into a transition frontier by requesting a path of external_transitions
    from its peer. It receives the state_hash to catchup from
    [Catchup_scheduler]. With that state_hash, it will ask its peers for
    a merkle path/list from their oldest transition to the state_hash it is
    asking for. Upon receiving the merkle path/list, it will do the following:

    1. verify the merkle path/list is correct by calling
    [Transition_chain_verifier.verify]. This function would returns a list
    of state hashes if the verification is successful.

    2. using the list of state hashes to poke a transition frontier
    in order to find the hashes of missing transitions. If none of the hashes
    are found, then it means some more transitions are missing.

    Once the list of missing hashes are computed, it would do another request to
    download the corresponding transitions in a batch fashion. Next it will perform the
    following validations on each external_transition:

    1. Check the list of transitions corresponds to the list of hashes that we
    requested;

    2. Each transition is checked through [Transition_processor.Validator] and
    [Protocol_state_validator]

    If any of the external_transitions is invalid,
    1) the sender is punished;
    2) those external_transitions that already passed validation would be
       invalidated.
    Otherwise, [Ledger_catchup] will build a corresponding breadcrumb path from
    the path of external_transitions. A breadcrumb from the path is built using
    its corresponding external_transition staged_ledger_diff and applying it to
    its preceding breadcrumb staged_ledger to obtain its corresponding
    staged_ledger. If there was an error in building the breadcrumbs, then
    catchup would invalidate the cached transitions.
    After building the breadcrumb path, [Ledger_catchup] will then send it to
    the [Processor] via writing them to catchup_breadcrumbs_writer. *)

let verify_transition ~logger ~consensus_constants ~trust_system ~frontier
    ~unprocessed_transition_cache enveloped_transition =
  let sender = Envelope.Incoming.sender enveloped_transition in
  let genesis_state_hash = Transition_frontier.genesis_state_hash frontier in
  let transition_with_hash = Envelope.Incoming.data enveloped_transition in
  let cached_initially_validated_transition_result =
    let open Result.Let_syntax in
    let%bind initially_validated_transition =
      transition_with_hash
      |> External_transition.skip_time_received_validation
           `This_transition_was_not_received_via_gossip
      |> External_transition.validate_genesis_protocol_state
           ~genesis_state_hash
      >>= External_transition.validate_protocol_versions
      >>= External_transition.validate_delta_transition_chain
    in
    let enveloped_initially_validated_transition =
      Envelope.Incoming.map enveloped_transition
        ~f:(Fn.const initially_validated_transition)
    in
    Transition_handler.Validator.validate_transition ~logger ~frontier
      ~consensus_constants ~unprocessed_transition_cache
      enveloped_initially_validated_transition
  in
  let open Deferred.Let_syntax in
  match cached_initially_validated_transition_result with
  | Ok x ->
      Deferred.return @@ Ok (`Building_path x)
  | Error (`In_frontier hash) ->
      [%log trace]
        "transition queried during ledger catchup has already been seen" ;
      Deferred.return @@ Ok (`In_frontier hash)
  | Error (`In_process consumed_state) -> (
      [%log trace]
        "transition queried during ledger catchup is still in process in one \
         of the components in transition_frontier" ;
      match%map Ivar.read consumed_state with
      | `Failed ->
          [%log trace] "transition queried during ledger catchup failed" ;
          Error (Error.of_string "Previous transition failed")
      | `Success hash ->
          Ok (`In_frontier hash) )
  | Error (`Verifier_error error) ->
      [%log warn]
        ~metadata:[("error", Error_json.error_to_yojson error)]
        "verifier threw an error while verifying transiton queried during \
         ledger catchup: $error" ;
      Deferred.Or_error.fail (Error.tag ~tag:"verifier threw an error" error)
  | Error `Invalid_proof ->
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Gossiped_invalid_transition
          , Some ("invalid proof", []) )
      in
      Error (Error.of_string "invalid proof")
  | Error `Invalid_genesis_protocol_state ->
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Gossiped_invalid_transition
          , Some ("invalid genesis protocol state", []) )
      in
      Error (Error.of_string "invalid genesis protocol state")
  | Error `Invalid_delta_transition_chain_proof ->
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Gossiped_invalid_transition
          , Some ("invalid delta transition chain witness", []) )
      in
      Error (Error.of_string "invalid delta transition chain witness")
  | Error `Invalid_protocol_version ->
      let transition =
        External_transition.Validation.forget_validation transition_with_hash
      in
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Sent_invalid_protocol_version
          , Some
              ( "Invalid current or proposed protocol version in catchup block"
              , [ ( "current_protocol_version"
                  , `String
                      ( External_transition.current_protocol_version transition
                      |> Protocol_version.to_string ) )
                ; ( "proposed_protocol_version"
                  , `String
                      ( External_transition.proposed_protocol_version_opt
                          transition
                      |> Option.value_map ~default:"<None>"
                           ~f:Protocol_version.to_string ) ) ] ) )
      in
      Error (Error.of_string "invalid protocol version")
  | Error `Mismatched_protocol_version ->
      let transition =
        External_transition.Validation.forget_validation transition_with_hash
      in
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Sent_mismatched_protocol_version
          , Some
              ( "Current protocol version in catchup block does not match \
                 daemon protocol version"
              , [ ( "block_current_protocol_version"
                  , `String
                      ( External_transition.current_protocol_version transition
                      |> Protocol_version.to_string ) )
                ; ( "daemon_current_protocol_version"
                  , `String Protocol_version.(get_current () |> to_string) ) ]
              ) )
      in
      Error (Error.of_string "mismatched protocol version")
  | Error `Disconnected ->
      Deferred.Or_error.fail @@ Error.of_string "disconnected chain"

let rec fold_until ~(init : 'accum)
    ~(f :
       'accum -> 'a -> ('accum, 'final) Continue_or_stop.t Deferred.Or_error.t)
    ~(finish : 'accum -> 'final Deferred.Or_error.t) :
    'a list -> 'final Deferred.Or_error.t = function
  | [] ->
      finish init
  | x :: xs -> (
      let open Deferred.Or_error.Let_syntax in
      match%bind f init x with
      | Continue_or_stop.Stop res ->
          Deferred.Or_error.return res
      | Continue_or_stop.Continue init ->
          fold_until ~init ~f ~finish xs )

let find_map_ok l ~f =
  Deferred.repeat_until_finished (l, []) (fun (l, errors) ->
      match l with
      | [] ->
          let errors = List.rev errors in
          Deferred.return @@ `Finished (Error errors)
      | hd :: tl ->
          Deferred.map (f hd) ~f:(function
            | Error current_error ->
                `Repeat (tl, current_error :: errors)
            | Ok result ->
                `Finished (Ok result) ) )

type download_state_hashes_error =
  [ `Peer_moves_too_fast of Error.t
  | `No_common_ancestor of Error.t
  | `Failed_to_download_transition_chain_proof of Error.t
  | `Invalid_transition_chain_proof of Error.t ]

let display_error = function
  | `Peer_moves_too_fast err ->
      Error.to_string_hum err
  | `No_common_ancestor err ->
      Error.to_string_hum err
  | `Failed_to_download_transition_chain_proof err ->
      Error.to_string_hum err
  | `Invalid_transition_chain_proof err ->
      Error.to_string_hum err

let rec contains_no_common_ancestor = function
  | [] ->
      false
  | `No_common_ancestor _ :: _ ->
      true
  | _ :: errors ->
      contains_no_common_ancestor errors

let to_error = function
  | `Peer_moves_too_fast err ->
      err
  | `No_common_ancestor err ->
      err
  | `Failed_to_download_transition_chain_proof err ->
      err
  | `Invalid_transition_chain_proof err ->
      err

(* returns a list of state-hashes with the older ones at the front *)
let download_state_hashes ~logger ~trust_system ~network ~frontier ~peers
    ~target_hash ~job ~hash_tree ~blockchain_length_of_target_hash =
  [%log debug]
    ~metadata:[("target_hash", State_hash.to_yojson target_hash)]
    "Doing a catchup job with target $target_hash" ;
  let blockchain_length_of_root =
    Transition_frontier.root frontier
    |> Transition_frontier.Breadcrumb.blockchain_length
  in
  let open Deferred.Result.Let_syntax in
  find_map_ok peers ~f:(fun peer ->
      let%bind transition_chain_proof =
        let open Deferred.Let_syntax in
        match%map
          Mina_networking.get_transition_chain_proof network peer target_hash
        with
        | Error err ->
            Result.fail @@ `Failed_to_download_transition_chain_proof err
        | Ok transition_chain_proof ->
            Result.return transition_chain_proof
      in
      (* a list of state_hashes from new to old *)
      let%bind hashes =
        match
          Transition_chain_verifier.verify ~target_hash ~transition_chain_proof
        with
        | Some hashes ->
            Deferred.Result.return hashes
        | None ->
            let error_msg =
              sprintf
                !"Peer %{sexp:Network_peer.Peer.t} sent us bad proof"
                peer
            in
            ignore
              Trust_system.(
                record trust_system logger peer
                  Actions.
                    ( Sent_invalid_transition_chain_merkle_proof
                    , Some (error_msg, []) )) ;
            Deferred.Result.fail
            @@ `Invalid_transition_chain_proof (Error.of_string error_msg)
      in
      Deferred.return
      @@ List.fold_until
           (Non_empty_list.to_list hashes)
           ~init:(blockchain_length_of_target_hash, [])
           ~f:(fun (blockchain_length, acc) hash ->
             match Transition_frontier.find frontier hash with
             | Some final ->
                 Continue_or_stop.Stop
                   (Ok (peer, Frontier_base.Breadcrumb.state_hash final, acc))
             | None ->
                 Continue_or_stop.Continue
                   (Unsigned.UInt32.pred blockchain_length, hash :: acc) )
           ~finish:(fun (blockchain_length, acc) ->
             let module T = struct
               type t = State_hash.t list [@@deriving to_yojson]
             end in
             let all_hashes =
               List.map (Transition_frontier.all_breadcrumbs frontier)
                 ~f:(fun b -> Frontier_base.Breadcrumb.state_hash b)
             in
             [%log debug]
               ~metadata:
                 [ ("n", `Int (List.length acc))
                 ; ("hashes", T.to_yojson acc)
                 ; ("all_hashes", T.to_yojson all_hashes) ]
               "Finishing download_state_hashes with $n $hashes. with \
                $all_hashes" ;
             if
               Unsigned.UInt32.compare blockchain_length
                 blockchain_length_of_root
               <= 0
             then
               Result.fail
               @@ `No_common_ancestor
                    (Error.of_string
                       "Requested block doesn't have a path to the root of \
                        our frontier")
             else
               let err_msg =
                 sprintf
                   !"Peer %{sexp:Network_peer.Peer.t} moves too fast"
                   peer
               in
               Result.fail @@ `Peer_moves_too_fast (Error.of_string err_msg) )
  )
  >>| fun (peer, final, hashes) ->
  let (_ : State_hash.t) =
    List.fold hashes ~init:final ~f:(fun parent h ->
        Transition_frontier.Catchup_hash_tree.add hash_tree h ~parent ~job ;
        h )
  in
  (peer, hashes)

let verify_against_hashes transitions hashes =
  List.length transitions = List.length hashes
  && List.for_all2_exn transitions hashes ~f:(fun transition hash ->
         State_hash.equal (External_transition.state_hash transition) hash )

let rec partition size = function
  | [] ->
      []
  | ls ->
      let sub, rest = List.split_n ls size in
      sub :: partition size rest

module Peers_pool = struct
  type t =
    {preferred: Peer.t Queue.t; normal: Peer.t Queue.t; busy: Peer.Hash_set.t}

  let create ~busy ~preferred peers =
    {preferred= Queue.of_list preferred; normal= Queue.of_list peers; busy}

  let dequeue {preferred; normal; busy} =
    let find_available q =
      let n = Queue.length q in
      let rec go tried =
        if tried = n then `All_busy
        else
          match Queue.dequeue q with
          | None ->
              `Empty
          | Some x ->
              if Hash_set.mem busy x then (
                Queue.enqueue q x ;
                go (tried + 1) )
              else `Available x
      in
      go 0
    in
    match find_available preferred with
    | `Available x ->
        `Available x
    | `Empty ->
        find_available normal
    | `All_busy -> (
      match find_available normal with
      | `Available x ->
          `Available x
      | `Empty | `All_busy ->
          `All_busy )
end

(* returns a list of transitions with old ones comes first *)
let download_transitions ~target_hash ~logger ~trust_system ~network
    ~preferred_peer ~hashes_of_missing_transitions =
  let busy = Peer.Hash_set.create () in
  Deferred.Or_error.List.concat_map
    (partition Transition_frontier.max_catchup_chunk_length
       hashes_of_missing_transitions) ~how:`Parallel ~f:(fun hashes ->
      let%bind.Async peers = Mina_networking.peers network in
      let peers =
        Peers_pool.create ~busy ~preferred:[preferred_peer]
          (List.permute peers)
      in
      let rec go errs =
        match Peers_pool.dequeue peers with
        | `Empty ->
            (* Tried everyone *)
            Deferred.return (Error (Error.of_list (List.rev errs)))
        | `All_busy ->
            let%bind () = after (Time.Span.of_sec 10.) in
            go errs
        | `Available peer -> (
            Hash_set.add busy peer ;
            let%bind res =
              Deferred.Or_error.try_with_join ~here:[%here] (fun () ->
                  let open Deferred.Or_error.Let_syntax in
                  [%log debug]
                    ~metadata:
                      [ ("n", `Int (List.length hashes))
                      ; ("peer", Peer.to_yojson peer)
                      ; ("target_hash", State_hash.to_yojson target_hash) ]
                    "requesting $n blocks from $peer for catchup to \
                     $target_hash" ;
                  let%bind transitions =
                    match%map.Async
                      Mina_networking.get_transition_chain network peer hashes
                    with
                    | Ok x ->
                        Ok x
                    | Error e ->
                        [%log debug]
                          ~metadata:
                            [ ("error", `String (Error.to_string_hum e))
                            ; ("n", `Int (List.length hashes))
                            ; ("peer", Peer.to_yojson peer) ]
                          "$error from downloading $n blocks from $peer" ;
                        Error e
                  in
                  Mina_metrics.(
                    Gauge.set
                      Transition_frontier_controller
                      .transitions_downloaded_from_catchup
                      (Float.of_int (List.length transitions))) ;
                  [%log debug]
                    ~metadata:
                      [ ("n", `Int (List.length transitions))
                      ; ("peer", Peer.to_yojson peer) ]
                    "downloaded $n blocks from $peer" ;
                  if not @@ verify_against_hashes transitions hashes then (
                    let error_msg =
                      sprintf
                        !"Peer %{sexp:Network_peer.Peer.t} returned a list \
                          that is different from the one that is requested."
                        peer
                    in
                    Trust_system.(
                      record trust_system logger peer
                        Actions.(Violated_protocol, Some (error_msg, [])))
                    |> don't_wait_for ;
                    Deferred.Or_error.error_string error_msg )
                  else
                    Deferred.Or_error.return
                    @@ List.map2_exn hashes transitions
                         ~f:(fun hash transition ->
                           let transition_with_hash =
                             With_hash.of_data transition
                               ~hash_data:(Fn.const hash)
                           in
                           Envelope.Incoming.wrap_peer
                             ~data:transition_with_hash ~sender:peer ) )
            in
            Hash_set.remove busy peer ;
            match res with
            | Ok x ->
                Deferred.return (Ok x)
            | Error e ->
                go (e :: errs) )
      in
      go [] )

let verify_transitions_and_build_breadcrumbs ~logger
    ~(precomputed_values : Precomputed_values.t) ~trust_system ~verifier
    ~frontier ~unprocessed_transition_cache ~transitions ~target_hash ~subtrees
    =
  let open Deferred.Or_error.Let_syntax in
  let verification_start_time = Core.Time.now () in
  let%bind transitions_with_initial_validation, initial_hash =
    let%bind tvs =
      let open Deferred.Let_syntax in
      match%bind
        External_transition.validate_proofs ~verifier
          (List.map transitions ~f:(fun t ->
               External_transition.Validation.wrap (Envelope.Incoming.data t)
           ))
      with
      | Ok tvs ->
          return
            (Ok
               (List.map2_exn transitions tvs ~f:(fun e data ->
                    (* this does not update the envelope timestamps *)
                    {e with data} )))
      | Error (`Verifier_error error) ->
          [%log warn]
            ~metadata:[("error", Error_json.error_to_yojson error)]
            "verifier threw an error while verifying transition queried \
             during ledger catchup: $error" ;
          Deferred.Or_error.fail
            (Error.tag ~tag:"verifier threw an error" error)
      | Error `Invalid_proof ->
          let%map () =
            (* TODO: Isolate and punish all the evil sender *)
            Deferred.unit
          in
          Error (Error.of_string "invalid proof")
    in
    let verification_end_time = Core.Time.now () in
    [%log debug]
      ~metadata:
        [ ("target_hash", State_hash.to_yojson target_hash)
        ; ( "time_elapsed"
          , `Float
              Core.Time.(
                Span.to_sec
                @@ diff verification_end_time verification_start_time) ) ]
      "verification of proofs complete" ;
    fold_until (List.rev tvs) ~init:[]
      ~f:(fun acc transition ->
        let open Deferred.Let_syntax in
        match%bind
          verify_transition ~logger
            ~consensus_constants:precomputed_values.consensus_constants
            ~trust_system ~frontier ~unprocessed_transition_cache transition
        with
        | Error e ->
            List.map acc ~f:Cached.invalidate_with_failure |> ignore ;
            Deferred.Or_error.fail e
        | Ok (`In_frontier initial_hash) ->
            Deferred.Or_error.return
            @@ Continue_or_stop.Stop (acc, initial_hash)
        | Ok (`Building_path transition_with_initial_validation) ->
            Deferred.Or_error.return
            @@ Continue_or_stop.Continue
                 (transition_with_initial_validation :: acc) )
      ~finish:(fun acc ->
        let validation_end_time = Core.Time.now () in
        [%log debug]
          ~metadata:
            [ ("target_hash", State_hash.to_yojson target_hash)
            ; ( "time_elapsed"
              , `Float
                  Core.Time.(
                    Span.to_sec
                    @@ diff validation_end_time verification_end_time) ) ]
          "validation of transitions complete" ;
        if List.length transitions <= 0 then
          Deferred.Or_error.return ([], target_hash)
        else
          let oldest_missing_transition =
            List.hd_exn transitions |> Envelope.Incoming.data |> With_hash.data
          in
          let initial_state_hash =
            External_transition.parent_hash oldest_missing_transition
          in
          Deferred.Or_error.return (acc, initial_state_hash) )
  in
  let build_start_time = Core.Time.now () in
  let trees_of_transitions =
    Option.fold
      (Non_empty_list.of_list_opt transitions_with_initial_validation)
      ~init:subtrees ~f:(fun _ transitions ->
        [Rose_tree.of_non_empty_list ~subtrees transitions] )
  in
  let open Deferred.Let_syntax in
  match%bind
    Transition_handler.Breadcrumb_builder.build_subtrees_of_breadcrumbs ~logger
      ~precomputed_values ~verifier ~trust_system ~frontier ~initial_hash
      trees_of_transitions
  with
  | Ok result ->
      [%log debug]
        ~metadata:
          [ ("target_hash", State_hash.to_yojson target_hash)
          ; ( "time_elapsed"
            , `Float Core.Time.(Span.to_sec @@ diff (now ()) build_start_time)
            ) ]
        "build of breadcrumbs complete" ;
      Deferred.Or_error.return result
  | Error e ->
      [%log debug]
        ~metadata:
          [ ("target_hash", State_hash.to_yojson target_hash)
          ; ( "time_elapsed"
            , `Float Core.Time.(Span.to_sec @@ diff (now ()) build_start_time)
            )
          ; ("error", `String (Error.to_string_hum e)) ]
        "build of breadcrumbs failed with $error" ;
      ( try
          List.map transitions_with_initial_validation
            ~f:Cached.invalidate_with_failure
          |> ignore
        with e ->
          [%log error]
            ~metadata:[("exn", `String (Exn.to_string e))]
            "$exn in cached" ) ;
      Deferred.Or_error.fail e

let garbage_collect_subtrees ~logger ~subtrees =
  List.iter subtrees ~f:(fun subtree ->
      Rose_tree.map subtree ~f:Cached.invalidate_with_failure |> ignore ) ;
  [%log trace] "garbage collected failed cached transitions"

let run ~logger ~precomputed_values ~trust_system ~verifier ~network ~frontier
    ~catchup_job_reader
    ~(catchup_breadcrumbs_writer :
       ( (Transition_frontier.Breadcrumb.t, State_hash.t) Cached.t Rose_tree.t
         list
         * [`Ledger_catchup of unit Ivar.t | `Catchup_scheduler]
       , Strict_pipe.crash Strict_pipe.buffered
       , unit )
       Strict_pipe.Writer.t) ~unprocessed_transition_cache : unit =
  let hash_tree =
    match Transition_frontier.catchup_tree frontier with
    | Hash t ->
        t
    | Full _ ->
        failwith
          "If normal catchup is running, the frontier should have a hash \
           tree, got a full one."
  in
  don't_wait_for
    (Strict_pipe.Reader.iter_without_pushback catchup_job_reader
       ~f:(fun (target_hash, subtrees) ->
         let job =
           Transition_frontier.Catchup_hash_tree.Catchup_job_id.create ()
         in
         let notify_hash_tree_of_failure () =
           Transition_frontier.(Catchup_hash_tree.catchup_failed hash_tree job)
         in
         don't_wait_for
           (let start_time = Core.Time.now () in
            [%log info] "Catch up to $target_hash"
              ~metadata:[("target_hash", State_hash.to_yojson target_hash)] ;
            let%bind () = Catchup_jobs.incr () in
            let blockchain_length_of_target_hash =
              let blockchain_length_of_dangling_block =
                List.hd_exn subtrees |> Rose_tree.root |> Cached.peek
                |> Envelope.Incoming.data
                |> External_transition.Initial_validated.blockchain_length
              in
              Unsigned.UInt32.pred blockchain_length_of_dangling_block
            in
            let subtree_peers =
              List.fold subtrees ~init:[] ~f:(fun acc_outer tree ->
                  let cacheds = Rose_tree.flatten tree in
                  let cached_peers =
                    List.fold cacheds ~init:[] ~f:(fun acc_inner cached ->
                        let envelope = Cached.peek cached in
                        match Envelope.Incoming.sender envelope with
                        | Local ->
                            acc_inner
                        | Remote peer ->
                            peer :: acc_inner )
                  in
                  cached_peers @ acc_outer )
              |> List.dedup_and_sort ~compare:Peer.compare
            in
            match%bind
              let open Deferred.Or_error.Let_syntax in
              let%bind preferred_peer, hashes_of_missing_transitions =
                (* try peers from subtrees first *)
                let open Deferred.Let_syntax in
                match%bind
                  download_state_hashes ~hash_tree ~logger ~trust_system
                    ~network ~frontier ~peers:subtree_peers ~target_hash ~job
                    ~blockchain_length_of_target_hash
                with
                | Ok (peer, hashes) ->
                    return (Ok (peer, hashes))
                | Error errors -> (
                    [%log info]
                      "Could not download state hashes using peers from \
                       subtrees; trying again with random peers"
                      ~metadata:
                        [ ( "errors"
                          , `List
                              (List.map errors ~f:(fun err ->
                                   `String (display_error err) )) ) ] ;
                    let%bind random_peers =
                      Mina_networking.peers network >>| List.permute
                    in
                    match%bind
                      download_state_hashes ~hash_tree ~logger ~trust_system
                        ~network ~frontier ~peers:random_peers ~target_hash
                        ~job ~blockchain_length_of_target_hash
                    with
                    | Ok (peer, hashes) ->
                        return (Ok (peer, hashes))
                    | Error errors ->
                        [%log info]
                          "Could not download state hashes using random peers"
                          ~metadata:
                            [ ( "errors"
                              , `List
                                  (List.map errors ~f:(fun err ->
                                       `String (display_error err) )) ) ] ;
                        if contains_no_common_ancestor errors then
                          List.iter subtrees ~f:(fun subtree ->
                              let transition =
                                Rose_tree.root subtree |> Cached.peek
                                |> Envelope.Incoming.data
                              in
                              let children_transitions =
                                List.concat_map
                                  (Rose_tree.children subtree)
                                  ~f:Rose_tree.flatten
                              in
                              let children_state_hashes =
                                List.map children_transitions
                                  ~f:(fun cached_transition ->
                                    Cached.peek cached_transition
                                    |> Envelope.Incoming.data
                                    |> External_transition.Initial_validated
                                       .state_hash )
                              in
                              [%log error]
                                ~metadata:
                                  [ ( "state_hashes_of_children"
                                    , `List
                                        (List.map children_state_hashes
                                           ~f:State_hash.to_yojson) )
                                  ; ( "state_hash"
                                    , State_hash.to_yojson
                                      @@ External_transition.Initial_validated
                                         .state_hash transition )
                                  ; ( "reason"
                                    , `String
                                        "no common ancestor with our \
                                         transition frontier" )
                                  ; ( "protocol_state"
                                    , External_transition.Initial_validated
                                      .protocol_state transition
                                      |> Mina_state.Protocol_state
                                         .value_to_yojson ) ]
                                "Validation error: external transition with \
                                 state hash $state_hash and its children were \
                                 rejected for reason $reason" ;
                              Mina_metrics.(
                                Counter.inc Rejected_blocks.no_common_ancestor
                                  ( Float.of_int
                                  @@ (1 + List.length children_transitions) ))
                          ) ;
                        return
                          (Error (Error.of_list @@ List.map errors ~f:to_error))
                    )
              in
              let num_of_missing_transitions =
                List.length hashes_of_missing_transitions
              in
              [%log debug]
                ~metadata:
                  [ ( "hashes_of_missing_transitions"
                    , `List
                        (List.map hashes_of_missing_transitions
                           ~f:State_hash.to_yojson) ) ]
                !"Number of missing transitions is %d"
                num_of_missing_transitions ;
              let%bind transitions =
                if num_of_missing_transitions <= 0 then
                  Deferred.Or_error.return []
                else
                  download_transitions ~logger ~trust_system ~network
                    ~preferred_peer ~hashes_of_missing_transitions ~target_hash
              in
              [%log debug]
                ~metadata:[("target_hash", State_hash.to_yojson target_hash)]
                "Download transitions complete" ;
              verify_transitions_and_build_breadcrumbs ~logger
                ~precomputed_values ~trust_system ~verifier ~frontier
                ~unprocessed_transition_cache ~transitions ~target_hash
                ~subtrees
            with
            | Ok trees_of_breadcrumbs ->
                [%log trace]
                  ~metadata:
                    [ ( "hashes of transitions"
                      , `List
                          (List.map trees_of_breadcrumbs ~f:(fun tree ->
                               Rose_tree.to_yojson
                                 (fun breadcrumb ->
                                   Cached.peek breadcrumb
                                   |> Transition_frontier.Breadcrumb.state_hash
                                   |> State_hash.to_yojson )
                                 tree )) ) ]
                  "about to write to the catchup breadcrumbs pipe" ;
                if Strict_pipe.Writer.is_closed catchup_breadcrumbs_writer then (
                  [%log trace]
                    "catchup breadcrumbs pipe was closed; attempt to write to \
                     closed pipe" ;
                  notify_hash_tree_of_failure () ;
                  garbage_collect_subtrees ~logger
                    ~subtrees:trees_of_breadcrumbs ;
                  Mina_metrics.(
                    Gauge.set Transition_frontier_controller.catchup_time_ms
                      Core.Time.(Span.to_ms @@ diff (now ()) start_time)) ;
                  Catchup_jobs.decr () )
                else
                  let ivar = Ivar.create () in
                  Strict_pipe.Writer.write catchup_breadcrumbs_writer
                    (trees_of_breadcrumbs, `Ledger_catchup ivar) ;
                  let%bind () = Ivar.read ivar in
                  Mina_metrics.(
                    Gauge.set Transition_frontier_controller.catchup_time_ms
                      Core.Time.(Span.to_ms @@ diff (now ()) start_time)) ;
                  Catchup_jobs.decr ()
            | Error e ->
                [%log warn]
                  ~metadata:[("error", Error_json.error_to_yojson e)]
                  "Catchup process failed -- unable to receive valid data \
                   from peers or transition frontier progressed faster than \
                   catchup data received. See error for details: $error" ;
                notify_hash_tree_of_failure () ;
                garbage_collect_subtrees ~logger ~subtrees ;
                Mina_metrics.(
                  Gauge.set Transition_frontier_controller.catchup_time_ms
                    Core.Time.(Span.to_ms @@ diff (now ()) start_time)) ;
                Catchup_jobs.decr ()) ))

let%test_module "Ledger_catchup tests" =
  ( module struct
    let () =
      Core.Backtrace.elide := false ;
      Async.Scheduler.set_record_backtraces true

    let max_frontier_length = 10

    let logger = Logger.null ()

    let precomputed_values = Lazy.force Precomputed_values.for_unit_tests

    let proof_level = precomputed_values.proof_level

    let constraint_constants = precomputed_values.constraint_constants

    let trust_system = Trust_system.null ()

    let time_controller = Block_time.Controller.basic ~logger

    let verifier =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Verifier.create ~logger ~proof_level ~constraint_constants
            ~conf_dir:None
            ~pids:(Child_processes.Termination.create_pid_table ()) )

    let downcast_transition transition =
      let transition =
        transition
        |> External_transition.Validation
           .reset_frontier_dependencies_validation
        |> External_transition.Validation.reset_staged_ledger_diff_validation
      in
      Envelope.Incoming.wrap ~data:transition ~sender:Envelope.Sender.Local

    let downcast_breadcrumb breadcrumb =
      downcast_transition
        (Transition_frontier.Breadcrumb.validated_transition breadcrumb)

    type catchup_test =
      { cache: Transition_handler.Unprocessed_transition_cache.t
      ; job_writer:
          ( State_hash.t
            * ( External_transition.Initial_validated.t Envelope.Incoming.t
              , State_hash.t )
              Cached.t
              Rose_tree.t
              list
          , Strict_pipe.crash Strict_pipe.buffered
          , unit )
          Strict_pipe.Writer.t
      ; breadcrumbs_reader:
          ( (Transition_frontier.Breadcrumb.t, State_hash.t) Cached.t
            Rose_tree.t
            list
          * [`Catchup_scheduler | `Ledger_catchup of unit Ivar.t] )
          Strict_pipe.Reader.t }

    let run_catchup ~network ~frontier =
      let catchup_job_reader, catchup_job_writer =
        Strict_pipe.create ~name:(__MODULE__ ^ __LOC__)
          (Buffered (`Capacity 10, `Overflow Crash))
      in
      let catchup_breadcrumbs_reader, catchup_breadcrumbs_writer =
        Strict_pipe.create ~name:(__MODULE__ ^ __LOC__)
          (Buffered (`Capacity 10, `Overflow Crash))
      in
      let unprocessed_transition_cache =
        Transition_handler.Unprocessed_transition_cache.create ~logger
      in
      run ~logger ~precomputed_values ~verifier ~trust_system ~network
        ~frontier ~catchup_breadcrumbs_writer ~catchup_job_reader
        ~unprocessed_transition_cache ;
      { cache= unprocessed_transition_cache
      ; job_writer= catchup_job_writer
      ; breadcrumbs_reader= catchup_breadcrumbs_reader }

    let run_catchup_with_target ~network ~frontier ~target_breadcrumb =
      let test = run_catchup ~network ~frontier in
      let parent_hash =
        Transition_frontier.Breadcrumb.parent_hash target_breadcrumb
      in
      let target_transition =
        Transition_handler.Unprocessed_transition_cache.register_exn test.cache
          (downcast_breadcrumb target_breadcrumb)
      in
      Strict_pipe.Writer.write test.job_writer
        (parent_hash, [Rose_tree.T (target_transition, [])]) ;
      (`Test test, `Cached_transition target_transition)

    let test_successful_catchup ~my_net ~target_best_tip_path =
      let open Fake_network in
      let target_breadcrumb = List.last_exn target_best_tip_path in
      let `Test {breadcrumbs_reader; _}, _ =
        run_catchup_with_target ~network:my_net.network
          ~frontier:my_net.state.frontier ~target_breadcrumb
      in
      (* TODO: expose Strict_pipe.read *)
      let%map cached_catchup_breadcrumbs =
        Block_time.Timeout.await_exn time_controller
          ~timeout_duration:(Block_time.Span.of_ms 30000L)
          ( match%map Strict_pipe.Reader.read breadcrumbs_reader with
          | `Eof ->
              failwith "unexpected EOF"
          | `Ok (_, `Catchup_scheduler) ->
              failwith "did not expect a catchup scheduler action"
          | `Ok (breadcrumbs, `Ledger_catchup ivar) ->
              Ivar.fill ivar () ; List.hd_exn breadcrumbs )
      in
      let catchup_breadcrumbs =
        Rose_tree.map cached_catchup_breadcrumbs
          ~f:Cache_lib.Cached.invalidate_with_success
      in
      [%test_result: int]
        ~message:
          "Transition_frontier should not have any more catchup jobs at the \
           end of the test"
        ~equal:( = ) ~expect:0
        (Broadcast_pipe.Reader.peek Catchup_jobs.reader) ;
      let catchup_breadcrumbs_are_best_tip_path =
        Rose_tree.equal (Rose_tree.of_list_exn target_best_tip_path)
          catchup_breadcrumbs ~f:(fun breadcrumb_tree1 breadcrumb_tree2 ->
            External_transition.Validated.equal
              (Transition_frontier.Breadcrumb.validated_transition
                 breadcrumb_tree1)
              (Transition_frontier.Breadcrumb.validated_transition
                 breadcrumb_tree2) )
      in
      if not catchup_breadcrumbs_are_best_tip_path then
        failwith
          "catchup breadcrumbs were not equal to the best tip path we expected"

    let%test_unit "can catchup to a peer within [2/k,k]" =
      Quickcheck.test ~trials:5
        Fake_network.Generator.(
          let open Quickcheck.Generator.Let_syntax in
          let%bind peer_branch_size =
            Int.gen_incl (max_frontier_length / 2) (max_frontier_length - 1)
          in
          gen ~precomputed_values ~verifier ~max_frontier_length
            [ fresh_peer
            ; peer_with_branch ~frontier_branch_size:peer_branch_size ])
        ~f:(fun network ->
          let open Fake_network in
          let [my_net; peer_net] = network.peer_networks in
          (* TODO: I don't think I'm testing this right... *)
          let target_best_tip_path =
            Transition_frontier.(
              path_map ~f:Fn.id peer_net.state.frontier
                (best_tip peer_net.state.frontier))
          in
          Thread_safe.block_on_async_exn (fun () ->
              test_successful_catchup ~my_net ~target_best_tip_path ) )

    let%test_unit "catchup succeeds even if the parent transition is already \
                   in the frontier" =
      Quickcheck.test ~trials:1
        Fake_network.Generator.(
          gen ~precomputed_values ~verifier ~max_frontier_length
            [fresh_peer; peer_with_branch ~frontier_branch_size:1])
        ~f:(fun network ->
          let open Fake_network in
          let [my_net; peer_net] = network.peer_networks in
          let target_best_tip_path =
            [Transition_frontier.best_tip peer_net.state.frontier]
          in
          Thread_safe.block_on_async_exn (fun () ->
              test_successful_catchup ~my_net ~target_best_tip_path ) )

    let%test_unit "catchup fails if one of the parent transitions fail" =
      Quickcheck.test ~trials:1
        Fake_network.Generator.(
          gen ~precomputed_values ~verifier ~max_frontier_length
            [ fresh_peer
            ; peer_with_branch ~frontier_branch_size:(max_frontier_length * 2)
            ])
        ~f:(fun network ->
          let open Fake_network in
          let [my_net; peer_net] = network.peer_networks in
          let target_breadcrumb =
            Transition_frontier.best_tip peer_net.state.frontier
          in
          let failing_transition =
            let open Transition_frontier.Extensions in
            let history =
              get_extension
                (Transition_frontier.extensions peer_net.state.frontier)
                Root_history
            in
            let failing_root_data =
              List.nth_exn (Root_history.to_list history) 1
            in
            downcast_transition
              (Frontier_base.Root_data.Historical.transition failing_root_data)
          in
          Thread_safe.block_on_async_exn (fun () ->
              let `Test {cache; _}, `Cached_transition cached_transition =
                run_catchup_with_target ~network:my_net.network
                  ~frontier:my_net.state.frontier ~target_breadcrumb
              in
              let cached_failing_transition =
                Transition_handler.Unprocessed_transition_cache.register_exn
                  cache failing_transition
              in
              let%bind () = after (Core.Time.Span.of_sec 1.) in
              ignore
                (Cache_lib.Cached.invalidate_with_failure
                   cached_failing_transition) ;
              let%map result =
                Block_time.Timeout.await_exn time_controller
                  ~timeout_duration:(Block_time.Span.of_ms 10000L)
                  (Ivar.read (Cache_lib.Cached.final_state cached_transition))
              in
              if result <> `Failed then
                failwith "expected ledger catchup to fail, but it succeeded" )
          )

    (* TODO: fix and re-enable *)
    (*
    let%test_unit "catchup won't be blocked by transitions that are still being processed" =
      Quickcheck.test ~trials:1
        Fake_network.Generator.(
          gen ~max_frontier_length
            [ fresh_peer
            ; peer_with_branch ~frontier_branch_size:(max_frontier_length-1) ])
        ~f:(fun network ->
          let open Fake_network in
          let [my_net; peer_net] = network.peer_networks in
          Core.Printf.printf "$my_net.state.frontier.root = %s\n"
            (State_hash.to_base58_check @@ Transition_frontier.(Breadcrumb.state_hash @@ root my_net.state.frontier));
          Core.Printf.printf "$peer_net.state.frontier.root = %s\n"
            (State_hash.to_base58_check @@ Transition_frontier.(Breadcrumb.state_hash @@ root my_net.state.frontier));
          let missing_breadcrumbs =
            let best_tip_path = Transition_frontier.best_tip_path peer_net.state.frontier in
            Core.Printf.printf "$best_tip_path=\n  %s\n"
              (String.concat ~sep:"\n  " @@ List.map ~f:(Fn.compose State_hash.to_base58_check Transition_frontier.Breadcrumb.state_hash) best_tip_path);
            (* List.take best_tip_path (List.length best_tip_path - 1) *)
            best_tip_path
          in
          Async.Thread_safe.block_on_async_exn (fun () ->
            let%bind {cache; job_writer; breadcrumbs_reader} = run_catchup ~network:my_net.network ~frontier:my_net.state.frontier in
            let jobs =
              List.map (List.rev missing_breadcrumbs) ~f:(fun breadcrumb ->
                let parent_hash = Transition_frontier.Breadcrumb.parent_hash breadcrumb in
                let cached_transition =
                  Transition_handler.Unprocessed_transition_cache.register_exn cache
                    (downcast_breadcrumb breadcrumb)
                in
                Core.Printf.printf "$job = %s --> %s\n"
                  (State_hash.to_base58_check @@ External_transition.Initial_validated.state_hash @@ Envelope.Incoming.data @@ Cached.peek cached_transition)
                  (State_hash.to_base58_check parent_hash);
                (parent_hash, [Rose_tree.T (cached_transition, [])]))
            in
            let%bind () = after (Core.Time.Span.of_ms 500.) in
            List.iter jobs ~f:(Strict_pipe.Writer.write job_writer);
            match%map
              Block_time.Timeout.await_exn time_controller
                ~timeout_duration:(Block_time.Span.of_ms 15000L)
                (Strict_pipe.Reader.fold_until breadcrumbs_reader ~init:missing_breadcrumbs ~f:(fun remaining_breadcrumbs (rose_trees, catchup_signal) ->
                  let[@warning "-8"] [rose_tree] = rose_trees in
                  let catchup_breadcrumb_tree = Rose_tree.map rose_tree ~f:Cached.invalidate_with_success in
                  Core.Printf.printf "!!!%d\n" (List.length @@ Rose_tree.flatten catchup_breadcrumb_tree);
                  let[@warning "-8"] [received_breadcrumb] = Rose_tree.flatten catchup_breadcrumb_tree in
                  match remaining_breadcrumbs with
                  | [] -> failwith "received more breadcrumbs than expected"
                  | expected_breadcrumb :: remaining_breadcrumbs' ->
                      Core.Printf.printf "COMPARING %s vs. %s..."
                        (State_hash.to_base58_check @@ Transition_frontier.Breadcrumb.state_hash expected_breadcrumb)
                        (State_hash.to_base58_check @@ Transition_frontier.Breadcrumb.state_hash received_breadcrumb);
                      [%test_eq: State_hash.t]
                        (Transition_frontier.Breadcrumb.state_hash expected_breadcrumb)
                        (Transition_frontier.Breadcrumb.state_hash received_breadcrumb)
                        ~message:"received breadcrumb state hash did not match expected breadcrumb state hash";
                      [%test_eq: Transition_frontier.Breadcrumb.t]
                        expected_breadcrumb
                        received_breadcrumb
                        ~message:"received breadcrumb matched expected state hash, but was not equal to expected breadcrumb";
                      ( match catchup_signal with
                      | `Catchup_scheduler ->
                          failwith "Did not expect a catchup scheduler action"
                      | `Ledger_catchup ivar ->
                          Ivar.fill ivar () ) ;
                      print_endline " ok";
                      if remaining_breadcrumbs' = [] then
                        return (`Stop ())
                      else
                        return (`Continue remaining_breadcrumbs')))
            with
            | `Eof _ -> failwith "unexpected EOF"
            | `Terminated () -> ()))
    *)
  end )
