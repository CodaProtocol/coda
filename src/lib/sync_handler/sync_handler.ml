open Core_kernel
open Async
open Mina_base
open Mina_transition
open Frontier_base
open Network_peer

module type Inputs_intf = sig
  module Transition_frontier : module type of Transition_frontier

  module Best_tip_prover :
    Mina_intf.Best_tip_prover_intf
    with type transition_frontier := Transition_frontier.t
end

module Make (Inputs : Inputs_intf) :
  Mina_intf.Sync_handler_intf
  with type transition_frontier := Inputs.Transition_frontier.t = struct
  open Inputs

  let find_in_root_history frontier state_hash =
    let open Transition_frontier.Extensions in
    let root_history =
      get_extension (Transition_frontier.extensions frontier) Root_history
    in
    Root_history.lookup root_history state_hash

  let protocol_states_in_root_history frontier state_hash =
    let open Transition_frontier.Extensions in
    let root_history =
      get_extension (Transition_frontier.extensions frontier) Root_history
    in
    Root_history.protocol_states_for_scan_state root_history state_hash

  let get_ledger_by_hash ~frontier ledger_hash =
    let root_ledger =
      Ledger.Any_ledger.cast (module Ledger.Db)
      @@ Transition_frontier.root_snarked_ledger frontier
    in
    let staking_epoch_ledger =
      Transition_frontier.consensus_local_state frontier
      |> Consensus.Data.Local_state.staking_epoch_ledger
    in
    let next_epoch_ledger =
      Transition_frontier.consensus_local_state frontier
      |> Consensus.Data.Local_state.next_epoch_ledger
    in
    if
      Ledger_hash.equal ledger_hash
        (Ledger.Any_ledger.M.merkle_root root_ledger)
    then Some root_ledger
    else if
      Ledger_hash.equal ledger_hash
        (Consensus.Data.Local_state.Snapshot.Ledger_snapshot.merkle_root
           staking_epoch_ledger)
    then
      match staking_epoch_ledger with
      | Consensus.Data.Local_state.Snapshot.Ledger_snapshot
        .Genesis_epoch_ledger _ ->
          None
      | Ledger_db ledger ->
          Some (Ledger.Any_ledger.cast (module Ledger.Db) ledger)
    else if
      Ledger_hash.equal ledger_hash
        (Consensus.Data.Local_state.Snapshot.Ledger_snapshot.merkle_root
           next_epoch_ledger)
    then
      match next_epoch_ledger with
      | Consensus.Data.Local_state.Snapshot.Ledger_snapshot
        .Genesis_epoch_ledger _ ->
          None
      | Ledger_db ledger ->
          Some (Ledger.Any_ledger.cast (module Ledger.Db) ledger)
    else None

  let answer_query :
         frontier:Inputs.Transition_frontier.t
      -> Ledger_hash.t
      -> Sync_ledger.Query.t Envelope.Incoming.t
      -> logger:Logger.t
      -> trust_system:Trust_system.t
      -> Sync_ledger.Answer.t Option.t Deferred.t =
   fun ~frontier hash query ~logger ~trust_system ->
    match get_ledger_by_hash ~frontier hash with
    | None ->
        return None
    | Some ledger ->
        let responder =
          Sync_ledger.Any_ledger.Responder.create ledger ignore ~logger
            ~trust_system
        in
        Sync_ledger.Any_ledger.Responder.answer_query responder query

  let get_staged_ledger_aux_and_pending_coinbases_at_hash ~frontier state_hash
      =
    let open Option.Let_syntax in
    let protocol_states scan_state =
      Staged_ledger.Scan_state.required_state_hashes scan_state
      |> State_hash.Set.to_list
      |> List.fold_until ~init:(Some [])
           ~f:(fun acc hash ->
             match
               Option.map2
                 (Transition_frontier.find_protocol_state frontier hash)
                 acc ~f:List.cons
             with
             | None ->
                 Stop None
             | Some acc' ->
                 Continue (Some acc') )
           ~finish:Fn.id
    in
    match
      let%bind breadcrumb = Transition_frontier.find frontier state_hash in
      let staged_ledger =
        Transition_frontier.Breadcrumb.staged_ledger breadcrumb
      in
      let scan_state = Staged_ledger.scan_state staged_ledger in
      let merkle_root =
        Staged_ledger.hash staged_ledger |> Staged_ledger_hash.ledger_hash
      in
      let%map scan_state_protocol_states = protocol_states scan_state in
      let pending_coinbase =
        Staged_ledger.pending_coinbase_collection staged_ledger
      in
      (scan_state, merkle_root, pending_coinbase, scan_state_protocol_states)
    with
    | Some res ->
        Some res
    | None ->
        let open Root_data.Historical in
        let%bind root = find_in_root_history frontier state_hash in
        let%map scan_state_protocol_states =
          protocol_states_in_root_history frontier state_hash
        in
        ( scan_state root
        , staged_ledger_target_ledger_hash root
        , pending_coinbase root
        , scan_state_protocol_states )

  let get_transition_chain ~frontier hashes =
    let open Option.Let_syntax in
    let%bind () =
      let requested = List.length hashes in
      if requested <= Transition_frontier.max_catchup_chunk_length then Some ()
      else (
        [%log' trace (Logger.create ())]
          ~metadata:[("n", `Int requested)]
          "get_transition_chain requested $n > %d hashes"
          Transition_frontier.max_catchup_chunk_length ;
        None )
    in
    let get hash =
      let%map validated_transition =
        Option.merge
          Transition_frontier.(
            find frontier hash >>| Breadcrumb.validated_transition)
          ( find_in_root_history frontier hash
          >>| fun x -> Root_data.Historical.transition x )
          ~f:Fn.const
      in
      External_transition.Validation.forget_validation validated_transition
    in
    match Transition_frontier.catchup_tree frontier with
    | Full _ ->
        (* Super catchup *)
        Option.return @@ List.filter_map hashes ~f:get
    | Hash _ ->
        (* Normal catchup *)
        Option.all @@ List.map hashes ~f:get

  let best_tip_path ~frontier =
    let rec go acc b =
      let acc = Breadcrumb.state_hash b :: acc in
      match Transition_frontier.find frontier (Breadcrumb.parent_hash b) with
      | None ->
          acc
      | Some b' ->
          go acc b'
    in
    go [] (Transition_frontier.best_tip frontier)

  module Root = struct
    let prove ~logger ~consensus_constants ~frontier seen_consensus_state =
      let open Option.Let_syntax in
      let%bind best_tip_with_witness =
        Best_tip_prover.prove ~logger frontier
      in
      let is_tip_better =
        Consensus.Hooks.equal_select_status
          (Consensus.Hooks.select ~constants:consensus_constants
             ~logger:
               (Logger.extend logger
                  [("selection_context", `String "Root.prove")])
             ~existing:
               (With_hash.map ~f:External_transition.consensus_state
                  best_tip_with_witness.data)
             ~candidate:seen_consensus_state)
          `Keep
      in
      let%map () = Option.some_if is_tip_better () in
      { best_tip_with_witness with
        data= With_hash.data best_tip_with_witness.data }

    let verify ~logger ~verifier ~consensus_constants ~genesis_constants
        ~precomputed_values observed_state peer_root =
      let open Deferred.Result.Let_syntax in
      let%bind ( (`Root _, `Best_tip (best_tip_transition, _)) as
               verified_witness ) =
        Best_tip_prover.verify ~verifier ~genesis_constants ~precomputed_values
          peer_root
      in
      let is_before_best_tip candidate =
        Consensus.Hooks.equal_select_status
          (Consensus.Hooks.select ~constants:consensus_constants
             ~logger:
               (Logger.extend logger
                  [("selection_context", `String "Root.verify")])
             ~existing:
               (With_hash.map ~f:External_transition.consensus_state
                  best_tip_transition)
             ~candidate)
          `Keep
      in
      let%map () =
        Deferred.return
          (Result.ok_if_true
             (is_before_best_tip observed_state)
             ~error:
               (Error.createf
                  !"Peer lied about it's best tip %{sexp:State_hash.t}"
                  best_tip_transition.hash))
      in
      verified_witness
  end
end

include Make (struct
  module Transition_frontier = Transition_frontier
  module Best_tip_prover = Best_tip_prover
end)

(* TODO: port these tests *)
(*
let%test_module "Sync_handler" =
  ( module struct
    let logger = Logger.null ()

    let hb_logger = Logger.create ()

    let pids = Child_processes.Termination.create_pid_table ()

    let trust_system = Trust_system.null ()

    let f_with_verifier ~f ~logger ~pids =
      let%map verifier = Verifier.create ~logger ~pids in
      f ~logger ~verifier

    let%test "sync with ledgers from another peer via glue_sync_ledger" =
      Backtrace.elide := false ;
      Printexc.record_backtrace true ;
      heartbeat_flag := true ;
      Ledger.with_ephemeral_ledger ~f:(fun dest_ledger ->
          Thread_safe.block_on_async_exn (fun () ->
              print_heartbeat hb_logger |> don't_wait_for ;
              let%bind frontier =
                create_root_frontier ~logger ~pids Test_genesis_ledger.accounts
              in
              let source_ledger =
                Transition_frontier.For_tests.root_snarked_ledger frontier
                |> Ledger.of_database
              in
              let desired_root = Ledger.merkle_root source_ledger in
              let sync_ledger =
                Sync_ledger.Mask.create dest_ledger ~logger ~trust_system
              in
              let query_reader = Sync_ledger.Mask.query_reader sync_ledger in
              let answer_writer = Sync_ledger.Mask.answer_writer sync_ledger in
              let peer =
                Network_peer.Peer.create Unix.Inet_addr.localhost
                  ~discovery_port:0 ~communication_port:1
              in
              let network =
                Network.create_stub ~logger
                  ~ip_table:
                    (Hashtbl.of_alist_exn
                       (module Unix.Inet_addr)
                       [(peer.host, frontier)])
                  ~peers:(Hash_set.of_list (module Network_peer.Peer) [peer])
              in
              Network.glue_sync_ledger network query_reader answer_writer ;
              match%map
                Sync_ledger.Mask.fetch sync_ledger desired_root ~data:()
                  ~equal:(fun () () -> true)
              with
              | `Ok synced_ledger ->
                  heartbeat_flag := false ;
                  Ledger_hash.equal
                    (Ledger.merkle_root dest_ledger)
                    (Ledger.merkle_root source_ledger)
                  && Ledger_hash.equal
                       (Ledger.merkle_root synced_ledger)
                       (Ledger.merkle_root source_ledger)
              | `Target_changed _ ->
                  heartbeat_flag := false ;
                  failwith "target of sync_ledger should not change" ) )

    let to_external_transition breadcrumb =
      Transition_frontier.Breadcrumb.validated_transition breadcrumb
      |> External_transition.Validation.forget_validation

    let%test "a node should be able to give a valid proof of their root" =
      heartbeat_flag := true ;
      let max_length = 4 in
      (* Generating this many breadcrumbs will ernsure the transition_frontier to be full  *)
      let num_breadcrumbs = max_length + 2 in
      Thread_safe.block_on_async_exn (fun () ->
          print_heartbeat hb_logger |> don't_wait_for ;
          let%bind frontier =
            create_root_frontier ~logger ~pids Test_genesis_ledger.accounts
          in
          let%bind () =
            build_frontier_randomly frontier
              ~gen_root_breadcrumb_builder:
                (gen_linear_breadcrumbs ~logger ~pids ~trust_system
                   ~size:num_breadcrumbs
                   ~accounts_with_secret_keys:Test_genesis_ledger.accounts)
          in
          let seen_transition =
            Transition_frontier.(
              all_breadcrumbs frontier |> List.permute |> List.hd_exn
              |> Breadcrumb.validated_transition)
          in
          let observed_state =
            External_transition.Validated.protocol_state seen_transition
            |> Protocol_state.consensus_state
          in
          let root_with_proof =
            Option.value_exn ~message:"Could not produce an ancestor proof"
              (Sync_handler.Root.prove ~logger ~frontier observed_state)
          in
          let%bind verify =
            f_with_verifier ~f:Sync_handler.Root.verify ~logger ~pids
          in
          let%map `Root (root_transition, _), `Best_tip (best_tip_transition, _)
              =
            verify observed_state root_with_proof |> Deferred.Or_error.ok_exn
          in
          heartbeat_flag := false ;
          External_transition.(
            equal
              (With_hash.data root_transition)
              (to_external_transition (Transition_frontier.root frontier))
            && equal
                 (With_hash.data best_tip_transition)
                 (to_external_transition
                    (Transition_frontier.best_tip frontier))) )
  end )
*)
