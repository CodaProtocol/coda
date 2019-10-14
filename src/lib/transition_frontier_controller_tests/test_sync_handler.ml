open Core
open Async
open Coda_base
open Coda_state
open Coda_transition

let num_breadcrumb_to_add = 3

let max_length = num_breadcrumb_to_add + 2

module Stubs = Stubs.Make (struct
  let max_length = max_length
end)

open Stubs

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
                create_root_frontier ~logger ~pids Genesis_ledger.accounts
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
            create_root_frontier ~logger ~pids Genesis_ledger.accounts
          in
          let%bind () =
            build_frontier_randomly frontier
              ~gen_root_breadcrumb_builder:
                (gen_linear_breadcrumbs ~logger ~pids ~trust_system
                   ~size:num_breadcrumbs
                   ~accounts_with_secret_keys:Genesis_ledger.accounts)
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
