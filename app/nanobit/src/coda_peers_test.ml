open Core
open Async
open Coda_worker
open Coda_main

module Make
    (Ledger_proof : Ledger_proof_intf)
    (Kernel : Kernel_intf with type Ledger_proof.t = Ledger_proof.t)
    (Coda : Coda_intf.S with type ledger_proof = Ledger_proof.t) :
  Integration_test_intf.S =
struct
  let name = "coda-peers-test"

  module Coda_processes = Coda_processes.Make (Ledger_proof) (Kernel) (Coda)
  open Coda_processes

  let main () =
    let%bind program_dir = Unix.getcwd () in
    let n = 3 in
    let log = Logger.create () in
    let log = Logger.child log name in
    Coda_processes.init () ;
    Coda_processes.spawn_local_processes_exn n ~program_dir
      ~should_propose:(Fn.const false) ~f:(fun workers ->
        let _, _, expected_peers = Coda_processes.net_configs n in
        let%bind _ = after (Time.Span.of_sec 10.) in
        Deferred.all_unit
          (List.map2_exn workers expected_peers ~f:
             (fun worker expected_peers ->
               let%bind peers = Coda_process.peers_exn worker in
               Logger.debug log
                 !"got peers %{sexp: Kademlia.Peer.t list} %{sexp: \
                   Host_and_port.t list}\n"
                 peers expected_peers ;
               let module S = Host_and_port.Set in
               assert (
                 S.equal
                   (S.of_list (peers |> List.map ~f:fst))
                   (S.of_list expected_peers) ) ;
               Deferred.unit )) )

  let command =
    Command.async_spec ~summary:"Simple use of Async Rpc_parallel V2"
      Command.Spec.(empty)
      main
end
