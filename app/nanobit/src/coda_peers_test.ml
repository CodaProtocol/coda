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
    let n = 1 in
    let log = Logger.create () in
    let log = Logger.child log name in
    Coda_processes.init () ;
    Logger.info log "A";
    let configs = Coda_processes.local_configs  n ~program_dir
        ~snark_worker_public_keys:None ~should_propose:(Fn.const false)
    in
    let config = List.hd_exn configs in
    Logger.info log "B";
    let%bind worker = Coda_process.spawn_exn config in
    Logger.info log "C";
    after (Time.Span.of_sec 10.)
    (*
    let%bind program_dir = Unix.getcwd () in
    let n = 3 in
    let log = Logger.create () in
    let log = Logger.child log name in
    Coda_processes.init () ;
    let configs = Coda_processes.local_configs  n ~program_dir
        ~snark_worker_public_keys:None ~should_propose:(Fn.const false)
    in
    let%bind workers = Coda_processes.spawn_local_processes_exn configs in
    let _, _, expected_peers = Coda_processes.net_configs n in
    let%bind _ = after (Time.Span.of_sec 10.) in
    Deferred.all_unit
      (List.map2_exn workers expected_peers ~f:
         (fun worker expected_peers ->
            let%map peers = Coda_process.peers_exn worker in
            Logger.debug log
              !"got peers %{sexp: Kademlia.Peer.t list} %{sexp: \
                Host_and_port.t list}\n"
              peers expected_peers ;
            let module S = Host_and_port.Set in
            assert (
              S.equal
                (S.of_list (peers |> List.map ~f:fst))
                (S.of_list expected_peers) )))*)

  let command =
    Command.async_spec ~summary:"Simple use of Async Rpc_parallel V2"
      Command.Spec.(empty)
      main
end
