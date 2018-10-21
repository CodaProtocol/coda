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
  module Coda_processes = Coda_processes.Make (Ledger_proof) (Kernel) (Coda)
  open Coda_processes

  module Coda_worker_testnet = Coda_worker_testnet.Make (Ledger_proof) (Kernel) (Coda)

  let name = "coda-shared-prefix-test"

  let main who_proposes proposal_interval () =
    let log = Logger.create () in
    let log = Logger.child log name in
    let n = 2 in
    let should_propose = fun i -> i = who_proposes in
    let snark_work_public_keys = fun i -> None in
    let%bind (api, finished) = 
      Coda_worker_testnet.test log n should_propose snark_work_public_keys
    in
    let%bind () = after (Time.Span.of_sec 30.) in
    finished

  let command =
    let open Command.Let_syntax in
    Command.async ~summary:"Test that workers share prefixes"
      (let%map_open who_proposes =
         flag "who-proposes" ~doc:"ID node number which will be proposing"
           (required int)
       and proposal_interval =
         flag "proposal-interval"
           ~doc:"MILLIS proposal interval in proof of sig" (optional int)
       in
       main who_proposes proposal_interval)
end
