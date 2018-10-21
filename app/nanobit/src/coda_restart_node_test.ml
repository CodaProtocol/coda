open Core
open Async
open Coda_worker
open Coda_main
open Nanobit_base

module Make
    (Ledger_proof : Ledger_proof_intf)
    (Kernel : Kernel_intf with type Ledger_proof.t = Ledger_proof.t)
    (Coda : Coda_intf.S with type ledger_proof = Ledger_proof.t) :
  Integration_test_intf.S =
struct
  module Coda_processes = Coda_processes.Make (Ledger_proof) (Kernel) (Coda)
  open Coda_processes
  module Coda_worker_testnet =
    Coda_worker_testnet.Make (Ledger_proof) (Kernel) (Coda)

  let name = "coda-restart-node-test"

  let main () =
    let log = Logger.create () in
    let log = Logger.child log name in
    let n = 2 in
    let should_propose i = i = 0 in
    let snark_work_public_keys i = None in
    let receiver_pk = Genesis_ledger.low_balance_pk in
    let sender_sk = Genesis_ledger.high_balance_sk in
    let send_amount = Currency.Amount.of_int 10 in
    let fee = Currency.Fee.of_int 0 in
    let%bind testnet =
      Coda_worker_testnet.test log n should_propose snark_work_public_keys
    in
    let%bind () = after (Time.Span.of_sec 5.) in
    let%bind () = Coda_worker_testnet.Api.stop testnet 1 in
    let%bind () = 
      Coda_worker_testnet.Api.send_transaction testnet 0 sender_sk
        receiver_pk send_amount fee
    in
    let%bind () = after (Time.Span.of_sec 5.) in
    let%bind () = Coda_worker_testnet.Api.start testnet 1 in
    let%map () = after (Time.Span.of_sec 30.) in
    ()

  let command =
    Command.async_spec ~summary:"Simple use of Async Rpc_parallel V2"
      Command.Spec.(empty)
      main
end
