open Core
open Async

include Heartbeat.Make ()

let name = "coda-shared-state-test"

let runtime_config = Runtime_config.Test_configs.transactions

let main () =
  let logger = Logger.create () in
  let%bind precomputed_values, _runtime_config =
    Genesis_ledger_helper.inputs_from_config_file ~logger ~proof_level:None
      (Lazy.force runtime_config)
    >>| Or_error.ok_exn
  in
  (* keep CI alive *)
  let n = 2 in
  let keypairs =
    List.map
      (Lazy.force (Genesis_proof.Inputs.accounts precomputed_values))
      ~f:Genesis_proof.Inputs.keypair_of_account_record_exn
  in
  let public_keys =
    List.map ~f:Genesis_proof.Inputs.pk_of_account_record
      (Lazy.force (Genesis_proof.Inputs.accounts precomputed_values))
  in
  let snark_work_public_keys i = Some (List.nth_exn public_keys i) in
  let%bind testnet =
    Coda_worker_testnet.test ~name logger n Option.some snark_work_public_keys
      Cli_lib.Arg_type.Work_selection_method.Sequence
      ~max_concurrent_connections:None ~precomputed_values
  in
  Deferred.don't_wait_for (print_heartbeat logger) ;
  let%bind () =
    Coda_worker_testnet.Payments.send_several_payments testnet ~node:0
      ~keypairs ~n:3
  in
  Coda_worker_testnet.Api.teardown testnet ~logger

let command =
  Command.async ~summary:"Test that workers share states"
    (Command.Param.return main)
