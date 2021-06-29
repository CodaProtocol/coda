open Core
open Async

let name = "coda-restart-node-test"

include Heartbeat.Make ()

let main () =
  let logger = Logger.create () in
  let precomputed_values = Lazy.force Precomputed_values.compiled_inputs in
  let largest_account_pk =
    Genesis_proof.Inputs.largest_account_pk_exn precomputed_values
  in
  Deferred.don't_wait_for (print_heartbeat logger) ;
  let n = 2 in
  let block_production_keys i = if i = 0 then Some i else None in
  let snark_work_public_keys i =
    if i = 0 then Some largest_account_pk else None
  in
  let%bind testnet =
    Coda_worker_testnet.test ~name logger n block_production_keys
      snark_work_public_keys Cli_lib.Arg_type.Work_selection_method.Sequence
      ~max_concurrent_connections:None ~precomputed_values
  in
  let%bind () =
    Coda_worker_testnet.Restarts.trigger_catchup testnet ~logger ~node:1
  in
  let%bind () = after (Time.Span.of_min 2.) in
  heartbeat_flag := false ;
  Coda_worker_testnet.Api.teardown testnet ~logger

let command =
  Command.async ~summary:"Test of stopping, waiting, then starting a node"
    (Command.Param.return main)
