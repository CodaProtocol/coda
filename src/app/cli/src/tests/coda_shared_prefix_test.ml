open Core
open Async

let name = "coda-shared-prefix-test"

let runtime_config = Runtime_config.Test_configs.split_snarkless

let main who_produces () =
  let logger = Logger.create () in
  let%bind precomputed_values, _runtime_config =
    Genesis_ledger_helper.inputs_from_config_file ~logger ~proof_level:None
      (Lazy.force runtime_config)
    >>| Or_error.ok_exn
  in
  let n = 2 in
  let block_producers i = if i = who_produces then Some i else None in
  let snark_work_public_keys _ = None in
  let%bind testnet =
    Coda_worker_testnet.test ~name logger n block_producers
      snark_work_public_keys Cli_lib.Arg_type.Work_selection_method.Sequence
      ~max_concurrent_connections:None ~precomputed_values
  in
  let%bind () = after (Time.Span.of_sec 60.) in
  Coda_worker_testnet.Api.teardown testnet ~logger

let command =
  let open Command.Let_syntax in
  Command.async ~summary:"Test that workers share prefixes"
    (let%map_open who_produces =
       flag "--who-produces" ~aliases:["who-produces"]
         ~doc:"ID node number which will be producing blocks" (required int)
     in
     main who_produces)
