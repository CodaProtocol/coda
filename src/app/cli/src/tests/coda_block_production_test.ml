open Core
open Async

let name = "coda-block-production-test"

let runtime_config = Runtime_config.Test_configs.split_snarkless

let main () =
  let logger = Logger.create () in
  let%bind precomputed_values, _runtime_config =
    Genesis_ledger_helper.inputs_from_config_file ~logger ~proof_level:None
      (Lazy.force runtime_config)
    >>| Or_error.ok_exn
  in
  let n = 1 in
  let snark_work_public_keys _ = None in
  let%bind testnet =
    Coda_worker_testnet.test ~name logger n Option.some snark_work_public_keys
      Cli_lib.Arg_type.Work_selection_method.Sequence
      ~max_concurrent_connections:None ~precomputed_values
  in
  let%bind () = after (Time.Span.of_sec 30.) in
  Coda_worker_testnet.Api.teardown testnet ~logger

let command =
  Command.async ~summary:"Test that blocks get produced"
    (Command.Param.return main)
