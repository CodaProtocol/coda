open Core
open Async
open Mina_base

let name = "coda-five-nodes-test"

let main () =
  let logger = Logger.create () in
  let precomputed_values = Lazy.force Precomputed_values.compiled_inputs in
  let n = 5 in
  let snark_work_public_keys = function
    | 0 ->
        Some
          ( List.nth_exn
              (Lazy.force (Genesis_proof.Inputs.accounts precomputed_values))
              5
          |> snd |> Account.public_key )
    | _ ->
        None
  in
  let%bind testnet =
    Coda_worker_testnet.test ~name logger n Option.some snark_work_public_keys
      Cli_lib.Arg_type.Work_selection_method.Sequence
      ~max_concurrent_connections:None ~precomputed_values
  in
  let%bind () = after (Time.Span.of_min 10.) in
  Coda_worker_testnet.Api.teardown testnet ~logger

let command =
  Command.async ~summary:"Test that five nodes work"
    (Command.Param.return main)
