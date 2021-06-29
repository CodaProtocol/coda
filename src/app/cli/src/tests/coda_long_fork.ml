open Core
open Async

let name = "coda-long-fork"

let main n waiting_time () =
  let precomputed_values =
    (* TODO: Load for this specific test. *)
    Lazy.force Precomputed_values.compiled_inputs
  in
  let consensus_constants = precomputed_values.consensus_constants in
  let logger = Logger.create () in
  let public_keys =
    List.map
      (Lazy.force (Genesis_proof.Inputs.accounts precomputed_values))
      ~f:Genesis_proof.Inputs.pk_of_account_record
  in
  let snark_work_public_keys i = Some (List.nth_exn public_keys i) in
  let%bind testnet =
    Coda_worker_testnet.test ~name logger n Option.some snark_work_public_keys
      Cli_lib.Arg_type.Work_selection_method.Sequence
      ~max_concurrent_connections:None ~precomputed_values
  in
  let epoch_duration =
    let block_window_duration_ms =
      Block_time.Span.to_ms consensus_constants.block_window_duration_ms
      |> Int64.to_int_exn
    in
    block_window_duration_ms
    * Unsigned.UInt32.to_int consensus_constants.slots_per_epoch
  in
  let%bind () =
    Coda_worker_testnet.Restarts.restart_node testnet ~logger ~node:1
      ~duration:(Time.Span.of_ms (2 * epoch_duration |> Float.of_int))
  in
  let%bind () = after (Time.Span.of_sec (waiting_time |> Float.of_int)) in
  Coda_worker_testnet.Api.teardown testnet ~logger

let command =
  let open Command.Let_syntax in
  Command.async ~summary:"Test that one worker goes offline for a long time"
    (let%map_open num_block_producers =
       flag "--num-block-producers" ~aliases:["num-block-producers"]
         ~doc:"NUM number of block producers to have" (required int)
     and waiting_time =
       flag "--waiting-time" ~aliases:["waiting-time"]
         ~doc:"the waiting time after the nodes coming back alive"
         (optional_with_default 120 int)
     in
     main num_block_producers waiting_time)
