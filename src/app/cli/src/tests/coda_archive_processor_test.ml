open Core
open Async

let name = "coda-archive-processor-test"

let main () =
  let archive_address = Host_and_port.of_string "localhost:3086" in
  let postgres_address =
    Uri.of_string "postgres://admin:codarules@localhost:5432/archiver"
  in
  let precomputed_values = Lazy.force Precomputed_values.compiled_inputs in
  let constraint_constants = precomputed_values.constraint_constants in
  let%bind conn =
    match%map Caqti_async.connect postgres_address with
    | Ok conn ->
        conn
    | Error e ->
        failwith @@ Caqti_error.show e
  in
  let logger = Logger.create () in
  Archive_lib.Processor.setup_server ~metrics_server_port:None
    ~missing_blocks_width:None ~logger ~constraint_constants ~postgres_address
    ~server_port:(Host_and_port.port archive_address)
    ~delete_older_than:None
    ~runtime_config_opt:
      (Some (Lazy.force Runtime_config.Test_configs.transactions))
  |> don't_wait_for ;
  let public_key =
    Genesis_proof.Inputs.largest_account_pk_exn precomputed_values
  in
  let n = 2 in
  let block_production_keys i = if i = 0 then Some i else None in
  let snark_work_public_keys i = if i = 0 then Some public_key else None in
  let is_archive_rocksdb i = i = 1 in
  let archive_process_location i =
    if i = 1 then Some archive_address else None
  in
  let%bind testnet =
    Coda_worker_testnet.test ~name logger n block_production_keys
      snark_work_public_keys Cli_lib.Arg_type.Work_selection_method.Sequence
      ~max_concurrent_connections:None ~is_archive_rocksdb
      ~archive_process_location ~precomputed_values
  in
  let%bind new_block_pipe =
    let%map pipe = Coda_worker_testnet.Api.new_block testnet 1 public_key in
    (Option.value_exn pipe).pipe
  in
  let num_blocks_to_wait = 5 in
  let%bind _, observed_transitions =
    Pipe.fold new_block_pipe ~init:(0, []) ~f:(fun (i, acc) transition ->
        if i >= num_blocks_to_wait then Pipe.close_read new_block_pipe ;
        Deferred.return (i + 1, transition :: acc) )
  in
  let%bind () = after (Time.Span.of_sec 10.) in
  Deferred.List.iter observed_transitions
    ~f:(fun With_hash.{hash; data= transition} ->
      match%map
        let open Deferred.Result.Let_syntax in
        match%bind
          Archive_lib.Processor.Block.find_opt conn ~state_hash:hash
        with
        | Some id ->
            let%bind Archive_lib.Processor.Block.{parent_id; _} =
              Archive_lib.Processor.Block.load conn ~id
            in
            Archive_lib.Processor.For_test.assert_parent_exist conn ~parent_id
              ~parent_hash:
                transition.Filtered_external_transition.protocol_state
                  .previous_state_hash
        | None ->
            failwith "Failed to find saved block in database"
      with
      | Ok () ->
          ()
      | Error e ->
          failwith @@ Caqti_error.show e )

let command =
  Command.async
    ~summary:
      "Testing that an archive processor stores all blocks that it has seen"
    (Command.Param.return main)
