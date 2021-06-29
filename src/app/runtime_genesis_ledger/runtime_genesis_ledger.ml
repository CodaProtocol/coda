open Core
open Async
open Mina_base

type t = Ledger.t

let main ~config_file ~genesis_dir ~proof_level () =
  let%bind config =
    match config_file with
    | Some config_file -> (
        let%map config_json =
          Deferred.Or_error.ok_exn
          @@ Genesis_ledger_helper.load_config_json config_file
        in
        match Runtime_config.of_yojson config_json with
        | Ok config ->
            config
        | Error err ->
            failwithf "Could not parse configuration: %s" err () )
    | None ->
        return Runtime_config.default
  in
  Deferred.Or_error.ok_exn @@ Deferred.Or_error.ignore_m
  @@ Genesis_ledger_helper.init_from_config_file ?genesis_dir
       ~logger:(Logger.create ()) ~proof_level config

let () =
  Command.run
    (Command.async
       ~summary:
         "Generate the genesis ledger and genesis proof for a given \
          configuration file, or for the compile-time configuration if none \
          is provided"
       Command.(
         let open Let_syntax in
         let open Command.Param in
         let%map config_file =
           flag "--config-file" ~doc:"PATH path to the JSON configuration file"
             (optional string)
         and genesis_dir =
           flag "--genesis-dir"
             ~doc:
               (sprintf
                  "Dir where the genesis ledger and genesis proof is to be \
                   saved (default: %s)"
                  Cache_dir.autogen_path)
             (optional string)
         and proof_level =
           flag "--proof-level"
             (optional
                (Arg_type.create Genesis_constants.Proof_level.of_string))
             ~doc:"full|check|none"
         in
         main ~config_file ~genesis_dir ~proof_level))
