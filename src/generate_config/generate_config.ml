open StdLabels

type execution_kind = Dune | Generate

let execution_kind_to_string = function
  | Dune ->
      "dune"
  | Generate ->
      "generate"

let execution_kind = ref None

let set_execution_kind ?error_prefix kind =
  match (!execution_kind, kind) with
  | None, _ ->
      execution_kind := Some kind
  | Some Dune, Dune | Some Generate, Generate ->
      ()
  | Some execution_kind, _ ->
      let error_message =
        Format.sprintf "%acannot mix arguments for %s mode and %s mode"
          (fun _fmt error_prefix ->
            match error_prefix with
            | Some error_prefix ->
                error_prefix
            | None ->
                "" )
          error_prefix
          (execution_kind_to_string execution_kind)
          (execution_kind_to_string kind)
      in
      failwith error_message

let build_profile = ref None

let config_file_path = ref None

let dune_file_path = ref None

let args =
  [ ( "--profile"
    , Arg.String
        (fun profile ->
          match !build_profile with
          | Some _ ->
              failwith "Unexpected multiple --profile arguments"
          | None ->
              set_execution_kind Generate
                ~error_prefix:"Unexpected --profile argument: " ;
              build_profile := Some profile )
    , "The build profile to generate a configuration for. If omitted, \
       generates the dune dependency list for this executable." )
  ; ( "--config-file-path"
    , Arg.String
        (fun path ->
          match !config_file_path with
          | Some _ ->
              failwith "Unexpected multiple --config-file-path arguments"
          | None ->
              set_execution_kind Generate
                ~error_prefix:"Unexpected --config-file-path argument: " ;
              config_file_path := Some path )
    , "The path to write the config file to. If omitted, defaults to \
       config.mlh." )
  ; ( "--dune-file-path"
    , Arg.String
        (fun path ->
          match !dune_file_path with
          | Some _ ->
              failwith "Unexpected multiple --dune-file-path arguments"
          | None ->
              set_execution_kind Generate
                ~error_prefix:"Unexpected --dune-file-path argument: " ;
              dune_file_path := Some path )
    , "The path to write the dune file to. If omitted, defaults to \
       config_rule.sexp." ) ]

let value ~default x = match !x with Some x -> x | None -> default

let profile_env = "MINA_COMPILE_CONFIG"

type env_var_kind = Env_string | Env_int | Env_bool

(* If you modify this list, run
   [dune build src/generate_config/config_rule.sexp]
   to regenerate the dependencies for dune.
*)
let env_overrides =
  [ ("ACCOUNT_CREATION_FEE", Env_string, "account_creation_fee_int")
  ; ("DEFAULT_TXN_FEE", Env_string, "default_transaction_fee")
  ; ("MIN_TXN_FEE", Env_string, "minimum_user_command_fee")
  ; ("DEFAULT_SNARK_FEE", Env_string, "default_snark_worker_fee")
  ; ("COINBASE_AMOUNT", Env_string, "coinbase")
  ; ("CONSENSUS_K", Env_int, "k")
  ; ("CONSENSUS_DELTA", Env_int, "delta")
  ; ("BLOCK_WINDOW_DURATION", Env_int, "block_window_duration")
  ; ("SLOTS_PER_EPOCH", Env_int, "slots_per_epoch")
  ; ("SLOTS_PER_SUB_WINDOW", Env_int, "slots_per_sub_window")
  ; ("SUB_WINDOWS_PER_WINDOW", Env_int, "sub_windows_per_window")
  ; ("FEATURE_SNAPPS", Env_bool, "feature_snapps")
  ; ("FEATURE_TOKENS", Env_bool, "feature_tokens")
  ; ("FEATURE_PLUGINS", Env_bool, "plugins")
  ; ("FEATURE_MAINNET_SIGNATURES", Env_bool, "mainnet")
  ; ("FEATURE_TIME_OFFSETS", Env_bool, "time_offsets")
  ; ("FEATURE_INTEGRATION_TESTS", Env_bool, "integration_tests")
  ; ("FEATURE_FORCE_UPDATES", Env_bool, "force_updates")
  ; ("DOWNLOAD_SNARK_KEYS", Env_bool, "download_snark_keys")
  ; ("GENESIS_LEDGER", Env_string, "genesis_ledger")
  ; ("GENESIS_STATE_TIMESTAMP", Env_string, "genesis_state_timestamp")
  ; ("GENERATE_GENESIS_PROOF", Env_bool, "generate_genesis_proof")
  ; ("PRINT_VERSIONED_TYPES", Env_bool, "print_versioned_types")
  ; ("DAEMON_EXPIRY", Env_string, "daemon_expiry")
  ; ("INTEGRATION_TEST_FULL_EPOCH", Env_bool, "test_full_epoch")
  ; ("DEBUGGING_LOGS", Env_bool, "debug_logs")
  ; ("DEBUGGING_LOG_CALLS", Env_bool, "call_logger")
  ; ("DEBUGGING_TRACING", Env_bool, "tracing")
  ; ("DEBUGGING_CACHE_EXCEPTIONS", Env_bool, "cache_exceptions")
  ; ("DEBUGGING_ASYNC_BACKTRACES", Env_bool, "record_async_backtraces")
  ; ("LEDGER_DEPTH", Env_int, "ledger_depth")
  ; ("GC_COMPACT_INTERVAL", Env_int, "compaction_interval")
  ; ("PROTOCOL_VERSION", Env_string, "current_protocol_version")
  ; ("PROOF_LEVEL", Env_string, "proof_level") ]

let generate_dune path =
  let file = open_out path in
  let fmt = Format.formatter_of_out_channel file in
  Format.fprintf fmt
    "(rule\n\
    \ (targets config.mlh)\n\
    \ (mode fallback)\n\
    \ (deps\n\
    \  (source_tree ../config)%a)\n\
    \ (action (run %%{exe:generate_config.exe} --profile=%%{profile})))"
    (fun fmt env_overrides ->
      Format.fprintf fmt "\n  (env_var %s)" profile_env ;
      List.iter env_overrides ~f:(fun (env_var, _, _) ->
          Format.fprintf fmt "\n  (env_var %s)" env_var ) )
    env_overrides ;
  close_out file

let generate_config ~path ~profile () =
  let file = open_out path in
  let fmt = Format.formatter_of_out_channel file in
  let profile_path = Format.sprintf "../config/%s.mlh" profile in
  let profile_file = open_in profile_path in
  let rec copy_lines () =
    try
      let line = input_line profile_file in
      Format.fprintf fmt "%s@." line ;
      copy_lines ()
    with End_of_file -> ()
  in
  copy_lines () ;
  List.iter env_overrides ~f:(fun (env_var, env_var_kind, config_var) ->
      match Sys.getenv_opt env_var with
      | Some env_value ->
          let value =
            match env_var_kind with
            | Env_string ->
                Format.sprintf "\"%s\"" (String.escaped env_value)
            | Env_int -> (
              match int_of_string_opt env_value with
              | Some _ ->
                  env_value
              | None ->
                  let error_message =
                    Format.sprintf
                      "Could not parse environment variable %s as an int"
                      env_var
                  in
                  failwith error_message )
            | Env_bool -> (
              match bool_of_string_opt env_value with
              | Some _ ->
                  env_value
              | None ->
                  let error_message =
                    Format.sprintf
                      "Could not parse environment variable %s as an string"
                      env_var
                  in
                  failwith error_message )
          in
          Format.fprintf fmt "[%%%%define %s %s]@." config_var value
      | None ->
          () ) ;
  close_in profile_file ;
  close_out file

let () =
  Arg.parse args
    (fun _ -> failwith "Unexpected anonymous argument")
    "Generate a compile-time configuration file for the given profile, \
     including any overrides from the environment." ;
  let execution_kind = value ~default:Dune execution_kind in
  match execution_kind with
  | Dune ->
      let dune_file_path = value ~default:"config_rule.sexp" dune_file_path in
      generate_dune dune_file_path
  | Generate ->
      let build_profile =
        match Sys.getenv_opt profile_env with
        | Some profile ->
            profile
        | None ->
            value ~default:"dev" build_profile
      in
      let config_file_path = value ~default:"config.mlh" config_file_path in
      generate_config ~path:config_file_path ~profile:build_profile ()
