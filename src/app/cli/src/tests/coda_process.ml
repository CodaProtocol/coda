[%%import
"/src/config.mlh"]

open Core
open Async
open Pipe_lib

type t = Coda_worker.Connection.t * Process.t * Coda_worker.Input.t

let spawn_exn (config : Coda_worker.Input.t) =
  let%bind conn, process =
    Coda_worker.spawn_in_foreground_exn ~env:config.env ~on_failure:Error.raise
      ~cd:config.program_dir ~shutdown_on:Disconnect
      ~connection_state_init_arg:() ~connection_timeout:(Time.Span.of_sec 15.)
      config
  in
  File_system.dup_stdout process ;
  File_system.dup_stderr process ;
  return (conn, process, config)

let local_config ?block_production_interval:_ ~is_seed ~peers ~addrs_and_ports
    ~chain_id ~libp2p_keypair
    ~net_configs:(addrs_and_ports_list, all_peers_list) ~acceptable_delay
    ~program_dir ~block_production_key ~snark_worker_key ~work_selection_method
    ~offset ~trace_dir ~max_concurrent_connections ~is_archive_rocksdb
    ~archive_process_location ~runtime_config () =
  let conf_dir =
    match Sys.getenv "CODA_INTEGRATION_TEST_DIR" with
    | Some dir ->
        dir
        ^/ Network_peer.Peer.Id.to_string
             (Mina_net2.Keypair.to_peer_id libp2p_keypair)
    | None ->
        Filename.temp_dir_name
        ^/ String.init 16 ~f:(fun _ -> (Int.to_string (Random.int 10)).[0])
  in
  if Core.Sys.file_exists conf_dir <> `No then
    failwithf
      "cannot configure coda process because directory already exists: %s"
      conf_dir () ;
  let config =
    { Coda_worker.Input.addrs_and_ports
    ; libp2p_keypair
    ; net_configs=
        ( List.map
            ~f:(fun (na, kp) -> (Node_addrs_and_ports.to_display na, kp))
            addrs_and_ports_list
        , List.map
            ~f:(List.map ~f:Node_addrs_and_ports.to_display)
            all_peers_list )
    ; env=
        ( "CODA_TIME_OFFSET"
        , Time.Span.to_int63_seconds_round_down_exn offset
          |> Int63.to_int
          |> Option.value_exn ?here:None ?message:None ?error:None
          |> Int.to_string )
        :: ( Core.Unix.environment () |> Array.to_list
           |> List.filter_map
                ~f:
                  (Fn.compose
                     (function [a; b] -> Some (a, b) | _ -> None)
                     (String.split ~on:'=')) )
    ; block_production_key
    ; snark_worker_key
    ; work_selection_method
    ; conf_dir
    ; chain_id
    ; peers
    ; trace_dir
    ; program_dir
    ; acceptable_delay
    ; is_archive_rocksdb
    ; is_seed
    ; archive_process_location
    ; max_concurrent_connections
    ; runtime_config }
  in
  config

let peers_exn (conn, _, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.peers ~arg:()

let get_balance_exn (conn, _, _) pk =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.get_balance
    ~arg:pk

let get_nonce_exn (conn, _, _) pk =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.get_nonce
    ~arg:pk

let root_length_exn (conn, _, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.root_length
    ~arg:()

let send_user_command_exn (conn, _, _) sk pk amount fee memo =
  Coda_worker.Connection.run_exn conn
    ~f:Coda_worker.functions.send_user_command
    ~arg:(sk, pk, amount, fee, memo)

let process_user_command_exn (conn, _, _) cmd =
  Coda_worker.Connection.run_exn conn
    ~f:Coda_worker.functions.process_user_command ~arg:cmd

let sync_status_exn (conn, _, _) =
  let%map r =
    Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.sync_status
      ~arg:()
  in
  Linear_pipe.wrap_reader r

let verified_transitions_exn (conn, _, _) =
  let%map r =
    Coda_worker.Connection.run_exn conn
      ~f:Coda_worker.functions.verified_transitions ~arg:()
  in
  Linear_pipe.wrap_reader r

(* TODO: 2836 delete once transition_frontier extensions refactoring gets in *)
let validated_transitions_keyswaptest_exn (conn, _, _) =
  let%map r =
    Coda_worker.Connection.run_exn conn
      ~f:Coda_worker.functions.validated_transitions_keyswaptest ~arg:()
  in
  Linear_pipe.wrap_reader r

let new_block_exn (conn, _, _) key =
  let%map r =
    Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.new_block
      ~arg:key
  in
  Linear_pipe.wrap_reader r

let root_diff_exn (conn, _, _) =
  let%map r =
    Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.root_diff
      ~arg:()
  in
  Linear_pipe.wrap_reader r

let initialization_finish_signal_exn (conn, _, _) =
  let%map p =
    Coda_worker.Connection.run_exn conn
      ~f:Coda_worker.functions.initialization_finish_signal ~arg:()
  in
  Linear_pipe.wrap_reader p

let start_exn (conn, _, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.start ~arg:()

let new_user_command_exn (conn, _, _) pk =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.new_user_command
    ~arg:pk

let dump_tf (conn, _, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.dump_tf ~arg:()

let best_path (conn, _, _) =
  Coda_worker.Connection.run_exn conn ~f:Coda_worker.functions.best_path
    ~arg:()

let replace_snark_worker_key (conn, _, _) key =
  Coda_worker.Connection.run_exn conn
    ~f:Coda_worker.functions.replace_snark_worker_key ~arg:key

let stop_snark_worker (conn, _, _) =
  Coda_worker.Connection.run_exn conn
    ~f:Coda_worker.functions.stop_snark_worker ~arg:()

let disconnect ((conn, proc, _) as t) ~logger =
  Child_processes.Termination.wait_for_process_log_errors ~logger proc
    ~module_:__MODULE__ ~location:__LOC__ ;
  (* This kills any straggling snark worker process *)
  let%bind () =
    match%map
      Monitor.try_with ~here:[%here] (fun () -> stop_snark_worker t)
    with
    | Ok () ->
        ()
    | Error exn ->
        [%log info] "Harmless error when stopping snark worker: $exn"
          ~metadata:[("exn", `String (Exn.to_string exn))]
  in
  let%bind () = Coda_worker.Connection.close conn in
  match%map Monitor.try_with ~here:[%here] (fun () -> Process.wait proc) with
  | Ok _ ->
      ()
  | Error e ->
      [%log info]
        ~metadata:[("e", `String (Exn.to_string e))]
        "Harmless error when stopping test node: $exn"
