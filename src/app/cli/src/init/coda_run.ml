open Core
open Async
open Signature_lib
open O1trace
module Graphql_cohttp_async =
  Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)

let snark_job_list_json t =
  let open Participating_state.Let_syntax in
  let%map sl = Mina_lib.best_staged_ledger t in
  Staged_ledger.Scan_state.snark_job_list_json (Staged_ledger.scan_state sl)

let snark_pool_list t =
  Mina_lib.snark_pool t |> Network_pool.Snark_pool.resource_pool
  |> Network_pool.Snark_pool.Resource_pool.snark_pool_json
  |> Yojson.Safe.to_string

(* create reader, writer for protocol versions, but really for any one-line item in conf_dir *)
let make_conf_dir_item_io ~conf_dir ~filename =
  let item_file = conf_dir ^/ filename in
  let read_item () =
    let open Stdlib in
    let inp = open_in item_file in
    let res = input_line inp in
    close_in inp ; res
  in
  let write_item item =
    let open Stdlib in
    let outp = open_out item_file in
    output_string outp (item ^ "\n") ;
    close_out outp
  in
  (read_item, write_item)

let get_current_protocol_version ~compile_time_current_protocol_version
    ~conf_dir ~logger =
  let read_protocol_version, write_protocol_version =
    make_conf_dir_item_io ~conf_dir ~filename:"current_protocol_version"
  in
  function
  | None -> (
    try
      (* not provided on command line, try to read from config dir *)
      let protocol_version = read_protocol_version () in
      [%log info]
        "Setting current protocol version to $protocol_version from config"
        ~metadata:[("protocol_version", `String protocol_version)] ;
      Protocol_version.of_string_exn protocol_version
    with Sys_error _ ->
      (* not on command-line, not in config dir, use compile-time value *)
      [%log info]
        "Setting current protocol version to $protocol_version from \
         compile-time config"
        ~metadata:
          [("protocol_version", `String compile_time_current_protocol_version)] ;
      Protocol_version.of_string_exn compile_time_current_protocol_version )
  | Some protocol_version -> (
    try
      (* it's an error if the command line value disagrees with the value in the config *)
      let config_protocol_version = read_protocol_version () in
      if String.equal config_protocol_version protocol_version then (
        [%log info]
          "Using current protocol version $protocol_version from command \
           line, which matches the one in the config"
          ~metadata:[("protocol_version", `String protocol_version)] ;
        Protocol_version.of_string_exn config_protocol_version )
      else (
        [%log fatal]
          "Current protocol version $protocol_version from the command line \
           disagrees with $config_protocol_version from the Coda config"
          ~metadata:
            [ ("protocol_version", `String protocol_version)
            ; ("config_protocol_version", `String config_protocol_version) ] ;
        failwith
          "Current protocol version from command line disagrees with protocol \
           version in Coda config; please delete your Coda config if you wish \
           to use a new protocol version" )
    with Sys_error _ -> (
      (* use value provided on command line, write to config dir *)
      match Protocol_version.of_string_opt protocol_version with
      | None ->
          [%log fatal] "Protocol version provided on command line is invalid"
            ~metadata:[("protocol_version", `String protocol_version)] ;
          failwith "Protocol version from command line is invalid"
      | Some pv ->
          write_protocol_version protocol_version ;
          [%log info]
            "Using current protocol_version $protocol_version from command \
             line, writing to config"
            ~metadata:[("protocol_version", `String protocol_version)] ;
          pv ) )

let get_proposed_protocol_version_opt ~conf_dir ~logger =
  let read_protocol_version, write_protocol_version =
    make_conf_dir_item_io ~conf_dir ~filename:"proposed_protocol_version"
  in
  function
  | None -> (
    try
      (* not provided on command line, try to read from config dir *)
      let protocol_version = read_protocol_version () in
      [%log info]
        "Setting proposed protocol version to $protocol_version from config"
        ~metadata:[("protocol_version", `String protocol_version)] ;
      Some (Protocol_version.of_string_exn protocol_version)
    with Sys_error _ ->
      (* not on command-line, not in config dir, there's no proposed protocol version *)
      None )
  | Some protocol_version -> (
      let validate_cli_protocol_version protocol_version =
        if Option.is_none (Protocol_version.of_string_opt protocol_version)
        then (
          [%log fatal]
            "Proposed protocol version provided on command line is invalid"
            ~metadata:[("proposed_protocol_version", `String protocol_version)] ;
          failwith "Proposed protocol version from command line is invalid" )
      in
      try
        (* overwrite if the command line value disagrees with the value in the config *)
        let config_protocol_version = read_protocol_version () in
        if String.equal config_protocol_version protocol_version then (
          [%log info]
            "Using proposed protocol version $protocol_version from command \
             line, which matches the one in the config"
            ~metadata:[("protocol_version", `String protocol_version)] ;
          Some (Protocol_version.of_string_exn config_protocol_version) )
        else (
          validate_cli_protocol_version protocol_version ;
          write_protocol_version protocol_version ;
          [%log info]
            "Overwriting Coda config proposed protocol version \
             $config_proposed_protocol_version with proposed protocol version \
             $protocol_version from the command line"
            ~metadata:
              [ ( "config_proposed_protocol_version"
                , `String config_protocol_version )
              ; ("proposed_protocol_version", `String protocol_version) ] ;
          Some (Protocol_version.of_string_exn protocol_version) )
      with Sys_error _ ->
        (* use value provided on command line, write to config dir *)
        validate_cli_protocol_version protocol_version ;
        write_protocol_version protocol_version ;
        [%log info]
          "Using proposed protocol version from command line, writing to config"
          ~metadata:[("protocol_version", `String protocol_version)] ;
        Some (Protocol_version.of_string_exn protocol_version) )

(*TODO check deferred now and copy theose files to the temp directory*)
let log_shutdown ~conf_dir ~top_logger coda_ref =
  let logger =
    Logger.extend top_logger
      [("coda_run", `String "Logging state before program ends")]
  in
  let frontier_file = conf_dir ^/ "frontier.dot" in
  let mask_file = conf_dir ^/ "registered_masks.dot" in
  (* ledger visualization *)
  [%log debug] "%s"
    (Visualization_message.success "registered masks" mask_file) ;
  Mina_base.Ledger.Debug.visualize ~filename:mask_file ;
  match !coda_ref with
  | None ->
      [%log warn]
        "Shutdown before Coda instance was created, not saving a visualization"
  | Some t -> (
    (*Transition frontier visualization*)
    match Mina_lib.visualize_frontier ~filename:frontier_file t with
    | `Active () ->
        [%log debug] "%s"
          (Visualization_message.success "transition frontier" frontier_file)
    | `Bootstrapping ->
        [%log debug] "%s"
          (Visualization_message.bootstrap "transition frontier") )

let remove_prev_crash_reports ~conf_dir =
  Core.Sys.command (sprintf "rm -rf %s/coda_crash_report*" conf_dir)

let summary exn_json =
  let uname = Core.Unix.uname () in
  let daemon_command =
    sprintf !"Command: %{sexp: string array}" (Sys.get_argv ())
  in
  `Assoc
    [ ("OS_type", `String Sys.os_type)
    ; ("Release", `String (Core.Unix.Utsname.release uname))
    ; ("Machine", `String (Core.Unix.Utsname.machine uname))
    ; ("Sys_name", `String (Core.Unix.Utsname.sysname uname))
    ; ("Exception", exn_json)
    ; ("Command", `String daemon_command)
    ; ("Coda_branch", `String Mina_version.branch)
    ; ("Coda_commit", `String Mina_version.commit_id) ]

let coda_status coda_ref =
  Option.value_map coda_ref
    ~default:
      (Deferred.return (`String "Shutdown before Coda instance was created"))
    ~f:(fun t ->
      Mina_commands.get_status ~flag:`Performance t
      >>| Daemon_rpcs.Types.Status.to_yojson )

let make_report exn_json ~conf_dir ~top_logger coda_ref =
  (* TEMP MAKE REPORT TRACE *)
  [%log' trace top_logger] "make_report: enter" ;
  ignore (remove_prev_crash_reports ~conf_dir : int) ;
  let crash_time = Time.to_filename_string ~zone:Time.Zone.utc (Time.now ()) in
  let temp_config = conf_dir ^/ "coda_crash_report_" ^ crash_time in
  let () = Core.Unix.mkdir temp_config in
  (*Transition frontier and ledger visualization*)
  log_shutdown ~conf_dir:temp_config ~top_logger coda_ref ;
  let report_file = temp_config ^ ".tar.gz" in
  (*Coda status*)
  let status_file = temp_config ^/ "coda_status.json" in
  let%map status = coda_status !coda_ref in
  Yojson.Safe.to_file status_file status ;
  (* TEMP MAKE REPORT TRACE *)
  [%log' trace top_logger] "make_report: acquired and wrote status" ;
  (*coda logs*)
  let coda_log = conf_dir ^/ "mina.log" in
  let () =
    match Core.Sys.file_exists coda_log with
    | `Yes ->
        let coda_short_log = temp_config ^/ "coda_short.log" in
        (*get the last 4MB of the log*)
        let log_size = 4 * 1024 * 1024 |> Int64.of_int in
        let log =
          In_channel.with_file coda_log ~f:(fun in_chan ->
              let len = In_channel.length in_chan in
              In_channel.seek in_chan
                Int64.(max 0L (Int64.( + ) len (Int64.neg log_size))) ;
              In_channel.input_all in_chan )
        in
        Out_channel.write_all coda_short_log ~data:log
    | _ ->
        ()
  in
  (*System info/crash summary*)
  let summary = summary exn_json in
  Yojson.Safe.to_file (temp_config ^/ "crash_summary.json") summary ;
  (*copy daemon_json to the temp dir *)
  let daemon_config = conf_dir ^/ "daemon.json" in
  let eq = [%equal: [`Yes | `Unknown | `No]] in
  let () =
    if eq (Core.Sys.file_exists daemon_config) `Yes then
      ignore
        ( Core.Sys.command
            (sprintf "cp %s %s" daemon_config (temp_config ^/ "daemon.json"))
          : int )
  in
  (*Zip them all up*)
  let tmp_files =
    [ "coda_short.log"
    ; "registered_mask.dot"
    ; "frontier.dot"
    ; "coda_status.json"
    ; "crash_summary.json"
    ; "daemon.json" ]
    |> List.filter ~f:(fun f ->
           eq (Core.Sys.file_exists (temp_config ^/ f)) `Yes )
  in
  let files = tmp_files |> String.concat ~sep:" " in
  let tar_command =
    sprintf "tar  -C %s -czf %s %s" temp_config report_file files
  in
  let exit = Core.Sys.command tar_command in
  if exit = 2 then (
    [%log' fatal top_logger] "Error making the crash report. Exit code: %d"
      exit ;
    None )
  else Some (report_file, temp_config)

(* TODO: handle participation_status more appropriately than doing participate_exn *)
let setup_local_server ?(client_trustlist = []) ?rest_server_port
    ?limited_graphql_port ?(open_limited_graphql_port = false)
    ?(insecure_rest_server = false) coda =
  let client_trustlist =
    ref
      (Unix.Cidr.Set.of_list
         ( Unix.Cidr.create ~base_address:Unix.Inet_addr.localhost ~bits:8
         :: client_trustlist ))
  in
  (* Setup RPC server for client interactions *)
  let implement rpc f =
    Rpc.Rpc.implement rpc (fun () input ->
        trace_recurring (Rpc.Rpc.name rpc) (fun () -> f () input) )
  in
  let implement_notrace = Rpc.Rpc.implement in
  let logger =
    Logger.extend
      (Mina_lib.top_level_logger coda)
      [("coda_run", `String "Setting up server logs")]
  in
  let client_impls =
    [ implement Daemon_rpcs.Send_user_commands.rpc (fun () ts ->
          Deferred.map
            ( Mina_commands.setup_and_submit_user_commands coda ts
            |> Participating_state.to_deferred_or_error )
            ~f:Or_error.join )
    ; implement Daemon_rpcs.Get_balance.rpc (fun () aid ->
          return
            ( Mina_commands.get_balance coda aid
            |> Participating_state.active_error ) )
    ; implement Daemon_rpcs.Get_trust_status.rpc (fun () ip_address ->
          return (Mina_commands.get_trust_status coda ip_address) )
    ; implement Daemon_rpcs.Get_trust_status_all.rpc (fun () () ->
          return (Mina_commands.get_trust_status_all coda) )
    ; implement Daemon_rpcs.Reset_trust_status.rpc (fun () ip_address ->
          return (Mina_commands.reset_trust_status coda ip_address) )
    ; implement Daemon_rpcs.Chain_id_inputs.rpc (fun () () ->
          return (Mina_commands.chain_id_inputs coda) )
    ; implement Daemon_rpcs.Verify_proof.rpc (fun () (aid, tx, proof) ->
          return
            ( Mina_commands.verify_payment coda aid tx proof
            |> Participating_state.active_error |> Or_error.join ) )
    ; implement Daemon_rpcs.Get_public_keys_with_details.rpc (fun () () ->
          return
            ( Mina_commands.get_keys_with_details coda
            |> Participating_state.active_error ) )
    ; implement Daemon_rpcs.Get_public_keys.rpc (fun () () ->
          return
            ( Mina_commands.get_public_keys coda
            |> Participating_state.active_error ) )
    ; implement Daemon_rpcs.Get_nonce.rpc (fun () aid ->
          return
            ( Mina_commands.get_nonce coda aid
            |> Participating_state.active_error ) )
    ; implement Daemon_rpcs.Get_inferred_nonce.rpc (fun () aid ->
          return
            ( Mina_lib.get_inferred_nonce_from_transaction_pool_and_ledger coda
                aid
            |> Participating_state.active_error ) )
    ; implement_notrace Daemon_rpcs.Get_status.rpc (fun () flag ->
          Mina_commands.get_status ~flag coda )
    ; implement Daemon_rpcs.Clear_hist_status.rpc (fun () flag ->
          Mina_commands.clear_hist_status ~flag coda )
    ; implement Daemon_rpcs.Get_ledger.rpc (fun () lh ->
          (* getting the ledger may take more time than a heartbeat timeout
             run in thread to allow RPC heartbeats to proceed
          *)
          Async.In_thread.run (fun () -> Mina_lib.get_ledger coda lh) )
    ; implement Daemon_rpcs.Get_snarked_ledger.rpc (fun () lh ->
          Mina_lib.get_snarked_ledger coda lh |> return )
    ; implement Daemon_rpcs.Get_staking_ledger.rpc (fun () which ->
          ( match which with
          | Next ->
              Option.value_map (Mina_lib.next_epoch_ledger coda)
                ~default:
                  (Or_error.error_string "next staking ledger not available")
                ~f:(function
                | `Finalized ledger ->
                    Ok ledger
                | `Notfinalized ->
                    Or_error.error_string
                      "next staking ledger is not finalized yet" )
          | Current ->
              Option.value_map
                (Mina_lib.staking_ledger coda)
                ~default:
                  (Or_error.error_string "current staking ledger not available")
                ~f:Or_error.return )
          |> Or_error.map ~f:(function
               | Genesis_epoch_ledger l ->
                   Mina_base.Ledger.to_list l
               | Ledger_db db ->
                   Mina_base.Ledger.Db.to_list db )
          |> Deferred.return )
    ; implement Daemon_rpcs.Stop_daemon.rpc (fun () () ->
          Scheduler.yield () >>= (fun () -> exit 0) |> don't_wait_for ;
          Deferred.unit )
    ; implement Daemon_rpcs.Snark_job_list.rpc (fun () () ->
          return (snark_job_list_json coda |> Participating_state.active_error)
      )
    ; implement Daemon_rpcs.Snark_pool_list.rpc (fun () () ->
          return (snark_pool_list coda) )
    ; implement Daemon_rpcs.Start_tracing.rpc (fun () () ->
          let open Mina_lib.Config in
          Coda_tracing.start (Mina_lib.config coda).conf_dir )
    ; implement Daemon_rpcs.Stop_tracing.rpc (fun () () ->
          Coda_tracing.stop () ; Deferred.unit )
    ; implement Daemon_rpcs.Visualization.Frontier.rpc (fun () filename ->
          return (Mina_lib.visualize_frontier ~filename coda) )
    ; implement Daemon_rpcs.Visualization.Registered_masks.rpc
        (fun () filename -> return (Mina_base.Ledger.Debug.visualize ~filename)
      )
    ; implement Daemon_rpcs.Set_staking.rpc (fun () keypairs ->
          let keypair_and_compressed_key =
            List.map keypairs
              ~f:(fun ({Keypair.Stable.Latest.public_key; _} as keypair) ->
                (keypair, Public_key.compress public_key) )
          in
          Mina_lib.replace_block_production_keypairs coda
            (Keypair.And_compressed_pk.Set.of_list keypair_and_compressed_key) ;
          Deferred.unit )
    ; implement Daemon_rpcs.Add_trustlist.rpc (fun () cidr ->
          return
            (let cidr_str = Unix.Cidr.to_string cidr in
             if Unix.Cidr.Set.mem !client_trustlist cidr then
               Or_error.errorf "%s already present in trustlist" cidr_str
             else (
               client_trustlist := Unix.Cidr.Set.add !client_trustlist cidr ;
               Ok () )) )
    ; implement Daemon_rpcs.Remove_trustlist.rpc (fun () cidr ->
          return
            (let cidr_str = Unix.Cidr.to_string cidr in
             if not @@ Unix.Cidr.Set.mem !client_trustlist cidr then
               Or_error.errorf "%s not present in trustlist" cidr_str
             else (
               client_trustlist := Unix.Cidr.Set.remove !client_trustlist cidr ;
               Ok () )) )
    ; implement Daemon_rpcs.Get_trustlist.rpc (fun () () ->
          return (Set.to_list !client_trustlist) )
    ; implement Daemon_rpcs.Get_node_status.rpc (fun () peers ->
          Node_status.get_node_status_from_peers (Mina_lib.net coda) peers )
    ; implement Daemon_rpcs.Get_object_lifetime_statistics.rpc (fun () () ->
          return
            (Yojson.Safe.pretty_to_string @@ Allocation_functor.Table.dump ())
      ) ]
  in
  let snark_worker_impls =
    [ implement Snark_worker.Rpcs_versioned.Get_work.Latest.rpc (fun () () ->
          Deferred.return
            (let open Option.Let_syntax in
            let%bind key =
              Option.merge
                (Mina_lib.snark_worker_key coda)
                (Mina_lib.snark_coordinator_key coda)
                ~f:Fn.const
            in
            let%map r = Mina_lib.request_work coda key in
            [%log trace]
              ~metadata:[("work_spec", Snark_worker.Work.Spec.to_yojson r)]
              "responding to a Get_work request with some new work" ;
            Mina_metrics.(Counter.inc_one Snark_work.snark_work_assigned_rpc) ;
            (r, key)) )
    ; implement Snark_worker.Rpcs_versioned.Submit_work.Latest.rpc
        (fun () (work : Snark_worker.Work.Result.t) ->
          Mina_metrics.(
            Counter.inc_one Snark_work.completed_snark_work_received_rpc) ;
          [%log trace] "received completed work from a snark worker"
            ~metadata:
              [("work_spec", Snark_worker.Work.Spec.to_yojson work.spec)] ;
          One_or_two.iter work.metrics ~f:(fun (total, tag) ->
              match tag with
              | `Merge ->
                  Perf_histograms.add_span ~name:"snark_worker_merge_time"
                    total
              | `Transition ->
                  Perf_histograms.add_span ~name:"snark_worker_transition_time"
                    total ) ;
          Deferred.return @@ Mina_lib.add_work coda work ) ]
  in
  let create_graphql_server ~bind_to_address ~schema ~server_description port =
    let graphql_callback =
      Graphql_cohttp_async.make_callback (fun _req -> coda) schema
    in
    Cohttp_async.(
      Server.create_expert
        ~on_handler_error:
          (`Call
            (fun _net exn ->
              [%log error]
                "Exception while handling REST server request: $error"
                ~metadata:
                  [ ("error", `String (Exn.to_string_mach exn))
                  ; ("context", `String "rest_server") ] ))
        (Tcp.Where_to_listen.bind_to bind_to_address (On_port port))
        (fun ~body _sock req ->
          let uri = Cohttp.Request.uri req in
          let status flag =
            let%bind status = Mina_commands.get_status ~flag coda in
            Server.respond_string
              ( status |> Daemon_rpcs.Types.Status.to_yojson
              |> Yojson.Safe.pretty_to_string )
          in
          let lift x = `Response x in
          match Uri.path uri with
          | "/" ->
              let body =
                "This page is intentionally left blank. The graphql endpoint \
                 can be found at `/graphql`."
              in
              Server.respond_string ~status:`OK body >>| lift
          | "/graphql" ->
              [%log debug] "Received graphql request. Uri: $uri"
                ~metadata:
                  [ ("uri", `String (Uri.to_string uri))
                  ; ("context", `String "rest_server") ] ;
              graphql_callback () req body
          | "/status" ->
              status `None >>| lift
          | "/status/performance" ->
              status `Performance >>| lift
          | _ ->
              Server.respond_string ~status:`Not_found "Route not found"
              >>| lift ))
    |> Deferred.map ~f:(fun _ ->
           [%log info]
             !"Created %s at: http://localhost:%i/graphql"
             server_description port )
  in
  Option.iter rest_server_port ~f:(fun rest_server_port ->
      trace_task "REST server" (fun () ->
          create_graphql_server
            ~bind_to_address:
              Tcp.Bind_to_address.(
                if insecure_rest_server then All_addresses else Localhost)
            ~schema:Mina_graphql.schema ~server_description:"GraphQL server"
            rest_server_port ) ) ;
  (*Second graphql server with limited queries exopsed*)
  Option.iter limited_graphql_port ~f:(fun rest_server_port ->
      trace_task "Second REST server (with limited queries)" (fun () ->
          create_graphql_server
            ~bind_to_address:
              Tcp.Bind_to_address.(
                if open_limited_graphql_port then All_addresses else Localhost)
            ~schema:Mina_graphql.schema_limited
            ~server_description:"GraphQL server with limited queries"
            rest_server_port ) ) ;
  let where_to_listen =
    Tcp.Where_to_listen.bind_to All_addresses
      (On_port (Mina_lib.client_port coda))
  in
  don't_wait_for
    (Deferred.ignore_m
       (trace "client RPC handling" (fun () ->
            Tcp.Server.create
              ~on_handler_error:
                (`Call
                  (fun _net exn ->
                    [%log error]
                      "Exception while handling TCP server request: $error"
                      ~metadata:
                        [ ("error", `String (Exn.to_string_mach exn))
                        ; ("context", `String "rpc_tcp_server") ] ))
              where_to_listen
              (fun address reader writer ->
                let address = Socket.Address.Inet.addr address in
                if
                  not
                    (Set.exists !client_trustlist ~f:(fun cidr ->
                         Unix.Cidr.does_match cidr address ))
                then (
                  [%log error]
                    !"Rejecting client connection from $address, it is not \
                      present in the trustlist."
                    ~metadata:
                      [("$address", `String (Unix.Inet_addr.to_string address))] ;
                  Deferred.unit )
                else
                  Rpc.Connection.server_with_close
                    ~handshake_timeout:
                      (Time.Span.of_sec
                         Mina_compile_config.rpc_handshake_timeout_sec)
                    ~heartbeat_config:
                      (Rpc.Connection.Heartbeat_config.create
                         ~timeout:
                           (Time_ns.Span.of_sec
                              Mina_compile_config.rpc_heartbeat_timeout_sec)
                         ~send_every:
                           (Time_ns.Span.of_sec
                              Mina_compile_config.rpc_heartbeat_send_every_sec)
                         ())
                    reader writer
                    ~implementations:
                      (Rpc.Implementations.create_exn
                         ~implementations:(client_impls @ snark_worker_impls)
                         ~on_unknown_rpc:`Raise)
                    ~connection_state:(fun _ -> ())
                    ~on_handshake_error:
                      (`Call
                        (fun exn ->
                          [%log warn]
                            "Handshake error while handling RPC server \
                             request from $address"
                            ~metadata:
                              [ ("error", `String (Exn.to_string_mach exn))
                              ; ("context", `String "rpc_server")
                              ; ( "address"
                                , `String (Unix.Inet_addr.to_string address) )
                              ] ;
                          Deferred.unit )) ) )))

let coda_crash_message ~log_issue ~action ~error =
  let followup =
    if log_issue then
      sprintf
        !{err| The Mina Protocol developers would like to know why!

    Please:
      Open an issue:
        <https://github.com/MinaProtocol/mina/issues/new>

      Briefly describe what you were doing and %s

    %!|err}
        action
    else action
  in
  sprintf !{err|

  ☠  Mina Daemon %s.
  %s
%!|err} error followup

let no_report exn_json status =
  sprintf
    "include the last 20 lines from .mina-config/mina.log and then paste the \
     following:\n\
     Summary:\n\
     %s\n\
     Status:\n\
     %s\n"
    (Yojson.Safe.to_string status)
    (Yojson.Safe.to_string (summary exn_json))

let handle_crash e ~time_controller ~conf_dir ~child_pids ~top_logger coda_ref
    =
  (* attempt to free up some memory before handling crash *)
  (* this circumvents using Child_processes.kill, and instead sends SIGKILL to all children *)
  Hashtbl.keys child_pids
  |> List.iter ~f:(fun pid ->
         ignore (Signal.send Signal.kill (`Pid pid) : [`No_such_process | `Ok])
     ) ;
  let exn_json = Error_json.error_to_yojson (Error.of_exn ~backtrace:`Get e) in
  [%log' fatal top_logger]
    "Unhandled top-level exception: $exn\nGenerating crash report"
    ~metadata:[("exn", exn_json)] ;
  let%bind status = coda_status !coda_ref in
  (* TEMP MAKE REPORT TRACE *)
  [%log' trace top_logger] "handle_crash: acquired coda status" ;
  let%map action_string =
    match%map
      Block_time.Timeout.await
        ~timeout_duration:(Block_time.Span.of_ms 30_000L)
        time_controller
        ( try
            make_report exn_json ~conf_dir coda_ref ~top_logger
            >>| fun k -> Ok k
          with exn -> return (Error (Error.of_exn exn)) )
    with
    | `Ok (Ok (Some (report_file, temp_config))) ->
        ( try ignore (Core.Sys.command (sprintf "rm -rf %s" temp_config) : int)
          with _ -> () ) ;
        sprintf "attach the crash report %s" report_file
    | `Ok (Ok None) ->
        (*TODO: tar failed, should we ask people to zip the temp directory themselves?*)
        no_report exn_json status
    | `Ok (Error e) ->
        [%log' fatal top_logger] "Exception when generating crash report: $exn"
          ~metadata:[("exn", Error_json.error_to_yojson e)] ;
        no_report exn_json status
    | `Timeout ->
        [%log' fatal top_logger] "Timed out while generated crash report" ;
        no_report exn_json status
  in
  let message =
    coda_crash_message ~error:"crashed" ~action:action_string ~log_issue:true
  in
  Core.print_string message

let handle_shutdown ~monitor ~time_controller ~conf_dir ~child_pids ~top_logger
    coda_ref =
  Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
      don't_wait_for
        (let%bind () =
           match Monitor.extract_exn exn with
           | Mina_networking.No_initial_peers ->
               let message =
                 coda_crash_message
                   ~error:"failed to connect to any initial peers"
                   ~action:
                     "You might be trying to connect to a different network \
                      version, or need to troubleshoot your configuration. \
                      See https://codaprotocol.com/docs/troubleshooting/ for \
                      details."
                   ~log_issue:false
               in
               Core.print_string message ; Deferred.unit
           | Genesis_ledger_helper.Genesis_state_initialization_error ->
               let message =
                 coda_crash_message
                   ~error:"failed to initialize the genesis state"
                   ~action:
                     "include the last 50 lines from .mina-config/mina.log"
                   ~log_issue:true
               in
               Core.print_string message ; Deferred.unit
           | Mina_user_error.Mina_user_error {message; where} ->
               Core.print_string "\nFATAL ERROR" ;
               let error =
                 match where with
                 | None ->
                     "encountered a configuration error"
                 | Some where ->
                     sprintf "encountered a configuration error %s" where
               in
               let message =
                 coda_crash_message ~error ~action:("\n" ^ message)
                   ~log_issue:false
               in
               Core.print_string message ; Deferred.unit
           | _exn ->
               handle_crash exn ~time_controller ~conf_dir ~child_pids
                 ~top_logger coda_ref
         in
         Stdlib.exit 1) ) ;
  Async_unix.Signal.(
    handle terminating ~f:(fun signal ->
        log_shutdown ~conf_dir ~top_logger coda_ref ;
        let logger =
          Logger.extend top_logger
            [("coda_run", `String "Program was killed by signal")]
        in
        [%log info]
          !"Mina process was interrupted by $signal"
          ~metadata:[("signal", `String (to_string signal))] ;
        (* causes async shutdown and at_exit handlers to run *)
        Async.shutdown 130 ))
