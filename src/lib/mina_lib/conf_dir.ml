(* conf_dir.ml -- config directory management *)

open Core

let compute_conf_dir conf_dir_opt =
  let home = Sys.home_directory () in
  Option.value ~default:(home ^/ Cli_lib.Default.conf_dir_name) conf_dir_opt

let check_and_set_lockfile ~logger conf_dir =
  let lockfile = conf_dir ^/ ".mina-lock" in
  match Sys.file_exists lockfile with
  | `No -> (
      let open Async in
      match%map
        Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
            Writer.with_file ~exclusive:true lockfile ~f:(fun writer ->
                let pid = Unix.getpid () in
                return (Writer.writef writer "%d\n" (Pid.to_int pid)) ) )
      with
      | Ok () ->
          [%log info] "Created daemon lockfile $lockfile"
            ~metadata:[("lockfile", `String lockfile)] ;
          Exit_handlers.register_async_shutdown_handler ~logger
            ~description:"Remove daemon lockfile" (fun () ->
              match%bind Sys.file_exists lockfile with
              | `Yes ->
                  Unix.unlink lockfile
              | _ ->
                  return () )
      | Error exn ->
          Error.tag_arg (Error.of_exn exn)
            "Could not create the daemon lockfile" ("lockfile", lockfile)
            [%sexp_of: string * string]
          |> Error.raise )
  | `Yes -> (
      let open Async in
      match%map
        Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
            Reader.with_file ~exclusive:true lockfile ~f:(fun reader ->
                let%bind pid =
                  let rm_and_raise () =
                    Core.Unix.unlink lockfile ;
                    Mina_user_error.raise
                      "Invalid format in lockfile (removing it)"
                  in
                  match%map Reader.read_line reader with
                  | `Ok s -> (
                    try Pid.of_string s with _ -> rm_and_raise () )
                  | `Eof ->
                      rm_and_raise ()
                in
                let still_running =
                  (* using signal 0 does not send a signal; see man page `kill(2)` *)
                  match Signal.(send zero) (`Pid pid) with
                  | `Ok ->
                      true
                  | `No_such_process ->
                      false
                in
                if still_running then
                  if Pid.equal pid (Unix.getpid ()) then
                    (* can happen when running in Docker *)
                    return ()
                  else
                    Mina_user_error.raisef
                      "A daemon (process id %d) is already running with the \
                       current configuration directory (%s)"
                      (Pid.to_int pid) conf_dir
                else (
                  [%log info] "Removing lockfile for terminated process"
                    ~metadata:
                      [ ("lockfile", `String lockfile)
                      ; ("pid", `Int (Pid.to_int pid)) ] ;
                  Unix.unlink lockfile ) ) )
      with
      | Ok () ->
          ()
      | Error exn ->
          Error.tag_arg (Error.of_exn exn) "Error processing lockfile"
            ("lockfile", lockfile) [%sexp_of: string * string]
          |> Error.raise )
  | `Unknown ->
      Error.create "Could not determine whether the daemon lockfile exists"
        ("lockfile", lockfile) [%sexp_of: string * string]
      |> Error.raise

let export_logs_to_tar ?basename ~conf_dir =
  let open Async in
  let open Deferred.Result.Let_syntax in
  let basename =
    match basename with
    | None ->
        let date, day = Time.(now () |> to_date_ofday ~zone:Zone.utc) in
        let Time.Span.Parts.{hr; min; sec; _} = Time.Ofday.to_parts day in
        sprintf "%s_%02d-%02d-%02d" (Date.to_string date) hr min sec
    | Some basename ->
        basename
  in
  let export_dir = conf_dir ^/ "exported_logs" in
  ( match Core.Sys.file_exists export_dir with
  | `No ->
      Core.Unix.mkdir export_dir
  | _ ->
      () ) ;
  let tarfile = export_dir ^/ basename ^ ".tar.gz" in
  let log_files =
    Core.Sys.ls_dir conf_dir
    |> List.filter ~f:(String.is_substring ~substring:".log")
  in
  let%bind.Deferred.Let_syntax linux_info =
    if String.equal Sys.os_type "Unix" then
      match%map.Deferred.Let_syntax
        Process.run ~prog:"uname" ~args:["-a"] ()
      with
      | Ok s when String.is_prefix s ~prefix:"Linux" ->
          Some s
      | _ ->
          None
    else Deferred.return None
  in
  let%bind.Deferred.Let_syntax hw_info_opt =
    if Option.is_some linux_info then
      let open Deferred.Let_syntax in
      let linux_hw_progs =
        [ ("cat", ["/etc/os-release"])
        ; ("lscpu", [])
        ; ("lsgpu", [])
        ; ("lsmem", [])
        ; ("lsblk", []) ]
      in
      let%map outputs =
        Deferred.List.map linux_hw_progs ~f:(fun (prog, args) ->
            let header =
              sprintf "*** Output from '%s' ***\n"
                (String.concat ~sep:" " (prog :: args))
            in
            let%bind output =
              match%map Process.run_lines ~prog ~args () with
              | Ok lines ->
                  lines
              | Error err ->
                  [sprintf "Error: %s" (Error.to_string_hum err)]
            in
            return ((header :: output) @ [""]) )
      in
      Some (Option.value_exn linux_info :: List.concat outputs)
    else (* TODO: Mac, other Unixes *)
      Deferred.return None
  in
  let%bind.Deferred.Let_syntax hw_file_opt =
    if Option.is_some hw_info_opt then
      let open Async in
      let hw_info = "hardware.info" in
      let hw_info_file = conf_dir ^/ hw_info in
      match%map
        Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
            Writer.with_file ~exclusive:true hw_info_file ~f:(fun writer ->
                Deferred.List.map (Option.value_exn hw_info_opt)
                  ~f:(fun line -> return (Writer.write_line writer line)) ) )
      with
      | Ok _units ->
          Some hw_info
      | Error _exn ->
          (* carry on, despite the error *)
          None
    else Deferred.return None
  in
  let base_files = "mina.version" :: log_files in
  let files =
    Option.value_map hw_file_opt ~default:base_files ~f:(fun hw_file ->
        hw_file :: base_files )
  in
  let tmp_dir =
    Filename.temp_dir ~in_dir:"/tmp" ("mina-logs_" ^ basename) ""
  in
  let files_in_dir dir = List.map files ~f:(fun file -> dir ^/ file) in
  let conf_dir_files = files_in_dir conf_dir in
  let%bind _result0 =
    Process.run ~prog:"cp" ~args:(("-p" :: conf_dir_files) @ [tmp_dir]) ()
  in
  let%bind _result1 =
    Process.run ~prog:"tar"
      ~args:
        ( ["-C"; tmp_dir; (* Create gzipped tar file [file]. *) "-czf"; tarfile]
        @ files )
      ()
  in
  let tmp_dir_files = files_in_dir tmp_dir in
  let open Deferred.Let_syntax in
  let%bind () = Deferred.List.iter tmp_dir_files ~f:Unix.remove in
  let%bind () = Unix.rmdir tmp_dir in
  Deferred.Or_error.return tarfile
