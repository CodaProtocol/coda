(* child_processes.ml -- management of starting, tracking, and killing child processes. *)

open Core
open Async
open Pipe_lib
module Termination = Termination

exception Child_died

type t =
  { process: Process.t
  ; stdout_pipe: string Strict_pipe.Reader.t
  ; stderr_pipe: string Strict_pipe.Reader.t
  ; stdin: Writer.t
  ; terminated_ivar: Unix.Exit_or_signal.t Or_error.t Ivar.t
  ; mutable killing: bool
  ; mutable termination_response:
      [ `Always_raise
      | `Raise_on_failure
      | `Handler of
        killed:bool -> Unix.Exit_or_signal.t Or_error.t -> unit Deferred.t
      | `Ignore ] }

let stdout_lines : t -> string Strict_pipe.Reader.t = fun t -> t.stdout_pipe

let stderr_lines : t -> string Strict_pipe.Reader.t = fun t -> t.stderr_pipe

let stdin : t -> Writer.t = fun t -> t.stdin

let termination_status : t -> Unix.Exit_or_signal.t Or_error.t option =
 fun t -> Ivar.peek t.terminated_ivar

(* Try running [f] until it returns [Ok], returning the first [Ok] or [Error]
   if all attempts fail. *)
let keep_trying :
    f:('a -> 'b Deferred.Or_error.t) -> 'a list -> 'b Deferred.Or_error.t =
 fun ~f xs ->
  let open Deferred.Let_syntax in
  let rec go e xs : 'b Deferred.Or_error.t =
    match xs with
    | [] ->
        return e
    | x :: xs -> (
        match%bind f x with
        | Ok r ->
            return (Ok r)
        | Error e ->
            go (Error e) xs )
  in
  go (Or_error.error_string "empty input") xs

(* Unfortunately, `dune runtest` runs in a pwd deep inside the build directory.
   This hack finds the project root by recursively looking for the dune-project
   file. *)
let get_project_root () =
  let open Filename in
  let rec go dir =
    if Core.Sys.file_exists_exn @@ dir ^/ "src/dune-project" then Some dir
    else if String.equal dir "/" then None
    else go @@ fst @@ split dir
  in
  go @@ realpath current_dir_name

(* This snippet was taken from our fork of RPC Parallel. Would be nice to have a
   shared utility, but this is easiest for now. *)
(* To get the currently running executable:
   On Darwin:
   Use _NSGetExecutablePath via Ctypes

   On Linux:
   Use /proc/PID/exe
   - argv[0] might have been deleted (this is quite common with jenga)
   - `cp /proc/PID/exe dst` works as expected while `cp /proc/self/exe dst` does
     not *)
let get_mina_binary () =
  let open Async in
  let open Deferred.Or_error.Let_syntax in
  let%bind os = Process.run ~prog:"uname" ~args:["-s"] () in
  if os = "Darwin\n" then
    let open Ctypes in
    let ns_get_executable_path =
      Foreign.foreign "_NSGetExecutablePath"
        (ptr char @-> ptr uint32_t @-> returning int)
    in
    let path_max = Syslimits.path_max () in
    let buf = Ctypes.allocate_n char ~count:path_max in
    let count = Ctypes.allocate uint32_t (Unsigned.UInt32.of_int path_max) in
    let%map () =
      Deferred.return
        (Result.ok_if_true
           (ns_get_executable_path buf count = 0)
           ~error:
             (Error.of_string
                "call to _NSGetExecutablePath failed unexpectedly"))
    in
    let s = string_from_ptr buf ~length:(!@count |> Unsigned.UInt32.to_int) in
    List.hd_exn @@ String.split s ~on:(Char.of_int 0 |> Option.value_exn)
  else
    (* FIXME for finding the executable relative to the install path this should
       deference the symlink if possible. *)
    Deferred.Or_error.return
      (Unix.getpid () |> Pid.to_int |> sprintf "/proc/%d/exe")

(* Check the PID file, and if it exists and corresponds to a currently running
   process, kill that process. This runs when the daemon starts, and should
   *not* be used to kill a process that was started during this run of the
   daemon.
*)
let maybe_kill_and_unlock : string -> Filename.t -> Logger.t -> unit Deferred.t
    =
 fun name lockpath logger ->
  let open Deferred.Let_syntax in
  match%bind Sys.file_exists lockpath with
  | `Yes -> (
      let%bind pid_str = Reader.file_contents lockpath in
      let pid = Pid.of_string pid_str in
      [%log debug] "Found PID file for %s %s with contents %s" name lockpath
        pid_str ;
      let%bind () =
        match Signal.send Signal.term (`Pid pid) with
        | `No_such_process ->
            [%log debug] "Couldn't kill %s with PID %s, does not exist" name
              pid_str ;
            Deferred.unit
        | `Ok -> (
            [%log debug] "Successfully sent TERM signal to %s (%s)" name
              pid_str ;
            let%bind () = after (Time.Span.of_sec 0.5) in
            match Signal.send Signal.kill (`Pid pid) with
            | `No_such_process ->
                Deferred.unit
            | `Ok ->
                [%log error]
                  "helper process %s (%s) didn't die after being sent TERM, \
                   KILLed it"
                  name pid_str ;
                Deferred.unit )
      in
      match%bind Sys.file_exists lockpath with
      | `Yes ->
          Deferred.unit
      | `Unknown | `No -> (
          match%bind try_with (fun () -> Sys.remove lockpath) with
          | Ok () ->
              Deferred.unit
          | Error exn ->
              [%log warn]
                !"Couldn't delete lock file for %s (pid $childPid) after \
                  killing it. If another Mina daemon was already running it \
                  may have cleaned it up for us. ($exn)"
                name
                ~metadata:
                  [ ("childPid", `Int (Pid.to_int pid))
                  ; ("exn", `String (Exn.to_string exn)) ] ;
              Deferred.unit ) )
  | `Unknown | `No ->
      [%log debug] "No PID file for %s" name ;
      Deferred.unit

type output_handling =
  [`Log of Logger.Level.t | `Don't_log]
  * [`Pipe | `No_pipe]
  * [`Keep_empty | `Filter_empty]

(* Given a Reader.t coming from a process output, optionally log the lines
   coming from it and return a strict pipe that will get the lines if the
   argument is `Pipe and be empty if it's `No_pipe.
*)
let reader_to_strict_pipe_with_logging :
       Reader.t
    -> string
    -> output_handling
    -> Logger.t
    -> string Strict_pipe.Reader.t =
 fun reader name (log, pipe, filter_empty) logger ->
  let master_r, master_w =
    Strict_pipe.create ~name
      (Strict_pipe.Buffered (`Capacity 100, `Overflow Crash))
  in
  let lines_js_pipe = Reader.lines reader in
  don't_wait_for
    ( Pipe.iter_without_pushback lines_js_pipe ~f:(fun line ->
          match filter_empty with
          | `Keep_empty ->
              Strict_pipe.Writer.write master_w line
          | `Filter_empty ->
              if not (String.equal line "") then
                Strict_pipe.Writer.write master_w line )
    >>= fun () ->
    Strict_pipe.Writer.close master_w ;
    Deferred.unit ) ;
  let logging_r, client_r = Strict_pipe.Reader.Fork.two master_r in
  don't_wait_for
    (Strict_pipe.Reader.iter_without_pushback logging_r ~f:(fun line ->
         match log with
         | `Log level -> (
             let simple_log_msg =
               lazy
                 { Logger.Message.timestamp= Time.now ()
                 ; level
                 ; source=
                     Some
                       (Logger.Source.create ~module_:__MODULE__
                          ~location:__LOC__)
                 ; message= "Output from process $child_name: $line"
                 ; metadata=
                     String.Map.set ~key:"child_name" ~data:(`String name)
                       (String.Map.set ~key:"line" ~data:(`String line)
                          (Logger.metadata logger))
                 ; event_id= None }
             in
             match
               Option.try_with (fun () -> Yojson.Safe.from_string line)
             with
             | Some json -> (
               match Logger.Message.of_yojson json with
               | Ok msg ->
                   Logger.raw logger msg
               | Error _err ->
                   Logger.raw logger (Lazy.force simple_log_msg) )
             | None ->
                 Logger.raw logger (Lazy.force simple_log_msg) )
         | `Don't_log ->
             () )) ;
  (* Ideally we'd close the pipe, but you can't do that with a reader, so we
     iterate over it and drop everything. Since Strict_pipe enforces a single
     reader this is safe. *)
  don't_wait_for
    ( if pipe = `No_pipe then
      Strict_pipe.Reader.iter client_r ~f:(Fn.const Deferred.unit)
    else Deferred.unit ) ;
  client_r

let start_custom :
       logger:Logger.t
    -> name:string
    -> git_root_relative_path:string
    -> conf_dir:string
    -> args:string list
    -> stdout:output_handling
    -> stderr:output_handling
    -> termination:[ `Always_raise
                   | `Raise_on_failure
                   | `Handler of
                        killed:bool
                     -> Unix.Exit_or_signal.t Or_error.t
                     -> unit Deferred.t
                   | `Ignore ]
    -> t Deferred.Or_error.t =
 fun ~logger ~name ~git_root_relative_path ~conf_dir ~args ~stdout ~stderr
     ~termination ->
  let open Deferred.Or_error.Let_syntax in
  let%bind () =
    Sys.is_directory conf_dir
    |> Deferred.bind ~f:(function
         | `Yes ->
             Deferred.Or_error.return ()
         | _ ->
             Deferred.Or_error.errorf "Config directory %s does not exist"
               conf_dir )
  in
  let lock_path = conf_dir ^/ name ^ ".lock" in
  let%bind () =
    Deferred.map ~f:Or_error.return
    @@ maybe_kill_and_unlock name lock_path logger
  in
  [%log debug] "Starting custom child process $name with args $args"
    ~metadata:
      [ ("name", `String name)
      ; ("args", `List (List.map args ~f:(fun a -> `String a))) ] ;
  let%bind mina_binary_path = get_mina_binary () in
  let relative_to_root =
    get_project_root ()
    |> Option.map ~f:(fun root -> root ^/ git_root_relative_path)
  in
  let%bind process =
    keep_trying
      (List.filter_opt
         [ Unix.getenv @@ "CODA_" ^ String.uppercase name ^ "_PATH"
         ; relative_to_root
         ; Some (Filename.dirname mina_binary_path ^/ name)
         ; Some ("mina-" ^ name)
         ; Some ("coda-" ^ name) ])
      ~f:(fun prog -> Process.create ~stdin:"" ~prog ~args ())
  in
  [%log info] "Custom child process $name started with pid $pid"
    ~metadata:
      [ ("name", `String name)
      ; ("args", `List (List.map args ~f:(fun a -> `String a)))
      ; ("pid", `Int (Process.pid process |> Pid.to_int)) ] ;
  Termination.wait_for_process_log_errors ~logger process ~module_:__MODULE__
    ~location:__LOC__ ;
  let%bind () =
    Deferred.map ~f:Or_error.return
    @@ Async.Writer.save lock_path
         ~contents:(Pid.to_string @@ Process.pid process)
  in
  let terminated_ivar = Ivar.create () in
  let stdout_pipe =
    reader_to_strict_pipe_with_logging (Process.stdout process)
      (name ^ "-stdout") stdout
      (Logger.extend logger
         [("process", `String name); ("handle", `String "stdout")])
  in
  let stderr_pipe =
    reader_to_strict_pipe_with_logging (Process.stderr process)
      (name ^ "-stderr") stderr
      (Logger.extend logger
         [("process", `String name); ("handle", `String "stderr")])
  in
  let t =
    { process
    ; stdout_pipe
    ; stderr_pipe
    ; stdin= Process.stdin process
    ; terminated_ivar
    ; killing= false
    ; termination_response= termination }
  in
  don't_wait_for
    (let open Deferred.Let_syntax in
    let%bind termination_status =
      Deferred.Or_error.try_with ~here:[%here] (fun () -> Process.wait process)
    in
    [%log trace] "child process %s died" name ;
    don't_wait_for
      (let%bind () = after (Time.Span.of_sec 1.) in
       let%bind () = Writer.close @@ Process.stdin process in
       let%bind () = Reader.close @@ Process.stdout process in
       Reader.close @@ Process.stderr process) ;
    let%bind () = Sys.remove lock_path in
    Ivar.fill terminated_ivar termination_status ;
    let log_bad_termination () =
      let exit_or_signal =
        match termination_status with
        | Ok termination_status ->
            `String (Unix.Exit_or_signal.to_string_hum termination_status)
        | Error err ->
            Error_json.error_to_yojson err
      in
      [%log fatal] "Process died unexpectedly: $exit_or_signal"
        ~metadata:[("exit_or_signal", exit_or_signal)] ;
      raise Child_died
    in
    match (t.termination_response, termination_status) with
    | `Ignore, _ ->
        Deferred.unit
    | `Always_raise, _ ->
        log_bad_termination ()
    | `Raise_on_failure, (Error _ | Ok (Error _)) ->
        log_bad_termination ()
    | `Raise_on_failure, Ok (Ok ()) ->
        Deferred.unit
    | `Handler f, _ ->
        f ~killed:t.killing termination_status) ;
  Deferred.Or_error.return t

let kill : t -> Unix.Exit_or_signal.t Deferred.Or_error.t =
 fun t ->
  match Ivar.peek t.terminated_ivar with
  | None | Some (Error _) ->
      (* The [Error] termination status indicates that we were not able to
         monitor this process, and it may still be running. In these cases, it
         is reasonable to call [kill], and we should attempt to end the process
         if it is running.
      *)
      if t.killing then Ivar.read t.terminated_ivar
      else (
        t.killing <- true ;
        ( match t.termination_response with
        | `Handler _ ->
            ()
        | _ ->
            t.termination_response <- `Ignore ) ;
        match Signal.send Signal.term (`Pid (Process.pid t.process)) with
        | `Ok ->
            Ivar.read t.terminated_ivar
        | `No_such_process ->
            Deferred.Or_error.error_string
              "No such process running. This should be impossible." )
  | Some _ ->
      Deferred.Or_error.error_string "already terminated"

let register_process ?termination_expected (termination : Termination.t)
    (process : t) kind =
  Termination.register_process ?termination_expected termination
    process.process kind

let%test_module _ =
  ( module struct
    let logger = Logger.create ()

    let async_with_temp_dir f =
      Async.Thread_safe.block_on_async_exn (fun () ->
          File_system.with_temp_dir
            (Filename.temp_dir_name ^/ "child-processes")
            ~f )

    let name = "tester.sh"

    let git_root_relative_path = "src/lib/child_processes/tester.sh"

    let process_wait_timeout = Time.Span.of_sec 2.1

    let%test_unit "can launch and get stdout" =
      async_with_temp_dir (fun conf_dir ->
          let open Deferred.Let_syntax in
          let%bind process =
            start_custom ~logger ~name ~git_root_relative_path ~conf_dir
              ~args:["exit"]
              ~stdout:(`Log Logger.Level.Debug, `Pipe, `Keep_empty)
              ~stderr:(`Log Logger.Level.Error, `No_pipe, `Keep_empty)
              ~termination:`Raise_on_failure
            |> Deferred.map ~f:Or_error.ok_exn
          in
          let%bind () =
            Strict_pipe.Reader.iter (stdout_lines process) ~f:(fun line ->
                [%test_eq: string] "hello" line ;
                Deferred.unit )
          in
          (* Pipe will be closed before the ivar is filled, so we need to wait a
             bit. *)
          let%bind () = after process_wait_timeout in
          [%test_eq: Unix.Exit_or_signal.t Or_error.t option]
            (Some (Ok (Ok ())))
            (termination_status process) ;
          Deferred.unit )

    let%test_unit "killing works" =
      async_with_temp_dir (fun conf_dir ->
          let open Deferred.Let_syntax in
          let%bind process =
            start_custom ~logger ~name ~git_root_relative_path ~conf_dir
              ~args:["loop"]
              ~stdout:(`Don't_log, `Pipe, `Keep_empty)
              ~stderr:(`Don't_log, `No_pipe, `Keep_empty)
              ~termination:`Always_raise
            |> Deferred.map ~f:Or_error.ok_exn
          in
          let lock_exists () =
            Deferred.map
              (Sys.file_exists (conf_dir ^/ name ^ ".lock"))
              ~f:(function `Yes -> true | _ -> false)
          in
          let assert_lock_exists () =
            Deferred.map (lock_exists ()) ~f:(fun exists -> assert exists)
          in
          let assert_lock_does_not_exist () =
            Deferred.map (lock_exists ()) ~f:(fun exists -> assert (not exists))
          in
          let%bind () = assert_lock_exists () in
          let output = ref [] in
          let rec go () =
            match%bind Strict_pipe.Reader.read (stdout_lines process) with
            | `Eof ->
                failwith "pipe closed when process should've run forever"
            | `Ok line ->
                output := line :: !output ;
                if List.length !output = 10 then Deferred.unit else go ()
          in
          let%bind () = go () in
          [%test_eq: string list] !output (List.init 10 ~f:(fun _ -> "hello")) ;
          let%bind () = after process_wait_timeout in
          assert (Option.is_none @@ termination_status process) ;
          let%bind kill_res = kill process in
          let%bind () = assert_lock_does_not_exist () in
          let exit_or_signal = Or_error.ok_exn kill_res in
          [%test_eq: Unix.Exit_or_signal.t] exit_or_signal
            (Error (`Signal Signal.term)) ;
          assert (Option.is_some @@ termination_status process) ;
          Deferred.unit )

    let%test_unit "if you spawn two processes it kills the earlier one" =
      async_with_temp_dir (fun conf_dir ->
          let open Deferred.Let_syntax in
          let mk_process () =
            start_custom ~logger ~name ~git_root_relative_path ~conf_dir
              ~args:["loop"]
              ~stdout:(`Don't_log, `No_pipe, `Keep_empty)
              ~stderr:(`Don't_log, `No_pipe, `Keep_empty)
              ~termination:`Ignore
          in
          let%bind process1 =
            mk_process () |> Deferred.map ~f:Or_error.ok_exn
          in
          let%bind process2 =
            mk_process () |> Deferred.map ~f:Or_error.ok_exn
          in
          let%bind () = after process_wait_timeout in
          [%test_eq: Unix.Exit_or_signal.t Or_error.t option]
            (termination_status process1)
            (Some (Ok (Error (`Signal Core.Signal.term)))) ;
          [%test_eq: Unix.Exit_or_signal.t Or_error.t option]
            (termination_status process2)
            None ;
          let%bind _ = kill process2 in
          Deferred.unit )
  end )
