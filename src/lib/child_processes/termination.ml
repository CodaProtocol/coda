(* termination.ml -- maintain a set of child pids
   when a child not expected to terminate does terminate, raise an exception
*)

open Async
open Core_kernel
include Hashable.Make_binable (Pid)

type process_kind = Prover | Verifier | Libp2p_helper
[@@deriving show {with_path= false}, yojson]

type data = {kind: process_kind; termination_expected: bool}
[@@deriving yojson]

type t = data Pid.Table.t

let create_pid_table () : t = Pid.Table.create ()

let register_process ?(termination_expected = false) (t : t) process kind =
  let data = {kind; termination_expected} in
  Pid.Table.add_exn t ~key:(Process.pid process) ~data

let mark_termination_as_expected t child_pid =
  Pid.Table.change t child_pid
    ~f:(Option.map ~f:(fun r -> {r with termination_expected= true}))

let remove : t -> Pid.t -> unit = Pid.Table.remove

(** for some signals that cause termination, offer a possible explanation *)
let get_signal_cause_opt =
  let open Signal in
  let signal_causes_tbl : string Table.t = Table.create () in
  List.iter
    [ (kill, "Process killed because out of memory")
    ; (int, "Process interrupted by user or other program") ]
    ~f:(fun (signal, msg) ->
      Base.ignore (Table.add signal_causes_tbl ~key:signal ~data:msg) ) ;
  fun signal -> Signal.Table.find signal_causes_tbl signal

let get_child_data (t : t) child_pid = Pid.Table.find t child_pid

let check_terminated_child (t : t) child_pid logger =
  match get_child_data t child_pid with
  | None ->
      (* not a process of interest *)
      ()
  | Some data ->
      if not data.termination_expected then (
        let kind = show_process_kind data.kind in
        [%log error]
          "Child process of kind $process_kind with pid $child_pid has \
           unexpectedly terminated"
          ~metadata:
            [ ("child_pid", `Int (Pid.to_int child_pid))
            ; ("process_kind", `String kind) ] ;
        failwithf "Child process of kind %s has unexpectedly terminated" kind
          () )

(** wait for a [process], which may resolve immediately or in a Deferred.t,
    log any errors, attributing the source to the provided [module] and [location]
*)
let wait_for_process_log_errors ~logger process ~module_ ~location ~here =
  (* Handle implicit raciness in the wait syscall by calling [Process.wait]
     early, so that its value will be correctly cached when we actually need
     it.
  *)
  match
    Or_error.try_with (fun () ->
        (* Eagerly force [Process.wait], so that it won't be captured
           elsewhere on exit.
        *)
        let waiting =
          Monitor.try_with ~here ~run:`Now
            ~rest:
              (`Call
                (fun exn ->
                  let err = Error.of_exn exn in
                  Logger.error logger ~module_ ~location
                    "Saw a deferred exception $exn after waiting for process"
                    ~metadata:[("exn", Error_json.error_to_yojson err)] ))
            (fun () -> Process.wait process)
        in
        don't_wait_for
          ( match%map waiting with
          | Ok _ ->
              ()
          | Error exn ->
              let err = Error.of_exn exn in
              Logger.error logger ~module_ ~location
                "Saw a deferred exception $exn while waiting for process"
                ~metadata:[("exn", Error_json.error_to_yojson err)] ) )
  with
  | Ok _ ->
      ()
  | Error err ->
      Logger.error logger ~module_ ~location
        "Saw an immediate exception $exn while waiting for process"
        ~metadata:[("exn", Error_json.error_to_yojson err)]
