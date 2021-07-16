open Core
open Async

(** [start dir] starts writing the trace output to [dir ^/ "trace" ^/(current_pid ^ ".trace")]. *)
let start dir =
  O1trace.forget_tid (fun () ->
      let trace_dir = dir ^/ "trace" in
      let%bind () = File_system.create_dir trace_dir in
      Writer.open_file ~append:true
        (trace_dir ^/ sprintf "%d.trace" (Unix.getpid () |> Pid.to_int))
      >>| O1trace.start_tracing)

(** Stop tracing. *)
let stop () = O1trace.stop_tracing ()
