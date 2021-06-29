(* exit_handlers -- code to call at daemon exit *)

open Core_kernel
open Async_kernel
open Async_unix

(* register a thunk to be called at exit; log registration and execution *)
let register_handler ~logger ~description (f : unit -> unit) =
  [%log info] "Registering exit handler: $description"
    ~metadata:[("description", `String description)] ;
  let logging_thunk () =
    [%log info] "Running exit handler: $description"
      ~metadata:[("description", `String description)] ;
    (* if there's an exception, log it, allow other handlers to run *)
    try f ()
    with exn ->
      [%log info] "When running exit handler: $description, got exception $exn"
        ~metadata:
          [ ("description", `String description)
          ; ("exn", `String (Exn.to_string exn)) ]
  in
  Stdlib.at_exit logging_thunk

(* register a Deferred.t thunk to be called at Async shutdown; log registration and execution *)
let register_async_shutdown_handler ~logger ~description
    (f : unit -> unit Deferred.t) =
  [%log info] "Registering async shutdown handler: $description"
    ~metadata:[("description", `String description)] ;
  let logging_thunk () =
    [%log info] "Running async shutdown handler: $description"
      ~metadata:[("description", `String description)] ;
    let open Deferred.Let_syntax in
    let%map () =
      match%map Monitor.try_with ~here:[%here] ~extract_exn:true f with
      | Ok () ->
          ()
      | Error exn ->
          [%log info]
            "When running async shutdown handler: $description, got exception \
             $exn"
            ~metadata:
              [ ("description", `String description)
              ; ("exn", `String (Exn.to_string exn)) ]
    in
    ()
  in
  Shutdown.at_shutdown logging_thunk
