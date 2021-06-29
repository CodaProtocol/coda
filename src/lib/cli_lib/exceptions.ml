open Core_kernel
open Async

let handle_nicely (type a) (f : unit -> a Deferred.t) () : a Deferred.t =
  match%bind Deferred.Or_error.try_with ~here:[%here] ~extract_exn:true f with
  | Ok e ->
      return e
  | Error e ->
      eprintf "Error: %s" (Error.to_string_hum e) ;
      exit 4
