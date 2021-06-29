open Core_kernel
open Async

type t = string

let create () = Deferred.Or_error.return "s3://o1labs-snarkette-data"

let put ?(options = []) t filename =
  let subcommand = ["s3"; "cp"] in
  let args = subcommand @ [filename; t] @ options in
  let result = Process.run ~prog:"aws" ~args () in
  result |> Deferred.Result.ignore_m
