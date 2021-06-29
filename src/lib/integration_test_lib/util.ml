open Core
open Async

(* module util with  *)

let run_cmd dir prog args =
  [%log' spam (Logger.create ())]
    "Running command (from %s): $command" dir
    ~metadata:[("command", `String (String.concat (prog :: args) ~sep:" "))] ;
  Process.create_exn ~working_dir:dir ~prog ~args ()
  >>= Process.collect_output_and_wait

let run_cmd_or_error dir prog args =
  let open Process.Output in
  let%bind output = run_cmd dir prog args in
  let print_output () =
    let indent str =
      String.split str ~on:'\n'
      |> List.map ~f:(fun s -> "    " ^ s)
      |> String.concat ~sep:"\n"
    in
    print_endline "=== COMMAND ===" ;
    print_endline
      (indent
         ( prog ^ " "
         ^ String.concat ~sep:" "
             (List.map args ~f:(fun arg -> "\"" ^ arg ^ "\"")) )) ;
    print_endline "=== STDOUT ===" ;
    print_endline (indent output.stdout) ;
    print_endline "=== STDERR ===" ;
    print_endline (indent output.stderr) ;
    Writer.(flushed (Lazy.force stdout))
  in
  match output.exit_status with
  | Ok () ->
      return (Ok output.stdout)
  | Error (`Exit_non_zero status) ->
      let%map () = print_output () in
      Or_error.errorf "command exited with status code %d" status
  | Error (`Signal signal) ->
      let%map () = print_output () in
      Or_error.errorf "command exited prematurely due to signal %d"
        (Signal.to_system_int signal)

let run_cmd_exn dir prog args =
  match%map run_cmd_or_error dir prog args with
  | Ok output ->
      output
  | Error error ->
      Error.raise error

let rec prompt_continue prompt_string =
  print_string prompt_string ;
  let%bind () = Writer.flushed (Lazy.force Writer.stdout) in
  let c = Option.value_exn In_channel.(input_char stdin) in
  print_newline () ;
  if c = 'y' || c = 'Y' then Deferred.unit else prompt_continue prompt_string

module Make (Engine : Intf.Engine.S) = struct
  let pub_key_of_node node =
    let open Signature_lib in
    (* let n = Engine.Network.Node *)
    match Engine.Network.Node.network_keypair node with
    | Some nk ->
        Malleable_error.return (nk.keypair.public_key |> Public_key.compress)
    | None ->
        Malleable_error.hard_error_format
          "Node '%s' did not have a network keypair, if node is a block \
           producer this should not happen"
          (Engine.Network.Node.id node)
end
