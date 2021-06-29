open Async

let () =
  Command.run
    (Command.group
       ~summary:"OCaml reference signer implementation for Rosetta."
       Signer_cli.commands)
