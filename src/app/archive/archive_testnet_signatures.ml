open Async

let () =
  Command.run
    (Command.group ~summary:"Archive node commands" Archive_cli.commands)
