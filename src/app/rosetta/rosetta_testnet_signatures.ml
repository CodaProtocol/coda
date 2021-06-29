open Lib.Rosetta
open Async

let () =
  Command.run
    (Command.async ~summary:"Run Rosetta process on top of Coda" command)
