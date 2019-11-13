open Frontier_base

type create_args = {db: Database.t; logger: Logger.t}

include
  Otp_lib.Worker_supervisor.S
  with type create_args := create_args
   and type input := Diff.Lite.E.t list * Frontier_hash.t
   and type output := unit
