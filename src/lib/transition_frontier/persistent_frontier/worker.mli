open Frontier_base

type create_args =
  { db: Database.t
  ; logger: Logger.t
  ; persistent_root_instance: Persistent_root.Instance.t }

type input = Diff.Lite.E.t list

include
  Otp_lib.Worker_supervisor.S
  with type create_args := create_args
   and type input := input
   and type output := unit
