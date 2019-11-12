open Coda_base

type view =
  { new_user_commands: User_command.t list
  ; removed_user_commands: User_command.t list
  ; reorg_best_tip: bool }

include Intf.Extension_intf with type view := view
