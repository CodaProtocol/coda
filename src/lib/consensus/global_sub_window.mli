type t [@@deriving equal]

val succ : t -> t

val of_global_slot : constants:Constants.t -> Global_slot.t -> t

val sub_window : constants:Constants.t -> t -> Sub_window.t

val constant : Unsigned.UInt32.t -> t

val add : t -> t -> t

val sub : t -> t -> Unsigned.UInt32.t

val ( >= ) : t -> t -> bool

module Checked : sig
  open Snark_params.Tick

  type t

  val succ : t -> t

  val equal : t -> t -> (Boolean.var, _) Checked.t

  val constant : Unsigned.UInt32.t -> t

  val add : t -> Mina_numbers.Length.Checked.t -> t

  val ( >= ) : t -> t -> (Boolean.var, _) Checked.t

  val of_global_slot :
    constants:Constants.var -> Global_slot.Checked.t -> (t, _) Checked.t

  val sub_window :
    constants:Constants.var -> t -> (Sub_window.Checked.t, _) Checked.t
end
