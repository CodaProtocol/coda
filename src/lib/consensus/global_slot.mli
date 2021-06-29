[%%import "/src/config.mlh"]

open Core_kernel

module Poly : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type ('slot_number, 'slots_per_epoch) t
      [@@deriving sexp, equal, compare, hash, yojson]
    end
  end]
end

[%%versioned:
module Stable : sig
  module V1 : sig
    type t =
      ( Mina_numbers.Global_slot.Stable.V1.t
      , Mina_numbers.Length.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving compare, equal, sexp, hash, yojson]
  end
end]

val to_input : t -> (_, bool) Random_oracle.Input.t

val of_slot_number : constants:Constants.t -> Mina_numbers.Global_slot.t -> t

val gen : constants:Constants.t -> t Quickcheck.Generator.t

val ( + ) : t -> int -> t

val ( - ) : t -> t -> Mina_numbers.Global_slot.t option

val ( < ) : t -> t -> bool

val max : t -> t -> t

val succ : t -> t

val create : constants:Constants.t -> epoch:Epoch.t -> slot:Slot.t -> t

val of_epoch_and_slot : constants:Constants.t -> Epoch.t * Slot.t -> t

val zero : constants:Constants.t -> t

val to_bits : t -> bool list

val epoch : t -> Epoch.t

val slot : t -> Slot.t

val start_time : constants:Constants.t -> t -> Block_time.t

val end_time : constants:Constants.t -> t -> Block_time.t

val time_hum : t -> string

val to_epoch_and_slot : t -> Epoch.t * Slot.t

val of_time_exn : constants:Constants.t -> Block_time.t -> t

val diff : constants:Constants.t -> t -> Epoch.t * Slot.t -> t

[%%ifdef consensus_mechanism]

open Snark_params.Tick
open Bitstring_lib

module Checked : sig
  open Snark_params.Tick

  type t =
    (Mina_numbers.Global_slot.Checked.t, Mina_numbers.Length.Checked.t) Poly.t

  val ( < ) : t -> t -> (Boolean.var, _) Checked.t

  val of_slot_number :
    constants:Constants.var -> Mina_numbers.Global_slot.Checked.t -> t

  val to_bits : t -> (Boolean.var Bitstring.Lsb_first.t, _) Checked.t

  val to_input :
       t
    -> ( (Field.Var.t, Snark_params.Tick.Boolean.var) Random_oracle.Input.t
       , _ )
       Checked.t

  val to_epoch_and_slot : t -> (Epoch.Checked.t * Slot.Checked.t, _) Checked.t

  (** [sub ~m x y] computes [x - y] and ensures that [0 <= x - y] *)
  val sub : t -> t -> (Mina_numbers.Global_slot.Checked.t, _) Checked.t
end

val typ : (Checked.t, t) Typ.t

[%%endif]

val slot_number : ('a, _) Poly.t -> 'a

val slots_per_epoch : (_, 'b) Poly.t -> 'b

module For_tests : sig
  val of_global_slot : t -> Mina_numbers.Global_slot.t -> t
end
