open Core_kernel
open Nanobit_base
open Snark_params
open Tick

type t = private Field.t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving bin_io, sexp]
  end
end

val of_field : Field.t -> t

val meets_target_unchecked
  : t
  -> hash:Pedersen.Digest.t
  -> bool

include Snarkable.Bits.S
  with type Unpacked.value = t
   and type Packed.value = t
   and type Packed.var = Cvar.t

val strength_unchecked : t -> Strength.t

(* Someday: Have a dual variable type so I don't have to pass both packed and unpacked
   versions. *)
val strength
  : Packed.var
  -> Unpacked.var
  -> (Strength.Packed.var, _) Tick.Checked.t
