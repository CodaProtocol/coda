open Core_kernel
open Snark_params

type t [@@deriving sexp, bin_io]

module Bits : Bits_intf.S with type t := t

include Tick.Snarkable.Bits.S
  with type Unpacked.value = t
   and type Packed.value = t
   and type Packed.var = private Tick.Cvar.t

module Span : sig
  type t [@@deriving bin_io]

  val of_time_span : Time.Span.t -> t

  include Tick.Snarkable.Bits.S
    with type Unpacked.value = t
    and type Packed.value = t
end

val diff_checked
  : Unpacked.var -> Unpacked.var -> (Span.Unpacked.var, _) Tick.Checked.t

val diff : t -> t -> Span.t

val of_time : Time.t -> t

val to_time : t -> Time.t
