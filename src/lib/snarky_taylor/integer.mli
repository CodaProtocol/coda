open Snarky
open Snark
open Bitstring_lib

type 'f t

val constant : m:'f m -> Bigint.t -> 'f t

val shift_left : m:'f m -> 'f t -> int -> 'f t

val of_bits : m:'f m -> 'f Cvar.t Boolean.t Bitstring.Lsb_first.t -> 'f t

val to_bits : m:'f m -> 'f t -> 'f Cvar.t Boolean.t Bitstring.Lsb_first.t

val div_mod : m:'f m -> 'f t -> 'f t -> 'f t * 'f t

val to_field : 'f t -> 'f Cvar.t

val create : value:'f Cvar.t -> upper_bound:Bigint.t -> 'f t
