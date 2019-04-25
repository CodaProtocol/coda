(* Points on elliptic curves over finite fields by M. SKALBA
 * found at eg https://www.impan.pl/pl/wydawnictwa/czasopisma-i-serie-wydawnicze/acta-arithmetica/all/117/3/82159/points-on-elliptic-curves-over-finite-fields
 *)
open Core

module Intf (F : sig
  type t
end) : sig
  module type S = sig
    val to_group : F.t -> F.t * F.t
  end
end

module Params : sig
  type 'f t

  val a : 'f t -> 'f

  val b : 'f t -> 'f

  val create : (module Field_intf.S with type t = 'f) -> a:'f -> b:'f -> 'f t
end

module Make_group_map
    (Constant : Field_intf.S) (F : sig
        include Field_intf.S

        val constant : Constant.t -> t
    end) (Params : sig
      val params : Constant.t Params.t
    end) : sig
  val potential_xs : F.t -> F.t * F.t * F.t
end
