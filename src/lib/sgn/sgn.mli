[%%import "/src/config.mlh"]

open Core_kernel

[%%ifdef consensus_mechanism]

open Snark_params.Tick

[%%else]

open Snark_params_nonconsensus

[%%endif]

[%%versioned:
module Stable : sig
  module V1 : sig
    type t = Sgn_type.Sgn.Stable.V1.t = Pos | Neg
    [@@deriving sexp, hash, compare, equal, yojson]
  end
end]

val to_field : t -> Field.t

val of_field_exn : Field.t -> t

val gen : t Quickcheck.Generator.t

val negate : t -> t

[%%ifdef consensus_mechanism]

type var = private Field.Var.t

val typ : (var, t) Typ.t

module Checked : sig
  val constant : t -> var

  val neg : var

  val pos : var

  val is_pos : var -> Boolean.var

  val is_neg : var -> Boolean.var

  val pos_if_true : Boolean.var -> var

  val neg_if_true : Boolean.var -> var

  val negate : var -> var

  val if_ : Boolean.var -> then_:var -> else_:var -> (var, _) Checked.t
end

[%%endif]
