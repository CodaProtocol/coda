[%%import "/src/config.mlh"]

open Core_kernel

[%%ifdef consensus_mechanism]

open Snark_params
open Tick

[%%else]

module Random_oracle = Random_oracle_nonconsensus
open Snark_params_nonconsensus

[%%endif]

[%%versioned:
module Stable : sig
  module V1 : sig
    type t [@@deriving version, sexp, eq, hash, compare, yojson]
  end
end]

type t = Stable.Latest.t [@@deriving sexp, compare]

val to_input : t -> (Field.t, bool) Random_oracle.Input.t

val to_string : t -> string

val of_string : string -> t

(** The default token ID, associated with the native coda token.

    This key should be used for fee and coinbase transactions.
*)
val default : t

(** An invalid token ID. This should be used for transactions that will mint
    new tokens, where the token ID is not known yet. This token should not
    appear in the ledger.
*)
val invalid : t

val next : t -> t

val gen : t Quickcheck.Generator.t

val unpack : t -> bool list

include Comparable.S with type t := t

[%%ifdef consensus_mechanism]

type var

val typ : (var, t) Typ.t

val var_of_t : t -> var

module Checked : sig
  val to_input : var -> (Field.Var.t, Boolean.var) Random_oracle.Input.t

  val equal : var -> var -> (Boolean.var, _) Checked.t

  val if_ : Boolean.var -> then_:var -> else_:var -> (var, _) Checked.t

  module Assert : sig
    val equal : var -> var -> (unit, _) Checked.t

    val not_equal : var -> var -> (unit, _) Checked.t
  end
end

[%%endif]
