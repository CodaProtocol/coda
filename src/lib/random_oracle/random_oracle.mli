[%%import "/src/config.mlh"]

[%%ifdef consensus_mechanism]

open Pickles.Impls.Step.Internal_Basic

[%%else]

open Snark_params_nonconsensus

[%%endif]

module Input = Random_oracle_input

module State : sig
  type 'a t [@@deriving equal, sexp, compare]

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
end

include
  Intf.S
  with type field := Field.t
   and type field_constant := Field.t
   and type bool := bool
   and module State := State

val salt : string -> Field.t State.t

[%%ifdef consensus_mechanism]

module Checked :
  Intf.S
  with type field := Field.Var.t
   and type field_constant := Field.t
   and type bool := Boolean.var
   and module State := State

[%%endif]
