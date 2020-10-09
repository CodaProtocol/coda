open Coda_base
open Coda_state
open Core_kernel
open Pickles_types

module Witness : sig
  type t =
    {prev_state: Protocol_state.Value.t; transition: Snark_transition.Value.t}
end

type tag =
  (State_hash.var, Protocol_state.value, Nat.N2.n, Nat.N1.n) Pickles.Tag.t

val verify :
  Protocol_state.Value.t -> Proof.t -> key:Pickles.Verification_key.t -> bool

val check :
     Witness.t
  -> ?handler:(   Snarky_backendless.Request.request
               -> Snarky_backendless.Request.response)
  -> proof_level:Genesis_constants.Proof_level.t
  -> constraint_constants:Genesis_constants.Constraint_constants.t
  -> Transaction_snark.Statement.With_sok.t
  -> State_hash.t
  -> unit Or_error.t

module type S = sig
  module Proof :
    Pickles.Proof_intf
    with type t = (Nat.N2.n, Nat.N2.n) Pickles.Proof.t
     and type statement = Protocol_state.Value.t

  val tag : tag

  val cache_handle : Pickles.Cache_handle.t

  open Nat

  val step :
       Witness.t
    -> ( Protocol_state.Value.t
         * (Transaction_snark.Statement.With_sok.t * unit)
       , N2.n * (N2.n * unit)
       , N1.n * (N5.n * unit)
       , Protocol_state.Value.t
       , Proof.t )
       Pickles.Prover.t
end

module Make (T : sig
  val tag : Transaction_snark.tag
end) : S

val constraint_system_digests : unit -> (string * Md5.t) list
