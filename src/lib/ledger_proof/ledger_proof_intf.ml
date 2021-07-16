open Core_kernel
open Mina_base

module type S = sig
  type t [@@deriving compare, equal, sexp, yojson, hash]

  [%%versioned:
  module Stable : sig
    [@@@no_toplevel_latest_type]

    module V2 : sig
      type nonrec t = t [@@deriving compare, equal, sexp, yojson, hash]

      val to_latest : t -> t
    end

    module V1 : sig
      type t [@@deriving compare, equal, sexp, yojson, hash]

      val to_latest : t -> V2.t
    end
  end]

  val create :
       statement:Transaction_snark.Statement.t
    -> sok_digest:Sok_message.Digest.t
    -> proof:Proof.t
    -> t

  val statement_target :
       Transaction_snark.Statement.t
    -> ( Frozen_ledger_hash.t
       , Pending_coinbase.Stack_versioned.t
       , Token_id.t
       , Mina_state.Local_state.t )
       Mina_state.Registers.t

  val statement : t -> Transaction_snark.Statement.t

  val sok_digest : t -> Sok_message.Digest.t

  val underlying_proof : t -> Proof.t
end
