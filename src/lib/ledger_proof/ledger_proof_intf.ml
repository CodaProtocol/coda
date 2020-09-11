open Core_kernel
open Coda_base

module type S = sig
  type t [@@deriving compare, sexp, to_yojson]

  val id : t Type_equal.Id.t

  [%%versioned:
  module Stable : sig
    module V1 : sig
      val to_latest : t -> t

      val of_latest : t -> (t, _) Result.t

      type nonrec t = t [@@deriving compare, sexp, to_yojson]
    end
  end]

  val create :
       statement:Transaction_snark.Statement.t
    -> sok_digest:Sok_message.Digest.t
    -> proof:Proof.t
    -> t

  val statement_target : Transaction_snark.Statement.t -> Frozen_ledger_hash.t

  val statement : t -> Transaction_snark.Statement.t

  val sok_digest : t -> Sok_message.Digest.t

  val underlying_proof : t -> Proof.t
end
