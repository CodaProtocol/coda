open Core
open Protocols

module type S = sig
  open Coda_pow

  module Compressed_public_key : Compressed_public_key_intf

  module Transaction :
    Coda_pow.Transaction_intf with type public_key := Compressed_public_key.t

  module Fee_transfer :
    Coda_pow.Fee_transfer_intf with type public_key := Compressed_public_key.t

  module Super_transaction :
    Coda_pow.Super_transaction_intf
    with type valid_transaction := Transaction.With_valid_signature.t
     and type fee_transfer := Fee_transfer.t

  module Ledger_hash : Coda_pow.Ledger_hash_intf

  module Ledger_proof_statement : sig
    type t =
      { source: Ledger_hash.t
      ; target: Ledger_hash.t
      ; fee_excess: Fee.Signed.t
      ; proof_type: [`Base | `Merge] }
    [@@deriving bin_io, sexp, eq]
  end

  module Proof : sig type t end

  module Ledger_proof : sig
    include Coda_pow.Ledger_proof_intf
            with type statement := Ledger_proof_statement.t
             and type message := Fee.Unsigned.t * Compressed_public_key.t
             and type ledger_hash := Ledger_hash.t
             and type proof := Proof.t

    include Binable.S with type t := t

    include Sexpable.S with type t := t
  end

  module Ledger_proof_verifier : Ledger_proof_verifier_intf
    with type statement := Ledger_proof_statement.t
     and type message := Fee.Unsigned.t * Compressed_public_key.t
     and type ledger_proof := Ledger_proof.t

  module Ledger :
    Coda_pow.Ledger_intf
    with type ledger_hash := Ledger_hash.t
     and type super_transaction := Super_transaction.t
     and type valid_transaction := Transaction.With_valid_signature.t

  module Sparse_ledger : sig
    type t [@@deriving sexp, bin_io]

    val of_ledger_subset_exn : Ledger.t -> Compressed_public_key.t list -> t
  end

  module Ledger_builder_aux_hash : Coda_pow.Ledger_builder_aux_hash_intf

  module Ledger_builder_hash :
    Coda_pow.Ledger_builder_hash_intf
    with type ledger_hash := Ledger_hash.t
     and type ledger_builder_aux_hash := Ledger_builder_aux_hash.t

  module Completed_work :
    Coda_pow.Completed_work_intf
    with type proof := Ledger_proof.t
     and type statement := Ledger_proof_statement.t
     and type public_key := Compressed_public_key.t

  module Ledger_builder_diff :
    Coda_pow.Ledger_builder_diff_intf
    with type completed_work := Completed_work.t
     and type completed_work_checked := Completed_work.Checked.t
     and type transaction := Transaction.t
     and type transaction_with_valid_signature :=
                Transaction.With_valid_signature.t
     and type public_key := Compressed_public_key.t
     and type ledger_builder_hash := Ledger_builder_hash.t

  module Config : sig
    val parallelism_log_2 : int
  end
end
