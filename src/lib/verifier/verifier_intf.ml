open Async_kernel
open Core_kernel

module Base = struct
  module type S = sig
    type t

    type ledger_proof

    val verify_commands :
         t
      -> Mina_base.User_command.Verifiable.t list
         (* The first level of error represents failure to verify, the second a failure in
   communicating with the verifier. *)
      -> [ `Valid of Mina_base.User_command.Valid.t
         | `Invalid
         | `Valid_assuming of
           ( Pickles.Side_loaded.Verification_key.t
           * Mina_base.Snapp_statement.t
           * Pickles.Side_loaded.Proof.t )
           list ]
         list
         Deferred.Or_error.t

    val verify_blockchain_snarks :
      t -> Blockchain_snark.Blockchain.t list -> bool Or_error.t Deferred.t

    val verify_transaction_snarks :
         t
      -> (ledger_proof * Mina_base.Sok_message.t) list
      -> bool Or_error.t Deferred.t
  end
end

module type S = sig
  include Base.S

  val create :
       logger:Logger.t
    -> proof_level:Genesis_constants.Proof_level.t
    -> constraint_constants:Genesis_constants.Constraint_constants.t
    -> pids:Child_processes.Termination.t
    -> conf_dir:string option
    -> t Deferred.t
end
