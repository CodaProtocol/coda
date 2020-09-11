open Async_kernel
open Core_kernel

module Base = struct
  module type S = sig
    type t

    type ledger_proof

    val verify_blockchain_snark :
      t -> Blockchain_snark.Blockchain.t -> bool Or_error.t Deferred.t

    val verify_transaction_snarks :
         t
      -> (ledger_proof * Coda_base.Sok_message.t) list
      -> bool Or_error.t Deferred.t

    val of_generic : ledger_proof Generic.t -> t
  end
end

module type S = sig
  include Base.S

  val create :
       logger:Logger.t
    -> proof_level:Genesis_constants.Proof_level.t
    -> pids:Child_processes.Termination.t
    -> conf_dir:string option
    -> t Deferred.t
end
