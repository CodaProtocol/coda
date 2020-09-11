open Core_kernel
open Async_kernel

type 'ledger_proof t =
  { verify_blockchain_snark:
      Blockchain_snark.Blockchain.t -> bool Or_error.t Deferred.t
  ; verify_transaction_snarks:
         ('ledger_proof * Coda_base.Sok_message.t) list
      -> bool Or_error.t Deferred.t }
