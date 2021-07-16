open Core_kernel
open Async_kernel
open Mina_base
open Signature_lib

type t [@@deriving sexp]

module Scan_state : sig
  [%%versioned:
  module Stable : sig
    module V2 : sig
      type t [@@deriving sexp]

      val hash : t -> Staged_ledger_hash.Aux_hash.t
    end

    module V1 : sig
      type t [@@deriving sexp]

      val to_latest : t -> V2.t

      val hash : t -> Staged_ledger_hash.Aux_hash.t
    end
  end]

  module Job_view : sig
    type t [@@deriving sexp, to_yojson]
  end

  module Space_partition : sig
    type t = { first : int * int; second : (int * int) option }
    [@@deriving sexp]
  end

  val hash : t -> Staged_ledger_hash.Aux_hash.t

  val empty :
    constraint_constants:Genesis_constants.Constraint_constants.t -> unit -> t

  val snark_job_list_json : t -> string

  val staged_transactions : t -> Transaction.t With_status.t list

  val staged_transactions_with_protocol_states :
       t
    -> get_state:(State_hash.t -> Mina_state.Protocol_state.value Or_error.t)
    -> (Transaction.t With_status.t * Mina_state.Protocol_state.value) list
       Or_error.t

  val all_work_statements_exn : t -> Transaction_snark_work.Statement.t list

  (** Hashes of the protocol states required for proving pending transactions*)
  val required_state_hashes : t -> State_hash.Set.t

  (** Validate protocol states required for proving the transactions. Returns an association list of state_hash and the corresponding state*)
  val check_required_protocol_states :
       t
    -> protocol_states:Mina_state.Protocol_state.value list
    -> (State_hash.t * Mina_state.Protocol_state.value) list Or_error.t
end

module Pre_diff_info : Pre_diff_info.S

module Staged_ledger_error : sig
  type t =
    | Non_zero_fee_excess of
        Scan_state.Space_partition.t * Transaction.t With_status.t list
    | Invalid_proofs of
        ( Ledger_proof.t
        * Transaction_snark.Statement.t
        * Mina_base.Sok_message.t )
        list
    | Couldn't_reach_verifier of Error.t
    | Pre_diff of Pre_diff_info.Error.t
    | Insufficient_work of string
    | Mismatched_statuses of Transaction.t With_status.t * Transaction_status.t
    | Invalid_public_key of Public_key.Compressed.t
    | Unexpected of Error.t
  [@@deriving sexp]

  val to_string : t -> string

  val to_error : t -> Error.t
end

val ledger : t -> Ledger.t

val scan_state : t -> Scan_state.t

val pending_coinbase_collection : t -> Pending_coinbase.t

val create_exn :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> ledger:Ledger.t
  -> t

val of_scan_state_and_ledger :
     logger:Logger.t
  -> constraint_constants:Genesis_constants.Constraint_constants.t
  -> verifier:Verifier.t
  -> snarked_registers:
       ( Frozen_ledger_hash.t
       , unit
       , Token_id.t
       , Mina_state.Local_state.t )
       Mina_state.Registers.t
  -> ledger:Ledger.t
  -> scan_state:Scan_state.t
  -> pending_coinbase_collection:Pending_coinbase.t
  -> t Deferred.Or_error.t

val of_scan_state_and_ledger_unchecked :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> snarked_registers:
       ( Frozen_ledger_hash.t
       , unit
       , Token_id.t
       , Mina_state.Local_state.t )
       Mina_state.Registers.t
  -> ledger:Ledger.t
  -> scan_state:Scan_state.t
  -> pending_coinbase_collection:Pending_coinbase.t
  -> t Deferred.Or_error.t

val replace_ledger_exn : t -> Ledger.t -> t

val proof_txns_with_state_hashes :
  t -> (Transaction.t With_status.t * State_hash.t) Non_empty_list.t option

val copy : t -> t

val hash : t -> Staged_ledger_hash.t

val apply :
     ?skip_verification:[ `Proofs | `All ]
  -> constraint_constants:Genesis_constants.Constraint_constants.t
  -> t
  -> Staged_ledger_diff.t
  -> logger:Logger.t
  -> verifier:Verifier.t
  -> current_state_view:Snapp_predicate.Protocol_state.View.t
  -> state_and_body_hash:State_hash.t * State_body_hash.t
  -> coinbase_receiver:Public_key.Compressed.t
  -> supercharge_coinbase:bool
  -> ( [ `Hash_after_applying of Staged_ledger_hash.t ]
       * [ `Ledger_proof of
           (Ledger_proof.t * (Transaction.t With_status.t * State_hash.t) list)
           option ]
       * [ `Staged_ledger of t ]
       * [ `Pending_coinbase_update of bool * Pending_coinbase.Update.t ]
     , Staged_ledger_error.t )
     Deferred.Result.t

val apply_diff_unchecked :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> t
  -> Staged_ledger_diff.With_valid_signatures_and_proofs.t
  -> logger:Logger.t
  -> current_state_view:Snapp_predicate.Protocol_state.View.t
  -> state_and_body_hash:State_hash.t * State_body_hash.t
  -> coinbase_receiver:Public_key.Compressed.t
  -> supercharge_coinbase:bool
  -> ( [ `Hash_after_applying of Staged_ledger_hash.t ]
       * [ `Ledger_proof of
           (Ledger_proof.t * (Transaction.t With_status.t * State_hash.t) list)
           option ]
       * [ `Staged_ledger of t ]
       * [ `Pending_coinbase_update of bool * Pending_coinbase.Update.t ]
     , Staged_ledger_error.t )
     Deferred.Result.t

val current_ledger_proof : t -> Ledger_proof.t option

(* This should memoize the snark verifications *)

val create_diff :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> ?log_block_creation:bool
  -> t
  -> coinbase_receiver:Public_key.Compressed.t
  -> logger:Logger.t
  -> current_state_view:Snapp_predicate.Protocol_state.View.t
  -> transactions_by_fee:User_command.Valid.t Sequence.t
  -> get_completed_work:
       (   Transaction_snark_work.Statement.t
        -> Transaction_snark_work.Checked.t option)
  -> supercharge_coinbase:bool
  -> ( Staged_ledger_diff.With_valid_signatures_and_proofs.t
     , Pre_diff_info.Error.t )
     Result.t

val can_apply_supercharged_coinbase_exn :
     winner:Public_key.Compressed.t
  -> epoch_ledger:Mina_base.Sparse_ledger.t
  -> global_slot:Mina_numbers.Global_slot.t
  -> bool

val statement_exn :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> t
  -> [ `Non_empty of Transaction_snark.Statement.t | `Empty ] Deferred.t

val of_scan_state_pending_coinbases_and_snarked_ledger :
     logger:Logger.t
  -> constraint_constants:Genesis_constants.Constraint_constants.t
  -> verifier:Verifier.t
  -> scan_state:Scan_state.t
  -> snarked_ledger:Ledger.t
  -> snarked_local_state:Mina_state.Local_state.t
  -> expected_merkle_root:Ledger_hash.t
  -> pending_coinbases:Pending_coinbase.t
  -> get_state:(State_hash.t -> Mina_state.Protocol_state.value Or_error.t)
  -> t Or_error.t Deferred.t

val all_work_pairs :
     t
  -> get_state:(State_hash.t -> Mina_state.Protocol_state.value Or_error.t)
  -> (Transaction_witness.t, Ledger_proof.t) Snark_work_lib.Work.Single.Spec.t
     One_or_two.t
     list
     Or_error.t

val all_work_statements_exn : t -> Transaction_snark_work.Statement.t list

val check_commands :
     Ledger.t
  -> verifier:Verifier.t
  -> User_command.t list
  -> (User_command.Valid.t list, Verifier.Failure.t) Result.t
     Deferred.Or_error.t
