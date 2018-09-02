open Core_kernel
open Async_kernel

module type Security_intf = sig
  val max_depth : [`Infinity | `Finite of int]
  (** In production we set this to (hopefully a prefix of) k for our consensus
   * mechanism; infinite is for tests *)
end

module type Time_controller_intf = sig
  type t

  val create : unit -> t
end

module type Time_intf = sig
  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp, bin_io]
    end
  end

  module Controller : Time_controller_intf

  type t [@@deriving sexp]

  type t0 = t

  module Span : sig
    type t

    val of_time_span : Core_kernel.Time.Span.t -> t

    val to_ms : t -> Int64.t

    val of_ms : Int64.t -> t

    val ( < ) : t -> t -> bool

    val ( > ) : t -> t -> bool

    val ( >= ) : t -> t -> bool

    val ( <= ) : t -> t -> bool

    val ( = ) : t -> t -> bool
  end

  module Timeout : sig
    type 'a t

    val create : Controller.t -> Span.t -> f:(t0 -> 'a) -> 'a t

    val to_deferred : 'a t -> 'a Deferred.t

    val peek : 'a t -> 'a option

    val cancel : Controller.t -> 'a t -> 'a -> unit
  end

  val to_span_since_epoch : t -> Span.t

  val of_span_since_epoch : Span.t -> t

  val diff : t -> t -> Span.t

  val sub : t -> Span.t -> t

  val add : t -> Span.t -> t

  val modulus : t -> Span.t -> Span.t

  val now : Controller.t -> t
end

module type Ledger_hash_intf = sig
  type t [@@deriving bin_io, eq, sexp]

  val to_bytes : t -> string

  include Hashable.S_binable with type t := t
end

module type Protocol_state_hash_intf = sig
  type t [@@deriving bin_io, sexp, eq]

  include Hashable.S_binable with type t := t
end

module type Protocol_state_proof_intf = sig
  type t
end

module type Ledger_builder_aux_hash_intf = sig
  type t [@@deriving bin_io, sexp, eq]

  val of_bytes : string -> t
end

module type Ledger_builder_hash_intf = sig
  type t [@@deriving bin_io, sexp, eq, compare]

  type ledger_hash

  type ledger_builder_aux_hash

  val of_bytes : string -> t

  val of_aux_and_ledger_hash : ledger_builder_aux_hash -> ledger_hash -> t

  include Hashable.S_binable with type t := t
end

module type Proof_intf = sig
  type input

  type t

  val verify : t -> input -> bool Deferred.t
end

module type Ledger_intf = sig
  type t [@@deriving sexp, compare, hash, bin_io]

  module Undo : sig
    type t [@@deriving sexp]
  end

  type valid_transaction

  type super_transaction

  type ledger_hash

  val create : unit -> t

  val copy : t -> t

  val merkle_root : t -> ledger_hash

  val apply_super_transaction : t -> super_transaction -> Undo.t Or_error.t

  val undo : t -> Undo.t -> unit Or_error.t
end

module Fee = struct
  module Unsigned = struct
    include Currency.Fee

    include (
      Currency.Fee.Stable.V1 :
        module type of Currency.Fee.Stable.V1 with type t := t )
  end

  module Signed = struct
    include Currency.Fee.Signed

    include (
      Currency.Fee.Signed.Stable.V1 :
        module type of Currency.Fee.Signed.Stable.V1
        with type t := t
         and type ('a, 'b) t_ := ('a, 'b) t_ )
  end
end

module type Snark_pool_proof_intf = sig
  module Statement : sig
    type t [@@deriving sexp, bin_io]
  end

  type t [@@deriving sexp, bin_io]
end

module type Transaction_intf = sig
  type t [@@deriving sexp, compare, eq, bin_io]

  type public_key

  module With_valid_signature : sig
    type nonrec t = private t [@@deriving sexp, compare, eq]
  end

  val check : t -> With_valid_signature.t option

  val fee : t -> Fee.Unsigned.t

  val sender : t -> public_key

  val receiver : t -> public_key
end

module type Private_key_intf = sig
  type t
end

module type Compressed_public_key_intf = sig
  type t [@@deriving sexp, bin_io, compare]

  include Comparable.S with type t := t
end

module type Public_key_intf = sig
  module Private_key : Private_key_intf

  type t [@@deriving sexp]

  module Compressed : Compressed_public_key_intf

  val of_private_key : Private_key.t -> t
end

module type Fee_transfer_intf = sig
  type t [@@deriving sexp, compare, eq]

  type public_key

  type single = public_key * Fee.Unsigned.t

  val of_single_list : (public_key * Fee.Unsigned.t) list -> t list

  val receivers : t -> public_key list
end

module type Super_transaction_intf = sig
  type valid_transaction

  type fee_transfer

  type t = Transaction of valid_transaction | Fee_transfer of fee_transfer
  [@@deriving sexp, compare, eq, bin_io]

  val fee_excess : t -> Fee.Unsigned.t Or_error.t
end

module type Ledger_proof_statement_intf = sig
  type ledger_hash

  type t =
    { source: ledger_hash
    ; target: ledger_hash
    ; fee_excess: Fee.Signed.t
    ; proof_type: [`Base | `Merge] }
  [@@deriving sexp, bin_io, compare]

  val merge : t -> t -> t Or_error.t

  include Comparable.S with type t := t
end

module type Ledger_proof_intf = sig
  type ledger_hash

  type statement

  type message

  type proof

  type t [@@deriving sexp]

  val statement_target : statement -> ledger_hash

  val underlying_proof : t -> proof
end

module type Ledger_proof_verifier_intf = sig
  type ledger_proof

  type message

  type statement

  val verify : ledger_proof -> statement -> message:message -> bool Deferred.t
end

module type Completed_work_intf = sig
  type proof

  type statement

  type public_key

  module Statement : sig
    type t = statement list

    include Sexpable.S with type t := t

    include Binable.S with type t := t

    include Hashable.S_binable with type t := t

    val gen : t Quickcheck.Generator.t
  end

  (* TODO: The SOK message actually should bind the SNARK to
   be in this particular bundle. The easiest way would be to
   SOK with
   H(all_statements_in_bundle || fee || public_key)
*)

  type t = {fee: Fee.Unsigned.t; proofs: proof list; prover: public_key}
  [@@deriving sexp, bin_io]

  type unchecked = t

  module Checked : sig
    type t [@@deriving sexp, bin_io]

    val create_unsafe : unchecked -> t
  end

  val forget : Checked.t -> t

  val proofs_length : int
end

module type Ledger_builder_diff_intf = sig
  type transaction

  type transaction_with_valid_signature

  type ledger_builder_hash

  type public_key

  type completed_work

  type completed_work_checked

  type t =
    { prev_hash: ledger_builder_hash
    ; completed_works: completed_work list
    ; transactions: transaction list
    ; creator: public_key }
  [@@deriving sexp, bin_io]

  module With_valid_signatures_and_proofs : sig
    type t =
      { prev_hash: ledger_builder_hash
      ; completed_works: completed_work_checked list
      ; transactions: transaction_with_valid_signature list
      ; creator: public_key }
    [@@deriving sexp]
  end

  val forget : With_valid_signatures_and_proofs.t -> t
end

module type Ledger_builder_transition_intf = sig
  type ledger_builder

  type diff

  type diff_with_valid_signatures_and_proofs

  type t = {old: ledger_builder; diff: diff}

  module With_valid_signatures_and_proofs : sig
    type t = {old: ledger_builder; diff: diff_with_valid_signatures_and_proofs}
  end

  val forget : With_valid_signatures_and_proofs.t -> t
end

module type Ledger_builder_intf = sig
  type t [@@deriving sexp, bin_io]

  type diff

  type valid_diff

  type ledger_builder_aux_hash

  type ledger_builder_hash

  type ledger_hash

  type public_key

  type ledger

  type ledger_proof

  type super_transaction

  type transaction_with_valid_signature

  type statement

  type ledger_proof_statement

  type ledger_proof_statement_set

  type sparse_ledger

  type completed_work

  val copy : t -> t

  val hash : t -> ledger_builder_hash

  val ledger : t -> ledger

  val create : ledger:ledger -> self:public_key -> t

  val current_ledger_proof : t -> ledger_proof option

  val apply : t -> diff -> ledger_proof option Deferred.Or_error.t

  (* This should memoize the snark verifications *)

  val create_diff :
       t
    -> logger:Logger.t
    -> transactions_by_fee:transaction_with_valid_signature Sequence.t
    -> get_completed_work:(statement -> completed_work option)
    -> valid_diff
       * [`Hash_after_applying of ledger_builder_hash]
       * [`Ledger_proof of (ledger_proof * ledger_proof_statement) option]

  module Aux : sig
    type t [@@deriving bin_io]

    val hash : t -> ledger_builder_aux_hash
  end

  val aux : t -> Aux.t

  val make : public_key:public_key -> ledger:ledger -> aux:Aux.t -> t

  val random_work_spec_chunk :
       t
    -> ledger_proof_statement_set * ledger_proof_statement option
    -> ( ledger_proof_statement
       , super_transaction
       , sparse_ledger
       , ledger_proof )
       Snark_work_lib.Work.Single.Spec.t
       list
       option
       * (ledger_proof_statement_set * ledger_proof_statement option)
end

module type Tip_intf = sig
  type protocol_state

  type protocol_state_proof

  type ledger_builder

  type external_transition

  type t =
    { protocol_state: protocol_state
    ; proof: protocol_state_proof
    ; ledger_builder: ledger_builder }
  [@@deriving sexp]

  val of_transition_and_lb : external_transition -> ledger_builder -> t
end

module type Blockchain_state_intf = sig
  type ledger_builder_hash

  type ledger_hash

  type time

  type value [@@deriving sexp, bin_io]

  type var

  val create_value :
       ledger_builder_hash:ledger_builder_hash
    -> ledger_hash:ledger_hash
    -> timestamp:time
    -> value

  val ledger_builder_hash : value -> ledger_builder_hash

  val ledger_hash : value -> ledger_hash

  val timestamp : value -> time
end

module type Consensus_mechanism_intf = sig
  type protocol_state_proof

  type protocol_state_hash

  type blockchain_state

  type proof

  type ledger_builder_diff

  type transaction

  type ledger

  module Local_state : sig
    type t [@@deriving sexp]

    val create : unit -> t
  end

  module Consensus_transition_data : sig
    type value [@@deriving sexp]

    type var
  end

  module Consensus_state : sig
    type value

    type var
  end

  module Protocol_state : sig
    type value [@@deriving sexp, bin_io]

    type var

    val create_value :
         previous_state_hash:protocol_state_hash
      -> blockchain_state:blockchain_state
      -> consensus_state:Consensus_state.value
      -> value

    val previous_state_hash : value -> protocol_state_hash

    val blockchain_state : value -> blockchain_state

    val consensus_state : value -> Consensus_state.value

    val hash : value -> protocol_state_hash
  end

  module Snark_transition : sig
    type value

    type var

    val create_value :
         blockchain_state:blockchain_state
      -> consensus_data:Consensus_transition_data.value
      -> ledger_proof:proof option
      -> value

    val blockchain_state : value -> blockchain_state

    val consensus_data : value -> Consensus_transition_data.value
  end

  module Internal_transition : sig
    type t [@@deriving sexp]

    val create :
         snark_transition:Snark_transition.value
      -> ledger_builder_diff:ledger_builder_diff
      -> t

    val snark_transition : t -> Snark_transition.value

    val ledger_builder_diff : t -> ledger_builder_diff
  end

  module External_transition : sig
    type t [@@deriving sexp]

    val create :
         protocol_state:Protocol_state.value
      -> protocol_state_proof:protocol_state_proof
      -> ledger_builder_diff:ledger_builder_diff
      -> t

    val protocol_state : t -> Protocol_state.value

    val protocol_state_proof : t -> protocol_state_proof
  end

  val generate_transition :
       previous_protocol_state:Protocol_state.value
    -> blockchain_state:blockchain_state
    -> local_state:Local_state.t
    -> time:Int64.t
    -> transactions:transaction list
    -> ledger:ledger
    -> (Protocol_state.value * Consensus_transition_data.value) option
end

module type Time_close_validator_intf = sig
  type time

  val validate : time -> bool
end

module type Machine_intf = sig
  type t

  type state

  type transition

  type ledger_builder_transition

  module Event : sig
    type e = Found of transition | New_state of state

    type t = e * ledger_builder_transition
  end

  val current_state : t -> state

  val create : initial:state -> t

  val step : t -> transition -> t

  val drive :
       t
    -> scan:(   init:t
             -> f:(t -> Event.t -> t Deferred.t)
             -> t Linear_pipe.Reader.t)
    -> t Linear_pipe.Reader.t
end

module type Block_state_transition_proof_intf = sig
  type protocol_state

  type protocol_state_proof

  type internal_transition

  module Witness : sig
    type t =
      { old_state: protocol_state
      ; old_proof: protocol_state_proof
      ; transition: internal_transition }
  end

  (*
Blockchain_snark ~old ~nonce ~ledger_snark ~ledger_hash ~timestamp ~new_hash
  Input:
    old : Blockchain.t
    old_snark : proof
    nonce : int
    work_snark : proof
    ledger_hash : Ledger_hash.t
    timestamp : Time.t
    new_hash : State_hash.t
  Witness:
    transition : Transition.t
  such that
    the old_snark verifies against old
    new = update_with_asserts(old, nonce, timestamp, ledger_hash)
    hash(new) = new_hash
    the work_snark verifies against the old.ledger_hash and new_ledger_hash
    new.timestamp > old.timestamp
    hash(new_hash||nonce) < target(old.next_difficulty)
  *)
  (* TODO: Why is this taking new_state? *)

  val prove_zk_state_valid :
    Witness.t -> new_state:protocol_state -> protocol_state_proof Deferred.t
end

module Proof_carrying_data = struct
  type ('a, 'b) t = {data: 'a; proof: 'b} [@@deriving sexp, fields, bin_io]
end

module type Inputs_intf = sig
  module Time : Time_intf

  module Private_key : Private_key_intf

  module Compressed_public_key : Compressed_public_key_intf

  module Public_key :
    Public_key_intf
    with module Private_key := Private_key
     and module Compressed = Compressed_public_key

  module Transaction :
    Transaction_intf with type public_key := Public_key.Compressed.t

  module Fee_transfer :
    Fee_transfer_intf with type public_key := Public_key.Compressed.t

  module Super_transaction :
    Super_transaction_intf
    with type valid_transaction := Transaction.With_valid_signature.t
     and type fee_transfer := Fee_transfer.t

  module Ledger_hash : Ledger_hash_intf

  module Proof : sig
    type t
  end

  module Ledger_proof_statement :
    Ledger_proof_statement_intf with type ledger_hash := Ledger_hash.t

  module Ledger_proof :
    Ledger_proof_intf
    with type message := Fee.Unsigned.t * Public_key.Compressed.t
     and type ledger_hash := Ledger_hash.t
     and type statement := Ledger_proof_statement.t
     and type proof := Proof.t

  module Ledger_proof_verifier :
    Ledger_proof_verifier_intf
    with type message := Fee.Unsigned.t * Public_key.Compressed.t
     and type ledger_proof := Ledger_proof.t
     and type statement := Ledger_proof_statement.t

  module Ledger :
    Ledger_intf
    with type valid_transaction := Transaction.With_valid_signature.t
     and type super_transaction := Super_transaction.t
     and type ledger_hash := Ledger_hash.t

  module Ledger_builder_aux_hash : Ledger_builder_aux_hash_intf

  module Ledger_builder_hash :
    Ledger_builder_hash_intf
    with type ledger_builder_aux_hash := Ledger_builder_aux_hash.t
     and type ledger_hash := Ledger_hash.t

  (*
Bundle Snark:
   Input:
      l1 : Ledger_hash.t,
      l2 : Ledger_hash.t,
      fee_excess : Amount.Signed.t,
   Witness:
      t : Tagged_transaction.t
   such that
     applying [t] to ledger with merkle hash [l1] results in ledger with merkle hash [l2].

Merge Snark:
    Input:
      s1 : state
      s3 : state
      fee_excess_total : Amount.Signed.t
    Witness:
      s2 : state
      p12 : proof
      p23 : proof
      fee_excess12 : Amount.Signed.t
      fee_excess23 : Amount.Signed.t
    s.t.
      p12 verifies s1 -> s2 is a valid transition with fee_excess12
      p23 verifies s2 -> s3 is a valid transition with fee_excess23
      fee_excess_total = fee_excess12 + fee_excess23
  *)

  module Time_close_validator :
    Time_close_validator_intf with type time := Time.t

  module Completed_work :
    Completed_work_intf
    with type proof := Ledger_proof.t
     and type statement := Ledger_proof_statement.t
     and type public_key := Public_key.Compressed.t

  module Ledger_builder_diff :
    Ledger_builder_diff_intf
    with type transaction := Transaction.t
     and type transaction_with_valid_signature :=
                Transaction.With_valid_signature.t
     and type ledger_builder_hash := Ledger_builder_hash.t
     and type public_key := Public_key.Compressed.t
     and type completed_work := Completed_work.t
     and type completed_work_checked := Completed_work.Checked.t

  module Sparse_ledger : sig
    type t
  end

  module Ledger_builder :
    Ledger_builder_intf
    with type diff := Ledger_builder_diff.t
     and type valid_diff :=
                Ledger_builder_diff.With_valid_signatures_and_proofs.t
     and type ledger_builder_hash := Ledger_builder_hash.t
     and type ledger_builder_aux_hash := Ledger_builder_aux_hash.t
     and type ledger_hash := Ledger_hash.t
     and type public_key := Public_key.Compressed.t
     and type ledger := Ledger.t
     and type ledger_proof := Ledger_proof.t
     and type transaction_with_valid_signature :=
                Transaction.With_valid_signature.t
     and type statement := Completed_work.Statement.t
     and type completed_work := Completed_work.Checked.t
     and type sparse_ledger := Sparse_ledger.t
     and type ledger_proof_statement := Ledger_proof_statement.t
     and type ledger_proof_statement_set := Ledger_proof_statement.Set.t
     and type super_transaction := Super_transaction.t

  module Ledger_builder_transition :
    Ledger_builder_transition_intf
    with type diff := Ledger_builder_diff.t
     and type ledger_builder := Ledger_builder.t
     and type diff_with_valid_signatures_and_proofs :=
                Ledger_builder_diff.With_valid_signatures_and_proofs.t

  module Blockchain_state :
    Blockchain_state_intf
    with type ledger_builder_hash := Ledger_builder_hash.t
     and type ledger_hash := Ledger_hash.t
     and type time := Time.t

  module Protocol_state_hash : Protocol_state_hash_intf

  module Protocol_state_proof : Protocol_state_proof_intf

  module Consensus_mechanism :
    Consensus_mechanism_intf
    with type protocol_state_hash := Protocol_state_hash.t
     and type protocol_state_proof := Protocol_state_proof.t
     and type blockchain_state := Blockchain_state.value
     and type proof := Proof.t
     and type ledger_builder_diff := Ledger_builder_diff.t
     and type transaction := Transaction.t
     and type ledger := Ledger.t

  module Tip :
    Tip_intf
    with type ledger_builder := Ledger_builder.t
     and type protocol_state := Consensus_mechanism.Protocol_state.value
     and type protocol_state_proof := Protocol_state_proof.t
     and type external_transition := Consensus_mechanism.External_transition.t
end

module Make
    (Inputs : Inputs_intf)
    (Block_state_transition_proof : Block_state_transition_proof_intf
                                    with type protocol_state :=
                                                Inputs.Consensus_mechanism.
                                                Protocol_state.value
                                     and type protocol_state_proof :=
                                                Inputs.Protocol_state_proof.t
                                     and type internal_transition :=
                                                Inputs.Consensus_mechanism.
                                                Internal_transition.t) =
struct
  open Inputs

  module Proof_carrying_state = struct
    type t =
      ( Consensus_mechanism.Protocol_state.value
      , Protocol_state_proof.t )
      Proof_carrying_data.t
  end

  module Event = struct
    type t =
      | Found of Consensus_mechanism.Internal_transition.t
      | New_state of Proof_carrying_state.t * Ledger_builder_transition.t
  end

  type t = {state: Proof_carrying_state.t} [@@deriving fields]
end
