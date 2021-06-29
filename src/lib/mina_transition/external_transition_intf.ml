open Core_kernel
open Async_kernel
open Mina_base
open Mina_state
open Signature_lib

module type External_transition_common_intf = sig
  type t

  type protocol_version_status =
    {valid_current: bool; valid_next: bool; matches_daemon: bool}

  val protocol_version_status : t -> protocol_version_status

  val protocol_state : t -> Protocol_state.Value.t

  val protocol_state_proof : t -> Proof.t

  val blockchain_state : t -> Blockchain_state.Value.t

  val blockchain_length : t -> Unsigned.UInt32.t

  val consensus_state : t -> Consensus.Data.Consensus_state.Value.t

  val staged_ledger_diff : t -> Staged_ledger_diff.t

  val state_hash : t -> State_hash.t

  val parent_hash : t -> State_hash.t

  val consensus_time_produced_at : t -> Consensus.Data.Consensus_time.t

  val block_producer : t -> Public_key.Compressed.t

  val block_winner : t -> Public_key.Compressed.t

  val coinbase_receiver : t -> Public_key.Compressed.t

  val supercharge_coinbase : t -> bool

  val transactions :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> t
    -> Transaction.t With_status.t list

  val commands : t -> User_command.t With_status.t list

  val payments : t -> Signed_command.t With_status.t list

  val completed_works : t -> Transaction_snark_work.t list

  val global_slot : t -> Unsigned.uint32

  val delta_transition_chain_proof : t -> State_hash.t * State_body_hash.t list

  val current_protocol_version : t -> Protocol_version.t

  val proposed_protocol_version_opt : t -> Protocol_version.t option

  val broadcast : t -> unit

  val don't_broadcast : t -> unit

  val poke_validation_callback : t -> Mina_net2.Validation_callback.t -> unit
end

module type External_transition_base_intf = sig
  type t [@@deriving sexp, to_yojson, eq]

  [%%versioned:
  module Stable : sig
    [@@@no_toplevel_latest_type]

    module V1 : sig
      type nonrec t = t [@@deriving sexp]
    end
  end]

  include External_transition_common_intf with type t := t
end

module type S = sig
  include External_transition_base_intf

  type external_transition = t

  module Precomputed_block : sig
    module Proof : sig
      (* Proof with overridden base64-encoding *)
      type t = Proof.t [@@deriving sexp, yojson]

      val to_bin_string : t -> string

      val of_bin_string : string -> t
    end

    type t =
      { scheduled_time: Block_time.Time.t
      ; protocol_state: Protocol_state.value
      ; protocol_state_proof: Proof.t
      ; staged_ledger_diff: Staged_ledger_diff.t
      ; delta_transition_chain_proof:
          Frozen_ledger_hash.t * Frozen_ledger_hash.t list }
    [@@deriving sexp, yojson]

    [%%versioned:
    module Stable : sig
      [@@@no_toplevel_latest_type]

      module V1 : sig
        type nonrec t = t =
          { scheduled_time: Block_time.Stable.V1.t
          ; protocol_state: Protocol_state.Value.Stable.V1.t
          ; protocol_state_proof: Mina_base.Proof.Stable.V1.t
          ; staged_ledger_diff: Staged_ledger_diff.Stable.V1.t
          ; delta_transition_chain_proof:
              Frozen_ledger_hash.Stable.V1.t
              * Frozen_ledger_hash.Stable.V1.t list }

        val to_latest : t -> t
      end
    end]

    val of_external_transition :
      scheduled_time:Block_time.Time.t -> external_transition -> t
  end

  module Validation : sig
    type ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         t =
      'time_received
      * 'genesis_state
      * 'proof
      * 'delta_transition_chain
      * 'frontier_dependencies
      * 'staged_ledger_diff
      * 'protocol_versions
      constraint 'time_received = [`Time_received] * (unit, _) Truth.t
      constraint 'genesis_state = [`Genesis_state] * (unit, _) Truth.t
      constraint 'proof = [`Proof] * (unit, _) Truth.t
      constraint
        'delta_transition_chain =
        [`Delta_transition_chain] * (State_hash.t Non_empty_list.t, _) Truth.t
      constraint
        'frontier_dependencies =
        [`Frontier_dependencies] * (unit, _) Truth.t
      constraint
        'staged_ledger_diff =
        [`Staged_ledger_diff] * (unit, _) Truth.t
      constraint 'protocol_versions = [`Protocol_versions] * (unit, _) Truth.t

    type fully_invalid =
      ( [`Time_received] * unit Truth.false_t
      , [`Genesis_state] * unit Truth.false_t
      , [`Proof] * unit Truth.false_t
      , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.false_t
      , [`Frontier_dependencies] * unit Truth.false_t
      , [`Staged_ledger_diff] * unit Truth.false_t
      , [`Protocol_versions] * unit Truth.false_t )
      t

    type fully_valid =
      ( [`Time_received] * unit Truth.true_t
      , [`Genesis_state] * unit Truth.true_t
      , [`Proof] * unit Truth.true_t
      , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.true_t
      , [`Frontier_dependencies] * unit Truth.true_t
      , [`Staged_ledger_diff] * unit Truth.true_t
      , [`Protocol_versions] * unit Truth.true_t )
      t

    type initial_valid =
      ( [`Time_received] * unit Truth.true_t
      , [`Genesis_state] * unit Truth.true_t
      , [`Proof] * unit Truth.true_t
      , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.true_t
      , [`Frontier_dependencies] * unit Truth.false_t
      , [`Staged_ledger_diff] * unit Truth.false_t
      , [`Protocol_versions] * unit Truth.true_t )
      t

    type almost_valid =
      ( [`Time_received] * unit Truth.true_t
      , [`Genesis_state] * unit Truth.true_t
      , [`Proof] * unit Truth.true_t
      , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.true_t
      , [`Frontier_dependencies] * unit Truth.true_t
      , [`Staged_ledger_diff] * unit Truth.false_t
      , [`Protocol_versions] * unit Truth.true_t )
      t

    type ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         with_transition =
      (external_transition, State_hash.t) With_hash.t
      * ( 'time_received
        , 'genesis_state
        , 'proof
        , 'delta_transition_chain
        , 'frontier_dependencies
        , 'staged_ledger_diff
        , 'protocol_versions )
        t

    val fully_invalid : fully_invalid

    val wrap :
         (external_transition, State_hash.t) With_hash.t
      -> (external_transition, State_hash.t) With_hash.t * fully_invalid

    val extract_delta_transition_chain_witness :
         ( 'time_received
         , 'genesis_state
         , 'proof
         , [`Delta_transition_chain]
           * State_hash.t Non_empty_list.t Truth.true_t
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         t
      -> State_hash.t Non_empty_list.t

    val reset_frontier_dependencies_validation :
         ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , [`Frontier_dependencies] * unit Truth.true_t
         , 'staged_ledger_diff
         , 'protocol_versions )
         with_transition
      -> ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , [`Frontier_dependencies] * unit Truth.false_t
         , 'staged_ledger_diff
         , 'protocol_versions )
         with_transition

    val reset_staged_ledger_diff_validation :
         ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , [`Staged_ledger_diff] * unit Truth.true_t
         , 'protocol_versions )
         with_transition
      -> ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , [`Staged_ledger_diff] * unit Truth.false_t
         , 'protocol_versions )
         with_transition

    val forget_validation :
         ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         with_transition
      -> external_transition

    val forget_validation_with_hash :
         ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         with_transition
      -> (external_transition, State_hash.t) With_hash.t
  end

  module Initial_validated : sig
    type t =
      (external_transition, State_hash.t) With_hash.t
      * Validation.initial_valid
    [@@deriving compare]

    include External_transition_common_intf with type t := t
  end

  module Almost_validated : sig
    type t =
      (external_transition, State_hash.t) With_hash.t * Validation.almost_valid
    [@@deriving compare]

    include External_transition_common_intf with type t := t
  end

  module Validated : sig
    type t =
      (external_transition, State_hash.t) With_hash.t * Validation.fully_valid
    [@@deriving compare]

    val erase :
         t
      -> ( Stable.Latest.t
         , State_hash.Stable.Latest.t )
         With_hash.Stable.Latest.t
         * State_hash.Stable.Latest.t Non_empty_list.Stable.Latest.t

    val create_unsafe :
      external_transition -> [`I_swear_this_is_safe_see_my_comment of t]

    include External_transition_base_intf with type t := t

    val commands : t -> User_command.Valid.t With_status.t list

    val to_initial_validated : t -> Initial_validated.t
  end

  val create :
       protocol_state:Protocol_state.Value.t
    -> protocol_state_proof:Proof.t
    -> staged_ledger_diff:Staged_ledger_diff.t
    -> delta_transition_chain_proof:State_hash.t * State_body_hash.t list
    -> validation_callback:Mina_net2.Validation_callback.t
    -> ?proposed_protocol_version_opt:Protocol_version.t
    -> unit
    -> t

  val genesis : precomputed_values:Precomputed_values.t -> Validated.t

  module For_tests : sig
    val create :
         protocol_state:Protocol_state.Value.t
      -> protocol_state_proof:Proof.t
      -> staged_ledger_diff:Staged_ledger_diff.t
      -> delta_transition_chain_proof:State_hash.t * State_body_hash.t list
      -> validation_callback:Mina_net2.Validation_callback.t
      -> ?proposed_protocol_version_opt:Protocol_version.t
      -> unit
      -> t

    val genesis : precomputed_values:Precomputed_values.t -> Validated.t
  end

  val timestamp : t -> Block_time.t

  val skip_time_received_validation :
       [`This_transition_was_not_received_via_gossip]
    -> ( [`Time_received] * unit Truth.false_t
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
    -> ( [`Time_received] * unit Truth.true_t
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition

  val validate_time_received :
       precomputed_values:Precomputed_values.t
    -> ( [`Time_received] * unit Truth.false_t
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
    -> time_received:Block_time.t
    -> ( ( [`Time_received] * unit Truth.true_t
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         Validation.with_transition
       , [> `Invalid_time_received of [`Too_early | `Too_late of int64]] )
       Result.t

  val skip_proof_validation :
       [`This_transition_was_generated_internally]
    -> ( 'time_received
       , 'genesis_state
       , [`Proof] * unit Truth.false_t
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
    -> ( 'time_received
       , 'genesis_state
       , [`Proof] * unit Truth.true_t
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition

  val skip_delta_transition_chain_validation :
       [`This_transition_was_not_received_via_gossip]
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , [`Delta_transition_chain]
         * State_hash.t Non_empty_list.t Truth.false_t
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.true_t
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition

  val skip_genesis_protocol_state_validation :
       [`This_transition_was_generated_internally]
    -> ( 'time_received
       , [`Genesis_state] * unit Truth.false_t
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
    -> ( 'time_received
       , [`Genesis_state] * unit Truth.true_t
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition

  val validate_genesis_protocol_state :
       genesis_state_hash:State_hash.t
    -> ( 'time_received
       , [`Genesis_state] * unit Truth.false_t
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
    -> ( ( 'time_received
         , [`Genesis_state] * unit Truth.true_t
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         Validation.with_transition
       , [> `Invalid_genesis_protocol_state] )
       Result.t

  val validate_proofs :
       ( 'time_received
       , 'genesis_state
       , [`Proof] * unit Truth.false_t
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
       list
    -> verifier:Verifier.t
    -> ( ( 'time_received
         , 'genesis_state
         , [`Proof] * unit Truth.true_t
         , 'delta_transition_chain
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         Validation.with_transition
         list
       , [> `Invalid_proof | `Verifier_error of Error.t] )
       Deferred.Result.t

  val validate_delta_transition_chain :
       ( 'time_received
       , 'genesis_state
       , 'proof
       , [`Delta_transition_chain]
         * State_hash.t Non_empty_list.t Truth.false_t
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
    -> ( ( 'time_received
         , 'genesis_state
         , 'proof
         , [`Delta_transition_chain]
           * State_hash.t Non_empty_list.t Truth.true_t
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'protocol_versions )
         Validation.with_transition
       , [> `Invalid_delta_transition_chain_proof] )
       Result.t

  val validate_protocol_versions :
       ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , [`Protocol_versions] * unit Truth.false_t )
       Validation.with_transition
    -> ( ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , [`Protocol_versions] * unit Truth.true_t )
         Validation.with_transition
       , [> `Invalid_protocol_version | `Mismatched_protocol_version] )
       Result.t

  (* This functor is necessary to break the dependency cycle between the Transition_fronter and the External_transition *)
  module Transition_frontier_validation (Transition_frontier : sig
    type t

    module Breadcrumb : sig
      type t

      val validated_transition : t -> Validated.t
    end

    val root : t -> Breadcrumb.t

    val find : t -> State_hash.t -> Breadcrumb.t option
  end) : sig
    val validate_frontier_dependencies :
         ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , [`Frontier_dependencies] * unit Truth.false_t
         , 'staged_ledger_diff
         , 'protocol_versions )
         Validation.with_transition
      -> consensus_constants:Consensus.Constants.t
      -> logger:Logger.t
      -> frontier:Transition_frontier.t
      -> ( ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , [`Frontier_dependencies] * unit Truth.true_t
           , 'staged_ledger_diff
           , 'protocol_versions )
           Validation.with_transition
         , [> `Already_in_frontier
           | `Parent_missing_from_frontier
           | `Not_selected_over_frontier_root ] )
         Result.t
  end

  val skip_frontier_dependencies_validation :
       [ `This_transition_belongs_to_a_detached_subtree
       | `This_transition_was_loaded_from_persistence ]
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , [`Frontier_dependencies] * unit Truth.false_t
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , [`Frontier_dependencies] * unit Truth.true_t
       , 'staged_ledger_diff
       , 'protocol_versions )
       Validation.with_transition

  val validate_staged_ledger_hash :
       [`Staged_ledger_already_materialized of Staged_ledger_hash.t]
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , [`Staged_ledger_diff] * unit Truth.false_t
       , 'protocol_versions )
       Validation.with_transition
    -> ( ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , [`Staged_ledger_diff] * unit Truth.true_t
         , 'protocol_versions )
         Validation.with_transition
       , [> `Staged_ledger_hash_mismatch] )
       Result.t

  val skip_staged_ledger_diff_validation :
       [`This_transition_has_a_trusted_staged_ledger]
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , [`Staged_ledger_diff] * unit Truth.false_t
       , 'protocol_versions )
       Validation.with_transition
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , [`Staged_ledger_diff] * unit Truth.true_t
       , 'protocol_versions )
       Validation.with_transition

  val skip_protocol_versions_validation :
       [`This_transition_has_valid_protocol_versions]
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , [`Protocol_versions] * unit Truth.false_t )
       Validation.with_transition
    -> ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , [`Protocol_versions] * unit Truth.true_t )
       Validation.with_transition

  module Staged_ledger_validation : sig
    val validate_staged_ledger_diff :
         ?skip_staged_ledger_verification:[`All | `Proofs]
      -> ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , [`Staged_ledger_diff] * unit Truth.false_t
         , 'protocol_versions )
         Validation.with_transition
      -> logger:Logger.t
      -> precomputed_values:Precomputed_values.t
      -> verifier:Verifier.t
      -> parent_staged_ledger:Staged_ledger.t
      -> parent_protocol_state:Protocol_state.value
      -> ( [`Just_emitted_a_proof of bool]
           * [ `External_transition_with_validation of
               ( 'time_received
               , 'genesis_state
               , 'proof
               , 'delta_transition_chain
               , 'frontier_dependencies
               , [`Staged_ledger_diff] * unit Truth.true_t
               , 'protocol_versions )
               Validation.with_transition ]
           * [`Staged_ledger of Staged_ledger.t]
         , [ `Invalid_staged_ledger_diff of
             [ `Incorrect_target_staged_ledger_hash
             | `Incorrect_target_snarked_ledger_hash ]
             list
           | `Staged_ledger_application_failed of
             Staged_ledger.Staged_ledger_error.t ] )
         Deferred.Result.t
  end
end
