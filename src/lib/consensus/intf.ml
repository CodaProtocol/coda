open Core_kernel
open Coda_numbers
open Async
open Currency
open Fold_lib
open Tuple_lib
open Signature_lib
open Coda_base

(** Constants are defined with a single letter (latin or greek) based on
 * their usage in the Ouroboros suite of papers *)
module type Constants_intf = sig
  (** The timestamp for the genesis block *)
  val genesis_state_timestamp : Coda_base.Block_time.t

  (** [k] is the number of blocks required to reach finality *)
  val k : int

  (** The amount of money minted and given to the proposer whenever a block
   * is created *)
  val coinbase : Currency.Amount.t

  val block_window_duration_ms : int

  (** The window duration in which blocks are created *)
  val block_window_duration : Coda_base.Block_time.Span.t

  (** [delta] is the number of slots in the valid window for receiving blocks over the network *)
  val delta : int

  (** [c] is the number of slots in which we can probalistically expect at least 1
   * block. In sig, it's exactly 1 as blocks should be produced every slot. *)
  val c : int

  val inactivity_secs : int

  (** Number of slots in one epoch *)
  val slots_per_epoch : Unsigned.UInt32.t
end

module type Blockchain_state_intf = sig
  module Poly : sig
    module Stable : sig
      module V1 : sig
        type ('staged_ledger_hash, 'snarked_ledger_hash, 'time) t
        [@@deriving sexp, bin_io]
      end
    end

    type ('staged_ledger_hash, 'snarked_ledger_hash, 'time) t =
      ('staged_ledger_hash, 'snarked_ledger_hash, 'time) Stable.V1.t
    [@@deriving sexp]
  end

  module Value : sig
    module Stable : sig
      module V1 : sig
        type t =
          (Staged_ledger_hash.t, Frozen_ledger_hash.t, Block_time.t) Poly.t
        [@@deriving sexp, bin_io]
      end
    end

    type t = Stable.V1.t [@@deriving sexp]
  end

  type var =
    ( Staged_ledger_hash.var
    , Frozen_ledger_hash.var
    , Block_time.Unpacked.var )
    Poly.t

  val create_value :
       staged_ledger_hash:Staged_ledger_hash.t
    -> snarked_ledger_hash:Frozen_ledger_hash.t
    -> timestamp:Block_time.t
    -> Value.t

  val staged_ledger_hash :
    ('staged_ledger_hash, _, _) Poly.t -> 'staged_ledger_hash

  val snarked_ledger_hash :
    (_, 'frozen_ledger_hash, _) Poly.t -> 'frozen_ledger_hash

  val timestamp : (_, _, 'time) Poly.t -> 'time
end

module type Protocol_state_intf = sig
  type blockchain_state

  type blockchain_state_var

  type consensus_state

  type consensus_state_var

  module Poly : sig
    module Stable : sig
      module V1 : sig
        type ('state_hash, 'body) t
        [@@deriving eq, bin_io, hash, sexp, to_yojson, version]
      end

      module Latest = V1
    end

    type ('state_hash, 'body) t = ('state_hash, 'body) Stable.Latest.t
    [@@deriving sexp]
  end

  module Body : sig
    module Poly : sig
      module Stable : sig
        module V1 : sig
          type ('blockchain_state, 'consensus_state) t
          [@@deriving bin_io, sexp]
        end

        module Latest = V1
      end

      type ('blockchain_state, 'consensus_state) t =
        ('blockchain_state, 'consensus_state) Stable.V1.t
      [@@deriving sexp]
    end

    module Value : sig
      module Stable : sig
        module V1 : sig
          type t = (blockchain_state, consensus_state) Poly.Stable.V1.t
          [@@deriving bin_io, sexp, to_yojson]
        end
      end
    end

    type var = (blockchain_state_var, consensus_state_var) Poly.Stable.V1.t
  end

  module Value : sig
    module Stable : sig
      module V1 : sig
        type t = (State_hash.t, Body.Value.Stable.V1.t) Poly.Stable.V1.t
        [@@deriving sexp, bin_io, eq, compare]
      end

      module Latest = V1
    end

    (* bin_io omitted *)
    type t = Stable.V1.t [@@deriving sexp, eq, compare]
  end

  type var = (State_hash.var, Body.var) Poly.t

  val create_value :
       previous_state_hash:State_hash.t
    -> blockchain_state:blockchain_state
    -> consensus_state:consensus_state
    -> Value.t

  val previous_state_hash : ('state_hash, _) Poly.t -> 'state_hash

  val body : (_, 'body) Poly.t -> 'body

  val blockchain_state :
    (_, ('blockchain_state, _) Body.Poly.t) Poly.t -> 'blockchain_state

  val consensus_state :
    (_, (_, 'consensus_state) Body.Poly.t) Poly.t -> 'consensus_state

  val hash : Value.t -> State_hash.t
end

module type Snark_transition_intf = sig
  type blockchain_state_var

  type consensus_transition_var

  module Poly : sig
    type ( 'blockchain_state
         , 'consensus_transition
         , 'sok_digest
         , 'amount
         , 'public_key )
         t
    [@@deriving sexp]
  end

  module Value : sig
    type t [@@deriving sexp]
  end

  type var =
    ( blockchain_state_var
    , consensus_transition_var
    , Sok_message.Digest.Checked.t
    , Amount.var
    , Public_key.Compressed.var )
    Poly.t

  val consensus_transition :
    (_, 'consensus_transition, _, _, _) Poly.t -> 'consensus_transition
end

module type State_hooks_intf = sig
  type consensus_state

  type consensus_state_var

  type consensus_transition

  type proposal_data

  type blockchain_state

  type protocol_state

  type protocol_state_var

  type snark_transition_var

  (**
   * Generate a new protocol state and consensus specific transition data
   * for a new transition. Called from the proposer in order to generate
   * a new transition to propose to the network. Returns `None` if a new
   * transition cannot be generated.
  *)
  val generate_transition :
       previous_protocol_state:protocol_state
    -> blockchain_state:blockchain_state
    -> current_time:Unix_timestamp.t
    -> proposal_data:proposal_data
    -> transactions:Coda_base.User_command.t list
    -> snarked_ledger_hash:Coda_base.Frozen_ledger_hash.t
    -> supply_increase:Currency.Amount.t
    -> logger:Logger.t
    -> protocol_state * consensus_transition

  (**
   * Create a constrained, checked var for the next consensus state of
   * a given consensus state and snark transition.
  *)
  val next_state_checked :
       prev_state:protocol_state_var
    -> prev_state_hash:Coda_base.State_hash.var
    -> snark_transition_var
    -> Currency.Amount.var
    -> ( [`Success of Snark_params.Tick.Boolean.var] * consensus_state_var
       , _ )
       Snark_params.Tick.Checked.t

  module For_tests : sig
    val gen_consensus_state :
         gen_slot_advancement:int Quickcheck.Generator.t
      -> (   previous_protocol_state:( protocol_state
                                     , Coda_base.State_hash.t )
                                     With_hash.t
          -> snarked_ledger_hash:Coda_base.Frozen_ledger_hash.t
          -> consensus_state)
         Quickcheck.Generator.t
  end
end

module type S = sig
  val name : string

  (** Return a string that tells a human what the consensus view of an instant in time is.
    *
    * This is mostly useful for PoStake and other consensus mechanisms that have their own
    * notions of time.
  *)
  val time_hum : Coda_base.Block_time.t -> string

  module Constants : Constants_intf

  (** from postake *)
  val epoch_size : int

  module Configuration : sig
    type t =
      { delta: int
      ; k: int
      ; c: int
      ; c_times_k: int
      ; slots_per_epoch: int
      ; slot_duration: int
      ; epoch_duration: int
      ; acceptable_network_delay: int }
    [@@deriving yojson, bin_io, fields]

    val t : t
  end

  module Data : sig
    module Local_state : sig
      type t [@@deriving sexp, to_yojson]

      val create : Signature_lib.Public_key.Compressed.Set.t -> t

      val current_proposers : t -> Signature_lib.Public_key.Compressed.Set.t

      (** Swap in a new set of proposers and invalidate and/or recompute cached
       * data *)
      val proposer_swap :
           t
        -> Signature_lib.Public_key.Compressed.Set.t
        -> Coda_base.Block_time.t
        -> unit
    end

    module Prover_state : sig
      type t [@@deriving to_yojson, sexp]

      module Stable :
        sig
          module V1 : sig
            type t [@@deriving bin_io, sexp, to_yojson, version]
          end

          module Latest = V1
        end
        with type V1.t = t

      val precomputed_handler : Snark_params.Tick.Handler.t Lazy.t

      val handler :
           t
        -> pending_coinbase:Coda_base.Pending_coinbase_witness.t
        -> Snark_params.Tick.Handler.t
    end

    module Consensus_transition : sig
      module Value : sig
        module Stable : sig
          module V1 : sig
            type t [@@deriving sexp, bin_io, to_yojson, version]
          end
        end

        type t = Stable.V1.t [@@deriving to_yojson, sexp]
      end

      include Snark_params.Tick.Snarkable.S with type value := Value.t

      val genesis : Value.t
    end

    module Consensus_state : sig
      module Value : sig
        (* bin_io omitted *)
        type t [@@deriving hash, eq, compare, sexp, to_yojson]

        module Stable :
          sig
            module V1 : sig
              type t
              [@@deriving hash, eq, compare, bin_io, sexp, to_yojson, version]
            end
          end
          with type V1.t = t
      end

      type display [@@deriving yojson]

      include Snark_params.Tick.Snarkable.S with type value := Value.t

      val negative_one : Value.t Lazy.t

      val create_genesis_from_transition :
           negative_one_protocol_state_hash:Coda_base.State_hash.t
        -> consensus_transition:Consensus_transition.Value.t
        -> Value.t

      val create_genesis :
        negative_one_protocol_state_hash:Coda_base.State_hash.t -> Value.t

      val length_in_triples : int

      val var_to_triples :
           var
        -> ( Snark_params.Tick.Boolean.var Triple.t list
           , _ )
           Snark_params.Tick.Checked.t

      val fold : Value.t -> bool Triple.t Fold.t

      val block_height : Value.t -> Length.t

      val time_hum : Value.t -> string

      val to_lite : (Value.t -> Lite_base.Consensus_state.t) option

      val display : Value.t -> display

      val network_delay : Configuration.t -> int

      val curr_epoch : Value.t -> Epoch.t

      val curr_slot : Value.t -> Slot.t
    end

    module Proposal_data : sig
      type t

      val prover_state : t -> Prover_state.t
    end
  end

  module Hooks : sig
    open Data

    module Rpcs : sig
      val implementations :
           logger:Logger.t
        -> local_state:Local_state.t
        -> Host_and_port.t Rpc.Implementation.t list
    end

    (* Check whether we are in the genesis epoch *)
    val is_genesis : Coda_base.Block_time.t -> bool

    (**
     * Check that a consensus state was received at a valid time.
    *)
    val received_at_valid_time :
         Consensus_state.Value.t
      -> time_received:Unix_timestamp.t
      -> (unit, [`Too_early | `Too_late of int64]) result

    (**
     * Select between two ledger builder controller tips given the consensus
     * states for the two tips. Returns `\`Keep` if the first tip should be
     * kept, or `\`Take` if the second tip should be taken instead.
    *)
    val select :
         existing:Consensus_state.Value.t
      -> candidate:Consensus_state.Value.t
      -> logger:Logger.t
      -> [`Keep | `Take]

    (**
     * Determine if and when to perform the next transition proposal. Either
     * informs the callee to check again at some time in the future, or to
     * schedule a proposal with some particular keypair at some time in the
     * future, or to propose now with some keypair and check again some time in
     * the future.
    *)
    val next_proposal :
         Unix_timestamp.t
      -> Consensus_state.Value.t
      -> local_state:Local_state.t
      -> keypairs:Signature_lib.Keypair.And_compressed_pk.Set.t
      -> logger:Logger.t
      -> [ `Check_again of Unix_timestamp.t
         | `Propose_now of Signature_lib.Keypair.t * Proposal_data.t
         | `Propose of
           Unix_timestamp.t * Signature_lib.Keypair.t * Proposal_data.t ]

    (**
     * A hook for managing local state when the locked tip is updated.
    *)
    val frontier_root_transition :
         Consensus_state.Value.t
      -> Consensus_state.Value.t
      -> local_state:Local_state.t
      -> snarked_ledger:Coda_base.Ledger.Any_ledger.witness
      -> unit

    (**
       * Indicator of when we should bootstrap
    *)
    val should_bootstrap :
         existing:Consensus_state.Value.t
      -> candidate:Consensus_state.Value.t
      -> logger:Logger.t
      -> bool

    (** Data needed to synchronize the local state. *)
    type local_state_sync [@@deriving to_yojson]

    (**
      * Predicate indicating whether or not the local state requires synchronization.
    *)
    val required_local_state_sync :
         consensus_state:Consensus_state.Value.t
      -> local_state:Local_state.t
      -> local_state_sync Non_empty_list.t option

    (**
      * Synchronize local state over the network.
    *)
    val sync_local_state :
         logger:Logger.t
      -> trust_system:Trust_system.t
      -> local_state:Local_state.t
      -> random_peers:(int -> Network_peer.Peer.t list)
      -> query_peer:Network_peer.query_peer
      -> local_state_sync Non_empty_list.t
      -> unit Deferred.Or_error.t

    module type State_hooks_intf =
      State_hooks_intf
      with type consensus_state := Consensus_state.Value.t
       and type consensus_state_var := Consensus_state.var
       and type consensus_transition := Consensus_transition.Value.t
       and type proposal_data := Proposal_data.t

    module Make_state_hooks
        (Blockchain_state : Blockchain_state_intf)
        (Protocol_state : Protocol_state_intf
                          with type blockchain_state :=
                                      Blockchain_state.Value.t
                           and type blockchain_state_var :=
                                      Blockchain_state.var
                           and type consensus_state := Consensus_state.Value.t
                           and type consensus_state_var := Consensus_state.var)
        (Snark_transition : Snark_transition_intf
                            with type blockchain_state_var :=
                                        Blockchain_state.var
                             and type consensus_transition_var :=
                                        Consensus_transition.var) :
      State_hooks_intf
      with type blockchain_state := Blockchain_state.Value.t
       and type protocol_state := Protocol_state.Value.t
       and type protocol_state_var := Protocol_state.var
       and type snark_transition_var := Snark_transition.var
  end
end
