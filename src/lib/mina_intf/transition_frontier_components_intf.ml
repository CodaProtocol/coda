open Core
open Async_kernel
open Pipe_lib
open Cache_lib
open Mina_base
open Mina_transition
open Network_peer

module type Transition_handler_validator_intf = sig
  type unprocessed_transition_cache

  type transition_frontier

  val run :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> time_controller:Block_time.Controller.t
    -> frontier:transition_frontier
    -> transition_reader:External_transition.Initial_validated.t
                         Envelope.Incoming.t
                         Strict_pipe.Reader.t
    -> valid_transition_writer:( ( External_transition.Initial_validated.t
                                   Envelope.Incoming.t
                                 , State_hash.t )
                                 Cached.t
                               , Strict_pipe.crash Strict_pipe.buffered
                               , unit )
                               Strict_pipe.Writer.t
    -> unprocessed_transition_cache:unprocessed_transition_cache
    -> unit

  val validate_transition :
       logger:Logger.t
    -> frontier:transition_frontier
    -> unprocessed_transition_cache:unprocessed_transition_cache
    -> External_transition.Initial_validated.t Envelope.Incoming.t
    -> ( ( External_transition.Initial_validated.t Envelope.Incoming.t
         , State_hash.t )
         Cached.t
       , [> `In_frontier of State_hash.t
         | `In_process of State_hash.t Cache_lib.Intf.final_state
         | `Disconnected ] )
       Result.t
end

module type Breadcrumb_builder_intf = sig
  type transition_frontier

  type transition_frontier_breadcrumb

  val build_subtrees_of_breadcrumbs :
       logger:Logger.t
    -> verifier:Verifier.t
    -> trust_system:Trust_system.t
    -> frontier:transition_frontier
    -> initial_hash:State_hash.t
    -> ( External_transition.Initial_validated.t Envelope.Incoming.t
       , State_hash.t )
       Cached.t
       Rose_tree.t
       List.t
    -> (transition_frontier_breadcrumb, State_hash.t) Cached.t Rose_tree.t
       List.t
       Deferred.Or_error.t
end

module type Transition_handler_processor_intf = sig
  type transition_frontier

  type transition_frontier_breadcrumb

  val run :
       logger:Logger.t
    -> verifier:Verifier.t
    -> trust_system:Trust_system.t
    -> time_controller:Block_time.Controller.t
    -> frontier:transition_frontier
    -> primary_transition_reader:( External_transition.Initial_validated.t
                                   Envelope.Incoming.t
                                 , State_hash.t )
                                 Cached.t
                                 Strict_pipe.Reader.t
    -> producer_transition_reader:transition_frontier_breadcrumb
                                  Strict_pipe.Reader.t
    -> clean_up_catchup_scheduler:unit Ivar.t
    -> catchup_job_writer:( State_hash.t
                            * ( External_transition.Initial_validated.t
                                Envelope.Incoming.t
                              , State_hash.t )
                              Cached.t
                              Rose_tree.t
                              list
                          , Strict_pipe.crash Strict_pipe.buffered
                          , unit )
                          Strict_pipe.Writer.t
    -> catchup_breadcrumbs_reader:( ( transition_frontier_breadcrumb
                                    , State_hash.t )
                                    Cached.t
                                    Rose_tree.t
                                    list
                                  * [ `Ledger_catchup of unit Ivar.t
                                    | `Catchup_scheduler ] )
                                  Strict_pipe.Reader.t
    -> catchup_breadcrumbs_writer:( ( transition_frontier_breadcrumb
                                    , State_hash.t )
                                    Cached.t
                                    Rose_tree.t
                                    list
                                    * [ `Ledger_catchup of unit Ivar.t
                                      | `Catchup_scheduler ]
                                  , Strict_pipe.crash Strict_pipe.buffered
                                  , unit )
                                  Strict_pipe.Writer.t
    -> processed_transition_writer:( [ `Transition of
                                       External_transition.Validated.t ]
                                     * [ `Source of
                                         [`Gossip | `Catchup | `Internal] ]
                                   , Strict_pipe.crash Strict_pipe.buffered
                                   , unit )
                                   Strict_pipe.Writer.t
    -> unit
end

module type Unprocessed_transition_cache_intf = sig
  type t

  val create : logger:Logger.t -> t

  val register_exn :
       t
    -> External_transition.Initial_validated.t Envelope.Incoming.t
    -> ( External_transition.Initial_validated.t Envelope.Incoming.t
       , State_hash.t )
       Cached.t
end

module type Transition_handler_intf = sig
  type transition_frontier

  type transition_frontier_breadcrumb

  module Unprocessed_transition_cache : Unprocessed_transition_cache_intf

  module Breadcrumb_builder :
    Breadcrumb_builder_intf
    with type transition_frontier := transition_frontier
     and type transition_frontier_breadcrumb := transition_frontier_breadcrumb

  module Validator :
    Transition_handler_validator_intf
    with type unprocessed_transition_cache := Unprocessed_transition_cache.t
     and type transition_frontier := transition_frontier

  module Processor :
    Transition_handler_processor_intf
    with type transition_frontier := transition_frontier
     and type transition_frontier_breadcrumb := transition_frontier_breadcrumb
end

(** Interface that allows a peer to prove their best_tip in the
    transition_frontier *)
module type Best_tip_prover_intf = sig
  type transition_frontier

  val prove :
       logger:Logger.t
    -> transition_frontier
    -> ( (External_transition.t, State_hash.t) With_hash.t
       , State_body_hash.t list * External_transition.t )
       Proof_carrying_data.t
       option

  val verify :
       verifier:Verifier.t
    -> genesis_constants:Genesis_constants.t
    -> precomputed_values:Precomputed_values.t
    -> ( External_transition.t
       , State_body_hash.t list * External_transition.t )
       Proof_carrying_data.t
    -> ( [`Root of External_transition.Initial_validated.t]
       * [`Best_tip of External_transition.Initial_validated.t] )
       Deferred.Or_error.t
end

(** Interface that allows a peer to prove their best_tip in the
    transition_frontier based off of a condition on the consensus_state from
    the requesting node *)
module type Consensus_best_tip_prover_intf = sig
  type transition_frontier

  val prove :
       logger:Logger.t
    -> consensus_constants:Consensus.Constants.t
    -> frontier:transition_frontier
    -> (Consensus.Data.Consensus_state.Value.t, State_hash.t) With_hash.t
    -> ( External_transition.t
       , State_body_hash.t list * External_transition.t )
       Proof_carrying_data.t
       option

  val verify :
       logger:Logger.t
    -> verifier:Verifier.t
    -> consensus_constants:Consensus.Constants.t
    -> genesis_constants:Genesis_constants.t
    -> precomputed_values:Precomputed_values.t
    -> (Consensus.Data.Consensus_state.Value.t, State_hash.t) With_hash.t
    -> ( External_transition.t
       , State_body_hash.t list * External_transition.t )
       Proof_carrying_data.t
    -> ( [`Root of External_transition.Initial_validated.t]
       * [`Best_tip of External_transition.Initial_validated.t] )
       Deferred.Or_error.t
end

module type Sync_handler_intf = sig
  type transition_frontier

  val answer_query :
       frontier:transition_frontier
    -> Ledger_hash.t
    -> Sync_ledger.Query.t Envelope.Incoming.t
    -> logger:Logger.t
    -> trust_system:Trust_system.t
    -> Sync_ledger.Answer.t option Deferred.t

  val get_staged_ledger_aux_and_pending_coinbases_at_hash :
       frontier:transition_frontier
    -> State_hash.t
    -> ( Staged_ledger.Scan_state.t
       * Ledger_hash.t
       * Pending_coinbase.t
       * Mina_state.Protocol_state.value list )
       Option.t

  val get_transition_chain :
       frontier:transition_frontier
    -> State_hash.t list
    -> External_transition.t list option

  val best_tip_path : frontier:transition_frontier -> State_hash.t list

  (** Allows a peer to prove to a node that they can bootstrap from transition
      that they have gossiped to the network *)
  module Root :
    Consensus_best_tip_prover_intf
    with type transition_frontier := transition_frontier
end

module type Transition_chain_prover_intf = sig
  type transition_frontier

  val prove :
       ?length:int
    -> frontier:transition_frontier
    -> State_hash.t
    -> (State_hash.t * State_body_hash.t list) option
end

module type Bootstrap_controller_intf = sig
  type network

  type transition_frontier

  type persistent_root

  type persistent_frontier

  val run :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> verifier:Verifier.t
    -> network:network
    -> consensus_local_state:Consensus.Data.Local_state.t
    -> transition_reader:External_transition.Initial_validated.t
                         Envelope.Incoming.t
                         Strict_pipe.Reader.t
    -> persistent_root:persistent_root
    -> persistent_frontier:persistent_frontier
    -> initial_root_transition:External_transition.Validated.t
    -> genesis_state_hash:State_hash.t
    -> genesis_ledger:Ledger.t Lazy.t
    -> genesis_constants:Genesis_constants.t
    -> ( transition_frontier
       * External_transition.Initial_validated.t Envelope.Incoming.t list )
       Deferred.t
end

module type Transition_frontier_controller_intf = sig
  type transition_frontier

  type breadcrumb

  type network

  val run :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> verifier:Verifier.t
    -> network:network
    -> time_controller:Block_time.Controller.t
    -> collected_transitions:External_transition.Initial_validated.t
                             Envelope.Incoming.t
                             list
    -> frontier:transition_frontier
    -> network_transition_reader:External_transition.Initial_validated.t
                                 Envelope.Incoming.t
                                 Strict_pipe.Reader.t
    -> producer_transition_reader:breadcrumb Strict_pipe.Reader.t
    -> clear_reader:[`Clear] Strict_pipe.Reader.t
    -> External_transition.Validated.t Strict_pipe.Reader.t
end

module type Initial_validator_intf = sig
  type external_transition

  type external_transition_with_initial_validation

  val run :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> transition_reader:( [ `Transition of
                             external_transition Envelope.Incoming.t ]
                         * [`Time_received of Block_time.t]
                         * [ `Valid_cb of
                             Mina_net2.Validation_callback.t -> unit ] )
                         Strict_pipe.Reader.t
    -> valid_transition_writer:( [ `Transition of
                                   external_transition_with_initial_validation
                                   Envelope.Incoming.t ]
                                 * [`Time_received of Block_time.t]
                               , Strict_pipe.crash Strict_pipe.buffered
                               , unit )
                               Strict_pipe.Writer.t
    -> genesis_state_hash:State_hash.t
    -> genesis_constants:Genesis_constants.t
    -> unit
end

module type Transition_router_intf = sig
  type transition_frontier

  type transition_frontier_persistent_root

  type transition_frontier_persistent_frontier

  type breadcrumb

  type network

  val run :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> verifier:Verifier.t
    -> network:network
    -> is_seed:bool
    -> is_demo_mode:bool
    -> time_controller:Block_time.Controller.t
    -> consensus_local_state:Consensus.Data.Local_state.t
    -> persistent_root_location:string
    -> persistent_frontier_location:string
    -> frontier_broadcast_pipe:transition_frontier option
                               Pipe_lib.Broadcast_pipe.Reader.t
                               * transition_frontier option
                                 Pipe_lib.Broadcast_pipe.Writer.t
    -> network_transition_reader:( [ `Transition of
                                     External_transition.t Envelope.Incoming.t
                                   ]
                                 * [`Time_received of Block_time.t]
                                 * [ `Valid_cb of
                                     Mina_net2.Validation_callback.t ] )
                                 Strict_pipe.Reader.t
    -> producer_transition_reader:breadcrumb Strict_pipe.Reader.t
    -> most_recent_valid_block:External_transition.Initial_validated.t
                               Broadcast_pipe.Reader.t
                               * External_transition.Initial_validated.t
                                 Broadcast_pipe.Writer.t
    -> precomputed_values:Precomputed_values.t
    -> catchup_mode:[`Normal | `Super]
    -> ( [`Transition of External_transition.Validated.t]
       * [`Source of [`Gossip | `Catchup | `Internal]] )
       Strict_pipe.Reader.t
       * unit Ivar.t
end
