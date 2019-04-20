open Core_kernel
open Async_kernel
open Pipe_lib
open Cache_lib

module Transition_frontier_diff = struct
  type 'a t =
    | New_breadcrumb of 'a
        (** Triggered when a new breadcrumb is added without changing the root or best_tip *)
    | New_frontier of 'a
        (** First breadcrumb to become the root of the frontier  *)
    | New_best_tip of
        { old_root: 'a
        ; old_root_length: int
        ; new_root: 'a  (** Same as old root if the root doesn't change *)
        ; added_to_best_tip_path: 'a Non_empty_list.t (* oldest first *)
        ; new_best_tip_length: int
        ; removed_from_best_tip_path: 'a list (* also oldest first *)
        ; garbage: 'a list }
        (** Triggered when a new breadcrumb is added, causing a new best_tip *)
  [@@deriving sexp]
end

module type Diff_hash = sig
  type t [@@deriving bin_io]

  val merge : t -> string -> t

  val empty : t

  val equal : t -> t -> bool
end

module type Diff_mutant = sig
  type external_transition

  type state_hash

  type scan_state

  type pending_coinbases

  type consensus_state

  (** Diff_mutant is a GADT that represents operations that affect the changes
      on the transition_frontier. The left-hand side of the GADT represents
      change that will occur to the transition_frontier. The right-hand side of
      the GADT represents which components are are effected by these changes
      and a certification that these components are handled appropriately.
      There are comments for each GADT that will discuss the operations that
      changes a `transition_frontier` and their corresponding side-effects.*)
  module T : sig
    type ('external_transition, _) t =
      | New_frontier :
          ( (external_transition, state_hash) With_hash.t
          * scan_state
          * pending_coinbases )
          -> ('external_transition, unit) t
          (** New_frontier: When creating a new transition frontier, the
            transition_frontier will begin with a single breadcrumb that can be
            constructed mainly with a root external transition and a
            scan_state. There are no components in the frontier that affects
            the frontier. Therefore, the type of this diff is tagged as a unit. *)
      | Add_transition :
          (external_transition, state_hash) With_hash.t
          -> ('external_transition, consensus_state) t
          (** Add_transition: Add_transition would simply add a transition to the
            frontier and is therefore the parameter for Add_transition. After
            adding the transition, we add the transition to its parent list of
            successors. To certify that we added it to the right parent. The
            consensus_state of the parent can accomplish this. *)
      | Remove_transitions :
          'external_transition list
          -> ('external_transition, consensus_state list) t
          (** Remove_transitions: Remove_transitions is an operation that removes
            a set of transitions. We need to make sure that we are deleting the
            right transition and we use their consensus_state to accomplish
            this. Therefore the type of Remove_transitions is indexed by a list
            of consensus_state. *)
      | Update_root :
          (state_hash * scan_state * pending_coinbases)
          -> ( 'external_transition
             , state_hash * scan_state * pending_coinbases )
             t
          (** Update_root: Update root is an indication that the root state_hash
            and the root scan_state state. To verify that we update the right
            root, we can indicate the old root is being updated. Therefore, the
            type of Update_root is indexed by a state_hash and scan_state. *)
  end

  type ('a, 'b) t = ('a, 'b) T.t

  type hash

  val key_to_yojson :
       ('external_transition, 'output) T.t
    -> f:('external_transition -> Yojson.Safe.json)
    -> Yojson.Safe.json

  val value_to_yojson :
    ('external_transition, 'output) T.t -> 'output -> Yojson.Safe.json

  val hash :
       hash
    -> ('external_transition, 'output) T.t
    -> f:('external_transition -> string)
    -> 'output
    -> hash

  module E : sig
    type 'external_transition t =
      | E : ('external_transition, 'output) T.t -> 'external_transition t

    include
      Binable.S1 with type 'external_transition t := 'external_transition t
  end
end

(** An extension to the transition frontier that provides a view onto the data
    other components can use. These are exposed through the broadcast pipes
    accessible by calling extension_pipes on a Transition_frontier.t. *)
module type Transition_frontier_extension_intf0 = sig
  (** Internal state of the extension. *)
  type t

  (** Data needed for setting up the extension*)
  type input

  type transition_frontier_breadcrumb

  (** The view type we're emitting. *)
  type view

  val create : input -> t

  (** The first view that is ever available. *)
  val initial_view : unit -> view

  (** Handle a transition frontier diff, and return the new version of the
        computed view, if it's updated. *)
  val handle_diff :
       t
    -> transition_frontier_breadcrumb Transition_frontier_diff.t
    -> view Option.t
end

(** The type of the view onto the changes to the current best tip. This type
    needs to be here to avoid dependency cycles. *)
module Best_tip_diff_view = struct
  type 'b t = {new_user_commands: 'b list; removed_user_commands: 'b list}
end

module Root_diff_view = struct
  type 'b t = {user_commands: 'b list; root_length: int option}
  [@@deriving bin_io]
end

module type Network_intf = sig
  type t

  type peer

  type state_hash

  type ledger_hash

  type pending_coinbases

  type consensus_state

  type sync_ledger_query

  type sync_ledger_answer

  type external_transition

  type state_body_hash

  type parallel_scan_state

  val random_peers : t -> int -> peer list

  val catchup_transition :
       t
    -> peer
    -> state_hash
    -> external_transition Non_empty_list.t option Deferred.Or_error.t

  val get_staged_ledger_aux_and_pending_coinbases_at_hash :
       t
    -> peer
    -> state_hash
    -> (parallel_scan_state * ledger_hash * pending_coinbases)
       Deferred.Or_error.t

  val get_ancestry :
       t
    -> peer
    -> consensus_state
    -> ( external_transition
       , state_body_hash list * external_transition )
       Proof_carrying_data.t
       Deferred.Or_error.t

  (* TODO: Change this to strict_pipe *)
  val glue_sync_ledger :
       t
    -> (ledger_hash * sync_ledger_query) Pipe_lib.Linear_pipe.Reader.t
    -> ( ledger_hash
       * sync_ledger_query
       * sync_ledger_answer Envelope.Incoming.t )
       Pipe_lib.Linear_pipe.Writer.t
    -> unit
end

module type Transition_frontier_Breadcrumb_intf = sig
  type t [@@deriving sexp, eq, compare, to_yojson]

  type display [@@deriving yojson]

  type state_hash

  type staged_ledger

  type external_transition_verified

  type user_command

  val create :
       (external_transition_verified, state_hash) With_hash.t
    -> staged_ledger
    -> t

  (** The copied breadcrumb delegates to [Staged_ledger.copy], the other fields are already immutable *)
  val copy : t -> t

  val build :
       logger:Logger.t
    -> parent:t
    -> transition_with_hash:( external_transition_verified
                            , state_hash )
                            With_hash.t
    -> (t, [`Validation_error of Error.t | `Fatal_error of exn]) Result.t
       Deferred.t

  val transition_with_hash :
    t -> (external_transition_verified, state_hash) With_hash.t

  val staged_ledger : t -> staged_ledger

  val hash : t -> int

  val external_transition : t -> external_transition_verified

  val state_hash : t -> state_hash

  val display : t -> display

  val name : t -> string

  val to_user_commands : t -> user_command list
end

module type Transition_frontier_base_intf = sig
  type state_hash

  type external_transition_verified

  type transaction_snark_scan_state

  type masked_ledger

  type user_command

  type staged_ledger

  type consensus_local_state

  type ledger_database

  type staged_ledger_diff

  type diff_mutant

  type t [@@deriving eq]

  module Breadcrumb :
    Transition_frontier_Breadcrumb_intf
    with type external_transition_verified := external_transition_verified
     and type state_hash := state_hash
     and type staged_ledger := staged_ledger
     and type user_command := user_command

  val create :
       logger:Logger.t
    -> root_transition:(external_transition_verified, state_hash) With_hash.t
    -> root_snarked_ledger:ledger_database
    -> root_staged_ledger:staged_ledger
    -> consensus_local_state:consensus_local_state
    -> t Deferred.t

  (** Clean up internal state. *)
  val close : t -> unit

  val find_exn : t -> state_hash -> Breadcrumb.t

  val logger : t -> Logger.t
end

module type Transition_frontier_intf = sig
  include Transition_frontier_base_intf

  exception
    Parent_not_found of ([`Parent of state_hash] * [`Target of state_hash])

  exception Already_exists of state_hash

  val max_length : int

  val consensus_local_state : t -> consensus_local_state

  val all_breadcrumbs : t -> Breadcrumb.t list

  val root : t -> Breadcrumb.t

  val previous_root : t -> Breadcrumb.t option

  val root_length : t -> int

  val best_tip : t -> Breadcrumb.t

  val path_map : t -> Breadcrumb.t -> f:(Breadcrumb.t -> 'a) -> 'a list

  val hash_path : t -> Breadcrumb.t -> state_hash list

  val find : t -> state_hash -> Breadcrumb.t option

  val find_in_root_history : t -> state_hash -> Breadcrumb.t option

  val root_history_path_map :
    t -> state_hash -> f:(Breadcrumb.t -> 'a) -> 'a Non_empty_list.t option

  val successor_hashes : t -> state_hash -> state_hash list

  val successor_hashes_rec : t -> state_hash -> state_hash list

  val successors : t -> Breadcrumb.t -> Breadcrumb.t list

  val successors_rec : t -> Breadcrumb.t -> Breadcrumb.t list

  val common_ancestor : t -> Breadcrumb.t -> Breadcrumb.t -> state_hash

  val iter : t -> f:(Breadcrumb.t -> unit) -> unit

  (** Adds a breadcrumb to the transition frontier or throws. It possibly
   * triggers a root move and it triggers any extensions that are listening to
   * events on the frontier. *)
  val add_breadcrumb_exn : t -> Breadcrumb.t -> unit Deferred.t

  (** Like add_breadcrumb_exn except it doesn't throw if the parent hash is
   * missing from the transition frontier *)
  val add_breadcrumb_if_present_exn : t -> Breadcrumb.t -> unit Deferred.t

  val best_tip_path_length_exn : t -> int

  val shallow_copy_root_snarked_ledger : t -> masked_ledger

  val wait_for_transition : t -> state_hash -> unit Deferred.t

  module type Transition_frontier_extension_intf =
    Transition_frontier_extension_intf0
    with type transition_frontier_breadcrumb := Breadcrumb.t

  module Extensions : sig
    module Work : sig
      type t [@@deriving sexp]

      module Stable :
        sig
          module V1 : sig
            type t [@@deriving sexp, bin_io]

            include Hashable.S_binable with type t := t
          end
        end
        with type V1.t = t

      include Hashable.S with type t := t
    end

    module Snark_pool_refcount : sig
      include
        Transition_frontier_extension_intf
        with type view = int * int Work.Table.t
    end

    module Best_tip_diff :
      Transition_frontier_extension_intf
      with type view = user_command Best_tip_diff_view.t

    module Root_diff :
      Transition_frontier_extension_intf
      with type view = user_command Root_diff_view.t

    module Persistence_diff :
      Transition_frontier_extension_intf with type view = diff_mutant list

    type readers =
      { snark_pool: Snark_pool_refcount.view Broadcast_pipe.Reader.t
      ; best_tip_diff: Best_tip_diff.view Broadcast_pipe.Reader.t
      ; root_diff: Root_diff.view Broadcast_pipe.Reader.t
      ; persistence_diff: Persistence_diff.view Broadcast_pipe.Reader.t }
    [@@deriving fields]
  end

  val snark_pool_refcount_pipe :
    t -> Extensions.Snark_pool_refcount.view Broadcast_pipe.Reader.t

  val best_tip_diff_pipe :
    t -> Extensions.Best_tip_diff.view Broadcast_pipe.Reader.t

  val root_diff_pipe : t -> Extensions.Root_diff.view Broadcast_pipe.Reader.t

  val persistence_diff_pipe :
    t -> Extensions.Persistence_diff.view Broadcast_pipe.Reader.t

  val visualize_to_string : t -> string

  val visualize : filename:string -> t -> unit

  module For_tests : sig
    val root_snarked_ledger : t -> ledger_database

    val root_history_mem : t -> state_hash -> bool

    val root_history_is_empty : t -> bool
  end
end

module type Catchup_intf = sig
  type state_hash

  type external_transition_verified

  type unprocessed_transition_cache

  type transition_frontier

  type transition_frontier_breadcrumb

  type network

  val run :
       logger:Logger.t
    -> network:network
    -> frontier:transition_frontier
    -> catchup_job_reader:( state_hash
                          * ( ( external_transition_verified
                              , state_hash )
                              With_hash.t
                              Envelope.Incoming.t
                            , state_hash )
                            Cached.t
                            Rose_tree.t
                            list )
                          Strict_pipe.Reader.t
    -> catchup_breadcrumbs_writer:( ( transition_frontier_breadcrumb
                                    , state_hash )
                                    Cached.t
                                    Rose_tree.t
                                    list
                                  , Strict_pipe.synchronous
                                  , unit Deferred.t )
                                  Strict_pipe.Writer.t
    -> unprocessed_transition_cache:unprocessed_transition_cache
    -> unit
end

module type Transition_handler_validator_intf = sig
  type time

  type state_hash

  type external_transition_verified

  type unprocessed_transition_cache

  type transition_frontier

  type staged_ledger

  val run :
       logger:Logger.t
    -> frontier:transition_frontier
    -> transition_reader:( [ `Transition of
                             external_transition_verified Envelope.Incoming.t
                           ]
                         * [`Time_received of time] )
                         Strict_pipe.Reader.t
    -> valid_transition_writer:( ( ( external_transition_verified
                                   , state_hash )
                                   With_hash.t
                                   Envelope.Incoming.t
                                 , state_hash )
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
    -> (external_transition_verified, state_hash) With_hash.t
       Envelope.Incoming.t
    -> ( ( (external_transition_verified, state_hash) With_hash.t
           Envelope.Incoming.t
         , state_hash )
         Cached.t
       , [ `In_frontier of state_hash
         | `Invalid of string
         | `In_process of state_hash Cache_lib.Intf.final_state ] )
       Result.t
end

module type Breadcrumb_builder_intf = sig
  type state_hash

  type transition_frontier

  type transition_frontier_breadcrumb

  type external_transition_verified

  val build_subtrees_of_breadcrumbs :
       logger:Logger.t
    -> frontier:transition_frontier
    -> initial_hash:state_hash
    -> ( (external_transition_verified, state_hash) With_hash.t
         Envelope.Incoming.t
       , state_hash )
       Cached.t
       Rose_tree.t
       List.t
    -> (transition_frontier_breadcrumb, state_hash) Cached.t Rose_tree.t List.t
       Deferred.Or_error.t
end

module type Transition_handler_processor_intf = sig
  type state_hash

  type time_controller

  type external_transition_verified

  type unprocessed_transition_cache

  type transition_frontier

  type transition_frontier_breadcrumb

  val run :
       logger:Logger.t
    -> time_controller:time_controller
    -> frontier:transition_frontier
    -> primary_transition_reader:( ( external_transition_verified
                                   , state_hash )
                                   With_hash.t
                                   Envelope.Incoming.t
                                 , state_hash )
                                 Cached.t
                                 Strict_pipe.Reader.t
    -> proposer_transition_reader:( external_transition_verified
                                  , state_hash )
                                  With_hash.t
                                  Strict_pipe.Reader.t
    -> clean_up_catchup_scheduler:unit Ivar.t
    -> catchup_job_writer:( state_hash
                            * ( ( external_transition_verified
                                , state_hash )
                                With_hash.t
                                Envelope.Incoming.t
                              , state_hash )
                              Cached.t
                              Rose_tree.t
                              list
                          , Strict_pipe.synchronous
                          , unit Deferred.t )
                          Strict_pipe.Writer.t
    -> catchup_breadcrumbs_reader:( transition_frontier_breadcrumb
                                  , state_hash )
                                  Cached.t
                                  Rose_tree.t
                                  list
                                  Strict_pipe.Reader.t
    -> catchup_breadcrumbs_writer:( ( transition_frontier_breadcrumb
                                    , state_hash )
                                    Cached.t
                                    Rose_tree.t
                                    list
                                  , Strict_pipe.synchronous
                                  , unit Deferred.t )
                                  Strict_pipe.Writer.t
    -> processed_transition_writer:( ( external_transition_verified
                                     , state_hash )
                                     With_hash.t
                                   , Strict_pipe.crash Strict_pipe.buffered
                                   , unit )
                                   Strict_pipe.Writer.t
    -> unprocessed_transition_cache:unprocessed_transition_cache
    -> unit
end

module type Unprocessed_transition_cache_intf = sig
  type state_hash

  type external_transition_verified

  type t

  val create : logger:Logger.t -> t

  val register_exn :
       t
    -> (external_transition_verified, state_hash) With_hash.t
       Envelope.Incoming.t
    -> ( (external_transition_verified, state_hash) With_hash.t
         Envelope.Incoming.t
       , state_hash )
       Cached.t
end

module type Transition_handler_intf = sig
  type time_controller

  type time

  type state_hash

  type external_transition_verified

  type transition_frontier

  type staged_ledger

  type transition_frontier_breadcrumb

  module Unprocessed_transition_cache :
    Unprocessed_transition_cache_intf
    with type state_hash := state_hash
     and type external_transition_verified := external_transition_verified

  module Breadcrumb_builder :
    Breadcrumb_builder_intf
    with type state_hash := state_hash
    with type external_transition_verified := external_transition_verified
    with type transition_frontier := transition_frontier
    with type transition_frontier_breadcrumb := transition_frontier_breadcrumb

  module Validator :
    Transition_handler_validator_intf
    with type time := time
     and type state_hash := state_hash
     and type external_transition_verified := external_transition_verified
     and type unprocessed_transition_cache := Unprocessed_transition_cache.t
     and type transition_frontier := transition_frontier
     and type staged_ledger := staged_ledger

  module Processor :
    Transition_handler_processor_intf
    with type time_controller := time_controller
     and type external_transition_verified := external_transition_verified
     and type state_hash := state_hash
     and type unprocessed_transition_cache := Unprocessed_transition_cache.t
     and type transition_frontier := transition_frontier
     and type transition_frontier_breadcrumb := transition_frontier_breadcrumb
end

module type Sync_handler_intf = sig
  type ledger_hash

  type transition_frontier

  type state_hash

  type external_transition

  type syncable_ledger_query

  type syncable_ledger_answer

  type parallel_scan_state

  type pending_coinbases

  val answer_query :
       frontier:transition_frontier
    -> ledger_hash
    -> syncable_ledger_query Envelope.Incoming.t
    -> logger:Logger.t
    -> trust_system:Trust_system.t
    -> syncable_ledger_answer option Deferred.t

  val transition_catchup :
       frontier:transition_frontier
    -> state_hash
    -> external_transition Non_empty_list.t option

  val get_staged_ledger_aux_and_pending_coinbases_at_hash :
       frontier:transition_frontier
    -> state_hash
    -> (parallel_scan_state * ledger_hash * pending_coinbases) Option.t
end

module type Root_prover_intf = sig
  type state_body_hash

  type state_hash

  type transition_frontier

  type external_transition

  type consensus_state

  type proof_verified_external_transition

  val prove :
       logger:Logger.t
    -> frontier:transition_frontier
    -> consensus_state
    -> ( external_transition
       , state_body_hash list * external_transition )
       Proof_carrying_data.t
       option

  val verify :
       logger:Logger.t
    -> observed_state:consensus_state
    -> peer_root:( external_transition
                 , state_body_hash list * external_transition )
                 Proof_carrying_data.t
    -> ( (proof_verified_external_transition, state_hash) With_hash.t
       * (proof_verified_external_transition, state_hash) With_hash.t )
       Deferred.Or_error.t
end

module type Bootstrap_controller_intf = sig
  type network

  type transition_frontier

  type external_transition_verified

  type ledger_db

  val run :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> network:network
    -> frontier:transition_frontier
    -> ledger_db:ledger_db
    -> transition_reader:( [< `Transition of
                              external_transition_verified Envelope.Incoming.t
                           ]
                         * [< `Time_received of int64] )
                         Strict_pipe.Reader.t
    -> ( transition_frontier
       * external_transition_verified Envelope.Incoming.t list )
       Deferred.t
end

module type Transition_frontier_controller_intf = sig
  type time_controller

  type external_transition_verified

  type state_hash

  type transition_frontier

  type network

  type time

  val run :
       logger:Logger.t
    -> network:network
    -> time_controller:time_controller
    -> collected_transitions:( external_transition_verified
                             , state_hash )
                             With_hash.t
                             Envelope.Incoming.t
                             list
    -> frontier:transition_frontier
    -> network_transition_reader:( [ `Transition of
                                     external_transition_verified
                                     Envelope.Incoming.t ]
                                 * [`Time_received of time] )
                                 Strict_pipe.Reader.t
    -> proposer_transition_reader:( external_transition_verified
                                  , state_hash )
                                  With_hash.t
                                  Strict_pipe.Reader.t
    -> clear_reader:[`Clear] Strict_pipe.Reader.t
    -> (external_transition_verified, state_hash) With_hash.t
       Strict_pipe.Reader.t
end

module type Protocol_state_validator_intf = sig
  type time

  type state_hash

  type external_transition

  type external_transition_proof_verified

  type external_transition_verified

  val validate_proof :
       external_transition
    -> external_transition_proof_verified Or_error.t Deferred.t

  val validate_consensus_state :
       time_received:time
    -> external_transition
    -> external_transition_verified Or_error.t Deferred.t
end

module type Initial_validator_intf = sig
  type time

  type state_hash

  type external_transition

  type external_transition_verified

  val run :
       logger:Logger.t
    -> transition_reader:( [ `Transition of
                             external_transition Envelope.Incoming.t ]
                         * [`Time_received of time] )
                         Strict_pipe.Reader.t
    -> valid_transition_writer:( [ `Transition of
                                   external_transition_verified
                                   Envelope.Incoming.t ]
                                 * [`Time_received of time]
                               , Strict_pipe.crash Strict_pipe.buffered
                               , unit )
                               Strict_pipe.Writer.t
    -> unit
end

module type Transition_router_intf = sig
  type time_controller

  type external_transition

  type external_transition_verified

  type state_hash

  type transition_frontier

  type network

  type time

  type ledger_db

  val run :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> network:network
    -> time_controller:time_controller
    -> frontier_broadcast_pipe:transition_frontier option
                               Pipe_lib.Broadcast_pipe.Reader.t
                               * transition_frontier option
                                 Pipe_lib.Broadcast_pipe.Writer.t
    -> ledger_db:ledger_db
    -> network_transition_reader:( [ `Transition of
                                     external_transition Envelope.Incoming.t ]
                                 * [`Time_received of time] )
                                 Strict_pipe.Reader.t
    -> proposer_transition_reader:( external_transition_verified
                                  , state_hash )
                                  With_hash.t
                                  Strict_pipe.Reader.t
    -> (external_transition_verified, state_hash) With_hash.t
       Strict_pipe.Reader.t
end
