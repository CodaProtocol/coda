open Async_kernel
open Core_kernel
open Currency
open Mina_base
open Pipe_lib
open Signature_lib

(* TODO: malleable error -> or error *)

module Engine = struct
  module type Network_config_intf = sig
    module Cli_inputs : sig
      type t

      val term : t Cmdliner.Term.t
    end

    type t

    val expand :
         logger:Logger.t
      -> test_name:string
      -> cli_inputs:Cli_inputs.t
      -> debug:bool
      -> test_config:Test_config.t
      -> images:Test_config.Container_images.t
      -> t
  end

  module type Network_intf = sig
    module Node : sig
      type t

      val id : t -> string

      val network_keypair : t -> Network_keypair.t option

      val start : fresh_state:bool -> t -> unit Malleable_error.t

      val stop : t -> unit Malleable_error.t

      val send_payment :
           logger:Logger.t
        -> t
        -> sender_pub_key:Signature_lib.Public_key.Compressed.t
        -> receiver_pub_key:Signature_lib.Public_key.Compressed.t
        -> amount:Currency.Amount.t
        -> fee:Currency.Fee.t
        -> unit Deferred.Or_error.t

      val must_send_payment :
           logger:Logger.t
        -> t
        -> sender_pub_key:Signature_lib.Public_key.Compressed.t
        -> receiver_pub_key:Signature_lib.Public_key.Compressed.t
        -> amount:Currency.Amount.t
        -> fee:Currency.Fee.t
        -> unit Malleable_error.t

      val get_balance :
           logger:Logger.t
        -> t
        -> account_id:Mina_base.Account_id.t
        -> Currency.Balance.t Async_kernel.Deferred.Or_error.t

      val must_get_balance :
           logger:Logger.t
        -> t
        -> account_id:Mina_base.Account_id.t
        -> Currency.Balance.t Malleable_error.t

      val get_peer_id :
           logger:Logger.t
        -> t
        -> (string * string list) Async_kernel.Deferred.Or_error.t

      val must_get_peer_id :
        logger:Logger.t -> t -> (string * string list) Malleable_error.t

      val get_best_chain :
        logger:Logger.t -> t -> string list Async_kernel.Deferred.Or_error.t

      val must_get_best_chain :
        logger:Logger.t -> t -> string list Malleable_error.t

      val dump_archive_data :
        logger:Logger.t -> t -> data_file:string -> unit Malleable_error.t

      val dump_mina_logs :
        logger:Logger.t -> t -> log_file:string -> unit Malleable_error.t

      val dump_precomputed_blocks :
        logger:Logger.t -> t -> unit Malleable_error.t
    end

    type t

    val constants : t -> Test_config.constants

    val constraint_constants : t -> Genesis_constants.Constraint_constants.t

    val genesis_constants : t -> Genesis_constants.t

    val seeds : t -> Node.t list

    val block_producers : t -> Node.t list

    val snark_coordinators : t -> Node.t list

    val archive_nodes : t -> Node.t list

    val all_nodes : t -> Node.t list

    val keypairs : t -> Signature_lib.Keypair.t list

    val initialize : logger:Logger.t -> t -> unit Malleable_error.t
  end

  module type Network_manager_intf = sig
    module Network_config : Network_config_intf

    module Network : Network_intf

    type t

    val create : logger:Logger.t -> Network_config.t -> t Deferred.t

    val deploy : t -> Network.t Deferred.t

    val destroy : t -> unit Deferred.t

    val cleanup : t -> unit Deferred.t
  end

  module type Log_engine_intf = sig
    module Network : Network_intf

    type t

    val create : logger:Logger.t -> network:Network.t -> t Deferred.Or_error.t

    val destroy : t -> unit Deferred.Or_error.t

    val event_reader : t -> (Network.Node.t * Event_type.event) Pipe.Reader.t
  end

  (** The signature of integration test engines. An integration test engine
   *  provides the core functionality for deploying, monitoring, and
   *  interacting with networks.
   *)
  module type S = sig
    (* unique name identifying the engine (used in test executive cli) *)
    val name : string

    module Network_config : Network_config_intf

    module Network : Network_intf

    module Network_manager :
      Network_manager_intf
        with module Network_config := Network_config
         and module Network := Network

    module Log_engine : Log_engine_intf with module Network := Network
  end
end

module Dsl = struct
  module type Event_router_intf = sig
    module Engine : Engine.S

    type t

    type ('a, 'b) handler_func =
      Engine.Network.Node.t -> 'a -> [ `Stop of 'b | `Continue ] Deferred.t

    type 'a event_subscription

    val create :
         logger:Logger.t
      -> event_reader:(Engine.Network.Node.t * Event_type.event) Pipe.Reader.t
      -> t

    val on :
      t -> 'a Event_type.t -> f:('a, 'b) handler_func -> 'b event_subscription

    val cancel : t -> 'a event_subscription -> 'a -> unit

    val await : 'a event_subscription -> 'a Deferred.t

    val await_with_timeout :
         t
      -> 'a event_subscription
      -> timeout_duration:Time.Span.t
      -> timeout_cancellation:'a
      -> 'a Deferred.t
  end

  module type Network_state_intf = sig
    module Engine : Engine.S

    module Event_router : Event_router_intf with module Engine := Engine

    type t =
      { block_height : int
      ; epoch : int
      ; global_slot : int
      ; snarked_ledgers_generated : int
      ; blocks_generated : int
      ; node_initialization : bool String.Map.t
      ; gossip_received : Gossip_state.t String.Map.t
      ; best_tips_by_node : State_hash.t String.Map.t
      }

    val listen :
         logger:Logger.t
      -> Event_router.t
      -> t Broadcast_pipe.Reader.t * t Broadcast_pipe.Writer.t
  end

  module type Wait_condition_intf = sig
    module Engine : Engine.S

    module Event_router : Event_router_intf with module Engine := Engine

    module Network_state :
      Network_state_intf
        with module Engine := Engine
         and module Event_router := Event_router

    type t

    val with_timeouts :
         ?soft_timeout:Network_time_span.t
      -> ?hard_timeout:Network_time_span.t
      -> t
      -> t

    val node_to_initialize : Engine.Network.Node.t -> t

    val blocks_to_be_produced : int -> t

    val nodes_to_synchronize : Engine.Network.Node.t list -> t

    val payment_to_be_included_in_frontier :
         sender_pub_key:Public_key.Compressed.t
      -> receiver_pub_key:Public_key.Compressed.t
      -> amount:Amount.t
      -> t
  end

  module type Util_intf = sig
    module Engine : Engine.S

    val pub_key_of_node :
         Engine.Network.Node.t
      -> Signature_lib.Public_key.Compressed.t Malleable_error.t
  end

  module type S = sig
    module Engine : Engine.S

    module Event_router : Event_router_intf with module Engine := Engine

    module Network_state :
      Network_state_intf
        with module Engine := Engine
         and module Event_router := Event_router

    module Wait_condition :
      Wait_condition_intf
        with module Engine := Engine
         and module Event_router := Event_router
         and module Network_state := Network_state

    module Util : Util_intf with module Engine := Engine

    type t

    val section_hard : string -> 'a Malleable_error.t -> 'a Malleable_error.t

    val section : string -> unit Malleable_error.t -> unit Malleable_error.t

    val network_state : t -> Network_state.t

    val wait_for : t -> Wait_condition.t -> unit Malleable_error.t

    (* TODO: move this functionality to a more suitable location *)
    val create :
         logger:Logger.t
      -> network:Engine.Network.t
      -> event_router:Event_router.t
      -> network_state_reader:Network_state.t Broadcast_pipe.Reader.t
      -> [ `Don't_call_in_tests of t ]

    type log_error_accumulator

    val watch_log_errors :
         logger:Logger.t
      -> event_router:Event_router.t
      -> on_fatal_error:(Logger.Message.t -> unit)
      -> log_error_accumulator

    val lift_accumulated_log_errors :
      log_error_accumulator -> Test_error.remote_error Test_error.Set.t
  end
end

module Test = struct
  module type Inputs_intf = sig
    module Engine : Engine.S

    module Dsl : Dsl.S with module Engine := Engine
  end

  module type S = sig
    type network

    type node

    type dsl

    val config : Test_config.t

    val run : network -> dsl -> unit Malleable_error.t
  end

  (* NB: until the DSL is actually implemented, a test just takes in the engine
   * implementation directly. *)
  module type Functor_intf = functor (Inputs : Inputs_intf) ->
    S
      with type network = Inputs.Engine.Network.t
       and type node = Inputs.Engine.Network.Node.t
       and type dsl = Inputs.Dsl.t
end
