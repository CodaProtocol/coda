open Async_kernel
open Core
open Mina_base
open Mina_state
open Mina_transition
open Pipe_lib
open Signature_lib
module Archive_client = Archive_client
module Config = Config
module Conf_dir = Conf_dir
module Subscriptions = Coda_subscriptions

type t

type Structured_log_events.t +=
  | Connecting
  | Listening
  | Bootstrapping
  | Ledger_catchup
  | Synced
  | Rebroadcast_transition of {state_hash: State_hash.t}
  [@@deriving register_event]

exception Snark_worker_error of int

exception Snark_worker_signal_interrupt of Signal.t

val time_controller : t -> Block_time.Controller.t

val subscription : t -> Coda_subscriptions.t

(** Derived from local state (aka they may not reflect the latest public keys to which you've attempted to change *)
val block_production_pubkeys : t -> Public_key.Compressed.Set.t

val replace_block_production_keypairs :
  t -> Keypair.And_compressed_pk.Set.t -> unit

val coinbase_receiver : t -> Consensus.Coinbase_receiver.t

val replace_coinbase_receiver : t -> Consensus.Coinbase_receiver.t -> unit

val next_producer_timing :
  t -> Daemon_rpcs.Types.Status.Next_producer_timing.t option

val staking_ledger :
  t -> Consensus.Data.Local_state.Snapshot.Ledger_snapshot.t option

val next_epoch_ledger :
     t
  -> [ `Finalized of Consensus.Data.Local_state.Snapshot.Ledger_snapshot.t
     | `Notfinalized ]
     option

val current_epoch_delegators :
  t -> pk:Public_key.Compressed.t -> Mina_base.Account.t list option

val last_epoch_delegators :
  t -> pk:Public_key.Compressed.t -> Mina_base.Account.t list option

val replace_snark_worker_key :
  t -> Public_key.Compressed.t option -> unit Deferred.t

val add_block_subscriber :
     t
  -> Public_key.Compressed.t option
  -> (Filtered_external_transition.t, State_hash.t) With_hash.t Pipe.Reader.t

val add_payment_subscriber : t -> Account.key -> Signed_command.t Pipe.Reader.t

val snark_worker_key : t -> Public_key.Compressed.t option

val snark_coordinator_key : t -> Public_key.Compressed.t option

val snark_work_fee : t -> Currency.Fee.t

val set_snark_work_fee : t -> Currency.Fee.t -> unit

val request_work :
  t -> Public_key.Compressed.t -> Snark_worker.Work.Spec.t option

val work_selection_method : t -> (module Work_selector.Selection_method_intf)

val add_work : t -> Snark_worker.Work.Result.t -> unit

val snark_job_state : t -> Work_selector.State.t

val get_current_nonce :
     t
  -> Account_id.t
  -> ([> `Min of Account.Nonce.t] * Account.Nonce.t, string) result

val add_transactions :
     t
  -> User_command_input.t list
  -> ( Network_pool.Transaction_pool.Resource_pool.Diff.t
     * Network_pool.Transaction_pool.Resource_pool.Diff.Rejected.t )
     Deferred.Or_error.t

val add_full_transactions :
     t
  -> User_command.t list
  -> ( Network_pool.Transaction_pool.Resource_pool.Diff.t
     * Network_pool.Transaction_pool.Resource_pool.Diff.Rejected.t )
     Deferred.Or_error.t

val get_account : t -> Account_id.t -> Account.t option Participating_state.T.t

val get_inferred_nonce_from_transaction_pool_and_ledger :
  t -> Account_id.t -> Account.Nonce.t option Participating_state.t

val active_or_bootstrapping : t -> unit Participating_state.t

val best_staged_ledger : t -> Staged_ledger.t Participating_state.t

val best_ledger : t -> Ledger.t Participating_state.t

val root_length : t -> int Participating_state.t

val best_protocol_state : t -> Protocol_state.Value.t Participating_state.t

val best_tip : t -> Transition_frontier.Breadcrumb.t Participating_state.t

val sync_status : t -> Sync_status.t Mina_incremental.Status.Observer.t

val visualize_frontier : filename:string -> t -> unit Participating_state.t

val peers : t -> Network_peer.Peer.t list Deferred.t

val initial_peers : t -> Mina_net2.Multiaddr.t list

val client_port : t -> int

val validated_transitions :
  t -> External_transition.Validated.t Strict_pipe.Reader.t

module Root_diff : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t =
        { commands: User_command.Stable.V1.t With_status.Stable.V1.t list
        ; root_length: int }
    end
  end]
end

val root_diff : t -> Root_diff.t Strict_pipe.Reader.t

val initialization_finish_signal : t -> unit Ivar.t

val dump_tf : t -> string Or_error.t

val best_path : t -> State_hash.t list option

val best_chain :
  ?max_length:int -> t -> Transition_frontier.Breadcrumb.t list option

val transaction_pool : t -> Network_pool.Transaction_pool.t

val snark_pool : t -> Network_pool.Snark_pool.t

val start : t -> unit Deferred.t

val start_with_precomputed_blocks :
  t -> Block_producer.Precomputed_block.t Sequence.t -> unit Deferred.t

val stop_snark_worker : ?should_wait_kill:bool -> t -> unit Deferred.t

val create : ?wallets:Secrets.Wallets.t -> Config.t -> t Deferred.t

val staged_ledger_ledger_proof : t -> Ledger_proof.t option

val transition_frontier :
  t -> Transition_frontier.t option Broadcast_pipe.Reader.t

val get_ledger : t -> State_hash.t option -> Account.t list Or_error.t

val get_snarked_ledger : t -> State_hash.t option -> Account.t list Or_error.t

val wallets : t -> Secrets.Wallets.t

val subscriptions : t -> Coda_subscriptions.t

val most_recent_valid_transition :
  t -> External_transition.Initial_validated.t Broadcast_pipe.Reader.t

val top_level_logger : t -> Logger.t

val config : t -> Config.t

val net : t -> Mina_networking.t

val runtime_config : t -> Runtime_config.t
