open Core_kernel
open Async_kernel
open Signature_lib

(* TODO: Pass banlist to modules discussed in Ban Reasons issue: https://github.com/CodaProtocol/coda/issues/852 *)

module Snark_worker_config = struct
  type t =
    { initial_snark_worker_key: Public_key.Compressed.t option
    ; shutdown_on_disconnect: bool
    ; num_threads: int option }
end

(** If ledger_db_location is None, will auto-generate a db based on a UUID *)
type t =
  { conf_dir: string
  ; chain_id: string
  ; logger: Logger.t
  ; pids: Child_processes.Termination.t
  ; trust_system: Trust_system.t
  ; monitor: Monitor.t option
  ; is_seed: bool
  ; disable_node_status: bool
  ; super_catchup: bool
  ; initial_block_production_keypairs: Keypair.Set.t
  ; coinbase_receiver: Consensus.Coinbase_receiver.t
  ; work_selection_method: (module Work_selector.Selection_method_intf)
  ; snark_worker_config: Snark_worker_config.t
  ; snark_coordinator_key: Public_key.Compressed.t option [@default None]
  ; work_reassignment_wait: int
  ; gossip_net_params: Gossip_net.Libp2p.Config.t
  ; net_config: Mina_networking.Config.t
  ; initial_protocol_version: Protocol_version.t
        (* Option.t instead of option, so that the derived `make' requires an argument *)
  ; proposed_protocol_version_opt: Protocol_version.t Option.t
  ; snark_pool_disk_location: string
  ; wallets_disk_location: string
  ; persistent_root_location: string
  ; persistent_frontier_location: string
  ; epoch_ledger_location: string
  ; staged_ledger_transition_backup_capacity: int [@default 10]
  ; time_controller: Block_time.Controller.t
  ; snark_work_fee: Currency.Fee.t
  ; consensus_local_state: Consensus.Data.Local_state.t
  ; is_archive_rocksdb: bool [@default false]
  ; archive_process_location:
      Core.Host_and_port.t Cli_lib.Flag.Types.with_name option
        [@default None]
  ; demo_mode: bool [@default false]
  ; log_block_creation: bool [@default false]
  ; precomputed_values: Precomputed_values.t
  ; start_time: Time.t
  ; precomputed_blocks_path: string option
  ; log_precomputed_blocks: bool
  ; upload_blocks_to_gcloud: bool
  ; block_reward_threshold: Currency.Amount.t option [@default None] }
[@@deriving make]
