open Core
open Async
open Signature_lib
open Mina_numbers
open Mina_base
open Mina_state

(** For status *)
let txn_count = ref 0

let get_account t (addr : Account_id.t) =
  let open Participating_state.Let_syntax in
  let%map ledger = Mina_lib.best_ledger t in
  let open Option.Let_syntax in
  let%bind loc = Ledger.location_of_account ledger addr in
  Ledger.get ledger loc

let get_accounts t =
  let open Participating_state.Let_syntax in
  let%map ledger = Mina_lib.best_ledger t in
  Ledger.to_list ledger

let string_of_public_key =
  Fn.compose Public_key.Compressed.to_base58_check Account.public_key

let get_public_keys t =
  let open Participating_state.Let_syntax in
  let%map account = get_accounts t in
  List.map account ~f:string_of_public_key

let get_keys_with_details t =
  let open Participating_state.Let_syntax in
  let%map accounts = get_accounts t in
  List.map accounts ~f:(fun account ->
      ( string_of_public_key account
      , account.Account.Poly.balance |> Currency.Balance.to_int
      , account.Account.Poly.nonce |> Account.Nonce.to_int ))

let get_nonce t (addr : Account_id.t) =
  let open Participating_state.Option.Let_syntax in
  let%map account = get_account t addr in
  account.Account.Poly.nonce

let get_balance t (addr : Account_id.t) =
  let open Participating_state.Option.Let_syntax in
  let%map account = get_account t addr in
  account.Account.Poly.balance

let get_trust_status t (ip_address : Unix.Inet_addr.Blocking_sexp.t) =
  let config = Mina_lib.config t in
  let trust_system = config.trust_system in
  Trust_system.lookup_ip trust_system ip_address

let get_trust_status_all t =
  let config = Mina_lib.config t in
  let trust_system = config.trust_system in
  Trust_system.peer_statuses trust_system

let reset_trust_status t (ip_address : Unix.Inet_addr.Blocking_sexp.t) =
  let config = Mina_lib.config t in
  let trust_system = config.trust_system in
  Trust_system.reset_ip trust_system ip_address

let replace_block_production_keys keys pks =
  let kps =
    List.filter_map pks ~f:(fun pk ->
        let open Option.Let_syntax in
        let%map kps =
          Mina_lib.wallets keys |> Secrets.Wallets.find_unlocked ~needle:pk
        in
        (kps, pk))
  in
  Mina_lib.replace_block_production_keypairs keys
    (Keypair.And_compressed_pk.Set.of_list kps) ;
  kps |> List.map ~f:snd

let setup_and_submit_user_command t (user_command_input : User_command_input.t)
    =
  let open Participating_state.Let_syntax in
  (* hack to get types to work out *)
  let%map () = return () in
  let open Deferred.Let_syntax in
  let%map result = Mina_lib.add_transactions t [ user_command_input ] in
  txn_count := !txn_count + 1 ;
  match result with
  | Ok ([], [ failed_txn ]) ->
      Error
        (Error.of_string
           (sprintf !"%s"
              ( Network_pool.Transaction_pool.Resource_pool.Diff.Diff_error
                .to_yojson (snd failed_txn)
              |> Yojson.Safe.to_string )))
  | Ok ([ Signed_command txn ], []) ->
      [%log' info (Mina_lib.top_level_logger t)]
        ~metadata:[ ("command", User_command.to_yojson (Signed_command txn)) ]
        "Scheduled payment $command" ;
      Ok txn
  | Ok (valid_commands, invalid_commands) ->
      [%log' info (Mina_lib.top_level_logger t)]
        ~metadata:
          [ ( "valid_commands"
            , `List (List.map ~f:User_command.to_yojson valid_commands) )
          ; ( "invalid_commands"
            , `List
                (List.map
                   ~f:
                     (Fn.compose
                        Network_pool.Transaction_pool.Resource_pool.Diff
                        .Diff_error
                        .to_yojson snd)
                   invalid_commands) )
          ]
        "Invalid result from scheduling a payment" ;
      Error (Error.of_string "Internal error while scheduling a payment")
  | Error e ->
      Error e

let setup_and_submit_user_commands t user_command_list =
  let open Participating_state.Let_syntax in
  let%map _is_active = Mina_lib.active_or_bootstrapping t in
  [%log' warn (Mina_lib.top_level_logger t)]
    "batch-send-payments does not yet report errors"
    ~metadata:
      [ ("mina_command", `String "scheduling a batch of user transactions") ] ;
  Mina_lib.add_transactions t user_command_list

module Receipt_chain_verifier = Merkle_list_verifier.Make (struct
  type proof_elem = User_command.t

  type hash = Receipt.Chain_hash.t [@@deriving equal]

  let hash parent_hash (proof_elem : User_command.t) =
    let p =
      match proof_elem with
      | Signed_command c ->
          Receipt.Elt.Signed_command (Signed_command.payload c)
      | Parties x ->
          Receipt.Elt.Parties (Parties.commitment x)
    in
    Receipt.Chain_hash.cons p parent_hash
end)

let chain_id_inputs (t : Mina_lib.t) =
  (* these are the inputs to Blake2.digest_string in Mina.chain_id *)
  let config = Mina_lib.config t in
  let precomputed_values = config.precomputed_values in
  let genesis_state_hash =
    Precomputed_values.genesis_state_hash precomputed_values
  in
  let genesis_constants = precomputed_values.genesis_constants in
  let snark_keys =
    Lazy.force precomputed_values.constraint_system_digests
    |> List.map ~f:(fun (_, digest) -> Md5.to_hex digest)
  in
  (genesis_state_hash, genesis_constants, snark_keys)

let verify_payment t (addr : Account_id.t) (verifying_txn : User_command.t)
    (init_receipt, proof) =
  let open Participating_state.Let_syntax in
  let%map account = get_account t addr in
  let account = Option.value_exn account in
  let resulting_receipt = account.Account.Poly.receipt_chain_hash in
  let open Or_error.Let_syntax in
  let%bind (_ : Receipt.Chain_hash.t Non_empty_list.t) =
    Result.of_option
      (Receipt_chain_verifier.verify ~init:init_receipt proof resulting_receipt)
      ~error:(Error.createf "Merkle list proof of payment is invalid")
  in
  if List.exists proof ~f:(fun txn -> User_command.equal verifying_txn txn) then
    Ok ()
  else
    Or_error.errorf
      !"Merkle list proof does not contain payment %{sexp:User_command.t}"
      verifying_txn

let start_time = Time_ns.now ()

type active_state_fields =
  { num_accounts : int option
  ; blockchain_length : int option
  ; ledger_merkle_root : string option
  ; state_hash : string option
  ; consensus_time_best_tip : Consensus.Data.Consensus_time.t option
  ; global_slot_since_genesis_best_tip : int option
  }

let max_block_height = ref 1

let get_status ~flag t =
  let open Mina_lib.Config in
  let config = Mina_lib.config t in
  let precomputed_values = config.precomputed_values in
  let protocol_constants = precomputed_values.genesis_constants.protocol in
  let constraint_constants = precomputed_values.constraint_constants in
  let consensus_constants = precomputed_values.consensus_constants in
  let uptime_secs =
    Time_ns.diff (Time_ns.now ()) start_time
    |> Time_ns.Span.to_sec |> Int.of_float
  in
  let commit_id = Mina_version.commit_id in
  let conf_dir = config.conf_dir in
  let%map peers =
    let%map undisplay_peers = Mina_lib.peers t in
    List.map ~f:Network_peer.Peer.to_display undisplay_peers
  in
  let user_commands_sent = !txn_count in
  let snark_worker =
    Option.map
      (Mina_lib.snark_worker_key t)
      ~f:Public_key.Compressed.to_base58_check
  in
  let snark_work_fee = Currency.Fee.to_int @@ Mina_lib.snark_work_fee t in
  let block_production_keys = Mina_lib.block_production_pubkeys t in
  let coinbase_receiver =
    match Mina_lib.coinbase_receiver t with
    | `Producer ->
        None
    | `Other pk ->
        Some pk
  in
  let consensus_mechanism = Consensus.name in
  let time_controller = config.time_controller in
  let consensus_time_now =
    try
      Consensus.Data.Consensus_time.of_time_exn ~constants:consensus_constants
        (Block_time.now time_controller)
    with Invalid_argument _ ->
      (*setting 0 for the time before genesis timestamp*)
      Consensus.Data.Consensus_time.zero ~constants:consensus_constants
  in
  let consensus_configuration =
    Consensus.Configuration.t ~constraint_constants ~protocol_constants
  in
  let r = Perf_histograms.report in
  let histograms =
    match flag with
    | `Performance ->
        let rpc_timings =
          let open Daemon_rpcs.Types.Status.Rpc_timings in
          { get_staged_ledger_aux =
              { Rpc_pair.dispatch = r ~name:"rpc_dispatch_get_staged_ledger_aux"
              ; impl = r ~name:"rpc_impl_get_staged_ledger_aux"
              }
          ; answer_sync_ledger_query =
              { Rpc_pair.dispatch =
                  r ~name:"rpc_dispatch_answer_sync_ledger_query"
              ; impl = r ~name:"rpc_impl_answer_sync_ledger_query"
              }
          ; get_ancestry =
              { Rpc_pair.dispatch = r ~name:"rpc_dispatch_get_ancestry"
              ; impl = r ~name:"rpc_impl_get_ancestry"
              }
          ; get_transition_chain_proof =
              { Rpc_pair.dispatch =
                  r ~name:"rpc_dispatch_get_transition_chain_proof"
              ; impl = r ~name:"rpc_impl_get_transition_chain_proof"
              }
          ; get_transition_chain =
              { Rpc_pair.dispatch = r ~name:"rpc_dispatch_get_transition_chain"
              ; impl = r ~name:"rpc_impl_get_transition_chain"
              }
          }
        in
        Some
          { Daemon_rpcs.Types.Status.Histograms.rpc_timings
          ; external_transition_latency = r ~name:"external_transition_latency"
          ; accepted_transition_local_latency =
              r ~name:"accepted_transition_local_latency"
          ; accepted_transition_remote_latency =
              r ~name:"accepted_transition_remote_latency"
          ; snark_worker_transition_time =
              r ~name:"snark_worker_transition_time"
          ; snark_worker_merge_time = r ~name:"snark_worker_merge_time"
          }
    | `None ->
        None
  in
  let new_block_length_received =
    Length.to_int @@ Consensus.Data.Consensus_state.blockchain_length
    @@ Mina_transition.External_transition.Initial_validated.consensus_state
    @@ Pipe_lib.Broadcast_pipe.Reader.peek
         (Mina_lib.most_recent_valid_transition t)
  in
  let () =
    if new_block_length_received > !max_block_height then
      max_block_height := new_block_length_received
    else ()
  in
  let active_status () =
    let open Participating_state.Let_syntax in
    let%bind ledger = Mina_lib.best_ledger t in
    let ledger_merkle_root =
      Ledger.merkle_root ledger |> Ledger_hash.to_string
    in
    let num_accounts = Ledger.num_accounts ledger in
    let%bind state = Mina_lib.best_protocol_state t in
    let state_hash = Protocol_state.hash state |> State_hash.to_base58_check in
    let consensus_state = state |> Protocol_state.consensus_state in
    let blockchain_length =
      Length.to_int
      @@ Consensus.Data.Consensus_state.blockchain_length consensus_state
    in
    let%map sync_status =
      Mina_incremental.Status.stabilize () ;
      match
        Mina_incremental.Status.Observer.value_exn @@ Mina_lib.sync_status t
      with
      | `Bootstrap ->
          `Bootstrapping
      | `Connecting ->
          `Active `Connecting
      | `Listening ->
          `Active `Listening
      | `Offline ->
          `Active `Offline
      | `Synced | `Catchup ->
          if abs (!max_block_height - blockchain_length) < 5 then
            `Active `Synced
          else `Active `Catchup
    in
    let consensus_time_best_tip =
      Consensus.Data.Consensus_state.consensus_time consensus_state
    in
    let global_slot_since_genesis =
      Mina_numbers.Global_slot.to_int
      @@ Consensus.Data.Consensus_state.global_slot_since_genesis
           consensus_state
    in
    ( sync_status
    , { num_accounts = Some num_accounts
      ; blockchain_length = Some blockchain_length
      ; ledger_merkle_root = Some ledger_merkle_root
      ; state_hash = Some state_hash
      ; consensus_time_best_tip = Some consensus_time_best_tip
      ; global_slot_since_genesis_best_tip = Some global_slot_since_genesis
      } )
  in
  let ( sync_status
      , { num_accounts
        ; blockchain_length
        ; ledger_merkle_root
        ; state_hash
        ; consensus_time_best_tip
        ; global_slot_since_genesis_best_tip
        } ) =
    match active_status () with
    | `Active result ->
        result
    | `Bootstrapping ->
        ( `Bootstrap
        , { num_accounts = None
          ; blockchain_length = None
          ; ledger_merkle_root = None
          ; state_hash = None
          ; consensus_time_best_tip = None
          ; global_slot_since_genesis_best_tip = None
          } )
  in
  let next_block_production = Mina_lib.next_producer_timing t in
  let addrs_and_ports =
    Node_addrs_and_ports.to_display config.gossip_net_params.addrs_and_ports
  in
  let catchup_status =
    let open Option.Let_syntax in
    let%bind frontier =
      Mina_lib.transition_frontier t |> Pipe_lib.Broadcast_pipe.Reader.peek
    in
    match Transition_frontier.catchup_tree frontier with
    | Full full ->
        Some (Hashtbl.to_alist full.states)
    | _ ->
        None
  in
  { Daemon_rpcs.Types.Status.num_accounts
  ; sync_status
  ; catchup_status
  ; blockchain_length
  ; highest_block_length_received =
      (*if this function is not called until after catchup max_block_height will be 1 and most_recent_valid_transition pipe might have the genesis block as the latest transition in which case return the best tip length*)
      max (Option.value ~default:1 blockchain_length) !max_block_height
  ; highest_unvalidated_block_length_received =
      !Mina_metrics.Transition_frontier.max_unvalidated_blocklength_observed
  ; uptime_secs
  ; ledger_merkle_root
  ; state_hash
  ; chain_id = config.chain_id
  ; consensus_time_best_tip
  ; global_slot_since_genesis_best_tip
  ; commit_id
  ; conf_dir
  ; peers
  ; user_commands_sent
  ; snark_worker
  ; snark_work_fee
  ; block_production_keys =
      Public_key.Compressed.Set.to_list block_production_keys
      |> List.map ~f:Public_key.Compressed.to_base58_check
  ; coinbase_receiver =
      Option.map ~f:Public_key.Compressed.to_base58_check coinbase_receiver
  ; histograms
  ; next_block_production
  ; consensus_time_now
  ; consensus_mechanism
  ; consensus_configuration
  ; addrs_and_ports
  }

let clear_hist_status ~flag t = Perf_histograms.wipe () ; get_status ~flag t

module Subscriptions = struct
  let new_block t public_key =
    let subscription = Mina_lib.subscription t in
    Mina_lib.Subscriptions.add_block_subscriber subscription public_key

  let reorganization t =
    let subscription = Mina_lib.subscription t in
    Mina_lib.Subscriptions.add_reorganization_subscriber subscription
end

module For_tests = struct
  module Subscriptions = struct
    let new_user_commands coda public_key =
      Mina_lib.add_payment_subscriber coda public_key
  end
end
