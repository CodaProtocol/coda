open Core
open Async
open Signature_lib
open Coda_numbers
open Coda_base
open Coda_state

(** For status *)
let txn_count = ref 0

let record_payment t (txn : User_command.t) account =
  let logger =
    Logger.extend
      (Coda_lib.top_level_logger t)
      [("coda_command", `String "Recording payment")]
  in
  let previous = account.Account.Poly.receipt_chain_hash in
  let receipt_chain_database = Coda_lib.receipt_chain_database t in
  match Receipt_chain_database.add receipt_chain_database ~previous txn with
  | `Ok hash ->
      Logger.debug logger ~module_:__MODULE__ ~location:__LOC__
        ~metadata:
          [ ("user_command", User_command.to_yojson txn)
          ; ("receipt_chain_hash", Receipt.Chain_hash.to_yojson hash) ]
        "Added  payment $user_command into receipt_chain database. You should \
         wait for a bit to see your account's receipt chain hash update as \
         $receipt_chain_hash" ;
      hash
  | `Duplicate hash ->
      Logger.warn logger ~module_:__MODULE__ ~location:__LOC__
        ~metadata:[("user_command", User_command.to_yojson txn)]
        "Already sent transaction $user_command" ;
      hash
  | `Error_multiple_previous_receipts parent_hash ->
      Logger.fatal logger ~module_:__MODULE__ ~location:__LOC__
        ~metadata:
          [ ( "parent_receipt_chain_hash"
            , Receipt.Chain_hash.to_yojson parent_hash )
          ; ( "previous_receipt_chain_hash"
            , Receipt.Chain_hash.to_yojson previous ) ]
        "A payment is derived from two different blockchain states \
         ($parent_receipt_chain_hash, $previous_receipt_chain_hash). \
         Receipt.Chain_hash is supposed to be collision resistant. This \
         collision should not happen." ;
      Core.exit 1

let is_valid_user_command _t (txn : User_command.t) account_opt =
  let remainder =
    let open Option.Let_syntax in
    let%bind account = account_opt
    and cost =
      let fee = txn.payload.common.fee in
      match txn.payload.body with
      | Stake_delegation (Set_delegate _) ->
          Some (Currency.Amount.of_fee fee)
      | Payment {amount; _} ->
          Currency.Amount.add_fee amount fee
    in
    Currency.Balance.sub_amount account.Account.Poly.balance cost
  in
  Option.is_some remainder

let schedule_user_command t (txn : User_command.t) account_opt =
  if not (is_valid_user_command t txn account_opt) then
    Or_error.error_string "Invalid user command: account balance is too low"
  else
    let txn_pool = Coda_lib.transaction_pool t in
    don't_wait_for (Network_pool.Transaction_pool.add txn_pool txn) ;
    let logger =
      Logger.extend
        (Coda_lib.top_level_logger t)
        [("coda_command", `String "scheduling a user command")]
    in
    Logger.info logger ~module_:__MODULE__ ~location:__LOC__
      ~metadata:[("user_command", User_command.to_yojson txn)]
      "Added transaction $user_command to transaction pool successfully" ;
    txn_count := !txn_count + 1 ;
    Or_error.return ()

let get_account t (addr : Public_key.Compressed.t) =
  let open Participating_state.Let_syntax in
  let%map ledger = Coda_lib.best_ledger t in
  Ledger.location_of_key ledger addr |> Option.bind ~f:(Ledger.get ledger)

let get_accounts t =
  let open Participating_state.Let_syntax in
  let%map ledger = Coda_lib.best_ledger t in
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
      , account.Account.Poly.nonce |> Account.Nonce.to_string ) )

let get_inferred_nonce_from_transaction_pool_and_ledger t
    (addr : Public_key.Compressed.t) =
  let transaction_pool = Coda_lib.transaction_pool t in
  let resource_pool =
    Network_pool.Transaction_pool.resource_pool transaction_pool
  in
  let pooled_transactions =
    Network_pool.Transaction_pool.Resource_pool.all_from_user resource_pool
      addr
  in
  let txn_pool_nonce =
    let nonces =
      List.map pooled_transactions
        ~f:(Fn.compose User_command.nonce User_command.forget_check)
    in
    (* The last nonce gives us the maximum nonce in the transaction pool *)
    List.last nonces
  in
  match txn_pool_nonce with
  | Some nonce ->
      Participating_state.Option.return (Account.Nonce.succ nonce)
  | None ->
      let open Participating_state.Option.Let_syntax in
      let%map account = get_account t addr in
      account.Account.Poly.nonce

let get_nonce t (addr : Public_key.Compressed.t) =
  let open Participating_state.Option.Let_syntax in
  let%map account = get_account t addr in
  account.Account.Poly.nonce

let send_user_command t (txn : User_command.t) =
  Deferred.return
  @@
  let public_key = Public_key.compress txn.sender in
  let open Participating_state.Let_syntax in
  let%map account_opt = get_account t public_key in
  let open Or_error.Let_syntax in
  let%map () = schedule_user_command t txn account_opt in
  record_payment t txn (Option.value_exn account_opt)

let get_balance t (addr : Public_key.Compressed.t) =
  let open Participating_state.Option.Let_syntax in
  let%map account = get_account t addr in
  account.Account.Poly.balance

let get_trust_status t (ip_address : Unix.Inet_addr.Blocking_sexp.t) =
  let config = Coda_lib.config t in
  let trust_system = config.trust_system in
  Trust_system.lookup trust_system ip_address

let get_trust_status_all t =
  let config = Coda_lib.config t in
  let trust_system = config.trust_system in
  Trust_system.peer_statuses trust_system

let reset_trust_status t (ip_address : Unix.Inet_addr.Blocking_sexp.t) =
  let config = Coda_lib.config t in
  let trust_system = config.trust_system in
  Trust_system.reset trust_system ip_address

let replace_proposers keys pks =
  let kps =
    List.filter_map pks ~f:(fun pk ->
        let open Option.Let_syntax in
        let%map kps =
          Coda_lib.wallets keys |> Secrets.Wallets.find ~needle:pk
        in
        (kps, pk) )
  in
  Coda_lib.replace_propose_keypairs keys
    (Keypair.And_compressed_pk.Set.of_list kps) ;
  kps |> List.map ~f:snd

let setup_user_command ~fee ~nonce ~memo ~sender_kp user_command_body =
  let payload =
    User_command.Payload.create ~fee ~nonce ~memo ~body:user_command_body
  in
  let signed_user_command = User_command.sign sender_kp payload in
  User_command.forget_check signed_user_command

module Receipt_chain_hash = struct
  (* Receipt.Chain_hash does not have bin_io *)
  include Receipt.Chain_hash.Stable.V1

  [%%define_locally
  Receipt.Chain_hash.(cons, empty)]
end

module Payment_verifier =
  Receipt_chain_database_lib.Verifier.Make (User_command) (Receipt_chain_hash)

let verify_payment t (addr : Public_key.Compressed.Stable.Latest.t)
    (verifying_txn : User_command.t) proof =
  let open Participating_state.Let_syntax in
  let%map account = get_account t addr in
  let account = Option.value_exn account in
  let resulting_receipt = account.Account.Poly.receipt_chain_hash in
  let open Or_error.Let_syntax in
  let%bind () = Payment_verifier.verify ~resulting_receipt proof in
  if
    List.exists (Payment_proof.payments proof) ~f:(fun txn ->
        User_command.equal verifying_txn txn )
  then Ok ()
  else
    Or_error.errorf
      !"Merkle list proof does not contain payment %{sexp:User_command.t}"
      verifying_txn

(* TODO: Properly record receipt_chain_hash for multiple transactions. See #1143 *)
let schedule_user_commands t txns =
  List.map txns ~f:(fun (txn : User_command.t) ->
      let public_key = Public_key.compress txn.sender in
      let open Participating_state.Let_syntax in
      let%map account_opt = get_account t public_key in
      match schedule_user_command t txn account_opt with
      | Ok () ->
          ()
      | Error err ->
          let logger =
            Logger.extend
              (Coda_lib.top_level_logger t)
              [("coda_command", `String "scheduling a user command")]
          in
          Logger.warn logger ~module_:__MODULE__ ~location:__LOC__
            ~metadata:[("error", `String (Error.to_string_hum err))]
            "Failure in schedule_user_commands: $error. This is not yet \
             reported to the client, see #1143" )
  |> Participating_state.sequence
  |> Participating_state.map ~f:ignore

let prove_receipt t ~proving_receipt ~resulting_receipt :
    Payment_proof.t Deferred.Or_error.t =
  let receipt_chain_database = Coda_lib.receipt_chain_database t in
  (* TODO: since we are making so many reads to `receipt_chain_database`,
     reads should be async to not get IO-blocked. See #1125 *)
  let result =
    Receipt_chain_database.prove receipt_chain_database ~proving_receipt
      ~resulting_receipt
  in
  Deferred.return result

let start_time = Time_ns.now ()

type active_state_fields =
  { num_accounts: int option
  ; block_height: int option
  ; ledger_merkle_root: string option
  ; state_hash: string option
  ; consensus_time_best_tip: string option }

let get_status ~flag t =
  let open Coda_lib.Config in
  let uptime_secs =
    Time_ns.diff (Time_ns.now ()) start_time
    |> Time_ns.Span.to_sec |> Int.of_float
  in
  let commit_id = Coda_version.commit_id in
  let conf_dir = (Coda_lib.config t).conf_dir in
  let peers =
    List.map (Coda_lib.peers t) ~f:(fun peer ->
        Network_peer.Peer.to_discovery_host_and_port peer
        |> Host_and_port.to_string )
  in
  let user_commands_sent = !txn_count in
  let run_snark_worker = Option.is_some (Coda_lib.snark_worker_key t) in
  let propose_pubkeys = Coda_lib.propose_public_keys t in
  let consensus_mechanism = Consensus.name in
  let consensus_time_now =
    Consensus.time_hum (Block_time.now (Coda_lib.config t).time_controller)
  in
  let consensus_configuration = Consensus.Configuration.t in
  let r = Perf_histograms.report in
  let histograms =
    match flag with
    | `Performance ->
        let rpc_timings =
          let open Daemon_rpcs.Types.Status.Rpc_timings in
          { get_staged_ledger_aux=
              { Rpc_pair.dispatch= r ~name:"rpc_dispatch_get_staged_ledger_aux"
              ; impl= r ~name:"rpc_impl_get_staged_ledger_aux" }
          ; answer_sync_ledger_query=
              { Rpc_pair.dispatch=
                  r ~name:"rpc_dispatch_answer_sync_ledger_query"
              ; impl= r ~name:"rpc_impl_answer_sync_ledger_query" }
          ; get_ancestry=
              { Rpc_pair.dispatch= r ~name:"rpc_dispatch_get_ancestry"
              ; impl= r ~name:"rpc_impl_get_ancestry" }
          ; get_transition_chain_witness=
              { Rpc_pair.dispatch=
                  r ~name:"rpc_dispatch_get_transition_chain_witness"
              ; impl= r ~name:"rpc_impl_get_transition_chain_witness" }
          ; get_transition_chain=
              { Rpc_pair.dispatch= r ~name:"rpc_dispatch_get_transition_chain"
              ; impl= r ~name:"rpc_impl_get_transition_chain" } }
        in
        Some
          { Daemon_rpcs.Types.Status.Histograms.rpc_timings
          ; external_transition_latency= r ~name:"external_transition_latency"
          ; accepted_transition_local_latency=
              r ~name:"accepted_transition_local_latency"
          ; accepted_transition_remote_latency=
              r ~name:"accepted_transition_remote_latency"
          ; snark_worker_transition_time=
              r ~name:"snark_worker_transition_time"
          ; snark_worker_merge_time= r ~name:"snark_worker_merge_time" }
    | `None ->
        None
  in
  let max_block_height_observed =
    Length.to_int @@ Consensus.Data.Consensus_state.block_height
    @@ Coda_transition.External_transition.consensus_state
    @@ Pipe_lib.Broadcast_pipe.Reader.peek
         (Coda_lib.most_recent_valid_transition t)
  in
  let active_status () =
    let open Participating_state.Let_syntax in
    let%bind ledger = Coda_lib.best_ledger t in
    let ledger_merkle_root =
      Ledger.merkle_root ledger |> Ledger_hash.to_string
    in
    let num_accounts = Ledger.num_accounts ledger in
    let%bind state = Coda_lib.best_protocol_state t in
    let state_hash = Protocol_state.hash state |> State_hash.to_base58_check in
    let consensus_state = state |> Protocol_state.consensus_state in
    let block_height =
      Length.to_int
      @@ Consensus.Data.Consensus_state.block_height consensus_state
    in
    let%map sync_status =
      Coda_incremental.Status.stabilize () ;
      match
        Coda_incremental.Status.Observer.value_exn @@ Coda_lib.sync_status t
      with
      | `Bootstrap ->
          `Bootstrapping
      | `Connecting ->
          `Active `Connecting
      | `Listening ->
          `Active `Listening
      | `Offline ->
          `Active `Offline
      | `Synced ->
          `Active `Synced
      | `Catchup ->
          `Active `Catchup
    in
    let consensus_time_best_tip =
      Consensus.Data.Consensus_state.time_hum consensus_state
    in
    ( sync_status
    , { num_accounts= Some num_accounts
      ; block_height= Some block_height
      ; ledger_merkle_root= Some ledger_merkle_root
      ; state_hash= Some state_hash
      ; consensus_time_best_tip= Some consensus_time_best_tip } )
  in
  let ( sync_status
      , { num_accounts
        ; block_height
        ; ledger_merkle_root
        ; state_hash
        ; consensus_time_best_tip } ) =
    match active_status () with
    | `Active result ->
        result
    | `Bootstrapping ->
        ( `Bootstrap
        , { num_accounts= None
          ; block_height= None
          ; ledger_merkle_root= None
          ; state_hash= None
          ; consensus_time_best_tip= None } )
  in
  { Daemon_rpcs.Types.Status.num_accounts
  ; sync_status
  ; block_height
  ; max_block_height_observed
  ; uptime_secs
  ; ledger_merkle_root
  ; state_hash
  ; consensus_time_best_tip
  ; commit_id
  ; conf_dir
  ; peers
  ; user_commands_sent
  ; run_snark_worker
  ; propose_pubkeys= Public_key.Compressed.Set.to_list propose_pubkeys
  ; histograms
  ; consensus_time_now
  ; consensus_mechanism
  ; consensus_configuration }

let clear_hist_status ~flag t = Perf_histograms.wipe () ; get_status ~flag t

module Subscriptions = struct
  let new_block t public_key =
    let subscription = Coda_lib.subscription t in
    Coda_lib.Subscriptions.add_block_subscriber subscription public_key

  let reorganization t =
    let subscription = Coda_lib.subscription t in
    Coda_lib.Subscriptions.add_reorganization_subscriber subscription
end

module For_tests = struct
  let get_all_user_commands coda public_key =
    let external_transition_database =
      Coda_lib.external_transition_database coda
    in
    let user_commands =
      List.concat_map
        ~f:
          (Fn.compose
             Auxiliary_database.Filtered_external_transition.user_commands
             With_hash.data)
      @@ Auxiliary_database.External_transition_database.get_values
           external_transition_database public_key
    in
    let participants_user_commands =
      User_command.filter_by_participant user_commands public_key
    in
    List.dedup_and_sort participants_user_commands
      ~compare:User_command.compare

  module Subscriptions = struct
    let new_user_commands coda public_key =
      Coda_lib.add_payment_subscriber coda public_key
  end
end
