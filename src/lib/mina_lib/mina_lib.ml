open Core_kernel
open Async
open Unsigned
open Mina_base
open Mina_transition
open Pipe_lib
open Strict_pipe
open Signature_lib
open O1trace
open Otp_lib
open Network_peer
module Archive_client = Archive_client
module Config = Config
module Conf_dir = Conf_dir
module Subscriptions = Coda_subscriptions
module Snark_worker_lib = Snark_worker
module Timeout = Timeout_lib.Core_time

type Structured_log_events.t += Connecting
  [@@deriving register_event {msg= "Mina daemon is connecting"}]

type Structured_log_events.t += Listening
  [@@deriving register_event {msg= "Mina daemon is listening"}]

type Structured_log_events.t += Bootstrapping
  [@@deriving register_event {msg= "Mina daemon is bootstrapping"}]

type Structured_log_events.t += Ledger_catchup
  [@@deriving register_event {msg= "Mina daemon is doing ledger catchup"}]

type Structured_log_events.t += Synced
  [@@deriving register_event {msg= "Mina daemon is synced"}]

type Structured_log_events.t +=
  | Rebroadcast_transition of {state_hash: State_hash.t}
  [@@deriving register_event {msg= "Rebroadcasting $state_hash"}]

exception Snark_worker_error of int

exception Snark_worker_signal_interrupt of Signal.t

(* A way to run a single snark worker for a daemon in a lazy manner. Evaluating
   this lazy value will run the snark worker process. A snark work is
   assigned to a public key. This public key can change throughout the entire time
   the daemon is running *)
type snark_worker =
  { public_key: Public_key.Compressed.t
  ; process: Process.t Ivar.t
  ; kill_ivar: unit Ivar.t }

type processes =
  { prover: Prover.t
  ; verifier: Verifier.t
  ; mutable snark_worker:
      [`On of snark_worker * Currency.Fee.t | `Off of Currency.Fee.t] }

type components =
  { net: Mina_networking.t
  ; transaction_pool: Network_pool.Transaction_pool.t
  ; snark_pool: Network_pool.Snark_pool.t
  ; transition_frontier: Transition_frontier.t option Broadcast_pipe.Reader.t
  ; most_recent_valid_block:
      External_transition.Initial_validated.t Broadcast_pipe.Reader.t }

type pipes =
  { validated_transitions_reader:
      External_transition.Validated.t Strict_pipe.Reader.t
  ; producer_transition_writer:
      (Transition_frontier.Breadcrumb.t, synchronous, unit Deferred.t) Writer.t
  ; external_transitions_writer:
      ( External_transition.t Envelope.Incoming.t
      * Block_time.t
      * Mina_net2.Validation_callback.t )
      Pipe.Writer.t
  ; user_command_input_writer:
      ( User_command_input.t list
        * (   ( Network_pool.Transaction_pool.Resource_pool.Diff.t
              * Network_pool.Transaction_pool.Resource_pool.Diff.Rejected.t )
              Or_error.t
           -> unit)
        * (   Account_id.t
           -> ( [`Min of Mina_base.Account.Nonce.t] * Mina_base.Account.Nonce.t
              , string )
              Result.t)
        * (Account_id.t -> Account.t option Participating_state.T.t)
      , Strict_pipe.synchronous
      , unit Deferred.t )
      Strict_pipe.Writer.t
  ; user_command_writer:
      ( User_command.t list
        * (   ( Network_pool.Transaction_pool.Resource_pool.Diff.t
              * Network_pool.Transaction_pool.Resource_pool.Diff.Rejected.t )
              Or_error.t
           -> unit)
      , Strict_pipe.synchronous
      , unit Deferred.t )
      Strict_pipe.Writer.t
  ; snapp_command_input_writer:
      ( Snapp_command_input.t list
        * (   ( Network_pool.Transaction_pool.Resource_pool.Diff.t
              * Network_pool.Transaction_pool.Resource_pool.Diff.Rejected.t )
              Or_error.t
           -> unit)
        * (   Account_id.t
           -> ( [`Min of Mina_base.Account.Nonce.t] * Mina_base.Account.Nonce.t
              , string )
              Result.t)
        * (Account_id.t -> Account.t option Participating_state.T.t)
      , Strict_pipe.synchronous
      , unit Deferred.t )
      Strict_pipe.Writer.t
  ; local_snark_work_writer:
      ( Network_pool.Snark_pool.Resource_pool.Diff.t
        * (   ( Network_pool.Snark_pool.Resource_pool.Diff.t
              * Network_pool.Snark_pool.Resource_pool.Diff.rejected )
              Or_error.t
           -> unit)
      , Strict_pipe.synchronous
      , unit Deferred.t )
      Strict_pipe.Writer.t }

type t =
  { config: Config.t
  ; processes: processes
  ; components: components
  ; initialization_finish_signal: unit Ivar.t
  ; pipes: pipes
  ; wallets: Secrets.Wallets.t
  ; coinbase_receiver: Consensus.Coinbase_receiver.t ref
  ; block_production_keypairs:
      (Agent.read_write Agent.flag, Keypair.And_compressed_pk.Set.t) Agent.t
  ; snark_job_state: Work_selector.State.t
  ; mutable next_producer_timing:
      Daemon_rpcs.Types.Status.Next_producer_timing.t option
  ; subscriptions: Coda_subscriptions.t
  ; sync_status: Sync_status.t Mina_incremental.Status.Observer.t
  ; precomputed_block_writer: ([`Path of string] option * [`Log] option) ref
  ; block_production_status:
      [`Producing | `Producing_in_ms of float | `Free] ref }
[@@deriving fields]

let time_controller t = t.config.time_controller

let subscription t = t.subscriptions

let peek_frontier frontier_broadcast_pipe =
  Broadcast_pipe.Reader.peek frontier_broadcast_pipe
  |> Result.of_option
       ~error:
         (Error.of_string
            "Cannot retrieve transition frontier now. Bootstrapping right now.")

let client_port t =
  let {Node_addrs_and_ports.client_port; _} =
    t.config.gossip_net_params.addrs_and_ports
  in
  client_port

(* Get the most recently set public keys  *)
let block_production_pubkeys t : Public_key.Compressed.Set.t =
  let public_keys, _ = Agent.get t.block_production_keypairs in
  Public_key.Compressed.Set.map public_keys ~f:snd

let coinbase_receiver t = !(t.coinbase_receiver)

let replace_coinbase_receiver t coinbase_receiver =
  [%log' info t.config.logger]
    "Changing the coinbase receiver for produced blocks from $old_receiver to \
     $new_receiver"
    ~metadata:
      [ ( "old_receiver"
        , Consensus.Coinbase_receiver.to_yojson !(t.coinbase_receiver) )
      ; ( "new_receiver"
        , Consensus.Coinbase_receiver.to_yojson coinbase_receiver ) ] ;
  t.coinbase_receiver := coinbase_receiver

let replace_block_production_keypairs t kps =
  Agent.update t.block_production_keypairs kps

let log_snark_worker_warning t =
  if Option.is_some t.config.snark_coordinator_key then
    [%log' warn t.config.logger]
      "The snark coordinator flag is set; running a snark worker will \
       override the snark coordinator key"

let log_snark_coordinator_warning (config : Config.t) snark_worker =
  if Option.is_some config.snark_coordinator_key then
    match snark_worker with
    | `On _ ->
        [%log' warn config.logger]
          "The snark coordinator key will be ignored because the snark worker \
           key is set "
    | _ ->
        ()

module Snark_worker = struct
  let run_process ~logger ~proof_level client_port kill_ivar num_threads =
    let env =
      Option.map
        ~f:(fun num -> `Extend [("RAYON_NUM_THREADS", string_of_int num)])
        num_threads
    in
    let%map snark_worker_process =
      let our_binary = Sys.executable_name in
      Process.create_exn () ~prog:our_binary ?env
        ~args:
          ( "internal" :: Snark_worker.Intf.command_name
          :: Snark_worker.arguments ~proof_level
               ~daemon_address:
                 (Host_and_port.create ~host:"127.0.0.1" ~port:client_port)
               ~shutdown_on_disconnect:false )
    in
    Child_processes.Termination.wait_for_process_log_errors ~logger
      snark_worker_process ~module_:__MODULE__ ~location:__LOC__ ;
    don't_wait_for
      ( match%bind
          Monitor.try_with ~here:[%here] (fun () ->
              Process.wait snark_worker_process )
        with
      | Ok signal_or_error -> (
        match signal_or_error with
        | Ok () ->
            [%log info] "Snark worker process died" ;
            if Ivar.is_full kill_ivar then
              [%log error] "Ivar.fill bug is here!" ;
            Ivar.fill kill_ivar () ;
            Deferred.unit
        | Error (`Exit_non_zero non_zero_error) ->
            [%log fatal]
              !"Snark worker process died with a nonzero error %i"
              non_zero_error ;
            raise (Snark_worker_error non_zero_error)
        | Error (`Signal signal) ->
            [%log fatal]
              !"Snark worker died with signal %{sexp:Signal.t}. Aborting daemon"
              signal ;
            raise (Snark_worker_signal_interrupt signal) )
      | Error exn ->
          [%log info]
            !"Exception when waiting for snark worker process to terminate: \
              $exn"
            ~metadata:[("exn", `String (Exn.to_string exn))] ;
          Deferred.unit ) ;
    [%log trace]
      !"Created snark worker with pid: %i"
      (Pid.to_int @@ Process.pid snark_worker_process) ;
    (* We want these to be printfs so we don't double encode our logs here *)
    Pipe.iter_without_pushback
      (Async.Reader.pipe (Process.stdout snark_worker_process))
      ~f:(fun s -> printf "%s" s)
    |> don't_wait_for ;
    Pipe.iter_without_pushback
      (Async.Reader.pipe (Process.stderr snark_worker_process))
      ~f:(fun s -> printf "%s" s)
    |> don't_wait_for ;
    snark_worker_process

  let start t =
    match t.processes.snark_worker with
    | `On ({process= process_ivar; kill_ivar; _}, _) ->
        [%log' debug t.config.logger] !"Starting snark worker process" ;
        log_snark_worker_warning t ;
        let%map snark_worker_process =
          run_process ~logger:t.config.logger
            ~proof_level:t.config.precomputed_values.proof_level
            t.config.gossip_net_params.addrs_and_ports.client_port kill_ivar
            t.config.snark_worker_config.num_threads
        in
        [%log' debug t.config.logger]
          ~metadata:
            [ ( "snark_worker_pid"
              , `Int (Pid.to_int (Process.pid snark_worker_process)) ) ]
          "Started snark worker process with pid: $snark_worker_pid" ;
        if Ivar.is_full process_ivar then
          [%log' error t.config.logger] "Ivar.fill bug is here!" ;
        Ivar.fill process_ivar snark_worker_process
    | `Off _ ->
        [%log' info t.config.logger]
          !"Attempted to turn on snark worker, but snark worker key is set to \
            none" ;
        Deferred.unit

  let stop ?(should_wait_kill = false) t =
    match t.processes.snark_worker with
    | `On ({public_key= _; process; kill_ivar}, _) ->
        let%bind process = Ivar.read process in
        [%log' info t.config.logger]
          "Killing snark worker process with pid: $snark_worker_pid"
          ~metadata:
            [("snark_worker_pid", `Int (Pid.to_int (Process.pid process)))] ;
        Signal.send_exn Signal.term (`Pid (Process.pid process)) ;
        if should_wait_kill then Ivar.read kill_ivar else Deferred.unit
    | `Off _ ->
        [%log' warn t.config.logger]
          "Attempted to turn off snark worker, but no snark worker was running" ;
        Deferred.unit

  let get_key {processes= {snark_worker; _}; _} =
    match snark_worker with
    | `On ({public_key; _}, _) ->
        Some public_key
    | `Off _ ->
        None

  let replace_key ({processes= {snark_worker; _}; config= {logger; _}; _} as t)
      new_key =
    match (snark_worker, new_key) with
    | `Off _, None ->
        [%log info]
          "Snark work is still not happening since keys snark worker keys are \
           still set to None" ;
        Deferred.unit
    | `Off fee, Some new_key ->
        let process = Ivar.create () in
        let kill_ivar = Ivar.create () in
        t.processes.snark_worker
        <- `On ({public_key= new_key; process; kill_ivar}, fee) ;
        start t
    | `On ({public_key= old; process; kill_ivar}, fee), Some new_key ->
        [%log debug]
          !"Changing snark worker key from $old to $new"
          ~metadata:
            [ ("old", Public_key.Compressed.to_yojson old)
            ; ("new", Public_key.Compressed.to_yojson new_key) ] ;
        t.processes.snark_worker
        <- `On ({public_key= new_key; process; kill_ivar}, fee) ;
        Deferred.unit
    | `On (_, fee), None ->
        let%map () = stop t in
        t.processes.snark_worker <- `Off fee
end

let replace_snark_worker_key = Snark_worker.replace_key

let snark_worker_key = Snark_worker.get_key

let snark_coordinator_key t = t.config.snark_coordinator_key

let stop_snark_worker = Snark_worker.stop

let best_tip_opt t =
  let open Option.Let_syntax in
  let%map frontier =
    Broadcast_pipe.Reader.peek t.components.transition_frontier
  in
  Transition_frontier.best_tip frontier

let transition_frontier t = t.components.transition_frontier

let root_length_opt t =
  let open Option.Let_syntax in
  let%map frontier =
    Broadcast_pipe.Reader.peek t.components.transition_frontier
  in
  Transition_frontier.root_length frontier

let best_staged_ledger_opt t =
  let open Option.Let_syntax in
  let%map tip = best_tip_opt t in
  Transition_frontier.Breadcrumb.staged_ledger tip

let best_protocol_state_opt t =
  let open Option.Let_syntax in
  let%map tip = best_tip_opt t in
  Transition_frontier.Breadcrumb.protocol_state tip

let best_ledger_opt t =
  let open Option.Let_syntax in
  let%map staged_ledger = best_staged_ledger_opt t in
  Staged_ledger.ledger staged_ledger

let compose_of_option f =
  Fn.compose
    (Option.value_map ~default:`Bootstrapping ~f:(fun x -> `Active x))
    f

let best_tip = compose_of_option best_tip_opt

let root_length = compose_of_option root_length_opt

let active_or_bootstrapping =
  compose_of_option (fun t ->
      Option.bind
        (Broadcast_pipe.Reader.peek t.components.transition_frontier)
        ~f:(Fn.const (Some ())) )

(* This is a hack put in place to deal with nodes getting stuck
   in Offline states, that is, not receiving blocks for an extended period.

   To address this, we restart the libp2p helper when we become offline. *)
let next_helper_restart = ref None

let create_sync_status_observer ~logger ~is_seed ~demo_mode ~net
    ~transition_frontier_and_catchup_signal_incr ~online_status_incr
    ~first_connection_incr ~first_message_incr =
  let open Mina_incremental.Status in
  let restart_delay = Time.Span.of_min 5. in
  let incremental_status =
    map4 online_status_incr transition_frontier_and_catchup_signal_incr
      first_connection_incr first_message_incr
      ~f:(fun online_status active_status first_connection first_message ->
        (* Always be synced in demo mode, we don't expect peers to connect to us *)
        if demo_mode then `Synced
        else
          match online_status with
          | `Offline ->
              ( match !next_helper_restart with
              | None ->
                  next_helper_restart :=
                    Some
                      (Async.Clock.Event.run_after restart_delay
                         (fun () ->
                           Mina_networking.restart_helper net ;
                           next_helper_restart := None )
                         ())
              | Some _ ->
                  () ) ;
              if `Empty = first_connection then (
                [%str_log info] Connecting ;
                `Connecting )
              else if `Empty = first_message then (
                [%str_log info] Listening ;
                `Listening )
              else `Offline
          | `Online -> (
              Option.iter !next_helper_restart ~f:(fun e ->
                  Async.Clock.Event.abort_if_possible e () ) ;
              next_helper_restart := None ;
              match active_status with
              | None ->
                  let logger = Logger.create () in
                  [%str_log info] Bootstrapping ;
                  `Bootstrap
              | Some (_, catchup_jobs) ->
                  let logger = Logger.create () in
                  if catchup_jobs > 0 then (
                    [%str_log info] Ledger_catchup ;
                    `Catchup )
                  else (
                    [%str_log info] Synced ;
                    `Synced ) ) )
  in
  let observer = observe incremental_status in
  (* monitor Mina status, issue a warning if offline for too long (unless we are a seed node) *)
  ( if not is_seed then
    let offline_timeout_min = 15.0 in
    let offline_timeout_duration = Time.Span.of_min offline_timeout_min in
    let offline_timeout = ref None in
    let offline_warned = ref false in
    let log_offline_warning _tm =
      [%log error]
        "Daemon has not received any gossip messages for %0.0f minutes; check \
         the daemon's external port forwarding, if needed"
        offline_timeout_min ;
      offline_warned := true
    in
    let start_offline_timeout () =
      match !offline_timeout with
      | Some _ ->
          ()
      | None ->
          offline_timeout :=
            Some
              (Timeout.create () offline_timeout_duration
                 ~f:log_offline_warning)
    in
    let stop_offline_timeout () =
      match !offline_timeout with
      | Some timeout ->
          if !offline_warned then (
            [%log info]
              "Daemon had been offline (no gossip messages received), now \
               back online" ;
            offline_warned := false ) ;
          Timeout.cancel () timeout () ;
          offline_timeout := None
      | None ->
          ()
    in
    let handle_status_change status =
      if status = `Offline then start_offline_timeout ()
      else stop_offline_timeout ()
    in
    Observer.on_update_exn observer ~f:(function
      | Initialized value ->
          handle_status_change value
      | Changed (_, value) ->
          handle_status_change value
      | Invalidated ->
          () ) ) ;
  (* recompute Mina status on an interval *)
  stabilize () ;
  every (Time.Span.of_sec 15.0) ~stop:(never ()) stabilize ;
  observer

let sync_status t = t.sync_status

let visualize_frontier ~filename =
  compose_of_option
  @@ fun t ->
  let open Option.Let_syntax in
  let%map frontier =
    Broadcast_pipe.Reader.peek t.components.transition_frontier
  in
  Transition_frontier.visualize ~filename frontier

let best_staged_ledger = compose_of_option best_staged_ledger_opt

let best_protocol_state = compose_of_option best_protocol_state_opt

let best_ledger = compose_of_option best_ledger_opt

let get_ledger t state_hash_opt =
  let open Or_error.Let_syntax in
  let%bind state_hash =
    Option.value_map state_hash_opt ~f:Or_error.return
      ~default:
        ( match best_tip t with
        | `Active bc ->
            Or_error.return (Frontier_base.Breadcrumb.state_hash bc)
        | `Bootstrapping ->
            Or_error.error_string
              "get_ledger: can't get staged ledger hash while bootstrapping" )
  in
  let%bind frontier = t.components.transition_frontier |> peek_frontier in
  match Transition_frontier.find frontier state_hash with
  | Some b ->
      let staged_ledger = Transition_frontier.Breadcrumb.staged_ledger b in
      Ok (Ledger.to_list (Staged_ledger.ledger staged_ledger))
  | None ->
      Or_error.error_string
        "get_ledger: state hash not found in transition frontier"

let get_snarked_ledger t state_hash_opt =
  let open Or_error.Let_syntax in
  let%bind state_hash =
    Option.value_map state_hash_opt ~f:Or_error.return
      ~default:
        ( match best_tip t with
        | `Active bc ->
            Or_error.return (Frontier_base.Breadcrumb.state_hash bc)
        | `Bootstrapping ->
            Or_error.error_string
              "get_snarked_ledger: can't get snarked ledger hash while \
               bootstrapping" )
  in
  let%bind frontier = t.components.transition_frontier |> peek_frontier in
  match Transition_frontier.find frontier state_hash with
  | Some b ->
      let root_snarked_ledger =
        Transition_frontier.root_snarked_ledger frontier
      in
      let ledger = Ledger.of_database root_snarked_ledger in
      let path = Transition_frontier.path_map frontier b ~f:Fn.id in
      let%bind _ =
        List.fold_until ~init:(Ok ()) path
          ~f:(fun _acc b ->
            if Transition_frontier.Breadcrumb.just_emitted_a_proof b then
              match
                Staged_ledger.proof_txns_with_state_hashes
                  (Transition_frontier.Breadcrumb.staged_ledger b)
              with
              | None ->
                  Stop
                    (Or_error.error_string
                       (sprintf
                          "No transactions corresponding to the emitted proof \
                           for state_hash:%s"
                          (State_hash.to_string
                             (Transition_frontier.Breadcrumb.state_hash b))))
              | Some txns -> (
                match
                  List.fold_until ~init:(Ok ())
                    (Non_empty_list.to_list txns)
                    ~f:(fun _acc (txn, state_hash) ->
                      (*Validate transactions against the protocol state associated with the transaction*)
                      match
                        Transition_frontier.find_protocol_state frontier
                          state_hash
                      with
                      | Some state -> (
                          let txn_state_view =
                            Mina_state.Protocol_state.body state
                            |> Mina_state.Protocol_state.Body.view
                          in
                          match
                            Ledger.apply_transaction
                              ~constraint_constants:
                                t.config.precomputed_values
                                  .constraint_constants ~txn_state_view ledger
                              txn.data
                          with
                          | Ok _ ->
                              Continue (Ok ())
                          | e ->
                              Stop (Or_error.map e ~f:ignore) )
                      | None ->
                          Stop
                            (Or_error.errorf
                               !"Coudln't find protocol state with hash %s"
                               (State_hash.to_string state_hash)) )
                    ~finish:Fn.id
                with
                | Ok _ ->
                    Continue (Ok ())
                | e ->
                    Stop e )
            else Continue (Ok ()) )
          ~finish:Fn.id
      in
      let snarked_ledger_hash =
        Transition_frontier.Breadcrumb.blockchain_state b
        |> Mina_state.Blockchain_state.snarked_ledger_hash
      in
      let merkle_root = Ledger.merkle_root ledger in
      if Frozen_ledger_hash.equal snarked_ledger_hash merkle_root then (
        let res = Ledger.to_list ledger in
        ignore @@ Ledger.unregister_mask_exn ~loc:__LOC__ ledger ;
        Ok res )
      else
        Or_error.errorf
          "Expected snarked ledger hash %s but got %s for state hash %s"
          (Frozen_ledger_hash.to_string snarked_ledger_hash)
          (Frozen_ledger_hash.to_string merkle_root)
          (State_hash.to_string state_hash)
  | None ->
      Or_error.error_string
        "get_snarked_ledger: state hash not found in transition frontier"

let get_account t aid =
  let open Participating_state.Let_syntax in
  let%map ledger = best_ledger t in
  let open Option.Let_syntax in
  let%bind loc = Ledger.location_of_account ledger aid in
  Ledger.get ledger loc

let get_inferred_nonce_from_transaction_pool_and_ledger t
    (account_id : Account_id.t) =
  let transaction_pool = t.components.transaction_pool in
  let resource_pool =
    Network_pool.Transaction_pool.resource_pool transaction_pool
  in
  let pooled_transactions =
    Network_pool.Transaction_pool.Resource_pool.all_from_account resource_pool
      account_id
  in
  let txn_pool_nonce =
    let nonces =
      List.map pooled_transactions
        ~f:
          (Fn.compose User_command.nonce_exn
             Transaction_hash.User_command_with_valid_signature.command)
    in
    (* The last nonce gives us the maximum nonce in the transaction pool *)
    List.last nonces
  in
  match txn_pool_nonce with
  | Some nonce ->
      Participating_state.Option.return (Account.Nonce.succ nonce)
  | None ->
      let open Participating_state.Option.Let_syntax in
      let%map account = get_account t account_id in
      account.Account.Poly.nonce

let snark_job_state t = t.snark_job_state

let add_block_subscriber t public_key =
  Coda_subscriptions.add_block_subscriber t.subscriptions public_key

let add_payment_subscriber t public_key =
  Coda_subscriptions.add_payment_subscriber t.subscriptions public_key

let transaction_pool t = t.components.transaction_pool

let snark_pool t = t.components.snark_pool

let peers t = Mina_networking.peers t.components.net

let initial_peers t = Mina_networking.initial_peers t.components.net

let snark_work_fee t =
  match t.processes.snark_worker with `On (_, fee) -> fee | `Off fee -> fee

let set_snark_work_fee t new_fee =
  t.processes.snark_worker
  <- ( match t.processes.snark_worker with
     | `On (config, _) ->
         `On (config, new_fee)
     | `Off _ ->
         `Off new_fee )

let top_level_logger t = t.config.logger

let most_recent_valid_transition t = t.components.most_recent_valid_block

let staged_ledger_ledger_proof t =
  let open Option.Let_syntax in
  let%bind sl = best_staged_ledger_opt t in
  Staged_ledger.current_ledger_proof sl

let validated_transitions t = t.pipes.validated_transitions_reader

module Root_diff = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { commands: User_command.Stable.V1.t With_status.Stable.V1.t list
        ; root_length: int }

      let to_latest = Fn.id
    end
  end]
end

let initialization_finish_signal t = t.initialization_finish_signal

(* TODO: this is a bad pattern for two reasons:
 *   - uses an abstraction leak to patch new functionality instead of making a new extension
 *   - every call to this function will create a new, unique pipe with it's own thread for transfering
 *     items from the identity extension with no route for termination
*)
let root_diff t =
  let root_diff_reader, root_diff_writer =
    Strict_pipe.create ~name:"root diff"
      (Buffered (`Capacity 30, `Overflow Crash))
  in
  trace_recurring_task "root diff pipe reader" (fun () ->
      let open Root_diff.Stable.Latest in
      let length_of_breadcrumb =
        Fn.compose Unsigned.UInt32.to_int
          Transition_frontier.Breadcrumb.blockchain_length
      in
      Broadcast_pipe.Reader.iter t.components.transition_frontier ~f:(function
        | None ->
            Deferred.unit
        | Some frontier ->
            let root = Transition_frontier.root frontier in
            Strict_pipe.Writer.write root_diff_writer
              { commands=
                  List.map
                    (Transition_frontier.Breadcrumb.commands root)
                    ~f:(With_status.map ~f:User_command.forget_check)
              ; root_length= length_of_breadcrumb root } ;
            Broadcast_pipe.Reader.iter
              Transition_frontier.(
                Extensions.(get_view_pipe (extensions frontier) Identity))
              ~f:
                (Deferred.List.iter ~f:(function
                  | Transition_frontier.Diff.Full.With_mutant.E (New_node _, _)
                    ->
                      Deferred.unit
                  | Transition_frontier.Diff.Full.With_mutant.E
                      (Best_tip_changed _, _) ->
                      Deferred.unit
                  | Transition_frontier.Diff.Full.With_mutant.E
                      (Root_transitioned {new_root; _}, _) ->
                      let root_hash =
                        Transition_frontier.Root_data.Limited.hash new_root
                      in
                      let new_root_breadcrumb =
                        Transition_frontier.(find_exn frontier root_hash)
                      in
                      Strict_pipe.Writer.write root_diff_writer
                        { commands=
                            Transition_frontier.Breadcrumb.commands
                              new_root_breadcrumb
                            |> List.map
                                 ~f:
                                   (With_status.map
                                      ~f:User_command.forget_check)
                        ; root_length= length_of_breadcrumb new_root_breadcrumb
                        } ;
                      Deferred.unit )) ) ) ;
  root_diff_reader

let dump_tf t =
  peek_frontier t.components.transition_frontier
  |> Or_error.map ~f:Transition_frontier.visualize_to_string

(** The [best_path coda] is the list of state hashes from the root to the best_tip in the transition frontier. It includes the root hash and the hash *)
let best_path t =
  let open Option.Let_syntax in
  let%map tf = Broadcast_pipe.Reader.peek t.components.transition_frontier in
  let bt = Transition_frontier.best_tip tf in
  List.cons
    Transition_frontier.(root tf |> Breadcrumb.state_hash)
    (Transition_frontier.hash_path tf bt)

let best_chain ?max_length t =
  let open Option.Let_syntax in
  let%map frontier =
    Broadcast_pipe.Reader.peek t.components.transition_frontier
  in
  let best_tip_path = Transition_frontier.best_tip_path ?max_length frontier in
  match max_length with
  | Some max_length when max_length <= List.length best_tip_path ->
      (* The [best_tip_path] has already been truncated to the correct length,
       we skip adding the root to stay below the maximum.
    *)
      best_tip_path
  | _ ->
      Transition_frontier.root frontier :: best_tip_path

let request_work t =
  let (module Work_selection_method) = t.config.work_selection_method in
  let fee = snark_work_fee t in
  let instances_opt =
    Work_selection_method.work ~logger:t.config.logger ~fee
      ~snark_pool:(snark_pool t) (snark_job_state t)
  in
  Option.map instances_opt ~f:(fun instances ->
      {Snark_work_lib.Work.Spec.instances; fee} )

let work_selection_method t = t.config.work_selection_method

let add_work t (work : Snark_worker_lib.Work.Result.t) =
  let (module Work_selection_method) = t.config.work_selection_method in
  let update_metrics () =
    let snark_pool = snark_pool t in
    let fee_opt =
      Option.map (snark_worker_key t) ~f:(fun _ -> snark_work_fee t)
    in
    let pending_work =
      Work_selection_method.pending_work_statements ~snark_pool ~fee_opt
        t.snark_job_state
      |> List.length
    in
    Mina_metrics.(
      Gauge.set Snark_work.pending_snark_work (Int.to_float pending_work))
  in
  let spec = work.spec.instances in
  let cb _ =
    (* remove it from seen jobs after attempting to adding it to the pool to avoid this work being reassigned
     * If the diff is accepted then remove it from the seen jobs.
     * If not then the work should have already been in the pool with a lower fee or the statement isn't referenced anymore or any other error. In any case remove it from the seen jobs so that it can be picked up if needed *)
    Work_selection_method.remove t.snark_job_state spec
  in
  let _ = Or_error.try_with (fun () -> update_metrics ()) in
  Strict_pipe.Writer.write t.pipes.local_snark_work_writer
    (Network_pool.Snark_pool.Resource_pool.Diff.of_result work, cb)
  |> Deferred.don't_wait_for

let get_current_nonce t aid =
  match
    Participating_state.active
      (get_inferred_nonce_from_transaction_pool_and_ledger t aid)
    |> Option.join
  with
  | None ->
      Error
        "Couldn't infer nonce for transaction from specified `sender` since \
         `sender` is not in the ledger or sent a transaction in transaction \
         pool."
  | Some nonce ->
      let ledger_nonce =
        Participating_state.active (get_account t aid)
        |> Option.join
        |> Option.map ~f:(fun {Account.Poly.nonce; _} -> nonce)
        |> Option.value ~default:nonce
      in
      Ok (`Min ledger_nonce, nonce)

let add_transactions t (uc_inputs : User_command_input.t list) =
  let result_ivar = Ivar.create () in
  Strict_pipe.Writer.write t.pipes.user_command_input_writer
    (uc_inputs, Ivar.fill result_ivar, get_current_nonce t, get_account t)
  |> Deferred.don't_wait_for ;
  Ivar.read result_ivar

let add_snapp_transactions t (snapp_inputs : Snapp_command_input.t list) =
  let result_ivar = Ivar.create () in
  Strict_pipe.Writer.write t.pipes.snapp_command_input_writer
    (snapp_inputs, Ivar.fill result_ivar, get_current_nonce t, get_account t)
  |> Deferred.don't_wait_for ;
  Ivar.read result_ivar

let add_full_transactions t user_command =
  let result_ivar = Ivar.create () in
  Strict_pipe.Writer.write t.pipes.user_command_writer
    (user_command, Ivar.fill result_ivar)
  |> Deferred.don't_wait_for ;
  Ivar.read result_ivar

let next_producer_timing t = t.next_producer_timing

let staking_ledger t =
  let open Option.Let_syntax in
  let consensus_constants = t.config.precomputed_values.consensus_constants in
  let%map transition_frontier =
    Broadcast_pipe.Reader.peek t.components.transition_frontier
  in
  let consensus_state =
    Transition_frontier.Breadcrumb.consensus_state
      (Transition_frontier.best_tip transition_frontier)
  in
  let local_state = t.config.consensus_local_state in
  Consensus.Hooks.get_epoch_ledger ~constants:consensus_constants
    ~consensus_state ~local_state

let next_epoch_ledger t =
  let open Option.Let_syntax in
  let%map frontier =
    Broadcast_pipe.Reader.peek t.components.transition_frontier
  in
  let root = Transition_frontier.root frontier in
  let root_epoch =
    Transition_frontier.Breadcrumb.consensus_state root
    |> Consensus.Data.Consensus_state.epoch_count
  in
  let best_tip = Transition_frontier.best_tip frontier in
  let best_tip_epoch =
    Transition_frontier.Breadcrumb.consensus_state best_tip
    |> Consensus.Data.Consensus_state.epoch_count
  in
  if
    Mina_numbers.Length.(
      equal root_epoch best_tip_epoch || equal best_tip_epoch zero)
  then
    (*root is in the same epoch as the best tip and so the next epoch ledger in the local state will be updated by Proof_of_stake.frontier_root_transition. Next epoch ledger in genesis epoch is the genesis ledger*)
    `Finalized
      (Consensus.Data.Local_state.next_epoch_ledger
         t.config.consensus_local_state)
  else
    (*No blocks in the new epoch is finalized yet, return nothing*)
    `Notfinalized

let find_delegators table pk =
  Option.value_map
    (Public_key.Compressed.Table.find table pk)
    ~default:[] ~f:Mina_base.Account.Index.Table.data

let current_epoch_delegators t ~pk =
  let open Option.Let_syntax in
  let%map _transition_frontier =
    Broadcast_pipe.Reader.peek t.components.transition_frontier
  in
  let current_epoch_delegatee_table =
    Consensus.Data.Local_state.current_epoch_delegatee_table
      ~local_state:t.config.consensus_local_state
  in
  find_delegators current_epoch_delegatee_table pk

let last_epoch_delegators t ~pk =
  let open Option.Let_syntax in
  let%bind _transition_frontier =
    Broadcast_pipe.Reader.peek t.components.transition_frontier
  in
  let%map last_epoch_delegatee_table =
    Consensus.Data.Local_state.last_epoch_delegatee_table
      ~local_state:t.config.consensus_local_state
  in
  find_delegators last_epoch_delegatee_table pk

let perform_compaction t =
  match Mina_compile_config.compaction_interval_ms with
  | None ->
      ()
  | Some compaction_interval_compiled ->
      let slot_duration_ms =
        let leeway = 1000 in
        t.config.precomputed_values.constraint_constants
          .block_window_duration_ms + leeway
      in
      let expected_time_for_compaction =
        match Sys.getenv "MINA_COMPACTION_MS" with
        | Some ms ->
            Float.of_string ms
        | None ->
            6000.
      in
      let span ?(incr = 0.) ms =
        Float.(of_int ms +. incr) |> Time.Span.of_ms
      in
      let interval_configured =
        match Sys.getenv "MINA_COMPACTION_INTERVAL_MS" with
        | Some ms ->
            Time.Span.of_ms (Float.of_string ms)
        | None ->
            span compaction_interval_compiled
      in
      if Time.Span.(interval_configured <= of_ms expected_time_for_compaction)
      then (
        [%log' fatal t.config.logger]
          "Time between compactions %f should be greater than the expected \
           time for compaction %f"
          (Time.Span.to_ms interval_configured)
          expected_time_for_compaction ;
        failwith
          (sprintf
             "Time between compactions %f should be greater than the expected \
              time for compaction %f"
             (Time.Span.to_ms interval_configured)
             expected_time_for_compaction) ) ;
      let call_compact () =
        let start = Time.now () in
        Gc.compact () ;
        let span = Time.diff (Time.now ()) start in
        [%log' debug t.config.logger]
          ~metadata:[("time", `Float (Time.Span.to_ms span))]
          "Gc.compact took $time ms"
      in
      let rec perform interval =
        upon (after interval) (fun () ->
            match !(t.block_production_status) with
            | `Free ->
                call_compact () ;
                perform interval_configured
            | `Producing ->
                perform (span slot_duration_ms)
            | `Producing_in_ms ms ->
                if ms < expected_time_for_compaction then
                  (*too close to block production; perform compaction after block production*)
                  perform (span slot_duration_ms ~incr:ms)
                else (
                  call_compact () ;
                  perform interval_configured ) )
      in
      perform interval_configured

let start t =
  let set_next_producer_timing timing consensus_state =
    let block_production_status, next_producer_timing =
      let generated_from_consensus_at :
          Daemon_rpcs.Types.Status.Next_producer_timing.slot =
        { slot= Consensus.Data.Consensus_state.curr_global_slot consensus_state
        ; global_slot_since_genesis=
            Consensus.Data.Consensus_state.global_slot_since_genesis
              consensus_state }
      in
      let info time (data : Consensus.Data.Block_data.t) :
          Daemon_rpcs.Types.Status.Next_producer_timing.producing_time =
        let for_slot : Daemon_rpcs.Types.Status.Next_producer_timing.slot =
          { slot= Consensus.Data.Block_data.global_slot data
          ; global_slot_since_genesis=
              Consensus.Data.Block_data.global_slot_since_genesis data }
        in
        {time; for_slot}
      in
      let status, timing =
        match timing with
        | `Check_again time ->
            ( `Free
            , Daemon_rpcs.Types.Status.Next_producer_timing.Check_again
                ( time |> Block_time.Span.of_ms
                |> Block_time.of_span_since_epoch ) )
        | `Produce_now (block_data, _) ->
            let info :
                Daemon_rpcs.Types.Status.Next_producer_timing.producing_time =
              let time =
                Consensus.Data.Consensus_time.of_global_slot
                  ~constants:t.config.precomputed_values.consensus_constants
                  (Consensus.Data.Block_data.global_slot block_data)
                |> Consensus.Data.Consensus_time.to_time
                     ~constants:t.config.precomputed_values.consensus_constants
              in
              info time block_data
            in
            (`Producing, Produce_now info)
        | `Produce (time, block_data, _) ->
            ( `Producing_in_ms (Int64.to_float time)
            , Produce
                (info
                   ( time |> Block_time.Span.of_ms
                   |> Block_time.of_span_since_epoch )
                   block_data) )
      in
      ( status
      , { Daemon_rpcs.Types.Status.Next_producer_timing.timing
        ; generated_from_consensus_at } )
    in
    t.block_production_status := block_production_status ;
    t.next_producer_timing <- Some next_producer_timing
  in
  Block_producer.run ~logger:t.config.logger ~verifier:t.processes.verifier
    ~set_next_producer_timing ~prover:t.processes.prover
    ~trust_system:t.config.trust_system
    ~transaction_resource_pool:
      (Network_pool.Transaction_pool.resource_pool
         t.components.transaction_pool)
    ~get_completed_work:
      (Network_pool.Snark_pool.get_completed_work t.components.snark_pool)
    ~time_controller:t.config.time_controller
    ~keypairs:(Agent.read_only t.block_production_keypairs)
    ~coinbase_receiver:t.coinbase_receiver
    ~consensus_local_state:t.config.consensus_local_state
    ~frontier_reader:t.components.transition_frontier
    ~transition_writer:t.pipes.producer_transition_writer
    ~log_block_creation:t.config.log_block_creation
    ~precomputed_values:t.config.precomputed_values
    ~block_reward_threshold:t.config.block_reward_threshold ;
  perform_compaction t ;
  Snark_worker.start t

let start_with_precomputed_blocks t blocks =
  let%bind () =
    Block_producer.run_precomputed ~logger:t.config.logger
      ~verifier:t.processes.verifier ~trust_system:t.config.trust_system
      ~time_controller:t.config.time_controller
      ~frontier_reader:t.components.transition_frontier
      ~transition_writer:t.pipes.producer_transition_writer
      ~precomputed_values:t.config.precomputed_values
      ~precomputed_blocks:blocks
  in
  start t

let create ?wallets (config : Config.t) =
  let catchup_mode = if config.super_catchup then `Super else `Normal in
  let constraint_constants = config.precomputed_values.constraint_constants in
  let consensus_constants = config.precomputed_values.consensus_constants in
  let monitor = Option.value ~default:(Monitor.create ()) config.monitor in
  Async.Scheduler.within' ~monitor (fun () ->
      trace "coda" (fun () ->
          let%bind prover =
            Monitor.try_with ~here:[%here]
              ~rest:
                (`Call
                  (fun exn ->
                    let err = Error.of_exn ~backtrace:`Get exn in
                    [%log' warn config.logger]
                      "unhandled exception from daemon-side prover server: $exn"
                      ~metadata:[("exn", Error_json.error_to_yojson err)] ))
              (fun () ->
                trace "prover" (fun () ->
                    Prover.create ~logger:config.logger
                      ~proof_level:config.precomputed_values.proof_level
                      ~constraint_constants ~pids:config.pids
                      ~conf_dir:config.conf_dir ) )
            >>| Result.ok_exn
          in
          let%bind verifier =
            Monitor.try_with ~here:[%here]
              ~rest:
                (`Call
                  (fun exn ->
                    let err = Error.of_exn ~backtrace:`Get exn in
                    [%log' warn config.logger]
                      "unhandled exception from daemon-side verifier server: \
                       $exn"
                      ~metadata:[("exn", Error_json.error_to_yojson err)] ))
              (fun () ->
                trace "verifier" (fun () ->
                    Verifier.create ~logger:config.logger
                      ~proof_level:config.precomputed_values.proof_level
                      ~constraint_constants:
                        config.precomputed_values.constraint_constants
                      ~pids:config.pids ~conf_dir:(Some config.conf_dir) ) )
            >>| Result.ok_exn
          in
          let snark_worker =
            Option.value_map
              config.snark_worker_config.initial_snark_worker_key
              ~default:(`Off config.snark_work_fee) ~f:(fun public_key ->
                `On
                  ( { public_key
                    ; process= Ivar.create ()
                    ; kill_ivar= Ivar.create () }
                  , config.snark_work_fee ) )
          in
          log_snark_coordinator_warning config snark_worker ;
          Protocol_version.set_current config.initial_protocol_version ;
          Protocol_version.set_proposed_opt
            config.proposed_protocol_version_opt ;
          let external_transitions_reader, external_transitions_writer =
            let log_rate_limiter_occasionally rl =
              let t = Time.Span.of_min 1. in
              every t (fun () ->
                  [%log' debug config.logger]
                    ~metadata:
                      [("rate_limiter", Network_pool.Rate_limiter.summary rl)]
                    !"new_block $rate_limiter" )
            in
            let rl =
              Network_pool.Rate_limiter.create
                ~capacity:
                  ( (* Max of 20 transitions per slot per peer. *)
                    20
                  , `Per
                      (Block_time.Span.to_time_span
                         consensus_constants.slot_duration_ms) )
            in
            log_rate_limiter_occasionally rl ;
            let r, w = Strict_pipe.create Synchronous in
            ( Strict_pipe.Reader.filter_map r ~f:(fun ((e, _, cb) as x) ->
                  let sender = Envelope.Incoming.sender e in
                  match
                    Network_pool.Rate_limiter.add rl sender ~now:(Time.now ())
                      ~score:1
                  with
                  | `Capacity_exceeded ->
                      [%log' warn config.logger]
                        "$sender has sent many blocks. This is very unusual."
                        ~metadata:[("sender", Envelope.Sender.to_yojson sender)] ;
                      Mina_net2.Validation_callback.fire_if_not_already_fired
                        cb `Reject ;
                      None
                  | `Within_capacity ->
                      Some x )
            , w )
          in
          let producer_transition_reader, producer_transition_writer =
            Strict_pipe.create Synchronous
          in
          let frontier_broadcast_pipe_r, frontier_broadcast_pipe_w =
            Broadcast_pipe.create None
          in
          Exit_handlers.register_async_shutdown_handler ~logger:config.logger
            ~description:"Close transition frontier, if exists" (fun () ->
              match Broadcast_pipe.Reader.peek frontier_broadcast_pipe_r with
              | None ->
                  Deferred.unit
              | Some frontier ->
                  Transition_frontier.close ~loc:__LOC__ frontier ) ;
          let handle_request name ~f query_env =
            trace_recurring name (fun () ->
                let input = Envelope.Incoming.data query_env in
                Deferred.return
                @@
                let open Option.Let_syntax in
                let%bind frontier =
                  Broadcast_pipe.Reader.peek frontier_broadcast_pipe_r
                in
                f ~frontier input )
          in
          (* knot-tying hacks so we can pass a get_node_status function before net, Mina_lib.t created *)
          let net_ref = ref None in
          let sync_status_ref = ref None in
          let block_production_keypairs =
            Agent.create
              ~f:(fun kps ->
                Keypair.Set.to_list kps
                |> List.map ~f:(fun kp ->
                       (kp, Public_key.compress kp.Keypair.public_key) )
                |> Keypair.And_compressed_pk.Set.of_list )
              config.initial_block_production_keypairs
          in
          let get_node_status _env =
            let node_ip_addr =
              config.gossip_net_params.addrs_and_ports.external_ip
            in
            let peer_opt = config.gossip_net_params.addrs_and_ports.peer in
            let node_peer_id =
              Option.value_map peer_opt ~default:"<UNKNOWN>" ~f:(fun peer ->
                  peer.peer_id )
            in
            if config.disable_node_status then
              Deferred.return
              @@ Error
                   (Error.of_string
                      (sprintf
                         !"Node with IP address=%{sexp: Unix.Inet_addr.t}, \
                           peer ID=%s, node status is disabled"
                         node_ip_addr node_peer_id))
            else
              match !net_ref with
              | None ->
                  (* should be unreachable; without a network, we wouldn't receive this RPC call *)
                  [%log' info config.logger]
                    "Network not instantiated when node status requested" ;
                  Deferred.return
                  @@ Error
                       (Error.of_string
                          (sprintf
                             !"Node with IP address=%{sexp: \
                               Unix.Inet_addr.t}, peer ID=%s, network not \
                               instantiated when node status requested"
                             node_ip_addr node_peer_id))
              | Some net ->
                  let ( protocol_state_hash
                      , best_tip_opt
                      , k_block_hashes_and_timestamps ) =
                    match
                      Broadcast_pipe.Reader.peek frontier_broadcast_pipe_r
                    with
                    | None ->
                        ( config.precomputed_values.protocol_state_with_hash
                            .hash
                        , None
                        , [] )
                    | Some frontier ->
                        let tip = Transition_frontier.best_tip frontier in
                        let protocol_state_hash =
                          let state =
                            Transition_frontier.Breadcrumb.protocol_state tip
                          in
                          Mina_state.Protocol_state.hash state
                        in
                        let k_breadcrumbs =
                          Transition_frontier.root frontier
                          :: Transition_frontier.best_tip_path frontier
                        in
                        let k_block_hashes_and_timestamps =
                          List.map k_breadcrumbs ~f:(fun bc ->
                              ( Transition_frontier.Breadcrumb.state_hash bc
                              , Option.value_map
                                  (Transition_frontier.Breadcrumb
                                   .transition_receipt_time bc)
                                  ~default:"no timestamp available"
                                  ~f:
                                    (Time.to_string_iso8601_basic
                                       ~zone:Time.Zone.utc) ) )
                        in
                        ( protocol_state_hash
                        , Some tip
                        , k_block_hashes_and_timestamps )
                  in
                  let%bind peers = Mina_networking.peers net in
                  let open Deferred.Or_error.Let_syntax in
                  let%map sync_status =
                    match !sync_status_ref with
                    | None ->
                        Deferred.return (Ok `Offline)
                    | Some status ->
                        Deferred.return
                          (Mina_incremental.Status.Observer.value status)
                  in
                  let block_producers =
                    let public_keys, _ = Agent.get block_production_keypairs in
                    Public_key.Compressed.Set.map public_keys ~f:snd
                    |> Set.to_list
                  in
                  let ban_statuses =
                    Trust_system.Peer_trust.peer_statuses config.trust_system
                  in
                  let git_commit = Mina_version.commit_id_short in
                  let uptime_minutes =
                    let now = Time.now () in
                    let minutes_float =
                      Time.diff now config.start_time |> Time.Span.to_min
                    in
                    (* if rounding fails, just convert *)
                    Option.value_map
                      (Float.iround_nearest minutes_float)
                      ~f:Fn.id
                      ~default:(Float.to_int minutes_float)
                  in
                  let block_height_opt =
                    match best_tip_opt with
                    | None ->
                        None
                    | Some tip ->
                        let state =
                          Transition_frontier.Breadcrumb.protocol_state tip
                        in
                        let consensus_state =
                          state |> Mina_state.Protocol_state.consensus_state
                        in
                        Some
                          ( Mina_numbers.Length.to_int
                          @@ Consensus.Data.Consensus_state.blockchain_length
                               consensus_state )
                  in
                  Mina_networking.Rpcs.Get_node_status.Node_status.
                    { node_ip_addr
                    ; node_peer_id
                    ; sync_status
                    ; peers
                    ; block_producers
                    ; protocol_state_hash
                    ; ban_statuses
                    ; k_block_hashes_and_timestamps
                    ; git_commit
                    ; uptime_minutes
                    ; block_height_opt }
          in
          let get_some_initial_peers _ =
            match !net_ref with
            | None ->
                (* should be unreachable; without a network, we wouldn't receive this RPC call *)
                [%log' error config.logger]
                  "Network not instantiated when initial peers requested" ;
                Deferred.return []
            | Some net ->
                Mina_networking.peers net
          in
          let%bind net =
            Mina_networking.create config.net_config ~get_some_initial_peers
              ~get_staged_ledger_aux_and_pending_coinbases_at_hash:
                (fun query_env ->
                trace_recurring
                  "get_staged_ledger_aux_and_pending_coinbases_at_hash"
                  (fun () ->
                    let input = Envelope.Incoming.data query_env in
                    Deferred.return
                    @@
                    let open Option.Let_syntax in
                    let%bind frontier =
                      Broadcast_pipe.Reader.peek frontier_broadcast_pipe_r
                    in
                    let%map ( scan_state
                            , expected_merkle_root
                            , pending_coinbases
                            , protocol_states ) =
                      Sync_handler
                      .get_staged_ledger_aux_and_pending_coinbases_at_hash
                        ~frontier input
                    in
                    let staged_ledger_hash =
                      Staged_ledger_hash.of_aux_ledger_and_coinbase_hash
                        (Staged_ledger.Scan_state.hash scan_state)
                        expected_merkle_root pending_coinbases
                    in
                    [%log' debug config.logger]
                      ~metadata:
                        [ ( "staged_ledger_hash"
                          , Staged_ledger_hash.to_yojson staged_ledger_hash )
                        ]
                      "sending scan state and pending coinbase" ;
                    ( scan_state
                    , expected_merkle_root
                    , pending_coinbases
                    , protocol_states ) ) )
              ~answer_sync_ledger_query:(fun query_env ->
                let open Deferred.Or_error.Let_syntax in
                trace_recurring "answer_sync_ledger_query" (fun () ->
                    let ledger_hash, _ = Envelope.Incoming.data query_env in
                    let%bind frontier =
                      Deferred.return
                      @@ peek_frontier frontier_broadcast_pipe_r
                    in
                    Sync_handler.answer_query ~frontier ledger_hash
                      (Envelope.Incoming.map ~f:Tuple2.get2 query_env)
                      ~logger:config.logger ~trust_system:config.trust_system
                    |> Deferred.map
                       (* begin error string prefix so we can pattern-match *)
                         ~f:
                           (Result.of_option
                              ~error:
                                (Error.createf
                                   !"%s for ledger_hash: %{sexp:Ledger_hash.t}"
                                   Mina_networking.refused_answer_query_string
                                   ledger_hash)) ) )
              ~get_ancestry:
                (handle_request "get_ancestry"
                   ~f:
                     (Sync_handler.Root.prove ~consensus_constants
                        ~logger:config.logger))
              ~get_best_tip:
                (handle_request "get_best_tip" ~f:(fun ~frontier () ->
                     let open Option.Let_syntax in
                     let open Proof_carrying_data in
                     let%map proof_with_data =
                       Best_tip_prover.prove ~logger:config.logger frontier
                     in
                     { proof_with_data with
                       data= With_hash.data proof_with_data.data } ))
              ~get_node_status
              ~get_transition_chain_proof:
                (handle_request "get_transition_chain_proof"
                   ~f:(fun ~frontier hash ->
                     Transition_chain_prover.prove ~frontier hash ))
              ~get_transition_chain:
                (handle_request "get_transition_chain"
                   ~f:Sync_handler.get_transition_chain)
              ~get_transition_knowledge:(fun _q ->
                return
                  ( match
                      Broadcast_pipe.Reader.peek frontier_broadcast_pipe_r
                    with
                  | None ->
                      []
                  | Some frontier ->
                      Sync_handler.best_tip_path ~frontier ) )
          in
          (* tie the first knot *)
          net_ref := Some net ;
          let user_command_input_reader, user_command_input_writer =
            Strict_pipe.(create ~name:"local transactions" Synchronous)
          in
          let snapp_command_input_reader, snapp_command_input_writer =
            Strict_pipe.(create ~name:"local snapp transactions" Synchronous)
          in
          let local_txns_reader, local_txns_writer =
            Strict_pipe.(create ~name:"local transactions" Synchronous)
          in
          let local_snark_work_reader, local_snark_work_writer =
            Strict_pipe.(create ~name:"local snark work" Synchronous)
          in
          let txn_pool_config =
            Network_pool.Transaction_pool.Resource_pool.make_config ~verifier
              ~trust_system:config.trust_system
              ~pool_max_size:
                config.precomputed_values.genesis_constants.txpool_max_size
          in
          let transaction_pool =
            Network_pool.Transaction_pool.create ~config:txn_pool_config
              ~constraint_constants ~consensus_constants
              ~time_controller:config.time_controller ~logger:config.logger
              ~incoming_diffs:(Mina_networking.transaction_pool_diffs net)
              ~local_diffs:local_txns_reader
              ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
          in
          let command_to_network_pool input_pipe ~from_input ~to_transaction =
            (* Read from a command input pipe, convert it to a full
               transaction, and write to the pipe consumed by the network pool.
            *)
            Strict_pipe.Reader.iter input_pipe
              ~f:(fun (input_list, result_cb, get_current_nonce, get_account)
                 ->
                match%bind
                  from_input ~get_current_nonce ~get_account input_list
                with
                | Ok user_commands ->
                    if List.is_empty user_commands then (
                      result_cb
                        (Error (Error.of_string "No user commands to send")) ;
                      Deferred.unit )
                    else
                      Strict_pipe.Writer.write local_txns_writer
                        (List.map ~f:to_transaction user_commands, result_cb)
                | Error e ->
                    [%log' error config.logger]
                      "Failed to submit commands: $error"
                      ~metadata:[("error", `String (Error.to_string_hum e))] ;
                    result_cb (Error e) ;
                    Deferred.unit )
          in
          let%bind wallets =
            match wallets with
            | Some wallets ->
                return wallets
            | None ->
                Secrets.Wallets.load ~logger:config.logger
                  ~disk_location:config.wallets_disk_location
          in
          (* Redirect user command and snapp command inputs to the network
             pool.
          *)
          command_to_network_pool user_command_input_reader
            ~from_input:
              (User_command_input.to_user_commands ?nonce_map:None
                 ~constraint_constants ~logger:config.logger)
            ~to_transaction:(fun c -> User_command.Signed_command c)
          |> Deferred.don't_wait_for ;
          command_to_network_pool snapp_command_input_reader
            ~from_input:(fun ~get_current_nonce ~get_account:_ input_list ->
              Snapp_command_input.to_snapp_commands ~get_current_nonce
                ?nonce_map:None input_list
                ~find_identity:(Secrets.Wallets.find_identity wallets) )
            ~to_transaction:(fun c -> User_command.Snapp_command c)
          |> Deferred.don't_wait_for ;
          let ((most_recent_valid_block_reader, _) as most_recent_valid_block)
              =
            Broadcast_pipe.create
              ( External_transition.genesis
                  ~precomputed_values:config.precomputed_values
              |> External_transition.Validated.to_initial_validated )
          in
          let valid_transitions, initialization_finish_signal =
            trace "transition router" (fun () ->
                Transition_router.run ~logger:config.logger
                  ~trust_system:config.trust_system ~verifier ~network:net
                  ~is_seed:config.is_seed ~is_demo_mode:config.demo_mode
                  ~time_controller:config.time_controller
                  ~consensus_local_state:config.consensus_local_state
                  ~persistent_root_location:config.persistent_root_location
                  ~persistent_frontier_location:
                    config.persistent_frontier_location
                  ~frontier_broadcast_pipe:
                    (frontier_broadcast_pipe_r, frontier_broadcast_pipe_w)
                  ~catchup_mode
                  ~network_transition_reader:
                    (Strict_pipe.Reader.map external_transitions_reader
                       ~f:(fun (tn, tm, cb) ->
                         let lift_consensus_time =
                           Fn.compose UInt32.to_int
                             Consensus.Data.Consensus_time.to_uint32
                         in
                         let tn_production_consensus_time =
                           External_transition.consensus_time_produced_at
                             (Envelope.Incoming.data tn)
                         in
                         let tn_production_slot =
                           lift_consensus_time tn_production_consensus_time
                         in
                         let tn_production_time =
                           Consensus.Data.Consensus_time.to_time
                             ~constants:consensus_constants
                             tn_production_consensus_time
                         in
                         let tm_slot =
                           lift_consensus_time
                             (Consensus.Data.Consensus_time.of_time_exn
                                ~constants:consensus_constants tm)
                         in
                         Mina_metrics.Block_latency.Gossip_slots.update
                           (Float.of_int (tm_slot - tn_production_slot)) ;
                         Mina_metrics.Block_latency.Gossip_time.update
                           Block_time.(
                             Span.to_time_span @@ diff tm tn_production_time) ;
                         (`Transition tn, `Time_received tm, `Valid_cb cb) ))
                  ~producer_transition_reader:
                    (Strict_pipe.Reader.map producer_transition_reader
                       ~f:(fun breadcrumb ->
                         let et =
                           Transition_frontier.Breadcrumb.validated_transition
                             breadcrumb
                         in
                         let validation_callback =
                           Mina_net2.Validation_callback
                           .create_without_expiration ()
                         in
                         External_transition.Validated.poke_validation_callback
                           et validation_callback ;
                         don't_wait_for
                           (* this will never throw since the callback was created without expiration *)
                           (let%map v =
                              Mina_net2.Validation_callback.await_exn
                                validation_callback
                            in
                            if v = `Accept then
                              Mina_networking.broadcast_state net
                                (External_transition.Validation
                                 .forget_validation_with_hash et)) ;
                         breadcrumb ))
                  ~most_recent_valid_block
                  ~precomputed_values:config.precomputed_values )
          in
          let ( valid_transitions_for_network
              , valid_transitions_for_api
              , new_blocks ) =
            let network_pipe, downstream_pipe =
              Strict_pipe.Reader.Fork.two valid_transitions
            in
            let api_pipe, new_blocks_pipe =
              Strict_pipe.Reader.(
                Fork.two (map downstream_pipe ~f:(fun (`Transition t, _) -> t)))
            in
            (network_pipe, api_pipe, new_blocks_pipe)
          in
          trace_task "transaction pool broadcast loop" (fun () ->
              Linear_pipe.iter
                (Network_pool.Transaction_pool.broadcasts transaction_pool)
                ~f:(fun x ->
                  Mina_networking.broadcast_transaction_pool_diff net x ;
                  Deferred.unit ) ) ;
          trace_task "valid_transitions_for_network broadcast loop" (fun () ->
              Strict_pipe.Reader.iter_without_pushback
                valid_transitions_for_network
                ~f:(fun (`Transition transition, `Source source) ->
                  let hash =
                    External_transition.Validated.state_hash transition
                  in
                  let consensus_state =
                    transition |> External_transition.Validated.consensus_state
                  in
                  let now =
                    let open Block_time in
                    now config.time_controller |> to_span_since_epoch
                    |> Span.to_ms
                  in
                  match
                    Consensus.Hooks.received_at_valid_time
                      ~constants:consensus_constants ~time_received:now
                      consensus_state
                  with
                  | Ok () ->
                      (*Don't log rebroadcast message if it is internally generated; There is a broadcast log for it*)
                      if not (source = `Internal) then
                        [%str_log' info config.logger]
                          ~metadata:
                            [ ( "external_transition"
                              , External_transition.Validated.to_yojson
                                  transition ) ]
                          (Rebroadcast_transition {state_hash= hash}) ;
                      External_transition.Validated.broadcast transition
                  | Error reason -> (
                      let timing_error_json =
                        match reason with
                        | `Too_early ->
                            `String "too early"
                        | `Too_late slots ->
                            `String (sprintf "%Lu slots too late" slots)
                      in
                      let metadata =
                        [ ("state_hash", State_hash.to_yojson hash)
                        ; ( "external_transition"
                          , External_transition.Validated.to_yojson transition
                          )
                        ; ("timing", timing_error_json) ]
                      in
                      External_transition.Validated.don't_broadcast transition ;
                      match source with
                      | `Catchup ->
                          ()
                      | `Internal ->
                          [%log' error config.logger] ~metadata
                            "Internally generated block $state_hash cannot be \
                             rebroadcast because it's not a valid time to do \
                             so ($timing)"
                      | `Gossip ->
                          [%log' warn config.logger] ~metadata
                            "Not rebroadcasting block $state_hash because it \
                             was received $timing" ) ) ) ;
          don't_wait_for
            (Strict_pipe.transfer
               (Mina_networking.states net)
               external_transitions_writer ~f:ident) ;
          (* FIXME #4093: augment ban_notifications with a Peer.ID so we can implement ban_notify
             trace_task "ban notification loop" (fun () ->
              Linear_pipe.iter (Mina_networking.ban_notification_reader net)
                ~f:(fun notification ->
                  let open Gossip_net in
                  let peer = notification.banned_peer in
                  let banned_until = notification.banned_until in
                  (* if RPC call fails, will be logged in gossip net code *)
                  let%map _ =
                    Mina_networking.ban_notify net peer banned_until
                  in
                  () ) ) ; *)
          don't_wait_for
            (Linear_pipe.iter
               (Mina_networking.ban_notification_reader net)
               ~f:(Fn.const Deferred.unit)) ;
          let snark_pool_config =
            Network_pool.Snark_pool.Resource_pool.make_config ~verifier
              ~trust_system:config.trust_system
              ~disk_location:config.snark_pool_disk_location
          in
          let%bind snark_pool =
            Network_pool.Snark_pool.load ~config:snark_pool_config
              ~constraint_constants ~consensus_constants
              ~time_controller:config.time_controller ~logger:config.logger
              ~incoming_diffs:(Mina_networking.snark_pool_diffs net)
              ~local_diffs:local_snark_work_reader
              ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
          in
          let snark_jobs_state =
            Work_selector.State.init
              ~reassignment_wait:config.work_reassignment_wait
              ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
              ~logger:config.logger
          in
          trace_task "snark pool broadcast loop" (fun () ->
              Linear_pipe.iter (Network_pool.Snark_pool.broadcasts snark_pool)
                ~f:(fun x ->
                  Mina_networking.broadcast_snark_pool_diff net x ;
                  Deferred.unit ) ) ;
          Option.iter config.archive_process_location
            ~f:(fun archive_process_port ->
              [%log' info config.logger]
                "Communicating with the archive process"
                ~metadata:
                  [ ( "Host"
                    , `String (Host_and_port.host archive_process_port.value)
                    )
                  ; ( "Port"
                    , `Int (Host_and_port.port archive_process_port.value) ) ] ;
              Archive_client.run ~logger:config.logger
                ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
                archive_process_port ) ;
          let precomputed_block_writer =
            ref
              ( Option.map config.precomputed_blocks_path ~f:(fun path ->
                    `Path path )
              , if config.log_precomputed_blocks then Some `Log else None )
          in
          let subscriptions =
            Coda_subscriptions.create ~logger:config.logger
              ~constraint_constants ~new_blocks ~wallets
              ~transition_frontier:frontier_broadcast_pipe_r
              ~is_storing_all:config.is_archive_rocksdb
              ~upload_blocks_to_gcloud:config.upload_blocks_to_gcloud
              ~time_controller:config.time_controller ~precomputed_block_writer
          in
          let open Mina_incremental.Status in
          let transition_frontier_incr =
            Var.watch @@ of_broadcast_pipe frontier_broadcast_pipe_r
          in
          let transition_frontier_and_catchup_signal_incr =
            transition_frontier_incr
            >>= function
            | Some transition_frontier ->
                of_broadcast_pipe Ledger_catchup.Catchup_jobs.reader
                |> Var.watch
                >>| fun catchup_signal ->
                Some (transition_frontier, catchup_signal)
            | None ->
                return None
          in
          let sync_status =
            create_sync_status_observer ~logger:config.logger ~net
              ~is_seed:config.is_seed ~demo_mode:config.demo_mode
              ~transition_frontier_and_catchup_signal_incr
              ~online_status_incr:
                ( Var.watch @@ of_broadcast_pipe
                @@ Mina_networking.online_status net )
              ~first_connection_incr:
                ( Var.watch @@ of_deferred
                @@ Mina_networking.on_first_connect net ~f:Fn.id )
              ~first_message_incr:
                ( Var.watch @@ of_deferred
                @@ Mina_networking.on_first_received_message net ~f:Fn.id )
          in
          (* tie other knot *)
          sync_status_ref := Some sync_status ;
          Deferred.return
            { config
            ; next_producer_timing= None
            ; processes= {prover; verifier; snark_worker}
            ; initialization_finish_signal
            ; components=
                { net
                ; transaction_pool
                ; snark_pool
                ; transition_frontier= frontier_broadcast_pipe_r
                ; most_recent_valid_block= most_recent_valid_block_reader }
            ; pipes=
                { validated_transitions_reader= valid_transitions_for_api
                ; producer_transition_writer
                ; external_transitions_writer=
                    Strict_pipe.Writer.to_linear_pipe
                      external_transitions_writer
                ; user_command_input_writer
                ; snapp_command_input_writer
                ; user_command_writer= local_txns_writer
                ; local_snark_work_writer }
            ; wallets
            ; block_production_keypairs
            ; coinbase_receiver= ref config.coinbase_receiver
            ; snark_job_state= snark_jobs_state
            ; subscriptions
            ; sync_status
            ; precomputed_block_writer
            ; block_production_status= ref `Free } ) )

let net {components= {net; _}; _} = net
