open Core
open Async
open Mina_base
open Mina_transition
open Signature_lib
open Pipe_lib
open Init

module Input = struct
  type t =
    { addrs_and_ports: Node_addrs_and_ports.Display.Stable.Latest.t
    ; libp2p_keypair: Mina_net2.Keypair.Stable.Latest.t
    ; net_configs:
        ( Node_addrs_and_ports.Display.Stable.Latest.t
        * Mina_net2.Keypair.Stable.Latest.t )
        list
        * Node_addrs_and_ports.Display.Stable.Latest.t list list
    ; snark_worker_key: Public_key.Compressed.Stable.Latest.t option
    ; env: (string * string) list
    ; block_production_key: int option
    ; work_selection_method:
        Cli_lib.Arg_type.Work_selection_method.Stable.Latest.t
    ; conf_dir: string
    ; trace_dir: string option
    ; program_dir: string
    ; acceptable_delay: Time.Span.t
    ; chain_id: string
    ; peers: string list
    ; max_concurrent_connections: int option
    ; is_archive_rocksdb: bool
    ; is_seed: bool
    ; archive_process_location: Core.Host_and_port.t option
    ; runtime_config: Runtime_config.t }
  [@@deriving bin_io_unversioned]
end

open Input

module Send_payment_input = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        Private_key.Stable.V1.t
        * Public_key.Compressed.Stable.V1.t
        * Currency.Amount.Stable.V1.t
        * Currency.Fee.Stable.V1.t
        * Signed_command_memo.Stable.V1.t

      let to_latest = Fn.id
    end
  end]
end

module T = struct
  type state_hashes = bool list * bool list

  type 'worker functions =
    { peers: ('worker, unit, Network_peer.Peer.t list) Rpc_parallel.Function.t
    ; start: ('worker, unit, unit) Rpc_parallel.Function.t
    ; get_balance:
        ( 'worker
        , Account_id.t
        , Currency.Balance.t option )
        Rpc_parallel.Function.t
    ; get_nonce:
        ( 'worker
        , Account_id.t
        , Mina_numbers.Account_nonce.t option )
        Rpc_parallel.Function.t
    ; root_length: ('worker, unit, int) Rpc_parallel.Function.t
    ; send_user_command:
        ( 'worker
        , Send_payment_input.t
        , Signed_command.t Or_error.t )
        Rpc_parallel.Function.t
    ; process_user_command:
        ( 'worker
        , User_command_input.t
        , Signed_command.t Or_error.t )
        Rpc_parallel.Function.t
    ; verified_transitions:
        ('worker, unit, state_hashes Pipe.Reader.t) Rpc_parallel.Function.t
    ; sync_status:
        ('worker, unit, Sync_status.t Pipe.Reader.t) Rpc_parallel.Function.t
    ; new_user_command:
        ( 'worker
        , Public_key.Compressed.t
        , Signed_command.t Pipe.Reader.t )
        Rpc_parallel.Function.t
    ; root_diff:
        ( 'worker
        , unit
        , Mina_lib.Root_diff.t Pipe.Reader.t )
        Rpc_parallel.Function.t
    ; initialization_finish_signal:
        ('worker, unit, unit Pipe.Reader.t) Rpc_parallel.Function.t
    ; new_block:
        ( 'worker
        , Account.key
        , (Filtered_external_transition.t, State_hash.t) With_hash.t
          Pipe.Reader.t )
        Rpc_parallel.Function.t
    ; dump_tf: ('worker, unit, string) Rpc_parallel.Function.t
    ; best_path:
        ( 'worker
        , unit
        , State_hash.Stable.Latest.t list )
        Rpc_parallel.Function.t
    ; replace_snark_worker_key:
        ('worker, Public_key.Compressed.t option, unit) Rpc_parallel.Function.t
    ; stop_snark_worker: ('worker, unit, unit) Rpc_parallel.Function.t
    ; validated_transitions_keyswaptest:
        ( 'worker
        , unit
        , External_transition.Validated.t Pipe.Reader.t )
        Rpc_parallel.Function.t }

  type coda_functions =
    { coda_peers: unit -> Network_peer.Peer.t list Deferred.t
    ; coda_start: unit -> unit Deferred.t
    ; coda_get_balance: Account_id.t -> Currency.Balance.t option Deferred.t
    ; coda_get_nonce:
        Account_id.t -> Mina_numbers.Account_nonce.t option Deferred.t
    ; coda_root_length: unit -> int Deferred.t
    ; coda_send_payment:
        Send_payment_input.t -> Signed_command.t Or_error.t Deferred.t
    ; coda_process_user_command:
        User_command_input.t -> Signed_command.t Or_error.t Deferred.t
    ; coda_verified_transitions: unit -> state_hashes Pipe.Reader.t Deferred.t
    ; coda_sync_status: unit -> Sync_status.t Pipe.Reader.t Deferred.t
    ; coda_new_user_command:
        Public_key.Compressed.t -> Signed_command.t Pipe.Reader.t Deferred.t
    ; coda_replace_snark_worker_key:
        Public_key.Compressed.t option -> unit Deferred.t
    ; coda_stop_snark_worker: unit -> unit Deferred.t
    ; coda_validated_transitions_keyswaptest:
        unit -> External_transition.Validated.t Pipe.Reader.t Deferred.t
    ; coda_root_diff: unit -> Mina_lib.Root_diff.t Pipe.Reader.t Deferred.t
    ; coda_initialization_finish_signal: unit -> unit Pipe.Reader.t Deferred.t
    ; coda_new_block:
           Account.key
        -> (Filtered_external_transition.t, State_hash.t) With_hash.t
           Pipe.Reader.t
           Deferred.t
    ; coda_dump_tf: unit -> string Deferred.t
    ; coda_best_path: unit -> State_hash.t list Deferred.t }

  module Worker_state = struct
    type init_arg = Input.t [@@deriving bin_io_unversioned]

    type t = coda_functions
  end

  module Connection_state = struct
    type init_arg = unit [@@deriving bin_io_unversioned]

    type t = unit
  end

  module Functions
      (C : Rpc_parallel.Creator
           with type worker_state := Worker_state.t
            and type connection_state := Connection_state.t) =
  struct
    let peers_impl ~worker_state ~conn_state:() () = worker_state.coda_peers ()

    let verified_transitions_impl ~worker_state ~conn_state:() () =
      worker_state.coda_verified_transitions ()

    let sync_status_impl ~worker_state ~conn_state:() () =
      worker_state.coda_sync_status ()

    let new_user_command_impl ~worker_state ~conn_state:() pk =
      worker_state.coda_new_user_command pk

    let root_diff_impl ~worker_state ~conn_state:() () =
      worker_state.coda_root_diff ()

    let initialization_finish_signal_impl ~worker_state ~conn_state:() () =
      worker_state.coda_initialization_finish_signal ()

    let get_balance_impl ~worker_state ~conn_state:() pk =
      worker_state.coda_get_balance pk

    let root_length_impl ~worker_state ~conn_state:() () =
      worker_state.coda_root_length ()

    let get_nonce_impl ~worker_state ~conn_state:() pk =
      worker_state.coda_get_nonce pk

    let send_payment_impl ~worker_state ~conn_state:() input =
      worker_state.coda_send_payment input

    let process_user_command_impl ~worker_state ~conn_state:() cmd =
      worker_state.coda_process_user_command cmd

    let new_block_impl ~worker_state ~conn_state:() key =
      worker_state.coda_new_block key

    let start_impl ~worker_state ~conn_state:() () = worker_state.coda_start ()

    let dump_tf_impl ~worker_state ~conn_state:() () =
      worker_state.coda_dump_tf ()

    let best_path_impl ~worker_state ~conn_state:() () =
      worker_state.coda_best_path ()

    let stop_snark_worker_impl ~worker_state ~conn_state:() () =
      worker_state.coda_stop_snark_worker ()

    let replace_snark_worker_key_impl ~worker_state ~conn_state:() key =
      worker_state.coda_replace_snark_worker_key key

    let validated_transitions_keyswaptest_impl ~worker_state ~conn_state:() =
      worker_state.coda_validated_transitions_keyswaptest

    let peers =
      C.create_rpc ~f:peers_impl ~name:"peers" ~bin_input:Unit.bin_t
        ~bin_output:[%bin_type_class: Network_peer.Peer.Stable.Latest.t list]
        ()

    let start =
      C.create_rpc ~name:"start" ~f:start_impl ~bin_input:Unit.bin_t
        ~bin_output:Unit.bin_t ()

    let get_balance =
      C.create_rpc ~f:get_balance_impl ~name:"get_balance"
        ~bin_input:Account_id.Stable.Latest.bin_t
        ~bin_output:[%bin_type_class: Currency.Balance.Stable.Latest.t option]
        ()

    let get_nonce =
      C.create_rpc ~f:get_nonce_impl ~name:"get_nonce"
        ~bin_input:Account_id.Stable.Latest.bin_t
        ~bin_output:
          [%bin_type_class: Mina_numbers.Account_nonce.Stable.Latest.t option]
        ()

    let root_length =
      C.create_rpc ~name:"root_length" ~f:root_length_impl
        ~bin_input:Unit.bin_t ~bin_output:Int.bin_t ()

    let new_block =
      C.create_pipe ~f:new_block_impl ~name:"new_block"
        ~bin_input:[%bin_type_class: Account.Key.Stable.Latest.t]
        ~bin_output:
          [%bin_type_class:
            ( Filtered_external_transition.Stable.Latest.t
            , State_hash.Stable.Latest.t )
            With_hash.Stable.Latest.t] ()

    let send_user_command =
      C.create_rpc ~name:"send_user_command" ~f:send_payment_impl
        ~bin_input:Send_payment_input.Stable.Latest.bin_t
        ~bin_output:
          [%bin_type_class: Signed_command.Stable.Latest.t Or_error.t] ()

    let process_user_command =
      C.create_rpc ~name:"process_user_command" ~f:process_user_command_impl
        ~bin_input:User_command_input.Stable.Latest.bin_t
        ~bin_output:
          [%bin_type_class: Signed_command.Stable.Latest.t Or_error.t] ()

    let verified_transitions =
      C.create_pipe ~name:"verified_transitions" ~f:verified_transitions_impl
        ~bin_input:Unit.bin_t
        ~bin_output:[%bin_type_class: bool list * bool list] ()

    let root_diff =
      C.create_pipe ~name:"root_diff" ~f:root_diff_impl ~bin_input:Unit.bin_t
        ~bin_output:[%bin_type_class: Mina_lib.Root_diff.Stable.Latest.t] ()

    let initialization_finish_signal =
      C.create_pipe ~name:"initialization_finish_signal"
        ~f:initialization_finish_signal_impl ~bin_input:Unit.bin_t
        ~bin_output:Unit.bin_t ()

    let sync_status =
      C.create_pipe ~name:"sync_status" ~f:sync_status_impl
        ~bin_input:Unit.bin_t ~bin_output:Sync_status.Stable.Latest.bin_t ()

    let new_user_command =
      C.create_pipe ~name:"new_user_command" ~f:new_user_command_impl
        ~bin_input:Public_key.Compressed.Stable.Latest.bin_t
        ~bin_output:Signed_command.Stable.Latest.bin_t ()

    let dump_tf =
      C.create_rpc ~name:"dump_tf" ~f:dump_tf_impl ~bin_input:Unit.bin_t
        ~bin_output:String.bin_t ()

    let best_path =
      C.create_rpc ~name:"best_path" ~f:best_path_impl ~bin_input:Unit.bin_t
        ~bin_output:[%bin_type_class: State_hash.Stable.Latest.t list] ()

    let validated_transitions_keyswaptest =
      C.create_pipe ~name:"validated_transitions_keyswaptest"
        ~f:validated_transitions_keyswaptest_impl ~bin_input:Unit.bin_t
        ~bin_output:
          [%bin_type_class: External_transition.Validated.Stable.Latest.t] ()

    let replace_snark_worker_key =
      C.create_rpc ~name:"replace_snark_worker_key"
        ~f:replace_snark_worker_key_impl
        ~bin_input:
          [%bin_type_class: Public_key.Compressed.Stable.Latest.t option]
        ~bin_output:Unit.bin_t ()

    let stop_snark_worker =
      C.create_rpc ~name:"stop_snark_worker" ~f:stop_snark_worker_impl
        ~bin_input:Unit.bin_t ~bin_output:Unit.bin_t ()

    let functions =
      { peers
      ; start
      ; verified_transitions
      ; root_diff
      ; initialization_finish_signal
      ; get_balance
      ; get_nonce
      ; root_length
      ; send_user_command
      ; process_user_command
      ; new_block
      ; dump_tf
      ; best_path
      ; sync_status
      ; new_user_command
      ; replace_snark_worker_key
      ; stop_snark_worker
      ; validated_transitions_keyswaptest }

    let init_worker_state
        { addrs_and_ports
        ; libp2p_keypair
        ; block_production_key
        ; snark_worker_key
        ; work_selection_method
        ; conf_dir
        ; trace_dir
        ; chain_id
        ; peers
        ; max_concurrent_connections= _ (* FIXME #4095: use this *)
        ; is_archive_rocksdb
        ; is_seed
        ; archive_process_location
        ; runtime_config
        ; _ } =
      let logger =
        Logger.create
          ~metadata:
            [ ("host", `String addrs_and_ports.external_ip)
            ; ("port", `Int addrs_and_ports.libp2p_port) ]
          ()
      in
      let%bind precomputed_values, _runtime_config =
        Genesis_ledger_helper.init_from_config_file ~logger ~proof_level:None
          runtime_config
        >>| Or_error.ok_exn
      in
      let constraint_constants = precomputed_values.constraint_constants in
      let (module Genesis_ledger) = precomputed_values.genesis_ledger in
      let pids = Child_processes.Termination.create_pid_table () in
      let%bind () =
        Option.value_map trace_dir
          ~f:(fun d ->
            let%bind () = Async.Unix.mkdir ~p:() d in
            Coda_tracing.start d )
          ~default:Deferred.unit
      in
      let%bind () = File_system.create_dir conf_dir in
      O1trace.trace "worker_main" (fun () ->
          let%bind trust_dir = Unix.mkdtemp (conf_dir ^/ "trust") in
          let trace_database_initialization typ location =
            (* can't use %log because location is passed-in *)
            Logger.trace logger "Creating %s at %s" ~module_:__MODULE__
              ~location typ
          in
          let trust_system = Trust_system.create trust_dir in
          trace_database_initialization "trust_system" __LOC__ trust_dir ;
          let time_controller =
            Block_time.Controller.create (Block_time.Controller.basic ~logger)
          in
          let block_production_keypair =
            Option.map block_production_key ~f:(fun i ->
                List.nth_exn (Lazy.force Genesis_ledger.accounts) i
                |> Genesis_ledger.keypair_of_account_record_exn )
          in
          let initial_block_production_keypairs =
            Keypair.Set.of_list (block_production_keypair |> Option.to_list)
          in
          let initial_block_production_keys =
            Public_key.Compressed.Set.of_list
              ( Option.map block_production_keypair ~f:(fun keypair ->
                    let open Keypair in
                    Public_key.compress keypair.public_key )
              |> Option.to_list )
          in
          let epoch_ledger_location = conf_dir ^/ "epoch_ledger" in
          let consensus_local_state =
            Consensus.Data.Local_state.create initial_block_production_keys
              ~genesis_ledger:Genesis_ledger.t
              ~genesis_epoch_data:precomputed_values.genesis_epoch_data
              ~epoch_ledger_location
              ~ledger_depth:constraint_constants.ledger_depth
              ~genesis_state_hash:
                (With_hash.hash precomputed_values.protocol_state_with_hash)
          in
          let gossip_net_params =
            Gossip_net.Libp2p.Config.
              { timeout= Time.Span.of_sec 3.
              ; initial_peers= List.map ~f:Mina_net2.Multiaddr.of_string peers
              ; addrs_and_ports=
                  Node_addrs_and_ports.of_display addrs_and_ports
              ; metrics_port= None
              ; conf_dir
              ; chain_id
              ; logger
              ; seed_peer_list_url= None
              ; unsafe_no_trust_ip= true
              ; isolate= false
              ; trust_system
              ; flooding= false
              ; direct_peers= []
              ; max_connections= 50
              ; validation_queue_size= 150
              ; peer_exchange= true
              ; mina_peer_exchange= true
              ; keypair= Some libp2p_keypair
              ; all_peers_seen_metric= false }
          in
          let net_config =
            { Mina_networking.Config.logger
            ; trust_system
            ; time_controller
            ; consensus_local_state
            ; is_seed= List.is_empty peers
            ; genesis_ledger_hash=
                Ledger.merkle_root (Lazy.force Genesis_ledger.t)
            ; constraint_constants
            ; log_gossip_heard=
                { snark_pool_diff= true
                ; transaction_pool_diff= true
                ; new_state= true }
            ; creatable_gossip_net=
                Mina_networking.Gossip_net.(
                  Any.Creatable
                    ((module Libp2p), Libp2p.create gossip_net_params ~pids))
            }
          in
          let monitor = Async.Monitor.create ~name:"coda" () in
          let with_monitor f input =
            Async.Scheduler.within' ~monitor (fun () -> f input)
          in
          let start_time = Time.now () in
          let coda_deferred () =
            Mina_lib.create
              (Mina_lib.Config.make ~logger ~pids ~trust_system ~conf_dir
                 ~chain_id ~is_seed ~disable_node_status:true
                 ~super_catchup:true ~coinbase_receiver:`Producer ~net_config
                 ~gossip_net_params
                 ~initial_protocol_version:Protocol_version.zero
                 ~proposed_protocol_version_opt:None
                 ~work_selection_method:
                   (Cli_lib.Arg_type.work_selection_method_to_module
                      work_selection_method)
                 ~snark_worker_config:
                   Mina_lib.Config.Snark_worker_config.
                     { initial_snark_worker_key= snark_worker_key
                     ; shutdown_on_disconnect= true
                     ; num_threads= None }
                 ~boost_snark_worker_fee:false
                 ~snark_pool_disk_location:(conf_dir ^/ "snark_pool")
                 ~persistent_root_location:(conf_dir ^/ "root")
                 ~persistent_frontier_location:(conf_dir ^/ "frontier")
                 ~epoch_ledger_location
                 ~wallets_disk_location:(conf_dir ^/ "wallets")
                 ~time_controller ~snark_work_fee:(Currency.Fee.of_int 0)
                 ~initial_block_production_keypairs ~monitor
                 ~consensus_local_state ~is_archive_rocksdb
                 ~work_reassignment_wait:420000 ~precomputed_values ~start_time
                 ~upload_blocks_to_gcloud:false
                 ~archive_process_location:
                   (Option.map archive_process_location
                      ~f:(fun host_and_port ->
                        Cli_lib.Flag.Types.
                          {name= "dummy"; value= host_and_port} ))
                 ~log_precomputed_blocks:false ())
          in
          let coda_ref : Mina_lib.t option ref = ref None in
          Coda_run.handle_shutdown ~monitor ~time_controller ~conf_dir
            ~child_pids:pids ~top_logger:logger coda_ref ;
          let%map coda =
            with_monitor
              (fun () ->
                let%map coda = coda_deferred () in
                coda_ref := Some coda ;
                [%log info] "Setting up snark worker " ;
                Coda_run.setup_local_server coda ;
                coda )
              ()
          in
          [%log info] "Worker finish setting up coda" ;
          let coda_peers () = Mina_lib.peers coda in
          let coda_start () = Mina_lib.start coda in
          let coda_get_balance account_id =
            return
              ( Mina_commands.get_balance coda account_id
              |> Participating_state.active_exn )
          in
          let coda_get_nonce account_id =
            return
              ( Mina_commands.get_nonce coda account_id
              |> Participating_state.active_exn )
          in
          let coda_root_length () =
            return (Mina_lib.root_length coda |> Participating_state.active_exn)
          in
          let coda_send_payment (sk, pk, amount, fee, memo) =
            let pk_of_sk sk =
              Public_key.of_private_key_exn sk |> Public_key.compress
            in
            let build_user_command_input amount sender_sk receiver_pk fee =
              let sender_pk = pk_of_sk sender_sk in
              User_command_input.create ~fee ~fee_token:Token_id.default
                ~fee_payer_pk:sender_pk ~signer:sender_pk ~memo
                ~valid_until:None
                ~body:
                  (Payment
                     { source_pk= sender_pk
                     ; receiver_pk
                     ; token_id= Token_id.default
                     ; amount })
                ~sign_choice:
                  (User_command_input.Sign_choice.Keypair
                     (Keypair.of_private_key_exn sender_sk))
                ()
            in
            let payment_input = build_user_command_input amount sk pk fee in
            Deferred.map
              ( Mina_commands.setup_and_submit_user_command coda payment_input
              |> Participating_state.to_deferred_or_error )
              ~f:Or_error.join
          in
          let coda_process_user_command cmd_input =
            Deferred.map
              ( Mina_commands.setup_and_submit_user_command coda cmd_input
              |> Participating_state.to_deferred_or_error )
              ~f:Or_error.join
          in
          let coda_replace_snark_worker_key =
            Mina_lib.replace_snark_worker_key coda
          in
          let coda_stop_snark_worker () =
            Mina_lib.stop_snark_worker ~should_wait_kill:true coda
          in
          let coda_new_block key =
            Deferred.return
            @@ Mina_commands.Subscriptions.new_block coda (Some key)
          in
          (* TODO: #2836 Remove validated_transitions_keyswaptest once the refactoring of broadcast pipe enters the code base *)
          let ( validated_transitions_keyswaptest_reader
              , validated_transitions_keyswaptest_writer ) =
            Pipe.create ()
          in
          let coda_verified_transitions () =
            let r, w = Linear_pipe.create () in
            don't_wait_for
              (Strict_pipe.Reader.iter (Mina_lib.validated_transitions coda)
                 ~f:(fun t ->
                   Pipe.write_without_pushback_if_open
                     validated_transitions_keyswaptest_writer t ;
                   let prev_state_hash =
                     External_transition.Validated.parent_hash t
                   in
                   let state_hash =
                     External_transition.Validated.state_hash t
                   in
                   let prev_state_hash = State_hash.to_bits prev_state_hash in
                   let state_hash = State_hash.to_bits state_hash in
                   if Pipe.is_closed w then
                     [%log error]
                       "why is this w pipe closed? did someone close the \
                        reader end? dropping this write..." ;
                   Linear_pipe.write_without_pushback_if_open w
                     (prev_state_hash, state_hash) ;
                   Deferred.unit )) ;
            return r.pipe
          in
          let coda_validated_transitions_keyswaptest () =
            Deferred.return validated_transitions_keyswaptest_reader
          in
          let coda_root_diff () =
            let r, w = Linear_pipe.create () in
            don't_wait_for
              (Strict_pipe.Reader.iter (Mina_lib.root_diff coda)
                 ~f:(fun diff ->
                   if Pipe.is_closed w then
                     [%log error]
                       "[coda_root_diff] why is this w pipe closed? did \
                        someone close the reader end? dropping this write..." ;
                   Linear_pipe.write_if_open w diff )) ;
            return r.pipe
          in
          let coda_initialization_finish_signal () =
            let r, w = Linear_pipe.create () in
            upon
              (Ivar.read @@ Mina_lib.initialization_finish_signal coda)
              (fun () -> don't_wait_for @@ Linear_pipe.write_if_open w ()) ;
            return r.pipe
          in
          let coda_dump_tf () =
            Deferred.return
              ( Mina_lib.dump_tf coda |> Or_error.ok
              |> Option.value ~default:"<failed to visualize>" )
          in
          let coda_best_path () =
            let path = Mina_lib.best_path coda in
            Deferred.return (Option.value ~default:[] path)
          in
          let parse_sync_status_exn = function
            | `Assoc [("data", `Assoc [("newSyncUpdate", `String status)])] ->
                Sync_status.of_string status |> Or_error.ok_exn
            | unexpected_json ->
                failwithf
                  !"could not parse sync status from json. Got: %s"
                  (Yojson.Basic.to_string unexpected_json)
                  ()
          in
          let coda_sync_status () =
            let schema = Mina_graphql.schema in
            match Graphql_parser.parse "subscription { newSyncUpdate }" with
            | Ok query -> (
                match%map Graphql_async.Schema.execute schema coda query with
                | Ok (`Stream pipe) ->
                    Async.Pipe.map pipe ~f:(function
                      | Ok json ->
                          parse_sync_status_exn json
                      | Error json ->
                          failwith
                            (sprintf "Receiving sync status error: %s"
                               (Yojson.Basic.to_string json)) )
                | _ ->
                    failwith "Expected to get a stream of sync updates" )
            | Error e ->
                failwithf
                  !"unable to retrieve sync update subscription: %s"
                  e ()
          in
          let coda_new_user_command =
            Fn.compose Deferred.return
            @@ Mina_commands.For_tests.Subscriptions.new_user_commands coda
          in
          { coda_peers= with_monitor coda_peers
          ; coda_verified_transitions= with_monitor coda_verified_transitions
          ; coda_root_diff= with_monitor coda_root_diff
          ; coda_initialization_finish_signal=
              with_monitor coda_initialization_finish_signal
          ; coda_get_balance= with_monitor coda_get_balance
          ; coda_get_nonce= with_monitor coda_get_nonce
          ; coda_root_length= with_monitor coda_root_length
          ; coda_send_payment= with_monitor coda_send_payment
          ; coda_process_user_command= with_monitor coda_process_user_command
          ; coda_new_block= with_monitor coda_new_block
          ; coda_start= with_monitor coda_start
          ; coda_dump_tf= with_monitor coda_dump_tf
          ; coda_best_path= with_monitor coda_best_path
          ; coda_sync_status= with_monitor coda_sync_status
          ; coda_new_user_command= with_monitor coda_new_user_command
          ; coda_validated_transitions_keyswaptest=
              with_monitor coda_validated_transitions_keyswaptest
          ; coda_replace_snark_worker_key=
              with_monitor coda_replace_snark_worker_key
          ; coda_stop_snark_worker= with_monitor coda_stop_snark_worker } )

    let init_connection_state ~connection:_ ~worker_state:_ = return
  end
end

include Rpc_parallel.Make (T)
