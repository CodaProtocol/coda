open Core
open Async
open Mina_base
open Mina_state
open Mina_transition
open Blockchain_snark

module type S = Intf.S

module Extend_blockchain_input = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t =
        { chain : Blockchain.Stable.V1.t
        ; next_state : Protocol_state.Value.Stable.V1.t
        ; block : Snark_transition.Value.Stable.V1.t
        ; ledger_proof : Ledger_proof.Stable.V1.t option
        ; prover_state : Consensus.Data.Prover_state.Stable.V1.t
        ; pending_coinbase : Pending_coinbase_witness.Stable.V1.t
        }

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t =
    { chain : Blockchain.t
    ; next_state : Protocol_state.Value.t
    ; block : Snark_transition.Value.t
    ; ledger_proof : Ledger_proof.t option
    ; prover_state : Consensus.Data.Prover_state.t
    ; pending_coinbase : Pending_coinbase_witness.t
    }
  [@@deriving sexp]
end

module Consensus_mechanism = Consensus
module Blockchain = Blockchain

module Worker_state = struct
  module type S = sig
    val extend_blockchain :
         Blockchain.t
      -> Protocol_state.Value.t
      -> Snark_transition.value
      -> Ledger_proof.t option
      -> Consensus.Data.Prover_state.t
      -> Pending_coinbase_witness.t
      -> Blockchain.t Async.Deferred.Or_error.t

    val verify : Protocol_state.Value.t -> Proof.t -> bool Deferred.t
  end

  (* bin_io required by rpc_parallel *)
  type init_arg =
    { conf_dir : string
    ; logger : Logger.Stable.Latest.t
    ; proof_level : Genesis_constants.Proof_level.t
    ; constraint_constants : Genesis_constants.Constraint_constants.t
    }
  [@@deriving bin_io_unversioned]

  type t = (module S)

  let ledger_proof_opt (chain : Blockchain.t) next_state = function
    | Some t ->
        Ledger_proof.
          ({ (statement t) with sok_digest = sok_digest t }, underlying_proof t)
    | None ->
        let bs = Protocol_state.blockchain_state in
        let lh x = Blockchain_state.snarked_ledger_hash (bs x) in
        let tok x = Blockchain_state.snarked_next_available_token (bs x) in
        let chain_state = Blockchain_snark.Blockchain.state chain in
        ( { source = lh chain_state
          ; target = lh next_state
          ; supply_increase = Currency.Amount.zero
          ; fee_excess = Fee_excess.zero
          ; sok_digest = Sok_message.Digest.default
          ; next_available_token_before = tok chain_state
          ; next_available_token_after = tok next_state
          ; pending_coinbase_stack_state =
              { source = Pending_coinbase.Stack.empty
              ; target = Pending_coinbase.Stack.empty
              }
          }
        , Proof.transaction_dummy )

  let create { logger; proof_level; constraint_constants; _ } : t Deferred.t =
    Deferred.return
      (let m =
         match proof_level with
         | Genesis_constants.Proof_level.Full ->
             ( module struct
               module T = Transaction_snark.Make (struct
                 let constraint_constants = constraint_constants

                 let proof_level = proof_level
               end)

               module B = Blockchain_snark.Blockchain_snark_state.Make (struct
                 let tag = T.tag

                 let constraint_constants = constraint_constants

                 let proof_level = proof_level
               end)

               let (_ : Pickles.Dirty.t) =
                 Pickles.Cache_handle.generate_or_load B.cache_handle

               let extend_blockchain (chain : Blockchain.t)
                   (next_state : Protocol_state.Value.t)
                   (block : Snark_transition.value) (t : Ledger_proof.t option)
                   state_for_handler pending_coinbase =
                 let%map.Async.Deferred res =
                   Deferred.Or_error.try_with ~here:[%here] (fun () ->
                       let t = ledger_proof_opt chain next_state t in
                       let%map.Async.Deferred proof =
                         B.step
                           ~handler:
                             (Consensus.Data.Prover_state.handler
                                ~constraint_constants state_for_handler
                                ~pending_coinbase)
                           { transition = block
                           ; prev_state =
                               Blockchain_snark.Blockchain.state chain
                           }
                           [ ( Blockchain_snark.Blockchain.state chain
                             , Blockchain_snark.Blockchain.proof chain )
                           ; t
                           ]
                           next_state
                       in
                       Blockchain_snark.Blockchain.create ~state:next_state
                         ~proof)
                 in
                 Or_error.iter_error res ~f:(fun e ->
                     [%log error]
                       ~metadata:[ ("error", Error_json.error_to_yojson e) ]
                       "Prover threw an error while extending block: $error") ;
                 res

               let verify state proof = B.Proof.verify [ (state, proof) ]
             end : S )
         | Check ->
             ( module struct
               module Transaction_snark = Transaction_snark

               let extend_blockchain (chain : Blockchain.t)
                   (next_state : Protocol_state.Value.t)
                   (block : Snark_transition.value) (t : Ledger_proof.t option)
                   state_for_handler pending_coinbase =
                 let t, _proof = ledger_proof_opt chain next_state t in
                 let res =
                   Blockchain_snark.Blockchain_snark_state.check ~proof_level
                     ~constraint_constants
                     { transition = block
                     ; prev_state = Blockchain_snark.Blockchain.state chain
                     }
                     ~handler:
                       (Consensus.Data.Prover_state.handler state_for_handler
                          ~constraint_constants ~pending_coinbase)
                     t
                     (Protocol_state.hash next_state)
                   |> Or_error.map ~f:(fun () ->
                          Blockchain_snark.Blockchain.create ~state:next_state
                            ~proof:Mina_base.Proof.blockchain_dummy)
                 in
                 Or_error.iter_error res ~f:(fun e ->
                     [%log error]
                       ~metadata:[ ("error", Error_json.error_to_yojson e) ]
                       "Prover threw an error while extending block: $error") ;
                 Async.Deferred.return res

               let verify _state _proof = Deferred.return true
             end : S )
         | None ->
             ( module struct
               module Transaction_snark = Transaction_snark

               let extend_blockchain _chain next_state _block _ledger_proof
                   _state_for_handler _pending_coinbase =
                 Deferred.return
                 @@ Ok
                      (Blockchain_snark.Blockchain.create
                         ~proof:Mina_base.Proof.blockchain_dummy
                         ~state:next_state)

               let verify _ _ = Deferred.return true
             end : S )
       in
       Memory_stats.log_memory_stats logger ~process:"prover" ;
       m)

  let get = Fn.id
end

module Functions = struct
  type ('i, 'o) t =
    'i Bin_prot.Type_class.t
    * 'o Bin_prot.Type_class.t
    * (Worker_state.t -> 'i -> 'o Deferred.t)

  let create input output f : ('i, 'o) t = (input, output, f)

  let initialized =
    create bin_unit [%bin_type_class: [ `Initialized ]] (fun w () ->
        let (module W) = Worker_state.get w in
        Deferred.return `Initialized)

  let extend_blockchain =
    create Extend_blockchain_input.Stable.Latest.bin_t
      [%bin_type_class: Blockchain.Stable.Latest.t Or_error.t]
      (fun
        w
        { chain
        ; next_state
        ; ledger_proof
        ; block
        ; prover_state
        ; pending_coinbase
        }
      ->
        let (module W) = Worker_state.get w in
        W.extend_blockchain chain next_state block ledger_proof prover_state
          pending_coinbase)

  let verify_blockchain =
    create Blockchain.Stable.Latest.bin_t bin_bool (fun w chain ->
        let (module W) = Worker_state.get w in
        W.verify
          (Blockchain_snark.Blockchain.state chain)
          (Blockchain_snark.Blockchain.proof chain))
end

module Worker = struct
  module T = struct
    module F = Rpc_parallel.Function

    type 'w functions =
      { initialized : ('w, unit, [ `Initialized ]) F.t
      ; extend_blockchain :
          ('w, Extend_blockchain_input.t, Blockchain.t Or_error.t) F.t
      ; verify_blockchain : ('w, Blockchain.t, bool) F.t
      }

    module Worker_state = Worker_state

    module Connection_state = struct
      (* bin_io required by rpc_parallel *)
      type init_arg = unit [@@deriving bin_io_unversioned]

      type t = unit
    end

    module Functions
        (C : Rpc_parallel.Creator
               with type worker_state := Worker_state.t
                and type connection_state := Connection_state.t) =
    struct
      let functions =
        let f (i, o, f) =
          C.create_rpc
            ~f:(fun ~worker_state ~conn_state:_ i -> f worker_state i)
            ~bin_input:i ~bin_output:o ()
        in
        let open Functions in
        { initialized = f initialized
        ; extend_blockchain = f extend_blockchain
        ; verify_blockchain = f verify_blockchain
        }

      let init_worker_state
          Worker_state.{ conf_dir; logger; proof_level; constraint_constants } =
        let max_size = 256 * 1024 * 512 in
        let num_rotate = 1 in
        Logger.Consumer_registry.register ~id:"default"
          ~processor:(Logger.Processor.raw ())
          ~transport:
            (Logger.Transport.File_system.dumb_logrotate ~directory:conf_dir
               ~log_filename:"mina-prover.log" ~max_size ~num_rotate) ;
        [%log info] "Prover started" ;
        Worker_state.create
          { conf_dir; logger; proof_level; constraint_constants }

      let init_connection_state ~connection:_ ~worker_state:_ () = Deferred.unit
    end
  end

  include Rpc_parallel.Make (T)
end

type t =
  { connection : Worker.Connection.t; process : Process.t; logger : Logger.t }

let create ~logger ~pids ~conf_dir ~proof_level ~constraint_constants =
  let on_failure err =
    [%log error] "Prover process failed with error $err"
      ~metadata:[ ("err", Error_json.error_to_yojson err) ] ;
    Error.raise err
  in
  let%map connection, process =
    (* HACK: Need to make connection_timeout long since creating a prover can take a long time*)
    Worker.spawn_in_foreground_exn ~connection_timeout:(Time.Span.of_min 1.)
      ~on_failure ~shutdown_on:Disconnect ~connection_state_init_arg:()
      { conf_dir; logger; proof_level; constraint_constants }
  in
  [%log info]
    "Daemon started process of kind $process_kind with pid $prover_pid"
    ~metadata:
      [ ("prover_pid", `Int (Process.pid process |> Pid.to_int))
      ; ( "process_kind"
        , `String Child_processes.Termination.(show_process_kind Prover) )
      ] ;
  Child_processes.Termination.register_process pids process
    Child_processes.Termination.Prover ;
  let exit_or_signal =
    Child_processes.Termination.wait_safe ~logger process ~module_:__MODULE__
      ~location:__LOC__ ~here:[%here]
  in
  don't_wait_for
    ( match%bind exit_or_signal with
    | Ok (Ok ()) ->
        [%log fatal] "Unexpected prover termination, terminating daemon" ;
        Async.exit 1
    | Ok (Error (`Exit_non_zero n)) ->
        [%log fatal]
          "Prover terminated with nonzero exit code, terminating daemon"
          ~metadata:[ ("exit_code", `Int n) ] ;
        Async.exit 1
    | Ok (Error (`Signal signal)) ->
        let signal_str = Signal.to_string signal in
        [%log fatal] "Prover terminated due to signal, terminating daemon"
          ~metadata:[ ("signal", `String signal_str) ] ;
        Async.exit 1
    | Error err ->
        let err_str = Error.to_string_hum err in
        [%log fatal] "Error waiting on prover process, terminating daemon"
          ~metadata:[ ("error", `String err_str) ] ;
        Async.exit 1 ) ;
  don't_wait_for
  @@ Pipe.iter
       (Process.stdout process |> Reader.pipe)
       ~f:(fun stdout ->
         return
         @@ [%log debug] "Prover stdout: $stdout"
              ~metadata:[ ("stdout", `String stdout) ]) ;
  don't_wait_for
  @@ Pipe.iter
       (Process.stderr process |> Reader.pipe)
       ~f:(fun stderr ->
         return
         @@ [%log error] "Prover stderr: $stderr"
              ~metadata:[ ("stderr", `String stderr) ]) ;
  { connection; process; logger }

let initialized { connection; _ } =
  Worker.Connection.run connection ~f:Worker.functions.initialized ~arg:()

let prove_from_input_sexp { connection; logger; _ } sexp =
  let input = Extend_blockchain_input.t_of_sexp sexp in
  match%map
    Worker.Connection.run connection ~f:Worker.functions.extend_blockchain
      ~arg:input
    >>| Or_error.join
  with
  | Ok _ ->
      [%log info] "prover succeeded :)" ;
      true
  | Error e ->
      [%log error] "prover errored :("
        ~metadata:[ ("error", Error_json.error_to_yojson e) ] ;
      false

let extend_blockchain { connection; logger; _ } chain next_state block
    ledger_proof prover_state pending_coinbase =
  let input =
    { Extend_blockchain_input.chain
    ; next_state
    ; block
    ; ledger_proof
    ; prover_state
    ; pending_coinbase
    }
  in
  match%map
    Worker.Connection.run connection ~f:Worker.functions.extend_blockchain
      ~arg:input
    >>| Or_error.join
  with
  | Ok x ->
      Ok x
  | Error e ->
      [%log error]
        ~metadata:
          [ ( "input-sexp"
            , `String (Sexp.to_string (Extend_blockchain_input.sexp_of_t input))
            )
          ; ( "input-bin-io"
            , `String
                (Base64.encode_exn
                   (Binable.to_string
                      (module Extend_blockchain_input.Stable.Latest)
                      input)) )
          ; ("error", Error_json.error_to_yojson e)
          ]
        "Prover failed: $error" ;
      Error e

let prove t ~prev_state ~prev_state_proof ~next_state
    (transition : Internal_transition.t) pending_coinbase =
  let open Deferred.Or_error.Let_syntax in
  let start_time = Core.Time.now () in
  let%map chain =
    extend_blockchain t
      (Blockchain.create ~proof:prev_state_proof ~state:prev_state)
      next_state
      (Internal_transition.snark_transition transition)
      (Internal_transition.ledger_proof transition)
      (Internal_transition.prover_state transition)
      pending_coinbase
  in
  Mina_metrics.(
    Gauge.set Cryptography.blockchain_proving_time_ms
      (Core.Time.Span.to_ms @@ Core.Time.diff (Core.Time.now ()) start_time)) ;
  Blockchain_snark.Blockchain.proof chain

let create_genesis_block t (genesis_inputs : Genesis_proof.Inputs.t) =
  let start_time = Core.Time.now () in
  let genesis_ledger = Genesis_ledger.Packed.t genesis_inputs.genesis_ledger in
  let constraint_constants = genesis_inputs.constraint_constants in
  let consensus_constants = genesis_inputs.consensus_constants in
  let prev_state =
    Protocol_state.negative_one ~genesis_ledger
      ~genesis_epoch_data:genesis_inputs.genesis_epoch_data
      ~constraint_constants ~consensus_constants
  in
  let genesis_epoch_ledger =
    match genesis_inputs.genesis_epoch_data with
    | None ->
        genesis_ledger
    | Some data ->
        data.staking.ledger
  in
  let open Pickles_types in
  let blockchain_dummy = Pickles.Proof.dummy Nat.N2.n Nat.N2.n Nat.N2.n in
  let snark_transition =
    Snark_transition.genesis ~constraint_constants ~consensus_constants
      ~genesis_ledger
  in
  let pending_coinbase =
    { Mina_base.Pending_coinbase_witness.pending_coinbases =
        Mina_base.Pending_coinbase.create
          ~depth:constraint_constants.pending_coinbase_depth ()
        |> Or_error.ok_exn
    ; is_new_stack = true
    }
  in
  let prover_state : Consensus_mechanism.Data.Prover_state.t =
    Consensus.Data.Prover_state.genesis_data ~genesis_epoch_ledger
  in
  let%map chain =
    extend_blockchain t
      (Blockchain.create ~proof:blockchain_dummy ~state:prev_state)
      genesis_inputs.protocol_state_with_hash.data snark_transition None
      prover_state pending_coinbase
  in
  Mina_metrics.(
    Gauge.set Cryptography.blockchain_proving_time_ms
      (Core.Time.Span.to_ms @@ Core.Time.diff (Core.Time.now ()) start_time)) ;
  chain
