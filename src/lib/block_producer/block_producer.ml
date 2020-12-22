open Core
open Async
open Pipe_lib
open Mina_base
open Coda_state
open Coda_transition
open Signature_lib
open O1trace
open Otp_lib
module Time = Block_time

type Structured_log_events.t += Block_produced
  [@@deriving register_event {msg= "Successfully produced a new block"}]

module Singleton_supervisor : sig
  type ('data, 'a) t

  val create :
    task:(unit Ivar.t -> 'data -> ('a, unit) Interruptible.t) -> ('data, 'a) t

  val cancel : (_, _) t -> unit

  val dispatch : ('data, 'a) t -> 'data -> ('a, unit) Interruptible.t
end = struct
  type ('data, 'a) t =
    { mutable task: (unit Ivar.t * ('a, unit) Interruptible.t) option
    ; f: unit Ivar.t -> 'data -> ('a, unit) Interruptible.t }

  let create ~task = {task= None; f= task}

  let cancel t =
    match t.task with
    | Some (ivar, _) ->
        Ivar.fill ivar () ;
        t.task <- None
    | None ->
        ()

  let dispatch t data =
    cancel t ;
    let ivar = Ivar.create () in
    let interruptible =
      let open Interruptible.Let_syntax in
      t.f ivar data
      >>| fun x ->
      t.task <- None ;
      x
    in
    t.task <- Some (ivar, interruptible) ;
    interruptible
end

module Transition_frontier_validation =
  External_transition.Transition_frontier_validation (Transition_frontier)

let time_to_ms = Fn.compose Time.Span.to_ms Time.to_span_since_epoch

let time_of_ms = Fn.compose Time.of_span_since_epoch Time.Span.of_ms

let lift_sync f =
  Interruptible.uninterruptible
    (Deferred.create (fun ivar -> Ivar.fill ivar (f ())))

module Singleton_scheduler : sig
  type t

  val create : Time.Controller.t -> t

  (** If you reschedule when already scheduled, take the min of the two schedulings *)
  val schedule : t -> Time.t -> f:(unit -> unit) -> unit
end = struct
  type t =
    { mutable timeout: unit Time.Timeout.t option
    ; time_controller: Time.Controller.t }

  let create time_controller = {time_controller; timeout= None}

  let cancel t =
    match t.timeout with
    | Some timeout ->
        Time.Timeout.cancel t.time_controller timeout () ;
        t.timeout <- None
    | None ->
        ()

  let schedule t time ~f =
    let remaining_time = Option.map t.timeout ~f:Time.Timeout.remaining_time in
    cancel t ;
    let span_till_time = Time.diff time (Time.now t.time_controller) in
    let wait_span =
      match remaining_time with
      | Some remaining when Time.Span.(remaining > Time.Span.of_ms Int64.zero)
        ->
          let min a b = if Time.Span.(a < b) then a else b in
          min remaining span_till_time
      | None | Some _ ->
          span_till_time
    in
    let timeout =
      Time.Timeout.create t.time_controller wait_span ~f:(fun _ ->
          t.timeout <- None ;
          f () )
    in
    t.timeout <- Some timeout
end

let generate_next_state ~constraint_constants ~previous_protocol_state
    ~time_controller ~staged_ledger ~transactions ~get_completed_work ~logger
    ~(block_data : Consensus.Data.Block_data.t) ~winner_pk ~scheduled_time
    ~log_block_creation =
  let open Interruptible.Let_syntax in
  let previous_protocol_state_body_hash =
    Protocol_state.body previous_protocol_state |> Protocol_state.Body.hash
  in
  let previous_protocol_state_hash =
    Protocol_state.hash_with_body ~body_hash:previous_protocol_state_body_hash
      previous_protocol_state
  in
  let previous_state_view =
    Protocol_state.body previous_protocol_state
    |> Coda_state.Protocol_state.Body.view
  in
  let supercharge_coinbase =
    let epoch_ledger = Consensus.Data.Block_data.epoch_ledger block_data in
    let global_slot =
      Consensus.Data.Block_data.global_slot_since_genesis block_data
    in
    Staged_ledger.can_apply_supercharged_coinbase_exn ~winner:winner_pk
      ~epoch_ledger ~global_slot
  in
  let%bind res =
    Interruptible.uninterruptible
      (let open Deferred.Let_syntax in
      let coinbase_receiver =
        Consensus.Data.Block_data.coinbase_receiver block_data
      in
      let diff =
        measure "create_diff" (fun () ->
            Staged_ledger.create_diff ~constraint_constants staged_ledger
              ~coinbase_receiver ~logger
              ~current_state_view:previous_state_view
              ~transactions_by_fee:transactions ~get_completed_work
              ~log_block_creation ~supercharge_coinbase )
      in
      match%map
        Staged_ledger.apply_diff_unchecked staged_ledger ~constraint_constants
          diff ~logger ~current_state_view:previous_state_view
          ~state_and_body_hash:
            (previous_protocol_state_hash, previous_protocol_state_body_hash)
          ~coinbase_receiver ~supercharge_coinbase
      with
      | Ok
          ( `Hash_after_applying next_staged_ledger_hash
          , `Ledger_proof ledger_proof_opt
          , `Staged_ledger transitioned_staged_ledger
          , `Pending_coinbase_update (is_new_stack, pending_coinbase_update) )
        ->
          (*staged_ledger remains unchanged and transitioned_staged_ledger is discarded because the external transtion created out of this diff will be applied in Transition_frontier*)
          ignore
          @@ Ledger.unregister_mask_exn ~loc:__LOC__
               (Staged_ledger.ledger transitioned_staged_ledger) ;
          Some
            ( diff
            , next_staged_ledger_hash
            , ledger_proof_opt
            , is_new_stack
            , pending_coinbase_update )
      | Error (Staged_ledger.Staged_ledger_error.Unexpected e) ->
          raise (Error.to_exn e)
      | Error e ->
          [%log error]
            ~metadata:
              [ ( "error"
                , `String (Staged_ledger.Staged_ledger_error.to_string e) )
              ; ( "diff"
                , Staged_ledger_diff.With_valid_signatures_and_proofs.to_yojson
                    diff ) ]
            "Error applying the diff $diff: $error" ;
          None)
  in
  match res with
  | None ->
      Interruptible.return None
  | Some
      ( diff
      , next_staged_ledger_hash
      , ledger_proof_opt
      , is_new_stack
      , pending_coinbase_update ) ->
      let%bind protocol_state, consensus_transition_data =
        lift_sync (fun () ->
            let previous_ledger_hash =
              previous_protocol_state |> Protocol_state.blockchain_state
              |> Blockchain_state.snarked_ledger_hash
            in
            let next_ledger_hash =
              Option.value_map ledger_proof_opt
                ~f:(fun (proof, _) ->
                  Ledger_proof.statement proof |> Ledger_proof.statement_target
                  )
                ~default:previous_ledger_hash
            in
            let snarked_next_available_token =
              match ledger_proof_opt with
              | Some (proof, _) ->
                  (Ledger_proof.statement proof).next_available_token_after
              | None ->
                  previous_protocol_state |> Protocol_state.blockchain_state
                  |> Blockchain_state.snarked_next_available_token
            in
            let genesis_ledger_hash =
              previous_protocol_state |> Protocol_state.blockchain_state
              |> Blockchain_state.genesis_ledger_hash
            in
            let supply_increase =
              Option.value_map ledger_proof_opt
                ~f:(fun (proof, _) ->
                  (Ledger_proof.statement proof).supply_increase )
                ~default:Currency.Amount.Signed.zero
            in
            let blockchain_state =
              (* We use the time of the beginning of the slot because if things
                 are slower than expected, we may have entered the next slot and
                 putting the **current** timestamp rather than the expected one
                 will screw things up.

                 [generate_transition] will log an error if the [current_time]
                 has a different slot from the [scheduled_time]
              *)
              Blockchain_state.create_value ~timestamp:scheduled_time
                ~snarked_ledger_hash:next_ledger_hash ~genesis_ledger_hash
                ~snarked_next_available_token
                ~staged_ledger_hash:next_staged_ledger_hash
            in
            let current_time =
              Time.now time_controller |> Time.to_span_since_epoch
              |> Time.Span.to_ms
            in
            measure "consensus generate_transition" (fun () ->
                Consensus_state_hooks.generate_transition
                  ~previous_protocol_state ~blockchain_state ~current_time
                  ~block_data ~supercharge_coinbase
                  ~snarked_ledger_hash:previous_ledger_hash
                  ~genesis_ledger_hash ~supply_increase ~logger
                  ~constraint_constants ) )
      in
      lift_sync (fun () ->
          measure "making Snark and Internal transitions" (fun () ->
              let snark_transition =
                Snark_transition.create_value
                  ~blockchain_state:
                    (Protocol_state.blockchain_state protocol_state)
                  ~consensus_transition:consensus_transition_data
                  ~pending_coinbase_update ()
              in
              let internal_transition =
                Internal_transition.create ~snark_transition
                  ~prover_state:
                    (Consensus.Data.Block_data.prover_state block_data)
                  ~staged_ledger_diff:(Staged_ledger_diff.forget diff)
                  ~ledger_proof:
                    (Option.map ledger_proof_opt ~f:(fun (proof, _) -> proof))
              in
              let witness =
                { Pending_coinbase_witness.pending_coinbases=
                    Staged_ledger.pending_coinbase_collection staged_ledger
                ; is_new_stack }
              in
              Some (protocol_state, internal_transition, witness) ) )

module Precomputed_block = struct
  type t =
    { scheduled_time: Time.t
    ; protocol_state: Protocol_state.value
    ; protocol_state_proof: Proof.t
    ; staged_ledger_diff: Staged_ledger_diff.t
    ; delta_transition_chain_proof:
        Frozen_ledger_hash.t * Frozen_ledger_hash.t list }
  [@@deriving sexp, to_yojson]
end

let handle_block_production_errors ~logger ~previous_protocol_state
    ~protocol_state x =
  let transition_error_msg_prefix = "Validation failed: " in
  let transition_reason_for_failure =
    " One possible reason could be a ledger-catchup is triggered before we \
     produce a proof for the produced transition."
  in
  let exn_breadcrumb err =
    Error.tag err ~tag:"Error building breadcrumb from produced transition"
    |> Error.raise
  in
  match x with
  | Ok x ->
      return x
  | Error
      (`Prover_error
        ( err
        , ( previous_protocol_state_proof
          , internal_transition
          , pending_coinbase_witness ) )) ->
      [%log error]
        "Prover failed to prove freshly generated transition: $error"
        ~metadata:
          [ ("error", Error_json.error_to_yojson err)
          ; ( "prev_state"
            , Protocol_state.value_to_yojson previous_protocol_state )
          ; ("prev_state_proof", Proof.to_yojson previous_protocol_state_proof)
          ; ("next_state", Protocol_state.value_to_yojson protocol_state)
          ; ( "internal_transition"
            , Internal_transition.to_yojson internal_transition )
          ; ( "pending_coinbase_witness"
            , Pending_coinbase_witness.to_yojson pending_coinbase_witness ) ] ;
      return ()
  | Error `Invalid_genesis_protocol_state ->
      let state_yojson =
        Fn.compose State_hash.to_yojson Protocol_state.genesis_state_hash
      in
      [%log warn]
        ~metadata:
          [ ("expected", state_yojson previous_protocol_state)
          ; ("got", state_yojson protocol_state) ]
        "Produced transition has invalid genesis state hash" ;
      return ()
  | Error `Already_in_frontier ->
      [%log error]
        ~metadata:
          [("protocol_state", Protocol_state.value_to_yojson protocol_state)]
        "%sproduced transition is already in frontier"
        transition_error_msg_prefix ;
      return ()
  | Error `Not_selected_over_frontier_root ->
      [%log warn]
        "%sproduced transition is not selected over the root of transition \
         frontier.%s"
        transition_error_msg_prefix transition_reason_for_failure ;
      return ()
  | Error `Parent_missing_from_frontier ->
      [%log warn]
        "%sparent of produced transition is missing from the frontier.%s"
        transition_error_msg_prefix transition_reason_for_failure ;
      return ()
  | Error (`Fatal_error e) ->
      exn_breadcrumb (Error.tag ~tag:"Fatal error" (Error.of_exn e))
  | Error (`Invalid_staged_ledger_hash e) ->
      exn_breadcrumb (Error.tag ~tag:"Invalid staged ledger hash" e)
  | Error (`Invalid_staged_ledger_diff (e, staged_ledger_diff)) ->
      (* Unexpected errors from staged_ledger are captured in
                         `Fatal_error
                      *)
      [%log error]
        ~metadata:
          [ ("error", Error_json.error_to_yojson e)
          ; ("diff", Staged_ledger_diff.to_yojson staged_ledger_diff) ]
        !"Unable to build breadcrumb from produced transition due to invalid \
          staged ledger diff: $error" ;
      return ()

let time ~logger ~time_controller label f =
  let open Deferred.Result.Let_syntax in
  let t0 = Time.now time_controller in
  let%map x = f () in
  let span = Time.diff (Time.now time_controller) t0 in
  [%log info]
    ~metadata:[("time", `Int (Time.Span.to_ms span |> Int64.to_int_exn))]
    !"%s%!" label ;
  x

let run ~logger ~prover ~verifier ~trust_system ~get_completed_work
    ~transaction_resource_pool ~time_controller ~keypairs
    ~consensus_local_state ~coinbase_receiver ~frontier_reader
    ~transition_writer ~set_next_producer_timing ~log_block_creation
    ~(precomputed_values : Precomputed_values.t) =
  trace "block_producer" (fun () ->
      let constraint_constants = precomputed_values.constraint_constants in
      let consensus_constants = precomputed_values.consensus_constants in
      let log_bootstrap_mode () =
        [%log info] "Pausing block production while bootstrapping"
      in
      let module Breadcrumb = Transition_frontier.Breadcrumb in
      let produce ivar (scheduled_time, block_data, winner_pk) =
        let open Interruptible.Let_syntax in
        match Broadcast_pipe.Reader.peek frontier_reader with
        | None ->
            log_bootstrap_mode () ; Interruptible.return ()
        | Some frontier -> (
            let open Transition_frontier.Extensions in
            let transition_registry =
              get_extension
                (Transition_frontier.extensions frontier)
                Transition_registry
            in
            let crumb = Transition_frontier.best_tip frontier in
            [%log info]
              ~metadata:[("breadcrumb", Breadcrumb.to_yojson crumb)]
              "Producing new block with parent $breadcrumb%!" ;
            let previous_transition =
              Breadcrumb.validated_transition crumb
              |> External_transition.Validation.forget_validation_with_hash
            in
            let previous_protocol_state =
              External_transition.protocol_state
                (With_hash.data previous_transition)
            in
            let previous_protocol_state_proof =
              External_transition.protocol_state_proof
                (With_hash.data previous_transition)
            in
            let transactions =
              Network_pool.Transaction_pool.Resource_pool.transactions ~logger
                transaction_resource_pool
              |> Sequence.map
                   ~f:Transaction_hash.User_command_with_valid_signature.data
            in
            trace_event "waiting for ivar..." ;
            let%bind () =
              Interruptible.lift (Deferred.return ()) (Ivar.read ivar)
            in
            let%bind next_state_opt =
              generate_next_state ~constraint_constants ~scheduled_time
                ~block_data ~previous_protocol_state ~time_controller
                ~staged_ledger:(Breadcrumb.staged_ledger crumb)
                ~transactions ~get_completed_work ~logger ~log_block_creation
                ~winner_pk
            in
            trace_event "next state generated" ;
            match next_state_opt with
            | None ->
                Interruptible.return ()
            | Some
                (protocol_state, internal_transition, pending_coinbase_witness)
              ->
                let protocol_state_hash = Protocol_state.hash protocol_state in
                let consensus_state_with_hash =
                  { With_hash.hash= protocol_state_hash
                  ; data= Protocol_state.consensus_state protocol_state }
                in
                Debug_assert.debug_assert (fun () ->
                    [%test_result: [`Take | `Keep]]
                      (Consensus.Hooks.select ~constants:consensus_constants
                         ~existing:
                           (With_hash.map
                              ~f:External_transition.consensus_state
                              previous_transition)
                         ~candidate:consensus_state_with_hash ~logger)
                      ~expect:`Take
                      ~message:
                        "newly generated consensus states should be selected \
                         over their parent" ;
                    let root_consensus_state_with_hash =
                      Transition_frontier.root frontier
                      |> Breadcrumb.consensus_state_with_hash
                    in
                    [%test_result: [`Take | `Keep]]
                      (Consensus.Hooks.select
                         ~existing:root_consensus_state_with_hash
                         ~constants:consensus_constants
                         ~candidate:consensus_state_with_hash ~logger)
                      ~expect:`Take
                      ~message:
                        "newly generated consensus states should be selected \
                         over the tf root" ) ;
                Interruptible.uninterruptible
                  (let open Deferred.Let_syntax in
                  let emit_breadcrumb () =
                    let open Deferred.Result.Let_syntax in
                    let%bind protocol_state_proof =
                      time ~logger ~time_controller
                        "Protocol_state_proof proving time(ms)" (fun () ->
                          measure "proving state transition valid" (fun () ->
                              Prover.prove prover
                                ~prev_state:previous_protocol_state
                                ~prev_state_proof:previous_protocol_state_proof
                                ~next_state:protocol_state internal_transition
                                pending_coinbase_witness )
                          |> Deferred.Result.map_error ~f:(fun err ->
                                 `Prover_error
                                   ( err
                                   , ( previous_protocol_state_proof
                                     , internal_transition
                                     , pending_coinbase_witness ) ) ) )
                    in
                    let staged_ledger_diff =
                      Internal_transition.staged_ledger_diff
                        internal_transition
                    in
                    let previous_state_hash =
                      Protocol_state.hash previous_protocol_state
                    in
                    let delta_transition_chain_proof =
                      Transition_chain_prover.prove
                        ~length:
                          (Coda_numbers.Length.to_int consensus_constants.delta)
                        ~frontier previous_state_hash
                      |> Option.value_exn
                    in
                    let%bind transition =
                      let open Result.Let_syntax in
                      External_transition.Validation.wrap
                        { With_hash.hash= protocol_state_hash
                        ; data=
                            External_transition.create ~protocol_state
                              ~protocol_state_proof ~staged_ledger_diff
                              ~validation_callback:
                                (Coda_net2.Validation_callback
                                 .create_without_expiration ())
                              ~delta_transition_chain_proof () }
                      |> External_transition.skip_time_received_validation
                           `This_transition_was_not_received_via_gossip
                      |> External_transition.skip_protocol_versions_validation
                           `This_transition_has_valid_protocol_versions
                      |> External_transition.validate_genesis_protocol_state
                           ~genesis_state_hash:
                             (Protocol_state.genesis_state_hash
                                ~state_hash:(Some previous_state_hash)
                                previous_protocol_state)
                      >>| External_transition.skip_proof_validation
                            `This_transition_was_generated_internally
                      >>| External_transition
                          .skip_delta_transition_chain_validation
                            `This_transition_was_not_received_via_gossip
                      >>= Transition_frontier_validation
                          .validate_frontier_dependencies ~logger ~frontier
                            ~consensus_constants
                      |> Deferred.return
                    in
                    let transition_receipt_time =
                      Some (Core_kernel.Time.now ())
                    in
                    let%bind breadcrumb =
                      time ~logger ~time_controller
                        "Build breadcrumb on produced block" (fun () ->
                          Breadcrumb.build ~logger ~precomputed_values
                            ~verifier ~trust_system ~parent:crumb ~transition
                            ~sender:None (* Consider skipping here *)
                            ~skip_staged_ledger_verification:false
                            ~transition_receipt_time () )
                      |> Deferred.Result.map_error ~f:(function
                           | `Invalid_staged_ledger_diff e ->
                               `Invalid_staged_ledger_diff
                                 (e, staged_ledger_diff)
                           | ( `Fatal_error _
                             | `Invalid_genesis_protocol_state
                             | `Invalid_staged_ledger_hash _
                             | `Not_selected_over_frontier_root
                             | `Parent_missing_from_frontier
                             | `Prover_error _ ) as err ->
                               err )
                    in
                    [%str_log info]
                      ~metadata:
                        [("breadcrumb", Breadcrumb.to_yojson breadcrumb)]
                      Block_produced ;
                    let metadata =
                      [("state_hash", State_hash.to_yojson protocol_state_hash)]
                    in
                    Mina_metrics.(
                      Counter.inc_one Block_producer.blocks_produced) ;
                    let%bind.Async () =
                      Strict_pipe.Writer.write transition_writer breadcrumb
                    in
                    [%log debug] ~metadata
                      "Waiting for block $state_hash to be inserted into \
                       frontier" ;
                    Deferred.choose
                      [ Deferred.choice
                          (Transition_registry.register transition_registry
                             protocol_state_hash)
                          (Fn.const (Ok `Transition_accepted))
                      ; Deferred.choice
                          ( Time.Timeout.create time_controller
                              (* We allow up to 20 seconds for the transition
                                 to make its way from the transition_writer to
                                 the frontier.
                                 This value is chosen to be reasonably
                                 generous. In theory, this should not take
                                 terribly long. But long cycles do happen in
                                 our system, and with medium curves those long
                                 cycles can be substantial.
                              *)
                              (Time.Span.of_ms 20000L)
                              ~f:(Fn.const ())
                          |> Time.Timeout.to_deferred )
                          (Fn.const (Ok `Timed_out)) ]
                    >>= function
                    | `Transition_accepted ->
                        [%log info] ~metadata
                          "Generated transition $state_hash was accepted into \
                           transition frontier" ;
                        return ()
                    | `Timed_out ->
                        (* FIXME #3167: this should be fatal, and more
                           importantly, shouldn't happen.
                        *)
                        [%log fatal] ~metadata
                          "Timed out waiting for generated transition \
                           $state_hash to enter transition frontier. \
                           Continuing to produce new blocks anyway. This may \
                           mean your CPU is overloaded. Consider disabling \
                           `-run-snark-worker` if it's configured." ;
                        return ()
                  in
                  let%bind res = emit_breadcrumb () in
                  handle_block_production_errors ~logger
                    ~previous_protocol_state ~protocol_state res) )
      in
      let production_supervisor = Singleton_supervisor.create ~task:produce in
      let scheduler = Singleton_scheduler.create time_controller in
      let rec check_next_block_timing () =
        trace_recurring "check next block timing" (fun () ->
            (* See if we want to change keypairs *)
            let keypairs =
              match Agent.get keypairs with
              | keypairs, `Different ->
                  (* Perform block production key swap since we have new
                     keypairs *)
                  Consensus.Data.Local_state.block_production_keys_swap
                    ~constants:consensus_constants consensus_local_state
                    ( Keypair.And_compressed_pk.Set.to_list keypairs
                    |> List.map ~f:snd |> Public_key.Compressed.Set.of_list )
                    (Time.now time_controller) ;
                  keypairs
              | keypairs, `Same ->
                  keypairs
            in
            (* Begin checking for the ability to produce a block *)
            match Broadcast_pipe.Reader.peek frontier_reader with
            | None ->
                log_bootstrap_mode () ;
                don't_wait_for
                  (let%map () =
                     Broadcast_pipe.Reader.iter_until frontier_reader
                       ~f:(Fn.compose Deferred.return Option.is_some)
                   in
                   check_next_block_timing ())
            | Some transition_frontier -> (
                let consensus_state =
                  Transition_frontier.best_tip transition_frontier
                  |> Breadcrumb.consensus_state
                in
                (* TODO: Re-enable this assertion when it doesn't fail dev demos
                 *       (see #5354)
                 * assert (
                  Consensus.Hooks.required_local_state_sync
                    ~constants:consensus_constants ~consensus_state
                    ~local_state:consensus_local_state
                  = None ) ; *)
                let now = Time.now time_controller in
                let next_producer_timing =
                  measure "asking consensus what to do" (fun () ->
                      Consensus.Hooks.next_producer_timing
                        ~constraint_constants ~constants:consensus_constants
                        (time_to_ms now) consensus_state
                        ~local_state:consensus_local_state ~keypairs
                        ~coinbase_receiver ~logger )
                in
                set_next_producer_timing next_producer_timing ;
                match next_producer_timing with
                | `Check_again time ->
                    Singleton_scheduler.schedule scheduler (time_of_ms time)
                      ~f:check_next_block_timing
                | `Produce_now (data, winner_pk) ->
                    Mina_metrics.(Counter.inc_one Block_producer.slots_won) ;
                    Interruptible.finally
                      (Singleton_supervisor.dispatch production_supervisor
                         (now, data, winner_pk))
                      ~f:check_next_block_timing
                    |> ignore
                | `Produce (time, data, winner_pk) ->
                    Mina_metrics.(Counter.inc_one Block_producer.slots_won) ;
                    let scheduled_time = time_of_ms time in
                    Singleton_scheduler.schedule scheduler scheduled_time
                      ~f:(fun () ->
                        ignore
                          (Interruptible.finally
                             (Singleton_supervisor.dispatch
                                production_supervisor
                                (scheduled_time, data, winner_pk))
                             ~f:check_next_block_timing) ) ) )
      in
      let start () =
        (* Schedule to wake up immediately on the next tick of the producer loop
         * instead of immediately mutating local_state here as there could be a
         * race.
         *
         * Given that rescheduling takes the min of the two timeouts, we won't
         * erase this timeout even if the last run of the producer wants to wait
         * for a long while.
         * *)
        Agent.on_update keypairs ~f:(fun _new_keypairs ->
            Singleton_scheduler.schedule scheduler (Time.now time_controller)
              ~f:check_next_block_timing ) ;
        check_next_block_timing ()
      in
      let genesis_state_timestamp =
        consensus_constants.genesis_state_timestamp
      in
      (* if the producer starts before genesis, sleep until genesis *)
      let now = Time.now time_controller in
      if Time.( >= ) now genesis_state_timestamp then start ()
      else
        let time_till_genesis = Time.diff genesis_state_timestamp now in
        [%log warn]
          ~metadata:
            [ ( "time_till_genesis"
              , `Int (Int64.to_int_exn (Time.Span.to_ms time_till_genesis)) )
            ]
          "Node started before genesis: waiting $time_till_genesis \
           milliseconds before starting block producer" ;
        ignore
          (Time.Timeout.create time_controller time_till_genesis ~f:(fun _ ->
               start () )) )

let run_precomputed ~logger ~verifier ~trust_system ~time_controller
    ~frontier_reader ~transition_writer ~precomputed_blocks
    ~(precomputed_values : Precomputed_values.t) =
  let consensus_constants = precomputed_values.consensus_constants in
  let log_bootstrap_mode () =
    [%log info] "Pausing block production while bootstrapping"
  in
  let module Breadcrumb = Transition_frontier.Breadcrumb in
  let produce
      { Precomputed_block.scheduled_time= _
      ; protocol_state
      ; protocol_state_proof
      ; staged_ledger_diff
      ; delta_transition_chain_proof } =
    let protocol_state_hash = Protocol_state.hash protocol_state in
    let consensus_state_with_hash =
      { With_hash.hash= protocol_state_hash
      ; data= Protocol_state.consensus_state protocol_state }
    in
    match Broadcast_pipe.Reader.peek frontier_reader with
    | None ->
        log_bootstrap_mode () ; return ()
    | Some frontier ->
        let open Transition_frontier.Extensions in
        let transition_registry =
          get_extension
            (Transition_frontier.extensions frontier)
            Transition_registry
        in
        let crumb = Transition_frontier.best_tip frontier in
        [%log trace]
          ~metadata:[("breadcrumb", Breadcrumb.to_yojson crumb)]
          "Emitting precomputed block with parent $breadcrumb%!" ;
        let previous_transition =
          Breadcrumb.validated_transition crumb
          |> External_transition.Validation.forget_validation_with_hash
        in
        let previous_protocol_state =
          External_transition.protocol_state
            (With_hash.data previous_transition)
        in
        Debug_assert.debug_assert (fun () ->
            [%test_result: [`Take | `Keep]]
              (Consensus.Hooks.select ~constants:consensus_constants
                 ~existing:
                   (With_hash.map ~f:External_transition.consensus_state
                      previous_transition)
                 ~candidate:consensus_state_with_hash ~logger)
              ~expect:`Take
              ~message:
                "newly generated consensus states should be selected over \
                 their parent" ;
            let root_consensus_state_with_hash =
              Transition_frontier.root frontier
              |> Breadcrumb.consensus_state_with_hash
            in
            [%test_result: [`Take | `Keep]]
              (Consensus.Hooks.select ~existing:root_consensus_state_with_hash
                 ~constants:consensus_constants
                 ~candidate:consensus_state_with_hash ~logger)
              ~expect:`Take
              ~message:
                "newly generated consensus states should be selected over the \
                 tf root" ) ;
        let emit_breadcrumb () =
          let open Deferred.Result.Let_syntax in
          let previous_protocol_state_hash =
            With_hash.hash previous_transition
          in
          let%bind transition =
            let open Result.Let_syntax in
            External_transition.Validation.wrap
              { With_hash.hash= protocol_state_hash
              ; data=
                  External_transition.create ~protocol_state
                    ~protocol_state_proof ~staged_ledger_diff
                    ~validation_callback:
                      (Coda_net2.Validation_callback.create_without_expiration
                         ())
                    ~delta_transition_chain_proof () }
            |> External_transition.skip_time_received_validation
                 `This_transition_was_not_received_via_gossip
            |> External_transition.skip_protocol_versions_validation
                 `This_transition_has_valid_protocol_versions
            |> External_transition.validate_genesis_protocol_state
                 ~genesis_state_hash:
                   (Protocol_state.genesis_state_hash
                      ~state_hash:(Some previous_protocol_state_hash)
                      previous_protocol_state)
            >>| External_transition.skip_proof_validation
                  `This_transition_was_generated_internally
            >>| External_transition.skip_delta_transition_chain_validation
                  `This_transition_was_not_received_via_gossip
            >>= Transition_frontier_validation.validate_frontier_dependencies
                  ~logger ~frontier ~consensus_constants
            |> Deferred.return
          in
          let transition_receipt_time = None in
          let%bind breadcrumb =
            time ~logger ~time_controller
              "Build breadcrumb on produced block (precomputed)" (fun () ->
                Breadcrumb.build ~logger ~precomputed_values ~verifier
                  ~trust_system ~parent:crumb ~transition ~sender:None
                  ~skip_staged_ledger_verification:false
                  ~transition_receipt_time ()
                |> Deferred.Result.map_error ~f:(function
                     | `Invalid_staged_ledger_diff e ->
                         `Invalid_staged_ledger_diff (e, staged_ledger_diff)
                     | ( `Fatal_error _
                       | `Invalid_genesis_protocol_state
                       | `Invalid_staged_ledger_hash _
                       | `Not_selected_over_frontier_root
                       | `Parent_missing_from_frontier ) as err ->
                         err ) )
          in
          [%str_log trace]
            ~metadata:[("breadcrumb", Breadcrumb.to_yojson breadcrumb)]
            Block_produced ;
          let metadata =
            [("state_hash", State_hash.to_yojson protocol_state_hash)]
          in
          Mina_metrics.(Counter.inc_one Block_producer.blocks_produced) ;
          let%bind.Async () =
            Strict_pipe.Writer.write transition_writer breadcrumb
          in
          [%log debug] ~metadata
            "Waiting for block $state_hash to be inserted into frontier" ;
          Deferred.choose
            [ Deferred.choice
                (Transition_registry.register transition_registry
                   protocol_state_hash)
                (Fn.const (Ok `Transition_accepted))
            ; Deferred.choice
                ( Time.Timeout.create time_controller (Time.Span.of_ms 20000L)
                    ~f:(Fn.const ())
                |> Time.Timeout.to_deferred )
                (Fn.const (Ok `Timed_out)) ]
          >>= function
          | `Transition_accepted ->
              [%log info] ~metadata
                "Generated transition $state_hash was accepted into \
                 transition frontier" ;
              return ()
          | `Timed_out ->
              (* FIXME #3167: this should be fatal, and more importantly,
                 shouldn't happen.
              *)
              [%log fatal] ~metadata
                "Timed out waiting for generated transition $state_hash to \
                 enter transition frontier. Continuing to produce new blocks \
                 anyway. This may mean your CPU is overloaded. Consider \
                 disabling `-run-snark-worker` if it's configured." ;
              return ()
        in
        let%bind res = emit_breadcrumb () in
        handle_block_production_errors ~logger ~previous_protocol_state
          ~protocol_state res
  in
  let rec emit_next_block precomputed_blocks =
    (* Begin checking for the ability to produce a block *)
    match Broadcast_pipe.Reader.peek frontier_reader with
    | None ->
        log_bootstrap_mode () ;
        let%bind () =
          Broadcast_pipe.Reader.iter_until frontier_reader
            ~f:(Fn.compose Deferred.return Option.is_some)
        in
        emit_next_block precomputed_blocks
    | Some _transition_frontier -> (
      match Sequence.next precomputed_blocks with
      | Some (precomputed_block, precomputed_blocks) ->
          let new_time_offset =
            Core_kernel.Time.diff (Core_kernel.Time.now ())
              (Block_time.to_time
                 precomputed_block.Precomputed_block.scheduled_time)
          in
          [%log info]
            "Changing time offset from $old_time_offset to $new_time_offset"
            ~metadata:
              [ ( "old_time_offset"
                , `String
                    (Core_kernel.Time.Span.to_string_hum
                       (Block_time.Controller.get_time_offset ~logger)) )
              ; ( "new_time_offset"
                , `String (Core_kernel.Time.Span.to_string_hum new_time_offset)
                ) ] ;
          Block_time.Controller.set_time_offset new_time_offset ;
          let%bind () = produce precomputed_block in
          emit_next_block precomputed_blocks
      | None ->
          return () )
  in
  emit_next_block precomputed_blocks
