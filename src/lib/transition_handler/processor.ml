(** This module contains the transition processor. The transition processor is
 *  the thread in which transitions are attached the to the transition frontier.
 *
 *  Two types of data are handled by the transition processor: validated external transitions
 *  with precomputed state hashes (via the proposer and validator pipes) and breadcrumb rose
 *  trees (via the catchup pipe).
 *)

open Core_kernel
open Async_kernel
open Pipe_lib.Strict_pipe
open Coda_base
open Coda_state
open Cache_lib
open O1trace
open Coda_transition
module Transition_frontier_validation =
  External_transition.Transition_frontier_validation (Transition_frontier)

(* TODO: calculate a sensible value from postake consensus arguments *)
let catchup_timeout_duration =
  Block_time.Span.of_ms
    (Consensus.Constants.(delta * block_window_duration_ms) |> Int64.of_int)

let cached_transform_deferred_result ~transform_cached ~transform_result cached
    =
  Cached.transform cached ~f:transform_cached
  |> Cached.sequence_deferred
  >>= Fn.compose transform_result Cached.sequence_result

(* add a breadcrumb and perform post processing *)
let add_and_finalize ~logger ~frontier ~catchup_scheduler
    ~processed_transition_writer ~only_if_present cached_breadcrumb =
  let breadcrumb =
    if Cached.is_pure cached_breadcrumb then Cached.peek cached_breadcrumb
    else Cached.invalidate_with_success cached_breadcrumb
  in
  let transition =
    Transition_frontier.Breadcrumb.validated_transition breadcrumb
  in
  let%map () =
    if only_if_present then (
      let parent_hash =
        Transition_frontier.Breadcrumb.parent_hash breadcrumb
      in
      match Transition_frontier.find frontier parent_hash with
      | Some _ ->
          Transition_frontier.add_breadcrumb_exn frontier breadcrumb
      | None ->
          Logger.warn logger ~module_:__MODULE__ ~location:__LOC__
            !"When trying to add breadcrumb, its parent had been removed from \
              transition frontier: %{sexp: State_hash.t}"
            parent_hash ;
          Deferred.unit )
    else Transition_frontier.add_breadcrumb_exn frontier breadcrumb
  in
  Writer.write processed_transition_writer transition ;
  Catchup_scheduler.notify catchup_scheduler
    ~hash:(External_transition.Validated.state_hash transition)

let process_transition ~logger ~trust_system ~verifier ~frontier
    ~catchup_scheduler ~processed_transition_writer
    ~transition:cached_initially_validated_transition =
  let enveloped_initially_validated_transition =
    Cached.peek cached_initially_validated_transition
  in
  let sender =
    Envelope.Incoming.sender enveloped_initially_validated_transition
  in
  let initially_validated_transition =
    Envelope.Incoming.data enveloped_initially_validated_transition
  in
  let {With_hash.hash= transition_hash; data= transition}, _ =
    initially_validated_transition
  in
  let metadata = [("state_hash", State_hash.to_yojson transition_hash)] in
  Deferred.map ~f:(Fn.const ())
    (let open Deferred.Result.Let_syntax in
    let%bind mostly_validated_transition =
      let open Deferred.Let_syntax in
      match
        Transition_frontier_validation.validate_frontier_dependencies ~logger
          ~frontier initially_validated_transition
      with
      | Ok t ->
          return (Ok t)
      | Error `Not_selected_over_frontier_root ->
          let%map () =
            Trust_system.record_envelope_sender trust_system logger sender
              ( Trust_system.Actions.Gossiped_invalid_transition
              , Some
                  ( "The transition with hash $state_hash was not selected \
                     over the transition frontier root"
                  , metadata ) )
          in
          let (_ : External_transition.Initial_validated.t Envelope.Incoming.t)
              =
            Cached.invalidate_with_failure
              cached_initially_validated_transition
          in
          Error ()
      | Error `Already_in_frontier ->
          Logger.warn logger ~module_:__MODULE__ ~location:__LOC__ ~metadata
            "Refusing to process the transition with hash $state_hash because \
             is is already in the transition frontier" ;
          let (_ : External_transition.Initial_validated.t Envelope.Incoming.t)
              =
            Cached.invalidate_with_failure
              cached_initially_validated_transition
          in
          return (Error ())
      | Error `Parent_missing_from_frontier -> (
          let _, validation =
            Cached.peek cached_initially_validated_transition
            |> Envelope.Incoming.data
          in
          match validation with
          | ( _
            , _
            , (`Delta_transition_chain, Truth.True delta_state_hashes)
            , _
            , _ ) ->
              let timeout_duration =
                Option.fold
                  (Transition_frontier.find frontier
                     (Non_empty_list.head delta_state_hashes))
                  ~init:(Block_time.Span.of_ms 0L)
                  ~f:(fun _ _ -> catchup_timeout_duration)
              in
              Catchup_scheduler.watch catchup_scheduler ~timeout_duration
                ~cached_transition:cached_initially_validated_transition ;
              return (Error ())
          | _ ->
              failwith
                "This is impossible since the transition just passed \
                 initial_validation so delta_transition_chain_proof must be \
                 true" )
    in
    (* TODO: only access parent in transition frontier once (already done in call to validate dependencies) #2485 *)
    let parent_hash =
      Protocol_state.previous_state_hash
        (External_transition.protocol_state transition)
    in
    let parent_breadcrumb =
      Transition_frontier.find_exn frontier parent_hash
    in
    let%bind breadcrumb =
      cached_transform_deferred_result cached_initially_validated_transition
        ~transform_cached:(fun _ ->
          Transition_frontier.Breadcrumb.build ~logger ~verifier ~trust_system
            ~sender:(Some sender) ~parent:parent_breadcrumb
            ~transition:mostly_validated_transition )
        ~transform_result:(function
          | Error (`Invalid_staged_ledger_hash error)
          | Error (`Invalid_staged_ledger_diff error) ->
              Logger.error logger ~module_:__MODULE__ ~location:__LOC__
                ~metadata:
                  (metadata @ [("error", `String (Error.to_string_hum error))])
                "Error while building breadcrumb in the transition handler \
                 processor: $error" ;
              let (_
                    : External_transition.Initial_validated.t
                      Envelope.Incoming.t) =
                Cached.invalidate_with_failure
                  cached_initially_validated_transition
              in
              Deferred.return (Error ())
          | Error (`Fatal_error exn) ->
              let (_
                    : External_transition.Initial_validated.t
                      Envelope.Incoming.t) =
                Cached.invalidate_with_failure
                  cached_initially_validated_transition
              in
              raise exn
          | Ok breadcrumb ->
              Deferred.return (Ok breadcrumb) )
    in
    Coda_metrics.(
      Counter.inc_one
        Transition_frontier_controller.breadcrumbs_built_by_processor) ;
    Deferred.map ~f:Result.return
      (add_and_finalize ~logger ~frontier ~catchup_scheduler
         ~processed_transition_writer ~only_if_present:false breadcrumb))

let run ~logger ~verifier ~trust_system ~time_controller ~frontier
    ~(primary_transition_reader :
       ( External_transition.Initial_validated.t Envelope.Incoming.t
       , State_hash.t )
       Cached.t
       Reader.t)
    ~(producer_transition_reader : Transition_frontier.Breadcrumb.t Reader.t)
    ~(clean_up_catchup_scheduler : unit Ivar.t)
    ~(catchup_job_writer :
       ( State_hash.t
         * ( External_transition.Initial_validated.t Envelope.Incoming.t
           , State_hash.t )
           Cached.t
           Rose_tree.t
           list
       , crash buffered
       , unit )
       Writer.t)
    ~(catchup_breadcrumbs_reader :
       ( (Transition_frontier.Breadcrumb.t, State_hash.t) Cached.t Rose_tree.t
         list
       * [`Ledger_catchup of unit Ivar.t | `Catchup_scheduler] )
       Reader.t)
    ~(catchup_breadcrumbs_writer :
       ( (Transition_frontier.Breadcrumb.t, State_hash.t) Cached.t Rose_tree.t
         list
         * [`Ledger_catchup of unit Ivar.t | `Catchup_scheduler]
       , crash buffered
       , unit )
       Writer.t) ~processed_transition_writer =
  let catchup_scheduler =
    Catchup_scheduler.create ~logger ~verifier ~trust_system ~frontier
      ~time_controller ~catchup_job_writer ~catchup_breadcrumbs_writer
      ~clean_up_signal:clean_up_catchup_scheduler
  in
  let add_and_finalize =
    add_and_finalize ~frontier ~catchup_scheduler ~processed_transition_writer
  in
  let process_transition =
    process_transition ~logger ~trust_system ~verifier ~frontier
      ~catchup_scheduler ~processed_transition_writer
  in
  ignore
    (Reader.Merge.iter
       (* It is fine to skip the cache layer on propose transitions because it
          * is extradornarily unlikely we would write an internal bug triggering this
          * case, and the external case (where we received an identical external
          * transition from the network) can happen iff there is another node
          * with the exact same private key and view of the transaction pool. *)
       [ Reader.map producer_transition_reader ~f:(fun breadcrumb ->
             Coda_metrics.(
               Gauge.inc_one
                 Transition_frontier_controller.transitions_being_processed) ;
             `Proposed_breadcrumb (Cached.pure breadcrumb) )
       ; Reader.map catchup_breadcrumbs_reader
           ~f:(fun (cb, catchup_breadcrumbs_callback) ->
             `Catchup_breadcrumbs (cb, catchup_breadcrumbs_callback) )
       ; Reader.map primary_transition_reader ~f:(fun vt ->
             `Partially_valid_transition vt ) ]
       ~f:(fun msg ->
         let open Deferred.Let_syntax in
         trace_recurring "transition_handler_processor" (fun () ->
             match msg with
             | `Catchup_breadcrumbs
                 (breadcrumb_subtrees, subsequent_callback_action) -> (
                 ( match%map
                     Deferred.Or_error.List.iter breadcrumb_subtrees
                       ~f:(fun subtree ->
                         Rose_tree.Deferred.Or_error.iter
                           subtree
                           (* It could be the case that by the time we try and
                           * add the breadcrumb, it's no longer relevant when
                           * we're catching up *)
                           ~f:(add_and_finalize ~logger ~only_if_present:true)
                     )
                   with
                 | Ok () ->
                     ()
                 | Error err ->
                     List.iter breadcrumb_subtrees ~f:(fun tree ->
                         Rose_tree.iter tree ~f:(fun cached_breadcrumb ->
                             let (_ : Transition_frontier.Breadcrumb.t) =
                               Cached.invalidate_with_failure cached_breadcrumb
                             in
                             () ) ) ;
                     Logger.error logger ~module_:__MODULE__ ~location:__LOC__
                       "Error, failed to attach all catchup breadcrumbs to \
                        transition frontier: %s"
                       (Error.to_string_hum err) )
                 >>| fun () ->
                 match subsequent_callback_action with
                 | `Ledger_catchup decrement_signal ->
                     Ivar.fill decrement_signal ()
                 | `Catchup_scheduler ->
                     () )
             | `Proposed_breadcrumb breadcrumb ->
                 let transition_time =
                   Transition_frontier.Breadcrumb.validated_transition
                     (Cached.peek breadcrumb)
                   |> External_transition.Validated.protocol_state
                   |> Protocol_state.blockchain_state
                   |> Blockchain_state.timestamp |> Block_time.to_time
                 in
                 Perf_histograms.add_span
                   ~name:"accepted_transition_local_latency"
                   (Core_kernel.Time.diff
                      Block_time.(now time_controller |> to_time)
                      transition_time) ;
                 let%map () =
                   match%map
                     add_and_finalize ~logger ~only_if_present:false breadcrumb
                   with
                   | Ok () ->
                       ()
                   | Error err ->
                       Logger.error logger ~module_:__MODULE__
                         ~location:__LOC__
                         ~metadata:
                           [("error", `String (Error.to_string_hum err))]
                         "Error, failed to attach proposed breadcrumb to \
                          transition frontier: $error" ;
                       let (_ : Transition_frontier.Breadcrumb.t) =
                         Cached.invalidate_with_failure breadcrumb
                       in
                       ()
                 in
                 Coda_metrics.(
                   Gauge.dec_one
                     Transition_frontier_controller.transitions_being_processed)
             | `Partially_valid_transition transition ->
                 process_transition ~transition ) ))

let%test_module "Transition_handler.Processor tests" =
  ( module struct
    open Async
    open Pipe_lib

    let () =
      Backtrace.elide := false ;
      Printexc.record_backtrace true ;
      Async.Scheduler.set_record_backtraces true

    let logger = Logger.null ()

    let time_controller = Block_time.Controller.basic ~logger

    let trust_system = Trust_system.null ()

    let downcast_breadcrumb breadcrumb =
      let transition =
        Transition_frontier.Breadcrumb.validated_transition breadcrumb
        |> External_transition.Validation
           .reset_frontier_dependencies_validation
        |> External_transition.Validation.reset_staged_ledger_diff_validation
      in
      Envelope.Incoming.wrap ~data:transition ~sender:Envelope.Sender.Local

    let%test_unit "adding transitions whose parents are in the frontier" =
      let frontier_size = 1 in
      let branch_size = 10 in
      let max_length = frontier_size + branch_size in
      Quickcheck.test ~trials:4
        (Transition_frontier.For_tests.gen_with_branch ~max_length
           ~frontier_size ~branch_size ()) ~f:(fun (frontier, branch) ->
          assert (
            Thread_safe.block_on_async_exn (fun () ->
                let pids = Child_processes.Termination.create_pid_table () in
                let%bind verifier =
                  Verifier.create ~logger ~conf_dir:None ~pids
                in
                let valid_transition_reader, valid_transition_writer =
                  Strict_pipe.create
                    (Buffered (`Capacity branch_size, `Overflow Drop_head))
                in
                let producer_transition_reader, _ =
                  Strict_pipe.create
                    (Buffered (`Capacity branch_size, `Overflow Drop_head))
                in
                let _, catchup_job_writer =
                  Strict_pipe.create (Buffered (`Capacity 1, `Overflow Crash))
                in
                let catchup_breadcrumbs_reader, catchup_breadcrumbs_writer =
                  Strict_pipe.create (Buffered (`Capacity 1, `Overflow Crash))
                in
                let processed_transition_reader, processed_transition_writer =
                  Strict_pipe.create
                    (Buffered (`Capacity branch_size, `Overflow Drop_head))
                in
                let clean_up_catchup_scheduler = Ivar.create () in
                let cache = Unprocessed_transition_cache.create ~logger in
                run ~logger ~time_controller ~verifier ~trust_system
                  ~clean_up_catchup_scheduler ~frontier
                  ~primary_transition_reader:valid_transition_reader
                  ~producer_transition_reader ~catchup_job_writer
                  ~catchup_breadcrumbs_reader ~catchup_breadcrumbs_writer
                  ~processed_transition_writer ;
                List.iter branch ~f:(fun breadcrumb ->
                    downcast_breadcrumb breadcrumb
                    |> Unprocessed_transition_cache.register_exn cache
                    |> Strict_pipe.Writer.write valid_transition_writer ) ;
                match%map
                  Block_time.Timeout.await
                    ~timeout_duration:(Block_time.Span.of_ms 5000L)
                    time_controller
                    (Strict_pipe.Reader.fold_until processed_transition_reader
                       ~init:branch
                       ~f:(fun remaining_breadcrumbs newly_added_transition ->
                         Deferred.return
                           ( match remaining_breadcrumbs with
                           | next_expected_breadcrumb :: tail ->
                               [%test_eq: State_hash.t]
                                 (Transition_frontier.Breadcrumb.state_hash
                                    next_expected_breadcrumb)
                                 (External_transition.Validated.state_hash
                                    newly_added_transition) ;
                               if tail = [] then `Stop true else `Continue tail
                           | [] ->
                               `Stop false ) ))
                with
                | `Timeout ->
                    failwith "test timed out"
                | `Ok (`Eof _) ->
                    failwith "pipe closed unexpectedly"
                | `Ok (`Terminated x) ->
                    x ) ) )
  end )
