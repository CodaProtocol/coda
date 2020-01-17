open Core_kernel
open Async_kernel
open Pipe_lib
open O1trace

let run ~logger ~trust_system ~verifier ~network ~time_controller
    ~collected_transitions ~frontier ~network_transition_reader
    ~proposer_transition_reader ~clear_reader =
  let valid_transition_pipe_capacity = 30 in
  let valid_transition_reader, valid_transition_writer =
    Strict_pipe.create ~name:"valid transitions"
      (Buffered (`Capacity valid_transition_pipe_capacity, `Overflow Crash))
  in
  let primary_transition_pipe_capacity =
    valid_transition_pipe_capacity + List.length collected_transitions
  in
  let primary_transition_reader, primary_transition_writer =
    Strict_pipe.create ~name:"primary transitions"
      (Buffered (`Capacity primary_transition_pipe_capacity, `Overflow Crash))
  in
  let processed_transition_reader, processed_transition_writer =
    Strict_pipe.create ~name:"processed transitions"
      (Buffered (`Capacity 30, `Overflow Crash))
  in
  let catchup_job_reader, catchup_job_writer =
    Strict_pipe.create ~name:"catchup jobs"
      (Buffered (`Capacity 30, `Overflow Crash))
  in
  let catchup_breadcrumbs_reader, catchup_breadcrumbs_writer =
    Strict_pipe.create ~name:"catchup breadcrumbs"
      (Buffered (`Capacity 30, `Overflow Crash))
  in
  let proposer_transition_reader_copy, proposer_transition_writer_copy =
    Strict_pipe.create ~name:"block producer transition copy" Synchronous
  in
  Strict_pipe.transfer_while_writer_alive proposer_transition_reader
    proposer_transition_writer_copy ~f:(fun new_breadcrumb ->
      Coda_networking.broadcast_state network
        ( Transition_frontier.Breadcrumb.validated_transition new_breadcrumb
        |> Coda_transition.External_transition.Validation.forget_validation ) ;
      new_breadcrumb )
  |> don't_wait_for ;
  let unprocessed_transition_cache =
    Transition_handler.Unprocessed_transition_cache.create ~logger
  in
  List.iter collected_transitions ~f:(fun t ->
      (* since the cache was just built, it's safe to assume
       * registering these will not fail, so long as there
       * are no duplicates in the list *)
      Transition_handler.Unprocessed_transition_cache.register_exn
        unprocessed_transition_cache t
      |> Strict_pipe.Writer.write primary_transition_writer ) ;
  trace_recurring "validator" (fun () ->
      Transition_handler.Validator.run ~logger ~trust_system ~time_controller
        ~frontier ~transition_reader:network_transition_reader
        ~valid_transition_writer ~unprocessed_transition_cache ) ;
  Strict_pipe.Reader.iter_without_pushback valid_transition_reader
    ~f:(Strict_pipe.Writer.write primary_transition_writer)
  |> don't_wait_for ;
  let clean_up_catchup_scheduler = Ivar.create () in
  trace_recurring "processor" (fun () ->
      Transition_handler.Processor.run ~logger ~time_controller ~trust_system
        ~verifier ~frontier ~primary_transition_reader
        ~proposer_transition_reader:proposer_transition_reader_copy
        ~clean_up_catchup_scheduler ~catchup_job_writer
        ~catchup_breadcrumbs_reader ~catchup_breadcrumbs_writer
        ~processed_transition_writer ) ;
  trace_recurring "catchup" (fun () ->
      Ledger_catchup.run ~logger ~trust_system ~verifier ~network ~frontier
        ~catchup_job_reader ~catchup_breadcrumbs_writer
        ~unprocessed_transition_cache ) ;
  Strict_pipe.Reader.iter_without_pushback clear_reader ~f:(fun _ ->
      let open Strict_pipe.Writer in
      kill valid_transition_writer ;
      kill primary_transition_writer ;
      kill processed_transition_writer ;
      kill catchup_job_writer ;
      kill catchup_breadcrumbs_writer ;
      kill proposer_transition_writer_copy ;
      Ivar.fill clean_up_catchup_scheduler () )
  |> don't_wait_for ;
  processed_transition_reader
