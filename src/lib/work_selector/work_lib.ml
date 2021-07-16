open Core_kernel
open Currency
open Async

module Make (Inputs : Intf.Inputs_intf) = struct
  module Work_spec = Snark_work_lib.Work.Single.Spec

  module Job_status = struct
    type t = Assigned of Time.t

    let is_old (Assigned at_time) ~now ~reassignment_wait =
      let max_age = Time.Span.of_ms (Float.of_int reassignment_wait) in
      let delta = Time.diff now at_time in
      Time.Span.( > ) delta max_age
  end

  module State = struct
    module Seen_key = struct
      module T = struct
        type t = Transaction_snark.Statement.t One_or_two.t
        [@@deriving compare, sexp, to_yojson, hash]
      end

      include T
      include Comparable.Make (T)
    end

    type t =
      { mutable available_jobs :
          (Inputs.Transaction_witness.t, Inputs.Ledger_proof.t) Work_spec.t
          One_or_two.t
          list
      ; jobs_seen : (Seen_key.t, Job_status.t) Hashtbl.t
      ; reassignment_wait : int
      }

    let init :
           reassignment_wait:int
        -> frontier_broadcast_pipe:
             Inputs.Transition_frontier.t option
             Pipe_lib.Broadcast_pipe.Reader.t
        -> logger:Logger.t
        -> t =
     fun ~reassignment_wait ~frontier_broadcast_pipe ~logger ->
      let t =
        { available_jobs = []
        ; jobs_seen = Hashtbl.create (module Seen_key)
        ; reassignment_wait
        }
      in
      Pipe_lib.Broadcast_pipe.Reader.iter frontier_broadcast_pipe
        ~f:(fun frontier_opt ->
          ( match frontier_opt with
          | None ->
              [%log debug] "No frontier, setting available work to be empty" ;
              t.available_jobs <- []
          | Some frontier ->
              Pipe_lib.Broadcast_pipe.Reader.iter
                (Inputs.Transition_frontier.best_tip_pipe frontier) ~f:(fun _ ->
                  let best_tip_staged_ledger =
                    Inputs.Transition_frontier.best_tip_staged_ledger frontier
                  in
                  let start_time = Time.now () in
                  ( match
                      Inputs.Staged_ledger.all_work_pairs best_tip_staged_ledger
                        ~get_state:
                          (Inputs.Transition_frontier.get_protocol_state
                             frontier)
                    with
                  | Error e ->
                      [%log fatal]
                        "Error occured when updating available work: $error"
                        ~metadata:[ ("error", Error_json.error_to_yojson e) ]
                  | Ok new_available_jobs ->
                      let end_time = Time.now () in
                      [%log info] "Updating new available work took $time ms"
                        ~metadata:
                          [ ( "time"
                            , `Float
                                ( Time.diff end_time start_time
                                |> Time.Span.to_ms ) )
                          ] ;
                      t.available_jobs <- new_available_jobs ) ;
                  Deferred.unit)
              |> Deferred.don't_wait_for ) ;
          Deferred.unit)
      |> Deferred.don't_wait_for ;
      t

    let all_unseen_works t =
      List.filter t.available_jobs ~f:(fun js ->
          not
          @@ Hashtbl.mem t.jobs_seen (One_or_two.map ~f:Work_spec.statement js))

    let remove_old_assignments t ~logger =
      let now = Time.now () in
      Hashtbl.filteri_inplace t.jobs_seen ~f:(fun ~key:work ~data:status ->
          if
            Job_status.is_old status ~now ~reassignment_wait:t.reassignment_wait
          then (
            [%log info]
              ~metadata:[ ("work", Seen_key.to_yojson work) ]
              "Waited too long to get work for $work. Ready to be reassigned" ;
            Mina_metrics.(Counter.inc_one Snark_work.snark_work_timed_out_rpc) ;
            false )
          else true)

    let remove t x =
      Hashtbl.remove t.jobs_seen (One_or_two.map ~f:Work_spec.statement x)

    let set t x =
      Hashtbl.set t.jobs_seen
        ~key:(One_or_two.map ~f:Work_spec.statement x)
        ~data:(Job_status.Assigned (Time.now ()))
  end

  let does_not_have_better_fee ~snark_pool ~fee
      (statements : Inputs.Transaction_snark_work.Statement.t) : bool =
    Option.value_map ~default:true
      (Inputs.Snark_pool.get_completed_work snark_pool statements)
      ~f:(fun priced_proof ->
        let competing_fee = Inputs.Transaction_snark_work.fee priced_proof in
        Fee.compare fee competing_fee < 0)

  module For_tests = struct
    let does_not_have_better_fee = does_not_have_better_fee
  end

  let get_expensive_work ~snark_pool ~fee
      (jobs : ('a, 'b) Work_spec.t One_or_two.t list) :
      ('a, 'b) Work_spec.t One_or_two.t list =
    List.filter jobs ~f:(fun job ->
        does_not_have_better_fee ~snark_pool ~fee
          (One_or_two.map job ~f:Work_spec.statement))

  let all_pending_work ~snark_pool statements =
    List.filter statements ~f:(fun st ->
        Option.is_none (Inputs.Snark_pool.get_completed_work snark_pool st))

  (*Seen/Unseen jobs that are not in the snark pool yet*)
  let pending_work_statements ~snark_pool ~fee_opt (state : State.t) =
    let all_todo_statements =
      List.map state.available_jobs ~f:(One_or_two.map ~f:Work_spec.statement)
    in
    let expensive_work statements ~fee =
      List.filter statements ~f:(does_not_have_better_fee ~snark_pool ~fee)
    in
    match fee_opt with
    | None ->
        all_pending_work ~snark_pool all_todo_statements
    | Some fee ->
        expensive_work all_todo_statements ~fee
end
