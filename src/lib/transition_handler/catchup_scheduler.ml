(**
 * [Catchup_scheduler] defines a process which schedules catchup jobs and
 * monitors them for invalidation. This allows the transition frontier
 * controller to handle out of order transitions without spinning up
 * and tearing down catchup jobs constantly. The [Catchup_scheduler] must
 * receive notifications whenever a new transition is added to the
 * transition frontier so that it can determine if any pending catchup
 * jobs can be invalidated. When catchup jobs are invalidated, the
 * catchup scheduler extracts all of the invalidated catchup jobs and
 * spins up a process to materialize breadcrumbs from those transitions,
 * which will write the breadcrumbs back into the processor as if
 * catchup had successfully completed.
 *)

open Core_kernel
open Async_kernel
open Pipe_lib.Strict_pipe
open Otp_lib
open Coda_base

module Make (Inputs : Inputs.S) = struct
  open Inputs
  open Consensus

  type t =
    { logger: Logger.t
    ; time_controller: Time.Controller.t
    ; catchup_job_writer:
        ( (External_transition.Verified.t, State_hash.t) With_hash.t
        , synchronous
        , unit Deferred.t )
        Writer.t
    ; collected_transitions:
        (External_transition.Verified.t, State_hash.t) With_hash.t list
        State_hash.Table.t
        (* The new implementation has this new hash table storing all seen
     * transitions. Comparing with the original implementation, this hash
     * table has the invariant that all the transitions would appear as
     * keys in the hash table. Note, in the original version, transition
     * would only appear as key if it has a non-empty list of children;
     * while now if we add a new transition without children into the hash
     * table, we would always add it with an empty list of child associated
     * to it. *)
    ; parent_root_timeouts: unit Time.Timeout.t State_hash.Table.t
    ; breadcrumb_builder_supervisor:
        (External_transition.Verified.t, State_hash.t) With_hash.t Rose_tree.t
        list
        Capped_supervisor.t }

  let create ~logger ~frontier ~time_controller ~catchup_job_writer
      ~catchup_breadcrumbs_writer =
    let logger = Logger.child logger "catchup_scheduler" in
    let collected_transitions = State_hash.Table.create () in
    let parent_root_timeouts = State_hash.Table.create () in
    let breadcrumb_builder_supervisor =
      Capped_supervisor.create ~job_capacity:5 (fun transition_branches ->
          let%bind breadcrumbs =
            Deferred.List.map transition_branches ~f:(fun branch ->
                let (Rose_tree.T (branch_base, _)) = branch in
                let branch_parent_hash =
                  With_hash.data branch_base
                  |> External_transition.Verified.protocol_state
                  |> Protocol_state.previous_state_hash
                in
                let branch_parent =
                  Transition_frontier.find_exn frontier branch_parent_hash
                in
                Rose_tree.Deferred.fold_map branch ~init:branch_parent
                  ~f:(fun parent transition_with_hash ->
                    match%map
                      Transition_frontier.Breadcrumb.build ~logger ~parent
                        ~transition_with_hash
                    with
                    | Error (`Validation_error e) ->
                        (*TODO: Punish*) Error.raise e
                    | Error (`Fatal_error e) -> raise e
                    | Ok breadcrumb -> breadcrumb ) )
          in
          Writer.write catchup_breadcrumbs_writer breadcrumbs )
    in
    { logger
    ; collected_transitions
    ; time_controller
    ; catchup_job_writer
    ; parent_root_timeouts
    ; breadcrumb_builder_supervisor }

  let cancel_timeout t hash =
    Hashtbl.find_and_call t.parent_root_timeouts hash
      ~if_found:(fun timeout ->
        Time.Timeout.cancel t.time_controller timeout () )
      ~if_not_found:(Fn.const ()) ;
    Hashtbl.remove t.parent_root_timeouts hash

  let cancel_child_timeout t parent_hash =
    match Hashtbl.find t.collected_transitions parent_hash with
    | None -> ()
    | Some children ->
        List.iter children ~f:(fun child ->
            cancel_timeout t (With_hash.hash child) )

  let watch t ~timeout_duration ~transition =
    let hash = With_hash.hash transition in
    let parent_hash =
      With_hash.data transition |> External_transition.Verified.protocol_state
      |> Protocol_state.previous_state_hash
    in
    let make_timeout () =
      Time.Timeout.create t.time_controller timeout_duration ~f:(fun _ ->
          don't_wait_for (Writer.write t.catchup_job_writer transition) )
    in
    Hashtbl.update t.collected_transitions parent_hash ~f:(function
      | None ->
          cancel_child_timeout t hash ;
          Hashtbl.add_exn t.collected_transitions ~key:hash ~data:[] ;
          Hashtbl.add_exn t.parent_root_timeouts ~key:parent_hash
            ~data:(make_timeout ()) ;
          [transition]
      | Some sibling_transitions ->
          if
            List.exists sibling_transitions ~f:(fun collected_transition ->
                State_hash.equal hash @@ With_hash.hash collected_transition )
          then (
            Logger.info t.logger
              !"Received request to watch transition for catchup that already \
                was being watched: %{sexp: State_hash.t}"
              hash ;
            sibling_transitions )
          else (
            cancel_child_timeout t hash ;
            Hashtbl.add_exn t.collected_transitions ~key:hash ~data:[] ;
            transition :: sibling_transitions ) )

  let rec extract t transition =
    let successors =
      Option.value ~default:[]
        (Hashtbl.find t.collected_transitions (With_hash.hash transition))
    in
    Rose_tree.T (transition, List.map successors ~f:(extract t))

  let rec remove_tree t parent_hash =
    let children =
      Option.value ~default:[]
        (Hashtbl.find t.collected_transitions parent_hash)
    in
    Hashtbl.remove t.collected_transitions parent_hash ;
    List.iter children ~f:(Fn.compose (remove_tree t) With_hash.hash)

  let notify t ~transition =
    let hash = With_hash.hash transition in
    cancel_timeout t hash ;
    Option.iter (Hashtbl.find t.collected_transitions hash)
      ~f:(fun collected_transitions ->
        let transition_branches =
          List.map collected_transitions ~f:(extract t)
        in
        Capped_supervisor.dispatch t.breadcrumb_builder_supervisor
          transition_branches ) ;
    remove_tree t hash
end
