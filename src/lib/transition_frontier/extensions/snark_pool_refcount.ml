open Core_kernel
open Frontier_base
module Work = Transaction_snark_work.Statement

module T = struct
  type t = int Work.Table.t

  type view = int * int Work.Table.t

  let get_work = Staged_ledger.Scan_state.all_work_statements_exn

  (** Returns true if this update changed which elements are in the table
      (but not if the same elements exist with a different reference count) *)
  let add_scan_state_to_ref_table table scan_state : bool =
    List.fold ~init:false (get_work scan_state) ~f:(fun acc work ->
        match Work.Table.find table work with
        | Some count ->
            Work.Table.set table ~key:work ~data:(count + 1) ;
            acc
        | None ->
            Work.Table.set table ~key:work ~data:1 ;
            true )

  (** Returns true if this update changed which elements are in the table
      (but not if the same elements exist with a different reference count) *)
  let remove_scan_state_from_ref_table table scan_state : bool =
    List.fold (get_work scan_state) ~init:false ~f:(fun acc work ->
        match Work.Table.find table work with
        | Some 1 ->
            Work.Table.remove table work ;
            true
        | Some v ->
            Work.Table.set table ~key:work ~data:(v - 1) ;
            acc
        | None ->
            failwith "Removed a breadcrumb we didn't know about" )

  let create ~logger:_ frontier =
    let t = Work.Table.create () in
    let (_ : bool) =
      Full_frontier.root frontier
      |> Breadcrumb.staged_ledger |> Staged_ledger.scan_state
      |> add_scan_state_to_ref_table t
    in
    (t, (0, t))

  type diff_update = {num_removed: int; is_added: bool}

  let handle_diffs t _frontier diffs_with_mutants =
    let open Diff.Full.With_mutant in
    let {num_removed; is_added} =
      List.fold diffs_with_mutants ~init:{num_removed= 0; is_added= false}
        ~f:(fun ({num_removed; is_added} as init) -> function
        | E (New_node (Full breadcrumb), _) ->
            let scan_state =
              Breadcrumb.staged_ledger breadcrumb |> Staged_ledger.scan_state
            in
            { num_removed
            ; is_added= is_added || add_scan_state_to_ref_table t scan_state }
        | E (Root_transitioned {new_root= _; garbage= Full garbage_nodes; _}, _)
          ->
            let open Diff.Node_list in
            let extra_num_removed =
              List.fold garbage_nodes ~init:0 ~f:(fun acc node ->
                  let delta =
                    if remove_scan_state_from_ref_table t node.scan_state then
                      1
                    else 0
                  in
                  acc + delta )
            in
            {num_removed= num_removed + extra_num_removed; is_added}
        | E (Best_tip_changed _, _) ->
            init )
    in
    if num_removed > 0 || is_added then Some (num_removed, t) else None
end

include T
module Broadcasted = Functor.Make_broadcasted (T)
