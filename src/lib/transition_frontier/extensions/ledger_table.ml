open Core_kernel
open Mina_base
open Frontier_base

(* WARNING: don't use this code until @nholland has landed a PR that
   synchronize the read/write of transition frontier
 *)

module T = struct
  (* a pair of hash tables
     the first maps ledger hashes to ledgers
     the second maps ledger hashes to reference counts
   *)
  type t =
    {ledgers: Ledger.t Ledger_hash.Table.t; counts: int Ledger_hash.Table.t}

  type view = unit

  let add_entry t ~ledger_hash ~ledger =
    (* add ledger, increment ref count *)
    ignore
      (Hashtbl.add t.ledgers ~key:ledger_hash ~data:ledger : [`Duplicate | `Ok]) ;
    ignore (Hashtbl.incr t.counts ledger_hash : view)

  let remove_entry t ~ledger_hash =
    (* decrement ref count, remove ledger if count is 0 *)
    Hashtbl.decr t.counts ledger_hash ~remove_if_zero:true ;
    if not (Hashtbl.mem t.counts ledger_hash) then
      Hashtbl.remove t.ledgers ledger_hash

  let create ~logger:_ frontier =
    (* populate ledger table from breadcrumbs *)
    let t =
      { ledgers= Ledger_hash.Table.create ()
      ; counts= Ledger_hash.Table.create () }
    in
    let breadcrumbs = Full_frontier.all_breadcrumbs frontier in
    List.iter breadcrumbs ~f:(fun bc ->
        let ledger = Staged_ledger.ledger @@ Breadcrumb.staged_ledger bc in
        let ledger_hash = Ledger.merkle_root ledger in
        add_entry t ~ledger_hash ~ledger ) ;
    (t, ())

  let lookup t ledger_hash = Ledger_hash.Table.find t.ledgers ledger_hash

  let handle_diffs t _frontier diffs_with_mutants =
    let open Diff.Full.With_mutant in
    List.iter diffs_with_mutants ~f:(function
      | E (New_node (Full breadcrumb), _) ->
          let ledger =
            Staged_ledger.ledger @@ Breadcrumb.staged_ledger breadcrumb
          in
          let ledger_hash = Ledger.merkle_root ledger in
          add_entry t ~ledger_hash ~ledger
      | E (Root_transitioned transition, _) -> (
        match transition.garbage with
        | Full nodes ->
            let open Mina_state in
            List.iter nodes ~f:(fun node ->
                let With_hash.{data= external_transition; _}, _ =
                  node.transition
                in
                let blockchain_state =
                  Protocol_state.blockchain_state
                  @@ Mina_transition.External_transition.protocol_state
                       external_transition
                in
                let staged_ledger =
                  Blockchain_state.staged_ledger_hash blockchain_state
                in
                let ledger_hash =
                  Staged_ledger_hash.ledger_hash staged_ledger
                in
                remove_entry t ~ledger_hash ) )
      | E (Best_tip_changed _, _) ->
          () ) ;
    None
end

module Broadcasted = Functor.Make_broadcasted (T)
include T
