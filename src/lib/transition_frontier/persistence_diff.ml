(** A transition frontier extension that exposes the changes in the transactions
    in the best tip. *)

open Core
open Coda_base
open Protocols.Coda_pow

module type Inputs_intf = sig
  include Inputs.Inputs_intf

  module Breadcrumb :
    Transition_frontier_Breadcrumb_intf
    with type state_hash := State_hash.t
     and type staged_ledger := Staged_ledger.t
     and type external_transition_verified := External_transition.Verified.t
     and type user_command := User_command.t
end

module Make (Inputs : Inputs_intf) :
  Transition_frontier_extension_intf0
  with type transition_frontier_breadcrumb := Inputs.Breadcrumb.t
   and type input = unit
   and type view = Inputs.Diff_mutant.e list = struct
  open Inputs

  type t = unit

  type input = unit

  type view = Diff_mutant.e list

  let create () = ()

  let initial_view () = []

  let get_transition breadcrumb =
    let {With_hash.data= external_transition; hash} =
      Breadcrumb.transition_with_hash breadcrumb
    in
    {With_hash.data= External_transition.of_verified external_transition; hash}

  let scan_state breadcrumb =
    breadcrumb |> Breadcrumb.staged_ledger |> Staged_ledger.scan_state

  let pending_coinbase breadcrumb =
    breadcrumb |> Breadcrumb.staged_ledger
    |> Staged_ledger.pending_coinbase_collection

  let handle_diff () (diff : Breadcrumb.t Transition_frontier_diff.t) :
      view option =
    let open Transition_frontier_diff in
    let open Diff_mutant in
    Option.return
    @@
    match diff with
    | New_frontier breadcrumb ->
        [ E
            (New_frontier
               ( get_transition breadcrumb
               , scan_state breadcrumb
               , pending_coinbase breadcrumb )) ]
    | New_breadcrumb breadcrumb ->
        [E (Add_transition (get_transition breadcrumb))]
    | New_best_tip {garbage; added_to_best_tip_path; new_root; old_root; _} ->
        let added_transition =
          E
            (Add_transition
               (Non_empty_list.last added_to_best_tip_path |> get_transition))
        in
        let remove_transition =
          E (Remove_transitions (List.map garbage ~f:get_transition))
        in
        if
          State_hash.equal
            (Breadcrumb.state_hash old_root)
            (Breadcrumb.state_hash new_root)
        then [added_transition; remove_transition]
        else
          [ added_transition
          ; E
              (Update_root
                 ( Breadcrumb.state_hash new_root
                 , scan_state new_root
                 , pending_coinbase new_root ))
          ; remove_transition ]
end
