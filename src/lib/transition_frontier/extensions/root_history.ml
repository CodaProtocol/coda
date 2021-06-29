open Core_kernel
open Mina_base
open Mina_transition
open Frontier_base
module Queue = Hash_queue.Make (State_hash)

module T = struct
  type t =
    { history: Root_data.Historical.t Queue.t
    ; capacity: int
    ; mutable current_root: Root_data.Historical.t
    ; mutable protocol_states_for_root_scan_state:
        Full_frontier.Protocol_states_for_root_scan_state.t }

  type view = t

  let create ~logger:_ frontier =
    let capacity = 2 * Full_frontier.max_length frontier in
    let history = Queue.create () in
    let current_root =
      Root_data.Historical.of_breadcrumb (Full_frontier.root frontier)
    in
    let t =
      { history
      ; capacity
      ; current_root
      ; protocol_states_for_root_scan_state=
          Full_frontier.protocol_states_for_root_scan_state frontier }
    in
    (t, t)

  let enqueue t new_root =
    let open Root_data.Historical in
    ( if Queue.length t.history >= t.capacity then
      let oldest_root = Queue.dequeue_front_exn t.history in
      (*Update the protocol states required for scan state at the new root*)
      let _new_oldest_hash, new_oldest_root =
        Queue.first_with_key t.history |> Option.value_exn
      in
      let new_protocol_states_map =
        Full_frontier.Protocol_states_for_root_scan_state
        .protocol_states_for_next_root_scan_state
          t.protocol_states_for_root_scan_state
          ~new_scan_state:(scan_state new_oldest_root)
          ~old_root_state:
            { With_hash.data=
                External_transition.Validated.protocol_state
                  (transition oldest_root)
            ; hash=
                External_transition.Validated.state_hash
                  (transition oldest_root) }
        |> State_hash.Map.of_alist_exn
      in
      t.protocol_states_for_root_scan_state <- new_protocol_states_map ) ;
    assert (
      [%equal: [`Ok | `Key_already_present]] `Ok
        (Queue.enqueue_back t.history
           (External_transition.Validated.state_hash
              (transition t.current_root))
           t.current_root) ) ;
    t.current_root <- new_root

  let handle_diffs root_history frontier diffs_with_mutants =
    let open Diff.Full.With_mutant in
    let should_produce_view =
      List.exists diffs_with_mutants ~f:(function
        (* TODO: send full diffs to extensions to avoid extra lookups in frontier *)
        | E (Root_transitioned {new_root; _}, _) ->
            Full_frontier.find_exn frontier (Root_data.Limited.hash new_root)
            |> Root_data.Historical.of_breadcrumb |> enqueue root_history ;
            true
        | E _ ->
            false )
    in
    Option.some_if should_produce_view root_history
end

include T
module Broadcasted = Functor.Make_broadcasted (T)

let lookup {history; _} = Queue.lookup history

let mem {history; _} = Queue.mem history

let protocol_states_for_scan_state
    {history; protocol_states_for_root_scan_state; _} state_hash =
  let open Option.Let_syntax in
  let open Root_data.Historical in
  let%bind data = Queue.lookup history state_hash in
  let required_state_hashes =
    Staged_ledger.Scan_state.required_state_hashes (scan_state data)
    |> State_hash.Set.to_list
  in
  List.fold_until ~init:[]
    ~finish:(fun lst -> Some lst)
    required_state_hashes
    ~f:(fun acc hash ->
      let res =
        match Queue.lookup history hash with
        | Some data ->
            Some
              (External_transition.Validated.protocol_state (transition data))
        | None ->
            (*Not present in the history queue, check in the protocol states map that has all the protocol states required for transactions in the root*)
            State_hash.Map.find protocol_states_for_root_scan_state hash
      in
      match res with None -> Stop None | Some state -> Continue (state :: acc)
      )

let most_recent {history; _} =
  (* unfortunately, there is not function to inspect the last element in the queue,
   * so we need to remove it and reinsert it instead *)
  let open Option.Let_syntax in
  let%map state_hash, breadcrumb = Queue.dequeue_back_with_key history in
  (* should never return `Key_already_present since we just removed it *)
  assert (
    [%equal: [`Ok | `Key_already_present]] `Ok
      (Queue.enqueue_back history state_hash breadcrumb) ) ;
  breadcrumb

let oldest {history; _} = Queue.first history

let is_empty {history; _} = Queue.is_empty history

let to_list {history; _} = Queue.to_list history
