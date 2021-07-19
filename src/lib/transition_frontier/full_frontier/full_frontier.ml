open Core_kernel
open Mina_base
open Mina_state
open Mina_transition
open Frontier_base

module Node = struct
  type t =
    {breadcrumb: Breadcrumb.t; successor_hashes: State_hash.t list; length: int}
  [@@deriving sexp, fields]

  type display =
    { length: int
    ; state_hash: string
    ; blockchain_state: Blockchain_state.display
    ; consensus_state: Consensus.Data.Consensus_state.display }
  [@@deriving yojson]

  let equal node1 node2 = Breadcrumb.equal node1.breadcrumb node2.breadcrumb

  let hash node = Breadcrumb.hash node.breadcrumb

  let compare node1 node2 =
    Breadcrumb.compare node1.breadcrumb node2.breadcrumb

  let name t = Breadcrumb.name t.breadcrumb

  let display t =
    let {Breadcrumb.state_hash; consensus_state; blockchain_state; _} =
      Breadcrumb.display t.breadcrumb
    in
    {state_hash; blockchain_state; length= t.length; consensus_state}
end

module Protocol_states_for_root_scan_state = struct
  type t = Protocol_state.value State_hash.Map.t

  let protocol_states_for_next_root_scan_state protocol_states_for_old_root
      ~new_scan_state
      ~(old_root_state : (Protocol_state.value, State_hash.t) With_hash.t) =
    let required_state_hashes =
      Staged_ledger.Scan_state.required_state_hashes new_scan_state
      |> State_hash.Set.to_list
    in
    let protocol_state_map =
      (*Note: Protocol states for the next root should all be in this map
      assuming roots transition to their successors and do not skip any node in
      between*)
      State_hash.Map.set protocol_states_for_old_root ~key:old_root_state.hash
        ~data:old_root_state.data
    in
    List.map required_state_hashes ~f:(fun hash ->
        (hash, State_hash.Map.find_exn protocol_state_map hash) )
end

(* Invariant: The path from the root to the tip inclusively, will be max_length *)
type t =
  { root_ledger: Ledger.Any_ledger.witness
  ; mutable root: State_hash.t
  ; mutable best_tip: State_hash.t
  ; logger: Logger.t
  ; table: Node.t State_hash.Table.t
  ; mutable protocol_states_for_root_scan_state:
      Protocol_states_for_root_scan_state.t
  ; consensus_local_state: Consensus.Data.Local_state.t
  ; max_length: int
  ; precomputed_values: Precomputed_values.t
  ; time_controller: Block_time.Controller.t
  ; persistent_root_instance: Persistent_root.Instance.t }

let persistent_root_instance {persistent_root_instance; _} =
  persistent_root_instance

let consensus_local_state {consensus_local_state; _} = consensus_local_state

let all_breadcrumbs t =
  List.map (Hashtbl.data t.table) ~f:(fun {breadcrumb; _} -> breadcrumb)

let find t hash =
  let open Option.Let_syntax in
  let%map node = Hashtbl.find t.table hash in
  node.breadcrumb

let find_exn t hash =
  let node = Hashtbl.find_exn t.table hash in
  node.breadcrumb

let find_protocol_state (t : t) hash =
  match find t hash with
  | None ->
      State_hash.Map.find t.protocol_states_for_root_scan_state hash
  | Some breadcrumb ->
      Some
        ( Breadcrumb.validated_transition breadcrumb
        |> External_transition.Validated.protocol_state )

let root t = find_exn t t.root

let protocol_states_for_root_scan_state t =
  t.protocol_states_for_root_scan_state

let best_tip t = find_exn t t.best_tip

let close ~loc t =
  Mina_metrics.(Gauge.set Transition_frontier.active_breadcrumbs 0.0) ;
  ignore
    ( Ledger.Maskable.unregister_mask_exn ~loc ~grandchildren:`Recursive
        (Breadcrumb.mask (root t))
      : Ledger.unattached_mask )

let create ~logger ~root_data ~root_ledger ~consensus_local_state ~max_length
    ~precomputed_values ~persistent_root_instance ~time_controller =
  let open Root_data in
  let transition_receipt_time = None in
  let root_hash =
    External_transition.Validated.state_hash root_data.transition
  in
  let protocol_states_for_root_scan_state =
    State_hash.Map.of_alist_exn root_data.protocol_states
  in
  let root_protocol_state =
    External_transition.Validated.protocol_state root_data.transition
  in
  let root_blockchain_state =
    Protocol_state.blockchain_state root_protocol_state
  in
  let root_blockchain_state_ledger_hash =
    Blockchain_state.snarked_ledger_hash root_blockchain_state
  in
  assert (
    Frozen_ledger_hash.equal
      (Frozen_ledger_hash.of_ledger_hash
         (Ledger.Any_ledger.M.merkle_root root_ledger))
      root_blockchain_state_ledger_hash ) ;
  let root_breadcrumb =
    Breadcrumb.create ~validated_transition:root_data.transition
      ~staged_ledger:root_data.staged_ledger ~just_emitted_a_proof:false
      ~transition_receipt_time
  in
  let root_node =
    {Node.breadcrumb= root_breadcrumb; successor_hashes= []; length= 0}
  in
  let table = State_hash.Table.of_alist_exn [(root_hash, root_node)] in
  Mina_metrics.(Gauge.set Transition_frontier.active_breadcrumbs 1.0) ;
  { logger
  ; root_ledger
  ; root= root_hash
  ; best_tip= root_hash
  ; table
  ; consensus_local_state
  ; max_length
  ; precomputed_values
  ; protocol_states_for_root_scan_state
  ; persistent_root_instance
  ; time_controller }

let root_data t =
  let open Root_data in
  let root = root t in
  { transition= Breadcrumb.validated_transition root
  ; staged_ledger= Breadcrumb.staged_ledger root
  ; protocol_states=
      State_hash.Map.to_alist t.protocol_states_for_root_scan_state }

let max_length {max_length; _} = max_length

let root_length t = (Hashtbl.find_exn t.table t.root).length

let successor_hashes t hash =
  let node = Hashtbl.find_exn t.table hash in
  node.successor_hashes

let rec successor_hashes_rec t hash =
  List.bind (successor_hashes t hash) ~f:(fun succ_hash ->
      succ_hash :: successor_hashes_rec t succ_hash )

let successors t breadcrumb =
  List.map
    (successor_hashes t (Breadcrumb.state_hash breadcrumb))
    ~f:(find_exn t)

let rec successors_rec t breadcrumb =
  List.bind (successors t breadcrumb) ~f:(fun succ ->
      succ :: successors_rec t succ )

let path_map ?max_length t breadcrumb ~f =
  let rec find_path b count_opt acc =
    match count_opt with
    | Some count when count <= 0 ->
        acc
    | _ ->
        let count_opt = Option.map ~f:(fun x -> x - 1) count_opt in
        let elem = f b in
        let parent_hash = Breadcrumb.parent_hash b in
        if State_hash.equal (Breadcrumb.state_hash b) t.root then acc
        else if State_hash.equal parent_hash t.root then elem :: acc
        else find_path (find_exn t parent_hash) count_opt (elem :: acc)
  in
  find_path breadcrumb max_length []

let best_tip_path ?max_length t = path_map ?max_length t (best_tip t) ~f:Fn.id

let hash_path t breadcrumb = path_map t breadcrumb ~f:Breadcrumb.state_hash

let precomputed_values t = t.precomputed_values

let genesis_constants t = t.precomputed_values.genesis_constants

let iter t ~f = Hashtbl.iter t.table ~f:(fun n -> f n.breadcrumb)

let best_tip_path_length_exn {table; root; best_tip; _} =
  let open Option.Let_syntax in
  let result =
    let%bind best_tip_node = Hashtbl.find table best_tip in
    let%map root_node = Hashtbl.find table root in
    best_tip_node.length - root_node.length
  in
  result |> Option.value_exn

let common_ancestor t (bc1 : Breadcrumb.t) (bc2 : Breadcrumb.t) : State_hash.t
    =
  let rec go ancestors1 ancestors2 b1 b2 =
    let sh1 = Breadcrumb.state_hash b1 in
    let sh2 = Breadcrumb.state_hash b2 in
    Hash_set.add ancestors1 sh1 ;
    Hash_set.add ancestors2 sh2 ;
    if Hash_set.mem ancestors1 sh2 then sh2
    else if Hash_set.mem ancestors2 sh1 then sh1
    else
      let parent_unless_root breadcrumb =
        if State_hash.equal (Breadcrumb.state_hash breadcrumb) t.root then
          breadcrumb
        else find_exn t (Breadcrumb.parent_hash breadcrumb)
      in
      go ancestors1 ancestors2 (parent_unless_root b1) (parent_unless_root b2)
  in
  go
    (Hash_set.create (module State_hash))
    (Hash_set.create (module State_hash))
    bc1 bc2

(* TODO: separate visualizer? *)
(* Visualize the structure of the transition frontier or a particular node
 * within the frontier (for debugging purposes). *)
module Visualizor = struct
  let fold t ~f = Hashtbl.fold t.table ~f:(fun ~key:_ ~data -> f data)

  include Visualization.Make_ocamlgraph (Node)

  let to_graph t =
    fold t ~init:empty ~f:(fun (node : Node.t) graph ->
        let graph_with_node = add_vertex graph node in
        List.fold node.successor_hashes ~init:graph_with_node
          ~f:(fun acc_graph successor_state_hash ->
            match State_hash.Table.find t.table successor_state_hash with
            | Some child_node ->
                add_edge acc_graph node child_node
            | None ->
                [%log' debug t.logger]
                  ~metadata:
                    [ ("state_hash", State_hash.to_yojson successor_state_hash)
                    ; ("error", `String "missing from frontier") ]
                  "Could not visualize state $state_hash: $error" ;
                acc_graph ) )
end

let visualize ~filename (t : t) =
  Out_channel.with_file filename ~f:(fun output_channel ->
      let graph = Visualizor.to_graph t in
      Visualizor.output_graph output_channel graph )

let visualize_to_string t =
  let graph = Visualizor.to_graph t in
  let buf = Buffer.create 0 in
  let formatter = Format.formatter_of_buffer buf in
  Visualizor.fprint_graph formatter graph ;
  Format.pp_print_flush formatter () ;
  Buffer.contents buf

(* given an heir, calculate the diff that will transition the root to that heir *)
let calculate_root_transition_diff t heir =
  let root = root t in
  let root_hash = t.root in
  let heir_hash = Breadcrumb.state_hash heir in
  let heir_transition = Breadcrumb.validated_transition heir in
  let heir_staged_ledger = Breadcrumb.staged_ledger heir in
  let heir_siblings =
    List.filter (successors t root) ~f:(fun breadcrumb ->
        not (State_hash.equal heir_hash (Breadcrumb.state_hash breadcrumb)) )
  in
  let garbage_breadcrumbs =
    List.bind heir_siblings ~f:(fun sibling ->
        sibling :: successors_rec t sibling )
    |> List.rev
  in
  let garbage_nodes =
    List.map garbage_breadcrumbs ~f:(fun breadcrumb ->
        let open Diff.Node_list in
        let transition = Breadcrumb.validated_transition breadcrumb in
        let scan_state =
          Staged_ledger.scan_state (Breadcrumb.staged_ledger breadcrumb)
        in
        {transition; scan_state} )
  in
  let protocol_states =
    Protocol_states_for_root_scan_state
    .protocol_states_for_next_root_scan_state
      t.protocol_states_for_root_scan_state
      ~new_scan_state:(Staged_ledger.scan_state heir_staged_ledger)
      ~old_root_state:
        {With_hash.hash= root_hash; data= Breadcrumb.protocol_state root}
  in
  let new_root_data =
    Root_data.Limited.create ~transition:heir_transition
      ~scan_state:(Staged_ledger.scan_state heir_staged_ledger)
      ~pending_coinbase:
        (Staged_ledger.pending_coinbase_collection heir_staged_ledger)
      ~protocol_states
  in
  let just_emitted_a_proof = Breadcrumb.just_emitted_a_proof heir in
  Diff.Full.E.E
    (Root_transitioned
       { new_root= new_root_data
       ; garbage= Full garbage_nodes
       ; just_emitted_a_proof })

let move_root t ~new_root_hash ~new_root_protocol_states ~garbage
    ~enable_epoch_ledger_sync =
  (* The transition frontier at this point in time has the following mask topology:
   *
   *   (`s` represents a snarked ledger, `m` represents a mask)
   *
   *     garbage
   *     [m...]
   *       ^
   *       |          successors
   *       m0 -> m1 -> [m...]
   *       ^
   *       |
   *       s
   *
   * In this diagram, the old root's mask (`m0`) is parented off of the root snarked
   * ledger database, and the new root's mask (`m1`) is parented off of the `m0`.
   * There is also some garbage parented off of `m0`, and some successors that will
   * be kept in the tree after transition which are parented off of `m1`.
   *
   * In order to move the root, we must form a mask `m1'` with the same merkle root
   * as `m1`, except that it is parented directly off of the root snarked ledger
   * instead of `m0`. Furthermore, the root snarked ledger `s` may update to another
   * merkle root as `s'` if there is a proof emitted in the transition between `m0`
   * and `m1`.
   *
   * To form a mask `m1'` and update the snarked ledger from `s` to `s'` (which is a
   * noop in the case of no ledger proof emitted between `m0` and `m1`), we must perform
   * the following operations on masks in order:
   *
   *     0) notify consensus that root transitioned
   *     1) unattach and destroy all the garbage (to avoid unecessary trickling of
   *        invalidations from `m0` during the next step)
   *     2) commit `m1` into `m0`, making `m0` into `m1'` (same merkle root as `m1`), and
   *        making `m1` into an identity mask (an empty mask on top of `m1'`).
   *     3) safely remove `m1` and reparent all the successors of `m1` onto `m1'`
   *     4) create a new temporary mask `mt` with `s` as it's parent
   *     5) apply any transactions to `mt` that appear in the transition between `s` and `s'`
   *     6) commit `mt` into `s`, turning `s` into `s'`
   *     7) unattach and destroy `mt`
   *)
  let old_root_node = Hashtbl.find_exn t.table t.root in
  let new_root_node = Hashtbl.find_exn t.table new_root_hash in
  let genesis_ledger_hash =
    Breadcrumb.blockchain_state old_root_node.breadcrumb
    |> Blockchain_state.genesis_ledger_hash
  in
  (* STEP 0 *)
  let () =
    match enable_epoch_ledger_sync with
    | `Enabled snarked_ledger ->
        O1trace.measure "calling consensus hook frontier_root_transition"
          (fun () ->
            Consensus.Hooks.frontier_root_transition
              (Breadcrumb.consensus_state old_root_node.breadcrumb)
              (Breadcrumb.consensus_state new_root_node.breadcrumb)
              ~local_state:t.consensus_local_state ~snarked_ledger
              ~genesis_ledger_hash )
    | `Disabled ->
        ()
  in
  let new_staged_ledger =
    let m0 = Breadcrumb.mask old_root_node.breadcrumb in
    let m1 = Breadcrumb.mask new_root_node.breadcrumb in
    let m1_hash_pre_commit = Ledger.merkle_root m1 in
    (* STEP 1 *)
    List.iter garbage ~f:(fun node ->
        let open Diff.Node_list in
        let hash = External_transition.Validated.state_hash node.transition in
        let breadcrumb = find_exn t hash in
        let mask = Breadcrumb.mask breadcrumb in
        (* this should get garbage collected and should not require additional destruction *)
        ignore
          ( Ledger.Maskable.unregister_mask_exn ~loc:__LOC__ mask
            : Ledger.unattached_mask ) ;
        Hashtbl.remove t.table hash ) ;
    (* STEP 2 *)
    (* go ahead and remove the old root from the frontier *)
    Hashtbl.remove t.table t.root ;
    O1trace.measure "committing new root mask" (fun () -> Ledger.commit m1) ;
    [%test_result: Ledger_hash.t]
      ~message:
        "Merkle root of new root's staged ledger mask is the same after \
         committing"
      ~expect:m1_hash_pre_commit (Ledger.merkle_root m1) ;
    [%test_result: Ledger_hash.t]
      ~message:
        "Merkle root of old root's staged ledger mask is the same as the new \
         root's staged ledger mask after committing"
      ~expect:m1_hash_pre_commit (Ledger.merkle_root m0) ;
    (* STEP 3 *)
    (* the staged ledger's mask needs replaced before m1 is made invalid *)
    let new_staged_ledger =
      Staged_ledger.replace_ledger_exn
        (Breadcrumb.staged_ledger new_root_node.breadcrumb)
        m0
    in
    Ledger.remove_and_reparent_exn m1 m1 ;
    (* STEPS 4-7 *)
    (* we need to perform steps 4-7 iff there was a proof emitted in the scan
     * state we are transitioning to *)
    if Breadcrumb.just_emitted_a_proof new_root_node.breadcrumb then (
      let location =
        Persistent_root.Locations.potential_snarked_ledger
          t.persistent_root_instance.factory.directory
      in
      let () =
        Ledger.Db.make_checkpoint t.persistent_root_instance.snarked_ledger
          ~directory_name:location
      in
      [%log' info t.logger] "Enqueued a snarked ledger" ;
      Persistent_root.Instance.enqueue_snarked_ledger ~location
        t.persistent_root_instance ;
      let s = t.root_ledger in
      (* STEP 4 *)
      let mt =
        Ledger.Maskable.register_mask s
          (Ledger.Mask.create ~depth:(Ledger.Any_ledger.M.depth s) ())
      in
      (* STEP 5 *)
      Non_empty_list.iter
        (Option.value_exn
           (Staged_ledger.proof_txns_with_state_hashes
              (Breadcrumb.staged_ledger new_root_node.breadcrumb)))
        ~f:(fun (txn, state_hash) ->
          (*Validate transactions against the protocol state associated with the transaction*)
          let txn_state_view =
            find_protocol_state t state_hash
            |> Option.value_exn |> Protocol_state.body
            |> Protocol_state.Body.view
          in
          ignore
            ( Or_error.ok_exn
                (Ledger.apply_transaction
                   ~constraint_constants:
                     t.precomputed_values.constraint_constants ~txn_state_view
                   mt txn.data)
              : Ledger.Transaction_applied.t ) ) ;
      (* STEP 6 *)
      Ledger.commit mt ;
      (* STEP 7 *)
      ignore
        ( Ledger.Maskable.unregister_mask_exn ~loc:__LOC__ mt
          : Ledger.unattached_mask ) ) ;
    new_staged_ledger
  in
  (* rewrite the new root breadcrumb to contain the new root mask *)
  let new_root_breadcrumb =
    Breadcrumb.create
      ~validated_transition:
        (Breadcrumb.validated_transition new_root_node.breadcrumb)
      ~staged_ledger:new_staged_ledger
      ~just_emitted_a_proof:
        (Breadcrumb.just_emitted_a_proof new_root_node.breadcrumb)
      ~transition_receipt_time:
        (Breadcrumb.transition_receipt_time new_root_node.breadcrumb)
  in
  (*Update the protocol states required for scan state at the new root.
  Note: this should be after applying the transactions to the snarked ledger (Step 5)
  because the protocol states corresponding to those transactions won't be part
  of the new_root_protocol_states since those transactions would have been
  deleted from the scan state after emitting the proof*)
  let new_protocol_states_map =
    State_hash.Map.of_alist_exn new_root_protocol_states
  in
  t.protocol_states_for_root_scan_state <- new_protocol_states_map ;
  let new_root_node = {new_root_node with breadcrumb= new_root_breadcrumb} in
  (* update the new root breadcrumb in the frontier *)
  Hashtbl.set t.table ~key:new_root_hash ~data:new_root_node ;
  (* rewrite the root pointer to the new root hash *)
  t.root <- new_root_hash

(* calculates the diffs which need to be applied in order to add a breadcrumb to the frontier *)
let calculate_diffs t breadcrumb =
  let open Diff in
  O1trace.measure "calculate_diffs" (fun () ->
      let breadcrumb_hash = Breadcrumb.state_hash breadcrumb in
      let parent_node =
        Hashtbl.find_exn t.table (Breadcrumb.parent_hash breadcrumb)
      in
      let root_node = Hashtbl.find_exn t.table t.root in
      let current_best_tip = best_tip t in
      let diffs = [Full.E.E (New_node (Full breadcrumb))] in
      (* check if new breadcrumb extends frontier to longer than k *)
      let diffs =
        if parent_node.length + 1 - root_node.length > t.max_length then
          let heir = find_exn t (List.hd_exn (hash_path t breadcrumb)) in
          calculate_root_transition_diff t heir :: diffs
        else diffs
      in
      (* check if new breadcrumb will be best tip *)
      let diffs =
        if
          Consensus.Hooks.equal_select_status
            (Consensus.Hooks.select
               ~constants:t.precomputed_values.consensus_constants
               ~existing:
                 (Breadcrumb.consensus_state_with_hash current_best_tip)
               ~candidate:(Breadcrumb.consensus_state_with_hash breadcrumb)
               ~logger:
                 (Logger.extend t.logger
                    [ ( "selection_context"
                      , `String "comparing new breadcrumb to best tip" ) ]))
            `Take
        then Full.E.E (Best_tip_changed breadcrumb_hash) :: diffs
        else diffs
      in
      (* reverse diffs so that they are applied in the correct order *)
      List.rev diffs )

(* TODO: refactor metrics tracking outside of apply_diff (could maybe even be an extension?) *)
let apply_diff (type mutant) t (diff : (Diff.full, mutant) Diff.t)
    ~enable_epoch_ledger_sync : mutant * State_hash.t option =
  match diff with
  | New_node (Full breadcrumb) ->
      let breadcrumb_hash = Breadcrumb.state_hash breadcrumb in
      let parent_hash = Breadcrumb.parent_hash breadcrumb in
      let parent_node = Hashtbl.find_exn t.table parent_hash in
      Hashtbl.add_exn t.table ~key:breadcrumb_hash
        ~data:{breadcrumb; successor_hashes= []; length= parent_node.length + 1} ;
      Hashtbl.set t.table ~key:parent_hash
        ~data:
          { parent_node with
            successor_hashes= breadcrumb_hash :: parent_node.successor_hashes
          } ;
      ((), None)
  | Best_tip_changed new_best_tip ->
      let old_best_tip = t.best_tip in
      t.best_tip <- new_best_tip ;
      (old_best_tip, None)
  | Root_transitioned {new_root; garbage= Full garbage; _} ->
      let new_root_hash = Root_data.Limited.hash new_root in
      let old_root_hash = t.root in
      let new_root_protocol_states =
        Root_data.Limited.protocol_states new_root
      in
      move_root t ~new_root_hash ~new_root_protocol_states ~garbage
        ~enable_epoch_ledger_sync ;
      (old_root_hash, Some new_root_hash)

module Metrics = struct
  (* The max length of a path disjoint from the best tip path. O(n) *)
  let longest_fork t =
    let children : Breadcrumb.t -> Breadcrumb.t list =
      let tbl = State_hash.Table.create () in
      Hashtbl.iter t.table ~f:(fun node ->
          let b = node.breadcrumb in
          Hashtbl.add_multi tbl ~key:(Breadcrumb.parent_hash b) ~data:b ) ;
      fun b -> Hashtbl.find_multi tbl (Breadcrumb.state_hash b)
    in
    let on_best_tip_path : Breadcrumb.t -> bool =
      let s = State_hash.Hash_set.create () in
      List.iter (best_tip_path t) ~f:(fun b ->
          Hash_set.add s (Breadcrumb.state_hash b) ) ;
      fun b -> Hash_set.mem s (Breadcrumb.state_hash b)
    in
    let rec longest_fork subtree_root =
      (* TODO: Make tail recursive *)
      List.map (children subtree_root) ~f:(fun child ->
          if on_best_tip_path child then longest_fork child
          else 1 + longest_fork child )
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    longest_fork (find_exn t t.root)

  let parent t b = find t (Breadcrumb.parent_hash b)

  let empty_blocks_at_best_tip t =
    let rec go acc b =
      if not (List.is_empty (Breadcrumb.commands b)) then acc
      else match parent t b with None -> acc | Some b -> go (acc + 1) b
    in
    go 0 (best_tip t)

  let slot_time t b =
    Breadcrumb.consensus_state b
    |> Consensus.Data.Consensus_state.consensus_time
    |> Consensus.Data.Consensus_time.to_time
         ~constants:t.precomputed_values.consensus_constants

  let slot_time_to_offset_time_span s =
    let r =
      Block_time.to_span_since_epoch s
      |> Block_time.Span.to_ms
      |> (fun x -> Int64.(x / of_int 1000))
      |> Int64.to_float
    in
    r -. Mina_metrics.time_offset_sec

  let has_coinbase b =
    let d1, d2 =
      ( Breadcrumb.validated_transition b
      |> External_transition.Validated.staged_ledger_diff )
        .diff
    in
    match (d1.coinbase, d2) with
    | Zero, None | Zero, Some {coinbase= Zero; _} ->
        false
    | Zero, Some {coinbase= One _; _} | One _, _ | Two _, _ ->
        true

  let intprop f b = Unsigned.UInt32.to_int (f (Breadcrumb.consensus_state b))

  (* Rate of slots filled on the main chain in the k slots preceeding the best tip. *)
  let slot_fill_rate t =
    let open Consensus.Data.Consensus_state in
    let best_tip = best_tip t in
    let rec find_ancestor ~f b =
      if f b then `Found b
      else
        match find t (Breadcrumb.parent_hash b) with
        | None ->
            `Ended_search_at b
        | Some parent ->
            find_ancestor ~f parent
    in
    let start =
      let open Consensus.Data.Consensus_state in
      let slot = intprop curr_global_slot in
      let best_tip_slot = slot best_tip in
      let k =
        Unsigned.UInt32.to_int t.precomputed_values.consensus_constants.k
      in
      match
        find_ancestor best_tip ~f:(fun b -> best_tip_slot - slot b >= k)
      with
      | `Found b | `Ended_search_at b ->
          b
    in
    let change f = intprop f best_tip - intprop f start in
    let length_change = change blockchain_length in
    let slot_change = change curr_global_slot in
    if slot_change = 0 then 1.
    else Float.of_int length_change /. Float.of_int slot_change
end

let update_metrics_with_diff (type mutant) t
    (diff : (Diff.full, mutant) Diff.t) : unit =
  let open Metrics in
  match diff with
  | New_node (Full b) ->
      Mina_metrics.(
        Gauge.inc_one Transition_frontier.active_breadcrumbs ;
        Counter.inc_one Transition_frontier.total_breadcrumbs ;
        Gauge.set Transition_frontier.accepted_block_slot_time_sec
          (slot_time t b |> slot_time_to_offset_time_span))
  | Root_transitioned {garbage= Full garbage_breadcrumbs; _} ->
      let new_root_breadcrumb = root t in
      Mina_metrics.(
        let num_breadcrumbs_removed =
          Int.to_float (1 + List.length garbage_breadcrumbs)
        in
        let num_finalized_staged_txns =
          Int.to_float (List.length (Breadcrumb.commands new_root_breadcrumb))
        in
        Gauge.dec Transition_frontier.active_breadcrumbs
          num_breadcrumbs_removed ;
        Gauge.set Transition_frontier.recently_finalized_staged_txns
          num_finalized_staged_txns ;
        Counter.inc Transition_frontier.finalized_staged_txns
          num_finalized_staged_txns ;
        Counter.inc_one Transition_frontier.root_transitions ;
        Transition_frontier.TPS_30min.update num_finalized_staged_txns)
      (* TODO: optimize and add these metrics back in (#2850) *)
      (*
        let root_snarked_ledger_accounts =
          Ledger.Any_ledger.M.to_list t.root_ledger
        in
        let num_root_snarked_ledger_accounts =
          Int.to_float (List.length root_snarked_ledger_accounts)
        in
        let root_snarked_ledger_total_currency =
          Int.to_float
            (List.fold_left root_snarked_ledger_accounts ~init:0
               ~f:(fun sum account ->
                 sum + Currency.Balance.to_int account.balance ))
        in
        Gauge.set Transition_frontier.root_snarked_ledger_accounts
          num_root_snarked_ledger_accounts ;
        Gauge.set Transition_frontier.root_snarked_ledger_total_currency
          root_snarked_ledger_total_currency ;
        *)
  | Best_tip_changed _old_best_tip ->
      let best_tip = best_tip t in
      let open Consensus.Data.Consensus_state in
      let slot_time = slot_time t best_tip in
      let is_recent_block =
        let now = Block_time.now t.time_controller in
        let two_slots =
          let one_slot =
            t.precomputed_values.consensus_constants.block_window_duration_ms
          in
          Block_time.Span.(one_slot + one_slot)
        in
        Block_time.Span.( <= ) (Block_time.diff now slot_time) two_slots
      in
      Mina_metrics.(
        Gauge.set Transition_frontier.best_tip_user_txns
          (Int.to_float (List.length (Breadcrumb.commands best_tip))) ;
        if is_recent_block then
          Gauge.set Transition_frontier.best_tip_coinbase
            (if has_coinbase best_tip then 1. else 0.) ;
        Gauge.set Transition_frontier.slot_fill_rate (slot_fill_rate t) ;
        Gauge.set Transition_frontier.min_window_density
          (Int.to_float (intprop min_window_density best_tip)) ;
        Gauge.set Transition_frontier.longest_fork
          (Int.to_float (longest_fork t)) ;
        Gauge.set Transition_frontier.best_tip_slot_time_sec
          (slot_time_to_offset_time_span slot_time) ;
        Gauge.set Transition_frontier.empty_blocks_at_best_tip
          (Int.to_float (empty_blocks_at_best_tip t)))

let apply_diffs t diffs ~enable_epoch_ledger_sync ~has_long_catchup_job =
  let open Root_identifier.Stable.Latest in
  [%log' trace t.logger] "Applying %d diffs to full frontier "
    (List.length diffs) ;
  let consensus_constants = t.precomputed_values.consensus_constants in
  let local_state_was_synced_at_start =
    Consensus.Hooks.required_local_state_sync ~constants:consensus_constants
      ~consensus_state:(Breadcrumb.consensus_state (best_tip t))
      ~local_state:t.consensus_local_state
    |> Option.is_none
  in
  let new_root, diffs_with_mutants =
    List.fold diffs ~init:(None, [])
      ~f:(fun (prev_root, diffs_with_mutants) (Diff.Full.E.E diff) ->
        let mutant, new_root = apply_diff t diff ~enable_epoch_ledger_sync in
        update_metrics_with_diff t diff ;
        let new_root =
          match new_root with
          | None ->
              prev_root
          | Some state_hash ->
              Some {state_hash}
        in
        (new_root, Diff.Full.With_mutant.E (diff, mutant) :: diffs_with_mutants)
    )
  in
  [%log' trace t.logger] "after applying diffs to full frontier" ;
  if
    (not
       ([%equal: [`Enabled of _ | `Disabled]] enable_epoch_ledger_sync
          `Disabled))
    && not has_long_catchup_job
  then
    Debug_assert.debug_assert (fun () ->
        match
          Consensus.Hooks.required_local_state_sync
            ~constants:consensus_constants
            ~consensus_state:
              (Breadcrumb.consensus_state
                 (Hashtbl.find_exn t.table t.best_tip).breadcrumb)
            ~local_state:t.consensus_local_state
        with
        | Some jobs ->
            (* But if there wasn't sync work to do when we started, then there shouldn't be now. *)
            if local_state_was_synced_at_start then (
              [%log' fatal t.logger]
                "after lock transition, the best tip consensus state is out \
                 of sync with the local state -- bug in either \
                 required_local_state_sync or frontier_root_transition."
                ~metadata:
                  [ ( "sync_jobs"
                    , Consensus.Hooks.local_state_sync_to_yojson jobs )
                  ; ( "local_state"
                    , Consensus.Data.Local_state.to_yojson
                        t.consensus_local_state )
                  ; ("tf_viz", `String (visualize_to_string t)) ] ;
              failwith
                "local state desynced after applying diffs to full frontier" )
        | None ->
            () ) ;
  `New_root_and_diffs_with_mutants (new_root, diffs_with_mutants)

module For_tests = struct
  let find_protocol_state_exn t hash =
    match find_protocol_state t hash with
    | Some s ->
        s
    | None ->
        failwith
          (sprintf
             !"Protocol state with hash %s not found"
             (State_body_hash.to_yojson hash |> Yojson.Safe.to_string))

  let equal t1 t2 =
    let sort_breadcrumbs = List.sort ~compare:Breadcrumb.compare in
    let equal_breadcrumb breadcrumb1 breadcrumb2 =
      let open Breadcrumb in
      let open Option.Let_syntax in
      let get_successor_nodes frontier breadcrumb =
        let%map node = Hashtbl.find frontier.table @@ state_hash breadcrumb in
        Node.successor_hashes node
      in
      equal breadcrumb1 breadcrumb2
      && State_hash.equal (parent_hash breadcrumb1) (parent_hash breadcrumb2)
      && (let%bind successors1 = get_successor_nodes t1 breadcrumb1 in
          let%map successors2 = get_successor_nodes t2 breadcrumb2 in
          List.equal State_hash.equal
            (successors1 |> List.sort ~compare:State_hash.compare)
            (successors2 |> List.sort ~compare:State_hash.compare))
         |> Option.value_map ~default:false ~f:Fn.id
    in
    List.equal equal_breadcrumb
      (all_breadcrumbs t1 |> sort_breadcrumbs)
      (all_breadcrumbs t2 |> sort_breadcrumbs)
end
