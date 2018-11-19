open Core_kernel
open Protocols.Coda_pow

module type Inputs_intf = sig
  module Time : Time_intf

  module Proof : Proof_intf

  module State_hash : Hash_intf

  module Ledger_hash : Ledger_hash_intf

  module Frozen_ledger_hash :
    Frozen_ledger_hash_intf with type ledger_hash := Ledger_hash.t

  module Ledger_builder_aux_hash : Ledger_builder_aux_hash_intf

  module Ledger_builder_hash :
    Ledger_builder_hash_intf
    with type ledger_hash := Ledger_hash.t
     and type ledger_builder_aux_hash := Ledger_builder_aux_hash.t

  module Ledger_builder_diff : Ledger_builder_diff_intf

  module Blockchain_state :
    Blockchain_state_intf
    with type ledger_builder_hash := Ledger_builder_hash.t
     and type frozen_ledger_hash := Frozen_ledger_hash.t
     and type time := Time.t

  module Consensus_state : Consensus_state_intf

  module Protocol_state :
    Protocol_state_intf
    with type state_hash := State_hash.t
     and type blockchain_state := Blockchain_state.value
     and type consensus_state := Consensus_state.value

  module External_transition :
    External_transition_intf
    with type protocol_state := Protocol_state.value
     and type protocol_state_proof := Proof.t
     and type ledger_builder_diff := Ledger_builder_diff.t

  module Merkle_mask : sig
    type t

    val root : t -> Ledger_hash.t

    val derive : t -> t

    val merge : t -> t -> unit

    val apply : t -> Ledger_builder_diff.t -> unit
  end

  module Merkle_ledger : sig
    type t

    val root : t -> Ledger_hash.t

    val derive : t -> Merkle_mask.t

    val merge : t -> Merkle_mask.t -> unit
  end

  module Max_length : sig
    val t : int
  end
end

(* NOTE: is Consensus_mechanism.select preferable over distance? *)
module Make (Inputs : Inputs_intf) :
  Transition_frontier_intf
  with type state_hash := Inputs.State_hash.t
   and type external_transition := Inputs.External_transition.t
   and type merkle_ledger := Inputs.Merkle_ledger.t
   and type merkle_mask := Inputs.Merkle_mask.t = struct
  open Inputs

  exception Parent_not_found of ([`Parent of State_hash.t] * [`Target of State_hash.t])
  exception Already_exists of State_hash.t

  let max_length = Max_length.t

  module Breadcrumb = struct
    type t = {transition: External_transition.t; mask: Merkle_mask.t}
    [@@deriving fields]

    let hash t =
      t.transition |> External_transition.protocol_state |> Protocol_state.hash

    let parent_hash t =
      t.transition |> External_transition.protocol_state
      |> Protocol_state.previous_state_hash
  end

  type node =
    {breadcrumb: Breadcrumb.t; successor_hashes: State_hash.t list; length: int}

  type t =
    { root_ledger: Merkle_ledger.t
    ; mutable root: State_hash.t
    ; mutable best_tip: State_hash.t
    ; table: node State_hash.Table.t }

  (* TODO: load from and write to disk *)
  let create ~root ~ledger =
    let protocol_state = External_transition.protocol_state root in
    let blockchain_state = Protocol_state.blockchain_state protocol_state in
    assert (
      Ledger_hash.equal
        (Merkle_ledger.root ledger)
        (Frozen_ledger_hash.to_ledger_hash
           (Blockchain_state.ledger_hash blockchain_state)) ) ;
    let mask = Merkle_ledger.derive ledger in
    Merkle_mask.apply mask (External_transition.ledger_builder_diff root) ;
    assert (
      Ledger_hash.equal (Merkle_mask.root mask)
        (Ledger_builder_hash.ledger_hash
           (Blockchain_state.ledger_builder_hash blockchain_state)) ) ;
    let root_hash = Protocol_state.hash protocol_state in
    let root_breadcrumb = {Breadcrumb.transition= root; mask} in
    let root_node =
      {breadcrumb= root_breadcrumb; successor_hashes= []; length= 0}
    in
    let table = State_hash.Table.of_alist_exn [(root_hash, root_node)] in
    {root_ledger= ledger; root= root_hash; best_tip= root_hash; table}

  let find t hash =
    let open Option.Let_syntax in
    let%map node = Hashtbl.find t.table hash in
    node.breadcrumb

  let find_exn t hash =
    let node = Hashtbl.find_exn t.table hash in
    node.breadcrumb

  let path t breadcrumb =
    let rec find_path b =
      let hash = Breadcrumb.hash b in
      let parent_hash = Breadcrumb.parent_hash b in
      if State_hash.equal parent_hash t.root then [hash]
      else hash :: find_path (find_exn t parent_hash)
    in
    List.rev (find_path breadcrumb)

  let iter t ~f = Hashtbl.iter t.table ~f:(fun n -> f n.breadcrumb)

  let root t = find_exn t t.root

  let best_tip t = find_exn t t.best_tip

  let successor_hashes t hash =
    let node = Hashtbl.find_exn t.table hash in
    node.successor_hashes

  let rec successor_hashes_rec t hash =
    List.concat
      (List.map (successor_hashes t hash) ~f:(fun succ_hash ->
           succ_hash :: successor_hashes_rec t succ_hash ))

  let successors t breadcrumb =
    List.map (successor_hashes t (Breadcrumb.hash breadcrumb)) ~f:(find_exn t)

  let rec successors_rec t breadcrumb =
    List.concat
      (List.map (successors t breadcrumb) ~f:(fun succ ->
           succ :: successors_rec t succ ))

  (* Adding a transition to the transition frontier is broken into the following steps:
   *   1) create a new node for a transition
   *     a) derive a new mask from the parent mask
   *     b) apply the ledger builder diff to the new mask
   *     c) form the breadcrumb and node records
   *   2) add the node to the table
   *   3) add the successor_hashes entry to the parent node
   *   4) move the root if the path to the new node is longer than the max length
   *     a) calculate the distance from the new node to the parent
   *     b) if the distance is greater than the max length:
   *       I  ) find the immediate successor of the old root in the path to the
   *            longest node and make it the new root
   *       II ) find all successors of the other immediate successors of the old root
   *       III) remove the old root and all of the nodes found in (II) from the table
   *       IV ) merge the old root's merkle mask into the root ledger
   *   5) set the new node as the best tip if the new node has a greater length than
   *      the current best tip
   *)
  let add_exn t transition =
    let protocol_state = External_transition.protocol_state transition in
    let hash = Protocol_state.hash protocol_state in
    let root_node = Hashtbl.find_exn t.table t.root in
    let best_tip_node = Hashtbl.find_exn t.table t.best_tip in
    let parent_hash = Protocol_state.previous_state_hash protocol_state in
    let parent_node =
      Option.value_exn
        (Hashtbl.find t.table parent_hash)
        ~error:(Error.of_exn (Parent_not_found (`Parent parent_hash, `Target hash)))
    in
    (* 1.a *)
    let mask = Merkle_mask.derive (Breadcrumb.mask parent_node.breadcrumb) in
    (* 1.b *)
    Merkle_mask.apply mask (External_transition.ledger_builder_diff transition) ;
    (* 1.c *)
    let node =
      { breadcrumb= {Breadcrumb.transition; mask}
      ; successor_hashes= []
      ; length= parent_node.length + 1 }
    in
    (* 2 *)
    (if Hashtbl.add t.table ~key:hash ~data:node <> `Ok then
       Error.raise (Error.of_exn (Already_exists hash)));
    (* 3 *)
    Hashtbl.set t.table ~key:parent_hash
      ~data:
        { parent_node with
          successor_hashes= hash :: parent_node.successor_hashes } ;
    (* 4.a *)
    let distance_to_parent = root_node.length - node.length in
    (* 4.b *)
    if distance_to_parent > max_length then (
      (* 4.b.I *)
      let new_root_hash = List.hd_exn (path t node.breadcrumb) in
      (* 4.b.II *)
      let garbage_immediate_successors =
        List.filter root_node.successor_hashes ~f:(fun succ_hash ->
            not (State_hash.equal succ_hash new_root_hash) )
      in
      (* 4.b.III *)
      let garbage =
        t.root
        :: List.concat
             (List.map garbage_immediate_successors ~f:(successor_hashes_rec t))
      in
      t.root <- new_root_hash ;
      List.iter garbage ~f:(Hashtbl.remove t.table) ;
      (* 4.b.IV *)
      Merkle_ledger.merge t.root_ledger (Breadcrumb.mask root_node.breadcrumb) ) ;
    (* 5 *)
    let best_tip_node = Hashtbl.find_exn t.table t.best_tip in
    if node.length > best_tip_node.length then t.best_tip <- hash ;
    node.breadcrumb
end

(*
let%test "transitions can be added and interface will work at any point" =
  let module Frontier = Make (struct
    module State_hash = Test_mocks.Hash.Int_unchecked
    module External_transition = Test_mocks.External_transition.T
    module Max_length = struct
      let t = 5
    end
  end) in
  let open Frontier in
  let t = create ~log:(Logger.create ()) in

  (* test that functions that shouldn't throw exceptions don't *)
  let interface_works () =
    let r = root t in
    ignore (best_tip t);
    ignore (successor_hashes_rec r);
    ignore (successors_rec r);
    iter t ~f:(fun b ->
        let h = Breadcrumb.hash b in
        find_exn t h;
        ignore (successor_hashes t h)
        ignore (successors t b))
  in

  (* add a single random transition based off a random node *)
  let add_transition () =
    let base_hash = List.head_exn (List.shuffle (hashes t)) in
    let trans = Quickcheck.random_value (External_transition.gen base_hash) in
    add_exn t trans
  in

  interface_works ();
  for i = 1 to 200 do
    add_transition ();
    interface_works ()
  done
*)
