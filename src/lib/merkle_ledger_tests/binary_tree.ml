open Core

module Make (Account : sig
  type t
end)
(Hash : Merkle_ledger.Intf.Hash with type account := Account.t) (Depth : sig
    val depth : int
end) =
struct
  type t = Node of {hash: Hash.t; left: t; right: t} | Leaf of Hash.t
  [@@deriving sexp]

  let max_depth = Depth.depth

  let get_hash = function Leaf hash -> hash | Node {hash; _} -> hash

  let set_accounts (list : Account.t list) =
    let rec go (list : Hash.t list) num_nodes =
      if num_nodes = 1 then
        match list with
        | head :: next_nodes ->
            (Leaf head, next_nodes, 0)
        | [] ->
            failwith "Expected to recurse on a non-empty list"
      else
        let left_tree, right_list, left_height = go list (num_nodes / 2) in
        let right_tree, remaining_nodes, right_height =
          go right_list (num_nodes / 2)
        in
        assert (left_height = right_height) ;
        let hash =
          Hash.merge ~height:left_height (get_hash left_tree)
            (get_hash right_tree)
        in
        ( Node {hash; left= left_tree; right= right_tree}
        , remaining_nodes
        , left_height + 1 )
    in
    let max_num_accts = 1 lsl Depth.depth in
    let num_empty_hashes = max_num_accts - List.length list in
    let empty_hashes =
      List.init num_empty_hashes ~f:(fun _ -> Hash.empty_account)
    in
    let tree, remaining_nodes, _ =
      go (List.map ~f:Hash.hash_account list @ empty_hashes) max_num_accts
    in
    assert (List.is_empty remaining_nodes) ;
    tree

  let rec get_inner_hash_at_addr_exn = function
    | Leaf hash -> (
        function
        | [] -> hash | _ :: _ -> failwith "Could not traverse beyond a leaf" )
    | Node {hash; left; right} -> (
        function
        | [] ->
            hash
        | Direction.Left :: xs ->
            get_inner_hash_at_addr_exn left xs
        | Direction.Right :: xs ->
            get_inner_hash_at_addr_exn right xs )
end
