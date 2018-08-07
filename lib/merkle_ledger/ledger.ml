open Core
open Dyn_array

(* SOMEDAY: handle empty wallets *)

module type S = sig
  type hash

  type account

  type key

  val depth : int

  type index = int

  type t [@@deriving sexp, bin_io]

  include Container.S0 with type t := t and type elt := account

  val copy : t -> t

  module Path : sig
    type elem = [`Left of hash | `Right of hash] [@@deriving sexp]

    val elem_hash : elem -> hash

    type t = elem list [@@deriving sexp]

    val implied_root : t -> hash -> hash
  end

  module Addr : sig
    type t [@@deriving sexp, bin_io, hash, eq, compare]

    include Hashable.S with type t := t

    val depth : t -> int

    val parent : t -> t Or_error.t

    val parent_exn : t -> t

    val child : t -> [`Left | `Right] -> t Or_error.t

    val child_exn : t -> [`Left | `Right] -> t

    val dirs_from_root : t -> [`Left | `Right] list

    val root : t
  end

  val create : unit -> t

  val length : t -> int

  val get : t -> key -> account option

  val set : t -> key -> account -> unit

  val update : t -> key -> f:(account option -> account) -> unit

  val merkle_root : t -> hash

  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

  val hash_fold_t :
    Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state

  val compare : t -> t -> int

  val merkle_path : t -> key -> Path.t option

  val key_of_index : t -> index -> key option

  val index_of_key : t -> key -> index option

  val key_of_index_exn : t -> index -> key

  val index_of_key_exn : t -> key -> index

  val get_at_index : t -> index -> [`Ok of account | `Index_not_found]

  val set_at_index : t -> index -> account -> [`Ok | `Index_not_found]

  val merkle_path_at_index : t -> index -> [`Ok of Path.t | `Index_not_found]

  val get_at_index_exn : t -> index -> account

  val set_at_index_exn : t -> index -> account -> unit

  val merkle_path_at_addr_exn : t -> Addr.t -> Path.t

  val merkle_path_at_index_exn : t -> index -> Path.t

  val addr_of_index : t -> index -> Addr.t

  val set_at_addr_exn : t -> Addr.t -> account -> unit

  val get_inner_hash_at_addr_exn : t -> Addr.t -> hash

  val set_inner_hash_at_addr_exn : t -> Addr.t -> hash -> unit

  val extend_with_empty_to_fit : t -> int -> unit

  val set_syncing : t -> unit

  val clear_syncing : t -> unit

  val set_all_accounts_rooted_at_exn : t -> Addr.t -> account list -> unit

  val get_all_accounts_rooted_at_exn : t -> Addr.t -> account list
end

module type F = functor (Key :sig
                                
                                type t [@@deriving sexp, bin_io]

                                val empty : t

                                include Hashable.S_binable with type t := t
end) -> functor (Account :sig
                            
                            type t [@@deriving sexp, eq, bin_io]

                            val public_key : t -> Key.t
end) -> functor (Hash :sig
                         
                         type hash [@@deriving sexp, hash, compare, bin_io]

                         val hash_account : Account.t -> hash

                         val empty_hash : hash

                         val merge : height:int -> hash -> hash -> hash
end) -> functor (Depth :sig
                          
                          val depth : int
end) -> S
        with type hash := Hash.hash
         and type account := Account.t
         and type key := Key.t

module Make (Key : sig
  type t [@@deriving sexp, bin_io]

  val empty : t

  include Hashable.S_binable with type t := t
end) (Account : sig
  type t [@@deriving sexp, eq, bin_io]

  val public_key : t -> Key.t
end) (Hash : sig
  type hash [@@deriving sexp, hash, compare, bin_io]

  val hash_account : Account.t -> hash

  val empty_hash : hash

  val merge : height:int -> hash -> hash -> hash
end) (Depth : sig
  val depth : int
end) :
  S
  with type hash := Hash.hash
   and type account := Account.t
   and type key := Key.t =
struct
  include Depth

  module Addr = struct
    module T = struct
      type t = {depth: int; index: int}
      [@@deriving sexp, bin_io, hash, eq, compare]
    end

    include T
    include Hashable.Make (T)

    let depth {depth; _} = depth

    let bit_val = function `Left -> 0 | `Right -> 1

    let child {depth; index} d =
      if depth + 1 < Depth.depth then
        Ok {depth= depth + 1; index= index lor (bit_val d lsl depth)}
      else Or_error.error_string "Addr.child: Depth was too large"

    let child_exn a d = child a d |> Or_error.ok_exn

    let dirs_from_root {depth; index} =
      List.init depth ~f:(fun i ->
          if (index lsr i) land 1 = 1 then `Right else `Left )

    (* FIXME: this could be a lot faster. https://graphics.stanford.edu/~seander/bithacks.html#BitReverseObvious etc *)
    let to_index a =
      List.foldi
        (List.rev @@ dirs_from_root a)
        ~init:0
        ~f:(fun i acc dir -> acc lor (bit_val dir lsl i))

    let of_index index =
      let depth = Depth.depth in
      let bits = List.init depth ~f:(fun i -> (index lsr i) land 1) in
      (* XXX: LSB first *)
      {depth; index= List.fold bits ~init:0 ~f:(fun acc b -> (acc lsl 1) lor b)}

    let clear_all_but_first k i = i land ((1 lsl k) - 1)

    let parent {depth; index} =
      if depth > 0 then
        Ok {depth= depth - 1; index= clear_all_but_first (depth - 1) index}
      else Or_error.error_string "Addr.parent: depth <= 0"

    let parent_exn a = Or_error.ok_exn (parent a)

    let root = {depth= 0; index= 0}

    let%test_unit "dirs_from_root" =
      let dir_list =
        let open Quickcheck.Generator in
        let open Let_syntax in
        let%bind l = Int.gen_incl 0 (Depth.depth - 1) in
        list_with_length l (Bool.gen >>| fun b -> if b then `Right else `Left)
      in
      Quickcheck.test dir_list ~f:(fun dirs ->
          assert (
            dirs_from_root (List.fold dirs ~f:child_exn ~init:root) = dirs ) )

    let%test_unit "to_index (of_index i) = i" =
      Quickcheck.test ~sexp_of:[%sexp_of : int]
        (Int.gen_incl 0 (Depth.depth - 1))
        ~f:(fun i -> [%test_eq : int] (to_index (of_index i)) i)
  end

  type entry = {merkle_index: int; account: Account.t}
  [@@deriving sexp, bin_io]

  type accounts = entry Key.Table.t [@@deriving sexp, bin_io]

  type index = int

  type leafs = Key.t Dyn_array.t [@@deriving sexp, bin_io]

  type nodes = Hash.hash Dyn_array.t list [@@deriving sexp, bin_io]

  type tree =
    { leafs: leafs
    ; mutable dirty: bool
    ; mutable syncing: bool
    ; mutable nodes_height: int
    ; mutable nodes: nodes
    ; mutable dirty_indices: int list }
  [@@deriving sexp, bin_io]

  type t = {accounts: accounts; tree: tree} [@@deriving sexp, bin_io]

  module Container0 :
    Container.S0 with type t := t and type elt := Account.t =
  Container.Make0 (struct
    module Elt = Account

    type nonrec t = t

    let fold t ~init ~f =
      Hashtbl.fold t.accounts ~init ~f:(fun ~key:_ ~data:{account} acc ->
          f acc account )

    let iter = `Define_using_fold
  end)

  include Container0

  let copy t =
    let copy_tree tree =
      { leafs= Dyn_array.copy tree.leafs
      ; dirty= tree.dirty
      ; syncing= false
      ; nodes_height= tree.nodes_height
      ; nodes= List.map tree.nodes ~f:Dyn_array.copy
      ; dirty_indices= tree.dirty_indices }
    in
    {accounts= Key.Table.copy t.accounts; tree= copy_tree t.tree}

  module Path = struct
    type elem = [`Left of Hash.hash | `Right of Hash.hash] [@@deriving sexp]

    let elem_hash = function `Left h | `Right h -> h

    type t = elem list [@@deriving sexp]

    let implied_root (t: t) hash =
      List.fold t ~init:(hash, 0) ~f:(fun (acc, height) elem ->
          let acc =
            match elem with
            | `Left h -> Hash.merge ~height acc h
            | `Right h -> Hash.merge ~height h acc
          in
          (acc, height + 1) )
      |> fst
  end

  let create_account_table () = Key.Table.create ()

  let empty_hash_at_heights depth =
    let empty_hash_at_heights = Array.create (depth + 1) Hash.empty_hash in
    let rec go i =
      if i <= depth then (
        let h = empty_hash_at_heights.(i - 1) in
        empty_hash_at_heights.(i) <- Hash.merge ~height:(i - 1) h h ;
        go (i + 1) )
    in
    go 1 ; empty_hash_at_heights

  let memoized_empty_hash_at_height = empty_hash_at_heights depth

  let empty_hash_at_height d = memoized_empty_hash_at_height.(d)

  (* if depth = N, leafs = 2^N *)
  let create () =
    { accounts= create_account_table ()
    ; tree=
        { leafs= Dyn_array.create ()
        ; dirty= false
        ; syncing= false
        ; nodes_height= 0
        ; nodes= []
        ; dirty_indices= [] } }

  let length t = Key.Table.length t.accounts

  let key_of_index t index =
    if index >= Dyn_array.length t.tree.leafs then None
    else Some (Dyn_array.get t.tree.leafs index)

  let index_of_key t key =
    Option.map (Hashtbl.find t.accounts key) ~f:(fun {merkle_index; _} ->
        merkle_index )

  let key_of_index_exn t index = Option.value_exn (key_of_index t index)

  let index_of_key_exn t key = Option.value_exn (index_of_key t key)

  let get t key =
    Option.map (Hashtbl.find t.accounts key) ~f:(fun entry -> entry.account)

  let index_not_found label index =
    failwithf "Ledger.%s: Index %d not found" label index ()

  let get_at_index t index =
    if index >= Dyn_array.length t.tree.leafs then `Index_not_found
    else
      let key = Dyn_array.get t.tree.leafs index in
      `Ok (Hashtbl.find_exn t.accounts key).account

  let get_at_index_exn t index =
    match get_at_index t index with
    | `Ok account -> account
    | `Index_not_found -> index_not_found "get_at_index_exn" index

  let set t key account =
    match Hashtbl.find t.accounts key with
    | None ->
        let merkle_index = Dyn_array.length t.tree.leafs in
        Hashtbl.set t.accounts ~key ~data:{merkle_index; account} ;
        Dyn_array.add t.tree.leafs key ;
        (t.tree).dirty_indices <- merkle_index :: t.tree.dirty_indices
    | Some entry ->
        Hashtbl.set t.accounts ~key
          ~data:{merkle_index= entry.merkle_index; account} ;
        (t.tree).dirty_indices <- entry.merkle_index :: t.tree.dirty_indices

  let update t key ~f =
    match Hashtbl.find t.accounts key with
    | None ->
        let merkle_index = Dyn_array.length t.tree.leafs in
        Hashtbl.set t.accounts ~key ~data:{merkle_index; account= f None} ;
        Dyn_array.add t.tree.leafs key ;
        (t.tree).dirty_indices <- merkle_index :: t.tree.dirty_indices
    | Some {merkle_index; account} ->
        Hashtbl.set t.accounts ~key
          ~data:{merkle_index; account= f (Some account)} ;
        (t.tree).dirty_indices <- merkle_index :: t.tree.dirty_indices

  let set_at_index t index account =
    let leafs = t.tree.leafs in
    if index < Dyn_array.length leafs then (
      let key = Dyn_array.get leafs index in
      Hashtbl.set t.accounts ~key ~data:{merkle_index= index; account} ;
      (t.tree).dirty_indices <- index :: t.tree.dirty_indices ;
      `Ok )
    else `Index_not_found

  let set_at_index_exn t index account =
    match set_at_index t index account with
    | `Ok -> ()
    | `Index_not_found -> index_not_found "set_at_index_exn" index

  let extend_tree tree =
    let leafs = Dyn_array.length tree.leafs in
    if leafs <> 0 then (
      let target_depth = Int.max 1 (Int.ceil_log2 leafs) in
      let current_depth = tree.nodes_height in
      let additional_depth = target_depth - current_depth in
      tree.nodes_height <- tree.nodes_height + additional_depth ;
      tree.nodes
      <- List.concat
           [ tree.nodes
           ; List.init additional_depth ~f:(fun _ -> Dyn_array.create ()) ] ;
      List.iteri tree.nodes ~f:(fun i nodes ->
          let length = Int.pow 2 (tree.nodes_height - 1 - i) in
          let new_elems = length - Dyn_array.length nodes in
          Dyn_array.append
            (Dyn_array.init new_elems (fun x -> Hash.empty_hash))
            nodes ) )

  let rec recompute_layers curr_height get_prev_hash layers dirty_indices =
    match layers with
    | [] -> ()
    | curr :: layers ->
        let get_curr_hash =
          let n = Dyn_array.length curr in
          fun i ->
            if i < n then Dyn_array.get curr i
            else empty_hash_at_height curr_height
        in
        List.iter dirty_indices ~f:(fun i ->
            Dyn_array.set curr i
              (Hash.merge ~height:(curr_height - 1)
                 (get_prev_hash (2 * i))
                 (get_prev_hash ((2 * i) + 1))) ) ;
        let dirty_indices =
          List.dedup_and_sort ~compare:Int.compare
            (List.map dirty_indices ~f:(fun x -> x lsr 1))
        in
        recompute_layers (curr_height + 1) get_curr_hash layers dirty_indices

  let recompute_tree ?allow_sync t =
    if t.tree.syncing && not (allow_sync = Some true) then
      failwith "recompute tree while syncing -- logic error!" ;
    if not (List.is_empty t.tree.dirty_indices) || t.tree.dirty then (
      extend_tree t.tree ;
      (t.tree).dirty <- false ;
      let layer_dirty_indices =
        Int.Set.to_list
          (Int.Set.of_list (List.map t.tree.dirty_indices ~f:(fun x -> x / 2)))
      in
      let get_leaf_hash i =
        if i < Dyn_array.length t.tree.leafs then
          Hash.hash_account
            (Hashtbl.find_exn t.accounts (Dyn_array.get t.tree.leafs i))
              .account
        else Hash.empty_hash
      in
      recompute_layers 1 get_leaf_hash t.tree.nodes layer_dirty_indices ;
      (t.tree).dirty_indices <- [] )

  let merkle_root t =
    recompute_tree t ;
    let height = t.tree.nodes_height in
    let base_root =
      match List.last t.tree.nodes with
      | None -> Hash.empty_hash
      | Some a -> Dyn_array.get a 0
    in
    let rec go i hash =
      if i = 0 then hash
      else
        let height = depth - i in
        let hash = Hash.merge ~height hash (empty_hash_at_height height) in
        go (i - 1) hash
    in
    go (depth - height) base_root

  let hash t = Hash.hash_hash (merkle_root t)

  let hash_fold_t state t = Ppx_hash_lib.Std.Hash.fold_int state (hash t)

  let compare t t' = Hash.compare_hash (merkle_root t) (merkle_root t')

  let merkle_path t key =
    recompute_tree t ;
    Option.map (Hashtbl.find t.accounts key) ~f:(fun entry ->
        let addr0 = entry.merkle_index in
        let rec go height addr layers acc =
          match layers with
          | [] -> (acc, height)
          | curr :: layers ->
              let is_left = addr mod 2 = 0 in
              let hash =
                let sibling = addr lxor 1 in
                if sibling < Dyn_array.length curr then
                  Dyn_array.get curr sibling
                else empty_hash_at_height height
              in
              go (height + 1) (addr lsr 1) layers
                ((if is_left then `Left hash else `Right hash) :: acc)
        in
        let leaf_hash_idx = addr0 lxor 1 in
        let leaf_hash =
          if leaf_hash_idx >= Dyn_array.length t.tree.leafs then
            Hash.empty_hash
          else
            Hash.hash_account
              (Hashtbl.find_exn t.accounts
                 (Dyn_array.get t.tree.leafs leaf_hash_idx))
                .account
        in
        let is_left = addr0 mod 2 = 0 in
        let non_root_nodes = List.take t.tree.nodes (depth - 1) in
        let base_path, base_path_height =
          go 1 (addr0 lsr 1) non_root_nodes
            [(if is_left then `Left leaf_hash else `Right leaf_hash)]
        in
        List.rev_append base_path
          (List.init (depth - base_path_height) ~f:(fun i ->
               `Left (empty_hash_at_height (i + base_path_height)) )) )

  let merkle_path_at_index t index =
    match Option.(key_of_index t index >>= merkle_path t) with
    | None -> `Index_not_found
    | Some path -> `Ok path

  let merkle_path_at_index_exn t index =
    match merkle_path_at_index t index with
    | `Ok path -> path
    | `Index_not_found -> index_not_found "merkle_path_at_index_exn" index

  let addr_of_index t index = {Addr.depth; index}

  let extend_with_empty_to_fit t new_size =
    let tree = t.tree in
    let len = DynArray.length tree.leafs in
    if new_size > len then
      DynArray.append tree.leafs
        (DynArray.init (new_size - len) (fun x -> Key.empty)) ;
    recompute_tree ~allow_sync:true t

  let merkle_path_at_addr_exn t a =
    assert (a.Addr.depth = Depth.depth - 1) ;
    merkle_path_at_index_exn t (Addr.to_index a)

  let set_at_addr_exn t addr acct =
    assert (addr.Addr.depth = Depth.depth - 1) ;
    set_at_index_exn t (Addr.to_index addr) acct

  let get_inner_hash_at_addr_exn t a =
    assert (a.Addr.depth < depth) ;
    let l = List.nth_exn t.tree.nodes (depth - a.depth - 1) in
    DynArray.get l (Addr.to_index a)

  let set_inner_hash_at_addr_exn t a hash =
    assert (a.Addr.depth < depth) ;
    (t.tree).dirty <- true ;
    let l = List.nth_exn t.tree.nodes (depth - a.depth - 1) in
    let index = Addr.to_index a in
    DynArray.set l index hash

  let set_syncing t =
    recompute_tree t ;
    (t.tree).syncing <- true

  let clear_syncing t =
    (t.tree).syncing <- false ;
    recompute_tree t

  let set_all_accounts_rooted_at_exn t ({Addr.depth= adepth} as a) accounts =
    let height = depth - adepth in
    let first_index = Addr.to_index a lsl height in
    let count = min (1 lsl height) (length t - first_index) in
    assert (List.length accounts = count) ;
    List.iteri accounts ~f:(fun i a ->
        let pk = Account.public_key a in
        let entry = {merkle_index= first_index + i; account= a} in
        (t.tree).dirty_indices <- (first_index + i) :: t.tree.dirty_indices ;
        Key.Table.set t.accounts pk entry ;
        Dyn_array.set t.tree.leafs (first_index + i) pk )

  let get_all_accounts_rooted_at_exn t a =
    let height = depth - a.Addr.depth in
    let first_index = Addr.to_index a lsl height in
    let count = min (1 lsl height) (length t - first_index) in
    let subarr = Dyn_array.sub t.tree.leafs first_index count in
    Dyn_array.to_list
      (Dyn_array.map
         (fun key -> (Key.Table.find_exn t.accounts key).account)
         subarr)
end
