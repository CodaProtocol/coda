open Core_kernel

module type Inputs_intf = sig
  include Base_inputs_intf.S

  module Location : Location_intf.S
end

module Make (Inputs : Inputs_intf) : sig
  include
    Base_ledger_intf.S
    with module Addr = Inputs.Location.Addr
    with module Location = Inputs.Location
    with type key := Inputs.Key.t
     and type token_id := Inputs.Token_id.t
     and type token_id_set := Inputs.Token_id.Set.t
     and type account_id := Inputs.Account_id.t
     and type account_id_set := Inputs.Account_id.Set.t
     and type hash := Inputs.Hash.t
     and type root_hash := Inputs.Hash.t
     and type account := Inputs.Account.t

  val create : depth:int -> unit -> t
end = struct
  open Inputs

  type t = {uuid: Uuid.t; depth: int} [@@deriving sexp_of]

  let t_of_sexp _ = failwith "t_of_sexp unimplemented"

  type index = int

  module Location = Location
  module Path = Merkle_path.Make (Hash)

  type path = Path.t

  module Addr = Location.Addr

  let create ~depth () = {uuid= Uuid_unix.create (); depth}

  let remove_accounts_exn _t keys =
    if List.is_empty keys then ()
    else failwith "remove_accounts_exn: null ledgers cannot be mutated"

  let empty_hash_at_height =
    Empty_hashes.extensible_cache (module Hash) ~init_hash:Hash.empty_account

  let merkle_path t location =
    let location =
      if Location.is_account location then
        Location.Hash (Location.to_path_exn location)
      else location
    in
    assert (Location.is_hash location) ;
    let rec loop k =
      let h = Location.height ~ledger_depth:t.depth k in
      if h >= t.depth then []
      else
        let sibling_dir = Location.last_direction (Location.to_path_exn k) in
        let hash = empty_hash_at_height h in
        Direction.map sibling_dir ~left:(`Left hash) ~right:(`Right hash)
        :: loop (Location.parent k)
    in
    loop location

  let merkle_root t = empty_hash_at_height t.depth

  let merkle_path_at_addr_exn t addr = merkle_path t (Location.Hash addr)

  let merkle_path_at_index_exn t index =
    merkle_path_at_addr_exn t (Addr.of_int_exn ~ledger_depth:t.depth index)

  let index_of_account_exn _t =
    failwith "index_of_account_exn: null ledgers are empty"

  let set_at_index_exn _t =
    failwith "set_at_index_exn: null ledgers cannot be mutated"

  let get_at_index_exn _t = failwith "get_at_index_exn: null ledgers are empty"

  let set_batch _t = failwith "set_batch: null ledgers cannot be mutated"

  let set _t = failwith "set: null ledgers cannot be mutated"

  let get _t _loc = None

  let get_uuid t = t.uuid

  let get_directory _ = None

  let last_filled _t = None

  let close _t = ()

  let get_or_create_account _t =
    failwith "get_or_create_account: null ledgers cannot be mutated"

  let location_of_account _t _ = None

  let accounts _t = Account_id.Set.empty

  let token_owner _t _tid = None

  let token_owners _t = Account_id.Set.empty

  let tokens _t _pk = Token_id.Set.empty

  let next_available_token _t = Token_id.(next default)

  let set_next_available_token _t _tid =
    failwith "set_next_available_token: null ledgers cannot be mutated"

  let iteri _t ~f:_ = ()

  let fold_until _t ~init ~f:_ ~finish = finish init

  let foldi_with_ignored_accounts _t _ ~init ~f:_ = init

  let foldi _t ~init ~f:_ = init

  let to_list _t = []

  let make_space_for _t _tot = ()

  let get_all_accounts_rooted_at_exn t addr =
    let first_node, last_node =
      Addr.Range.subtree_range ~ledger_depth:t.depth addr
    in
    let first_index = Addr.to_int first_node in
    let last_index = Addr.to_int last_node in
    List.(
      zip_exn
        (map
           ~f:(Addr.of_int_exn ~ledger_depth:t.depth)
           (range first_index last_index))
        (init
           (1 lsl Addr.height ~ledger_depth:t.depth addr)
           ~f:(Fn.const Account.empty)))

  let set_all_accounts_rooted_at_exn _t =
    failwith "set_all_accounts_rooted_at_exn: null ledgers cannot be mutated"

  let set_batch_accounts _t =
    failwith "set_batch_accounts: null ledgers cannot be mutated"

  let set_inner_hash_at_addr_exn _t =
    failwith "set_inner_hash_at_addr_exn: null ledgers cannot be mutated"

  let get_inner_hash_at_addr_exn t addr =
    empty_hash_at_height (Addr.height ~ledger_depth:t.depth addr)

  let num_accounts _t = 0

  let depth t = t.depth

  let detached_signal _ = Async.Deferred.never ()
end
