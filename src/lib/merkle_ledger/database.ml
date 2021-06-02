open Core

module type Inputs_intf = sig
  include Base_inputs_intf.S

  module Location : Location_intf.S

  module Location_binable : Hashable.S_binable with type t := Location.t

  module Kvdb : Intf.Key_value_database with type config := string

  module Storage_locations : Intf.Storage_locations
end

module Make (Inputs : Inputs_intf) :
  Database_intf.S
  with module Location = Inputs.Location
   and module Addr = Inputs.Location.Addr
   and type key := Inputs.Key.t
   and type token_id := Inputs.Token_id.t
   and type token_id_set := Inputs.Token_id.Set.t
   and type account := Inputs.Account.t
   and type root_hash := Inputs.Hash.t
   and type hash := Inputs.Hash.t
   and type account_id := Inputs.Account_id.t
   and type account_id_set := Inputs.Account_id.Set.t = struct
  (* The max depth of a merkle tree can never be greater than 253. *)
  open Inputs

  module Db_error = struct
    type t = Account_location_not_found | Out_of_leaves | Malformed_database
    [@@deriving sexp]

    exception Db_exception of t
  end

  module Path = Merkle_path.Make (Hash)
  module Addr = Location.Addr
  module Location = Location
  module Key = Key

  type location = Location.t [@@deriving sexp]

  type index = int

  type path = Path.t

  module Detached_parent_signal = struct
    type t = unit Async.Ivar.t

    let sexp_of_t (_ : t) = Sexp.List []

    let t_of_sexp (_ : Sexp.t) : t = Async.Ivar.create ()
  end

  type t =
    { uuid: Uuid.Stable.V1.t
    ; kvdb: Kvdb.t sexp_opaque
    ; depth: int
    ; directory: string
    ; detached_parent_signal: Detached_parent_signal.t }
  [@@deriving sexp]

  let get_uuid t = t.uuid

  let get_directory t = Some t.directory

  let depth t = t.depth

  let create ?directory_name ~depth () =
    assert (depth < 0xfe) ;
    let uuid = Uuid_unix.create () in
    let directory =
      match directory_name with
      | None ->
          (* Create in the autogen path, where we know we have write
             permissions.
          *)
          Cache_dir.autogen_path ^/ Uuid.to_string uuid
      | Some name ->
          name
    in
    Unix.mkdir_p directory ;
    let kvdb = Kvdb.create directory in
    {uuid; kvdb; depth; directory; detached_parent_signal= Async.Ivar.create ()}

  let create_checkpoint t ~directory_name =
    let uuid = Uuid_unix.create () in
    let kvdb = Kvdb.create_checkpoint t.kvdb directory_name in
    { uuid
    ; kvdb
    ; depth= t.depth
    ; directory= directory_name
    ; detached_parent_signal= Async.Ivar.create () }

  let make_checkpoint t ~directory_name =
    Kvdb.make_checkpoint t.kvdb directory_name

  let close {kvdb; uuid= _; depth= _; directory= _; detached_parent_signal} =
    Kvdb.close kvdb ;
    Async.Ivar.fill_if_empty detached_parent_signal ()

  let detached_signal {detached_parent_signal; _} =
    Async.Ivar.read detached_parent_signal

  let with_ledger ~depth ~f =
    let t = create ~depth () in
    try
      let result = f t in
      close t ; result
    with exn -> close t ; raise exn

  let empty_hash =
    Empty_hashes.extensible_cache (module Hash) ~init_hash:Hash.empty_account

  let get_raw {kvdb; depth; _} location =
    Kvdb.get kvdb ~key:(Location.serialize ~ledger_depth:depth location)

  let get_bin mdb location bin_read =
    get_raw mdb location
    |> Option.map ~f:(fun v -> bin_read v ~pos_ref:(ref 0))

  let delete_raw {kvdb; depth; _} location =
    Kvdb.remove kvdb ~key:(Location.serialize ~ledger_depth:depth location)

  let get mdb location =
    assert (Location.is_account location) ;
    get_bin mdb location Account.bin_read_t

  let get_hash mdb location =
    assert (Location.is_hash location) ;
    match get_bin mdb location Hash.bin_read_t with
    | Some hash ->
        hash
    | None ->
        empty_hash (Location.height ~ledger_depth:mdb.depth location)

  let account_list_bin {kvdb; _} account_bin_read : Account.t list =
    let all_keys_values = Kvdb.to_alist kvdb in
    (* see comment at top of location.ml about encoding of locations *)
    let account_location_prefix =
      Location.Prefix.account |> Unsigned.UInt8.to_int
    in
    (* just want list of locations and accounts, ignoring other locations *)
    let locations_accounts_bin =
      List.filter all_keys_values ~f:(fun (loc, _v) ->
          let ch = Bigstring.get_uint8 loc ~pos:0 in
          Int.equal ch account_location_prefix )
    in
    List.map locations_accounts_bin ~f:(fun (_location_bin, account_bin) ->
        account_bin_read account_bin ~pos_ref:(ref 0) )

  let to_list mdb = account_list_bin mdb Account.bin_read_t

  let accounts mdb =
    to_list mdb |> List.map ~f:Account.identifier |> Account_id.Set.of_list

  let set_raw {kvdb; depth; _} location bin =
    Kvdb.set kvdb
      ~key:(Location.serialize ~ledger_depth:depth location)
      ~data:bin

  let set_raw_batch {kvdb; depth; _} locations_bins =
    let serialize_location (loc, bin) =
      (Location.serialize ~ledger_depth:depth loc, bin)
    in
    let serialized = List.map locations_bins ~f:serialize_location in
    Kvdb.set_batch kvdb ~key_data_pairs:serialized

  let set_bin mdb location bin_size bin_write v =
    let buf = Bigstring.create (bin_size v) in
    ignore (bin_write buf ~pos:0 v) ;
    set_raw mdb location buf

  let set_bin_batch mdb bin_size bin_write locations_vs =
    let create_buf (loc, v) =
      let buf = Bigstring.create (bin_size v) in
      ignore (bin_write buf ~pos:0 v) ;
      (loc, buf)
    in
    let locs_bufs = List.map locations_vs ~f:create_buf in
    set_raw_batch ~remove_keys:[] mdb locs_bufs

  let get_inner_hash_at_addr_exn mdb address =
    assert (Addr.depth address <= mdb.depth) ;
    get_hash mdb (Location.Hash address)

  let set_inner_hash_at_addr_exn mdb address hash =
    assert (Addr.depth address <= mdb.depth) ;
    set_bin mdb (Location.Hash address) Hash.bin_size_t Hash.bin_write_t hash

  let make_space_for _t _tot = ()

  let get_generic mdb location =
    assert (Location.is_generic location) ;
    get_raw mdb location

  module Account_location = struct
    (** encodes a key, token_id pair as a location used as a database key, so
        we can find the account location associated with that key.
    *)
    let build_location account_id =
      Location.build_generic
        (Bigstring.of_string
           ( "$"
           ^ Format.sprintf
               !"%{sexp: Key.t}!%{sexp: Token_id.t}"
               (Account_id.public_key account_id)
               (Account_id.token_id account_id) ))

    let serialize_kv ~ledger_depth (aid, location) =
      ( Location.serialize ~ledger_depth @@ build_location aid
      , Location.serialize ~ledger_depth location )

    let get mdb key =
      match get_generic mdb (build_location key) with
      | None ->
          Error Db_error.Account_location_not_found
      | Some location_bin ->
          Location.parse ~ledger_depth:mdb.depth location_bin
          |> Result.map_error ~f:(fun () -> Db_error.Malformed_database)

    let delete mdb key = delete_raw mdb (build_location key)

    let set mdb key location =
      set_raw mdb (build_location key)
        (Location.serialize ~ledger_depth:mdb.depth location)

    let set_batch_create ~ledger_depth keys_to_locations =
      List.map ~f:(serialize_kv ~ledger_depth) keys_to_locations

    let _set_batch mdb keys_to_locations =
      Kvdb.set_batch mdb.kvdb
        ~key_data_pairs:
          (set_batch_create ~ledger_depth:mdb.depth keys_to_locations)

    let last_location_key () =
      Location.build_generic (Bigstring.of_string "last_account_location")

    let serialize_last_account_kv ~ledger_depth
        (location, last_account_location) =
      ( Location.serialize ~ledger_depth location
      , Location.serialize ~ledger_depth last_account_location )

    let increment_last_account_location mdb =
      let location = last_location_key () in
      let ledger_depth = mdb.depth in
      match get_generic mdb location with
      | None ->
          let first_location =
            Location.Account
              ( Addr.of_directions
              @@ List.init mdb.depth ~f:(fun _ -> Direction.Left) )
          in
          set_raw mdb location
            (Location.serialize ~ledger_depth first_location) ;
          Result.return first_location
      | Some prev_location -> (
        match Location.parse ~ledger_depth:mdb.depth prev_location with
        | Error () ->
            Error Db_error.Malformed_database
        | Ok prev_account_location ->
            Location.next prev_account_location
            |> Result.of_option ~error:Db_error.Out_of_leaves
            |> Result.map ~f:(fun next_account_location ->
                   set_raw mdb location
                     (Location.serialize ~ledger_depth next_account_location) ;
                   next_account_location ) )

    let allocate mdb key =
      let location_result = increment_last_account_location mdb in
      Result.map location_result ~f:(fun location ->
          set mdb key location ; location )

    let last_location_address mdb =
      match
        last_location_key () |> get_raw mdb |> Result.of_option ~error:()
        |> Result.bind ~f:(Location.parse ~ledger_depth:mdb.depth)
      with
      | Error () ->
          None
      | Ok parsed_location ->
          Some (Location.to_path_exn parsed_location)

    let last_location mdb =
      match
        last_location_key () |> get_raw mdb |> Result.of_option ~error:()
        |> Result.bind ~f:(Location.parse ~ledger_depth:mdb.depth)
      with
      | Error () ->
          None
      | Ok parsed_location ->
          Some parsed_location
  end

  (** The tokens associated with each public key.

      These are represented as a [Token_id.Set.t], which is represented by an
      ordered list.
  *)
  module Tokens = struct
    let next_available_key =
      Memo.unit (fun () ->
          Location.build_generic (Bigstring.of_string "next_available_token")
      )

    let next_available mdb =
      Option.value
        ~default:Token_id.(next default)
        (get_bin mdb (next_available_key ()) Token_id.Stable.Latest.bin_read_t)

    let next_available_kv ~ledger_depth tid =
      let token_buf =
        Bin_prot.Common.create_buf (Token_id.Stable.Latest.bin_size_t tid)
      in
      ignore (Token_id.Stable.Latest.bin_write_t token_buf ~pos:0 tid) ;
      (Location.serialize ~ledger_depth (next_available_key ()), token_buf)

    let set_next_available mdb tid =
      set_bin mdb (next_available_key ()) Token_id.Stable.Latest.bin_size_t
        Token_id.Stable.Latest.bin_write_t tid

    let update_next_available_token mdb tid =
      if Token_id.(next_available mdb <= tid) then
        set_next_available mdb Token_id.(next tid)

    module Owner = struct
      let build_location token_id =
        Location.build_generic
          (Bigstring.of_string
             (Format.sprintf !"$tid!%{sexp: Token_id.t}" token_id))

      let serialize_kv ~ledger_depth (tid, pk) =
        let pk_buf =
          Bin_prot.Common.create_buf (Key.Stable.Latest.bin_size_t pk)
        in
        ignore (Key.Stable.Latest.bin_write_t pk_buf ~pos:0 pk) ;
        (Location.serialize ~ledger_depth (build_location tid), pk_buf)

      let get mdb token_id =
        get_bin mdb (build_location token_id) Key.Stable.Latest.bin_read_t

      let set mdb token_id public_key =
        set_bin mdb (build_location token_id) Key.Stable.Latest.bin_size_t
          Key.Stable.Latest.bin_write_t public_key

      let remove mdb token_id = delete_raw mdb (build_location token_id)

      let foldi t ~init ~f =
        let next_available_token = next_available t in
        let rec go acc tid =
          let tid = Token_id.next tid in
          if Token_id.(tid < next_available_token) then
            let acc =
              Option.fold ~init:acc
                ~f:(fun acc pk -> f ~key:tid ~data:pk acc)
                (get t tid)
            in
            go acc tid
          else acc
        in
        go init Token_id.default

      let _iteri t ~f = foldi t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)

      (* Newest tokens come first, but this should be fine. *)
      let get_all t =
        foldi t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)
    end

    let build_location pk =
      Location.build_generic
        (Bigstring.of_string (Format.sprintf !"$tids!%{sexp: Key.t}" pk))

    let serialize_kv ~ledger_depth (pk, tids) =
      let tokens_buf =
        Bin_prot.Common.create_buf (Token_id.Set.bin_size_t tids)
      in
      ignore (Token_id.Set.bin_write_t tokens_buf ~pos:0 tids) ;
      (Location.serialize ~ledger_depth (build_location pk), tokens_buf)

    let get_opt mdb pk =
      get_bin mdb (build_location pk) Token_id.Set.bin_read_t

    let get mdb pk = Option.value ~default:Token_id.Set.empty (get_opt mdb pk)

    let set mdb pk tids =
      set_bin mdb (build_location pk) Token_id.Set.bin_size_t
        Token_id.Set.bin_write_t tids

    let delete mdb pk = delete_raw mdb (build_location pk)

    let change_opt mdb pk ~f =
      let old = get_opt mdb pk in
      match (old, f old) with
      | _, Some tids ->
          set mdb pk tids
      | Some _, None ->
          delete mdb pk
      | None, None ->
          ()

    let to_opt s = if Set.is_empty s then None else Some s

    let _update_opt mdb pk ~f = change_opt mdb pk ~f:(fun x -> to_opt (f x))

    let update mdb pk ~f =
      change_opt mdb pk ~f:(fun x ->
          to_opt @@ f (Option.value ~default:Token_id.Set.empty x) )

    let add mdb pk tid = update mdb pk ~f:(fun tids -> Set.add tids tid)

    let _add_several mdb pk new_tids =
      update mdb pk ~f:(fun tids ->
          Set.union tids (Token_id.Set.of_list new_tids) )

    let add_account mdb aid account =
      let token = Account_id.token_id aid in
      let key = Account_id.public_key aid in
      add mdb key token ;
      update_next_available_token mdb token ;
      if Account.token_owner account then Owner.set mdb token key

    let remove mdb pk tid = update mdb pk ~f:(fun tids -> Set.remove tids tid)

    let _remove_several mdb pk rem_tids =
      update mdb pk ~f:(fun tids ->
          Set.diff tids (Token_id.Set.of_list rem_tids) )

    let remove_account mdb aid =
      let token = Account_id.token_id aid in
      let key = Account_id.public_key aid in
      remove mdb key token ;
      if Option.equal Key.equal (Owner.get mdb token) (Some key) then
        Owner.remove mdb token

    (** Generate a batch of database changes to add the given tokens. *)
    let add_batch_create mdb pks_to_tokens =
      let pks_to_all_tokens =
        Map.filter_mapi pks_to_tokens ~f:(fun ~key:pk ~data:tokens_to_add ->
            to_opt (Set.union (get mdb pk) tokens_to_add) )
      in
      Map.to_alist pks_to_all_tokens
      |> List.map ~f:(serialize_kv ~ledger_depth:mdb.depth)

    let _add_batch mdb pks_to_tokens =
      Kvdb.set_batch mdb.kvdb
        ~key_data_pairs:(add_batch_create mdb pks_to_tokens)
  end

  let location_of_account t key =
    match Account_location.get t key with
    | Error _ ->
        None
    | Ok location ->
        Some location

  let last_filled t = Account_location.last_location t

  let token_owners t =
    Tokens.Owner.get_all t
    |> List.map ~f:(fun (tid, pk) -> Account_id.create pk tid)
    |> Account_id.Set.of_list

  let token_owner = Tokens.Owner.get

  let tokens = Tokens.get

  let next_available_token = Tokens.next_available

  let set_next_available_token = Tokens.set_next_available

  include Util.Make (struct
    module Key = Key
    module Token_id = Token_id
    module Account_id = Account_id
    module Balance = Balance
    module Location = Location
    module Location_binable = Location_binable
    module Account = Account
    module Hash = Hash

    module Base = struct
      type nonrec t = t

      let get = get

      let last_filled = last_filled
    end

    let get_hash = get_hash

    let ledger_depth = depth

    let location_of_account_addr addr = Location.Account addr

    let location_of_hash_addr addr = Location.Hash addr

    let set_raw_hash_batch mdb addresses_and_hashes =
      set_bin_batch mdb Hash.bin_size_t Hash.bin_write_t addresses_and_hashes

    let set_location_batch ~last_location mdb key_to_location_list =
      let last_location_key_value =
        (Account_location.last_location_key (), last_location)
      in
      let next_available_token = Tokens.next_available mdb in
      let key_to_location_list = Non_empty_list.to_list key_to_location_list in
      let account_tokens, new_next_available_token =
        List.fold ~init:(Key.Map.empty, next_available_token)
          key_to_location_list ~f:(fun (map, next_available_token) (aid, _) ->
            ( Map.update map (Account_id.public_key aid) ~f:(function
                | Some set ->
                    Set.add set (Account_id.token_id aid)
                | None ->
                    Token_id.Set.singleton (Account_id.token_id aid) )
            , (* If the token is present in an account, it is no longer
                 available.
              *)
              Token_id.max next_available_token
                (Token_id.next (Account_id.token_id aid)) ) )
      in
      let next_available_token_change =
        if new_next_available_token > next_available_token then
          [ Tokens.next_available_kv ~ledger_depth:mdb.depth
              new_next_available_token ]
        else []
      in
      let batched_changes =
        next_available_token_change
        @ Account_location.serialize_last_account_kv ~ledger_depth:mdb.depth
            last_location_key_value
          :: ( Tokens.add_batch_create mdb account_tokens
             @ Account_location.set_batch_create ~ledger_depth:mdb.depth
                 key_to_location_list )
      in
      Kvdb.set_batch mdb.kvdb ~remove_keys:[] ~key_data_pairs:batched_changes

    let set_raw_account_batch mdb
        (addresses_and_accounts : (location * Account.t) list) =
      set_bin_batch mdb Account.bin_size_t Account.bin_write_t
        addresses_and_accounts ;
      let token_owner_changes =
        List.filter_map addresses_and_accounts ~f:(fun (_, account) ->
            if Account.token_owner account then
              let aid = Account.identifier account in
              Some
                (Tokens.Owner.serialize_kv ~ledger_depth:mdb.depth
                   (Account_id.token_id aid, Account_id.public_key aid))
            else None )
      in
      Kvdb.set_batch mdb.kvdb ~remove_keys:[]
        ~key_data_pairs:token_owner_changes
  end)

  let set_hash mdb location new_hash = set_hash_batch mdb [(location, new_hash)]

  module For_tests = struct
    let gen_account_location ~ledger_depth =
      let open Quickcheck.Let_syntax in
      let build_account (path : Direction.t list) =
        assert (List.length path = ledger_depth) ;
        Location.Account (Addr.of_directions path)
      in
      let%map dirs =
        Quickcheck.Generator.list_with_length ledger_depth Direction.gen
      in
      build_account dirs
  end

  let set mdb location account =
    set_bin mdb location Account.bin_size_t Account.bin_write_t account ;
    set_hash mdb
      (Location.Hash (Location.to_path_exn location))
      (Hash.hash_account account)

  let index_of_account_exn mdb account_id =
    let location = location_of_account mdb account_id |> Option.value_exn in
    let addr = Location.to_path_exn location in
    Addr.to_int addr

  let get_at_index_exn mdb index =
    let addr = Addr.of_int_exn ~ledger_depth:mdb.depth index in
    get mdb (Location.Account addr) |> Option.value_exn

  let set_at_index_exn mdb index account =
    let addr = Addr.of_int_exn ~ledger_depth:mdb.depth index in
    set mdb (Location.Account addr) account

  let get_or_create_account mdb account_id account =
    match Account_location.get mdb account_id with
    | Error Account_location_not_found -> (
      match Account_location.allocate mdb account_id with
      | Ok location ->
          set mdb location account ;
          Tokens.add_account mdb account_id account ;
          Ok (`Added, location)
      | Error err ->
          Error (Error.create "get_or_create_account" err Db_error.sexp_of_t) )
    | Error err ->
        Error (Error.create "get_or_create_account" err Db_error.sexp_of_t)
    | Ok location ->
        Ok (`Existed, location)

  let num_accounts t =
    match Account_location.last_location_address t with
    | None ->
        0
    | Some addr ->
        Addr.to_int addr + 1

  let iteri t ~f =
    match Account_location.last_location_address t with
    | None ->
        ()
    | Some last_addr ->
        Sequence.range ~stop:`inclusive 0 (Addr.to_int last_addr)
        |> Sequence.iter ~f:(fun i -> f i (get_at_index_exn t i))

  (* TODO : if key-value store supports iteration mechanism, like RocksDB,
     maybe use that here, instead of loading all accounts into memory See Issue
     #1191 *)
  let foldi_with_ignored_accounts t ignored_accounts ~init ~f =
    let f' index accum account =
      f (Addr.of_int_exn ~ledger_depth:(depth t) index) accum account
    in
    match Account_location.last_location_address t with
    | None ->
        init
    | Some last_addr ->
        let ignored_indices =
          Int.Set.map ignored_accounts ~f:(fun account_id ->
              try index_of_account_exn t account_id with _ -> -1
              (* dummy index for accounts not in database *) )
        in
        let last = Addr.to_int last_addr in
        Sequence.range ~stop:`inclusive 0 last
        (* filter out indices corresponding to ignored accounts *)
        |> Sequence.filter ~f:(fun loc -> not (Int.Set.mem ignored_indices loc))
        |> Sequence.map ~f:(get_at_index_exn t)
        |> Sequence.foldi ~init ~f:f'

  let foldi t ~init ~f =
    foldi_with_ignored_accounts t Account_id.Set.empty ~init ~f

  module C : Container.S0 with type t := t and type elt := Account.t =
  Container.Make0 (struct
    module Elt = Account

    type nonrec t = t

    let fold t ~init ~f =
      let f' _index accum account = f accum account in
      foldi t ~init ~f:f'

    let iter = `Define_using_fold

    (* Use num_accounts instead? *)
    let length = `Define_using_fold
  end)

  let fold_until = C.fold_until

  let merkle_root mdb = get_hash mdb Location.root_hash

  let remove_accounts_exn t keys =
    let locations =
      (* if we don't have a location for all keys, raise an exception *)
      let rec loop keys accum =
        match keys with
        | [] ->
            accum (* no need to reverse *)
        | key :: rest -> (
          match Account_location.get t key with
          | Ok loc ->
              loop rest (loc :: accum)
          | Error err ->
              raise (Db_error.Db_exception err) )
      in
      loop keys []
    in
    (* N.B.: we're not using stack database here to make available newly-freed
       locations *)
    List.iter keys ~f:(Account_location.delete t) ;
    List.iter keys ~f:(Tokens.remove_account t) ;
    List.iter locations ~f:(fun loc -> delete_raw t loc) ;
    (* recalculate hashes for each removed account *)
    List.iter locations ~f:(fun loc ->
        let hash_loc = Location.Hash (Location.to_path_exn loc) in
        set_hash t hash_loc Hash.empty_account )

  let merkle_path mdb location =
    let location =
      if Location.is_account location then
        Location.Hash (Location.to_path_exn location)
      else location
    in
    assert (Location.is_hash location) ;
    let rec loop k =
      if Location.height ~ledger_depth:mdb.depth k >= mdb.depth then []
      else
        let sibling = Location.sibling k in
        let sibling_dir = Location.last_direction (Location.to_path_exn k) in
        let hash = get_hash mdb sibling in
        Direction.map sibling_dir ~left:(`Left hash) ~right:(`Right hash)
        :: loop (Location.parent k)
    in
    loop location

  let merkle_path_at_addr_exn t addr = merkle_path t (Location.Hash addr)

  let merkle_path_at_index_exn t index =
    let addr = Addr.of_int_exn ~ledger_depth:t.depth index in
    merkle_path_at_addr_exn t addr
end
