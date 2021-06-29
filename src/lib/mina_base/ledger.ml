open Core
open Signature_lib
open Merkle_ledger

module Ledger_inner = struct
  module Location_at_depth : Merkle_ledger.Location_intf.S =
    Merkle_ledger.Location.T

  module Location_binable = struct
    module Arg = struct
      type t = Location_at_depth.t =
        | Generic of Location.Bigstring.Stable.Latest.t
        | Account of Location_at_depth.Addr.Stable.Latest.t
        | Hash of Location_at_depth.Addr.Stable.Latest.t
      [@@deriving bin_io_unversioned, hash, sexp, compare]
    end

    type t = Arg.t =
      | Generic of Location.Bigstring.t
      | Account of Location_at_depth.Addr.t
      | Hash of Location_at_depth.Addr.t
    [@@deriving hash, sexp, compare]

    include Hashable.Make_binable (Arg) [@@deriving
                                          sexp, compare, hash, yojson]
  end

  module Kvdb : Intf.Key_value_database with type config := string =
    Rocksdb.Database

  module Storage_locations : Intf.Storage_locations = struct
    let key_value_db_dir = "coda_key_value_db"
  end

  module Hash = struct
    module Arg = struct
      type t = Ledger_hash.Stable.Latest.t
      [@@deriving sexp, compare, hash, bin_io_unversioned]
    end

    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = Ledger_hash.Stable.V1.t
        [@@deriving sexp, compare, hash, eq, yojson]

        type _unused = unit constraint t = Arg.t

        let to_latest = Fn.id

        include Hashable.Make_binable (Arg) [@@deriving
                                              sexp, compare, hash, eq, yojson]

        let to_string = Ledger_hash.to_string

        let merge = Ledger_hash.merge

        let hash_account = Fn.compose Ledger_hash.of_digest Account.digest

        let empty_account = Ledger_hash.of_digest Account.empty_digest
      end
    end]
  end

  module Account = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = Account.Stable.V1.t [@@deriving eq, compare, sexp]

        let to_latest = Fn.id

        let identifier = Account.identifier

        let balance Account.Poly.{balance; _} = balance

        let token Account.Poly.{token_id; _} = token_id

        let empty = Account.empty

        let token_owner ({token_permissions; _} : t) =
          match token_permissions with
          | Token_owned _ ->
              true
          | Not_owned _ ->
              false
      end
    end]

    let empty = Stable.Latest.empty

    let initialize = Account.initialize
  end

  module Inputs = struct
    module Key = Public_key.Compressed
    module Token_id = Token_id
    module Account_id = Account_id
    module Balance = Currency.Balance
    module Account = Account.Stable.Latest
    module Hash = Hash.Stable.Latest
    module Kvdb = Kvdb
    module Location = Location_at_depth
    module Location_binable = Location_binable
    module Storage_locations = Storage_locations
  end

  module Db :
    Merkle_ledger.Database_intf.S
    with module Location = Location_at_depth
    with module Addr = Location_at_depth.Addr
    with type root_hash := Ledger_hash.t
     and type hash := Ledger_hash.t
     and type key := Public_key.Compressed.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account := Account.t
     and type account_id_set := Account_id.Set.t
     and type account_id := Account_id.t =
    Database.Make (Inputs)

  module Null = Null_ledger.Make (Inputs)

  module Any_ledger :
    Merkle_ledger.Any_ledger.S
    with module Location = Location_at_depth
    with type account := Account.t
     and type key := Public_key.Compressed.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t
     and type hash := Hash.t =
    Merkle_ledger.Any_ledger.Make_base (Inputs)

  module Mask :
    Merkle_mask.Masking_merkle_tree_intf.S
    with module Location = Location_at_depth
     and module Attached.Addr = Location_at_depth.Addr
    with type account := Account.t
     and type key := Public_key.Compressed.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t
     and type hash := Hash.t
     and type location := Location_at_depth.t
     and type parent := Any_ledger.M.t =
  Merkle_mask.Masking_merkle_tree.Make (struct
    include Inputs
    module Base = Any_ledger.M
  end)

  module Maskable :
    Merkle_mask.Maskable_merkle_tree_intf.S
    with module Location = Location_at_depth
    with module Addr = Location_at_depth.Addr
    with type account := Account.t
     and type key := Public_key.Compressed.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t
     and type hash := Hash.t
     and type root_hash := Hash.t
     and type unattached_mask := Mask.t
     and type attached_mask := Mask.Attached.t
     and type t := Any_ledger.M.t =
  Merkle_mask.Maskable_merkle_tree.Make (struct
    include Inputs
    module Base = Any_ledger.M
    module Mask = Mask

    let mask_to_base m = Any_ledger.cast (module Mask.Attached) m
  end)

  include Mask.Attached
  module Debug = Maskable.Debug

  type maskable_ledger = t

  let of_database db =
    let casted = Any_ledger.cast (module Db) db in
    let mask = Mask.create ~depth:(Db.depth db) () in
    Maskable.register_mask casted mask

  (* Mask.Attached.create () fails, can't create an attached mask directly
  shadow create in order to create an attached mask
  *)
  let create ?directory_name ~depth () =
    of_database (Db.create ?directory_name ~depth ())

  let create_ephemeral_with_base ~depth () =
    let maskable = Null.create ~depth () in
    let casted = Any_ledger.cast (module Null) maskable in
    let mask = Mask.create ~depth () in
    (casted, Maskable.register_mask casted mask)

  let create_ephemeral ~depth () =
    let _base, mask = create_ephemeral_with_base ~depth () in
    mask

  let with_ledger ~depth ~f =
    let ledger = create ~depth () in
    try
      let result = f ledger in
      close ledger ; result
    with exn -> close ledger ; raise exn

  let with_ephemeral_ledger ~depth ~f =
    let _base_ledger, masked_ledger = create_ephemeral_with_base ~depth () in
    try
      let result = f masked_ledger in
      let (_ : Mask.t) =
        Maskable.unregister_mask_exn ~loc:__LOC__ ~grandchildren:`Recursive
          masked_ledger
      in
      result
    with exn ->
      let (_ : Mask.t) =
        Maskable.unregister_mask_exn ~loc:__LOC__ ~grandchildren:`Recursive
          masked_ledger
      in
      raise exn

  let packed t = Any_ledger.cast (module Mask.Attached) t

  let register_mask t mask = Maskable.register_mask (packed t) mask

  let unregister_mask_exn ~loc mask = Maskable.unregister_mask_exn ~loc mask

  let remove_and_reparent_exn t t_as_mask =
    Maskable.remove_and_reparent_exn (packed t) t_as_mask

  type unattached_mask = Mask.t

  type attached_mask = Mask.Attached.t

  (* inside MaskedLedger, the functor argument has assigned to location, account, and path
  but the module signature for the functor result wants them, so we declare them here *)
  type location = Location.t

  (* TODO: Don't allocate: see Issue #1191 *)
  let fold_until t ~init ~f ~finish =
    let accounts = to_list t in
    List.fold_until accounts ~init ~f ~finish

  let create_new_account_exn t account_id account =
    let action, _ =
      get_or_create_account t account_id account |> Or_error.ok_exn
    in
    if action = `Existed then
      failwith
        (sprintf
           !"Could not create a new account with pk \
             %{sexp:Public_key.Compressed.t}: Account already exists"
           (Account_id.public_key account_id))

  (* shadows definition in MaskedLedger, extra assurance hash is of right type  *)
  let merkle_root t =
    Ledger_hash.of_hash (merkle_root t :> Random_oracle.Digest.t)

  let get_or_create ledger account_id =
    let open Or_error.Let_syntax in
    let%bind action, loc =
      get_or_create_account ledger account_id (Account.initialize account_id)
    in
    let%map account =
      Result.of_option (get ledger loc)
        ~error:
          (Error.of_string
             "get_or_create: Account was not found in the ledger after creation")
    in
    (action, account, loc)

  let create_empty_exn ledger account_id =
    let start_hash = merkle_root ledger in
    match
      get_or_create_account ledger account_id Account.empty |> Or_error.ok_exn
    with
    | `Existed, _ ->
        failwith "create_empty for a key already present"
    | `Added, new_loc ->
        Debug_assert.debug_assert (fun () ->
            [%test_eq: Ledger_hash.t] start_hash (merkle_root ledger) ) ;
        (merkle_path ledger new_loc, Account.empty)

  let _handler t =
    let open Snark_params.Tick in
    let path_exn idx =
      List.map (merkle_path_at_index_exn t idx) ~f:(function
        | `Left h ->
            h
        | `Right h ->
            h )
    in
    stage (fun (With {request; respond}) ->
        match request with
        | Ledger_hash.Get_element idx ->
            let elt = get_at_index_exn t idx in
            let path = (path_exn idx :> Random_oracle.Digest.t list) in
            respond (Provide (elt, path))
        | Ledger_hash.Get_path idx ->
            let path = (path_exn idx :> Random_oracle.Digest.t list) in
            respond (Provide path)
        | Ledger_hash.Set (idx, account) ->
            set_at_index_exn t idx account ;
            respond (Provide ())
        | Ledger_hash.Find_index pk ->
            let index = index_of_account_exn t pk in
            respond (Provide index)
        | _ ->
            unhandled )
end

include Ledger_inner
include Transaction_logic.Make (Ledger_inner)

type init_state =
  ( Signature_lib.Keypair.t
  * Currency.Amount.t
  * Mina_numbers.Account_nonce.t
  * Account_timing.t )
  array
[@@deriving sexp_of]

let gen_initial_ledger_state : init_state Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let%bind n_accounts = Int.gen_incl 2 10 in
  let%bind keypairs = Quickcheck_lib.replicate_gen Keypair.gen n_accounts in
  let%bind balances =
    let gen_balance =
      let%map whole_balance = Int.gen_incl 500_000_000 1_000_000_000 in
      Currency.Amount.of_int (whole_balance * 1_000_000_000)
    in
    Quickcheck_lib.replicate_gen gen_balance n_accounts
  in
  let%bind nonces =
    Quickcheck_lib.replicate_gen
      ( Quickcheck.Generator.map ~f:Mina_numbers.Account_nonce.of_int
      @@ Int.gen_incl 0 1000 )
      n_accounts
  in
  let rec zip3_exn a b c =
    match (a, b, c) with
    | [], [], [] ->
        []
    | x :: xs, y :: ys, z :: zs ->
        (x, y, z, Account_timing.Untimed) :: zip3_exn xs ys zs
    | _ ->
        failwith "zip3 unequal lengths"
  in
  return @@ Array.of_list @@ zip3_exn keypairs balances nonces

let apply_initial_ledger_state : t -> init_state -> unit =
 fun t accounts ->
  Array.iter accounts ~f:(fun (kp, balance, nonce, timing) ->
      let pk_compressed = Public_key.compress kp.public_key in
      let account_id = Account_id.create pk_compressed Token_id.default in
      let account = Account.initialize account_id in
      let account' =
        { account with
          balance= Currency.Balance.of_int (Currency.Amount.to_int balance)
        ; nonce
        ; timing }
      in
      create_new_account_exn t account_id account' )
