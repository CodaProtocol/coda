open Core
module Balance = Currency.Balance

module Account = struct
  (* want bin_io, not available with Account.t *)
  type t = Mina_base.Account.Stable.Latest.t
  [@@deriving bin_io_unversioned, sexp, equal, compare, hash, yojson]

  type key = Mina_base.Account.Key.Stable.Latest.t
  [@@deriving bin_io_unversioned, sexp, equal, compare, hash]

  (* use Account items needed *)
  let empty = Mina_base.Account.empty

  let public_key = Mina_base.Account.public_key

  let identifier = Mina_base.Account.identifier

  let key_gen = Mina_base.Account.key_gen

  let gen = Mina_base.Account.gen

  let create = Mina_base.Account.create

  let balance Mina_base.Account.Poly.{ balance; _ } = balance

  let update_balance t bal = { t with Mina_base.Account.Poly.balance = bal }

  let token Mina_base.Account.Poly.{ token_id; _ } = token_id

  let token_owner Mina_base.Account.Poly.{ token_permissions; _ } =
    match token_permissions with
    | Mina_base.Token_permissions.Token_owned _ ->
        true
    | Not_owned _ ->
        false
end

module Receipt = Mina_base.Receipt

module Hash = struct
  module T = struct
    type t = Md5.t [@@deriving sexp, hash, compare, bin_io_unversioned, equal]

    let to_string = Md5.to_hex

    let to_yojson t = `String (Md5.to_hex t)

    let of_yojson = function
      | `String s ->
          Ok (Md5.of_hex_exn s)
      | _ ->
          Error "expected string"
  end

  include T
  include Hashable.Make_binable (T)

  (* to prevent pre-image attack,
   * important impossible to create an account such that (merge a b = hash_account account) *)

  let hash_account account =
    Md5.digest_string ("0" ^ Format.sprintf !"%{sexp: Account.t}" account)

  let merge ~height a b =
    let res =
      Md5.digest_string
        (sprintf "test_ledger_%d:" height ^ Md5.to_hex a ^ Md5.to_hex b)
    in
    res

  let empty_account = hash_account Account.empty
end

module Intf = Merkle_ledger.Intf

module In_memory_kvdb : Intf.Key_value_database with type config := string =
struct
  module Bigstring_frozen = struct
    module T = struct
      include Bigstring.Stable.V1

      (* we're not mutating Bigstrings, which would invalidate hashes
         OK to use these hash functions
      *)
      let hash = hash_t_frozen

      let hash_fold_t = hash_fold_t_frozen
    end

    include T
    include Hashable.Make_binable (T)
  end

  type t =
    { uuid : Uuid.Stable.V1.t
    ; table : Bigstring_frozen.t Bigstring_frozen.Table.t
    }
  [@@deriving sexp]

  let to_alist t =
    let unsorted = Bigstring_frozen.Table.to_alist t.table in
    (* sort by key *)
    List.sort
      ~compare:(fun (k1, _) (k2, _) -> Bigstring_frozen.compare k1 k2)
      unsorted

  let get_uuid t = t.uuid

  let create _ =
    { uuid = Uuid_unix.create (); table = Bigstring_frozen.Table.create () }

  let create_checkpoint t _ =
    { uuid = Uuid_unix.create ()
    ; table =
        Bigstring_frozen.Table.of_alist_exn
        @@ Bigstring_frozen.Table.to_alist t.table
    }

  let close _ = ()

  let get t ~key = Bigstring_frozen.Table.find t.table key

  let set t ~key ~data = Bigstring_frozen.Table.set t.table ~key ~data

  let set_batch t ?(remove_keys = []) ~key_data_pairs =
    List.iter key_data_pairs ~f:(fun (key, data) -> set t ~key ~data) ;
    List.iter remove_keys ~f:(fun key ->
        Bigstring_frozen.Table.remove t.table key)

  let remove t ~key = Bigstring_frozen.Table.remove t.table key

  let make_checkpoint _ _ = ()
end

module Storage_locations : Intf.Storage_locations = struct
  (* TODO: The name of this value should be dynamically generated per test run*)
  let key_value_db_dir = ""
end

module Key = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Mina_base.Account.Key.Stable.V1.t
      [@@deriving sexp, equal, compare, hash]

      let to_latest = Fn.id
    end
  end]

  let to_string = Signature_lib.Public_key.Compressed.to_base58_check

  let gen = Account.key_gen

  let empty : t = Account.empty.public_key

  let gen_keys num_keys =
    Quickcheck.random_value (Quickcheck.Generator.list_with_length num_keys gen)

  include Hashable.Make_binable (Stable.Latest)
  include Comparable.Make (Stable.Latest)
end

module Token_id = Mina_base.Token_id

module Account_id = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Mina_base.Account_id.Stable.V1.t
      [@@deriving sexp, equal, compare, hash]

      let to_latest = Fn.id
    end
  end]

  include Hashable.Make_binable (Stable.Latest)
  include Comparable.Make (Stable.Latest)

  let create = Mina_base.Account_id.create

  let token_id = Mina_base.Account_id.token_id

  let public_key = Mina_base.Account_id.public_key

  (* TODO: Non-default tokens *)
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map pk = Key.gen in
    create pk Token_id.default

  let gen_accounts num_accounts =
    Quickcheck.random_value
      (Quickcheck.Generator.list_with_length num_accounts gen)
end

module Base_inputs = struct
  module Key = Key
  module Account_id = Account_id
  module Token_id = Token_id
  module Balance = Balance
  module Account = Account
  module Hash = Hash
end
