open Core
open Unsigned

(* add functions to library module Bigstring so we can derive hash for the type t below *)
module Bigstring = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Core_kernel.Bigstring.Stable.V1.t [@@deriving sexp, compare]

      let to_latest = Fn.id

      let equal = Bigstring.equal

      let hash t = Bigstring.to_string t |> String.hash

      let hash_fold_t hash_state t =
        String.hash_fold_t hash_state (Bigstring.to_string t)
    end
  end]

  [%%define_locally
  Bigstring.(get, length, equal, create, to_string, set, blit, sub)]

  include Hashable.Make (Stable.Latest)
end

module T = struct
  (* Locations are a bitstring prefixed by a byte. In the case of accounts, the prefix
   * byte is 0xfe. In the case of a hash node in the merkle tree, the prefix is between
   * 1 and N (where N is the height of the root of the merkle tree, with 1 representing
   * the leafs of the tree, and N representing the root of the merkle tree. For account
   * and node locations, the bitstring represents the path in the tree where that node exists.
   * For all other locations (generic locations), the prefix is 0xff. Generic locations can contain
   * any bitstring.
   *)

  module Addr = Merkle_address

  module Prefix = struct
    let generic = UInt8.of_int 0xff

    let account = UInt8.of_int 0xfe

    let hash ~ledger_depth depth = UInt8.of_int (ledger_depth - depth)
  end

  type t = Generic of Bigstring.t | Account of Addr.t | Hash of Addr.t
  [@@deriving hash, sexp, compare]

  let is_generic = function Generic _ -> true | _ -> false

  let is_account = function Account _ -> true | _ -> false

  let is_hash = function Hash _ -> true | _ -> false

  let height ~ledger_depth : t -> int = function
    | Generic _ ->
        raise (Invalid_argument "height: generic location has no height")
    | Account _ ->
        0
    | Hash path ->
        Addr.height ~ledger_depth path

  let root_hash : t = Hash (Addr.root ())

  let last_direction path =
    Direction.of_bool (Addr.get path (Addr.depth path - 1) <> 0)

  let build_generic (data : Bigstring.t) : t = Generic data

  let parse ~ledger_depth (str : Bigstring.t) : (t, unit) Result.t =
    let prefix = Bigstring.get str 0 |> Char.to_int |> UInt8.of_int in
    let data = Bigstring.sub str ~pos:1 ~len:(Bigstring.length str - 1) in
    if UInt8.equal prefix Prefix.generic then Result.return (Generic data)
    else
      let path = Addr.of_byte_string (Bigstring.to_string data) in
      let slice_path = Addr.slice path 0 in
      if UInt8.equal prefix Prefix.account then
        Result.return (Account (slice_path ledger_depth))
      else if UInt8.to_int prefix <= ledger_depth then
        Result.return (Hash (slice_path (ledger_depth - UInt8.to_int prefix)))
      else Result.fail ()

  let prefix_bigstring prefix src =
    let src_len = Bigstring.length src in
    let dst = Bigstring.create (src_len + 1) in
    Bigstring.set dst 0 (Char.of_int_exn (UInt8.to_int prefix)) ;
    Bigstring.blit ~src ~src_pos:0 ~dst ~dst_pos:1 ~len:src_len ;
    dst

  let to_path_exn = function
    | Account path | Hash path ->
        path
    | Generic _ ->
        raise (Invalid_argument "to_path_exn: generic does not have a path")

  let serialize ~ledger_depth = function
    | Generic data ->
        prefix_bigstring Prefix.generic data
    | Account path ->
        assert (Addr.depth path = ledger_depth) ;
        prefix_bigstring Prefix.account (Addr.serialize ~ledger_depth path)
    | Hash path ->
        assert (Addr.depth path <= ledger_depth) ;
        prefix_bigstring
          (Prefix.hash ~ledger_depth (Addr.depth path))
          (Addr.serialize ~ledger_depth path)

  let parent : t -> t = function
    | Generic _ ->
        raise (Invalid_argument "parent: generic locations have no parent")
    | Account _ ->
        raise (Invalid_argument "parent: account locations have no parent")
    | Hash path ->
        assert (Addr.depth path > 0) ;
        Hash (Addr.parent_exn path)

  let next : t -> t Option.t = function
    | Generic _ ->
        raise
          (Invalid_argument "next: generic locations have no next location")
    | Account path ->
        Addr.next path |> Option.map ~f:(fun next -> Account next)
    | Hash path ->
        Addr.next path |> Option.map ~f:(fun next -> Hash next)

  let prev : t -> t Option.t = function
    | Generic _ ->
        raise
          (Invalid_argument "prev: generic locations have no prev location")
    | Account path ->
        Addr.prev path |> Option.map ~f:(fun prev -> Account prev)
    | Hash path ->
        Addr.prev path |> Option.map ~f:(fun prev -> Hash prev)

  let sibling : t -> t = function
    | Generic _ ->
        raise (Invalid_argument "sibling: generic locations have no sibling")
    | Account path ->
        Account (Addr.sibling path)
    | Hash path ->
        Hash (Addr.sibling path)

  let order_siblings (location : t) (base : 'a) (sibling : 'a) : 'a * 'a =
    match last_direction (to_path_exn location) with
    | Left ->
        (base, sibling)
    | Right ->
        (sibling, base)

  type location = t [@@deriving sexp, compare]

  include Comparable.Make (struct
    type t = location [@@deriving sexp, compare]
  end)
end
