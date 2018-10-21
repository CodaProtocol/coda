open Core
open Unsigned
module Intf = Merkle_ledger.Intf
module Ledger = Merkle_ledger.Ledger

let%test_module "test functor on in memory databases" =
  ( module struct
    module Key = Test_stubs.Key
    module Hash = Test_stubs.Hash
    module Account = Test_stubs.Account

    module Make (Depth : Intf.Depth) = struct
      include Ledger.Make (Key) (Account) (Hash) (Depth)

      type key = Key.t

      type account = Account.t

      type hash = Hash.t

      let load_ledger n b : t * key list =
        let ledger = create () in
        let keys = List.init n ~f:(fun i -> Int.to_string i) in
        List.iter keys ~f:(fun k ->
            ignore
            @@ create_account_exn ledger k
                 {Account.balance= UInt64.of_int b; public_key= k} ) ;
        (ledger, keys)
    end

    module L16 = Make (struct
      let depth = 16
    end)

    module L3 = Make (struct
      let depth = 3
    end)

    let%test "empty_length" =
      let ledger = L16.create () in
      L16.num_accounts ledger = 0

    let%test "length" =
      let n = 10 in
      let b = 100 in
      let ledger, _ = L16.load_ledger n b in
      L16.num_accounts ledger = n

    let get (type t key account) (module L
        : Merkle_ledger.Ledger_intf.S with type t = t and type key = key and type account = 
          account) ledger public_key =
      let open Option.Let_syntax in
      let%bind location = L.location_of_key ledger public_key in
      L.get ledger location

    let set (type t key account location) (module L
        : Merkle_ledger.Ledger_intf.S with type t = t and type key = key and type account = 
          account and type Location.t = location) ledger public_key account =
      ignore @@ L.create_account_exn ledger public_key account

    let gkey = Option.map ~f:(Fn.compose UInt64.to_int Account.balance)

    let%test "key_retrieval" =
      let b = 100 in
      let ledger, keys = L16.load_ledger 10 b in
      Some 100 = gkey (get (module L16) ledger (List.nth_exn keys 0))

    let%test "idx_retrieval" =
      let b = 100 in
      let ledger, _keys = L16.load_ledger 10 b in
      L16.get_at_index_exn ledger 0 |> Account.balance = UInt64.of_int 100

    let%test "key_nonexist" =
      let b = 100 in
      let ledger, _ = L16.load_ledger 10 b in
      None = L16.location_of_key ledger "aintioaerntnearst"

    let%test "idx_nonexist" =
      let b = 100 in
      let ledger, _keys = L16.load_ledger 10 b in
      None = get (module L16) ledger "1234567"

    let%test_unit "modify_account" =
      let initial_balance = 100 in
      let ledger, keys = L16.load_ledger 10 initial_balance in
      let public_key = List.nth_exn keys 0 in
      assert (Some initial_balance = gkey @@ get (module L16) ledger public_key) ;
      set (module L16) ledger public_key {balance= UInt64.of_int 50; public_key} ;
      assert (Some 50 = gkey @@ get (module L16) ledger public_key)

    let%test_unit "update_account" =
      let b = 100 in
      let ledger, keys = L16.load_ledger 10 b in
      let public_key = List.nth_exn keys 0 in
      L16.update ledger public_key ~f:(function
        | None -> assert false
        | Some {balance; public_key} ->
            {balance= UInt64.succ balance; public_key} ) ;
      assert (Some (b + 1) = gkey @@ get (module L16) ledger public_key)

    let%test_unit "modify_account_by_idx" =
      let b = 100 in
      let ledger, _ = L16.load_ledger 10 b in
      let idx = 0 in
      assert (
        L16.get_at_index_exn ledger idx |> Account.balance = UInt64.of_int 100
      ) ;
      let new_b = UInt64.of_int 50 in
      L16.set_at_index_exn ledger idx
        {balance= new_b; public_key= Int.to_string idx} ;
      assert (L16.get_at_index_exn ledger idx |> Account.balance = new_b)

    let compose_hash n hash =
      let rec go i hash =
        if i = n then hash
        else
          let hash = Hash.merge ~height:i hash hash in
          go (i + 1) hash
      in
      go 0 hash

    let%test "merkle_root" =
      let ledger = L16.create () in
      let root = L16.merkle_root ledger in
      compose_hash 16 Hash.empty = root

    let%test "merkle_root_nonempty" =
      let l = (1 lsl (3 - 1)) + 1 in
      let ledger, _ = L3.load_ledger l 1 in
      let root = L3.merkle_root ledger in
      Hash.empty <> root

    let%test_unit "merkle_root_edit" =
      let b1 = 10 in
      let b2 = UInt64.of_int 50 in
      let n = 10 in
      let ledger, keys = L16.load_ledger n b1 in
      let public_key = List.nth_exn keys 0 in
      let root0 = L16.merkle_root ledger in
      assert (Hash.empty <> root0) ;
      set (module L16) ledger public_key {balance= b2; public_key} ;
      let root1 = L16.merkle_root ledger in
      assert (root1 <> root0) ;
      set (module L16) ledger public_key {balance= UInt64.of_int b1; public_key} ;
      let root2 = L16.merkle_root ledger in
      assert (root2 = root0) ;
      set (module L16) ledger public_key {balance= b2; public_key} ;
      let root3 = L16.merkle_root ledger in
      assert (root3 = root1)

    module Path = Merkle_ledger.Merkle_path.Make (Hash)

    let check_path account (path: Path.t) root =
      Path.check_path path (Hash.hash_account account) root

    let merkle_path (type t key hash) (module L
        : Merkle_ledger.Ledger_intf.S with type t = t and type key = key and type hash = 
          hash) ledger public_key =
      L.location_of_key ledger public_key
      |> Option.value_exn |> L.merkle_path ledger

    let%test_unit "merkle_path" =
      let b1 = 10 in
      List.iter
        (List.range ~stop:`inclusive 1 (1 lsl 3))
        ~f:(fun n ->
          let ledger, keys = L3.load_ledger n b1 in
          let key = List.nth_exn keys 0 in
          let path = merkle_path (module L3) ledger key in
          let account = get (module L3) ledger key |> Option.value_exn in
          let root = L3.merkle_root ledger in
          assert (List.length path = 3) ;
          assert (check_path account path root) )

    let%test_unit "little_merkle_path" =
      let b1 = 10 in
      List.iter
        (List.range ~stop:`inclusive 1 (1 lsl 3))
        ~f:(fun n ->
          let ledger, keys = L3.load_ledger n b1 in
          let key = List.nth_exn keys 0 in
          let path = merkle_path (module L3) ledger key in
          let account = get (module L3) ledger key |> Option.value_exn in
          let root = L3.merkle_root ledger in
          assert (List.length path = 3) ;
          assert (check_path account path root) )

    let%test_unit "merkle_path_at_index" =
      let b1 = 10 in
      let idx = 0 in
      List.iter (List.range 1 20) ~f:(fun n ->
          let ledger, _ = L16.load_ledger n b1 in
          let path = L16.merkle_path_at_index ledger idx in
          let account = L16.get_at_index_exn ledger idx in
          let root = L16.merkle_root ledger in
          assert (List.length path = 16) ;
          assert (check_path account path root) )

    let%test_unit "merkle_path_edits" =
      let b1 = 10 in
      let b2 = 50 in
      let n = 10 in
      let ledger, keys = L16.load_ledger n b1 in
      List.iter (List.range 0 n) ~f:(fun i ->
          let public_key = List.nth_exn keys i in
          set
            (module L16)
            ledger public_key
            {balance= UInt64.of_int b2; public_key} ;
          let path = merkle_path (module L16) ledger public_key in
          let account =
            get (module L16) ledger public_key |> Option.value_exn
          in
          let root = L16.merkle_root ledger in
          assert (check_path account path root) )

    let%test_unit "set_inner_can_copy_correctly" =
      let rec all_inner_of a =
        if L3.Addr.depth a = L3.depth - 1 then []
        else
          let lc = L3.Addr.child a Left in
          let rc = L3.Addr.child a Right in
          match (lc, rc) with
          | Ok lc, Ok rc -> [lc; rc] @ all_inner_of lc @ all_inner_of rc
          | _ -> []
      in
      let n = 8 in
      let b1 = 1 in
      let b2 = 2 in
      let ledger1, _ = L3.load_ledger n b1 in
      let ledger2, _ = L3.load_ledger n b2 in
      L3.recompute_tree ledger1 ;
      L3.recompute_tree ledger2 ;
      let all_children = all_inner_of (L3.Addr.root ()) in
      List.iter all_children ~f:(fun x ->
          let src = L3.get_inner_hash_at_addr_exn ledger2 x in
          L3.set_inner_hash_at_addr_exn ledger1 x src ) ;
      List.iter (List.range 0 8) ~f:(fun x ->
          let src = L3.get_at_index_exn ledger2 x in
          L3.set_at_index_exn ledger1 x src ) ;
      assert (L3.merkle_root ledger1 = L3.merkle_root ledger2)

    let%test_unit "set_inner_hash_at_addr_exn t a h ; \
                   get_inner_hash_at_addr_exn t a = h" =
      let rec repeated n f r = if n > 0 then repeated (n - 1) f (f r) else r in
      let rec mk_addr ix h a =
        if h = 0 then a
        else if ix land 1 = 1 then
          mk_addr (ix lsr 1) (h - 1) (L16.Addr.child_exn a Right)
        else mk_addr (ix lsr 1) (h - 1) (L16.Addr.child_exn a Left)
      in
      let count = 8192 in
      let ledger, _ = L16.load_ledger count 1 in
      let mr_start = L16.merkle_root ledger in
      let max_height = Int.ceil_log2 count in
      let hash_to_set = Hash.(merge ~height:80 empty empty) in
      let open Quickcheck.Generator in
      Quickcheck.test
        (tuple2 (Int.gen_incl 0 8192) (Int.gen_incl 0 (max_height - 1)))
        ~f:(fun (idx, height) ->
          let a =
            mk_addr idx height
              (repeated (L16.depth - max_height)
                 (fun a -> L16.Addr.child_exn a Left)
                 (L16.Addr.root ()))
          in
          let old_hash = L16.get_inner_hash_at_addr_exn ledger a in
          L16.set_inner_hash_at_addr_exn ledger a hash_to_set ;
          let res =
            [%test_result : Hash.t] ~equal:Hash.equal
              (L16.get_inner_hash_at_addr_exn ledger a)
              ~expect:hash_to_set
          in
          L16.set_inner_hash_at_addr_exn ledger a old_hash ;
          res ) ;
      assert (mr_start = L16.merkle_root ledger)
  end )
