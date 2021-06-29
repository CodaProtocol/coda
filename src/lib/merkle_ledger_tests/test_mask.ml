(* test_make.ml -- tests Merkle mask connected to underlying Merkle tree *)

open Core
open Test_stubs
module Database = Merkle_ledger.Database

module type Test_intf = sig
  val depth : int

  module Location : Merkle_ledger.Location_intf.S

  module Base :
    Merkle_mask.Base_merkle_tree_intf.S
    with module Addr = Location.Addr
     and module Location = Location
     and type account := Account.t
     and type root_hash := Hash.t
     and type hash := Hash.t
     and type key := Key.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t

  module Mask :
    Merkle_mask.Masking_merkle_tree_intf.S
    with module Location = Location
     and module Attached.Addr = Location.Addr
    with type account := Account.t
     and type location := Location.t
     and type hash := Hash.t
     and type parent := Base.t
     and type key := Key.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t

  module Maskable :
    Merkle_mask.Maskable_merkle_tree_intf.S
    with module Location = Location
     and module Addr = Location.Addr
    with type account := Account.t
     and type root_hash := Hash.t
     and type hash := Hash.t
     and type unattached_mask := Mask.t
     and type attached_mask := Mask.Attached.t
     and type t := Base.t
     and type key := Key.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t

  val with_instances : (Base.t -> Mask.t -> 'a) -> 'a

  (** Here we provide a base ledger and two layers of attached masks
          * one ontop another *)
  val with_chain :
       (   Base.t
        -> mask:Mask.Attached.t
        -> mask_as_base:Base.t
        -> mask2:Mask.Attached.t
        -> 'a)
    -> 'a
end

module Make (Test : Test_intf) = struct
  module Maskable = Test.Maskable
  module Mask = Test.Mask

  let directions =
    let rec add_direction count b accum =
      if count >= Test.depth then accum
      else
        let dir = if b then Direction.Right else Direction.Left in
        add_direction (count + 1) (not b) (dir :: accum)
    in
    add_direction 0 false []

  let dummy_address = Test.Location.Addr.of_directions directions

  let dummy_location = Test.Location.Account dummy_address

  let dummy_account = Quickcheck.random_value Account.gen

  let create_new_account_exn mask account =
    let public_key = Account.identifier account in
    let action, location =
      Mask.Attached.get_or_create_account mask public_key account
      |> Or_error.ok_exn
    in
    match action with
    | `Existed ->
        failwith "Expected to allocate a new account"
    | `Added ->
        location

  let create_existing_account_exn mask account =
    let public_key = Account.identifier account in
    let action, location =
      Mask.Attached.get_or_create_account mask public_key account
      |> Or_error.ok_exn
    in
    match action with
    | `Existed ->
        Mask.Attached.set mask location account ;
        location
    | `Added ->
        failwith "Expected to re-use an existing account"

  let parent_create_new_account_exn parent account =
    let public_key = Account.identifier account in
    let action, location =
      Maskable.get_or_create_account parent public_key account
      |> Or_error.ok_exn
    in
    match action with
    | `Existed ->
        failwith "Expected to allocate a new account"
    | `Added ->
        location

  let%test "parent, mask agree on set" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        Maskable.set maskable dummy_location dummy_account ;
        Mask.Attached.set attached_mask dummy_location dummy_account ;
        let maskable_result = Maskable.get maskable dummy_location in
        let mask_result = Mask.Attached.get attached_mask dummy_location in
        Option.is_some maskable_result
        && Option.is_some mask_result
        &&
        let maskable_account = Option.value_exn maskable_result in
        let mask_account = Option.value_exn mask_result in
        Account.equal maskable_account mask_account )

  let compare_maskable_mask_hashes ?(check_hash_in_mask = false) maskable mask
      addr =
    let root = Mask.Attached.Addr.root () in
    let rec test_hashes_at_address addr =
      ( (not check_hash_in_mask)
      || Mask.Attached.For_testing.address_in_mask mask addr )
      &&
      let maybe_mask_hash = Mask.Attached.get_hash mask addr in
      Option.is_some maybe_mask_hash
      &&
      let mask_hash = Option.value_exn maybe_mask_hash in
      let maskable_hash = Maskable.get_inner_hash_at_addr_exn maskable addr in
      Hash.equal mask_hash maskable_hash
      &&
      if Mask.Addr.equal root addr then true
      else test_hashes_at_address (Mask.Attached.Addr.parent_exn addr)
    in
    test_hashes_at_address addr

  let%test "parent, mask agree on set" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        Maskable.set maskable dummy_location dummy_account ;
        Mask.Attached.set attached_mask dummy_location dummy_account ;
        let maskable_result = Maskable.get maskable dummy_location in
        let mask_result = Mask.Attached.get attached_mask dummy_location in
        Option.is_some maskable_result
        && Option.is_some mask_result
        &&
        let maskable_account = Option.value_exn maskable_result in
        let mask_account = Option.value_exn mask_result in
        Account.equal maskable_account mask_account )

  let%test "parent, mask agree on hashes; set in both mask and parent" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        (* set in both parent and mask *)
        Maskable.set maskable dummy_location dummy_account ;
        Mask.Attached.set attached_mask dummy_location dummy_account ;
        (* verify all hashes to root are same in mask and parent *)
        compare_maskable_mask_hashes ~check_hash_in_mask:true maskable
          attached_mask dummy_address )

  let%test "parent, mask agree on hashes; set only in parent" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        (* set only in parent *)
        Maskable.set maskable dummy_location dummy_account ;
        (* verify all hashes to root are same in mask and parent *)
        compare_maskable_mask_hashes maskable attached_mask dummy_address )

  let%test "mask delegates to parent" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        (* set to parent, get from mask *)
        Maskable.set maskable dummy_location dummy_account ;
        let mask_result = Mask.Attached.get attached_mask dummy_location in
        Option.is_some mask_result
        &&
        let mask_account = Option.value_exn mask_result in
        Account.equal dummy_account mask_account )

  let%test "mask prune after parent notification" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        (* set to mask *)
        Mask.Attached.set attached_mask dummy_location dummy_account ;
        (* verify account is in mask *)
        if
          Mask.Attached.For_testing.location_in_mask attached_mask
            dummy_location
        then (
          Maskable.set maskable dummy_location dummy_account ;
          (* verify account pruned from mask *)
          not
            (Mask.Attached.For_testing.location_in_mask attached_mask
               dummy_location) )
        else false )

  let%test "commit puts mask contents in parent, flushes mask" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        (* set to mask *)
        Mask.Attached.set attached_mask dummy_location dummy_account ;
        (* verify account is in mask *)
        if
          Mask.Attached.For_testing.location_in_mask attached_mask
            dummy_location
        then (
          Mask.Attached.commit attached_mask ;
          (* verify account no longer in mask but is in parent *)
          (not
             (Mask.Attached.For_testing.location_in_mask attached_mask
                dummy_location))
          && Option.is_some (Maskable.get maskable dummy_location) )
        else false )

  let%test_unit "commit at layer2, dumps to layer1, not in base" =
    Test.with_chain (fun base ~mask:level1 ~mask_as_base:_ ~mask2:level2 ->
        Mask.Attached.set level2 dummy_location dummy_account ;
        (* verify account is in the layer2 mask *)
        assert (
          Mask.Attached.For_testing.location_in_mask level2 dummy_location ) ;
        Mask.Attached.commit level2 ;
        (* account is no longer in layer2 *)
        assert (
          not
            (Mask.Attached.For_testing.location_in_mask level2 dummy_location)
        ) ;
        (* account is still not in base *)
        assert (Option.is_none @@ Maskable.get base dummy_location) ;
        (* account is present in layer1 *)
        assert (
          Mask.Attached.For_testing.location_in_mask level1 dummy_location ) )

  let%test "register and unregister mask" =
    Test.with_instances (fun maskable mask ->
        let (attached_mask : Mask.Attached.t) =
          Maskable.register_mask maskable mask
        in
        try
          let (_unattached_mask : Mask.t) =
            Maskable.unregister_mask_exn ~loc:__LOC__ attached_mask
          in
          true
        with Failure _ -> false )

  let%test_unit "root hash invariant if interior changes but not accounts" =
    if Test.depth <= 8 then
      Test.with_instances (fun maskable mask ->
          let attached_mask = Maskable.register_mask maskable mask in
          Mask.Attached.set attached_mask dummy_location dummy_account ;
          (* Make some accounts *)
          let num_accounts = 1 lsl Test.depth in
          let gen_values gen =
            Quickcheck.random_value
              (Quickcheck.Generator.list_with_length num_accounts gen)
          in
          let account_ids = Account_id.gen_accounts num_accounts in
          let balances = gen_values Balance.gen in
          let accounts =
            List.map2_exn account_ids balances ~f:(fun public_key balance ->
                Account.create public_key balance )
          in
          List.iter accounts ~f:(fun account ->
              ignore @@ create_new_account_exn attached_mask account ) ;
          (* Set some inner hashes *)
          let reset_hash_of_parent_of_index i =
            let a1 = List.nth_exn accounts i in
            let aid = Account.identifier a1 in
            let location =
              Mask.Attached.location_of_account attached_mask aid
              |> Option.value_exn
            in
            let addr = Test.Location.to_path_exn location in
            let parent_addr =
              Test.Location.Addr.parent addr |> Or_error.ok_exn
            in
            Mask.Attached.set_inner_hash_at_addr_exn attached_mask parent_addr
              Hash.empty_account
          in
          let root_hash = Mask.Attached.merkle_root attached_mask in
          reset_hash_of_parent_of_index 0 ;
          reset_hash_of_parent_of_index 3 ;
          let root_hash' = Mask.Attached.merkle_root attached_mask in
          assert (Hash.equal root_hash root_hash') )

  let%test "mask and parent agree on Merkle path" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        Mask.Attached.set attached_mask dummy_location dummy_account ;
        (* set affects hashes along the path P from location to the root, while the Merkle path for the location
               contains the siblings of P elements; to observe a hash in the Merkle path changed by the set, choose an
               address that is a sibling of an element in P; the Merkle path for that address will include a P element
            *)
        let address =
          dummy_address |> Maskable.Addr.parent_exn |> Maskable.Addr.sibling
        in
        let mask_merkle_path =
          Mask.Attached.merkle_path_at_addr_exn attached_mask address
        in
        Maskable.set maskable dummy_location dummy_account ;
        let maskable_merkle_path =
          Maskable.merkle_path_at_addr_exn maskable address
        in
        [%equal: Mask.Attached.Path.t] mask_merkle_path maskable_merkle_path )

  let%test "mask and parent agree on Merkle root before set" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let mask_merkle_root = Mask.Attached.merkle_root attached_mask in
        let maskable_merkle_root = Maskable.merkle_root maskable in
        Hash.equal mask_merkle_root maskable_merkle_root )

  let%test "mask and parent agree on Merkle root after set" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        (* the order of sets matters here; if we set in the mask first,
               the set in the maskable notifies the mask, which then removes
               the account, changing the Merkle root to what it was before the set
            *)
        Maskable.set maskable dummy_location dummy_account ;
        Mask.Attached.set attached_mask dummy_location dummy_account ;
        let mask_merkle_root = Mask.Attached.merkle_root attached_mask in
        let maskable_merkle_root = Maskable.merkle_root maskable in
        (* verify root address in mask *)
        Mask.Attached.For_testing.address_in_mask attached_mask
          (Mask.Addr.root ())
        && Hash.equal mask_merkle_root maskable_merkle_root )

  let%test_unit "add and retrieve a block of accounts" =
    (* see similar test in test_database *)
    if Test.depth <= 8 then
      Test.with_instances (fun maskable mask ->
          let attached_mask = Maskable.register_mask maskable mask in
          let num_accounts = 1 lsl Test.depth in
          let gen_values gen =
            Quickcheck.random_value
              (Quickcheck.Generator.list_with_length num_accounts gen)
          in
          let account_ids = Account_id.gen_accounts num_accounts in
          let balances = gen_values Balance.gen in
          let accounts =
            List.map2_exn account_ids balances ~f:(fun public_key balance ->
                Account.create public_key balance )
          in
          List.iter accounts ~f:(fun account ->
              ignore @@ create_new_account_exn attached_mask account ) ;
          let retrieved_accounts =
            List.map ~f:snd
            @@ Mask.Attached.get_all_accounts_rooted_at_exn attached_mask
                 (Mask.Addr.root ())
          in
          assert (List.length accounts = List.length retrieved_accounts) ;
          assert (List.equal Account.equal accounts retrieved_accounts) )

  let%test_unit "get_all_accounts should preserve the ordering of accounts by \
                 location with noncontiguous updates of accounts on the mask" =
    (* see similar test in test_database *)
    if Test.depth <= 8 then
      Test.with_chain (fun _ ~mask:mask1 ~mask_as_base:_ ~mask2 ->
          let num_accounts = 1 lsl Test.depth in
          let gen_values gen list_length =
            Quickcheck.random_value
              (Quickcheck.Generator.list_with_length list_length gen)
          in
          let account_ids = Account_id.gen_accounts num_accounts in
          let balances = gen_values Balance.gen num_accounts in
          let base_accounts =
            List.map2_exn account_ids balances ~f:(fun public_key balance ->
                Account.create public_key balance )
          in
          List.iter base_accounts ~f:(fun account ->
              ignore @@ create_new_account_exn mask1 account ) ;
          let num_subset =
            Quickcheck.random_value (Int.gen_incl 3 num_accounts)
          in
          let subset_indices, subset_accounts =
            List.permute
              (List.mapi base_accounts ~f:(fun index account -> (index, account)
               ))
            |> (Fn.flip List.take) num_subset
            |> List.unzip
          in
          let subset_balances = gen_values Balance.gen num_subset in
          let subset_updated_accounts =
            List.map2_exn subset_accounts subset_balances
              ~f:(fun account balance ->
                let updated_account = {account with balance} in
                ignore
                  ( create_existing_account_exn mask2 updated_account
                    : Test.Location.t ) ;
                updated_account )
          in
          let updated_accounts_map =
            Int.Map.of_alist_exn
              (List.zip_exn subset_indices subset_updated_accounts)
          in
          let expected_accounts =
            List.mapi base_accounts ~f:(fun index base_account ->
                Option.value
                  (Map.find updated_accounts_map index)
                  ~default:base_account )
          in
          let retrieved_accounts =
            List.map ~f:snd
            @@ Mask.Attached.get_all_accounts_rooted_at_exn mask2
                 (Mask.Addr.root ())
          in
          assert (
            Int.equal
              (List.length base_accounts)
              (List.length retrieved_accounts) ) ;
          assert (List.equal Account.equal expected_accounts retrieved_accounts)
      )

  let%test_unit "removing accounts from mask restores Merkle root" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let num_accounts = 5 in
        let account_ids = Account_id.gen_accounts num_accounts in
        let balances =
          Quickcheck.random_value
            (Quickcheck.Generator.list_with_length num_accounts Balance.gen)
        in
        let accounts = List.map2_exn account_ids balances ~f:Account.create in
        let merkle_root0 = Mask.Attached.merkle_root attached_mask in
        List.iter accounts ~f:(fun account ->
            ignore @@ create_new_account_exn attached_mask account ) ;
        let merkle_root1 = Mask.Attached.merkle_root attached_mask in
        (* adding accounts should change the Merkle root *)
        assert (not (Hash.equal merkle_root0 merkle_root1)) ;
        Mask.Attached.remove_accounts_exn attached_mask account_ids ;
        (* should see original Merkle root after removing the accounts *)
        let merkle_root2 = Mask.Attached.merkle_root attached_mask in
        assert (Hash.equal merkle_root2 merkle_root0) )

  let%test_unit "removing accounts from parent restores Merkle root" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let num_accounts = 5 in
        let account_ids = Account_id.gen_accounts num_accounts in
        let balances =
          Quickcheck.random_value
            (Quickcheck.Generator.list_with_length num_accounts Balance.gen)
        in
        let accounts = List.map2_exn account_ids balances ~f:Account.create in
        let merkle_root0 = Mask.Attached.merkle_root attached_mask in
        (* add accounts to parent *)
        List.iter accounts ~f:(fun account ->
            ignore @@ parent_create_new_account_exn maskable account ) ;
        (* observe Merkle root in mask *)
        let merkle_root1 = Mask.Attached.merkle_root attached_mask in
        (* adding accounts should change the Merkle root *)
        assert (not (Hash.equal merkle_root0 merkle_root1)) ;
        Mask.Attached.remove_accounts_exn attached_mask account_ids ;
        (* should see original Merkle root after removing the accounts *)
        let merkle_root2 = Mask.Attached.merkle_root attached_mask in
        assert (Hash.equal merkle_root2 merkle_root0) )

  let%test_unit "removing accounts from parent and mask restores Merkle root" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let num_accounts_parent = 5 in
        let num_accounts_mask = 5 in
        let num_accounts = num_accounts_parent + num_accounts_mask in
        let account_ids = Account_id.gen_accounts num_accounts in
        let balances =
          Quickcheck.random_value
            (Quickcheck.Generator.list_with_length num_accounts Balance.gen)
        in
        let accounts = List.map2_exn account_ids balances ~f:Account.create in
        let parent_accounts, mask_accounts =
          List.split_n accounts num_accounts_parent
        in
        let merkle_root0 = Mask.Attached.merkle_root attached_mask in
        (* add accounts to parent *)
        List.iter parent_accounts ~f:(fun account ->
            ignore @@ parent_create_new_account_exn maskable account ) ;
        (* add accounts to mask *)
        List.iter mask_accounts ~f:(fun account ->
            ignore @@ create_new_account_exn attached_mask account ) ;
        (* observe Merkle root in mask *)
        let merkle_root1 = Mask.Attached.merkle_root attached_mask in
        (* adding accounts should change the Merkle root *)
        assert (not (Hash.equal merkle_root0 merkle_root1)) ;
        (* remove accounts from mask and parent *)
        Mask.Attached.remove_accounts_exn attached_mask account_ids ;
        (* should see original Merkle root after removing the accounts *)
        let merkle_root2 = Mask.Attached.merkle_root attached_mask in
        assert (Hash.equal merkle_root2 merkle_root0) )

  let%test_unit "fold of addition over account balances in parent and mask" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let num_accounts_parent = 5 in
        let num_accounts_mask = 5 in
        let num_accounts = num_accounts_parent + num_accounts_mask in
        let account_ids = Account_id.gen_accounts num_accounts in
        let balances =
          Quickcheck.random_value
            (Quickcheck.Generator.list_with_length num_accounts Balance.gen)
        in
        let accounts = List.map2_exn account_ids balances ~f:Account.create in
        let total =
          List.fold balances ~init:0 ~f:(fun accum balance ->
              Balance.to_int balance + accum )
        in
        let parent_accounts, mask_accounts =
          List.split_n accounts num_accounts_parent
        in
        (* add accounts to parent *)
        List.iter parent_accounts ~f:(fun account ->
            ignore @@ parent_create_new_account_exn maskable account ) ;
        (* add accounts to mask *)
        List.iter mask_accounts ~f:(fun account ->
            ignore @@ create_new_account_exn attached_mask account ) ;
        (* folding over mask also folds over maskable *)
        let retrieved_total =
          Mask.Attached.foldi attached_mask ~init:0
            ~f:(fun _addr total account ->
              Balance.to_int (Account.balance account) + total )
        in
        assert (Int.equal retrieved_total total) )

  let%test_unit "masking in to_list" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let num_accounts = 10 in
        let account_ids = Account_id.gen_accounts num_accounts in
        (* parent balances all non-zero *)
        let balances =
          List.init num_accounts ~f:(fun n -> Balance.of_int (n + 1))
        in
        let parent_accounts =
          List.map2_exn account_ids balances ~f:Account.create
        in
        (* add accounts to parent *)
        List.iter parent_accounts ~f:(fun account ->
            ignore @@ parent_create_new_account_exn maskable account ) ;
        (* all accounts in parent to_list *)
        let parent_list = Maskable.to_list maskable in
        let zero_balance account =
          Account.update_balance account Balance.zero
        in
        (* put same accounts in mask, but with zero balance *)
        let mask_accounts = List.map parent_accounts ~f:zero_balance in
        List.iter mask_accounts ~f:(fun account ->
            ignore @@ create_existing_account_exn attached_mask account ) ;
        let mask_list = Mask.Attached.to_list attached_mask in
        (* same number of accounts after adding them to mask *)
        assert (Int.equal (List.length parent_list) (List.length mask_list)) ;
        (* should only see the zero balances in mask list *)
        let is_in_same_order =
          List.for_all2_exn parent_list mask_list
            ~f:(fun parent_account mask_account ->
              Account_id.equal
                (Account.identifier parent_account)
                (Account.identifier mask_account) )
        in
        assert is_in_same_order ;
        assert (
          List.for_all mask_list ~f:(fun account ->
              Balance.equal (Account.balance account) Balance.zero ) ) )

  let%test_unit "masking in foldi" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let num_accounts = 10 in
        let account_ids = Account_id.gen_accounts num_accounts in
        (* parent balances all non-zero *)
        let balances =
          List.init num_accounts ~f:(fun n -> Balance.of_int (n + 1))
        in
        let parent_accounts =
          List.map2_exn account_ids balances ~f:Account.create
        in
        (* add accounts to parent *)
        List.iter parent_accounts ~f:(fun account ->
            ignore @@ parent_create_new_account_exn maskable account ) ;
        let balance_summer _addr accum acct =
          accum + Balance.to_int (Account.balance acct)
        in
        let parent_sum = Maskable.foldi maskable ~init:0 ~f:balance_summer in
        (* non-zero sum of parent account balances *)
        assert (Int.equal parent_sum 55) (* HT Gauss *) ;
        let zero_balance account =
          Account.update_balance account Balance.zero
        in
        (* put same accounts in mask, but with zero balance *)
        let mask_accounts = List.map parent_accounts ~f:zero_balance in
        List.iter mask_accounts ~f:(fun account ->
            ignore @@ create_existing_account_exn attached_mask account ) ;
        let mask_sum =
          Mask.Attached.foldi attached_mask ~init:0 ~f:balance_summer
        in
        (* sum should not include any parent balances *)
        assert (Int.equal mask_sum 0) )

  let%test_unit "create_empty doesn't modify the hash" =
    Test.with_instances (fun maskable mask ->
        let open Mask.Attached in
        let ledger = Maskable.register_mask maskable mask in
        let key = List.nth_exn (Account_id.gen_accounts 1) 0 in
        let start_hash = merkle_root ledger in
        match
          get_or_create_account ledger key Account.empty |> Or_error.ok_exn
        with
        | `Existed, _ ->
            failwith
              "create_empty with empty ledger somehow already has that key?"
        | `Added, _new_loc ->
            [%test_eq: Hash.t] start_hash (merkle_root ledger) )

  let%test_unit "reuse of locations for removed accounts" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let num_accounts = 5 in
        let account_ids = Account_id.gen_accounts num_accounts in
        let balances =
          Quickcheck.random_value
            (Quickcheck.Generator.list_with_length num_accounts Balance.gen)
        in
        let accounts = List.map2_exn account_ids balances ~f:Account.create in
        assert (
          Option.is_none
            (Mask.Attached.For_testing.current_location attached_mask) ) ;
        (* add accounts to mask *)
        List.iter accounts ~f:(fun account ->
            ignore @@ create_new_account_exn attached_mask account ) ;
        assert (
          Option.is_some
            (Mask.Attached.For_testing.current_location attached_mask) ) ;
        (* remove accounts *)
        Mask.Attached.remove_accounts_exn attached_mask account_ids ;
        assert (
          Option.is_none
            (Mask.Attached.For_testing.current_location attached_mask) ) )

  let%test_unit "num_accounts for unique keys in mask and parent" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let num_accounts = 5 in
        let account_ids = Account_id.gen_accounts num_accounts in
        let balances =
          Quickcheck.random_value
            (Quickcheck.Generator.list_with_length num_accounts Balance.gen)
        in
        let accounts = List.map2_exn account_ids balances ~f:Account.create in
        (* add accounts to mask *)
        List.iter accounts ~f:(fun account ->
            ignore @@ create_new_account_exn attached_mask account ) ;
        let mask_num_accounts_before =
          Mask.Attached.num_accounts attached_mask
        in
        (* add same accounts to parent *)
        List.iter accounts ~f:(fun account ->
            ignore @@ parent_create_new_account_exn maskable account ) ;
        let parent_num_accounts = Maskable.num_accounts maskable in
        (* should not change number of accounts in mask, since they have the same keys *)
        let mask_num_accounts_after =
          Mask.Attached.num_accounts attached_mask
        in
        (* the number of accounts in parent, mask should agree *)
        assert (
          Int.equal parent_num_accounts (List.length accounts)
          && Int.equal parent_num_accounts mask_num_accounts_before
          && Int.equal parent_num_accounts mask_num_accounts_after ) )

  let%test_unit "Mask reparenting works" =
    Test.with_chain (fun base ~mask:m1 ~mask_as_base ~mask2:m2 ->
        let num_accounts = 3 in
        let account_ids = Account_id.gen_accounts num_accounts in
        let balances =
          Quickcheck.random_value
            (Quickcheck.Generator.list_with_length num_accounts Balance.gen)
        in
        let accounts = List.map2_exn account_ids balances ~f:Account.create in
        match accounts with
        | [a1; a2; a3] ->
            let loc1 = parent_create_new_account_exn base a1 in
            let loc2 = create_new_account_exn m1 a2 in
            let loc3 = create_new_account_exn m2 a3 in
            let locs = [(loc1, a1); (loc2, a2); (loc3, a3)] in
            (* all accounts are here *)
            List.iter locs ~f:(fun (loc, a) ->
                [%test_result: Account.t option]
                  ~message:"All accounts are accessible from m2"
                  ~expect:(Some a) (Mask.Attached.get m2 loc) ) ;
            [%test_result: Account.t option] ~message:"a1 is in base"
              ~expect:(Some a1) (Test.Base.get base loc1) ;
            Mask.Attached.commit m1 ;
            [%test_result: Account.t option] ~message:"a2 is in base"
              ~expect:(Some a2) (Test.Base.get base loc2) ;
            Maskable.remove_and_reparent_exn mask_as_base m1 ;
            [%test_result: Account.t option] ~message:"a1 is in base"
              ~expect:(Some a1) (Test.Base.get base loc1) ;
            [%test_result: Account.t option] ~message:"a2 is in base"
              ~expect:(Some a2) (Test.Base.get base loc2) ;
            (* all accounts are still here *)
            List.iter locs ~f:(fun (loc, a) ->
                [%test_result: Account.t option]
                  ~message:"All accounts are accessible from m2"
                  ~expect:(Some a) (Mask.Attached.get m2 loc) )
        | _ ->
            failwith "unexpected" )

  let%test_unit "setting an account in the parent doesn't remove the masked \
                 copy if the mask is still dirty for that account" =
    Test.with_instances (fun maskable mask ->
        let attached_mask = Maskable.register_mask maskable mask in
        let k = Account_id.gen_accounts 1 |> List.hd_exn in
        let acct1 = Account.create k (Balance.of_int 10) in
        let loc =
          Mask.Attached.get_or_create_account attached_mask k acct1
          |> Or_error.ok_exn |> snd
        in
        let acct2 = Account.create k (Balance.of_int 5) in
        Maskable.set maskable loc acct2 ;
        [%test_result: Account.t]
          ~message:"account in mask should be unchanged" ~expect:acct1
          (Mask.Attached.get attached_mask loc |> Option.value_exn) )
end

module type Depth_S = sig
  val depth : int
end

module Make_maskable_and_mask_with_depth (Depth : Depth_S) = struct
  let depth = Depth.depth

  module Location : Merkle_ledger.Location_intf.S = Merkle_ledger.Location.T

  module Location_binable = struct
    module Arg = struct
      type t = Location.t =
        | Generic of Merkle_ledger.Location.Bigstring.Stable.Latest.t
        | Account of Location.Addr.Stable.Latest.t
        | Hash of Location.Addr.Stable.Latest.t
      [@@deriving bin_io_unversioned, hash, sexp, compare]
    end

    type t = Arg.t =
      | Generic of Merkle_ledger.Location.Bigstring.t
      | Account of Location.Addr.t
      | Hash of Location.Addr.t
    [@@deriving hash, sexp, compare]

    include Hashable.Make_binable (Arg) [@@deriving
                                          sexp, compare, hash, yojson]
  end

  module Inputs = struct
    include Test_stubs.Base_inputs
    module Location = Location
    module Location_binable = Location_binable
    module Kvdb = In_memory_kvdb
    module Storage_locations = Storage_locations
  end

  (* underlying Merkle tree *)
  module Base_db :
    Merkle_ledger.Database_intf.S
    with module Location = Location
     and module Addr = Location.Addr
     and type account := Account.t
     and type root_hash := Hash.t
     and type hash := Hash.t
     and type key := Key.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t =
    Database.Make (Inputs)

  module Any_base = Merkle_ledger.Any_ledger.Make_base (Inputs)
  module Base = Any_base.M

  (* the mask tree *)
  module Mask :
    Merkle_mask.Masking_merkle_tree_intf.S
    with module Location = Location
     and module Attached.Addr = Location.Addr
    with type account := Account.t
     and type location := Location.t
     and type key := Key.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t
     and type hash := Hash.t
     and type parent := Base.t = Merkle_mask.Masking_merkle_tree.Make (struct
    include Inputs
    module Base = Base
  end)

  (* tree that can register masks *)
  module Maskable :
    Merkle_mask.Maskable_merkle_tree_intf.S
    with module Addr = Location.Addr
     and module Location = Location
    with type account := Account.t
     and type key := Key.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account_id := Account_id.t
     and type account_id_set := Account_id.Set.t
     and type root_hash := Hash.t
     and type hash := Hash.t
     and type unattached_mask := Mask.t
     and type attached_mask := Mask.Attached.t
     and type t := Base.t = Merkle_mask.Maskable_merkle_tree.Make (struct
    include Inputs
    module Base = Base
    module Mask = Mask

    let mask_to_base m = Any_base.cast (module Mask.Attached) m
  end)

  (* test runner *)
  let with_instances f =
    let db = Base_db.create ~depth:Depth.depth () in
    [%test_result: Int.t] ~message:"Base_db num accounts should start at zero"
      ~expect:0 (Base_db.num_accounts db) ;
    let maskable = Any_base.cast (module Base_db) db in
    let mask = Mask.create ~depth:Depth.depth () in
    f maskable mask

  let with_chain f =
    with_instances (fun maskable mask ->
        let attached1 = Maskable.register_mask maskable mask in
        let attached1_as_base =
          Any_base.cast (module Mask.Attached) attached1
        in
        let mask2 = Mask.create ~depth:Depth.depth () in
        let attached2 = Maskable.register_mask attached1_as_base mask2 in
        f maskable ~mask:attached1 ~mask_as_base:attached1_as_base
          ~mask2:attached2 )
end

module Make_maskable_and_mask (Depth : Depth_S) =
  Make (Make_maskable_and_mask_with_depth (Depth))

let%test_module "Test mask connected to underlying Merkle tree" =
  ( module struct
    module Depth_4 = struct
      let depth = 4
    end

    module Mdb_d4 = Make_maskable_and_mask (Depth_4)

    module Depth_30 = struct
      let depth = 30
    end

    module Mdb_d30 = Make_maskable_and_mask (Depth_30)
  end )
