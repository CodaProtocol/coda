open Core
module Intf = Merkle_ledger.Intf
module Database = Merkle_ledger.Database

let%test_module "test functor on in memory databases" =
  ( module struct
    module Mdb_d (Depth : Intf.Depth) = struct
      open Test_stubs

      module Make (Depth : Intf.Depth) = struct
        module MT =
          Database.Make (Balance) (Account) (Hash) (Depth) (In_memory_kvdb)
            (In_memory_sdb)
        include MT
      end

      module MT = Make (Depth)

      let with_test_instance f =
        let uuid = Uuid.create () in
        let tmp_dir = "/tmp/merkle_database_test-" ^ Uuid.to_string uuid in
        let key_value_db_dir = Filename.concat tmp_dir "kvdb" in
        let stack_db_file = Filename.concat tmp_dir "sdb" in
        assert (Unix.system ("rm -rf " ^ tmp_dir) = Result.Ok ()) ;
        Unix.mkdir tmp_dir ;
        let mdb = MT.create ~key_value_db_dir ~stack_db_file in
        let cleanup () =
          MT.destroy mdb ;
          assert (Unix.system ("rm -rf " ^ tmp_dir) = Result.Ok ())
        in
        try
          let result = f mdb in
          cleanup () ; result
        with exn -> cleanup () ; raise exn

      exception Error_exception of MT.error

      let exn_of_error err = Error_exception err

      let%test_unit "getting a non existing account returns None" =
        with_test_instance (fun mdb ->
            Quickcheck.test MT.For_tests.gen_account_key ~f:(fun key ->
                assert (MT.get_account mdb key = None) ) )

      let%test "add and retrieve an account" =
        with_test_instance (fun mdb ->
            let account = Quickcheck.random_value Account.gen in
            assert (MT.set_account mdb account = Ok ()) ;
            let key =
              MT.get_key_of_account mdb account
              |> Result.map_error ~f:exn_of_error
              |> Result.ok_exn
            in
            Account.equal (Option.value_exn (MT.get_account mdb key)) account
        )

      let%test "accounts are atomic" =
        with_test_instance (fun mdb ->
            let account = Quickcheck.random_value Account.gen in
            assert (MT.set_account mdb account = Ok ()) ;
            let key =
              MT.get_key_of_account mdb account
              |> Result.map_error ~f:exn_of_error
              |> Result.ok_exn
            in
            assert (MT.set_account mdb account = Ok ()) ;
            let key' =
              MT.get_key_of_account mdb account
              |> Result.map_error ~f:exn_of_error
              |> Result.ok_exn
            in
            key = key' && MT.get_account mdb key = MT.get_account mdb key' )

      let%test_unit "num_accounts" =
        with_test_instance (fun mdb ->
            let open Quickcheck.Generator in
            let max_accounts = Int.min (1 lsl Depth.depth) (1 lsl 5) in
            let gen_unique_nonzero_balance_accounts n =
              let open Quickcheck.Let_syntax in
              let%bind num_initial_accounts = Int.gen_incl 0 n in
              let%map accounts =
                list_with_length num_initial_accounts Account.gen
              in
              List.filter accounts ~f:(fun account ->
                  not (Balance.equal (Account.balance account) Balance.zero) )
              |> List.dedup_and_sort ~compare:(fun account1 account2 ->
                     String.compare
                       (Account.public_key account1)
                       (Account.public_key account2) )
            in
            let accounts =
              Quickcheck.random_value
                (gen_unique_nonzero_balance_accounts (max_accounts / 2))
            in
            let num_initial_accounts = List.length accounts in
            List.iter accounts ~f:(fun account ->
                assert (MT.set_account mdb account = Ok ()) ) ;
            assert (MT.length mdb = num_initial_accounts) )

      let%test "deleted account keys are reassigned" =
        with_test_instance (fun mdb ->
            let account = Quickcheck.random_value Account.gen in
            let account' = Quickcheck.random_value Account.gen in
            assert (MT.set_account mdb account = Ok ()) ;
            let key =
              MT.get_key_of_account mdb account
              |> Result.map_error ~f:exn_of_error
              |> Result.ok_exn
            in
            let account = Account.set_balance account Balance.zero in
            assert (MT.set_account mdb account = Ok ()) ;
            assert (MT.set_account mdb account' = Ok ()) ;
            MT.get_account mdb key = Some account' )

      let%test_unit "set_inner_hash_at_addr_exn(address,hash); \
                     get_inner_hash_at_addr_exn(address) = hash" =
        let random_hash =
          Hash.hash_account @@ Quickcheck.random_value Account.gen
        in
        with_test_instance (fun mdb ->
            Quickcheck.test
              (Direction.gen_var_length_list ~start:1 Depth.depth)
              ~sexp_of:[%sexp_of : Direction.t List.t] ~f:(fun direction ->
                let address = MT.Addr.of_directions direction in
                MT.set_inner_hash_at_addr_exn mdb address random_hash ;
                let result = MT.get_inner_hash_at_addr_exn mdb address in
                assert (Hash.equal result random_hash) ) )

      let populate_db mdb max_height =
        let num_accounts = 1 lsl max_height in
        let initial_accounts =
          Quickcheck.random_value
            (Quickcheck.Generator.list_with_length num_accounts Account.gen)
        in
        List.iter initial_accounts ~f:(fun account ->
            ignore @@ MT.set_account mdb account )

      let%test_unit "If the entire database is full,\n\
                     \ \
                     set_all_accounts_rooted_at_exn(address,accounts);get_all_accounts_rooted_at_exn(address) \
                     = accounts" =
        with_test_instance (fun mdb ->
            let max_height = Int.min Depth.depth 5 in
            populate_db mdb max_height ;
            Quickcheck.test (Direction.gen_var_length_list max_height)
              ~sexp_of:[%sexp_of : Direction.t List.t] ~f:(fun directions ->
                let offset =
                  List.init (Depth.depth - max_height) ~f:(fun _ ->
                      Direction.Left )
                in
                let padded_directions = List.concat [offset; directions] in
                let address = MT.Addr.of_directions padded_directions in
                let num_accounts =
                  1 lsl (Depth.depth - List.length padded_directions)
                in
                let accounts =
                  Quickcheck.random_value
                    (Quickcheck.Generator.list_with_length num_accounts
                       Account.gen)
                in
                MT.set_all_accounts_rooted_at_exn mdb address accounts ;
                let result = MT.get_all_accounts_rooted_at_exn mdb address in
                assert (List.equal ~equal:Account.equal accounts result) ) )

      let%test_unit "implied_root(account) = root_hash" =
        with_test_instance (fun mdb ->
            let max_height = Int.min Depth.depth 5 in
            populate_db mdb max_height ;
            Quickcheck.test (Direction.gen_list max_height)
              ~sexp_of:[%sexp_of : Direction.t List.t] ~f:(fun directions ->
                let offset =
                  List.init (Depth.depth - max_height) ~f:(fun _ ->
                      Direction.Left )
                in
                let padded_directions = List.concat [offset; directions] in
                let address = MT.Addr.of_directions padded_directions in
                let path = MT.merkle_path_at_addr_exn mdb address in
                let leaf_hash = MT.get_inner_hash_at_addr_exn mdb address in
                let root_hash = MT.merkle_root mdb in
                assert (MT.Path.check_path path leaf_hash root_hash) ) )

      let%test_unit "Add 2^d accounts (for testing, d is small)" =
        if Depth.depth <= 8 then
          with_test_instance (fun mdb ->
              let gen_balance = Int.gen_incl 1 Int.max_value in
              let accounts =
                List.init (1 lsl Depth.depth) ~f:(fun public_key ->
                    Account.create (Int.to_string public_key)
                      (Quickcheck.random_value gen_balance) )
              in
              List.iter accounts ~f:(fun account ->
                  assert (MT.set_account mdb account = Ok ()) ) ;
              let retrieved_accounts =
                MT.get_all_accounts_rooted_at_exn mdb (MT.Addr.root ())
              in
              assert (List.length accounts = List.length retrieved_accounts) ;
              assert (
                List.equal ~equal:Account.equal accounts retrieved_accounts )
          )
    end

    module Mdb_d4 = Mdb_d (struct
      let depth = 4
    end)

    module Mdb_d30 = Mdb_d (struct
      let depth = 30
    end)
  end )
