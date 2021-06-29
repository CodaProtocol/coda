open Core_kernel

let%test_module "merkle_tree" =
  ( module struct
    open Snarky_backendless.Merkle_tree

    let merge x y = Free_hash.Merge (x, y)

    let hash =
      Option.value_map ~default:Free_hash.Hash_empty ~f:(fun x ->
          Free_hash.Hash_value x )

    let create_tree n =
      let tree = create ~hash ~merge 0 in
      add_many tree (List.init (n - 1) ~f:(fun i -> i + 1))

    let n = 10

    let tree = create_tree n

    let%test "length" = List.length (to_list tree) = n

    let%test_unit "key_retrieval" =
      for i = 0 to n - 1 do
        assert (get_exn tree i = i)
      done

    let%test "key_nonexist" = Option.is_none (get tree n)

    let%test_unit "modify" =
      let key = 5 in
      assert (key = get_exn tree key) ;
      let new_value = 123 in
      let tree = update tree key new_value in
      assert (new_value = get_exn tree key)

    let%test_unit "merkle_root" =
      for key = 0 to n - 1 do
        let path = get_path tree key in
        (* need poly equality here *)
        assert (
          Poly.( = )
            (implied_root ~merge key (hash (Some key)) path)
            (root tree) )
      done
  end )
