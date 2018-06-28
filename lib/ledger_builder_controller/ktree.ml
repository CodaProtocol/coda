open Core_kernel

(* See https://en.wikipedia.org/wiki/Rose_tree *)
module Rose = struct
  type 'a t = Rose of 'a * 'a t list [@@deriving eq, sexp, bin_io, fold]

  let single a = Rose (a, [])

  let extract (Rose (x, _)) = x

  let gen a_gen =
    Quickcheck.Generator.fixed_point (fun self ->
        let open Quickcheck.Generator.Let_syntax in
        let%bind children = Quickcheck.Generator.list self in
        let%map a = a_gen in
        Rose (a, children) )

  module C = Container.Make (struct
    type nonrec 'a t = 'a t

    let fold t ~init ~f = fold f init t

    let iter = `Define_using_fold
  end)

  let to_list = C.to_list

  let to_array = C.to_array

  let mem = C.mem
end

(** A Rose tree with max-depth k. Whenever we want to add a node that would increase the depth past k, we instead move the tree forward and root it at the node towards that path *)
module Make (Elem : sig
  type t [@@deriving eq, compare, bin_io, sexp]

  val gen : t Quickcheck.Generator.t
end) (Small_k : sig
  val k : int
  (** The idea is k is "small" in the sense of probability of forking within k is < some nontrivial epsilon (like once a week?) *)
end) =
struct
  module Elem_set = Set.Make_binable (Elem)

  type t = {tree: Elem.t Rose.t; elems: Elem_set.t} [@@deriving sexp, bin_io]

  let single (e: Elem.t) : t =
    {tree= Rose.single e; elems= Elem_set.singleton e}

  let gen =
    let open Quickcheck.Generator.Let_syntax in
    (* We need to force the ref to be under the monad so it regenerates *)
    let%bind () = return () in
    let r = ref Elem_set.empty in
    let elem_unique_gen =
      let%map e =
        Quickcheck.Generator.filter Elem.gen ~f:(fun e ->
            not (Elem_set.mem !r e) )
      in
      r := Elem_set.add !r e ;
      e
    in
    let%map tree = Rose.gen elem_unique_gen in
    {tree; elems= !r}

  (* Note: This won't work in proof-of-work, but it's not a prefix of the proof-of-stakeversion, so I'm just going to use a longest heuristic for now *)
  let longest_path {tree} =
    let rec go tree depth path =
      match tree with
      | Rose.Rose (x, []) -> (x :: path, depth)
      | Rose.Rose (x, children) ->
          let path_depths =
            List.map children ~f:(fun c -> go c (depth + 1) (x :: path))
          in
          List.max_elt path_depths ~compare:(fun (_, d) (_, d') ->
              Int.compare d d' )
          |> Option.value_exn
    in
    go tree 0 [] |> fst |> List.rev

  let add t e ~parent =
    if Elem_set.mem t.elems e then t
    else
      let rec go node depth =
        let (Rose.Rose (x, xs)) = node in
        if Elem.equal x parent then
          (Rose.Rose (x, Rose.single e :: xs), depth + 1)
        else
          let xs, ds =
            List.map xs ~f:(fun x -> go x (depth + 1)) |> List.unzip
          in
          ( Rose.Rose (x, xs)
          , List.fold ds ~init:0 ~f:(fun a b ->
                if Int.compare a b >= 0 then a else b ) )
      in
      let x, tree_and_depths =
        let (Rose.Rose (x, xs)) = t.tree in
        let children = List.map xs ~f:(fun x -> go x 1) in
        if Elem.equal x parent then (x, (Rose.single e, 1) :: children)
        else (
          assert (List.length xs <> 0) ;
          (x, children) )
      in
      let default =
        { tree= Rose.Rose (x, tree_and_depths |> List.map ~f:fst)
        ; elems= Elem_set.add t.elems e }
      in
      match tree_and_depths with
      | [] | [_] -> default
      | _ ->
          let longest_subtree, longest_depth =
            List.max_elt tree_and_depths ~compare:(fun (_, d) (_, d') ->
                Int.compare d d' )
            |> Option.value_exn
          in
          (*printf*)
          (*!"Tree_and_depths %{sexp: (Elem.t Rose.t * int) list}\n%!"*)
          (*tree_and_depths ;*)
          (*printf*)
          (*!"Longest_subtree, longest_depth : %{sexp: Elem.t Rose.t} , %d\n%!"*)
          (*longest_subtree longest_depth ;*)
          if longest_depth >= Small_k.k then
            {tree= longest_subtree; elems= Elem_set.add t.elems e}
          else default

  let%test_unit "Adding an element either changes the tree or it was already \
                 in the set" =
    Quickcheck.test ~sexp_of:[%sexp_of : t * Elem.t * Elem.t]
      (let open Quickcheck.Generator.Let_syntax in
      let%bind r = gen and e = Elem.gen in
      let candidates = Rose.to_array r.tree in
      let%map idx = Int.gen_incl 0 (Array.length candidates - 1) in
      (r, e, candidates.(idx)))
      ~f:(fun (r, e, parent) ->
        let r' = add r e ~parent in
        assert (
          Elem_set.mem r.elems e || not (Rose.equal Elem.equal r.tree r'.tree)
        ) )

  let%test_unit "Adding to the end of the longest_path extends the path \
                 (modulo the last thing / first-thing)" =
    Quickcheck.test ~sexp_of:[%sexp_of : t * Elem.t]
      ( Quickcheck.Generator.tuple2 gen Elem.gen
      |> Quickcheck.Generator.filter ~f:(fun (r, e) ->
             not (Rose.mem r.tree e ~equal:Elem.equal) ) )
      ~f:(fun (r, e) ->
        let path = longest_path r in
        let r' = add r e ~parent:(List.last_exn path) in
        assert (Elem.equal e (List.last_exn (longest_path r'))) ;
        (* If there were two paths of the same length, we may be missing the
         * last thing in our first path *)
        let path = longest_path r in
        let potential_prefix = List.take path (List.length path - 1) in
        let path' = longest_path r' in
        assert (
          List.is_prefix ~equal:Elem.equal ~prefix:potential_prefix path'
          || List.is_prefix ~equal:Elem.equal
               ~prefix:(List.drop potential_prefix 1)
               path' ) )

  let%test_unit "Extending a tree with depth-k, extends full-tree properly" =
    let elem_pairs =
      Quickcheck.random_value ~seed:(`Deterministic "seed")
        (Quickcheck.Generator.list_with_length Small_k.k
           (Quickcheck.Generator.tuple2 Elem.gen Elem.gen))
    in
    let (e1, e2), es = (List.hd_exn elem_pairs, List.tl_exn elem_pairs) in
    let t =
      let tree =
        List.fold es
          ~init:(Rose.Rose (e1, []))
          ~f:(fun r (e, e') -> Rose.Rose (e, [Rose.single e'; r]))
      in
      {tree; elems= Elem_set.of_list (Rose.to_list tree)}
    in
    assert (List.length (longest_path t) = Small_k.k) ;
    let (Rose.Rose (head, first_children)) = t.tree in
    let t' = add t e2 ~parent:e1 in
    (*printf !"Length: %d\n%!" (List.length @@ longest_path t') ;*)
    assert (List.length (longest_path t') = Small_k.k) ;
    assert (not (Rose.mem t'.tree head ~equal:Elem.equal)) ;
    assert (
      not
        (Rose.mem t'.tree
           (Rose.extract (List.hd_exn first_children))
           ~equal:Elem.equal) ) ;
    assert (
      Rose.mem t'.tree
        (Rose.extract (List.nth_exn first_children 1))
        ~equal:Elem.equal )
end

let%test_module "K-tree" =
  ( module struct
    module Tree =
      Make (Int)
        (struct
          let k = 10
        end)

    module Big_tree =
      Make (Int)
        (struct
          let k = 50
        end)

    let%test_unit "longest_path finds longest path" =
      let t =
        { Tree.tree=
            Rose.Rose (1, [Rose.Rose (2, [Rose.single 3]); Rose.single 4])
        ; elems= Tree.Elem_set.of_list [1; 2; 3; 4] }
      in
      assert (List.equal ~equal:Int.equal (Tree.longest_path t) [1; 2; 3])
  end )
