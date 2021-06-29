open Core

(** A digit is a container of 1-4 elements. *)
module Digit = struct
  (* We use GADTs to track whether it's valid to remove/add an element to a
     digit, which gets us type safe (un)cons and (un)snoc. *)

  [@@@warning "-37"]

  type addable = Type_addable

  type not_addable = Type_not_addable

  type removable = Type_removable

  type not_removable = Type_not_removable

  [@@@warning "+37"]

  type (_, _, 'e) t =
    | One : 'e -> (addable, not_removable, 'e) t
    | Two : 'e * 'e -> (addable, removable, 'e) t
    | Three : 'e * 'e * 'e -> (addable, removable, 'e) t
    | Four : 'e * 'e * 'e * 'e -> (not_addable, removable, 'e) t

  (* Can't derive compare on GADTs, instead we provide an explicit instance. *)
  let compare (type add_x rem_x add_y rem_y) cmp_e (x : (add_x, rem_x, 'e) t)
      (y : (add_y, rem_y, 'e) t) =
    let fallthrough ~f = function 0 -> f () | n -> n in
    match (x, y) with
    | One x, One y ->
        cmp_e x y
    | One _, _ ->
        -1
    | _, One _ ->
        1
    | Two (x1, x2), Two (y1, y2) ->
        fallthrough (cmp_e x1 y1) ~f:(fun () -> cmp_e x2 y2)
    | Two _, _ ->
        -1
    | _, Two _ ->
        1
    | Three (x1, x2, x3), Three (y1, y2, y3) ->
        fallthrough (cmp_e x1 y1) ~f:(fun () ->
            fallthrough (cmp_e x2 y2) ~f:(fun () -> cmp_e x3 y3) )
    | Three _, _ ->
        -1
    | _, Three _ ->
        1
    | Four (x1, x2, x3, x4), Four (y1, y2, y3, y4) ->
        fallthrough (cmp_e x1 y1) ~f:(fun () ->
            fallthrough (cmp_e x2 y2) ~f:(fun () ->
                fallthrough (cmp_e x3 y3) ~f:(fun () -> cmp_e x4 y4) ) )
    | Four _, _ ->
        .
    | _, Four _ ->
        .

  (* "Eliminators" dispatching on addability/removability. You could achieve
      the same effect more directly using or-patterns, but the code that
      makes the typechecker understand existentials under or-patterns isn't
      in our compiler version. (ocaml/ocaml#2110)
  *)
  let addable_elim : type a r.
         ((addable, r, 'e) t -> 'o) (** Function handling addable case *)
      -> ((not_addable, removable, 'e) t -> 'o)
         (** Function handling non-addable case *)
      -> (a, r, 'e) t
      -> 'o =
   fun f g t ->
    match t with One _ -> f t | Two _ -> f t | Three _ -> f t | Four _ -> g t

  let removable_elim : type a r.
         ((a, removable, 'e) t -> 'o) (** Function handling removable case*)
      -> ((addable, not_removable, 'e) t -> 'o)
         (** Function handling non-removable case *)
      -> (a, r, 'e) t
      -> 'o =
   fun f g t ->
    match t with One _ -> g t | Two _ -> f t | Three _ -> f t | Four _ -> f t

  (** Existential type for when addability is determined at runtime. *)
  type ('r, 'e) t_any_a = Mk_any_a : ('a, 'r, 'e) t -> ('r, 'e) t_any_a

  (** Same for removability. *)
  type ('a, 'e) t_any_r = Mk_any_r : ('a, 'r, 'e) t -> ('a, 'e) t_any_r

  (** Both. *)
  type 'e t_any_ar = Mk_any_ar : ('a, 'r, 'e) t -> 'e t_any_ar

  (** "Broaden" a t_any_a into a t_any_ar, i.e. forget that we know the
      removability status. *)
  let broaden_any_a : ('r, 'e) t_any_a -> 'e t_any_ar =
   fun (Mk_any_a t) -> Mk_any_ar t

  (** Same deal for t_any_r *)
  let broaden_any_r : ('a, 'e) t_any_r -> 'e t_any_ar =
   fun (Mk_any_r t) -> Mk_any_ar t

  let cons : type r. 'e -> (addable, r, 'e) t -> (removable, 'e) t_any_a =
   fun v d ->
    match d with
    | One a ->
        Mk_any_a (Two (v, a))
    | Two (a, b) ->
        Mk_any_a (Three (v, a, b))
    | Three (a, b, c) ->
        Mk_any_a (Four (v, a, b, c))

  let snoc : type r. (addable, r, 'e) t -> 'e -> (removable, 'e) t_any_a =
   fun d v ->
    match d with
    | One a ->
        Mk_any_a (Two (a, v))
    | Two (a, b) ->
        Mk_any_a (Three (a, b, v))
    | Three (a, b, c) ->
        Mk_any_a (Four (a, b, c, v))

  let uncons : type a. (a, removable, 'e) t -> 'e * (addable, 'e) t_any_r =
    function
    | Two (a, b) ->
        (a, Mk_any_r (One b))
    | Three (a, b, c) ->
        (a, Mk_any_r (Two (b, c)))
    | Four (a, b, c, d) ->
        (a, Mk_any_r (Three (b, c, d)))

  let unsnoc : type a. (a, removable, 'e) t -> (addable, 'e) t_any_r * 'e =
    function
    | Two (a, b) ->
        (Mk_any_r (One a), b)
    | Three (a, b, c) ->
        (Mk_any_r (Two (a, b)), c)
    | Four (a, b, c, d) ->
        (Mk_any_r (Three (a, b, c)), d)

  let foldr : type a r. ('e -> 'acc -> 'acc) -> 'acc -> (a, r, 'e) t -> 'acc =
   fun f z d ->
    match d with
    | One a ->
        f a z
    | Two (a, b) ->
        f a (f b z)
    | Three (a, b, c) ->
        f a (f b (f c z))
    | Four (a, b, c, d) ->
        f a (f b (f c (f d z)))

  let foldl : type a r. ('acc -> 'e -> 'acc) -> 'acc -> (a, r, 'e) t -> 'acc =
   fun f z d ->
    match d with
    | One a ->
        f z a
    | Two (a, b) ->
        f (f z a) b
    | Three (a, b, c) ->
        f (f (f z a) b) c
    | Four (a, b, c, d) ->
        f (f (f (f z a) b) c) d

  let to_list : type a r. (a, r, 'e) t -> 'e list =
   fun t -> foldr List.cons [] t

  let gen_any_ar : int t_any_ar Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    let gen_measure = Int.gen_incl 1 20 in
    let%bind a, b, c, d =
      Quickcheck.Generator.tuple4 gen_measure gen_measure gen_measure
        gen_measure
    in
    Quickcheck.Generator.of_list
      [ Mk_any_ar (One a)
      ; Mk_any_ar (Two (a, b))
      ; Mk_any_ar (Three (a, b, c))
      ; Mk_any_ar (Four (a, b, c, d)) ]

  (** Given a measurement function, compute the total measure of a digit.
      See below for an explanation of what measure is.
  *)
  let measure : ('e -> int) -> (_, _, 'e) t -> int =
   fun measure' -> foldl (fun m e -> m + measure' e) 0

  (** Split a digit by measure. Again see below. *)
  let split : type a r.
         ('e -> int)
      -> int
      -> int
      -> (a, r, 'e) t
      -> 'e t_any_ar option * 'e * 'e t_any_ar option =
   fun measure' target acc t ->
    (* Addable inputs go to addable outputs, but non-addable inputs may go to
       either. We use a separate function for addables to represent this and
       minimizing the amount of Obj.magicking we need to do. *)
    let rec split_addable : type r.
           int
        -> (addable, r, 'e) t
        -> (addable, 'e) t_any_r option * 'e * (addable, 'e) t_any_r option =
     fun acc t ->
      removable_elim
        (fun t' ->
          let head, Mk_any_r tail = uncons t' in
          if acc + measure' head >= target then
            (None, head, Some (Mk_any_r tail))
          else
            match split_addable (acc + measure' head) tail with
            | Some (Mk_any_r lhs), m, rhs ->
                let (Mk_any_a cons_res) = cons head lhs in
                (* t' is addable, so the tail of t' is twice-addable. We just
                   passed that tail to split_addable, which always returns
                   digits with <= the number of elements of the input. So
                   cons_res is addable but it's not possible to convince the
                   typechecker of that, as far as I can tell.
                *)
                let cons_res' : (addable, removable, 'e) t =
                  Obj.magic cons_res
                in
                (Some (Mk_any_r cons_res'), m, rhs)
            | None, m, rhs ->
                (Some (Mk_any_r (One head)), m, rhs) )
        (fun (One a) ->
          if acc + measure' a >= target then (None, a, None)
          else failwith "Digit.split index out of bounds" )
        t
    in
    addable_elim
      (fun t' ->
        let lhs, m, rhs = split_addable acc t' in
        (Option.map ~f:broaden_any_r lhs, m, Option.map ~f:broaden_any_r rhs)
        )
      (fun t' ->
        let head, Mk_any_r tail = uncons t' in
        if acc + measure' head >= target then
          (None, head, Some (Mk_any_ar tail))
        else
          let lhs, m, rhs = split_addable (acc + measure' head) tail in
          match lhs with
          | None ->
              (Some (Mk_any_ar (One head)), m, Option.map ~f:broaden_any_r rhs)
          | Some (Mk_any_r lhs') ->
              ( Some (broaden_any_a (cons head lhs'))
              , m
              , Option.map ~f:broaden_any_r rhs ) )
      t

  let opt_to_list : 'a t_any_ar option -> 'a list = function
    | None ->
        []
    | Some (Mk_any_ar dig) ->
        to_list dig

  let%test_unit "Digit.split preserves contents and order" =
    Quickcheck.test
      (let open Quickcheck.Generator.Let_syntax in
      let%bind (Mk_any_ar dig as dig') = gen_any_ar in
      let%bind idx = Int.gen_incl 1 (List.length @@ to_list dig) in
      return (dig', idx))
      ~f:(fun (Mk_any_ar dig, target) ->
        let lhs_opt, m, rhs_opt = split Fn.id target 0 dig in
        let lhs', rhs' = (opt_to_list lhs_opt, opt_to_list rhs_opt) in
        [%test_eq: int list] (lhs' @ [m] @ rhs') (to_list dig) )

  let%test_unit "Digit.split matches list implementation" =
    Quickcheck.test
      ~sexp_of:(fun (Mk_any_ar dig, idx) ->
        Tuple2.sexp_of_t
          (List.sexp_of_t Int.sexp_of_t)
          Int.sexp_of_t
          (to_list dig, idx) )
      (let open Quickcheck.Generator.Let_syntax in
      let%bind (Mk_any_ar dig) = gen_any_ar in
      let%bind idx = Int.gen_incl 1 (List.length @@ to_list dig) in
      return (Mk_any_ar dig, idx))
      ~f:(fun (Mk_any_ar dig, idx) ->
        let as_list = to_list dig in
        let lhs_list = List.take as_list (idx - 1) in
        let m_list = List.nth_exn as_list (idx - 1) in
        let rhs_list = List.drop as_list idx in
        [%test_eq: int list] (lhs_list @ (m_list :: rhs_list)) as_list ;
        let lhs_fseq, m_fseq, rhs_fseq = split (Fn.const 1) idx 0 dig in
        let lhs_fseq', rhs_fseq' =
          (opt_to_list lhs_fseq, opt_to_list rhs_fseq)
        in
        [%test_eq: int list] (lhs_fseq' @ (m_fseq :: rhs_fseq')) (to_list dig) ;
        [%test_eq: int list] lhs_list lhs_fseq' ;
        [%test_eq: int] m_list m_fseq ;
        [%test_eq: int list] rhs_list rhs_fseq' ;
        [%test_eq: int] (List.length lhs_fseq') (idx - 1) ;
        [%test_eq: int] (List.length rhs_fseq') (List.length as_list - idx) )

  (* See comment below about measures for why index 0 is an edge case. *)
  let%test_unit "Digit.split with index 0 is trivial" =
    Quickcheck.test
      (Quickcheck.Generator.tuple2 gen_any_ar (Int.gen_incl 0 200))
      ~f:(fun (Mk_any_ar dig, acc) ->
        let as_list = to_list dig in
        let lhs, m, rhs = split Fn.id acc acc dig in
        assert (Option.is_none lhs) ;
        [%test_eq: int] m (List.hd_exn as_list) ;
        match rhs with
        | None ->
            [%test_eq: int list] [] (List.tl_exn as_list)
        | Some (Mk_any_ar rhs') ->
            [%test_eq: int list] (to_list rhs') (List.tl_exn as_list) )

  let%test _ =
    match split Fn.id 1 0 (One 1) with None, 1, None -> true | _ -> false

  let%test _ =
    match split Fn.id 5 0 (Three (0, 2, 4)) with
    | Some (Mk_any_ar (Two (0, 2))), 4, None ->
        true
    | _ ->
        false

  let%test _ =
    match split Fn.id 10 0 (Four (2, 3, 5, 1)) with
    | Some (Mk_any_ar (Two (2, 3))), 5, Some (Mk_any_ar (One 1)) ->
        true
    | _ ->
        false

  let%test _ =
    match split Fn.id 7 0 (Four (2, 4, 3, 2)) with
    | Some (Mk_any_ar (Two (2, 4))), 3, Some (Mk_any_ar (One 2)) ->
        true
    | _ ->
        false
end

(** Nodes containing 2-3 elements, with a cached measurement. *)
module Node = struct
  (** This implementation doesn't actually use 2-nodes, but they're here for
      future use. The paper uses them in the append operation, which isn't
      implemented here.
  *)
  type 'e t = Two of int * 'e * 'e | Three of int * 'e * 'e * 'e
  [@@deriving equal, compare]

  (** Extract the cached measurement *)
  let measure : 'e t -> int =
   fun t -> match t with Two (m, _, _) -> m | Three (m, _, _, _) -> m

  let to_digit : 'e t -> (Digit.addable, Digit.removable, 'e) Digit.t =
    function
    | Two (_m, a, b) ->
        Digit.Two (a, b)
    | Three (_m, a, b, c) ->
        Digit.Three (a, b, c)

  (* smart constructors to maintain correct measures *)
  let _mk_2 : ('e -> int) -> 'e -> 'e -> 'e t =
   fun f a b -> Two (f a + f b, a, b)

  let mk_3 : ('e -> int) -> 'e -> 'e -> 'e -> 'e t =
   fun f a b c -> Three (f a + f b + f c, a, b, c)

  let split_to_digits :
         ('e -> int)
      -> int
      -> int
      -> 'e t
      -> 'e Digit.t_any_ar option * 'e * 'e Digit.t_any_ar option =
   fun measure' target acc t -> to_digit t |> Digit.split measure' target acc
end

(** Finally, the actual finger tree type! *)
type 'e t =
  | Empty : 'e t  (** Empty tree *)
  | Single : 'e -> 'e t  (** Single element tree *)
  | Deep :
      ( int
      * ('aL, 'rL, 'e) Digit.t
      * 'e Node.t t Lazy.t
      * ('aR, 'rR, 'e) Digit.t )
      -> 'e t
      (** The recursive case. We have a cached measurement, prefix and suffix
          fingers, and a subtree. Note the subtree has a different type than its
          parent. The top level has 'es, the next level has 'e Node.ts, the next
          has 'e Node.t Node.ts and so on. As you go deeper, the breadth
          increases exponentially. *)

(* Can't derive compare for GADTs.. *)
let rec compare : type e. (e -> e -> int) -> e t -> e t -> int =
 fun cmp_e x y ->
  match (x, y) with
  | Empty, Empty ->
      0
  | Empty, _ ->
      -1
  | _, Empty ->
      1
  | Single x, Single y ->
      cmp_e x y
  | Single _, _ ->
      -1
  | _, Single _ ->
      1
  | Deep (i, pre_x, mid_x, suff_x), Deep (j, pre_y, mid_y, suff_y) ->
      let cmp = Int.compare i j in
      if cmp <> 0 then cmp
      else
        let cmp = Digit.compare cmp_e pre_x pre_y in
        if cmp <> 0 then cmp
        else
          let cmp = Lazy.compare (compare (Node.compare cmp_e)) mid_x mid_y in
          if cmp <> 0 then cmp else Digit.compare cmp_e suff_x suff_y
  | Deep _, _ ->
      .
  | _, Deep _ ->
      .

(* About measurements: in the paper they define finger trees more generally than
   this implementation. Given a monoid m, a measurement function for elements
   e -> m, and "monotonic" predicates on m, if you cache the measure of subtrees
   you can index into and split finger trees at the transition point of the
   predicates in log time. The output of any of the split functions is a triple
   of the longest subsequence starting from the beginning where the predicate is
   false, the element that flips it to true, and the subsequence after that up
   to the end. In this implementation the monoid is natural numbers under
   summation, the measurement is 'Fn.const 1' and the predicates are
   (fun x -> x >= idx). So the measure of a tree is how many elements are in it
   and the transition point is where there are >= idx elements. Index 0 is a
   special case, since forall x : ℕ. x >= 0. In that case the first subsequence
   is empty, the transition point is the first element, and the second
   subsequence is the rest of the input sequence.

   You'll see many functions take a parameter measure' to compute measures of
   elements with. This is always either Node.measure or 'Fn.const 1' depending
   on if we're at the top level or not.

   Other measurement functions and monoids get you priority queues, search trees
   and interval trees.
*)
let measure : ('e -> int) -> 'e t -> int =
 fun measure' t ->
  match t with Empty -> 0 | Single a -> measure' a | Deep (m, _, _, _) -> m

(** Smart constructor for deep nodes that tracks measure. *)
let deep :
       ('e -> int)
    -> (_, _, 'e) Digit.t
    -> 'e Node.t t
    -> (_, _, 'e) Digit.t
    -> 'e t =
 fun measure' prefix middle suffix ->
  Deep
    ( Digit.measure measure' prefix
      + measure Node.measure middle
      + Digit.measure measure' suffix
    , prefix
    , lazy middle
    , suffix )

let empty : 'e t = Empty

(** Add a new element to the left end of the tree. *)
let rec cons' : 'e. ('e -> int) -> 'e -> 'e t -> 'e t =
 fun measure' v t ->
  match t with
  | Empty ->
      Single v
  | Single v' ->
      deep measure' (Digit.One v) Empty (Digit.One v')
  | Deep (_, prefix, middle, suffix) ->
      (* If there is space in the left finger, the finger is the only thing that
         needs to change. If not we need to make a recursive call. A recursive
         call frees up two finger slots and is needed every third cons
         operation, so the amortized cost is constant for a two layer tree.
         Because each level triples the number of elements in the fingers, we
         free up 2 * 3^level slots per recursive call and need to do so every
         2 * 3^level conses. So cons is amortized O(1) for arbitrary depth
         trees.
      *)
      Digit.addable_elim
        (fun prefix' ->
          let (Mk_any_a prefix'') = Digit.cons v prefix' in
          deep measure' prefix'' (Lazy.force middle) suffix )
        (fun (Four (a, b, c, d)) ->
          deep measure'
            (Digit.Two (v, a))
            (cons' Node.measure (Node.mk_3 measure' b c d) @@ Lazy.force middle)
            suffix )
        prefix

let cons : 'e -> 'e t -> 'e t = fun x xs -> cons' (Fn.const 1) x xs

(** Add a new element to the right end of the tree. This is a mirror of cons' *)
let rec snoc' : 'e. ('e -> int) -> 'e t -> 'e -> 'e t =
 fun measure' t v ->
  match t with
  | Empty ->
      Single v
  | Single v' ->
      deep measure' (Digit.One v') Empty (Digit.One v)
  | Deep (_, prefix, middle, suffix) ->
      Digit.addable_elim
        (fun digit ->
          let (Mk_any_a digit') = Digit.snoc digit v in
          deep measure' prefix (Lazy.force middle) digit' )
        (fun (Four (a, b, c, d)) ->
          deep measure' prefix
            (snoc' Node.measure (Lazy.force middle) @@ Node.mk_3 measure' a b c)
            (Digit.Two (d, v)) )
        suffix

let snoc : 'e t -> 'e -> 'e t = fun xs x -> snoc' (Fn.const 1) xs x

(** Create a finger tree from a digit *)
let tree_of_digit : ('e -> int) -> ('a, 'r, 'e) Digit.t -> 'e t =
 fun measure' dig -> Digit.foldr (cons' measure') Empty dig

(** If the input is non-empty, get the first element and the rest of the
    sequence. If it is empty, return None. *)
let rec uncons' : 'e. ('e -> int) -> 'e t -> ('e * 'e t) option =
 fun measure' t ->
  match t with
  | Empty ->
      None
  | Single e ->
      Some (e, empty)
  | Deep (_m, prefix, middle, suffix) ->
      Digit.removable_elim
        (fun prefix' ->
          let head, Mk_any_r prefix_rest = Digit.uncons prefix' in
          Some (head, deep measure' prefix_rest (force middle) suffix) )
        (fun (One e) ->
          match uncons' Node.measure (force middle) with
          | None ->
              Some (e, tree_of_digit measure' suffix)
          | Some (node, rest) ->
              Some (e, deep measure' (Node.to_digit node) rest suffix) )
        prefix

(** Uncons for the top level trees. *)
let uncons : 'e t -> ('e * 'e t) option = fun t -> uncons' (Fn.const 1) t

(** Mirror of uncons' for the last element. *)
let rec unsnoc' : 'e. ('e -> int) -> 'e t -> ('e t * 'e) option =
 fun measure' t ->
  match t with
  | Empty ->
      None
  | Single e ->
      Some (empty, e)
  | Deep (_m, prefix, middle, suffix) ->
      Digit.removable_elim
        (fun suffix' ->
          let Mk_any_r liat, deah = Digit.unsnoc suffix' in
          Some (deep measure' prefix (force middle) liat, deah) )
        (fun (One e) ->
          match unsnoc' Node.measure (force middle) with
          | None ->
              Some (tree_of_digit measure' prefix, e)
          | Some (rest, node) ->
              Some (deep measure' prefix rest (Node.to_digit node), e) )
        suffix

(** Mirror of uncons. *)
let unsnoc : 'e t -> ('e t * 'e) option = fun t -> unsnoc' (Fn.const 1) t

let head_exn : 'e t -> 'e = fun t -> Option.value_exn (uncons t) |> Tuple2.get1

let last_exn : 'e t -> 'e =
 fun t -> unsnoc t |> Option.value_exn |> Tuple2.get2

let rec foldl : ('a -> 'e -> 'a) -> 'a -> 'e t -> 'a =
 fun f acc t ->
  match uncons t with
  | None ->
      acc
  | Some (head, tail) ->
      foldl f (f acc head) tail

let rec foldr : ('e -> 'a -> 'a) -> 'a -> 'e t -> 'a =
 fun f acc t ->
  match uncons t with
  | None ->
      acc
  | Some (head, tail) ->
      f head (foldr f acc tail)

module C = Container.Make (struct
  type nonrec 'a t = 'a t

  let fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum =
   fun t ~init ~f -> foldl f init t

  let iter = `Define_using_fold

  let length = `Custom (fun t -> measure (Fn.const 1) t)
end)

let is_empty = C.is_empty

let length = C.length

let iter = C.iter

let find = C.find

let to_seq : 'e t -> 'e Sequence.t = fun t -> Sequence.unfold ~init:t ~f:uncons

let sexp_of_t : ('e -> Sexp.t) -> 'e t -> Sexp.t =
 fun sexp_inner -> Fn.compose (Sequence.sexp_of_t sexp_inner) to_seq

let rec equal : ('e -> 'e -> bool) -> 'e t -> 'e t -> bool =
 fun eq_inner xs ys ->
  match (uncons xs, uncons ys) with
  | Some (x, xs_tail), Some (y, ys_tail) ->
      eq_inner x y && equal eq_inner xs_tail ys_tail
  | _ ->
      false

let to_list : 'e t -> 'e list = fun fseq -> Sequence.to_list (to_seq fseq)

let of_list : 'e list -> 'e t = List.fold_left ~init:empty ~f:snoc

(* Split a tree into the elements before a given index, the element at that
   index and the elements after it. The index must exist in the tree. *)
let rec split : 'e. ('e -> int) -> 'e t -> int -> int -> 'e t * 'e * 'e t =
 fun measure' t target acc ->
  match t with
  | Empty ->
      failwith "FSequence.split index out of bounds (1)"
  | Single x ->
      if acc + measure' x >= target then (Empty, x, Empty)
      else failwith "FSequence.split index out of bounds (2)"
  | Deep (_m, prefix, middle, suffix) ->
      let acc_p = acc + Digit.measure measure' prefix in
      if acc_p >= target then
        (* split point is in left finger *)
        let dl, m, dr = Digit.split measure' target acc prefix in
        ( (* left part of digit split *)
          ( match dl with
          | None ->
              Empty
          | Some (Mk_any_ar dig) ->
              tree_of_digit measure' dig )
        , (* middle of digit split *) m
        , (* right part of digit split + subtree + suffix *)
          match dr with
          | None -> (
            match uncons' Node.measure @@ force middle with
            | None ->
                tree_of_digit measure' suffix
            | Some (head, tail) ->
                deep measure' (Node.to_digit head) tail suffix )
          | Some (Mk_any_ar dig) ->
              deep measure' dig (force middle) suffix )
      else
        let acc_m = acc_p + measure Node.measure (force middle) in
        if acc_m >= target then
          (* split point is in subtree *)
          let lhs, m, rhs = split Node.measure (force middle) target acc_p in
          (* The subtree is made of nodes, so the midpoint we got from the
             recursive call is a node, so split that. *)
          let m_lhs, m_m, m_rhs =
            Node.split_to_digits measure' target
              (measure Node.measure lhs + acc_p)
              m
          in
          ( (* prefix + lhs of the split of the subtree + lhs of the split of
               them midpoint of the subtree *)
            ( match m_lhs with
            | None -> (
              match unsnoc' Node.measure lhs with
              | None ->
                  tree_of_digit measure' prefix
              | Some (liat, deah) ->
                  deep measure' prefix liat (Node.to_digit deah) )
            | Some (Mk_any_ar dig) ->
                deep measure' prefix lhs dig )
          , (* midpoint of the split of the subtree *)
            m_m
          , (* rhs of the split of the midpoint of the subtree + rhs of the
               split of the subtree + suffix *)
            match m_rhs with
            | None -> (
              match uncons' Node.measure rhs with
              | None ->
                  tree_of_digit measure' suffix
              | Some (head, tail) ->
                  deep measure' (Node.to_digit head) tail suffix )
            | Some (Mk_any_ar dig) ->
                deep measure' dig rhs suffix )
        else
          let acc_s = acc_m + Digit.measure measure' suffix in
          if acc_s >= target then
            (* split point is in right finger *)
            let dl, m, dr = Digit.split measure' target acc_m suffix in
            ( (* prefix + subtree + left part of digit split *)
              ( match dl with
              | None -> (
                match unsnoc' Node.measure (force middle) with
                | None ->
                    tree_of_digit measure' prefix
                | Some (liat, deah) ->
                    deep measure' prefix liat (Node.to_digit deah) )
              | Some (Mk_any_ar dig) ->
                  deep measure' prefix (force middle) dig )
            , (* midpoint of digit split *)
              m
            , (* right part of digit split *)
              match dr with
              | None ->
                  Empty
              | Some (Mk_any_ar dig) ->
                  tree_of_digit measure' dig )
          else failwith "FSequence.split index out of bounds (3)"

(* Split a tree into the elements before some index and the elements >= that
   index. split_at works when the index is out of range and returns a pair while
   split throws an exception if the index is out of range and returns a triple.
   The contract is that split_at xs i = take i xs, drop i xs.
*)
let split_at : 'e t -> int -> 'e t * 'e t =
 fun t idx ->
  match t with
  | Empty ->
      (empty, empty)
  | _ ->
      if idx = 0 then (empty, t)
      else if measure (Fn.const 1) t >= idx then
        match split (Fn.const 1) t idx 0 with lhs, m, rhs -> (snoc lhs m, rhs)
      else (t, empty)

let singleton : 'e -> 'e t = fun v -> Single v

(* Assert that the cached measures match the actual ones. *)
let rec assert_measure : type e. (e -> int) -> e t -> unit =
 fun measure' -> function
  | Empty ->
      ()
  | Single _ ->
      ()
  | Deep (cached_measure, prefix, middle, suffix) ->
      let measure_node_with_assert node =
        let expected = Node.measure node in
        let contents = node |> Node.to_digit |> Digit.to_list in
        [%test_eq: int] expected (List.sum (module Int) ~f:measure' contents) ;
        expected
      in
      let middle' = Lazy.force middle in
      assert_measure measure_node_with_assert middle' ;
      [%test_eq: int] cached_measure
        ( Digit.measure measure' prefix
        + measure Node.measure middle'
        + Digit.measure measure' suffix )

(* Quickcheck.Generator.list generates pretty small lists, which are not big
   enough to exercise multiple levels of the tree. So we use this instead. *)
let big_list : 'a Quickcheck.Generator.t -> 'a list Quickcheck.Generator.t =
 fun gen ->
  let open Quickcheck.Generator.Let_syntax in
  let%bind len = Int.gen_incl 0 1000 in
  Quickcheck.Generator.list_with_length len gen

let%test_unit "list isomorphism - cons" =
  Quickcheck.test (big_list Int.quickcheck_generator) ~f:(fun xs ->
      let xs_fseq = List.fold_right xs ~f:cons ~init:empty in
      assert_measure (Fn.const 1) xs_fseq ;
      [%test_eq: int list] xs (to_list xs_fseq) ;
      [%test_eq: int] (List.length xs) (length xs_fseq) )

let%test_unit "list isomorphism - snoc" =
  Quickcheck.test (big_list Int.quickcheck_generator) ~f:(fun xs ->
      let xs_fseq = List.fold_left xs ~init:empty ~f:snoc in
      assert_measure (Fn.const 1) xs_fseq ;
      [%test_eq: int list] xs (to_list xs_fseq) ;
      [%test_eq: int] (List.length xs) (length xs_fseq) )

let%test_unit "alternating cons/snoc" =
  Quickcheck.test
    Quickcheck.Generator.(
      big_list @@ variant2 (Int.gen_incl 0 500) (Int.gen_incl 0 500))
    ~f:(fun cmds ->
      let rec go list fseq cmds_acc =
        match cmds_acc with
        | [] ->
            assert_measure (Fn.const 1) fseq ;
            [%test_eq: int list] list (to_list fseq) ;
            [%test_eq: int] (List.length list) (length fseq)
        | `A x :: rest ->
            go (x :: list) (cons x fseq) rest
        | `B x :: rest ->
            go (list @ [x]) (snoc fseq x) rest
      in
      go [] empty cmds )

let%test_unit "split properties" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind xs = big_list (Int.gen_incl 0 500) in
    let%bind idx = Int.gen_incl 0 (List.length xs) in
    return (xs, idx)
  in
  let shrinker =
    Quickcheck.Shrinker.create (fun (xs, idx) ->
        Sequence.append
          ( if List.length xs - 1 > idx then
            Sequence.singleton (List.tl_exn xs, idx)
          else Sequence.empty )
          ( Sequence.range ~start:`inclusive ~stop:`inclusive 1 5
          |> Sequence.filter_map ~f:(fun offset ->
                 let res = idx - offset in
                 if res >= 0 then Some (xs, res) else None ) ) )
  in
  Quickcheck.test gen ~shrink_attempts:`Exhaustive
    ~sexp_of:[%sexp_of: int list * int] ~shrinker ~f:(fun (xs, idx) ->
      let len = List.length xs in
      let split_l_list = List.take xs idx in
      let split_r_list = List.drop xs idx in
      let xs_fseq = of_list xs in
      let split_l_fseq, split_r_fseq = split_at xs_fseq idx in
      let split_l_fseq', split_r_fseq' =
        (to_list split_l_fseq, to_list split_r_fseq)
      in
      assert_measure (Fn.const 1) split_l_fseq ;
      assert_measure (Fn.const 1) split_r_fseq ;
      [%test_eq: int] (List.length split_l_list + List.length split_r_list) len ;
      [%test_eq: int list] split_l_list split_l_fseq' ;
      [%test_eq: int list] split_r_list split_r_fseq' ;
      [%test_eq: int] (List.length split_l_fseq') (length split_l_fseq) ;
      [%test_eq: int] (List.length split_r_fseq') (length split_r_fseq) ;
      [%test_eq: int] (length split_l_fseq + length split_r_fseq) len )

(* Exercise all the functions that generate sequences, in random combinations. *)
let%test_module "random sequence generation, with splits" =
  ( module struct
    type action =
      [ `Cons of int
      | `Snoc of int
      | `Split_take_left of int
      | `Split_take_right of int ]
    [@@deriving sexp_of]

    let%test_unit _ =
      let rec gen_actions xs len n =
        let open Quickcheck.Generator in
        let open Quickcheck.Generator.Let_syntax in
        if n = 0 then return @@ List.rev xs
        else
          match%bind
            variant3 (Int.gen_incl 0 500) (Int.gen_incl 0 500)
              (Int.gen_uniform_incl 0 len)
          with
          | `A v ->
              gen_actions (`Cons v :: xs) (len + 1) (n - 1)
          | `B v ->
              gen_actions (`Snoc v :: xs) (len + 1) (n - 1)
          | `C idx -> (
              match%bind bool with
              | true ->
                  gen_actions (`Split_take_left idx :: xs) idx (n - 1)
              | false ->
                  gen_actions (`Split_take_right idx :: xs) (len - idx) (n - 1)
              )
      in
      let gen =
        let open Quickcheck.Generator.Let_syntax in
        let%bind len = Int.gen_incl 0 50 in
        gen_actions [] 0 len
      in
      let shrinker =
        Quickcheck.Shrinker.create (function
          | [] ->
              Sequence.empty
          | _ :: _ as acts ->
              Sequence.of_list
                [ List.take acts (List.length acts / 2)
                ; List.take acts (List.length acts - 1)
                ; List.map acts ~f:(function `Snoc x -> `Cons x | x -> x)
                ; List.map acts ~f:(function `Cons x -> `Snoc x | x -> x) ] )
      in
      Quickcheck.test gen ~trials:100_000 ~shrinker
        ~sexp_of:(List.sexp_of_t sexp_of_action) ~f:(fun acts ->
          let rec go fseq =
            let assert_m fseq' =
              assert_measure (Fn.const 1) fseq' ;
              fseq'
            in
            function
            | [] ->
                ()
            | `Cons x :: acts_rest ->
                go (assert_m @@ cons x fseq) acts_rest
            | `Snoc x :: acts_rest ->
                go (assert_m @@ snoc fseq x) acts_rest
            | `Split_take_left idx :: acts_rest ->
                go (assert_m @@ Tuple2.get1 @@ split_at fseq idx) acts_rest
            | `Split_take_right idx :: acts_rest ->
                go (assert_m @@ Tuple2.get2 @@ split_at fseq idx) acts_rest
          in
          go empty acts )
  end )
