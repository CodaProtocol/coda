type z = Z of z

type 'a s = Z | S of 'a

type _ t = Z : z t | S : 'n t -> 'n s t

module T = struct
  type nonrec 'a t = 'a t
end

type 'a nat = 'a t

type e = T : 'n nat -> e

let to_int : type n. n t -> int =
  let rec go : type n. int -> n t -> int =
   fun acc n -> match n with Z -> acc | S n -> go (acc + 1) n
  in
  fun x -> go 0 x

let rec of_int : int -> e =
 fun n ->
  if n < 0 then failwith "of_int: negative"
  else if n = 0 then T Z
  else
    let (T n) = of_int (n - 1) in
    T (S n)

module type Intf = sig
  type n

  val n : n t
end

type 'n m = (module Intf with type n = 'n)

module Is_succ = struct
  type 'n t = Has_pred : 'm t -> 'm s t
end

module Adds = struct
  type ('a, 'b, 'c) t =
    | Z : (z, 'n, 'n) t
    | S : ('a, 'b, 'c) t -> ('a s, 'b, 'c s) t

  let rec add_zr : type n. n nat -> (n, z, n) t = function
    | Z ->
        Z
    | S n ->
        let pi = add_zr n in
        S pi

  let rec add_zr_refl : type n m.
      (n, z, m) t -> (n, m) Core_kernel.Type_equal.t = function
    | Z ->
        T
    | S n ->
        let T = add_zr_refl n in
        T

  let rec to_nat : type n a b. (n, a, b) t -> n nat = function
    | Z ->
        Z
    | S n ->
        let pi = to_nat n in
        S pi

  let rec eq_exn : type n1 n2 a1 a2 total.
         (n1, a1, total) t
      -> (n2, a2, total) t
      -> (n1, n2) Core_kernel.Type_equal.t * (a1, a2) Core_kernel.Type_equal.t
      =
   fun x y ->
    match (x, y) with
    | Z, Z ->
        (T, T)
    | S x, S y ->
        let T, T = eq_exn x y in
        (T, T)
    | _ ->
        failwith "Adds.eq_exn"

  let rec eq_inner_exn : type n1 n2 a total1 total2.
         (n1, a, total1) t
      -> (n2, a, total2) t
      -> (n1, n2) Core_kernel.Type_equal.t
         * (total1, total2) Core_kernel.Type_equal.t =
   fun x y ->
    match (x, y) with
    | Z, Z ->
        (T, T)
    | S x, S y ->
        let T, T = eq_inner_exn x y in
        (T, T)
    | _ ->
        failwith "Adds.eq_inner_exn"
end

module Lte = struct
  type (_, _) t = Z : (z, _) t | S : ('n, 'm) t -> ('n s, 'm s) t

  let rec refl : type n. n nat -> (n, n) t = function
    | Z ->
        Z
    | S n ->
        S (refl n)

  let rec trans : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
   fun t1 t2 -> match (t1, t2) with Z, _ -> Z | S t1, S t2 -> S (trans t1 t2)
end

module N0 = struct
  type 'a plus_n = 'a

  type n = z

  let n = Z

  let add m = (m, Adds.Z)

  let eq = Core_kernel.Type_equal.T
end

module Add = struct
  module type Intf = sig
    type _ plus_n

    type n

    val eq : (n, z plus_n) Core_kernel.Type_equal.t

    val n : z plus_n t

    val add : 'm nat -> 'm plus_n nat * (z plus_n, 'm, 'm plus_n) Adds.t
  end

  let rec create : type n. n nat -> (module Intf with type n = n) = function
    | Z ->
        (module N0)
    | S n ->
        let (module N) = create n in
        let T = N.eq in
        let module Sn = struct
          type 'a plus_n = 'a N.plus_n s

          type n = N.n s

          let n = S N.n

          let eq = Core_kernel.Type_equal.T

          let add t =
            let t_plus_n, pi = N.add t in
            (S t_plus_n, Adds.S pi)
        end in
        (module Sn)

  let n : type n. (module Intf with type n = n) -> n nat =
   fun (module N) ->
    let T = N.eq in
    N.n

  module type Intf_transparent = sig
    type _ plus_n

    type n = z plus_n

    val eq : (n, z plus_n) Core_kernel.Type_equal.t

    val n : z plus_n t

    val add : 'm nat -> 'm plus_n nat * (z plus_n, 'm, 'm plus_n) Adds.t
  end
end

module S (N : Add.Intf) = struct
  type 'a plus_n = 'a N.plus_n s

  type n = z plus_n

  let n = S N.n

  let add m =
    let k, pi = N.add m in
    (S k, Adds.S pi)

  let eq = match N.eq with T -> Core_kernel.Type_equal.T
end

module Sum = struct
  type (_, _) t =
    | [] : (unit, z) t
    | ( :: ) :
        ('term, 'prev_total, 'total) Adds.t * ('terms, 'prev_total) t
        -> ('term * 'terms, 'total) t

  let rec eq_exn : type ns1 ns2 total1 total2.
         (ns1, total1) t
      -> (ns2, total2) t
      -> (ns1, ns2) Core_kernel.Type_equal.t
         * (total1, total2) Core_kernel.Type_equal.t =
   fun xs ys ->
    match (xs, ys) with
    | [], [] ->
        (T, T)
    | x :: xs, y :: ys ->
        let T, T = eq_exn xs ys in
        let T, T = Adds.eq_inner_exn x y in
        (T, T)
    | _ ->
        failwith "Sum.eq_exn"
end

module N1 = S (N0)
module N2 = S (N1)
module N3 = S (N2)
module N4 = S (N3)
module N5 = S (N4)
module N6 = S (N5)
module N7 = S (N6)
module N8 = S (N7)
module N9 = S (N8)
module N10 = S (N9)
module N11 = S (N10)
module N12 = S (N11)
module N13 = S (N12)
module N14 = S (N13)
module N15 = S (N14)
module N16 = S (N15)
module N17 = S (N16)
module N18 = S (N17)
module N19 = S (N18)
module N20 = S (N19)
module N21 = S (N20)
module N22 = S (N21)
module N23 = S (N22)
module N24 = S (N23)
module N25 = S (N24)
module N26 = S (N25)
module N27 = S (N26)
module N28 = S (N27)
module N29 = S (N28)
module N30 = S (N29)

module Empty = struct
  type t = T of t

  let rec elim : type a. t -> a = function T t -> elim t
end

module Not = struct
  type 'a t = 'a -> Empty.t
end

open Core_kernel

let rec compare : type n m.
    n t -> m t -> [`Lte of (n, m) Lte.t | `Gt of (n, m) Lte.t Not.t] =
 fun n m ->
  match (n, m) with
  | Z, _ ->
      `Lte Lte.Z
  | S _, Z ->
      `Gt (function _ -> .)
  | S n, S m -> (
    match compare n m with
    | `Lte pi ->
        `Lte (S pi)
    | `Gt gt ->
        `Gt (function S pi -> gt pi) )

let lte_exn n m =
  match compare n m with `Lte pi -> pi | `Gt _gt -> failwith "lte_exn"

let rec gt_implies_gte : type n m.
    n nat -> m nat -> (n, m) Lte.t Not.t -> (m, n) Lte.t =
 fun n m not_lte ->
  match (n, m) with
  | Z, _ ->
      Empty.elim (not_lte Z)
  | S _, Z ->
      Z
  | S n, S m ->
      S (gt_implies_gte n m (fun pi -> not_lte (S pi)))

let rec eq : type n m.
       n nat
    -> m nat
    -> [`Equal of (n, m) Type_equal.t | `Not_equal of (n, m) Type_equal.t Not.t]
    =
 fun n m ->
  match (n, m) with
  | Z, Z ->
      `Equal T
  | S _, Z ->
      `Not_equal (function _ -> .)
  | Z, S _ ->
      `Not_equal (function _ -> .)
  | S n, S m -> (
    match eq n m with
    | `Equal T ->
        `Equal T
    | `Not_equal f ->
        `Not_equal (function T -> f T) )

let eq_exn : type n m. n nat -> m nat -> (n, m) Type_equal.t =
 fun n m ->
  match eq n m with
  | `Equal t ->
      t
  | `Not_equal _ ->
      failwithf "eq_exn: %d vs %d" (to_int n) (to_int m) ()
