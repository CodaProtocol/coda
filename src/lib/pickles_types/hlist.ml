open Core_kernel
open Poly_types

module E13 (T : T1) = struct
  type ('a, _, _) t = 'a T.t
end

module E23 (T : T2) = struct
  type ('a, 'b, _) t = ('a, 'b) T.t
end

module E01 (T : T0) = struct
  type _ t = T.t
end

module E02 (T : T0) = struct
  type (_, _) t = T.t
end

module E03 (T : T0) = struct
  type (_, _, _) t = T.t
end

module E04 (T : T0) = struct
  type (_, _, _, _) t = T.t
end

module Tuple2 (F : T3) (G : T3) = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) F.t * ('a, 'b, 'c) G.t
end

module Tuple3 (F : T3) (G : T3) (H : T3) = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) F.t * ('a, 'b, 'c) G.t * ('a, 'b, 'c) H.t
end

module Tuple4 (F : T3) (G : T3) (H : T3) (I : T3) = struct
  type ('a, 'b, 'c) t =
    ('a, 'b, 'c) F.t * ('a, 'b, 'c) G.t * ('a, 'b, 'c) H.t * ('a, 'b, 'c) I.t
end

module Tuple5 (F : T3) (G : T3) (H : T3) (I : T3) (J : T3) = struct
  type ('a, 'b, 'c) t =
    ('a, 'b, 'c) F.t
    * ('a, 'b, 'c) G.t
    * ('a, 'b, 'c) H.t
    * ('a, 'b, 'c) I.t
    * ('a, 'b, 'c) J.t
end

module Fst = struct
  type ('a, _, _) t = 'a
end

module Snd = struct
  type (_, 'a, _) t = 'a
end

module Apply2 (F : T2) (X : T1) (Y : T1) = struct
  type ('a, 'b) t = ('a X.t, 'b Y.t) F.t
end

module Dup (F : T2) = struct
  type 'a t = ('a, 'a) F.t
end

module Length = Hlist0.Length

module H1 = struct
  module T = Hlist0.H1

  module Iter
      (F : T1) (C : sig
          val f : 'a F.t -> unit
      end) =
  struct
    let rec f : type a. a T(F).t -> unit = function
      | [] ->
          ()
      | x :: xs ->
          C.f x ; f xs
  end

  module Of_vector (X : T0) = struct
    let rec f : type xs length.
        (xs, length) Length.t -> (X.t, length) Vector.t -> xs T(E01(X)).t =
     fun l1 v ->
      match (l1, v) with Z, [] -> [] | S n1, x :: xs -> x :: f n1 xs
  end

  module Map
      (F : T1)
      (G : T1) (C : sig
          val f : 'a F.t -> 'a G.t
      end) =
  struct
    let rec f : type a. a T(F).t -> a T(G).t = function
      | [] ->
          []
      | x :: xs ->
          let y = C.f x in
          y :: f xs
  end

  module Fold
      (F : T1)
      (X : T0) (C : sig
          val f : X.t -> 'a F.t -> X.t
      end) =
  struct
    let rec f : type a. init:X.t -> a T(F).t -> X.t =
     fun ~init xs ->
      match xs with [] -> init | x :: xs -> f ~init:(C.f init x) xs
  end

  module Map_reduce
      (F : T1)
      (X : T0) (C : sig
          val reduce : X.t -> X.t -> X.t

          val map : 'a F.t -> X.t
      end) =
  struct
    let rec f : type a. X.t -> a T(F).t -> X.t =
     fun acc xs ->
      match xs with [] -> acc | x :: xs -> f (C.reduce acc (C.map x)) xs

    let f (type a) (xs : a T(F).t) =
      match xs with
      | [] ->
          failwith "Hlist.Map_reduce: empty list"
      | x :: xs ->
          f (C.map x) xs
  end

  module To_vector (X : T0) = struct
    let rec f : type xs length.
        (xs, length) Length.t -> xs T(E01(X)).t -> (X.t, length) Vector.t =
     fun l1 v ->
      match (l1, v) with Z, [] -> [] | S n1, x :: xs -> x :: f n1 xs
  end

  module Tuple2 (F : T1) (G : T1) = struct
    type 'a t = 'a F.t * 'a G.t
  end

  module Zip (F : T1) (G : T1) = struct
    let rec f : type a. a T(F).t -> a T(G).t -> a T(Tuple2(F)(G)).t =
     fun xs ys ->
      match (xs, ys) with
      | [], [] ->
          []
      | x :: xs, y :: ys ->
          (x, y) :: f xs ys
  end

  module Typ (Impl : sig
    type field
  end)
  (F : T1)
  (Var : T1)
  (Val : T1) (C : sig
      val f :
        'a F.t -> ('a Var.t, 'a Val.t, Impl.field) Snarky_backendless.Typ.t
  end) =
  struct
    let rec f : type xs.
           xs T(F).t
        -> (xs T(Var).t, xs T(Val).t, Impl.field) Snarky_backendless.Typ.t =
      let transport, transport_var, tuple2, unit =
        Snarky_backendless.Typ.(transport, transport_var, tuple2, unit)
      in
      fun ts ->
        match ts with
        | t :: ts ->
            let tail = f ts in
            transport
              (tuple2 (C.f t) tail)
              ~there:(fun (x :: xs : _ T(Val).t) -> (x, xs))
              ~back:(fun (x, xs) -> x :: xs)
            |> transport_var
                 ~there:(fun (x :: xs : _ T(Var).t) -> (x, xs))
                 ~back:(fun (x, xs) -> x :: xs)
        | [] ->
            let there _ = () in
            transport (unit ()) ~there ~back:(fun () -> ([] : _ T(Val).t))
            |> transport_var ~there ~back:(fun () -> ([] : _ T(Var).t))
  end
end

module H2 = struct
  module Fst = struct
    type ('a, _) t = 'a
  end

  module Tuple2 (F : T2) (G : T2) = struct
    type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t
  end

  module T (F : T2) = struct
    type (_, _) t =
      | [] : (unit, unit) t
      | ( :: ) : ('a1, 'a2) F.t * ('b1, 'b2) t -> ('a1 * 'b1, 'a2 * 'b2) t

    let rec length : type tail1 tail2. (tail1, tail2) t -> tail1 Length.n =
      function
      | [] ->
          T (Z, Z)
      | _ :: xs ->
          let (T (n, p)) = length xs in
          T (S n, S p)
  end

  module Zip (F : T2) (G : T2) = struct
    let rec f : type a b.
        (a, b) T(F).t -> (a, b) T(G).t -> (a, b) T(Tuple2(F)(G)).t =
     fun xs ys ->
      match (xs, ys) with
      | [], [] ->
          []
      | x :: xs, y :: ys ->
          (x, y) :: f xs ys
  end

  module Map
      (F : T2)
      (G : T2) (C : sig
          val f : ('a, 'b) F.t -> ('a, 'b) G.t
      end) =
  struct
    let rec f : type a b. (a, b) T(F).t -> (a, b) T(G).t = function
      | [] ->
          []
      | x :: xs ->
          let y = C.f x in
          y :: f xs
  end
end

module Hlist_1 (F : sig
  type (_, _) t
end) =
struct
  type (_, 's) t =
    | [] : (unit, _) t
    | ( :: ) : ('a1, 's) F.t * ('b1, 's) t -> ('a1 * 'b1, 's) t
end

module H3_2 = struct
  module T (F : sig
    type (_, _, _, _, _) t
  end) =
  struct
    type (_, _, _, 's1, 's2) t =
      | [] : (unit, unit, unit, _, _) t
      | ( :: ) :
          ('a1, 'a2, 'a3, 's1, 's2) F.t * ('b1, 'b2, 'b3, 's1, 's2) t
          -> ('a1 * 'b1, 'a2 * 'b2, 'a3 * 'b3, 's1, 's2) t

    let rec length : type t1 t2 t3 e1 e2. (t1, t2, t3, e1, e2) t -> t1 Length.n
        = function
      | [] ->
          T (Z, Z)
      | _ :: xs ->
          let (T (n, p)) = length xs in
          T (S n, S p)
  end
end

module H3_3 = struct
  module T (F : sig
    type (_, _, _, _, _, _) t
  end) =
  struct
    type (_, _, _, 's1, 's2, 's3) t =
      | [] : (unit, unit, unit, _, _, _) t
      | ( :: ) :
          ('a1, 'a2, 'a3, 's1, 's2, 's3) F.t * ('b1, 'b2, 'b3, 's1, 's2, 's3) t
          -> ('a1 * 'b1, 'a2 * 'b2, 'a3 * 'b3, 's1, 's2, 's3) t

    let rec length : type t1 t2 t3 e1 e2 e3.
        (t1, t2, t3, e1, e2, e3) t -> t1 Length.n = function
      | [] ->
          T (Z, Z)
      | _ :: xs ->
          let (T (n, p)) = length xs in
          T (S n, S p)
  end
end

module H3_4 = struct
  module T (F : sig
    type (_, _, _, _, _, _, _) t
  end) =
  struct
    type (_, _, _, 's1, 's2, 's3, 's4) t =
      | [] : (unit, unit, unit, _, _, _, _) t
      | ( :: ) :
          ('a1, 'a2, 'a3, 's1, 's2, 's3, 's4) F.t
          * ('b1, 'b2, 'b3, 's1, 's2, 's3, 's4) t
          -> ('a1 * 'b1, 'a2 * 'b2, 'a3 * 'b3, 's1, 's2, 's3, 's4) t

    let rec length : type t1 t2 t3 e1 e2 e3 e4.
        (t1, t2, t3, e1, e2, e3, e4) t -> t1 Length.n = function
      | [] ->
          T (Z, Z)
      | _ :: xs ->
          let (T (n, p)) = length xs in
          T (S n, S p)
  end
end

module H2_1 = struct
  module T (F : sig
    type (_, _, _) t
  end) =
  struct
    type (_, _, 's) t =
      | [] : (unit, unit, _) t
      | ( :: ) :
          ('a1, 'a2, 's) F.t * ('b1, 'b2, 's) t
          -> ('a1 * 'b1, 'a2 * 'b2, 's) t

    let rec length : type tail1 tail2 e. (tail1, tail2, e) t -> tail1 Length.n
        = function
      | [] ->
          T (Z, Z)
      | _ :: xs ->
          let (T (n, p)) = length xs in
          T (S n, S p)
  end

  module Iter
      (F : T3) (C : sig
          val f : ('a, 'b, 'c) F.t -> unit
      end) =
  struct
    let rec f : type a b c. (a, b, c) T(F).t -> unit = function
      | [] ->
          ()
      | x :: xs ->
          C.f x ; f xs
  end

  module Map_
      (F : T3)
      (G : T3) (Env : sig
          type t
      end) (C : sig
        val f : ('a, 'b, Env.t) F.t -> ('a, 'b, Env.t) G.t
      end) =
  struct
    let rec f : type a b. (a, b, Env.t) T(F).t -> (a, b, Env.t) T(G).t =
      function
      | [] ->
          []
      | x :: xs ->
          let y = C.f x in
          y :: f xs
  end

  module Map
      (F : T3)
      (G : T3) (C : sig
          val f : ('a, 'b, 'c) F.t -> ('a, 'b, 'c) G.t
      end) =
  struct
    let f : type a b c. (a, b, c) T(F).t -> (a, b, c) T(G).t =
     fun xs ->
      let module M =
        Map_ (F) (G)
          (struct
            type t = c
          end)
          (struct
            let f = C.f
          end)
      in
      M.f xs
  end

  module Zip (F : T3) (G : T3) = struct
    let rec f : type a b c.
        (a, b, c) T(F).t -> (a, b, c) T(G).t -> (a, b, c) T(Tuple2(F)(G)).t =
     fun xs ys ->
      match (xs, ys) with
      | [], [] ->
          []
      | x :: xs, y :: ys ->
          (x, y) :: f xs ys
  end

  module Zip3 (F1 : T3) (F2 : T3) (F3 : T3) = struct
    let rec f : type a b c.
           (a, b, c) T(F1).t
        -> (a, b, c) T(F2).t
        -> (a, b, c) T(F3).t
        -> (a, b, c) T(Tuple3(F1)(F2)(F3)).t =
     fun xs ys zs ->
      match (xs, ys, zs) with
      | [], [], [] ->
          []
      | x :: xs, y :: ys, z :: zs ->
          (x, y, z) :: f xs ys zs
  end

  module Unzip3 (F1 : T3) (F2 : T3) (F3 : T3) = struct
    let rec f : type a b c.
           (a, b, c) T(Tuple3(F1)(F2)(F3)).t
        -> (a, b, c) T(F1).t * (a, b, c) T(F2).t * (a, b, c) T(F3).t =
     fun ts ->
      match ts with
      | [] ->
          ([], [], [])
      | (x, y, z) :: ts ->
          let xs, ys, zs = f ts in
          (x :: xs, y :: ys, z :: zs)
  end

  module Zip4 (F1 : T3) (F2 : T3) (F3 : T3) (F4 : T3) = struct
    let rec f : type a b c.
           (a, b, c) T(F1).t
        -> (a, b, c) T(F2).t
        -> (a, b, c) T(F3).t
        -> (a, b, c) T(F4).t
        -> (a, b, c) T(Tuple4(F1)(F2)(F3)(F4)).t =
     fun xs ys zs ws ->
      match (xs, ys, zs, ws) with
      | [], [], [], [] ->
          []
      | x :: xs, y :: ys, z :: zs, w :: ws ->
          (x, y, z, w) :: f xs ys zs ws
  end

  module Zip5 (F1 : T3) (F2 : T3) (F3 : T3) (F4 : T3) (F5 : T3) = struct
    let rec f : type a b c.
           (a, b, c) T(F1).t
        -> (a, b, c) T(F2).t
        -> (a, b, c) T(F3).t
        -> (a, b, c) T(F4).t
        -> (a, b, c) T(F5).t
        -> (a, b, c) T(Tuple5(F1)(F2)(F3)(F4)(F5)).t =
     fun l1 l2 l3 l4 l5 ->
      match (l1, l2, l3, l4, l5) with
      | [], [], [], [], [] ->
          []
      | x1 :: l1, x2 :: l2, x3 :: l3, x4 :: l4, x5 :: l5 ->
          (x1, x2, x3, x4, x5) :: f l1 l2 l3 l4 l5
  end

  module Of_vector (X : T0) = struct
    let rec f : type e xs ys length.
           (xs, length) Length.t
        -> (ys, length) Length.t
        -> (X.t, length) Vector.t
        -> (xs, ys, e) T(E03(X)).t =
     fun l1 l2 v ->
      match (l1, l2, v) with
      | Z, Z, [] ->
          []
      | S n1, S n2, x :: xs ->
          x :: f n1 n2 xs
  end

  module To_vector (X : T0) = struct
    let rec f : type e xs ys length.
           (xs, length) Length.t
        -> (xs, ys, e) T(E03(X)).t
        -> (X.t, length) Vector.t =
     fun l1 v ->
      match (l1, v) with Z, [] -> [] | S n1, x :: xs -> x :: f n1 xs
  end
end

module H3 = struct
  module T (F : sig
    type (_, _, _) t
  end) =
  struct
    type (_, _, _) t =
      | [] : (unit, unit, unit) t
      | ( :: ) :
          ('a1, 'a2, 'a3) F.t * ('b1, 'b2, 'b3) t
          -> ('a1 * 'b1, 'a2 * 'b2, 'a3 * 'b3) t

    let rec length : type tail1 tail2 tail3.
        (tail1, tail2, tail3) t -> tail1 Length.n = function
      | [] ->
          T (Z, Z)
      | _ :: xs ->
          let (T (n, p)) = length xs in
          T (S n, S p)
  end

  module To_vector (X : T0) = struct
    let rec f : type a b c length.
        (a, length) Length.t -> (a, b, c) T(E03(X)).t -> (X.t, length) Vector.t
        =
     fun l1 v ->
      match (l1, v) with Z, [] -> [] | S n1, x :: xs -> x :: f n1 xs
  end

  module Zip (F : T3) (G : T3) = struct
    let rec f : type a b c.
        (a, b, c) T(F).t -> (a, b, c) T(G).t -> (a, b, c) T(Tuple2(F)(G)).t =
     fun xs ys ->
      match (xs, ys) with
      | [], [] ->
          []
      | x :: xs, y :: ys ->
          (x, y) :: f xs ys
  end

  module Fst = struct
    type ('a, _, _) t = 'a
  end

  module Map1_to_H1
      (F : T3)
      (G : T1) (C : sig
          val f : ('a, 'b, 'c) F.t -> 'a G.t
      end) =
  struct
    let rec f : type a b c. (a, b, c) T(F).t -> a H1.T(G).t = function
      | [] ->
          []
      | x :: xs ->
          let y = C.f x in
          y :: f xs
  end

  module Map2_to_H1
      (F : T3)
      (G : T1) (C : sig
          val f : ('a, 'b, 'c) F.t -> 'b G.t
      end) =
  struct
    let rec f : type a b c. (a, b, c) T(F).t -> b H1.T(G).t = function
      | [] ->
          []
      | x :: xs ->
          let y = C.f x in
          y :: f xs
  end

  module Map
      (F : T3)
      (G : T3) (C : sig
          val f : ('a, 'b, 'c) F.t -> ('a, 'b, 'c) G.t
      end) =
  struct
    let rec f : type a b c. (a, b, c) T(F).t -> (a, b, c) T(G).t = function
      | [] ->
          []
      | x :: xs ->
          let y = C.f x in
          y :: f xs
  end
end

module H4 = struct
  module T (F : sig
    type (_, _, _, _) t
  end) =
  struct
    type (_, _, _, _) t =
      | [] : (unit, unit, unit, unit) t
      | ( :: ) :
          ('a1, 'a2, 'a3, 'a4) F.t * ('b1, 'b2, 'b3, 'b4) t
          -> ('a1 * 'b1, 'a2 * 'b2, 'a3 * 'b3, 'a4 * 'b4) t

    let rec length : type tail1 tail2 tail3 tail4.
        (tail1, tail2, tail3, tail4) t -> tail1 Length.n = function
      | [] ->
          T (Z, Z)
      | _ :: xs ->
          let (T (n, p)) = length xs in
          T (S n, S p)
  end

  module Fold
      (F : T4)
      (X : T0) (C : sig
          val f : X.t -> _ F.t -> X.t
      end) =
  struct
    let rec f : type a b c d. init:X.t -> (a, b, c, d) T(F).t -> X.t =
     fun ~init xs ->
      match xs with [] -> init | x :: xs -> f ~init:(C.f init x) xs
  end

  module Iter
      (F : T4) (C : sig
          val f : _ F.t -> unit
      end) =
  struct
    let rec f : type a b c d. (a, b, c, d) T(F).t -> unit =
     fun xs -> match xs with [] -> () | x :: xs -> C.f x ; f xs
  end

  module Map
      (F : T4)
      (G : T4) (C : sig
          val f : ('a, 'b, 'c, 'd) F.t -> ('a, 'b, 'c, 'd) G.t
      end) =
  struct
    let rec f : type a b c d. (a, b, c, d) T(F).t -> (a, b, c, d) T(G).t =
      function
      | [] ->
          []
      | x :: xs ->
          let y = C.f x in
          y :: f xs
  end

  module To_vector (X : T0) = struct
    let rec f : type a b c d length.
           (a, length) Length.t
        -> (a, b, c, d) T(E04(X)).t
        -> (X.t, length) Vector.t =
     fun l1 v ->
      match (l1, v) with Z, [] -> [] | S n1, x :: xs -> x :: f n1 xs
  end

  module Tuple2 (F : T4) (G : T4) = struct
    type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t * ('a, 'b, 'c, 'd) G.t
  end

  module Zip (F : T4) (G : T4) = struct
    let rec f : type a b c d.
           (a, b, c, d) T(F).t
        -> (a, b, c, d) T(G).t
        -> (a, b, c, d) T(Tuple2(F)(G)).t =
     fun xs ys ->
      match (xs, ys) with
      | [], [] ->
          []
      | x :: xs, y :: ys ->
          (x, y) :: f xs ys
  end

  module Length_1_to_2 (F : T4) = struct
    let rec f : type n xs ys a b.
        (xs, ys, a, b) T(F).t -> (xs, n) Length.t -> (ys, n) Length.t =
     fun xs n -> match (xs, n) with [], Z -> Z | _ :: xs, S n -> S (f xs n)
  end

  module Typ (Impl : sig
    type field
  end)
  (F : T4)
  (Var : T3)
  (Val : T3) (C : sig
      val f :
           ('var, 'value, 'n1, 'n2) F.t
        -> ( ('var, 'n1, 'n2) Var.t
           , ('value, 'n1, 'n2) Val.t
           , Impl.field )
           Snarky_backendless.Typ.t
  end) =
  struct
    let transport, transport_var, tuple2, unit =
      Snarky_backendless.Typ.(transport, transport_var, tuple2, unit)

    let rec f : type vars values ns1 ns2.
           (vars, values, ns1, ns2) T(F).t
        -> ( (vars, ns1, ns2) H3.T(Var).t
           , (values, ns1, ns2) H3.T(Val).t
           , Impl.field )
           Snarky_backendless.Typ.t =
     fun ts ->
      match ts with
      | [] ->
          let there _ = () in
          transport (unit ()) ~there ~back:(fun () -> ([] : _ H3.T(Val).t))
          |> transport_var ~there ~back:(fun () -> ([] : _ H3.T(Var).t))
      | t :: ts ->
          transport
            (tuple2 (C.f t) (f ts))
            ~there:(fun (x :: xs : _ H3.T(Val).t) -> (x, xs))
            ~back:(fun (x, xs) -> x :: xs)
          |> transport_var
               ~there:(fun (x :: xs : _ H3.T(Var).t) -> (x, xs))
               ~back:(fun (x, xs) -> x :: xs)
  end
end

module H4_2 = struct
  module T (F : sig
    type (_, _, _, _, _, _) t
  end) =
  struct
    type (_, _, _, _, 's1, 's2) t =
      | [] : (unit, unit, unit, unit, _, _) t
      | ( :: ) :
          ('a1, 'a2, 'a3, 'a4, 's1, 's2) F.t * ('b1, 'b2, 'b3, 'b4, 's1, 's2) t
          -> ('a1 * 'b1, 'a2 * 'b2, 'a3 * 'b3, 'a4 * 'b4, 's1, 's2) t

    let rec length : type t1 t2 t3 t4 e1 e2.
        (t1, t2, t3, t4, e1, e2) t -> t1 Length.n = function
      | [] ->
          T (Z, Z)
      | _ :: xs ->
          let (T (n, p)) = length xs in
          T (S n, S p)
  end
end

module Hlist3_1 (F : sig
  type (_, _, _, _) t
end) =
struct
  type (_, _, _, 's) t =
    | [] : (unit, unit, unit, _) t
    | ( :: ) :
        ('a1, 'a2, 'a3, 's) F.t * ('b1, 'b2, 'b3, 's) t
        -> ('a1 * 'b1, 'a2 * 'b2, 'a3 * 'b3, 's) t

  let rec length : type tail1 tail2 tail3 e.
      (tail1, tail2, tail3, e) t -> tail1 Length.n = function
    | [] ->
        T (Z, Z)
    | _ :: xs ->
        let (T (n, p)) = length xs in
        T (S n, S p)
end

module Id = Hlist0.Id
module HlistId = Hlist0.HlistId

module Map_1_specific
    (F : T2)
    (G : T2) (C : sig
        type b1

        type b2

        val f : ('a, b1) F.t -> ('a, b2) G.t
    end) =
struct
  let rec f : type a. (a, C.b1) Hlist_1(F).t -> (a, C.b2) Hlist_1(G).t =
    function
    | [] ->
        []
    | x :: xs ->
        let y = C.f x in
        y :: f xs
end

open Nat

module type Max_s = sig
  type ns

  type n

  val n : n t

  val p : (ns, n) Hlist_1(Lte).t
end

type 'ns max = (module Max_s with type ns = 'ns)

let rec max : type n ns. (n * ns) H1.T(Nat).t -> (n * ns) max =
 fun xs ->
  match xs with
  | [x] ->
      let module M = struct
        type nonrec ns = n * ns

        type nonrec n = n

        let n = x

        let p : (_, _) Hlist_1(Lte).t = [Lte.refl x]
      end in
      (module M : Max_s with type ns = n * ns)
  | x :: (_ :: _ as ys) -> (
      let (module Max) = max ys in
      match compare x Max.n with
      | `Lte p_x ->
          let module M = struct
            type nonrec ns = n * ns

            type n = Max.n

            let n = Max.n

            let p : (ns, Max.n) Hlist_1(Lte).t = p_x :: Max.p
          end in
          (module M)
      | `Gt gt ->
          let max_lt_x = gt_implies_gte x Max.n gt in
          let module M =
            Map_1_specific (Lte) (Lte)
              (struct
                type b1 = Max.n

                type b2 = n

                let f : type a. (a, Max.n) Lte.t -> (a, n) Lte.t =
                 fun a_lt_max -> Lte.trans a_lt_max max_lt_x
              end)
          in
          let module M : Max_s with type ns = n * ns = struct
            type nonrec ns = n * ns

            type nonrec n = n

            let n = x

            let p : (ns, n) Hlist_1(Lte).t = Lte.refl x :: M.f Max.p
          end in
          (module M) )

let max_exn : type ns. ns H1.T(Nat).t -> ns max = function
  | [] ->
      failwith "max_exn: empty list"
  | _ :: _ as xs ->
      max xs

module Maxes = struct
  module type S = sig
    type ns

    type length

    val length : (ns, length) Length.t

    val maxes : ns H1.T(Nat).t
  end

  type 'length t = T : 'ns H1.T(Nat).t * ('ns, 'length) Length.t -> 'length t

  let rec f : type branches n. ((int, branches) Vector.t, n) Vector.t -> n t =
    function
    | [] ->
        T ([], Length.Z)
    | v :: vs ->
        let (T (maxes, len)) = f vs in
        let (T n) = Nat.of_int (Vector.reduce_exn v ~f:Int.max) in
        T (n :: maxes, S len)

  let m (type length) (vs : (_, length) Vector.t) :
      (module S with type length = length) =
    let g : type length ns.
           ns H1.T(Nat).t
        -> (ns, length) Length.t
        -> (module S with type length = length) =
     fun maxes length ->
      ( module struct
        type nonrec length = length

        type nonrec ns = ns

        let length = length

        let maxes = maxes
      end )
    in
    let (T (ms, len)) = f vs in
    g ms len
end

module Lengths = struct
  let rec extract : type prev_varss ns env.
      (prev_varss, ns, env) H2_1.T(E23(Length)).t -> ns H1.T(Nat).t = function
    | [] ->
        []
    | n :: ns ->
        (* TODO: This is quadratic because of Length.to_nat *)
        Length.to_nat n :: extract ns

  type ('prev_varss, 'prev_valss, 'env) t =
    | T :
        ('prev_varss, 'ns, 'env) H2_1.T(E23(Length)).t
        -> ('prev_varss, 'prev_valss, 'env) t
end
