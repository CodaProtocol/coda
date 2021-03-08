open Poly_types

module Length = struct
  type (_, _) t =
    | Z : (unit, Nat.z) t
    | S : ('tail, 'n) t -> ('a * 'tail, 'n Nat.s) t

  let rec to_nat : type xs n. (xs, n) t -> n Nat.t = function
    | Z ->
        Z
    | S n ->
        S (to_nat n)

  type 'xs n = T : 'n Nat.t * ('xs, 'n) t -> 'xs n

  let rec contr : type xs n m.
      (xs, n) t -> (xs, m) t -> (n, m) Core_kernel.Type_equal.t =
   fun t1 t2 ->
    match (t1, t2) with
    | Z, Z ->
        T
    | S n, S m ->
        let T = contr n m in
        T
end

module H1 (F : T1) = struct
  type _ t = [] : unit t | ( :: ) : 'a F.t * 'b t -> ('a * 'b) t

  let rec length : type tail1. tail1 t -> tail1 Length.n = function
    | [] ->
        T (Z, Z)
    | _ :: xs ->
        let (T (n, p)) = length xs in
        T (S n, S p)
end

module H2 (F : T2) = struct
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

module Lengths = struct
  type 'xss n =
    | T :
        'total Nat.t * ('xss, 'ns) H2(Length).t * ('ns, 'total) Nat.Sum.t
        -> 'xss n

  let rec contr : type xss ns ms.
         (xss, ns) H2(Length).t
      -> (xss, ms) H2(Length).t
      -> (ns, ms) Core_kernel.Type_equal.t =
   fun ts1 ts2 ->
    match (ts1, ts2) with
    | [], [] ->
        T
    | t1 :: ts1, t2 :: ts2 ->
        let T = Length.contr t1 t2 in
        let T = contr ts1 ts2 in
        T
end

module H1_1 (F : sig
  type (_, _) t
end) =
struct
  type (_, 's) t =
    | [] : (unit, _) t
    | ( :: ) : ('a, 's) F.t * ('b, 's) t -> ('a * 'b, 's) t

  let rec length : type tail1 tail2. (tail1, tail2) t -> tail1 Length.n =
    function
    | [] ->
        T (Z, Z)
    | _ :: xs ->
        let (T (n, p)) = length xs in
        T (S n, S p)
end

module Id = struct
  type 'a t = 'a
end

module HlistId = H1 (Id)
