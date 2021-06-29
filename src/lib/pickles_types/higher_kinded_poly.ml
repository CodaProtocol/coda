(* Caution: We do some low-level type manipulation to extract the value here.
   This is safe because:
   * the input types [_ t] are parameterized over [t1, ..., tn] which are
     distinct opaque types which cannot unify
   * the polymorphic types are parameterized over [(t1, ..., tn) t], which can
     only unify when the underlying parameterised type is the same
   * the underlying representation of OCaml values is homogeneous, which is
     what allows for polymorphism in the wider language.

   This can be used to approximate 'modular explicits' using packed modules
   instead of substitutive module application, but it is generally more
   powerful because it makes parameterised types first class outside of arrows
   too (e.g. in heterogeneous lists).
*)

open Core_kernel
open Poly_types

(* Opaque types. *)
module O = struct
  type t1

  type t2

  type t3

  type t4

  type t5
end

module P1 = struct
  (** This type is the application of a type [_ x] to the variable ['a],
      represented as [('a, O.t1 x) t].
  *)
  type (_, _) t

  module W (M : T1) = struct
    type t = O.t1 M.t
  end

  module type S = sig
    type _ p

    type witness

    val eq : (O.t1 p, witness) Type_equal.t

    type 'a1 poly = ('a1, witness) t

    val mk_eq : unit -> ('a1 poly, 'a1 p) Type_equal.t

    val to_poly : 'a1 p -> 'a1 poly

    val of_poly : 'a1 poly -> 'a1 p
  end

  module T (M : T1) : S with type 'a p = 'a M.t and type witness = W(M).t =
  struct
    type 'a p = 'a M.t

    type witness = W(M).t

    let eq = Type_equal.T

    type 'a1 poly = ('a1, witness) t

    (* Here be dragons *)

    let mk_eq () = Obj.magic Type_equal.T

    let to_poly = Obj.magic

    let of_poly = Obj.magic
  end
end

module P2 = struct
  (** This type is the application of a type [(_, _) x] to the variables ['a1],
      ['a2], represented as [('a1, 'a2, (O.t1, O.t2) x) t].
  *)
  type (_, _, _) t

  module W (M : T2) = struct
    type t = (O.t1, O.t2) M.t
  end

  module type S = sig
    type (_, _) p

    type witness

    val eq : ((O.t1, O.t2) p, witness) Type_equal.t

    type ('a1, 'a2) poly = ('a1, 'a2, witness) t

    val mk_eq : unit -> (('a1, 'a2) poly, ('a1, 'a2) p) Type_equal.t

    val to_poly : ('a1, 'a2) p -> ('a1, 'a2) poly

    val of_poly : ('a1, 'a2) poly -> ('a1, 'a2) p
  end

  module T (M : T2) :
    S with type ('a1, 'a2) p = ('a1, 'a2) M.t and type witness = W(M).t =
  struct
    type ('a1, 'a2) p = ('a1, 'a2) M.t

    type witness = W(M).t

    let eq = Type_equal.T

    type ('a1, 'a2) poly = ('a1, 'a2, witness) t

    (* Here be dragons *)

    let mk_eq () = Obj.magic Type_equal.T

    let to_poly = Obj.magic

    let of_poly = Obj.magic
  end
end

module P3 = struct
  (** This type is the application of a type [(_, _, _) x] to the variables
      ['a1], ['a2], ['a3], represented as
      [('a1, 'a2, 'a3, (O.t1, O.t2, O.t3) x) t].
  *)
  type (_, _, _, _) t

  module W (M : T3) = struct
    type t = (O.t1, O.t2, O.t3) M.t
  end

  module type S = sig
    type (_, _, _) p

    type witness

    val eq : ((O.t1, O.t2, O.t3) p, witness) Type_equal.t

    type ('a1, 'a2, 'a3) poly = ('a1, 'a2, 'a3, witness) t

    val mk_eq : unit -> (('a1, 'a2, 'a3) poly, ('a1, 'a2, 'a3) p) Type_equal.t

    val to_poly : ('a1, 'a2, 'a3) p -> ('a1, 'a2, 'a3) poly

    val of_poly : ('a1, 'a2, 'a3) poly -> ('a1, 'a2, 'a3) p
  end

  module T (M : T3) :
    S
    with type ('a1, 'a2, 'a3) p = ('a1, 'a2, 'a3) M.t
     and type witness = W(M).t = struct
    type ('a1, 'a2, 'a3) p = ('a1, 'a2, 'a3) M.t

    type witness = W(M).t

    let eq = Type_equal.T

    type ('a1, 'a2, 'a3) poly = ('a1, 'a2, 'a3, witness) t

    (* Here be dragons *)

    let mk_eq () = Obj.magic Type_equal.T

    let to_poly = Obj.magic

    let of_poly = Obj.magic
  end
end

module P4 = struct
  (** This type is the application of a type [(_, _, _, _) x] to the variables
      ['a1], ['a2], ['a3], ['a4] represented as
      [('a1, 'a2, 'a3, 'a4, (O.t1, O.t2, O.t3, O.t4) x) t].
  *)
  type (_, _, _, _, _) t

  module W (M : T4) = struct
    type t = (O.t1, O.t2, O.t3, O.t4) M.t
  end

  module type S = sig
    type (_, _, _, _) p

    type witness

    val eq : ((O.t1, O.t2, O.t3, O.t4) p, witness) Type_equal.t

    type ('a1, 'a2, 'a3, 'a4) poly = ('a1, 'a2, 'a3, 'a4, witness) t

    val mk_eq :
      unit -> (('a1, 'a2, 'a3, 'a4) poly, ('a1, 'a2, 'a3, 'a4) p) Type_equal.t

    val to_poly : ('a1, 'a2, 'a3, 'a4) p -> ('a1, 'a2, 'a3, 'a4) poly

    val of_poly : ('a1, 'a2, 'a3, 'a4) poly -> ('a1, 'a2, 'a3, 'a4) p
  end

  module T (M : T4) :
    S
    with type ('a1, 'a2, 'a3, 'a4) p = ('a1, 'a2, 'a3, 'a4) M.t
     and type witness = W(M).t = struct
    type ('a1, 'a2, 'a3, 'a4) p = ('a1, 'a2, 'a3, 'a4) M.t

    type witness = W(M).t

    let eq = Type_equal.T

    type ('a1, 'a2, 'a3, 'a4) poly = ('a1, 'a2, 'a3, 'a4, witness) t

    (* Here be dragons *)

    let mk_eq () = Obj.magic Type_equal.T

    let to_poly = Obj.magic

    let of_poly = Obj.magic
  end
end

module P5 = struct
  (** This type is the application of a type [(_, _, _, _, _) x] to the
      variables ['a1], ['a2], ['a3], ['a4], ['a5] represented as
      [('a1, 'a2, 'a3, 'a4, 'a5, (O.t1, O.t2, O.t3, O.t4, O.t5) x) t].
  *)
  type (_, _, _, _, _, _) t

  module W (M : T5) = struct
    type t = (O.t1, O.t2, O.t3, O.t4, O.t5) M.t
  end

  module type S = sig
    type (_, _, _, _, _) p

    type witness

    val eq : ((O.t1, O.t2, O.t3, O.t4, O.t5) p, witness) Type_equal.t

    type ('a1, 'a2, 'a3, 'a4, 'a5) poly = ('a1, 'a2, 'a3, 'a4, 'a5, witness) t

    val mk_eq :
         unit
      -> ( ('a1, 'a2, 'a3, 'a4, 'a5) poly
         , ('a1, 'a2, 'a3, 'a4, 'a5) p )
         Type_equal.t

    val to_poly : ('a1, 'a2, 'a3, 'a4, 'a5) p -> ('a1, 'a2, 'a3, 'a4, 'a5) poly

    val of_poly : ('a1, 'a2, 'a3, 'a4, 'a5) poly -> ('a1, 'a2, 'a3, 'a4, 'a5) p
  end

  module T (M : T5) :
    S
    with type ('a1, 'a2, 'a3, 'a4, 'a5) p = ('a1, 'a2, 'a3, 'a4, 'a5) M.t
     and type witness = W(M).t = struct
    type ('a1, 'a2, 'a3, 'a4, 'a5) p = ('a1, 'a2, 'a3, 'a4, 'a5) M.t

    type witness = W(M).t

    let eq = Type_equal.T

    type ('a1, 'a2, 'a3, 'a4, 'a5) poly = ('a1, 'a2, 'a3, 'a4, 'a5, witness) t

    (* Here be dragons *)

    let mk_eq () = Obj.magic Type_equal.T

    let to_poly = Obj.magic

    let of_poly = Obj.magic
  end
end

let%test_module "Higher_kinded_poly" =
  ( module struct
    module Int_1 = struct
      type _ t = int
    end

    module Poly_int_1 = P1.T (Int_1)

    module Int_2 = struct
      type (_, _) t = int
    end

    module Poly_int_2 = P2.T (Int_2)

    module Int_3 = struct
      type (_, _, _) t = int
    end

    module Poly_int_3 = P3.T (Int_3)

    module Int_4 = struct
      type (_, _, _, _) t = int
    end

    module Poly_int_4 = P4.T (Int_4)

    module Int_5 = struct
      type (_, _, _, _, _) t = int
    end

    module Poly_int_5 = P5.T (Int_5)

    let ints = [1; 2; 3; 4; 5]

    let poly_ints_1 = List.map ~f:Poly_int_1.to_poly ints

    let poly_ints_2 = List.map ~f:Poly_int_2.to_poly ints

    let poly_ints_3 = List.map ~f:Poly_int_3.to_poly ints

    let poly_ints_4 = List.map ~f:Poly_int_4.to_poly ints

    let poly_ints_5 = List.map ~f:Poly_int_5.to_poly ints

    let%test "P1 round-trips" =
      ints = List.map ~f:Poly_int_1.of_poly poly_ints_1

    let%test "P2 round-trips" =
      ints = List.map ~f:Poly_int_2.of_poly poly_ints_2

    let%test "P3 round-trips" =
      ints = List.map ~f:Poly_int_3.of_poly poly_ints_3

    let%test "P4 round-trips" =
      ints = List.map ~f:Poly_int_4.of_poly poly_ints_4

    let%test "P5 round-trips" =
      ints = List.map ~f:Poly_int_5.of_poly poly_ints_5

    module Poly_option = P1.T (Option)

    let options = [Some 1; None; None; Some 4; Some 5]

    let poly_options = List.map ~f:Poly_option.to_poly options

    let%test "P1.T(Option) round-trips" =
      options = List.map ~f:Poly_option.of_poly poly_options

    module Ignore_1 = struct
      type ('a, _) t = 'a * unit
    end

    module Poly_ignore_1 = P2.T (Ignore_1)

    module Ignore_2 = struct
      type ('a, _) t = 'a * unit
    end

    module Poly_ignore_2 = P2.T (Ignore_2)

    let num_unit_tuples = [(1, ()); (2, ()); (3, ()); (4, ())]

    let ignore_1s : (int, _, _) P2.t list =
      List.map ~f:Poly_ignore_1.to_poly num_unit_tuples

    let ignore_2s : (int, _, _) P2.t list =
      List.map ~f:Poly_ignore_2.to_poly num_unit_tuples

    let ignore_1s_and_2s = ignore_1s @ ignore_2s

    let%test "Mixing of distinct unifiable types" =
      num_unit_tuples @ num_unit_tuples
      = List.map ~f:Poly_ignore_1.of_poly ignore_1s_and_2s
  end )
