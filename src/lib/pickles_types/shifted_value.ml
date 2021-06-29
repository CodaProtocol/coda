open Core_kernel

(* Our custom constraints let us efficiently compute

   f = fun (g, t) -> (t + 2^{len(t) - 1}) g

   We want to compute

   f' = fun (g, s) -> s * g

   Let n be the field size in bits.

   For a scalar s, let t = s - 2^{n - 1}.
   t can be represented with an n bit string.

   Then

   f (g, t)
   = (t + 2^{len(t) - 1}) * g
   = (t + 2^{n - 1}) * g
   = (s - 2^{n - 1} + 2^{n - 1}) * g
   = s * g
   = f' (g, s)

   as desired.
*)

[%%versioned
module Stable = struct
  module V1 = struct
    type 'f t = Shifted_value of 'f
    [@@deriving sexp, compare, equal, yojson, hash]
  end
end]

let typ f =
  let there (Shifted_value x) = x in
  let back x = Shifted_value x in
  Snarky_backendless.Typ.(
    transport_var (transport f ~there ~back) ~there ~back)

let map (Shifted_value x) ~f = Shifted_value (f x)

module type Field_intf = sig
  type t

  val size_in_bits : int

  val ( - ) : t -> t -> t

  val ( + ) : t -> t -> t

  val one : t
end

module Shift : sig
  type 'f t = private 'f

  val create : (module Field_intf with type t = 'f) -> 'f t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end = struct
  type 'f t = 'f

  let map t ~f = f t

  (* 2^{field size in bits} *)
  let create (type f) (module F : Field_intf with type t = f) : f t =
    let rec two_to_the n =
      if n = 0 then F.one
      else
        let r = two_to_the (n - 1) in
        F.(r + r)
    in
    two_to_the F.size_in_bits
end

let of_field (type f) (module F : Field_intf with type t = f)
    ~(shift : f Shift.t) (s : f) : f t =
  Shifted_value F.(s - (shift :> t))

let to_field (type f) (module F : Field_intf with type t = f)
    ~(shift : f Shift.t) (Shifted_value t : f t) : f =
  F.(t + (shift :> t))

let equal equal (Shifted_value t1) (Shifted_value t2) = equal t1 t2
