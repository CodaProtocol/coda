open Core_kernel

[%%versioned
module Stable = struct
  module V1 = struct
    type 'a t = {a: 'a; b: 'a; c: 'a}
    [@@deriving sexp, equal, compare, hash, yojson, hlist, fields]
  end
end]

let typ (type a b f) (g : (a, b, f) Snarky_backendless.Typ.t) :
    (a t, b t, f) Snarky_backendless.Typ.t =
  Snarky_backendless.Typ.of_hlistable [g; g; g] ~var_to_hlist:to_hlist
    ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

let map {a; b; c} ~f = {a= f a; b= f b; c= f c}

let map2 t1 t2 ~f = {a= f t1.a t2.a; b= f t1.b t2.b; c= f t1.c t2.c}

module Label = struct
  type t = A | B | C [@@deriving equal]

  let all = [A; B; C]
end

let abc a b c = function Label.A -> a | B -> b | C -> c
