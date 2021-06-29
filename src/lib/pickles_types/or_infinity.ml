open Core_kernel

[%%versioned
module Stable = struct
  module V1 = struct
    type 'a t = 'a Marlin_plonk_bindings_types.Or_infinity.t =
      | Infinity
      | Finite of 'a
    [@@deriving sexp, equal, compare, hash, yojson]
  end
end]

let finite_exn = function Finite x -> x | Infinity -> failwith "finite_exn"
