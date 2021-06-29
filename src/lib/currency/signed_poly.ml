open Core_kernel

[%%versioned
module Stable = struct
  module V1 = struct
    type ('magnitude, 'sgn) t = {magnitude: 'magnitude; sgn: 'sgn}
    [@@deriving sexp, hash, compare, equal, yojson]
  end
end]

let map ~f {magnitude; sgn} = {magnitude= f magnitude; sgn}
