open Core_kernel

[%%versioned
module Stable = struct
  module V1 = struct
    type t = Pos | Neg [@@deriving sexp, hash, compare, equal, yojson]

    let to_latest = Fn.id
  end
end]
