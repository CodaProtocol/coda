open Core_kernel

[%%versioned
module Stable = struct
  module V1 = struct
    type ('field, 'scalar) t = 'field * 'scalar
    [@@deriving sexp, compare, equal, hash]
  end
end]
