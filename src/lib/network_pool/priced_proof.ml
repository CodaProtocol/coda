open Core_kernel
open Mina_base

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V1 = struct
    type 'proof t = { proof : 'proof; fee : Fee_with_prover.Stable.V1.t }
    [@@deriving compare, fields, sexp, yojson, hash]
  end
end]

type 'proof t = 'proof Stable.Latest.t =
  { proof : 'proof; fee : Fee_with_prover.t }
[@@deriving compare, fields, sexp, yojson, hash]

let map t ~f = { t with proof = f t.proof }
