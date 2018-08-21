open Core

module Stable = struct
  module V1 = struct
    type t = Snark_params.Tock.Field.t * Snark_params.Tock.Field.t
    [@@deriving sexp, eq, compare, hash, bin_io]
  end
end

include Stable.V1
open Snark_params.Tick

type var = Boolean.var list * Boolean.var list

let typ : (var, t) Typ.t =
  Schnorr.Signature.typ
