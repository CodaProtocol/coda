open Core
open Coda_base

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { key: Receipt.Chain_hash.Stable.V1.t
      ; value: User_command_payload.Stable.V1.t
      ; parent: Receipt.Chain_hash.Stable.V1.t }
    [@@deriving sexp]

    let to_latest = Fn.id
  end
end]

type t = Stable.Latest.t =
  { key: Receipt.Chain_hash.t
  ; value: User_command_payload.t
  ; parent: Receipt.Chain_hash.t }
[@@deriving sexp]
