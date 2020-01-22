open Core_kernel

[%%versioned:
module Stable : sig
  module V1 : sig
    type t =
      { ledger: Coda_base.Sparse_ledger.Stable.V1.t
      ; protocol_state_body: Coda_state.Protocol_state.Body.Value.Stable.V1.t
      }
    [@@deriving sexp]
  end
end]

type t = Stable.Latest.t =
  { ledger: Coda_base.Sparse_ledger.Stable.V1.t
  ; protocol_state_body: Coda_state.Protocol_state.Body.Value.Stable.V1.t }
[@@deriving sexp]
