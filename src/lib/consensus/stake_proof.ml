open Core
open Coda_base

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
        { delegator: Account.Index.Stable.V1.t
        ; ledger: Sparse_ledger.Stable.V1.t
        ; private_key: Signature_lib.Private_key.Stable.V1.t
        ; public_key: Signature_lib.Public_key.Stable.V1.t }
      [@@deriving bin_io, sexp, to_yojson, version]
    end

    include T
  end

  module Latest = V1
end

(* This is only the data that is neccessary for creating the
   blockchain SNARK which is not otherwise available. So in
   particular it excludes the epoch and slot this stake proof
   is for.
*)
type t = Stable.Latest.t =
  { delegator: Account.Index.t
  ; ledger: Sparse_ledger.Stable.V1.t
  ; private_key: Signature_lib.Private_key.t
  ; public_key: Signature_lib.Public_key.t }
[@@deriving to_yojson, sexp]
