open Core
open Import

module Stable : sig
  module V1 : sig
    type t = private
      { receiver: Public_key.Compressed.Stable.V1.t
      ; amount: Currency.Amount.Stable.V1.t
      ; fee_transfer: Fee_transfer.Single.Stable.V1.t option }
    [@@deriving sexp, bin_io, compare, eq, version, hash, yojson]
  end

  module Latest = V1
end

(* bin_io intentionally omitted in deriving list *)
type t = Stable.Latest.t = private
  { receiver: Public_key.Compressed.Stable.V1.t
  ; amount: Currency.Amount.Stable.V1.t
  ; fee_transfer: Fee_transfer.Single.Stable.V1.t option }
[@@deriving sexp, compare, eq, hash, yojson]

include Codable.Base58_check_intf with type t := t

val proposer : t -> Public_key.Compressed.t

val amount : t -> Currency.Amount.t

val fee_transfer : t -> Fee_transfer.Single.t option

val state_body_hash : t -> State_body_hash.t

val create :
     amount:Currency.Amount.t
  -> receiver:Public_key.Compressed.t
  -> fee_transfer:Fee_transfer.Single.Stable.V1.t option
  -> t Or_error.t

val supply_increase : t -> Currency.Amount.t Or_error.t

val fee_excess : t -> Currency.Fee.Signed.t Or_error.t

val gen : t Quickcheck.Generator.t
