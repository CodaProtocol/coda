open Core
open Import
module Fee_transfer = Coinbase_fee_transfer

module Stable : sig
  module V1 : sig
    type t = private
      { receiver: Public_key.Compressed.Stable.V1.t
      ; amount: Currency.Amount.Stable.V1.t
      ; fee_transfer: Fee_transfer.Stable.V1.t option }
    [@@deriving sexp, bin_io, compare, equal, version, hash, yojson]
  end

  module Latest = V1
end

(* bin_io intentionally omitted in deriving list *)
type t = Stable.Latest.t = private
  { receiver: Public_key.Compressed.t
  ; amount: Currency.Amount.t
  ; fee_transfer: Fee_transfer.t option }
[@@deriving sexp, compare, equal, hash, yojson]

include Codable.Base58_check_intf with type t := t

val receiver_pk : t -> Public_key.Compressed.t

val receiver : t -> Account_id.t

val fee_payer_pk : t -> Public_key.Compressed.t

val amount : t -> Currency.Amount.t

val fee_transfer : t -> Fee_transfer.t option

val accounts_accessed : t -> Account_id.t list

val create :
     amount:Currency.Amount.t
  -> receiver:Public_key.Compressed.t
  -> fee_transfer:Fee_transfer.t option
  -> t Or_error.t

val supply_increase : t -> Currency.Amount.t Or_error.t

val fee_excess : t -> Fee_excess.t Or_error.t

module Gen : sig
  val gen :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> (t * [`Supercharged_coinbase of bool]) Quickcheck.Generator.t

  (** Creates coinbase with reward between [min_amount] and [max_amount]. The generated amount[coinbase_amount] is then used as the upper bound for the fee transfer. *)
  val with_random_receivers :
       keys:Signature_keypair.t array
    -> min_amount:int
    -> max_amount:int
    -> fee_transfer:(   coinbase_amount:Currency.Amount.t
                     -> Fee_transfer.t Quickcheck.Generator.t)
    -> t Quickcheck.Generator.t
end
