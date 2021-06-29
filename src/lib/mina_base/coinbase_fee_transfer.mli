open Core_kernel
open Import

[%%versioned:
module Stable : sig
  module V1 : sig
    type t = private
      { receiver_pk: Public_key.Compressed.Stable.V1.t
      ; fee: Currency.Fee.Stable.V1.t }
    [@@deriving sexp, compare, equal, yojson, hash]
  end
end]

val create : receiver_pk:Public_key.Compressed.t -> fee:Currency.Fee.t -> t

include Comparable.S with type t := t

include Codable.Base58_check_intf with type t := t

val receiver_pk : t -> Public_key.Compressed.t

val receiver : t -> Account_id.t

val fee : t -> Currency.Fee.t

val to_fee_transfer : t -> Fee_transfer.Single.t

module Gen : sig
  val gen :
       ?min_fee:Currency.Fee.t
    -> max_fee:Currency.Fee.t
    -> t Quickcheck.Generator.t

  (** Creates coinbase fee transfers with fees between [min_fee] and [coinbase_amount]*)
  val with_random_receivers :
       keys:Signature_keypair.t array
    -> ?min_fee:Currency.Fee.t
    -> coinbase_amount:Currency.Amount.t
    -> t Quickcheck.Generator.t
end
