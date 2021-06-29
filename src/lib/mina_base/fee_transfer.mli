open Core_kernel
open Import

module Single : sig
  module Stable : sig
    module V1 : sig
      type t = private
        { receiver_pk: Public_key.Compressed.Stable.V1.t
        ; fee: Currency.Fee.Stable.V1.t
        ; fee_token: Token_id.Stable.V1.t }
      [@@deriving bin_io, sexp, compare, equal, yojson, version, hash]
    end

    module Latest = V1
  end

  type t = Stable.Latest.t = private
    { receiver_pk: Public_key.Compressed.t
    ; fee: Currency.Fee.t
    ; fee_token: Token_id.t }
  [@@deriving sexp, compare, yojson, hash]

  include Comparable.S with type t := t

  include Codable.Base58_check_intf with type t := t

  val create :
       receiver_pk:Public_key.Compressed.t
    -> fee:Currency.Fee.t
    -> fee_token:Token_id.t
    -> t

  val receiver_pk : t -> Public_key.Compressed.t

  val receiver : t -> Account_id.t

  val fee : t -> Currency.Fee.t

  val fee_token : t -> Token_id.t

  module Gen : sig
    val with_random_receivers :
         ?min_fee:int
      -> keys:Signature_keypair.t array
      -> max_fee:int
      -> token:Token_id.t Quickcheck.Generator.t
      -> t Quickcheck.Generator.t
  end
end

module Stable : sig
  module V1 : sig
    type t = private Single.Stable.V1.t One_or_two.Stable.V1.t
    [@@deriving bin_io, sexp, compare, equal, yojson, version, hash]
  end

  module Latest = V1
end

type t = Stable.Latest.t [@@deriving sexp, compare, yojson, hash]

type single = Single.t = private
  { receiver_pk: Public_key.Compressed.t
  ; fee: Currency.Fee.t
  ; fee_token: Token_id.t }
[@@deriving sexp, compare, yojson, hash]

include Comparable.S with type t := t

val create : Single.t -> Single.t option -> t Or_error.t

val create_single :
     receiver_pk:Public_key.Compressed.t
  -> fee:Currency.Fee.t
  -> fee_token:Token_id.t
  -> t

val to_singles : t -> Single.t One_or_two.t

val of_singles : Single.t One_or_two.t -> t Or_error.t

val fee_excess : t -> Fee_excess.t Or_error.t

val fee_token : single -> Token_id.t

val fee_tokens : t -> Token_id.t One_or_two.t

val receiver_pks : t -> Public_key.Compressed.t list

val receivers : t -> Account_id.t list

val fee_payer_pk : t -> Public_key.Compressed.t

val map : t -> f:(Single.t -> 'b) -> 'b One_or_two.t

val fold : t -> init:'acc -> f:('acc -> Single.t -> 'acc) -> 'acc

val to_list : t -> Single.t list

val to_numbered_list : t -> (int * Single.t) list
