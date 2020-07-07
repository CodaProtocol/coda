[%%import "/src/config.mlh"]

open Core_kernel
open Import

[%%ifdef consensus_mechanism]

open Snark_params.Tick

[%%else]

open Snark_params_nonconsensus
module Currency = Currency_nonconsensus.Currency
module Coda_numbers = Coda_numbers_nonconsensus.Coda_numbers
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module Body : sig
  type t =
    | Payment of Payment_payload.t
    | Stake_delegation of Stake_delegation.t
    | Create_new_token of New_token_payload.t
    | Create_token_account of New_account_payload.t
    | Mint_tokens of Minting_payload.t
    | Set_token_permissions of Token_permissions_payload.t
    | Set_account_permissions of Account_permissions_payload.t
  [@@deriving eq, sexp, hash, yojson]

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving compare, eq, sexp, hash, yojson]
    end
  end]

  val tag : t -> Transaction_union_tag.t

  val receiver_pk : t -> Signature_lib.Public_key.Compressed.t

  val receiver : next_available_token:Token_id.t -> t -> Account_id.t

  val source_pk : t -> Signature_lib.Public_key.Compressed.t

  val source : next_available_token:Token_id.t -> t -> Account_id.t

  val token : t -> Token_id.t
end

module Common : sig
  module Poly : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type ('fee, 'public_key, 'token_id, 'nonce, 'global_slot, 'memo) t =
          { fee: 'fee
          ; fee_token: 'token_id
          ; fee_payer_pk: 'public_key
          ; nonce: 'nonce
          ; valid_until: 'global_slot
          ; memo: 'memo }
        [@@deriving bin_io, eq, sexp, hash, yojson, version]
      end
    end]

    type ('fee, 'public_key, 'token_id, 'nonce, 'global_slot, 'memo) t =
          ( 'fee
          , 'public_key
          , 'token_id
          , 'nonce
          , 'global_slot
          , 'memo )
          Stable.Latest.t =
      { fee: 'fee
      ; fee_token: 'token_id
      ; fee_payer_pk: 'public_key
      ; nonce: 'nonce
      ; valid_until: 'global_slot
      ; memo: 'memo }
    [@@deriving eq, sexp, hash, yojson]
  end

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t =
        ( Currency.Fee.Stable.V1.t
        , Public_key.Compressed.Stable.V1.t
        , Token_id.Stable.V1.t
        , Coda_numbers.Account_nonce.Stable.V1.t
        , Coda_numbers.Global_slot.Stable.V1.t
        , User_command_memo.t )
        Poly.Stable.V1.t
      [@@deriving eq, sexp, hash]
    end
  end]

  type t = Stable.Latest.t [@@deriving compare, eq, sexp, hash]

  val to_input : t -> (Field.t, bool) Random_oracle.Input.t

  val gen : ?fee_token_id:Token_id.t -> unit -> t Quickcheck.Generator.t

  [%%ifdef consensus_mechanism]

  type var =
    ( Currency.Fee.var
    , Public_key.Compressed.var
    , Token_id.var
    , Coda_numbers.Account_nonce.Checked.t
    , Coda_numbers.Global_slot.Checked.t
    , User_command_memo.Checked.t )
    Poly.t

  val typ : (var, t) Typ.t

  module Checked : sig
    val to_input :
         var
      -> ( (Field.Var.t, Boolean.var) Random_oracle.Input.t
         , _ )
         Snark_params.Tick.Checked.t

    val constant : t -> var
  end

  [%%endif]
end

module Poly : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type ('common, 'body) t = {common: 'common; body: 'body}
      [@@deriving bin_io, eq, sexp, hash, yojson, compare, version]

      val of_latest :
           ('common1 -> ('common2, 'err) Result.t)
        -> ('body1 -> ('body2, 'err) Result.t)
        -> ('common1, 'body1) t
        -> (('common2, 'body2) t, 'err) Result.t
    end
  end]

  type ('common, 'body) t = ('common, 'body) Stable.Latest.t =
    {common: 'common; body: 'body}
  [@@deriving eq, sexp, hash, yojson, compare]
end

[%%versioned:
module Stable : sig
  module V1 : sig
    type t = (Common.Stable.V1.t, Body.Stable.V1.t) Poly.Stable.V1.t
    [@@deriving compare, eq, sexp, hash, yojson]
  end
end]

type t = Stable.Latest.t [@@deriving compare, eq, sexp, hash]

val create :
     fee:Currency.Fee.t
  -> fee_token:Token_id.t
  -> fee_payer_pk:Public_key.Compressed.t
  -> nonce:Coda_numbers.Account_nonce.t
  -> valid_until:Coda_numbers.Global_slot.t
  -> memo:User_command_memo.t
  -> body:Body.t
  -> t

val dummy : t

val fee : t -> Currency.Fee.t

val fee_token : t -> Token_id.t

val fee_payer_pk : t -> Public_key.Compressed.t

val fee_payer : t -> Account_id.t

val fee_excess : t -> Fee_excess.t

val nonce : t -> Coda_numbers.Account_nonce.t

val valid_until : t -> Coda_numbers.Global_slot.t

val memo : t -> User_command_memo.t

val body : t -> Body.t

val receiver_pk : t -> Public_key.Compressed.t

val receiver : next_available_token:Token_id.t -> t -> Account_id.t

val source_pk : t -> Public_key.Compressed.t

val source : next_available_token:Token_id.t -> t -> Account_id.t

val token : t -> Token_id.t

val amount : t -> Currency.Amount.t option

val accounts_accessed :
  next_available_token:Token_id.t -> t -> Account_id.t list

val next_available_token : t -> Token_id.t -> Token_id.t

val tag : t -> Transaction_union_tag.t

val gen : t Quickcheck.Generator.t
