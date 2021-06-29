open Core
open Snark_params.Tick
open Import

module Poly : sig
  type ('public_key, 'token_id, 'amount) t =
    { source_pk: 'public_key
    ; receiver_pk: 'public_key
    ; token_id: 'token_id
    ; amount: 'amount }
  [@@deriving equal, sexp, hash, yojson]

  module Stable :
    sig
      module V1 : sig
        type nonrec ('pk, 'tid, 'amount) t
        [@@deriving bin_io, equal, sexp, hash, yojson, version]
      end

      module Latest = V1
    end
    with type ('pk, 'tid, 'amount) V1.t = ('pk, 'tid, 'amount) t
end

[%%versioned:
module Stable : sig
  module V1 : sig
    type t =
      ( Public_key.Compressed.Stable.V1.t
      , Token_id.Stable.V1.t
      , Currency.Amount.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving compare, equal, sexp, hash, compare, yojson]
  end
end]

val dummy : t

val token : t -> Token_id.t

val gen :
     ?source_pk:Public_key.Compressed.t
  -> max_amount:Currency.Amount.t
  -> t Quickcheck.Generator.t

val gen_default_token :
     ?source_pk:Public_key.Compressed.t
  -> max_amount:Currency.Amount.t
  -> t Quickcheck.Generator.t

val gen_non_default_token :
     ?source_pk:Public_key.Compressed.t
  -> max_amount:Currency.Amount.t
  -> t Quickcheck.Generator.t

type var =
  (Public_key.Compressed.var, Token_id.var, Currency.Amount.var) Poly.t

val typ : (var, t) Typ.t

val to_input : t -> (Field.t, bool) Random_oracle.Input.t

val var_to_input :
  var -> ((Field.Var.t, Boolean.var) Random_oracle.Input.t, _) Checked.t

val var_of_t : t -> var
