(* payment_payload.ml *)

[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick
open Signature_lib

[%%else]

module Currency = Currency_nonconsensus.Currency
open Signature_lib_nonconsensus

[%%endif]

module Amount = Currency.Amount
module Fee = Currency.Fee

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('public_key, 'token_id, 'amount) t =
        { source_pk: 'public_key
        ; receiver_pk: 'public_key
        ; token_id: 'token_id
        ; amount: 'amount }
      [@@deriving equal, sexp, hash, yojson, compare, hlist]
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Public_key.Compressed.Stable.V1.t
      , Token_id.Stable.V1.t
      , Amount.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving equal, sexp, hash, compare, yojson]

    let to_latest = Fn.id
  end
end]

let dummy =
  Poly.
    { source_pk= Public_key.Compressed.empty
    ; receiver_pk= Public_key.Compressed.empty
    ; token_id= Token_id.invalid
    ; amount= Amount.zero }

let token {Poly.token_id; _} = token_id

[%%ifdef
consensus_mechanism]

type var = (Public_key.Compressed.var, Token_id.var, Amount.var) Poly.t

let typ : (var, t) Typ.t =
  let spec =
    let open Data_spec in
    [ Public_key.Compressed.typ
    ; Public_key.Compressed.typ
    ; Token_id.typ
    ; Amount.typ ]
  in
  Typ.of_hlistable spec ~var_to_hlist:Poly.to_hlist ~var_of_hlist:Poly.of_hlist
    ~value_to_hlist:Poly.to_hlist ~value_of_hlist:Poly.of_hlist

let to_input {Poly.source_pk; receiver_pk; token_id; amount} =
  Array.reduce_exn ~f:Random_oracle.Input.append
    [| Public_key.Compressed.to_input source_pk
     ; Public_key.Compressed.to_input receiver_pk
     ; Token_id.to_input token_id
     ; Amount.to_input amount |]

let var_to_input {Poly.source_pk; receiver_pk; token_id; amount} =
  let%map token_id = Token_id.Checked.to_input token_id in
  Array.reduce_exn ~f:Random_oracle.Input.append
    [| Public_key.Compressed.Checked.to_input source_pk
     ; Public_key.Compressed.Checked.to_input receiver_pk
     ; token_id
     ; Amount.var_to_input amount |]

let var_of_t ({source_pk; receiver_pk; token_id; amount} : t) : var =
  { source_pk= Public_key.Compressed.var_of_t source_pk
  ; receiver_pk= Public_key.Compressed.var_of_t receiver_pk
  ; token_id= Token_id.var_of_t token_id
  ; amount= Amount.var_of_t amount }

[%%endif]

let gen_aux ?source_pk ~token_id ~max_amount =
  let open Quickcheck.Generator.Let_syntax in
  let%bind source_pk =
    match source_pk with
    | Some source_pk ->
        return source_pk
    | None ->
        Public_key.Compressed.gen
  in
  let%bind receiver_pk = Public_key.Compressed.gen in
  let%map amount = Amount.gen_incl Amount.zero max_amount in
  Poly.{source_pk; receiver_pk; token_id; amount}

let gen ?source_pk ~max_amount =
  let open Quickcheck.Generator.Let_syntax in
  let%bind token_id = Token_id.gen in
  gen_aux ?source_pk ~token_id ~max_amount

let gen_default_token ?source_pk ~max_amount =
  gen_aux ?source_pk ~token_id:Token_id.default ~max_amount

let gen_non_default_token ?source_pk ~max_amount =
  let open Quickcheck.Generator.Let_syntax in
  let%bind token_id = Token_id.gen_non_default in
  gen_aux ?source_pk ~token_id ~max_amount
