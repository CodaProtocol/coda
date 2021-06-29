open Core_kernel
open Import

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { token_id: Token_id.Stable.V1.t
      ; token_owner_pk: Public_key.Compressed.Stable.V1.t
      ; receiver_pk: Public_key.Compressed.Stable.V1.t
      ; amount: Currency.Amount.Stable.V1.t }
    [@@deriving compare, equal, sexp, hash, yojson]

    let to_latest = Fn.id
  end
end]

let receiver_pk {receiver_pk; _} = receiver_pk

let receiver {token_id; receiver_pk; _} =
  Account_id.create receiver_pk token_id

let source_pk {token_owner_pk; _} = token_owner_pk

let source {token_id; token_owner_pk; _} =
  Account_id.create token_owner_pk token_id

let token {token_id; _} = token_id

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind token_id = Token_id.gen_non_default in
  let%bind token_owner_pk = Public_key.Compressed.gen in
  let%bind receiver_pk = Public_key.Compressed.gen in
  let%map amount = Currency.Amount.gen in
  {token_id; token_owner_pk; receiver_pk; amount}
