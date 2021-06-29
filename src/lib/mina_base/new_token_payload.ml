open Core_kernel
open Import

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { token_owner_pk: Public_key.Compressed.Stable.V1.t
      ; disable_new_accounts: bool }
    [@@deriving compare, equal, sexp, hash, yojson]

    let to_latest = Fn.id
  end
end]

let receiver_pk {token_owner_pk; _} = token_owner_pk

let receiver ~next_available_token {token_owner_pk; _} =
  Account_id.create token_owner_pk next_available_token

let source_pk {token_owner_pk; _} = token_owner_pk

let source ~next_available_token {token_owner_pk; _} =
  Account_id.create token_owner_pk next_available_token

let token (_ : t) = Token_id.invalid

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind token_owner_pk = Public_key.Compressed.gen in
  let%map disable_new_accounts = Quickcheck.Generator.bool in
  {token_owner_pk; disable_new_accounts}
