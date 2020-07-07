open Core_kernel
open Import

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { token_id: Token_id.Stable.V1.t
      ; token_owner_pk: Public_key.Compressed.Stable.V1.t
      ; target_pk: Public_key.Compressed.Stable.V1.t
      ; disabled: bool }
    [@@deriving compare, eq, sexp, hash, yojson]

    let to_latest = Fn.id
  end
end]

type t = Stable.Latest.t =
  { token_id: Token_id.t
  ; token_owner_pk: Public_key.Compressed.t
  ; target_pk: Public_key.Compressed.t
  ; disabled: bool }
[@@deriving compare, eq, sexp, hash, yojson]

let receiver_pk {target_pk; _} = target_pk

let receiver {token_id; target_pk; _} = Account_id.create target_pk token_id

let source_pk {token_owner_pk; _} = token_owner_pk

let source {token_id; token_owner_pk; _} =
  Account_id.create token_owner_pk token_id

let token {token_id; _} = token_id

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind token_id = Token_id.gen_non_default in
  let%bind token_owner_pk = Public_key.Compressed.gen in
  let%bind target_pk = Public_key.Compressed.gen in
  let%map disabled = Quickcheck.Generator.bool in
  {token_id; token_owner_pk; target_pk; disabled}
