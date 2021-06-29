(* stake_delegation.ml *)

[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Signature_lib

[%%else]

open Signature_lib_nonconsensus
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      | Set_delegate of
          { delegator: Public_key.Compressed.Stable.V1.t
          ; new_delegate: Public_key.Compressed.Stable.V1.t }
    [@@deriving compare, equal, sexp, hash, yojson]

    let to_latest = Fn.id
  end
end]

let receiver_pk = function Set_delegate {new_delegate; _} -> new_delegate

let receiver = function
  | Set_delegate {new_delegate; _} ->
      Account_id.create new_delegate Token_id.default

let source_pk = function Set_delegate {delegator; _} -> delegator

let source = function
  | Set_delegate {delegator; _} ->
      Account_id.create delegator Token_id.default

let gen_with_delegator delegator =
  Quickcheck.Generator.map Public_key.Compressed.gen ~f:(fun k ->
      Set_delegate {delegator; new_delegate= k} )

let gen =
  Quickcheck.Generator.bind ~f:gen_with_delegator Public_key.Compressed.gen

let to_input = function
  | Set_delegate {delegator; new_delegate} ->
      Random_oracle.Input.append
        (Public_key.Compressed.to_input delegator)
        (Public_key.Compressed.to_input new_delegate)
