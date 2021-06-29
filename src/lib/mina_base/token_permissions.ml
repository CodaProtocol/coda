[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifndef
consensus_mechanism]

open Import

[%%endif]

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      | Token_owned of {disable_new_accounts: bool}
      | Not_owned of {account_disabled: bool}
    [@@deriving compare, equal, sexp, hash, yojson]

    let to_latest = Fn.id
  end
end]

let default = Not_owned {account_disabled= false}

let to_input = function
  | Token_owned {disable_new_accounts} ->
      Random_oracle.Input.bitstring [true; disable_new_accounts]
  | Not_owned {account_disabled} ->
      Random_oracle.Input.bitstring [false; account_disabled]

[%%ifdef
consensus_mechanism]

open Snark_params.Tick

type var = {token_owner: Boolean.var; token_locked: Boolean.var}

let var_of_t = function
  | Token_owned {disable_new_accounts} ->
      { token_owner= Boolean.true_
      ; token_locked= Boolean.var_of_value disable_new_accounts }
  | Not_owned {account_disabled} ->
      { token_owner= Boolean.false_
      ; token_locked= Boolean.var_of_value account_disabled }

let typ : (var, t) Typ.t =
  let open Typ in
  { alloc=
      Alloc.(
        let%bind token_owner = Boolean.typ.alloc in
        let%map token_locked = Boolean.typ.alloc in
        {token_owner; token_locked})
  ; read=
      Read.(
        fun t ->
          let%bind token_owner = Boolean.typ.read t.token_owner in
          let%map token_locked = Boolean.typ.read t.token_locked in
          if token_owner then Token_owned {disable_new_accounts= token_locked}
          else Not_owned {account_disabled= token_locked})
  ; store=
      Store.(
        function
        | Token_owned {disable_new_accounts} ->
            let%bind token_owner = Boolean.typ.store true in
            let%map token_locked = Boolean.typ.store disable_new_accounts in
            {token_owner; token_locked}
        | Not_owned {account_disabled} ->
            let%bind token_owner = Boolean.typ.store false in
            let%map token_locked = Boolean.typ.store account_disabled in
            {token_owner; token_locked})
  ; check=
      Checked.(
        fun {token_owner; token_locked} ->
          all_unit
            [Boolean.typ.check token_owner; Boolean.typ.check token_locked]) }

let var_to_input {token_owner; token_locked} =
  Random_oracle.Input.bitstring [token_owner; token_locked]

[%%endif]

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind token_owner = Quickcheck.Generator.bool in
  let%map token_locked = Quickcheck.Generator.bool in
  if token_owner then Token_owned {disable_new_accounts= token_locked}
  else Not_owned {account_disabled= token_locked}
