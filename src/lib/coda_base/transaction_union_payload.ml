(* transaction_union_payload.ml *)

[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick
open Signature_lib
open Currency

[%%else]

open Signature_lib_nonconsensus
module Currency = Currency_nonconsensus.Currency
open Currency_nonconsensus.Currency
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module Tag = Transaction_union_tag

module Body = struct
  type ('tag, 'public_key, 'token_id, 'amount, 'bool) t_ =
    { tag: 'tag
    ; source_pk: 'public_key
    ; receiver_pk: 'public_key
    ; token_id: 'token_id
    ; amount: 'amount
    ; token_locked: 'bool }
  [@@deriving sexp]

  type t =
    (Tag.t, Public_key.Compressed.t, Token_id.t, Currency.Amount.t, bool) t_
  [@@deriving sexp]

  let of_user_command_payload_body = function
    | User_command_payload.Body.Payment
        {source_pk; receiver_pk; token_id; amount} ->
        { tag= Tag.Payment
        ; source_pk
        ; receiver_pk
        ; token_id
        ; amount
        ; token_locked= false }
    | Stake_delegation (Set_delegate {delegator; new_delegate}) ->
        { tag= Tag.Stake_delegation
        ; source_pk= delegator
        ; receiver_pk= new_delegate
        ; token_id= Token_id.default
        ; amount= Currency.Amount.zero
        ; token_locked= false }
    | Create_new_token {token_owner_pk; disable_new_accounts} ->
        { tag= Tag.Create_account
        ; source_pk= token_owner_pk
        ; receiver_pk= token_owner_pk
        ; token_id= Token_id.invalid
        ; amount= Currency.Amount.zero
        ; token_locked= disable_new_accounts }
    | Create_token_account
        {token_id; token_owner_pk; receiver_pk; account_disabled} ->
        { tag= Tag.Create_account
        ; source_pk= token_owner_pk
        ; receiver_pk
        ; token_id
        ; amount= Currency.Amount.zero
        ; token_locked= account_disabled }
    | Mint_tokens {token_id; token_owner_pk; receiver_pk; amount} ->
        { tag= Tag.Mint_tokens
        ; source_pk= token_owner_pk
        ; receiver_pk
        ; token_id
        ; amount
        ; token_locked= false }
    | Set_token_permissions {token_id; token_owner_pk; disable_new_accounts} ->
        { tag= Tag.Set_token_permissions
        ; source_pk= token_owner_pk
        ; receiver_pk= token_owner_pk
        ; token_id
        ; amount= Currency.Amount.zero
        ; token_locked= disable_new_accounts }
    | Set_account_permissions {token_id; token_owner_pk; target_pk; disabled}
      ->
        { tag= Tag.Set_token_permissions
        ; source_pk= token_owner_pk
        ; receiver_pk= target_pk
        ; token_id
        ; amount= Currency.Amount.zero
        ; token_locked= disabled }

  let gen ~fee =
    let open Quickcheck.Generator.Let_syntax in
    let%bind tag = Tag.gen in
    let%map amount =
      let min, max =
        let max_amount_without_overflow =
          Amount.(sub max_int (of_fee fee))
          |> Option.value_exn ?here:None ?message:None ?error:None
        in
        match tag with
        | Payment ->
            (Amount.zero, max_amount_without_overflow)
        | Stake_delegation ->
            (Amount.zero, Amount.zero)
        | Create_account ->
            (Amount.zero, Amount.zero)
        | Fee_transfer ->
            (Amount.zero, max_amount_without_overflow)
        | Coinbase ->
            (* In this case,
             amount - fee should be defined. In other words,
             amount >= fee *)
            (Amount.of_fee fee, Amount.max_int)
        | Mint_tokens ->
            (Amount.zero, Amount.max_int)
        | Set_token_permissions ->
            (Amount.zero, Amount.zero)
      in
      Amount.gen_incl min max
    and token_locked =
      match tag with
      | Payment ->
          return false
      | Stake_delegation ->
          return false
      | Create_account ->
          Quickcheck.Generator.bool
      | Fee_transfer ->
          return false
      | Coinbase ->
          return false
      | Mint_tokens ->
          return false
      | Set_token_permissions ->
          Quickcheck.Generator.bool
    and source_pk = Public_key.Compressed.gen
    and receiver_pk = Public_key.Compressed.gen
    and token_id =
      match tag with
      | Payment ->
          Token_id.gen
      | Stake_delegation ->
          return Token_id.default
      | Create_account ->
          Token_id.gen_with_invalid
      | Mint_tokens ->
          Token_id.gen_non_default
      | Fee_transfer ->
          return Token_id.default
      | Coinbase ->
          return Token_id.default
      | Set_token_permissions ->
          Token_id.gen_non_default
    in
    {tag; source_pk; receiver_pk; token_id; amount; token_locked}

  [%%ifdef
  consensus_mechanism]

  type var =
    ( Tag.Unpacked.var
    , Public_key.Compressed.var
    , Token_id.var
    , Currency.Amount.var
    , Boolean.var )
    t_

  let to_hlist {tag; source_pk; receiver_pk; token_id; amount; token_locked} =
    H_list.[tag; source_pk; receiver_pk; token_id; amount; token_locked]

  let spec =
    Data_spec.
      [ Tag.unpacked_typ
      ; Public_key.Compressed.typ
      ; Public_key.Compressed.typ
      ; Token_id.typ
      ; Currency.Amount.typ
      ; Boolean.typ ]

  let typ =
    Typ.of_hlistable spec ~var_to_hlist:to_hlist ~value_to_hlist:to_hlist
      ~var_of_hlist:
        (fun H_list.
               [tag; source_pk; receiver_pk; token_id; amount; token_locked] ->
        {tag; source_pk; receiver_pk; token_id; amount; token_locked} )
      ~value_of_hlist:
        (fun H_list.
               [tag; source_pk; receiver_pk; token_id; amount; token_locked] ->
        {tag; source_pk; receiver_pk; token_id; amount; token_locked} )

  module Checked = struct
    let constant
        ({tag; source_pk; receiver_pk; token_id; amount; token_locked} : t) :
        var =
      { tag= Tag.unpacked_of_t tag
      ; source_pk= Public_key.Compressed.var_of_t source_pk
      ; receiver_pk= Public_key.Compressed.var_of_t receiver_pk
      ; token_id= Token_id.var_of_t token_id
      ; amount= Currency.Amount.var_of_t amount
      ; token_locked= Boolean.var_of_value token_locked }

    let to_input {tag; source_pk; receiver_pk; token_id; amount; token_locked}
        =
      let%map token_id = Token_id.Checked.to_input token_id in
      Array.reduce_exn ~f:Random_oracle.Input.append
        [| Tag.Unpacked.to_input tag
         ; Public_key.Compressed.Checked.to_input source_pk
         ; Public_key.Compressed.Checked.to_input receiver_pk
         ; token_id
         ; Currency.Amount.var_to_input amount
         ; Random_oracle.Input.bitstring [token_locked] |]
  end

  [%%endif]

  let to_input {tag; source_pk; receiver_pk; token_id; amount; token_locked} =
    Array.reduce_exn ~f:Random_oracle.Input.append
      [| Tag.to_input tag
       ; Public_key.Compressed.to_input source_pk
       ; Public_key.Compressed.to_input receiver_pk
       ; Token_id.to_input token_id
       ; Currency.Amount.to_input amount
       ; Random_oracle.Input.bitstring [token_locked] |]
end

type t = (User_command_payload.Common.t, Body.t) User_command_payload.Poly.t
[@@deriving sexp]

type payload = t [@@deriving sexp]

let of_user_command_payload ({common; body} : User_command_payload.t) : t =
  {common; body= Body.of_user_command_payload_body body}

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind common =
    User_command_payload.Common.gen ~fee_token_id:Token_id.default ()
  in
  let%map body = Body.gen ~fee:common.fee in
  User_command_payload.Poly.{common; body}

[%%ifdef
consensus_mechanism]

type var =
  (User_command_payload.Common.var, Body.var) User_command_payload.Poly.t

type payload_var = var

let to_hlist ({common; body} : (_, _) User_command_payload.Poly.t) =
  H_list.[common; body]

let of_hlist : type c v.
    (unit, c -> v -> unit) H_list.t -> (c, v) User_command_payload.Poly.t =
 fun H_list.[common; body] -> {common; body}

let typ : (var, t) Typ.t =
  Typ.of_hlistable
    [User_command_payload.Common.typ; Body.typ]
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
    ~value_of_hlist:of_hlist

let payload_typ = typ

module Checked = struct
  let to_input ({common; body} : var) =
    let%map common = User_command_payload.Common.Checked.to_input common
    and body = Body.Checked.to_input body in
    Random_oracle.Input.append common body

  let constant ({common; body} : t) : var =
    { common= User_command_payload.Common.Checked.constant common
    ; body= Body.Checked.constant body }
end

[%%endif]

let to_input ({common; body} : t) =
  Random_oracle.Input.append
    (User_command_payload.Common.to_input common)
    (Body.to_input body)

let excess (payload : t) : Amount.Signed.t =
  let tag = payload.body.tag in
  let fee = payload.common.fee in
  let amount = payload.body.amount in
  match tag with
  | Payment
  | Stake_delegation
  | Create_account
  | Mint_tokens
  | Set_token_permissions ->
      (* For all user commands, the fee excess is just the fee. *)
      Amount.Signed.of_unsigned (Amount.of_fee fee)
  | Fee_transfer ->
      Option.value_exn (Amount.add_fee amount fee)
      |> Amount.Signed.of_unsigned |> Amount.Signed.negate
  | Coinbase ->
      Amount.Signed.zero

let fee_excess ({body= {tag; amount; _}; common= {fee_token; fee; _}} : t) =
  match tag with
  | Payment
  | Stake_delegation
  | Create_account
  | Mint_tokens
  | Set_token_permissions ->
      (* For all user commands, the fee excess is just the fee. *)
      Fee_excess.of_single (fee_token, Fee.Signed.of_unsigned fee)
  | Fee_transfer ->
      let excess =
        Option.value_exn (Amount.add_fee amount fee)
        |> Amount.to_fee |> Fee.Signed.of_unsigned |> Fee.Signed.negate
      in
      Fee_excess.of_single (fee_token, excess)
  | Coinbase ->
      Fee_excess.of_single (Token_id.default, Fee.Signed.zero)

let supply_increase (payload : payload) =
  let tag = payload.body.tag in
  match tag with
  | Coinbase ->
      payload.body.amount
  | Payment
  | Stake_delegation
  | Create_account
  | Mint_tokens
  | Set_token_permissions
  | Fee_transfer ->
      Amount.zero

let next_available_token (payload : payload) tid =
  match payload.body.tag with
  | Payment
  | Stake_delegation
  | Mint_tokens
  | Set_token_permissions
  | Coinbase
  | Fee_transfer ->
      tid
  | Create_account when Token_id.(equal invalid) payload.body.token_id ->
      (* Creating a new token. *)
      Token_id.next tid
  | Create_account ->
      (* Creating an account for an existing token. *)
      tid
