(* user_command_payload.ml *)

[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick
open Signature_lib
module Mina_numbers = Mina_numbers

[%%else]

open Signature_lib_nonconsensus
module Mina_numbers = Mina_numbers_nonconsensus.Mina_numbers
module Currency = Currency_nonconsensus.Currency
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module Memo = Signed_command_memo
module Account_nonce = Mina_numbers.Account_nonce
module Global_slot = Mina_numbers.Global_slot

module Common = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('fee, 'public_key, 'token_id, 'nonce, 'global_slot, 'memo) t =
          { fee: 'fee
          ; fee_token: 'token_id
          ; fee_payer_pk: 'public_key
          ; nonce: 'nonce
          ; valid_until: 'global_slot
          ; memo: 'memo }
        [@@deriving compare, equal, sexp, hash, yojson, hlist]
      end
    end]
  end

  [%%if
  feature_tokens]

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Currency.Fee.Stable.V1.t
        , Public_key.Compressed.Stable.V1.t
        , Token_id.Stable.V1.t
        , Account_nonce.Stable.V1.t
        , Global_slot.Stable.V1.t
        , Memo.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving compare, equal, sexp, hash, yojson]

      let to_latest = Fn.id
    end
  end]

  [%%else]

  module Binable_arg = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          ( Currency.Fee.Stable.V1.t
          , Public_key.Compressed.Stable.V1.t
          , Token_id.Stable.V1.t
          , Account_nonce.Stable.V1.t
          , Global_slot.Stable.V1.t
          , Memo.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving compare, equal, sexp, hash, yojson]

        let to_latest = Fn.id
      end
    end]
  end

  let check (t : Binable_arg.t) =
    if Token_id.equal t.fee_token Token_id.default then t
    else failwithf !"Tokens disabled. Read %{sexp:Binable_arg.t}" t ()

  [%%versioned_binable
  module Stable = struct
    module V1 = struct
      type t = Binable_arg.Stable.V1.t
      [@@deriving compare, equal, sexp, hash, yojson]

      include Binable.Of_binable
                (Binable_arg.Stable.V1)
                (struct
                  type nonrec t = t

                  let of_binable = check

                  let to_binable = check
                end)

      let to_latest = Fn.id
    end
  end]

  [%%endif]

  let to_input ({fee; fee_token; fee_payer_pk; nonce; valid_until; memo} : t) =
    let bitstring = Random_oracle.Input.bitstring in
    Array.reduce_exn ~f:Random_oracle.Input.append
      [| Currency.Fee.to_input fee
       ; Token_id.to_input fee_token
       ; Public_key.Compressed.to_input fee_payer_pk
       ; Account_nonce.to_input nonce
       ; Global_slot.to_input valid_until
       ; bitstring (Memo.to_bits memo) |]

  let gen ?fee_token_id () : t Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    let%map fee = Currency.Fee.gen
    and fee_token =
      match fee_token_id with
      | Some fee_token_id ->
          return fee_token_id
      | None ->
          Token_id.gen
    and fee_payer_pk = Public_key.Compressed.gen
    and nonce = Account_nonce.gen
    and valid_until = Global_slot.gen
    and memo =
      let%bind is_digest = Bool.quickcheck_generator in
      if is_digest then
        String.gen_with_length Memo.max_digestible_string_length
          Char.quickcheck_generator
        >>| Memo.create_by_digesting_string_exn
      else
        String.gen_with_length Memo.max_input_length Char.quickcheck_generator
        >>| Memo.create_from_string_exn
    in
    Poly.{fee; fee_token; fee_payer_pk; nonce; valid_until; memo}

  [%%ifdef
  consensus_mechanism]

  type var =
    ( Currency.Fee.var
    , Public_key.Compressed.var
    , Token_id.var
    , Account_nonce.Checked.t
    , Global_slot.Checked.t
    , Memo.Checked.t )
    Poly.t

  let typ =
    Typ.of_hlistable
      [ Currency.Fee.typ
      ; Token_id.typ
      ; Public_key.Compressed.typ
      ; Account_nonce.typ
      ; Global_slot.typ
      ; Memo.typ ]
      ~var_to_hlist:Poly.to_hlist ~var_of_hlist:Poly.of_hlist
      ~value_to_hlist:Poly.to_hlist ~value_of_hlist:Poly.of_hlist

  module Checked = struct
    let constant ({fee; fee_token; fee_payer_pk; nonce; valid_until; memo} : t)
        : var =
      { fee= Currency.Fee.var_of_t fee
      ; fee_token= Token_id.var_of_t fee_token
      ; fee_payer_pk= Public_key.Compressed.var_of_t fee_payer_pk
      ; nonce= Account_nonce.Checked.constant nonce
      ; memo= Memo.Checked.constant memo
      ; valid_until= Global_slot.Checked.constant valid_until }

    let to_input
        ({fee; fee_token; fee_payer_pk; nonce; valid_until; memo} : var) =
      let%map nonce = Account_nonce.Checked.to_input nonce
      and valid_until = Global_slot.Checked.to_input valid_until
      and fee_token = Token_id.Checked.to_input fee_token in
      Array.reduce_exn ~f:Random_oracle.Input.append
        [| Currency.Fee.var_to_input fee
         ; fee_token
         ; Public_key.Compressed.Checked.to_input fee_payer_pk
         ; nonce
         ; valid_until
         ; Random_oracle.Input.bitstring
             (Array.to_list (memo :> Boolean.var array)) |]
  end

  [%%endif]
end

module Body = struct
  module Binable_arg = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          | Payment of Payment_payload.Stable.V1.t
          | Stake_delegation of Stake_delegation.Stable.V1.t
          | Create_new_token of New_token_payload.Stable.V1.t
          | Create_token_account of New_account_payload.Stable.V1.t
          | Mint_tokens of Minting_payload.Stable.V1.t
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
  end

  [%%if
  feature_tokens]

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Binable_arg.Stable.V1.t =
        | Payment of Payment_payload.Stable.V1.t
        | Stake_delegation of Stake_delegation.Stable.V1.t
        | Create_new_token of New_token_payload.Stable.V1.t
        | Create_token_account of New_account_payload.Stable.V1.t
        | Mint_tokens of Minting_payload.Stable.V1.t
      [@@deriving compare, equal, sexp, hash, yojson]

      let to_latest = Fn.id
    end
  end]

  [%%else]

  let check (t : Binable_arg.t) =
    let fail () =
      failwithf !"Tokens disabled. Read %{sexp:Binable_arg.t}" t ()
    in
    match t with
    | Payment p ->
        if Token_id.equal p.token_id Token_id.default then t else fail ()
    | Stake_delegation _ ->
        t
    | Create_new_token _ | Create_token_account _ | Mint_tokens _ ->
        fail ()

  [%%versioned_binable
  module Stable = struct
    module V1 = struct
      type t = Binable_arg.Stable.V1.t =
        | Payment of Payment_payload.Stable.V1.t
        | Stake_delegation of Stake_delegation.Stable.V1.t
        | Create_new_token of New_token_payload.Stable.V1.t
        | Create_token_account of New_account_payload.Stable.V1.t
        | Mint_tokens of Minting_payload.Stable.V1.t
      [@@deriving compare, equal, sexp, hash, yojson]

      include Binable.Of_binable
                (Binable_arg.Stable.V1)
                (struct
                  type nonrec t = t

                  let of_binable = check

                  let to_binable = check
                end)

      let to_latest = Fn.id
    end
  end]

  [%%endif]

  module Tag = Transaction_union_tag

  let gen ?source_pk ~max_amount =
    let open Quickcheck.Generator in
    let stake_delegation_gen =
      match source_pk with
      | Some source_pk ->
          Stake_delegation.gen_with_delegator source_pk
      | None ->
          Stake_delegation.gen
    in
    let new_token_gen =
      match source_pk with
      | Some token_owner_pk ->
          map New_token_payload.gen ~f:(fun payload ->
              {payload with token_owner_pk} )
      | None ->
          New_token_payload.gen
    in
    let token_account_gen =
      match source_pk with
      | Some token_owner_pk ->
          map New_account_payload.gen ~f:(fun payload ->
              {payload with token_owner_pk} )
      | None ->
          New_account_payload.gen
    in
    let mint_tokens_gen =
      match source_pk with
      | Some token_owner_pk ->
          map Minting_payload.gen ~f:(fun payload ->
              {payload with token_owner_pk} )
      | None ->
          Minting_payload.gen
    in
    map
      (variant5
         (Payment_payload.gen ?source_pk ~max_amount)
         stake_delegation_gen new_token_gen token_account_gen mint_tokens_gen)
      ~f:(function
        | `A p ->
            Payment p
        | `B d ->
            Stake_delegation d
        | `C payload ->
            Create_new_token payload
        | `D payload ->
            Create_token_account payload
        | `E payload ->
            Mint_tokens payload )

  let source_pk (t : t) =
    match t with
    | Payment payload ->
        payload.source_pk
    | Stake_delegation payload ->
        Stake_delegation.source_pk payload
    | Create_new_token payload ->
        New_token_payload.source_pk payload
    | Create_token_account payload ->
        New_account_payload.source_pk payload
    | Mint_tokens payload ->
        Minting_payload.source_pk payload

  let receiver_pk (t : t) =
    match t with
    | Payment payload ->
        payload.receiver_pk
    | Stake_delegation payload ->
        Stake_delegation.receiver_pk payload
    | Create_new_token payload ->
        New_token_payload.receiver_pk payload
    | Create_token_account payload ->
        New_account_payload.receiver_pk payload
    | Mint_tokens payload ->
        Minting_payload.receiver_pk payload

  let token (t : t) =
    match t with
    | Payment payload ->
        payload.token_id
    | Stake_delegation _ ->
        Token_id.default
    | Create_new_token payload ->
        New_token_payload.token payload
    | Create_token_account payload ->
        New_account_payload.token payload
    | Mint_tokens payload ->
        Minting_payload.token payload

  let source ~next_available_token t =
    match t with
    | Payment payload ->
        Account_id.create payload.source_pk payload.token_id
    | Stake_delegation payload ->
        Stake_delegation.source payload
    | Create_new_token payload ->
        New_token_payload.source ~next_available_token payload
    | Create_token_account payload ->
        New_account_payload.source payload
    | Mint_tokens payload ->
        Minting_payload.source payload

  let receiver ~next_available_token t =
    match t with
    | Payment payload ->
        Account_id.create payload.receiver_pk payload.token_id
    | Stake_delegation payload ->
        Stake_delegation.receiver payload
    | Create_new_token payload ->
        New_token_payload.receiver ~next_available_token payload
    | Create_token_account payload ->
        New_account_payload.receiver payload
    | Mint_tokens payload ->
        Minting_payload.receiver payload

  let tag = function
    | Payment _ ->
        Transaction_union_tag.Payment
    | Stake_delegation _ ->
        Transaction_union_tag.Stake_delegation
    | Create_new_token _ ->
        Transaction_union_tag.Create_account
    | Create_token_account _ ->
        Transaction_union_tag.Create_account
    | Mint_tokens _ ->
        Transaction_union_tag.Mint_tokens
end

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('common, 'body) t = {common: 'common; body: 'body}
      [@@deriving equal, sexp, hash, yojson, compare, hlist]

      let of_latest common_latest body_latest {common; body} =
        let open Result.Let_syntax in
        let%map common = common_latest common and body = body_latest body in
        {common; body}
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t = (Common.Stable.V1.t, Body.Stable.V1.t) Poly.Stable.V1.t
    [@@deriving compare, equal, sexp, hash, yojson]

    let to_latest = Fn.id
  end
end]

let create ~fee ~fee_token ~fee_payer_pk ~nonce ~valid_until ~memo ~body : t =
  { common=
      { fee
      ; fee_token
      ; fee_payer_pk
      ; nonce
      ; valid_until= Option.value valid_until ~default:Global_slot.max_value
      ; memo }
  ; body }

let fee (t : t) = t.common.fee

let fee_token (t : t) = t.common.fee_token

let fee_payer_pk (t : t) = t.common.fee_payer_pk

let fee_payer (t : t) =
  Account_id.create t.common.fee_payer_pk t.common.fee_token

let nonce (t : t) = t.common.nonce

let valid_until (t : t) = t.common.valid_until

let memo (t : t) = t.common.memo

let body (t : t) = t.body

let source_pk (t : t) = Body.source_pk t.body

let source ~next_available_token (t : t) =
  Body.source ~next_available_token t.body

let receiver_pk (t : t) = Body.receiver_pk t.body

let receiver ~next_available_token (t : t) =
  Body.receiver ~next_available_token t.body

let token (t : t) = Body.token t.body

let tag (t : t) = Body.tag t.body

let amount (t : t) =
  match t.body with
  | Payment payload ->
      Some payload.Payment_payload.Poly.amount
  | Stake_delegation _ ->
      None
  | Create_new_token _ ->
      None
  | Create_token_account _ ->
      None
  | Mint_tokens payload ->
      Some payload.Minting_payload.amount

let fee_excess (t : t) =
  Fee_excess.of_single (fee_token t, Currency.Fee.Signed.of_unsigned (fee t))

let accounts_accessed ~next_available_token (t : t) =
  [ fee_payer t
  ; source ~next_available_token t
  ; receiver ~next_available_token t ]

let next_available_token (t : t) token =
  match t.body with Create_new_token _ -> Token_id.next token | _ -> token

let dummy : t =
  { common=
      { fee= Currency.Fee.zero
      ; fee_token= Token_id.default
      ; fee_payer_pk= Public_key.Compressed.empty
      ; nonce= Account_nonce.zero
      ; valid_until= Global_slot.max_value
      ; memo= Memo.dummy }
  ; body= Payment Payment_payload.dummy }

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind common = Common.gen ~fee_token_id:Token_id.default () in
  let max_amount =
    Currency.Amount.(sub max_int (of_fee common.fee))
    |> Option.value_exn ?here:None ?error:None ?message:None
  in
  let%map body = Body.gen ~source_pk:common.fee_payer_pk ~max_amount in
  Poly.{common; body}
