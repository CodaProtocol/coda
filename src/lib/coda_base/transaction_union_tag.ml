(* transaction_union_tag.ml *)

[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick

[%%else]

module Random_oracle = Random_oracle_nonconsensus

[%%endif]

type t =
  | Payment
  | Stake_delegation
  | Create_account
  | Mint_tokens
  | Set_token_permissions
  | Fee_transfer
  | Coinbase
[@@deriving enum, eq, sexp]

let to_string = function
  | Payment ->
      "payment"
  | Stake_delegation ->
      "delegation"
  | Create_account ->
      "create_account"
  | Mint_tokens ->
      "mint_tokens"
  | Set_token_permissions ->
      "set_token_permissions"
  | Fee_transfer ->
      "fee-transfer"
  | Coinbase ->
      "coinbase"

let gen =
  Quickcheck.Generator.map (Int.gen_incl min max) ~f:(fun i ->
      Option.value_exn (of_enum i) )

module Bits = struct
  type t = bool * bool * bool [@@deriving eq]

  let of_int i : t =
    let test_mask mask = i land mask = mask in
    (test_mask 0b100, test_mask 0b10, test_mask 0b1)

  let of_t x = of_int (to_enum x)

  let payment = of_t Payment

  let stake_delegation = of_t Stake_delegation

  let create_account = of_t Create_account

  let mint_tokens = of_t Mint_tokens

  let set_token_permissions = of_t Set_token_permissions

  let fee_transfer = of_t Fee_transfer

  let coinbase = of_t Coinbase

  let to_bits (b1, b2, b3) = [b1; b2; b3]

  let to_input t = Random_oracle.Input.bitstring (to_bits t)

  [%%ifdef
  consensus_mechanism]

  type var = Boolean.var * Boolean.var * Boolean.var

  let typ = Typ.tuple3 Boolean.typ Boolean.typ Boolean.typ

  let constant (b1, b2, b3) =
    Boolean.(var_of_value b1, var_of_value b2, var_of_value b3)

  [%%endif]
end

module Unpacked = struct
  (* Invariant: exactly one of the tag identifiers must be true. *)
  module Poly = struct
    type 'bool t =
      { is_payment: 'bool
      ; is_stake_delegation: 'bool
      ; is_create_account: 'bool
      ; is_mint_tokens: 'bool
      ; is_set_token_permissions: 'bool
      ; is_fee_transfer: 'bool
      ; is_coinbase: 'bool
      ; is_user_command: 'bool }
    [@@deriving eq]

    [%%ifdef
    consensus_mechanism]

    let to_hlist
        { is_payment
        ; is_stake_delegation
        ; is_create_account
        ; is_mint_tokens
        ; is_set_token_permissions
        ; is_fee_transfer
        ; is_coinbase
        ; is_user_command } =
      H_list.
        [ is_payment
        ; is_stake_delegation
        ; is_create_account
        ; is_mint_tokens
        ; is_set_token_permissions
        ; is_fee_transfer
        ; is_coinbase
        ; is_user_command ]

    let of_hlist
        ([ is_payment
         ; is_stake_delegation
         ; is_create_account
         ; is_mint_tokens
         ; is_set_token_permissions
         ; is_fee_transfer
         ; is_coinbase
         ; is_user_command ] :
          (unit, _) H_list.t) =
      { is_payment
      ; is_stake_delegation
      ; is_create_account
      ; is_mint_tokens
      ; is_set_token_permissions
      ; is_fee_transfer
      ; is_coinbase
      ; is_user_command }

    let typ (bool : ('bool_var, 'bool) Typ.t) : ('bool_var t, 'bool t) Typ.t =
      Typ.of_hlistable
        [bool; bool; bool; bool; bool; bool; bool; bool]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    [%%endif]
  end

  type t = bool Poly.t [@@deriving eq]

  (* An invalid value with all types empty. Do not use directly. *)
  let empty : t =
    { is_payment= false
    ; is_stake_delegation= false
    ; is_create_account= false
    ; is_mint_tokens= false
    ; is_set_token_permissions= false
    ; is_fee_transfer= false
    ; is_coinbase= false
    ; is_user_command= false }

  let payment = {empty with is_payment= true; is_user_command= true}

  let stake_delegation =
    {empty with is_stake_delegation= true; is_user_command= true}

  let create_account =
    {empty with is_create_account= true; is_user_command= true}

  let mint_tokens = {empty with is_mint_tokens= true; is_user_command= true}

  let set_token_permissions =
    {empty with is_set_token_permissions= true; is_user_command= true}

  let fee_transfer = {empty with is_fee_transfer= true; is_user_command= false}

  let coinbase = {empty with is_coinbase= true; is_user_command= false}

  let of_bits_t (bits : Bits.t) : t =
    match
      List.Assoc.find ~equal:Bits.equal
        [ (Bits.payment, payment)
        ; (Bits.stake_delegation, stake_delegation)
        ; (Bits.create_account, create_account)
        ; (Bits.mint_tokens, mint_tokens)
        ; (Bits.set_token_permissions, set_token_permissions)
        ; (Bits.fee_transfer, fee_transfer)
        ; (Bits.coinbase, coinbase) ]
        bits
    with
    | Some t ->
        t
    | None ->
        raise (Invalid_argument "Transaction_union_tag.Unpacked.of_bits_t")

  let to_bits_t (t : t) : Bits.t =
    match
      List.Assoc.find ~equal
        [ (payment, Bits.payment)
        ; (stake_delegation, Bits.stake_delegation)
        ; (create_account, Bits.create_account)
        ; (mint_tokens, Bits.mint_tokens)
        ; (set_token_permissions, Bits.set_token_permissions)
        ; (fee_transfer, Bits.fee_transfer)
        ; (coinbase, Bits.coinbase) ]
        t
    with
    | Some bits ->
        bits
    | None ->
        raise (Invalid_argument "Transaction_union_tag.Unpacked.to_bits_t")

  [%%ifdef
  consensus_mechanism]

  type var = Boolean.var Poly.t

  let to_bits_var
      ({ is_payment
       ; is_stake_delegation
       ; is_create_account
       ; is_mint_tokens
       ; is_set_token_permissions
       ; is_fee_transfer
       ; is_coinbase
       ; is_user_command= _ } :
        var) =
    (* For each bit, compute the sum of all the tags for which that bit is true
       in its bit representation.

       Since we have the invariant that exactly one tag identifier is true,
       exactly the bits in that tag's bit representation will be true in the
       resulting bits.
    *)
    let b1, b2, b3 =
      List.fold
        ~init:Field.(Var.(constant zero, constant zero, constant zero))
        [ (Bits.payment, is_payment)
        ; (Bits.stake_delegation, is_stake_delegation)
        ; (Bits.create_account, is_create_account)
        ; (Bits.mint_tokens, is_mint_tokens)
        ; (Bits.set_token_permissions, is_set_token_permissions)
        ; (Bits.fee_transfer, is_fee_transfer)
        ; (Bits.coinbase, is_coinbase) ]
        ~f:(fun (acc1, acc2, acc3) ((bit1, bit2, bit3), bool_var) ->
          let add_if_true bit acc =
            if bit then Field.Var.add acc (bool_var :> Field.Var.t) else acc
          in
          (add_if_true bit1 acc1, add_if_true bit2 acc2, add_if_true bit3 acc3)
          )
    in
    Boolean.Unsafe.(of_cvar b1, of_cvar b2, of_cvar b3)

  let typ : (var, t) Typ.t =
    let base_typ = Poly.typ Boolean.typ in
    { base_typ with
      check=
        (fun ( { is_payment
               ; is_stake_delegation
               ; is_create_account
               ; is_mint_tokens
               ; is_set_token_permissions
               ; is_fee_transfer
               ; is_coinbase
               ; is_user_command } as t ) ->
          let open Checked.Let_syntax in
          let%bind () = base_typ.check t in
          let%bind () =
            [%with_label "Only one tag is set"]
              (Boolean.Assert.exactly_one
                 [ is_payment
                 ; is_stake_delegation
                 ; is_create_account
                 ; is_set_token_permissions
                 ; is_mint_tokens
                 ; is_fee_transfer
                 ; is_coinbase ])
          in
          [%with_label "User command flag is correctly set"]
            (Boolean.Assert.exactly_one
               [is_user_command; is_fee_transfer; is_coinbase]) ) }

  let constant
      ({ is_payment
       ; is_stake_delegation
       ; is_create_account
       ; is_mint_tokens
       ; is_set_token_permissions
       ; is_fee_transfer
       ; is_coinbase
       ; is_user_command } :
        t) : var =
    { is_payment= Boolean.var_of_value is_payment
    ; is_stake_delegation= Boolean.var_of_value is_stake_delegation
    ; is_create_account= Boolean.var_of_value is_create_account
    ; is_mint_tokens= Boolean.var_of_value is_mint_tokens
    ; is_set_token_permissions= Boolean.var_of_value is_set_token_permissions
    ; is_fee_transfer= Boolean.var_of_value is_fee_transfer
    ; is_coinbase= Boolean.var_of_value is_coinbase
    ; is_user_command= Boolean.var_of_value is_user_command }

  let is_payment ({is_payment; _} : var) = is_payment

  let is_stake_delegation ({is_stake_delegation; _} : var) =
    is_stake_delegation

  let is_create_account ({is_create_account; _} : var) = is_create_account

  let is_mint_tokens ({is_mint_tokens; _} : var) = is_mint_tokens

  let is_set_token_permissions ({is_set_token_permissions; _} : var) =
    is_set_token_permissions

  let is_fee_transfer ({is_fee_transfer; _} : var) = is_fee_transfer

  let is_coinbase ({is_coinbase; _} : var) = is_coinbase

  let is_user_command ({is_user_command; _} : var) = is_user_command

  let to_bits t = Bits.to_bits (to_bits_var t)

  let to_input t = Random_oracle.Input.bitstring (to_bits t)

  [%%endif]
end

let unpacked_t_of_t = function
  | Payment ->
      Unpacked.payment
  | Stake_delegation ->
      Unpacked.stake_delegation
  | Create_account ->
      Unpacked.create_account
  | Mint_tokens ->
      Unpacked.mint_tokens
  | Set_token_permissions ->
      Unpacked.set_token_permissions
  | Fee_transfer ->
      Unpacked.fee_transfer
  | Coinbase ->
      Unpacked.coinbase

let to_bits tag = Bits.to_bits (Unpacked.to_bits_t (unpacked_t_of_t tag))

let to_input tag = Random_oracle.Input.bitstring (to_bits tag)

[%%ifdef
consensus_mechanism]

let t_of_unpacked_t (unpacked : Unpacked.t) : t =
  match
    List.Assoc.find ~equal:Unpacked.equal
      [ (Unpacked.payment, Payment)
      ; (Unpacked.stake_delegation, Stake_delegation)
      ; (Unpacked.create_account, Create_account)
      ; (Unpacked.mint_tokens, Mint_tokens)
      ; (Unpacked.set_token_permissions, Set_token_permissions)
      ; (Unpacked.fee_transfer, Fee_transfer)
      ; (Unpacked.coinbase, Coinbase) ]
      unpacked
  with
  | Some t ->
      t
  | None ->
      raise (Invalid_argument "Transaction_union_tag.t_of_unpacked_t")

let bits_t_of_t tag = Unpacked.to_bits_t (unpacked_t_of_t tag)

let t_of_bits_t tag = t_of_unpacked_t (Unpacked.of_bits_t tag)

let unpacked_of_t tag = Unpacked.constant (unpacked_t_of_t tag)

let bits_of_t tag = Bits.constant (bits_t_of_t tag)

let unpacked_typ =
  Typ.transport Unpacked.typ ~there:unpacked_t_of_t ~back:t_of_unpacked_t

let bits_typ = Typ.transport Bits.typ ~there:bits_t_of_t ~back:t_of_bits_t

let%test_module "predicates" =
  ( module struct
    let test_predicate checked unchecked =
      let checked x = Checked.return (checked x) in
      for i = min to max do
        Test_util.test_equal unpacked_typ Boolean.typ checked unchecked
          (Option.value_exn (of_enum i))
      done

    let one_of xs t = List.mem xs ~equal t

    let%test_unit "is_payment" =
      test_predicate Unpacked.is_payment (( = ) Payment)

    let%test_unit "is_stake_delegation" =
      test_predicate Unpacked.is_stake_delegation (( = ) Stake_delegation)

    let%test_unit "is_create_account" =
      test_predicate Unpacked.is_create_account (( = ) Create_account)

    let%test_unit "is_mint_tokens" =
      test_predicate Unpacked.is_mint_tokens (( = ) Mint_tokens)

    let%test_unit "is_set_token_permissions" =
      test_predicate Unpacked.is_mint_tokens (( = ) Set_token_permissions)

    let%test_unit "is_fee_transfer" =
      test_predicate Unpacked.is_fee_transfer (( = ) Fee_transfer)

    let%test_unit "is_coinbase" =
      test_predicate Unpacked.is_coinbase (( = ) Coinbase)

    let%test_unit "is_user_command" =
      test_predicate Unpacked.is_user_command
        (one_of
           [ Payment
           ; Stake_delegation
           ; Create_account
           ; Mint_tokens
           ; Set_token_permissions ])

    let%test_unit "not_user_command" =
      test_predicate
        (fun x -> Boolean.not (Unpacked.is_user_command x))
        (one_of [Fee_transfer; Coinbase])

    let%test_unit "bit_representation" =
      for i = min to max do
        Test_util.test_equal unpacked_typ Bits.typ
          (Fn.compose Checked.return Unpacked.to_bits_var)
          bits_t_of_t
          (Option.value_exn (of_enum i))
      done
  end )

[%%endif]
