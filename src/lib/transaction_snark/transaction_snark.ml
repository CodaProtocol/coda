open Core
open Signature_lib
open Coda_base
open Snark_params
module Global_slot = Coda_numbers.Global_slot
open Currency

let tick_input () =
  let open Tick in
  Data_spec.[Field.typ]

let wrap_input = Tock.Data_spec.[Wrap_input.typ]

let exists' typ ~f = Tick.(exists typ ~compute:As_prover.(map get_state ~f))

let top_hash_logging_enabled = ref false

let with_top_hash_logging f =
  let old = !top_hash_logging_enabled in
  top_hash_logging_enabled := true ;
  try
    let ret = f () in
    top_hash_logging_enabled := old ;
    ret
  with err ->
    top_hash_logging_enabled := old ;
    raise err

module Proof_type = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = [`Base | `Merge] [@@deriving compare, equal, hash, sexp, yojson]

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t [@@deriving sexp, hash, compare, yojson]

  let is_base = function `Base -> true | `Merge -> false
end

module Pending_coinbase_stack_state = struct
  module Init_stack = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = Base of Pending_coinbase.Stack_versioned.Stable.V1.t | Merge
        [@@deriving sexp, hash, compare, eq, yojson]

        let to_latest = Fn.id
      end
    end]

    type t = Stable.Latest.t = Base of Pending_coinbase.Stack.t | Merge
    [@@deriving sexp, hash, compare, yojson]
  end

  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type 'pending_coinbase t =
          {source: 'pending_coinbase; target: 'pending_coinbase}
        [@@deriving sexp, hash, compare, eq, fields, yojson]

        let to_latest pending_coinbase {source; target} =
          {source= pending_coinbase source; target= pending_coinbase target}
      end
    end]

    type 'pending_coinbase t = 'pending_coinbase Stable.Latest.t =
      {source: 'pending_coinbase; target: 'pending_coinbase}
    [@@deriving sexp, hash, compare, eq, fields, yojson]

    let to_hlist {source; target} = H_list.[source; target]

    let of_hlist ([source; target] : (unit, _) H_list.t) = {source; target}

    let typ pending_coinbase =
      Tick.Typ.of_hlistable
        [pending_coinbase; pending_coinbase]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
  end

  type 'pending_coinbase poly = 'pending_coinbase Poly.t =
    {source: 'pending_coinbase; target: 'pending_coinbase}
  [@@deriving sexp, hash, compare, eq, fields, yojson]

  (* State of the coinbase stack for the current transaction snark *)
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Pending_coinbase.Stack_versioned.Stable.V1.t Poly.Stable.V1.t
      [@@deriving sexp, hash, compare, eq, yojson]

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t [@@deriving sexp, hash, compare, yojson]

  type var = Pending_coinbase.Stack.var Poly.t

  let typ = Poly.typ Pending_coinbase.Stack.typ

  let to_input ({source; target} : t) =
    Random_oracle.Input.append
      (Pending_coinbase.Stack.to_input source)
      (Pending_coinbase.Stack.to_input target)

  let var_to_input ({source; target} : var) =
    Random_oracle.Input.append
      (Pending_coinbase.Stack.var_to_input source)
      (Pending_coinbase.Stack.var_to_input target)

  include Hashable.Make_binable (Stable.Latest)
  include Comparable.Make (Stable.Latest)
end

module Statement = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ( 'ledger_hash
             , 'amount
             , 'pending_coinbase
             , 'fee_excess
             , 'token_id
             , 'proof_type
             , 'sok_digest )
             t =
          { source: 'ledger_hash
          ; target: 'ledger_hash
          ; supply_increase: 'amount
          ; pending_coinbase_stack_state: 'pending_coinbase
          ; fee_excess: 'fee_excess
          ; next_available_token_before: 'token_id
          ; next_available_token_after: 'token_id
          ; proof_type: 'proof_type
          ; sok_digest: 'sok_digest }
        [@@deriving compare, equal, hash, sexp, yojson]

        let to_latest ledger_hash amount pending_coinbase fee_excess' token_id
            proof_type' sok_digest'
            { source
            ; target
            ; supply_increase
            ; pending_coinbase_stack_state
            ; fee_excess
            ; next_available_token_before
            ; next_available_token_after
            ; proof_type
            ; sok_digest } =
          { source= ledger_hash source
          ; target= ledger_hash target
          ; supply_increase= amount supply_increase
          ; pending_coinbase_stack_state=
              pending_coinbase pending_coinbase_stack_state
          ; fee_excess= fee_excess' fee_excess
          ; next_available_token_before= token_id next_available_token_before
          ; next_available_token_after= token_id next_available_token_after
          ; proof_type= proof_type' proof_type
          ; sok_digest= sok_digest' sok_digest }
      end
    end]

    type ( 'ledger_hash
         , 'amount
         , 'pending_coinbase
         , 'fee_excess
         , 'token_id
         , 'proof_type
         , 'sok_digest )
         t =
          ( 'ledger_hash
          , 'amount
          , 'pending_coinbase
          , 'fee_excess
          , 'token_id
          , 'proof_type
          , 'sok_digest )
          Stable.Latest.t =
      { source: 'ledger_hash
      ; target: 'ledger_hash
      ; supply_increase: 'amount
      ; pending_coinbase_stack_state: 'pending_coinbase
      ; fee_excess: 'fee_excess
      ; next_available_token_before: 'token_id
      ; next_available_token_after: 'token_id
      ; proof_type: 'proof_type
      ; sok_digest: 'sok_digest }
    [@@deriving compare, equal, hash, sexp, yojson]

    let to_hlist
        { source
        ; target
        ; supply_increase
        ; pending_coinbase_stack_state
        ; fee_excess
        ; next_available_token_before
        ; next_available_token_after
        ; proof_type
        ; sok_digest } =
      H_list.
        [ source
        ; target
        ; supply_increase
        ; pending_coinbase_stack_state
        ; fee_excess
        ; next_available_token_before
        ; next_available_token_after
        ; proof_type
        ; sok_digest ]

    let of_hlist
        ([ source
         ; target
         ; supply_increase
         ; pending_coinbase_stack_state
         ; fee_excess
         ; next_available_token_before
         ; next_available_token_after
         ; proof_type
         ; sok_digest ] :
          (unit, _) H_list.t) =
      { source
      ; target
      ; supply_increase
      ; pending_coinbase_stack_state
      ; fee_excess
      ; next_available_token_before
      ; next_available_token_after
      ; proof_type
      ; sok_digest }

    let typ ledger_hash amount pending_coinbase fee_excess token_id proof_type
        sok_digest =
      Tick.Typ.of_hlistable
        [ ledger_hash
        ; ledger_hash
        ; amount
        ; pending_coinbase
        ; fee_excess
        ; token_id
        ; token_id
        ; proof_type
        ; sok_digest ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
  end

  type ( 'ledger_hash
       , 'amount
       , 'pending_coinbase
       , 'fee_excess
       , 'token_id
       , 'proof_type
       , 'sok_digest )
       poly =
        ( 'ledger_hash
        , 'amount
        , 'pending_coinbase
        , 'fee_excess
        , 'token_id
        , 'proof_type
        , 'sok_digest )
        Poly.t =
    { source: 'ledger_hash
    ; target: 'ledger_hash
    ; supply_increase: 'amount
    ; pending_coinbase_stack_state: 'pending_coinbase
    ; fee_excess: 'fee_excess
    ; next_available_token_before: 'token_id
    ; next_available_token_after: 'token_id
    ; proof_type: 'proof_type
    ; sok_digest: 'sok_digest }
  [@@deriving compare, equal, hash, sexp, yojson]

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Frozen_ledger_hash.Stable.V1.t
        , Currency.Amount.Stable.V1.t
        , Pending_coinbase_stack_state.Stable.V1.t
        , Fee_excess.Stable.V1.t
        , Token_id.Stable.V1.t
        , Proof_type.Stable.V1.t
        , unit )
        Poly.Stable.V1.t
      [@@deriving compare, equal, hash, sexp, yojson]

      let to_latest = Fn.id
    end
  end]

  type t =
    ( Frozen_ledger_hash.t
    , Currency.Amount.t
    , Pending_coinbase_stack_state.t
    , Fee_excess.t
    , Token_id.t
    , Proof_type.t
    , unit )
    Poly.t
  [@@deriving sexp, hash, compare, yojson]

  module With_sok = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          ( Frozen_ledger_hash.Stable.V1.t
          , Currency.Amount.Stable.V1.t
          , Pending_coinbase_stack_state.Stable.V1.t
          , Fee_excess.Stable.V1.t
          , Token_id.Stable.V1.t
          , unit
          , Sok_message.Digest.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving compare, equal, hash, sexp, yojson]

        let to_latest = Fn.id
      end
    end]

    type t = Stable.Latest.t [@@deriving sexp, hash, compare, yojson]

    type var =
      ( Frozen_ledger_hash.var
      , Currency.Amount.var
      , Pending_coinbase_stack_state.var
      , Fee_excess.var
      , Token_id.var
      , unit
      , Sok_message.Digest.Checked.t )
      Poly.t

    let typ : (var, t) Tick.Typ.t =
      Poly.typ Frozen_ledger_hash.typ Currency.Amount.typ
        Pending_coinbase_stack_state.typ Fee_excess.typ Token_id.typ
        Tick.Typ.unit Sok_message.Digest.typ

    let to_input
        { source
        ; target
        ; supply_increase
        ; pending_coinbase_stack_state
        ; fee_excess
        ; next_available_token_before
        ; next_available_token_after
        ; proof_type= _
        ; sok_digest } =
      let input =
        Array.reduce_exn ~f:Random_oracle.Input.append
          [| Sok_message.Digest.to_input sok_digest
           ; Frozen_ledger_hash.to_input source
           ; Frozen_ledger_hash.to_input target
           ; Pending_coinbase_stack_state.to_input pending_coinbase_stack_state
           ; Amount.to_input supply_increase
           ; Fee_excess.to_input fee_excess
           ; Token_id.to_input next_available_token_before
           ; Token_id.to_input next_available_token_after |]
      in
      if !top_hash_logging_enabled then
        Format.eprintf
          !"Generating unchecked top hash from:@.%{sexp: (Tick.Field.t, bool) \
            Random_oracle.Input.t}@."
          input ;
      input

    let to_field_elements t = Random_oracle.pack_input (to_input t)

    module Checked = struct
      let to_input
          { source
          ; target
          ; supply_increase
          ; pending_coinbase_stack_state
          ; fee_excess
          ; next_available_token_before
          ; next_available_token_after
          ; proof_type= _
          ; sok_digest } =
        let open Tick in
        let open Checked.Let_syntax in
        let%bind fee_excess = Fee_excess.to_input_checked fee_excess in
        let%bind next_available_token_before =
          Token_id.Checked.to_input next_available_token_before
        in
        let%bind next_available_token_after =
          Token_id.Checked.to_input next_available_token_after
        in
        let input =
          Array.reduce_exn ~f:Random_oracle.Input.append
            [| Sok_message.Digest.Checked.to_input sok_digest
             ; Frozen_ledger_hash.var_to_input source
             ; Frozen_ledger_hash.var_to_input target
             ; Pending_coinbase_stack_state.var_to_input
                 pending_coinbase_stack_state
             ; Amount.var_to_input supply_increase
             ; fee_excess
             ; next_available_token_before
             ; next_available_token_after |]
        in
        let%map () =
          as_prover
            As_prover.(
              if !top_hash_logging_enabled then
                let%bind field_elements =
                  read
                    (Typ.list ~length:0 Field.typ)
                    (Array.to_list input.field_elements)
                in
                let%map bitstrings =
                  read
                    (Typ.list ~length:0 (Typ.list ~length:0 Boolean.typ))
                    (Array.to_list input.bitstrings)
                in
                Format.eprintf
                  !"Generating checked top hash from:@.%{sexp: (Field.t, \
                    bool) Random_oracle.Input.t}@."
                  { Random_oracle.Input.field_elements=
                      Array.of_list field_elements
                  ; bitstrings= Array.of_list bitstrings }
              else return ())
        in
        input

      let to_field_elements t =
        let open Tick.Checked.Let_syntax in
        to_input t >>| Random_oracle.Checked.pack_input
    end
  end

  let option lab =
    Option.value_map ~default:(Or_error.error_string lab) ~f:(fun x -> Ok x)

  let merge (s1 : t) (s2 : t) =
    let open Or_error.Let_syntax in
    let%map fee_excess = Fee_excess.combine s1.fee_excess s2.fee_excess
    and supply_increase =
      Currency.Amount.add s1.supply_increase s2.supply_increase
      |> option "Error adding supply_increase"
    and () =
      if
        Token_id.equal s1.next_available_token_after
          s2.next_available_token_before
      then return ()
      else
        Or_error.errorf
          !"Next available token is inconsistent between transitions (%{sexp: \
            Token_id.t} vs %{sexp: Token_id.t})"
          s1.next_available_token_after s2.next_available_token_before
    in
    ( { source= s1.source
      ; target= s2.target
      ; fee_excess
      ; next_available_token_before= s1.next_available_token_before
      ; next_available_token_after= s2.next_available_token_after
      ; proof_type= `Merge
      ; supply_increase
      ; pending_coinbase_stack_state=
          { source= s1.pending_coinbase_stack_state.source
          ; target= s2.pending_coinbase_stack_state.target }
      ; sok_digest= () }
      : t )

  include Hashable.Make_binable (Stable.Latest)
  include Comparable.Make (Stable.Latest)

  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map source = Frozen_ledger_hash.gen
    and target = Frozen_ledger_hash.gen
    and fee_excess = Fee_excess.gen
    and supply_increase = Currency.Amount.gen
    and pending_coinbase_before = Pending_coinbase.Stack.gen
    and pending_coinbase_after = Pending_coinbase.Stack.gen
    and next_available_token_before, next_available_token_after =
      let%map token1 = Token_id.gen_non_default
      and token2 = Token_id.gen_non_default in
      (Token_id.min token1 token2, Token_id.max token1 token2)
    and proof_type =
      Bool.quickcheck_generator >>| fun b -> if b then `Merge else `Base
    in
    ( { source
      ; target
      ; fee_excess
      ; next_available_token_before
      ; next_available_token_after
      ; proof_type
      ; supply_increase
      ; pending_coinbase_stack_state=
          {source= pending_coinbase_before; target= pending_coinbase_after}
      ; sok_digest= () }
      : t )
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { source: Frozen_ledger_hash.Stable.V1.t
      ; target: Frozen_ledger_hash.Stable.V1.t
      ; proof_type: Proof_type.Stable.V1.t
      ; supply_increase: Amount.Stable.V1.t
      ; pending_coinbase_stack_state: Pending_coinbase_stack_state.Stable.V1.t
      ; fee_excess: Fee_excess.Stable.V1.t
      ; next_available_token_before: Token_id.Stable.V1.t
      ; next_available_token_after: Token_id.Stable.V1.t
      ; sok_digest:
          (Sok_message.Digest.Stable.V1.t[@to_yojson
                                           fun _ -> `String "<opaque>"])
      ; proof: Proof.Stable.V1.t }
    [@@deriving compare, fields, sexp, version, to_yojson]

    let to_latest = Fn.id
  end
end]

type t = Stable.Latest.t =
  { source: Frozen_ledger_hash.t
  ; target: Frozen_ledger_hash.t
  ; proof_type: Proof_type.t
  ; supply_increase: Amount.t
  ; pending_coinbase_stack_state: Pending_coinbase_stack_state.t
  ; fee_excess: Fee_excess.t
  ; next_available_token_before: Token_id.t
  ; next_available_token_after: Token_id.t
  ; sok_digest: Sok_message.Digest.t
  ; proof: Proof.t }
[@@deriving fields, sexp]

let to_yojson = Stable.Latest.to_yojson

let statement
    ({ source
     ; target
     ; proof_type
     ; fee_excess
     ; next_available_token_before
     ; next_available_token_after
     ; supply_increase
     ; pending_coinbase_stack_state
     ; sok_digest= _
     ; proof= _ } :
      t) : Statement.t =
  { source
  ; target
  ; proof_type
  ; supply_increase
  ; pending_coinbase_stack_state
  ; fee_excess
  ; next_available_token_before
  ; next_available_token_after
  ; sok_digest= () }

let create = Fields.create

let base_top_hash t =
  Random_oracle.hash ~init:Hash_prefix.base_snark
    (Statement.With_sok.to_field_elements t)

let merge_top_hash wrap_vk_bits t =
  Random_oracle.hash ~init:wrap_vk_bits
    (Statement.With_sok.to_field_elements t)

module Verification_keys = struct
  [%%versioned_asserted
  module Stable = struct
    module V1 = struct
      type t =
        { base: Tick.Verification_key.t
        ; wrap: Tock.Verification_key.t
        ; merge: Tick.Verification_key.t }

      let to_latest = Fn.id
    end

    module Tests = struct
      let%test "verification keys v1" =
        let base = Tick.Verification_key.of_string "base key" in
        let wrap = Tock.Verification_key.of_string "wrap key" in
        let merge = Tick.Verification_key.of_string "merge key" in
        let keys = V1.{base; wrap; merge} in
        let known_good_digest = "1cade6287d659338ae1f2c3971ee8d06" in
        Ppx_version.Serialization.check_serialization
          (module V1)
          keys known_good_digest
    end
  end]

  type t = Stable.Latest.t =
    { base: Tick.Verification_key.t
    ; wrap: Tock.Verification_key.t
    ; merge: Tick.Verification_key.t }

  let dummy : t =
    let groth16 =
      Tick_backend.Verification_key.get_dummy
        ~input_size:(Tick.Data_spec.size (tick_input ()))
    in
    { merge= groth16
    ; base= groth16
    ; wrap= Tock_backend.Verification_key.get_dummy ~input_size:Wrap_input.size
    }
end

module Keys0 = struct
  module Verification = Verification_keys

  module Proving = struct
    type t =
      { base: Tick.Proving_key.t
      ; wrap: Tock.Proving_key.t
      ; merge: Tick.Proving_key.t }

    let dummy =
      { merge= Dummy_values.Tick.Groth16.proving_key
      ; base= Dummy_values.Tick.Groth16.proving_key
      ; wrap= Dummy_values.Tock.Bowe_gabizon18.proving_key }
  end

  module T = struct
    type t = {proving: Proving.t; verification: Verification.t}
  end

  include T
end

(* Staging:
   first make tick base.
   then make tick merge (which top_hashes in the tock wrap vk)
   then make tock wrap (which branches on the tick vk) *)

module Base = struct
  open Tick
  open Let_syntax

  type _ Snarky.Request.t +=
    | Transaction : Transaction_union.t Snarky.Request.t
    | State_body : Coda_state.Protocol_state.Body.Value.t Snarky.Request.t
    | Init_stack : Pending_coinbase.Stack.t Snarky.Request.t
    | Next_available_token : Token_id.t Snarky.Request.t

  module User_command_failure = struct
    (** The various ways that a user command may fail. These should be computed
        before applying the snark, to ensure that only the base fee is charged
        to the fee-payer if executing the user command will later fail.
    *)
    type 'bool t =
      { predicate_failed: 'bool (* All *)
      ; source_not_present: 'bool (* All *)
      ; receiver_not_present: 'bool (* Delegate, Mint_tokens *)
      ; amount_insufficient_to_create: 'bool (* Payment only *)
      ; token_cannot_create: 'bool (* Payment only, token<>default *)
      ; source_insufficient_balance: 'bool (* Payment only *)
      ; source_bad_timing: 'bool (* Payment only *)
      ; receiver_exists: 'bool (* Create_account only *)
      ; not_token_owner: 'bool (* Create_account, Mint_tokens *)
      ; token_auth: 'bool (* Create_account *)
      ; account_disabled: 'bool (* All *) }

    let num_fields = 11

    let to_list
        { predicate_failed
        ; source_not_present
        ; receiver_not_present
        ; amount_insufficient_to_create
        ; token_cannot_create
        ; source_insufficient_balance
        ; source_bad_timing
        ; receiver_exists
        ; not_token_owner
        ; token_auth
        ; account_disabled } =
      [ predicate_failed
      ; source_not_present
      ; receiver_not_present
      ; amount_insufficient_to_create
      ; token_cannot_create
      ; source_insufficient_balance
      ; source_bad_timing
      ; receiver_exists
      ; not_token_owner
      ; token_auth
      ; account_disabled ]

    let of_list = function
      | [ predicate_failed
        ; source_not_present
        ; receiver_not_present
        ; amount_insufficient_to_create
        ; token_cannot_create
        ; source_insufficient_balance
        ; source_bad_timing
        ; receiver_exists
        ; not_token_owner
        ; token_auth
        ; account_disabled ] ->
          { predicate_failed
          ; source_not_present
          ; receiver_not_present
          ; amount_insufficient_to_create
          ; token_cannot_create
          ; source_insufficient_balance
          ; source_bad_timing
          ; receiver_exists
          ; not_token_owner
          ; token_auth
          ; account_disabled }
      | _ ->
          failwith
            "Transaction_snark.Base.User_command_failure.to_list: bad length"

    let typ : (Boolean.var t, bool t) Typ.t =
      let open Typ in
      list ~length:num_fields Boolean.typ
      |> transport ~there:to_list ~back:of_list
      |> transport_var ~there:to_list ~back:of_list

    let any t = Boolean.any (to_list t)

    (** Compute which -- if any -- of the failure cases will be hit when
        evaluating the given user command, and indicate whether the fee-payer
        would need to pay the account creation fee if the user command were to
        succeed (irrespective or whether it actually will or not).
    *)
    let compute_unchecked
        ~(constraint_constants : Genesis_constants.Constraint_constants.t)
        ~txn_global_slot ~creating_new_token ~(fee_payer_account : Account.t)
        ~(receiver_account : Account.t) ~(source_account : Account.t)
        ({payload; signature= _; signer= _} : Transaction_union.t) =
      match payload.body.tag with
      | Fee_transfer | Coinbase ->
          (* Not user commands, return no failure. *)
          of_list (List.init num_fields ~f:(fun _ -> false))
      | _ -> (
          let fail s =
            failwithf
              "Transaction_snark.Base.User_command_failure.compute_unchecked: \
               %s"
              s ()
          in
          let fee_token = payload.common.fee_token in
          let token = payload.body.token_id in
          let fee_payer =
            Account_id.create payload.common.fee_payer_pk fee_token
          in
          let source = Account_id.create payload.body.source_pk token in
          let receiver = Account_id.create payload.body.receiver_pk token in
          (* This should shadow the logic in [Sparse_ledger]. *)
          let fee_payer_account =
            { fee_payer_account with
              balance=
                Option.value_exn ?here:None ?error:None ?message:None
                @@ Balance.sub_amount fee_payer_account.balance
                     (Amount.of_fee payload.common.fee) }
          in
          let predicate_failed, predicate_result =
            if
              Public_key.Compressed.equal payload.common.fee_payer_pk
                payload.body.source_pk
            then (false, true)
            else
              match payload.body.tag with
              | Create_account when creating_new_token ->
                  (* Any account is allowed to create a new token associated
                     with a public key.
                  *)
                  (false, true)
              | Create_account ->
                  (* Predicate failure is deferred here. It will be checked
                     later.
                  *)
                  let predicate_result =
                    (* TODO(#4554): Hook predicate evaluation in here once
                       implemented.
                    *)
                    false
                  in
                  (false, predicate_result)
              | Payment
              | Stake_delegation
              | Mint_tokens
              | Set_token_permissions ->
                  (* TODO(#4554): Hook predicate evaluation in here once
                     implemented.
                  *)
                  (true, false)
              | Fee_transfer | Coinbase ->
                  assert false
          in
          let account_disabled =
            match source_account.token_permissions with
            | Token_owned _ ->
                false
            | Not_owned {account_disabled} ->
                account_disabled
          in
          match payload.body.tag with
          | Fee_transfer | Coinbase ->
              assert false
          | Stake_delegation ->
              let receiver_account =
                if Account_id.equal receiver fee_payer then fee_payer_account
                else receiver_account
              in
              let receiver_not_present =
                let id = Account.identifier receiver_account in
                if Account_id.equal Account_id.empty id then true
                else if Account_id.equal receiver id then false
                else fail "bad receiver account ID"
              in
              let source_account =
                if Account_id.equal source fee_payer then fee_payer_account
                else source_account
              in
              let source_not_present =
                let id = Account.identifier source_account in
                if Account_id.equal Account_id.empty id then true
                else if Account_id.equal source id then false
                else fail "bad source account ID"
              in
              { predicate_failed
              ; source_not_present
              ; receiver_not_present
              ; amount_insufficient_to_create= false
              ; token_cannot_create= false
              ; source_insufficient_balance= false
              ; source_bad_timing= false
              ; receiver_exists= false
              ; not_token_owner= false
              ; token_auth= false
              ; account_disabled }
          | Payment ->
              let receiver_account =
                if Account_id.equal receiver fee_payer then fee_payer_account
                else receiver_account
              in
              let receiver_needs_creating =
                let id = Account.identifier receiver_account in
                if Account_id.equal Account_id.empty id then true
                else if Account_id.equal receiver id then false
                else fail "bad receiver account ID"
              in
              let token_is_default = Token_id.(equal default) token in
              let token_cannot_create =
                receiver_needs_creating && not token_is_default
              in
              let amount_insufficient_to_create =
                let creation_amount =
                  Amount.of_fee constraint_constants.account_creation_fee
                in
                receiver_needs_creating
                && Option.is_none
                     (Amount.sub payload.body.amount creation_amount)
              in
              let fee_payer_is_source = Account_id.equal fee_payer source in
              let source_account =
                if fee_payer_is_source then fee_payer_account
                else source_account
              in
              let source_not_present =
                let id = Account.identifier source_account in
                if Account_id.equal Account_id.empty id then true
                else if Account_id.equal source id then false
                else fail "bad source account ID"
              in
              let source_insufficient_balance =
                (* This failure is fatal if fee-payer and source account are
                   the same. This is checked in the transaction pool.
                *)
                (not fee_payer_is_source)
                &&
                if Account_id.equal source receiver then
                  (* The final balance will be [0 - account_creation_fee]. *)
                  receiver_needs_creating
                else
                  Amount.(
                    Balance.to_amount source_account.balance
                    < payload.body.amount)
              in
              let source_bad_timing =
                (* This failure is fatal if fee-payer and source account are
                   the same. This is checked in the transaction pool.
                *)
                (not fee_payer_is_source)
                && ( source_insufficient_balance
                   || Or_error.is_error
                        (Transaction_logic.validate_timing
                           ~txn_amount:payload.body.amount ~txn_global_slot
                           ~account:source_account) )
              in
              { predicate_failed
              ; source_not_present
              ; receiver_not_present= false
              ; amount_insufficient_to_create
              ; token_cannot_create
              ; source_insufficient_balance
              ; source_bad_timing
              ; receiver_exists= false
              ; not_token_owner= false
              ; token_auth= false
              ; account_disabled }
          | Create_account ->
              let receiver_account =
                if Account_id.equal receiver fee_payer then fee_payer_account
                else receiver_account
              in
              let receiver_exists =
                let id = Account.identifier receiver_account in
                if Account_id.equal Account_id.empty id then false
                else if Account_id.equal receiver id then true
                else fail "bad receiver account ID"
              in
              let receiver_account =
                { receiver_account with
                  public_key= Account_id.public_key receiver
                ; token_id= Account_id.token_id receiver
                ; token_permissions=
                    ( if receiver_exists then receiver_account.token_permissions
                    else if creating_new_token then
                      Token_permissions.Token_owned
                        {disable_new_accounts= payload.body.token_locked}
                    else
                      Token_permissions.Not_owned
                        {account_disabled= payload.body.token_locked} ) }
              in
              let source_account =
                if Account_id.equal source fee_payer then fee_payer_account
                else if Account_id.equal source receiver then receiver_account
                else source_account
              in
              let source_not_present =
                let id = Account.identifier source_account in
                if Account_id.equal Account_id.empty id then true
                else if Account_id.equal source id then false
                else fail "bad source account ID"
              in
              let token_auth, not_token_owner =
                if Token_id.(equal default) (Account_id.token_id receiver) then
                  (false, false)
                else
                  match source_account.token_permissions with
                  | Token_owned {disable_new_accounts} ->
                      ( not
                          ( Bool.equal payload.body.token_locked
                              disable_new_accounts
                          || predicate_result )
                      , false )
                  | Not_owned {account_disabled} ->
                      (* NOTE: This [token_auth] value doesn't matter, since we
                         know that there will be a [not_token_owner] failure
                         anyway. We choose this value, since it aliases to the
                         check above in the snark representation of accounts,
                         and so simplifies the snark code.
                      *)
                      ( not
                          ( Bool.equal payload.body.token_locked
                              account_disabled
                          || predicate_result )
                      , true )
              in
              { predicate_failed= false
              ; source_not_present
              ; receiver_not_present= false
              ; amount_insufficient_to_create= false
              ; token_cannot_create= false
              ; source_insufficient_balance= false
              ; source_bad_timing= false
              ; receiver_exists
              ; not_token_owner
              ; token_auth
              ; account_disabled }
          | Mint_tokens | Set_token_permissions ->
              let receiver_account =
                if Account_id.equal receiver fee_payer then fee_payer_account
                else receiver_account
              in
              let receiver_not_present =
                let id = Account.identifier receiver_account in
                if Account_id.equal Account_id.empty id then true
                else if Account_id.equal receiver id then false
                else fail "bad receiver account ID"
              in
              let source_not_present =
                let id = Account.identifier source_account in
                if Account_id.equal Account_id.empty id then true
                else if Account_id.equal source id then false
                else fail "bad source account ID"
              in
              let not_token_owner =
                match source_account.token_permissions with
                | Token_owned _ ->
                    false
                | Not_owned _ ->
                    true
              in
              { predicate_failed
              ; source_not_present
              ; receiver_not_present
              ; amount_insufficient_to_create= false
              ; token_cannot_create= false
              ; source_insufficient_balance= false
              ; source_bad_timing= false
              ; receiver_exists= false
              ; not_token_owner
              ; token_auth= false
              ; account_disabled } )

    let%snarkydef compute_as_prover ~constraint_constants ~txn_global_slot
        ~creating_new_token ~next_available_token (txn : Transaction_union.var)
        =
      let%bind data =
        exists (Typ.Internal.ref ())
          ~compute:
            As_prover.(
              let%bind txn = read Transaction_union.typ txn in
              let fee_token = txn.payload.common.fee_token in
              let token = txn.payload.body.token_id in
              let%map token =
                if Token_id.(equal invalid) token then
                  read Token_id.typ next_available_token
                else return token
              in
              let fee_payer =
                Account_id.create txn.payload.common.fee_payer_pk fee_token
              in
              let source =
                Account_id.create txn.payload.body.source_pk token
              in
              let receiver =
                Account_id.create txn.payload.body.receiver_pk token
              in
              (txn, fee_payer, source, receiver))
      in
      let%bind fee_payer_idx =
        exists (Typ.Internal.ref ())
          ~request:
            As_prover.(
              let%map _txn, fee_payer, _source, _receiver =
                read (Typ.Internal.ref ()) data
              in
              Ledger_hash.Find_index fee_payer)
      in
      let%bind fee_payer_account =
        exists (Typ.Internal.ref ())
          ~request:
            As_prover.(
              let%map fee_payer_idx =
                read (Typ.Internal.ref ()) fee_payer_idx
              in
              Ledger_hash.Get_element fee_payer_idx)
      in
      let%bind source_idx =
        exists (Typ.Internal.ref ())
          ~request:
            As_prover.(
              let%map _txn, _fee_payer, source, _receiver =
                read (Typ.Internal.ref ()) data
              in
              Ledger_hash.Find_index source)
      in
      let%bind source_account =
        exists (Typ.Internal.ref ())
          ~request:
            As_prover.(
              let%map source_idx = read (Typ.Internal.ref ()) source_idx in
              Ledger_hash.Get_element source_idx)
      in
      let%bind receiver_idx =
        exists (Typ.Internal.ref ())
          ~request:
            As_prover.(
              let%map _txn, _fee_payer, _source, receiver =
                read (Typ.Internal.ref ()) data
              in
              Ledger_hash.Find_index receiver)
      in
      let%bind receiver_account =
        exists (Typ.Internal.ref ())
          ~request:
            As_prover.(
              let%map receiver_idx = read (Typ.Internal.ref ()) receiver_idx in
              Ledger_hash.Get_element receiver_idx)
      in
      exists typ
        ~compute:
          As_prover.(
            let%bind txn, _fee_payer, _source, _receiver =
              read (Typ.Internal.ref ()) data
            in
            let%bind fee_payer_account, _path =
              read (Typ.Internal.ref ()) fee_payer_account
            in
            let%bind source_account, _path =
              read (Typ.Internal.ref ()) source_account
            in
            let%bind receiver_account, _path =
              read (Typ.Internal.ref ()) receiver_account
            in
            let%bind creating_new_token =
              read Boolean.typ creating_new_token
            in
            let%map txn_global_slot = read Global_slot.typ txn_global_slot in
            compute_unchecked ~constraint_constants ~txn_global_slot
              ~creating_new_token ~fee_payer_account ~source_account
              ~receiver_account txn)
  end

  let%snarkydef check_signature shifted ~payload ~is_user_command ~signer
      ~signature =
    let%bind input = Transaction_union_payload.Checked.to_input payload in
    let%bind verifies =
      Schnorr.Checked.verifies shifted signature signer input
    in
    Boolean.Assert.any [Boolean.not is_user_command; verifies]

  let check_timing ~balance_check ~timed_balance_check ~account ~txn_amount
      ~txn_global_slot =
    (* calculations should track Transaction_logic.validate_timing *)
    let open Account.Poly in
    let open Account.Timing.As_record in
    let { is_timed
        ; initial_minimum_balance
        ; cliff_time
        ; vesting_period
        ; vesting_increment } =
      account.timing
    in
    let%bind before_or_at_cliff =
      Global_slot.Checked.(txn_global_slot <= cliff_time)
    in
    let int_of_field field =
      Snarky_integer.Integer.constant ~m
        (Bigint.of_field field |> Bigint.to_bignum_bigint)
    in
    let zero_int = int_of_field Field.zero in
    let balance_to_int balance =
      Snarky_integer.Integer.of_bits ~m @@ Balance.var_to_bits balance
    in
    let txn_amount_int =
      Snarky_integer.Integer.of_bits ~m @@ Amount.var_to_bits txn_amount
    in
    let balance_int = balance_to_int account.balance in
    let%bind curr_min_balance =
      let open Snarky_integer.Integer in
      let initial_minimum_balance_int =
        balance_to_int initial_minimum_balance
      in
      make_checked (fun () ->
          if_ ~m before_or_at_cliff ~then_:initial_minimum_balance_int
            ~else_:
              (let txn_global_slot_int =
                 Global_slot.Checked.to_integer txn_global_slot
               in
               let cliff_time_int =
                 Global_slot.Checked.to_integer cliff_time
               in
               let _, slot_diff =
                 subtract_unpacking_or_zero ~m txn_global_slot_int
                   cliff_time_int
               in
               let vesting_period_int =
                 Global_slot.Checked.to_integer vesting_period
               in
               let num_periods, _ = div_mod ~m slot_diff vesting_period_int in
               let vesting_increment_int =
                 Amount.var_to_bits vesting_increment |> of_bits ~m
               in
               let min_balance_decrement =
                 mul ~m num_periods vesting_increment_int
               in
               let _, min_balance_less_decrement =
                 subtract_unpacking_or_zero ~m initial_minimum_balance_int
                   min_balance_decrement
               in
               min_balance_less_decrement) )
    in
    let%bind `Underflow underflow, proposed_balance_int =
      make_checked (fun () ->
          Snarky_integer.Integer.subtract_unpacking_or_zero ~m balance_int
            txn_amount_int )
    in
    (* underflow indicates insufficient balance *)
    let%bind () = balance_check (Boolean.not underflow) in
    let%bind sufficient_timed_balance =
      make_checked (fun () ->
          Snarky_integer.Integer.(gte ~m proposed_balance_int curr_min_balance)
      )
    in
    let%bind () =
      let%bind ok = Boolean.(any [not is_timed; sufficient_timed_balance]) in
      timed_balance_check ok
    in
    let%bind is_timed_balance_zero =
      make_checked (fun () ->
          Snarky_integer.Integer.equal ~m curr_min_balance zero_int )
    in
    (* if current min balance is zero, then timing becomes untimed *)
    let%bind is_untimed = Boolean.((not is_timed) || is_timed_balance_zero) in
    let%map timing =
      Account.Timing.if_ is_untimed ~then_:Account.Timing.untimed_var
        ~else_:account.timing
    in
    (`Min_balance curr_min_balance, timing)

  let chain if_ b ~then_ ~else_ =
    let%bind then_ = then_ and else_ = else_ in
    if_ b ~then_ ~else_

  let%snarkydef apply_tagged_transaction
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      (type shifted)
      (shifted : (module Inner_curve.Checked.Shifted.S with type t = shifted))
      root pending_coinbase_stack_init pending_coinbase_stack_before
      pending_coinbase_after next_available_token state_body
      ({signer; signature; payload} as txn : Transaction_union.var) =
    let tag = payload.body.tag in
    let is_user_command = Transaction_union.Tag.Unpacked.is_user_command tag in
    let%bind () =
      [%with_label "Check transaction signature"]
        (check_signature shifted ~payload ~is_user_command ~signer ~signature)
    in
    let%bind signer_pk = Public_key.compress_var signer in
    let%bind () =
      [%with_label "Fee-payer must sign the transaction"]
        ((* TODO: Enable multi-sig. *)
         Public_key.Compressed.Checked.Assert.equal signer_pk
           payload.common.fee_payer_pk)
    in
    (* Compute transaction kind. *)
    let is_payment = Transaction_union.Tag.Unpacked.is_payment tag in
    let is_mint_tokens = Transaction_union.Tag.Unpacked.is_mint_tokens tag in
    let is_set_token_permissions =
      Transaction_union.Tag.Unpacked.is_set_token_permissions tag
    in
    let is_stake_delegation =
      Transaction_union.Tag.Unpacked.is_stake_delegation tag
    in
    let is_create_account =
      Transaction_union.Tag.Unpacked.is_create_account tag
    in
    let is_fee_transfer = Transaction_union.Tag.Unpacked.is_fee_transfer tag in
    let is_coinbase = Transaction_union.Tag.Unpacked.is_coinbase tag in
    let fee_token = payload.common.fee_token in
    let%bind fee_token_invalid =
      Token_id.(Checked.equal fee_token (var_of_t invalid))
    in
    let%bind fee_token_default =
      Token_id.(Checked.equal fee_token (var_of_t default))
    in
    let token = payload.body.token_id in
    let%bind token_invalid =
      Token_id.(Checked.equal token (var_of_t invalid))
    in
    let%bind token_default =
      Token_id.(Checked.equal token (var_of_t default))
    in
    let%bind () =
      Checked.all_unit
        [ [%with_label
            "Token_locked value is compatible with the transaction kind"]
            (Boolean.Assert.any
               [ Boolean.not payload.body.token_locked
               ; is_create_account
               ; is_set_token_permissions ])
        ; [%with_label "Token_locked cannot be used with the default token"]
            (Boolean.Assert.any
               [ Boolean.not payload.body.token_locked
               ; Boolean.not token_default ]) ]
    in
    let%bind () =
      [%with_label "Validate tokens"]
        (Checked.all_unit
           [ [%with_label "Fee token is valid"]
               Boolean.(Assert.is_true (not fee_token_invalid))
           ; [%with_label
               "Fee token is default or command allows non-default fee"]
               (Boolean.Assert.any
                  [ fee_token_default
                  ; is_payment
                  ; is_mint_tokens
                  ; is_set_token_permissions
                  ; is_stake_delegation
                  ; is_fee_transfer ])
           ; (* TODO: Remove this check and update the transaction snark once we
               have an exchange rate mechanism. See issue #4447.
            *)
             [%with_label "Fees in tokens disabled"]
               (Boolean.Assert.is_true fee_token_default)
           ; [%with_label "Token is valid or command allows invalid token"]
               Boolean.(Assert.any [not token_invalid; is_create_account])
           ; [%with_label
               "Token is default or command allows non-default token"]
               (Boolean.Assert.any
                  [ token_default
                  ; is_payment
                  ; is_create_account
                  ; is_mint_tokens
                  ; is_set_token_permissions
                    (* TODO: Enable this when fees in tokens are enabled. *)
                    (*; is_fee_transfer*) ])
           ; [%with_label
               "Token is non-default or command allows default token"]
               Boolean.(
                 Assert.any
                   [ not token_default
                   ; is_payment
                   ; is_stake_delegation
                   ; is_create_account
                   ; is_fee_transfer
                   ; is_coinbase ]) ])
    in
    let current_global_slot =
      Coda_state.Protocol_state.Body.consensus_state state_body
      |> Consensus.Data.Consensus_state.curr_global_slot_var
    in
    let%bind creating_new_token =
      Boolean.(is_create_account && token_invalid)
    in
    (* Query user command predicted failure/success. *)
    let%bind user_command_failure =
      User_command_failure.compute_as_prover ~constraint_constants
        ~txn_global_slot:current_global_slot ~creating_new_token
        ~next_available_token txn
    in
    let%bind user_command_fails =
      User_command_failure.any user_command_failure
    in
    let%bind next_available_token_after, token =
      let%bind token =
        Token_id.Checked.if_ creating_new_token ~then_:next_available_token
          ~else_:token
      in
      let%bind will_create_new_token =
        Boolean.(creating_new_token && not user_command_fails)
      in
      let%map next_available_token =
        Token_id.Checked.next_if next_available_token will_create_new_token
      in
      (next_available_token, token)
    in
    let fee = payload.common.fee in
    let receiver = Account_id.Checked.create payload.body.receiver_pk token in
    let source = Account_id.Checked.create payload.body.source_pk token in
    (* Information for the fee-payer. *)
    let nonce = payload.common.nonce in
    let fee_payer =
      Account_id.Checked.create payload.common.fee_payer_pk fee_token
    in
    let%bind () =
      [%with_label "Check slot validity"]
        ( Global_slot.Checked.(
            current_global_slot <= payload.common.valid_until)
        >>= Boolean.Assert.is_true )
    in
    (* Check coinbase stack. Protocol state body is pushed into the Pending
       coinbase stack once per block. For example, consider any two
       transactions in a block. Their pending coinbase stacks would be:

       transaction1: s1 -> t1 = s1+ protocol_state_body + maybe_coinbase
       transaction2: t1 -> t1 + maybe_another_coinbase
         (Note: protocol_state_body is not pushed again)

       However, for each transaction, we need to constrain the protocol state
       body. This is done is by using the stack ([init_stack]) without the
       current protocol state body, pushing the state body to it in every
       transaction snark and checking if it matches the target.
       We also need to constrain the source for the merges to work correctly.
       Basically,

       init_stack + protocol_state_body + maybe_coinbase = target
       AND
       init_stack = source || init_stack + protocol_state_body = source *)

    (* These are all the possible cases:

       Init_stack     Source                 Target 
      --------------------------------------------------------------
        i               i                       i + state
        i               i                       i + state + coinbase
        i               i + state               i + state
        i               i + state               i + state + coinbase
        i + coinbase    i + state + coinbase    i + state + coinbase
    *)
    let%bind () =
      [%with_label "Compute coinbase stack"]
        (let%bind state_body_hash =
           Coda_state.Protocol_state.Body.hash_checked state_body
         in
         let%bind pending_coinbase_stack_with_state =
           Pending_coinbase.Stack.Checked.push_state state_body_hash
             pending_coinbase_stack_init
         in
         let%bind computed_pending_coinbase_stack_after =
           let coinbase =
             (Account_id.Checked.public_key receiver, payload.body.amount)
           in
           let%bind stack' =
             Pending_coinbase.Stack.Checked.push_coinbase coinbase
               pending_coinbase_stack_with_state
           in
           Pending_coinbase.Stack.Checked.if_ is_coinbase ~then_:stack'
             ~else_:pending_coinbase_stack_with_state
         in
         [%with_label "Check coinbase stack"]
           (let%bind correct_coinbase_target_stack =
              Pending_coinbase.Stack.equal_var
                computed_pending_coinbase_stack_after pending_coinbase_after
            in
            let%bind valid_init_state =
              let%bind equal_source =
                Pending_coinbase.Stack.equal_var pending_coinbase_stack_init
                  pending_coinbase_stack_before
              in
              let%bind equal_source_with_state =
                Pending_coinbase.Stack.equal_var
                  pending_coinbase_stack_with_state
                  pending_coinbase_stack_before
              in
              Boolean.(equal_source || equal_source_with_state)
            in
            Boolean.Assert.all [correct_coinbase_target_stack; valid_init_state]))
    in
    (* Interrogate failure cases. This value is created without constraints;
       the failures should be checked against potential failures to ensure
       consistency.
    *)
    let%bind () =
      [%with_label "A failing user command is a user command"]
        Boolean.(Assert.any [is_user_command; not user_command_fails])
    in
    let predicate_deferred =
      (* Predicate check is to be performed later if this is true. *)
      is_create_account
    in
    let%bind predicate_result =
      let%bind is_own_account =
        Public_key.Compressed.Checked.equal payload.common.fee_payer_pk
          payload.body.source_pk
      in
      let predicate_result =
        (* TODO: Predicates. *)
        Boolean.false_
      in
      Boolean.(is_own_account || predicate_result)
    in
    let%bind () =
      [%with_label "Check predicate failure against predicted"]
        (let%bind predicate_failed =
           Boolean.((not predicate_result) && not predicate_deferred)
         in
         assert_r1cs
           (predicate_failed :> Field.Var.t)
           (is_user_command :> Field.Var.t)
           (user_command_failure.predicate_failed :> Field.Var.t))
    in
    let account_creation_amount =
      Amount.Checked.of_fee
        Fee.(var_of_t constraint_constants.account_creation_fee)
    in
    let%bind is_zero_fee =
      fee |> Fee.var_to_number |> Number.to_var
      |> Field.(Checked.equal (Var.constant zero))
    in
    let%bind can_create_fee_payer_account =
      (* Fee transfers and coinbases may create an account. We check the normal
         invariants to ensure that the account creation fee is paid.
      *)
      let%bind fee_may_be_charged =
        (* If the fee is zero, we do not create the account at all, so we allow
           this through. Otherwise, the fee must be the default.
        *)
        Boolean.(token_default || is_zero_fee)
      in
      Boolean.((not is_user_command) && fee_may_be_charged)
    in
    let%bind root_after_fee_payer_update =
      [%with_label "Update fee payer"]
        (Frozen_ledger_hash.modify_account_send
           ~depth:constraint_constants.ledger_depth root
           ~is_writeable:can_create_fee_payer_account fee_payer
           ~f:(fun ~is_empty_and_writeable account ->
             (* this account is:
               - the fee-payer for payments
               - the fee-payer for stake delegation
               - the fee-payer for account creation
               - the fee-payer for token minting
               - the fee-payer for setting token permissions
               - the fee-receiver for a coinbase
               - the second receiver for a fee transfer
             *)
             let%bind next_nonce =
               Account.Nonce.Checked.succ_if account.nonce is_user_command
             in
             let%bind () =
               [%with_label "Check fee nonce"]
                 (let%bind nonce_matches =
                    Account.Nonce.Checked.equal nonce account.nonce
                  in
                  Boolean.Assert.any
                    [Boolean.not is_user_command; nonce_matches])
             in
             let%bind receipt_chain_hash =
               let current = account.receipt_chain_hash in
               let%bind r = Receipt.Chain_hash.Checked.cons ~payload current in
               Receipt.Chain_hash.Checked.if_ is_user_command ~then_:r
                 ~else_:current
             in
             let%bind is_empty_and_writeable =
               (* If this is a coinbase with zero fee, do not create the
                  account, since the fee amount won't be enough to pay for it.
               *)
               Boolean.(is_empty_and_writeable && not is_zero_fee)
             in
             let%bind should_pay_to_create =
               (* Coinbases and fee transfers may create, or we may be creating
                  a new token account. These are mutually exclusive, so we can
                  encode this as a boolean.
               *)
               let%bind is_create_account =
                 Boolean.(is_create_account && not user_command_fails)
               in
               Boolean.(is_empty_and_writeable || is_create_account)
             in
             let%bind amount =
               [%with_label "Compute fee payer amount"]
                 (let fee_payer_amount =
                    let sgn = Sgn.Checked.neg_if_true is_user_command in
                    Amount.Signed.create
                      ~magnitude:(Amount.Checked.of_fee fee)
                      ~sgn
                  in
                  (* Account creation fee for fee transfers/coinbases. *)
                  let%bind account_creation_fee =
                    let%map magnitude =
                      Amount.Checked.if_ should_pay_to_create
                        ~then_:account_creation_amount
                        ~else_:Amount.(var_of_t zero)
                    in
                    Amount.Signed.create ~magnitude ~sgn:Sgn.Checked.neg
                  in
                  Amount.Signed.Checked.(
                    add fee_payer_amount account_creation_fee))
             in
             let txn_global_slot = current_global_slot in
             let%bind `Min_balance _, timing =
               [%with_label "Check fee payer timing"]
                 (let%bind txn_amount =
                    Amount.Checked.if_
                      (Sgn.Checked.is_neg amount.sgn)
                      ~then_:amount.magnitude
                      ~else_:Amount.(var_of_t zero)
                  in
                  let balance_check ok =
                    [%with_label "Check fee payer balance"]
                      (Boolean.Assert.is_true ok)
                  in
                  let timed_balance_check ok =
                    [%with_label "Check fee payer timed balance"]
                      (Boolean.Assert.is_true ok)
                  in
                  check_timing ~balance_check ~timed_balance_check ~account
                    ~txn_amount ~txn_global_slot)
             in
             let%bind balance =
               [%with_label "Check payer balance"]
                 (Balance.Checked.add_signed_amount account.balance amount)
             in
             let%map public_key =
               Public_key.Compressed.Checked.if_ is_empty_and_writeable
                 ~then_:(Account_id.Checked.public_key fee_payer)
                 ~else_:account.public_key
             and token_id =
               Token_id.Checked.if_ is_empty_and_writeable
                 ~then_:(Account_id.Checked.token_id fee_payer)
                 ~else_:account.token_id
             and delegate =
               Public_key.Compressed.Checked.if_ is_empty_and_writeable
                 ~then_:(Account_id.Checked.public_key fee_payer)
                 ~else_:account.delegate
             in
             { Account.Poly.balance
             ; public_key
             ; token_id
             ; token_permissions= account.token_permissions
             ; nonce= next_nonce
             ; receipt_chain_hash
             ; delegate
             ; voting_for= account.voting_for
             ; timing } ))
    in
    let%bind receiver_increase =
      (* - payments:         payload.body.amount
         - stake delegation: 0
         - account creation: 0
         - token minting:    payload.body.amount
         - set token perms:  0
         - coinbase:         payload.body.amount - payload.common.fee
         - fee transfer:     payload.body.amount
      *)
      [%with_label "Compute receiver increase"]
        (let%bind base_amount =
           let%bind zero_transfer =
             Boolean.any
               [ is_stake_delegation
               ; is_create_account
               ; is_set_token_permissions ]
           in
           Amount.Checked.if_ zero_transfer
             ~then_:(Amount.var_of_t Amount.zero)
             ~else_:payload.body.amount
         in
         (* The fee for entering the coinbase transaction is paid up front. *)
         let%bind coinbase_receiver_fee =
           Amount.Checked.if_ is_coinbase
             ~then_:(Amount.Checked.of_fee fee)
             ~else_:(Amount.var_of_t Amount.zero)
         in
         Amount.Checked.sub base_amount coinbase_receiver_fee)
    in
    let%bind root_after_receiver_update =
      [%with_label "Update receiver"]
        (Frozen_ledger_hash.modify_account_recv
           ~depth:constraint_constants.ledger_depth root_after_fee_payer_update
           receiver ~f:(fun ~is_empty_and_writeable account ->
             (* this account is:
               - the receiver for payments
               - the delegated-to account for stake delegation
               - the created account for an account creation
               - the receiver for minted tokens
               - the modified account for setting token permissions
               - the receiver for a coinbase
               - the first receiver for a fee transfer
             *)
             let%bind is_empty_failure =
               let%bind must_not_be_empty =
                 Boolean.any
                   [ is_stake_delegation
                   ; is_mint_tokens
                   ; is_set_token_permissions ]
               in
               Boolean.(is_empty_and_writeable && must_not_be_empty)
             in
             let%bind () =
               [%with_label "Receiver existence failure matches predicted"]
                 (Boolean.Assert.( = ) is_empty_failure
                    user_command_failure.receiver_not_present)
             in
             let%bind () =
               [%with_label "Receiver creation failure matches predicted"]
                 (let%bind is_nonempty_creating =
                    Boolean.((not is_empty_and_writeable) && is_create_account)
                  in
                  Boolean.Assert.( = ) is_nonempty_creating
                    user_command_failure.receiver_exists)
             in
             let is_empty_and_writeable =
               (* is_empty_and_writable && not is_empty_failure *)
               Boolean.Unsafe.of_cvar
               @@ Field.Var.(
                    sub (is_empty_and_writeable :> t) (is_empty_failure :> t))
             in
             let%bind should_pay_to_create =
               Boolean.(is_empty_and_writeable && not is_create_account)
             in
             let%bind () =
               [%with_label
                 "Check whether creation fails due to a non-default token"]
                 (let%bind token_should_not_create =
                    Boolean.(should_pay_to_create && Boolean.not token_default)
                  in
                  let%bind token_cannot_create =
                    Boolean.(token_should_not_create && is_user_command)
                  in
                  let%bind () =
                    [%with_label
                      "Check that account creation is paid in the default \
                       token for non-user-commands"]
                      ((* This expands to
                          [token_should_not_create =
                            token_should_not_create && is_user_command]
                          which is
                          - [token_should_not_create = token_should_not_create]
                            (ie. always satisfied) for user commands
                          - [token_should_not_create = false] for coinbases/fee
                            transfers.
                       *)
                       Boolean.Assert.( = ) token_should_not_create
                         token_cannot_create)
                  in
                  Boolean.Assert.( = ) token_cannot_create
                    user_command_failure.token_cannot_create)
             in
             let%bind balance =
               (* [receiver_increase] will be zero in the stake delegation
                  case.
               *)
               let%bind receiver_amount =
                 let%bind account_creation_amount =
                   Amount.Checked.if_ should_pay_to_create
                     ~then_:account_creation_amount
                     ~else_:Amount.(var_of_t zero)
                 in
                 let%bind amount_for_new_account, `Underflow underflow =
                   Amount.Checked.sub_flagged receiver_increase
                     account_creation_amount
                 in
                 let%bind () =
                   [%with_label
                     "Receiver creation fee failure matches predicted"]
                     (Boolean.Assert.( = ) underflow
                        user_command_failure.amount_insufficient_to_create)
                 in
                 Currency.Amount.Checked.if_ user_command_fails
                   ~then_:Amount.(var_of_t zero)
                   ~else_:amount_for_new_account
               in
               (* TODO: This case can be contrived using minted tokens, handle
                  it in the transaction logic and add a case for it to
                  [User_command_failure.t].
                *)
               Balance.Checked.(account.balance + receiver_amount)
             in
             let%bind is_empty_and_writeable =
               (* Do not create a new account if the user command will fail. *)
               Boolean.(is_empty_and_writeable && not user_command_fails)
             in
             let%bind may_delegate =
               (* Only default tokens may participate in delegation. *)
               Boolean.(is_empty_and_writeable && token_default)
             in
             let%map delegate =
               Public_key.Compressed.Checked.if_ may_delegate
                 ~then_:(Account_id.Checked.public_key receiver)
                 ~else_:account.delegate
             and public_key =
               Public_key.Compressed.Checked.if_ is_empty_and_writeable
                 ~then_:(Account_id.Checked.public_key receiver)
                 ~else_:account.public_key
             and token_id =
               Token_id.Checked.if_ is_empty_and_writeable ~then_:token
                 ~else_:account.token_id
             and token_owner =
               Boolean.if_ is_empty_and_writeable ~then_:creating_new_token
                 ~else_:account.token_permissions.token_owner
             and token_locked =
               let%bind may_set_token_permissions =
                 Boolean.(is_set_token_permissions && not user_command_fails)
               in
               let%bind update =
                 Boolean.(is_empty_and_writeable || may_set_token_permissions)
               in
               Boolean.if_ update ~then_:payload.body.token_locked
                 ~else_:account.token_permissions.token_locked
             in
             { Account.Poly.balance
             ; public_key
             ; token_id
             ; token_permissions= {Token_permissions.token_owner; token_locked}
             ; nonce= account.nonce
             ; receipt_chain_hash= account.receipt_chain_hash
             ; delegate
             ; voting_for= account.voting_for
             ; timing= account.timing } ))
    in
    let%bind fee_payer_is_source = Account_id.Checked.equal fee_payer source in
    let%bind root_after_source_update =
      [%with_label "Update source"]
        (Frozen_ledger_hash.modify_account_send
           ~depth:constraint_constants.ledger_depth
           ~is_writeable:
             (* [modify_account_send] does this failure check for us. *)
             user_command_failure.source_not_present root_after_receiver_update
           source ~f:(fun ~is_empty_and_writeable account ->
             (* this account is:
               - the source for payments
               - the delegator for stake delegation
               - the token owner for account creation
               - the token owner for token minting
               - the fee-receiver for a coinbase
               - the second receiver for a fee transfer
             *)
             let%bind () =
               [%with_label "Check source presence failure matches predicted"]
                 (Boolean.Assert.( = ) is_empty_and_writeable
                    user_command_failure.source_not_present)
             in
             let%bind () =
               [%with_label
                 "Check source failure cases do not apply when fee-payer is \
                  source"]
                 (let num_failures =
                    let open Field.Var in
                    add
                      (user_command_failure.source_insufficient_balance :> t)
                      (user_command_failure.source_bad_timing :> t)
                  in
                  let not_fee_payer_is_source =
                    (Boolean.not fee_payer_is_source :> Field.Var.t)
                  in
                  (* Equivalent to:
                    if fee_payer_is_source then
                      num_failures = 0
                    else
                      num_failures = num_failures
                  *)
                  assert_r1cs not_fee_payer_is_source num_failures num_failures)
             in
             let%bind amount =
               (* Only payments should affect the balance at this stage. *)
               if_ is_payment ~typ:Amount.typ ~then_:payload.body.amount
                 ~else_:Amount.(var_of_t zero)
             in
             let txn_global_slot = current_global_slot in
             let%bind `Min_balance _, timing =
               [%with_label "Check source timing"]
                 (let balance_check ok =
                    [%with_label
                      "Check source balance failure matches predicted"]
                      (Boolean.Assert.( = ) ok
                         (Boolean.not
                            user_command_failure.source_insufficient_balance))
                  in
                  let timed_balance_check ok =
                    [%with_label
                      "Check source timed balance failure matches predicted"]
                      (let%bind ok =
                         Boolean.(
                           ok
                           && not
                                user_command_failure
                                  .source_insufficient_balance)
                       in
                       Boolean.Assert.( = ) ok
                         (Boolean.not user_command_failure.source_bad_timing))
                  in
                  check_timing ~balance_check ~timed_balance_check ~account
                    ~txn_amount:amount ~txn_global_slot)
             in
             let%bind balance, `Underflow underflow =
               Balance.Checked.sub_amount_flagged account.balance amount
             in
             let%bind () =
               (* TODO: Remove the redundancy in balance calculation between
                  here and [check_timing].
               *)
               [%with_label "Check source balance failure matches predicted"]
                 (Boolean.Assert.( = ) underflow
                    user_command_failure.source_insufficient_balance)
             in
             let%bind () =
               [%with_label "Check not_token_owner failure matches predicted"]
                 (let%bind token_owner_ok =
                    let%bind command_needs_token_owner =
                      Boolean.any
                        [ is_create_account
                        ; is_mint_tokens
                        ; is_set_token_permissions ]
                    in
                    Boolean.(
                      any
                        [ account.token_permissions.token_owner
                        ; token_default
                        ; not command_needs_token_owner ])
                  in
                  Boolean.(
                    Assert.( = ) (not token_owner_ok)
                      user_command_failure.not_token_owner))
             in
             let%bind () =
               [%with_label "Check that token_auth failure matches predicted"]
                 (let%bind token_auth_needed =
                    Field.Checked.equal
                      (payload.body.token_locked :> Field.Var.t)
                      (account.token_permissions.token_locked :> Field.Var.t)
                    >>| Boolean.not
                  in
                  let%bind token_auth_failed =
                    Boolean.(
                      all
                        [ token_auth_needed
                        ; not token_default
                        ; is_create_account
                        ; not creating_new_token
                        ; not predicate_result ])
                  in
                  Boolean.Assert.( = ) token_auth_failed
                    user_command_failure.token_auth)
             in
             let%bind () =
               [%with_label "Check account_disabled failure matches predicted"]
                 (let%bind account_disabled =
                    Boolean.(
                      all
                        [ not account.token_permissions.token_owner
                        ; account.token_permissions.token_locked
                        ; is_user_command ])
                  in
                  Boolean.Assert.( = ) account_disabled
                    user_command_failure.account_disabled)
             in
             let%map delegate =
               Public_key.Compressed.Checked.if_ is_stake_delegation
                 ~then_:(Account_id.Checked.public_key receiver)
                 ~else_:account.delegate
             in
             (* NOTE: Technically we update the account here even in the case
                of [user_command_fails], but we throw the resulting hash away
                in [final_root] below, so it shouldn't matter.
             *)
             { Account.Poly.balance
             ; public_key= account.public_key
             ; token_id= account.token_id
             ; token_permissions= account.token_permissions
             ; nonce= account.nonce
             ; receipt_chain_hash= account.receipt_chain_hash
             ; delegate
             ; voting_for= account.voting_for
             ; timing } ))
    in
    let%bind fee_excess =
      (* - payments:         payload.common.fee
         - stake delegation: payload.common.fee
         - account creation: payload.common.fee
         - token minting:    payload.common.fee
         - coinbase:         0 (fee already paid above)
         - fee transfer:     - payload.body.amount - payload.common.fee
      *)
      let open Amount in
      chain Signed.Checked.if_ is_coinbase
        ~then_:(return (Signed.Checked.of_unsigned (var_of_t zero)))
        ~else_:
          (let user_command_excess =
             Signed.Checked.of_unsigned (Checked.of_fee payload.common.fee)
           in
           let%bind fee_transfer_excess =
             let%map magnitude =
               Checked.(payload.body.amount + of_fee payload.common.fee)
             in
             Signed.create ~magnitude ~sgn:Sgn.Checked.neg
           in
           Signed.Checked.if_ is_fee_transfer ~then_:fee_transfer_excess
             ~else_:user_command_excess)
    in
    let%bind supply_increase =
      Amount.Checked.if_ is_coinbase ~then_:payload.body.amount
        ~else_:Amount.(var_of_t zero)
    in
    let%map final_root =
      (* Ensure that only the fee-payer was charged if this was an invalid user
         command.
      *)
      Frozen_ledger_hash.if_ user_command_fails
        ~then_:root_after_fee_payer_update ~else_:root_after_source_update
    in
    (final_root, fee_excess, supply_increase, next_available_token_after)

  (* Someday:
   write the following soundness tests:
   - apply a transaction where the signature is incorrect
   - apply a transaction where the sender does not have enough money in their account
   - apply a transaction and stuff in the wrong target hash
    *)

  module Prover_state = struct
    type t =
      { state1: Frozen_ledger_hash.t
      ; state2: Frozen_ledger_hash.t
      ; pending_coinbase_stack_state: Pending_coinbase_stack_state.t
      ; sok_digest: Sok_message.Digest.t }
    [@@deriving fields]
  end

  (* spec for [main top_hash]:
   constraints pass iff
   there exist
      l1 : Frozen_ledger_hash.t,
      l2 : Frozen_ledger_hash.t,
      fee_excess : Amount.Signed.t,
      supply_increase : Amount.t
      pending_coinbase_stack_state: Pending_coinbase_stack_state.t
      t : Tagged_transaction.t
   such that
   H(l1, l2, pending_coinbase_stack_state.source, pending_coinbase_stack_state.target, fee_excess, supply_increase) = top_hash,
   applying [t] to ledger with merkle hash [l1] results in ledger with merkle hash [l2]. *)
  let%snarkydef main ~constraint_constants top_hash =
    let%bind (module Shifted) = Tick.Inner_curve.Checked.Shifted.create () in
    let%bind root_before =
      exists' Frozen_ledger_hash.typ ~f:Prover_state.state1
    in
    let%bind t =
      with_label __LOC__
        (exists Transaction_union.typ ~request:(As_prover.return Transaction))
    in
    let%bind pending_coinbase_before =
      exists' Pending_coinbase.Stack.typ ~f:(fun s ->
          (Prover_state.pending_coinbase_stack_state s).source )
    in
    let%bind pending_coinbase_after =
      exists' Pending_coinbase.Stack.typ ~f:(fun s ->
          (Prover_state.pending_coinbase_stack_state s).target )
    in
    let%bind pending_coinbase_init =
      exists Pending_coinbase.Stack.typ ~request:(As_prover.return Init_stack)
    in
    let%bind next_available_token_before =
      exists Token_id.typ ~request:(As_prover.return Next_available_token)
    in
    let%bind state_body =
      exists
        (Coda_state.Protocol_state.Body.typ ~constraint_constants)
        ~request:(As_prover.return State_body)
    in
    let%bind ( root_after
             , fee_excess
             , supply_increase
             , next_available_token_after ) =
      apply_tagged_transaction ~constraint_constants
        (module Shifted)
        root_before pending_coinbase_init pending_coinbase_before
        pending_coinbase_after next_available_token_before state_body t
    in
    let%bind fee_excess =
      (* Use the default token for the fee excess if it is zero.
         This matches the behaviour of [Fee_excess.rebalance], which allows
         [verify_complete_merge] to verify a proof without knowledge of the
         particular fee tokens used.
      *)
      let%bind fee_excess_zero =
        Amount.equal_var fee_excess.magnitude Amount.(var_of_t zero)
      in
      let%map fee_token_l =
        Token_id.Checked.if_ fee_excess_zero
          ~then_:Token_id.(var_of_t default)
          ~else_:t.payload.common.fee_token
      in
      { Fee_excess.fee_token_l
      ; fee_excess_l= Signed_poly.map ~f:Amount.Checked.to_fee fee_excess
      ; fee_token_r= Token_id.(var_of_t default)
      ; fee_excess_r= Fee.Signed.(Checked.constant zero) }
    in
    let%map () =
      [%with_label "Check that the computed hash matches the input hash"]
        (let%bind sok_digest =
           [%with_label "Fetch the sok_digest"]
             (exists' Sok_message.Digest.typ ~f:Prover_state.sok_digest)
         in
         let%bind input =
           Statement.With_sok.Checked.to_field_elements
             { source= root_before
             ; target= root_after
             ; fee_excess
             ; next_available_token_before
             ; next_available_token_after
             ; supply_increase
             ; pending_coinbase_stack_state=
                 { source= pending_coinbase_before
                 ; target= pending_coinbase_after }
             ; proof_type= ()
             ; sok_digest }
         in
         [%with_label "Compare the hashes"]
           ( make_checked (fun () ->
                 Random_oracle.Checked.(
                   hash ~init:Hash_prefix.base_snark input) )
           >>= Field.Checked.Assert.equal top_hash ))
    in
    ()

  let transaction_union_handler handler (transaction : Transaction_union.t)
      (state_body : Coda_state.Protocol_state.Body.Value.t)
      (init_stack : Pending_coinbase.Stack.t)
      (next_available_token : Token_id.t) : Snarky.Request.request -> _ =
   fun (With {request; respond} as r) ->
    match request with
    | Transaction ->
        respond (Provide transaction)
    | State_body ->
        respond (Provide state_body)
    | Init_stack ->
        respond (Provide init_stack)
    | Next_available_token ->
        respond (Provide next_available_token)
    | _ ->
        handler r

  let create_keys () =
    generate_keypair
      (main
         ~constraint_constants:Genesis_constants.Constraint_constants.compiled)
      ~exposing:(tick_input ())

  let transaction_union_proof ?(preeval = false) ~constraint_constants
      ~proving_key sok_digest state1 state2 init_stack
      pending_coinbase_stack_state next_available_token_before
      next_available_token_after (transaction : Transaction_union.t) state_body
      handler =
    if preeval then failwith "preeval currently disabled" ;
    let prover_state : Prover_state.t =
      {state1; state2; sok_digest; pending_coinbase_stack_state}
    in
    let handler =
      transaction_union_handler handler transaction state_body init_stack
        next_available_token_before
    in
    let main top_hash = handle (main ~constraint_constants top_hash) handler in
    let statement : Statement.With_sok.t =
      { source= state1
      ; target= state2
      ; supply_increase= Transaction_union.supply_increase transaction
      ; pending_coinbase_stack_state
      ; fee_excess= Transaction_union.fee_excess transaction
      ; next_available_token_before
      ; next_available_token_after
      ; proof_type= ()
      ; sok_digest }
    in
    let top_hash = base_top_hash statement in
    (top_hash, prove proving_key (tick_input ()) prover_state main top_hash)

  let cached =
    let load =
      let open Cached.Let_syntax in
      let%map verification =
        Cached.component ~label:"transaction_snark_base_verification"
          ~f:Keypair.vk
          (module Verification_key)
      and proving =
        Cached.component ~label:"transaction_snark_base_proving" ~f:Keypair.pk
          (module Proving_key)
      in
      (verification, {proving with value= ()})
    in
    Cached.Spec.create ~load ~name:"transaction-snark base keys"
      ~autogen_path:Cache_dir.autogen_path
      ~manual_install_path:Cache_dir.manual_install_path
      ~brew_install_path:Cache_dir.brew_install_path
      ~s3_install_path:Cache_dir.s3_install_path
      ~digest_input:(fun x ->
        Md5.to_hex (R1CS_constraint_system.digest (Lazy.force x)) )
      ~input:
        ( lazy
          (constraint_system ~exposing:(tick_input ())
             (main
                ~constraint_constants:
                  Genesis_constants.Constraint_constants.compiled)) )
      ~create_env:(fun x -> Keypair.generate (Lazy.force x))
end

module Transition_data = struct
  type t =
    { proof: Proof_type.t * Tock_backend.Proof.t
    ; supply_increase: Amount.t
    ; fee_excess: Fee_excess.t
    ; sok_digest: Sok_message.Digest.t
    ; pending_coinbase_stack_state: Pending_coinbase_stack_state.t }
  [@@deriving fields]
end

module Merge = struct
  open Tick
  open Let_syntax

  module Prover_state = struct
    type t =
      { tock_vk: Tock_backend.Verification_key.t
      ; sok_digest: Sok_message.Digest.t
      ; ledger_hash1: Frozen_ledger_hash.t
      ; ledger_hash2: Frozen_ledger_hash.t
      ; transition12: Transition_data.t
      ; ledger_hash3: Frozen_ledger_hash.t
      ; transition23: Transition_data.t
      ; next_available_token1: Token_id.t
      ; next_available_token2: Token_id.t
      ; next_available_token3: Token_id.t
      ; pending_coinbase_stack1: Pending_coinbase.Stack.t
      ; pending_coinbase_stack2: Pending_coinbase.Stack.t
      ; pending_coinbase_stack3: Pending_coinbase.Stack.t
      ; pending_coinbase_stack4: Pending_coinbase.Stack.t }
    [@@deriving fields]
  end

  let input = tick_input

  let wrap_input_size = Tock.Data_spec.size wrap_input

  module Verifier = Tick.Verifier

  let hash_state_if b ~then_ ~else_ =
    make_checked (fun () ->
        Random_oracle.State.map2 then_ else_ ~f:(fun then_ else_ ->
            Run.Field.if_ b ~then_ ~else_ ) )

  (* spec for [verify_transition tock_vk proof_field s1 s2]:
     returns a bool which is true iff
     there is a snark proving making tock_vk
     accept on one of [ H(s1, s2, excess); H(s1, s2, excess, tock_vk) ] *)
  let%snarkydef verify_transition tock_vk tock_vk_precomp wrap_vk_hash_state
      get_transition_data s1 s2 ~pending_coinbase_stack1
      ~pending_coinbase_stack2 supply_increase ~fee_excess
      ~next_available_token_before ~next_available_token_after =
    let%bind is_base =
      let get_type s = get_transition_data s |> Transition_data.proof |> fst in
      with_label __LOC__
        (exists' Boolean.typ ~f:(fun s -> Proof_type.is_base (get_type s)))
    in
    let%bind sok_digest =
      exists' Sok_message.Digest.typ
        ~f:(Fn.compose Transition_data.sok_digest get_transition_data)
    in
    let%bind top_hash_init =
      hash_state_if is_base
        ~then_:
          (Random_oracle.State.map ~f:Run.Field.constant Hash_prefix.base_snark)
        ~else_:wrap_vk_hash_state
    in
    let%bind input =
      let%bind input =
        Statement.With_sok.Checked.to_field_elements
          { source= s1
          ; target= s2
          ; fee_excess
          ; next_available_token_before
          ; next_available_token_after
          ; supply_increase
          ; pending_coinbase_stack_state=
              {source= pending_coinbase_stack1; target= pending_coinbase_stack2}
          ; proof_type= ()
          ; sok_digest }
      in
      make_checked (fun () ->
          Random_oracle.Checked.(digest (update ~state:top_hash_init input)) )
      >>= Wrap_input.Checked.tick_field_to_scalars
    in
    let%bind proof =
      exists Verifier.Proof.typ
        ~compute:
          As_prover.(
            map get_state ~f:(fun s ->
                get_transition_data s |> Transition_data.proof |> snd
                |> Verifier.proof_of_backend_proof ))
    in
    Verifier.verify tock_vk tock_vk_precomp input proof

  (* spec for [main top_hash]:
     constraints pass iff
     there exist digest, s1, s3, fee_excess, supply_increase pending_coinbase_stack12.source, pending_coinbase_stack23.target, tock_vk such that
     H(digest,s1, s3, pending_coinbase_stack12.source, pending_coinbase_stack23.target, fee_excess, supply_increase, tock_vk) = top_hash,
     verify_transition tock_vk _ s1 s2 pending_coinbase_stack12.source, pending_coinbase_stack12.target is true
     verify_transition tock_vk _ s2 s3 pending_coinbase_stack23.source, pending_coinbase_stack23.target is true
  *)
  let%snarkydef main (top_hash : Random_oracle.Checked.Digest.t) =
    let%bind tock_vk =
      exists' (Verifier.Verification_key.typ ~input_size:wrap_input_size)
        ~f:(fun {Prover_state.tock_vk; _} -> Verifier.vk_of_backend_vk tock_vk
      )
    and s1 = exists' Frozen_ledger_hash.typ ~f:Prover_state.ledger_hash1
    and s2 = exists' Frozen_ledger_hash.typ ~f:Prover_state.ledger_hash2
    and s3 = exists' Frozen_ledger_hash.typ ~f:Prover_state.ledger_hash3
    and fee_excess12 =
      exists' Fee_excess.typ
        ~f:(Fn.compose Transition_data.fee_excess Prover_state.transition12)
    and fee_excess23 =
      exists' Fee_excess.typ
        ~f:(Fn.compose Transition_data.fee_excess Prover_state.transition23)
    and supply_increase12 =
      exists' Amount.typ
        ~f:
          (Fn.compose Transition_data.supply_increase Prover_state.transition12)
    and supply_increase23 =
      exists' Amount.typ
        ~f:
          (Fn.compose Transition_data.supply_increase Prover_state.transition23)
    and next_available_token1 =
      exists' Token_id.typ ~f:Prover_state.next_available_token1
    and next_available_token2 =
      exists' Token_id.typ ~f:Prover_state.next_available_token2
    and next_available_token3 =
      exists' Token_id.typ ~f:Prover_state.next_available_token3
    and pending_coinbase1 =
      exists' Pending_coinbase.Stack.typ
        ~f:Prover_state.pending_coinbase_stack1
    and pending_coinbase2 =
      exists' Pending_coinbase.Stack.typ
        ~f:Prover_state.pending_coinbase_stack2
    and pending_coinbase3 =
      exists' Pending_coinbase.Stack.typ
        ~f:Prover_state.pending_coinbase_stack3
    and pending_coinbase4 =
      exists' Pending_coinbase.Stack.typ
        ~f:Prover_state.pending_coinbase_stack4
    in
    let%bind () =
      with_label __LOC__
        (let%bind valid_pending_coinbase_stack_transition =
           Pending_coinbase.Stack.Checked.check_merge
             ~transition1:(pending_coinbase1, pending_coinbase2)
             ~transition2:(pending_coinbase3, pending_coinbase4)
         in
         Boolean.Assert.is_true valid_pending_coinbase_stack_transition)
    in
    let%bind wrap_vk_hash_state =
      make_checked (fun () ->
          Random_oracle.(
            Checked.update
              ~state:
                (State.map Hash_prefix_states.merge_snark ~f:Run.Field.constant)
              (Verifier.Verification_key.to_field_elements tock_vk)) )
    in
    let%bind tock_vk_precomp =
      Verifier.Verification_key.Precomputation.create tock_vk
    in
    let%bind () =
      [%with_label "Check top hash"]
        (let%bind fee_excess =
           Fee_excess.combine_checked fee_excess12 fee_excess23
         in
         let%bind supply_increase =
           Amount.Checked.add supply_increase12 supply_increase23
         in
         let%bind input =
           let%bind sok_digest =
             exists' Sok_message.Digest.typ ~f:Prover_state.sok_digest
           in
           let%bind input =
             Statement.With_sok.Checked.to_field_elements
               { source= s1
               ; target= s3
               ; fee_excess
               ; next_available_token_before= next_available_token1
               ; next_available_token_after= next_available_token3
               ; supply_increase
               ; pending_coinbase_stack_state=
                   {source= pending_coinbase1; target= pending_coinbase4}
               ; proof_type= ()
               ; sok_digest }
           in
           make_checked (fun () ->
               Random_oracle.Checked.(
                 digest (update ~state:wrap_vk_hash_state input)) )
         in
         Field.Checked.Assert.equal top_hash input)
    and verify_12 =
      [%with_label "Verify left transition"]
        (verify_transition tock_vk tock_vk_precomp wrap_vk_hash_state
           Prover_state.transition12 s1 s2
           ~pending_coinbase_stack1:pending_coinbase1
           ~pending_coinbase_stack2:pending_coinbase2 supply_increase12
           ~fee_excess:fee_excess12
           ~next_available_token_before:next_available_token1
           ~next_available_token_after:next_available_token2)
    and verify_23 =
      [%with_label "Verify right transition"]
        (verify_transition tock_vk tock_vk_precomp wrap_vk_hash_state
           Prover_state.transition23 s2 s3
           ~pending_coinbase_stack1:pending_coinbase3
           ~pending_coinbase_stack2:pending_coinbase4 supply_increase23
           ~fee_excess:fee_excess23
           ~next_available_token_before:next_available_token2
           ~next_available_token_after:next_available_token3)
    in
    Boolean.Assert.all [verify_12; verify_23]

  let create_keys () = generate_keypair ~exposing:(input ()) main

  let cached =
    let load =
      let open Cached.Let_syntax in
      let%map verification =
        Cached.component ~label:"transaction_snark_merge_verification"
          ~f:Keypair.vk
          (module Verification_key)
      and proving =
        Cached.component ~label:"transaction_snark_merge_proving" ~f:Keypair.pk
          (module Proving_key)
      in
      (verification, {proving with value= ()})
    in
    Cached.Spec.create ~load ~name:"transaction-snark merge keys"
      ~autogen_path:Cache_dir.autogen_path
      ~manual_install_path:Cache_dir.manual_install_path
      ~brew_install_path:Cache_dir.brew_install_path
      ~s3_install_path:Cache_dir.s3_install_path
      ~digest_input:(fun x ->
        Md5.to_hex (R1CS_constraint_system.digest (Lazy.force x)) )
      ~input:(lazy (constraint_system ~exposing:(input ()) main))
      ~create_env:(fun x -> Keypair.generate (Lazy.force x))
end

module Verification = struct
  module Keys = Verification_keys

  module type S = sig
    val verify : (t * Sok_message.t) list -> bool

    val verify_against_digest : t -> bool

    val verify_complete_merge :
         Sok_message.Digest.Checked.t
      -> Frozen_ledger_hash.var
      -> Frozen_ledger_hash.var
      -> Pending_coinbase.Stack.var
      -> Pending_coinbase.Stack.var
      -> Currency.Amount.var
      -> Token_id.var
      -> Token_id.var
      -> (Tock.Proof.t, 's) Tick.As_prover.t
      -> (Tick.Boolean.var, 's) Tick.Checked.t
  end

  module Make (K : sig
    val keys : Keys.t
  end) =
  struct
    open K

    let wrap_vk_state =
      Random_oracle.update ~state:Hash_prefix.merge_snark
        Snark_params.Tick.Verifier.(
          let vk = vk_of_backend_vk keys.wrap in
          let g1 = Tick.Inner_curve.to_affine_exn in
          let g2 = Tick.Pairing.G2.Unchecked.to_affine_exn in
          Verification_key.to_field_elements
            { vk with
              query_base= g1 vk.query_base
            ; query= List.map ~f:g1 vk.query
            ; delta= g2 vk.delta })

    (* someday: Reorganize this module so that the inputs are separated from the proof. *)
    let verify_against_digest
        { source
        ; target
        ; proof
        ; proof_type
        ; fee_excess
        ; next_available_token_before
        ; next_available_token_after
        ; sok_digest
        ; supply_increase
        ; pending_coinbase_stack_state } =
      let (stmt : Statement.With_sok.t) =
        { source
        ; target
        ; proof_type= ()
        ; fee_excess
        ; next_available_token_before
        ; next_available_token_after
        ; sok_digest
        ; supply_increase
        ; pending_coinbase_stack_state }
      in
      let input =
        match proof_type with
        | `Base ->
            base_top_hash stmt
        | `Merge ->
            merge_top_hash wrap_vk_state stmt
      in
      Tock.verify proof keys.wrap wrap_input (Wrap_input.of_tick_field input)

    let verify_one t ~message =
      Sok_message.Digest.equal t.sok_digest (Sok_message.digest message)
      && verify_against_digest t

    let verify = List.for_all ~f:(fun (t, m) -> verify_one t ~message:m)

    (* spec for [verify_merge s1 s2 _]:
      Returns a boolean which is true if there exists a tock proof proving
      (against the wrap verification key) H(s1, s2, Amount.Signed.zero, wrap_vk).
      This in turn should only happen if there exists a tick proof proving
      (against the merge verification key) H(s1, s2, Amount.Signed.zero, wrap_vk).

      We precompute the parts of the pedersen involving wrap_vk and
      Amount.Signed.zero outside the SNARK since this saves us many constraints.
    *)

    let wrap_vk = Merge.Verifier.(constant_vk (vk_of_backend_vk keys.wrap))

    let wrap_precomp =
      Merge.Verifier.(
        Verification_key.Precomputation.create_constant
          (vk_of_backend_vk keys.wrap))

    let verify_complete_merge sok_digest s1 s2
        (pending_coinbase_stack1 : Pending_coinbase.Stack.var)
        (pending_coinbase_stack2 : Pending_coinbase.Stack.var) supply_increase
        next_available_token_before next_available_token_after get_proof =
      let open Tick in
      let%bind top_hash =
        let%bind input =
          Statement.With_sok.Checked.to_field_elements
            { source= s1
            ; target= s2
            ; fee_excess= Fee_excess.(var_of_t empty)
            ; next_available_token_before
            ; next_available_token_after
            ; supply_increase
            ; pending_coinbase_stack_state=
                { source= pending_coinbase_stack1
                ; target= pending_coinbase_stack2 }
            ; proof_type= ()
            ; sok_digest }
        in
        make_checked (fun () ->
            Random_oracle.Checked.(
              digest
                (update
                   ~state:
                     (Random_oracle.State.map wrap_vk_state
                        ~f:Run.Field.constant)
                   input)) )
      in
      let%bind input = Wrap_input.Checked.tick_field_to_scalars top_hash in
      let%map result =
        let%bind proof =
          exists Merge.Verifier.Proof.typ
            ~compute:
              (As_prover.map get_proof ~f:Merge.Verifier.proof_of_backend_proof)
        in
        Merge.Verifier.verify wrap_vk wrap_precomp input proof
      in
      result
  end
end

module Wrap (Vk : sig
  val merge : Tick.Verification_key.t

  val base : Tick.Verification_key.t
end) =
struct
  open Tock
  module Verifier = Tock.Groth_verifier

  let merge_vk = Verifier.vk_of_backend_vk Vk.merge

  let merge_vk_precomp =
    Verifier.Verification_key.Precomputation.create_constant merge_vk

  let base_vk = Verifier.vk_of_backend_vk Vk.base

  let base_vk_precomp =
    Verifier.Verification_key.Precomputation.create_constant base_vk

  module Prover_state = struct
    type t = {proof_type: Proof_type.t; proof: Tick.Proof.t}
    [@@deriving fields]
  end

  let exists' typ ~f = exists typ ~compute:As_prover.(map get_state ~f)

  (* spec for [main input]:
   constraints pass iff
   (b1, b2, .., bn) = unpack input,
   there is a proof making one of [ base_vk; merge_vk ] accept (b1, b2, .., bn) *)
  let%snarkydef main (input : Wrap_input.var) =
    let%bind input = with_label __LOC__ (Wrap_input.Checked.to_scalar input) in
    let%bind is_base =
      exists' Boolean.typ ~f:(fun {Prover_state.proof_type; _} ->
          Proof_type.is_base proof_type )
    in
    let%bind verification_key_precomp =
      with_label __LOC__
        (Verifier.Verification_key.Precomputation.if_ is_base
           ~then_:base_vk_precomp ~else_:merge_vk_precomp)
    in
    let%bind verification_key =
      with_label __LOC__
        (Verifier.Verification_key.if_ is_base
           ~then_:(Verifier.constant_vk base_vk)
           ~else_:(Verifier.constant_vk merge_vk))
    in
    let%bind result =
      let%bind proof =
        exists Verifier.Proof.typ
          ~compute:
            As_prover.(
              map get_state
                ~f:
                  (Fn.compose Verifier.proof_of_backend_proof
                     Prover_state.proof))
      in
      with_label __LOC__
        (Verifier.verify verification_key verification_key_precomp [input]
           proof)
    in
    with_label __LOC__ (Boolean.Assert.is_true result)

  let create_keys () = generate_keypair ~exposing:wrap_input main

  let cached =
    let load =
      let open Cached.Let_syntax in
      let%map verification =
        Cached.component ~label:"transaction_snark_wrap_verification"
          ~f:Keypair.vk
          (module Verification_key)
      and proving =
        Cached.component ~label:"transaction_snark_wrap_proving" ~f:Keypair.pk
          (module Proving_key)
      in
      (verification, {proving with value= ()})
    in
    Cached.Spec.create ~load ~name:"transaction-snark wrap keys"
      ~autogen_path:Cache_dir.autogen_path
      ~manual_install_path:Cache_dir.manual_install_path
      ~brew_install_path:Cache_dir.brew_install_path
      ~s3_install_path:Cache_dir.s3_install_path
      ~digest_input:(fun x ->
        Md5.to_hex (R1CS_constraint_system.digest (Lazy.force x)) )
      ~input:(lazy (constraint_system ~exposing:wrap_input main))
      ~create_env:(fun x -> Keypair.generate (Lazy.force x))
end

module type S = sig
  include Verification.S

  val of_transaction :
       ?preeval:bool
    -> constraint_constants:Genesis_constants.Constraint_constants.t
    -> sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> Transaction.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t

  val of_user_command :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> User_command.With_valid_signature.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t

  val of_fee_transfer :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> Fee_transfer.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t

  val merge : t -> t -> sok_digest:Sok_message.Digest.t -> t Or_error.t
end

let check_transaction_union ?(preeval = false) ~constraint_constants
    sok_message source target init_stack pending_coinbase_stack_state
    next_available_token_before next_available_token_after transaction
    state_body handler =
  if preeval then failwith "preeval currently disabled" ;
  let sok_digest = Sok_message.digest sok_message in
  let prover_state : Base.Prover_state.t =
    {state1= source; state2= target; sok_digest; pending_coinbase_stack_state}
  in
  let handler =
    Base.transaction_union_handler handler transaction state_body init_stack
      next_available_token_before
  in
  let statement : Statement.With_sok.t =
    { source
    ; target
    ; supply_increase= Transaction_union.supply_increase transaction
    ; pending_coinbase_stack_state
    ; fee_excess= Transaction_union.fee_excess transaction
    ; next_available_token_before
    ; next_available_token_after
    ; proof_type= ()
    ; sok_digest }
  in
  let top_hash = base_top_hash statement in
  let open Tick in
  let main top_hash =
    handle (Base.main ~constraint_constants top_hash) handler
  in
  let main =
    Checked.map (main (Field.Var.constant top_hash)) ~f:As_prover.return
  in
  Or_error.ok_exn (run_and_check main prover_state) |> ignore

let check_transaction ?preeval ~constraint_constants ~sok_message ~source
    ~target ~init_stack ~pending_coinbase_stack_state
    ~next_available_token_before ~next_available_token_after
    (transaction_in_block : Transaction.t Transaction_protocol_state.t) handler
    =
  let transaction =
    Transaction_protocol_state.transaction transaction_in_block
  in
  let state_body =
    Transaction_protocol_state.block_data transaction_in_block
  in
  check_transaction_union ?preeval ~constraint_constants sok_message source
    target init_stack pending_coinbase_stack_state next_available_token_before
    next_available_token_after
    (Transaction_union.of_transaction transaction)
    state_body handler

let check_user_command ~constraint_constants ~sok_message ~source ~target
    ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
    ~next_available_token_after t_in_block handler =
  let user_command = Transaction_protocol_state.transaction t_in_block in
  check_transaction ~constraint_constants ~sok_message ~source ~target
    ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
    ~next_available_token_after
    {t_in_block with transaction= User_command user_command}
    handler

let generate_transaction_union_witness ?(preeval = false) ~constraint_constants
    sok_message source target transaction_in_block init_stack
    next_available_token_before next_available_token_after
    pending_coinbase_stack_state handler =
  if preeval then failwith "preeval currently disabled" ;
  let transaction =
    Transaction_protocol_state.transaction transaction_in_block
  in
  let state_body =
    Transaction_protocol_state.block_data transaction_in_block
  in
  let sok_digest = Sok_message.digest sok_message in
  let prover_state : Base.Prover_state.t =
    {state1= source; state2= target; sok_digest; pending_coinbase_stack_state}
  in
  let handler =
    Base.transaction_union_handler handler transaction state_body init_stack
      next_available_token_before
  in
  let statement : Statement.With_sok.t =
    { source
    ; target
    ; supply_increase= Transaction_union.supply_increase transaction
    ; pending_coinbase_stack_state
    ; fee_excess= Transaction_union.fee_excess transaction
    ; next_available_token_before
    ; next_available_token_after
    ; proof_type= ()
    ; sok_digest }
  in
  let top_hash = base_top_hash statement in
  let open Tick in
  let main top_hash =
    handle (Base.main ~constraint_constants top_hash) handler
  in
  generate_auxiliary_input (tick_input ()) prover_state main top_hash

let generate_transaction_witness ?preeval ~constraint_constants ~sok_message
    ~source ~target ~init_stack ~pending_coinbase_stack_state
    ~next_available_token_before ~next_available_token_after
    (transaction_in_block : Transaction.t Transaction_protocol_state.t) handler
    =
  let transaction =
    Transaction_protocol_state.transaction transaction_in_block
  in
  generate_transaction_union_witness ?preeval ~constraint_constants sok_message
    source target
    { transaction_in_block with
      transaction= Transaction_union.of_transaction transaction }
    init_stack next_available_token_before next_available_token_after
    pending_coinbase_stack_state handler

let verification_keys_of_keys {Keys0.verification; _} = verification

module Make (K : sig
  val keys : Keys0.t
end) =
struct
  open K

  include Verification.Make (struct
    let keys = verification_keys_of_keys keys
  end)

  module Wrap = Wrap (struct
    let merge = keys.verification.merge

    let base = keys.verification.base
  end)

  let wrap proof_type proof input =
    let prover_state = {Wrap.Prover_state.proof; proof_type} in
    Tock.prove keys.proving.wrap wrap_input prover_state Wrap.main
      (Wrap_input.of_tick_field input)

  let merge_proof sok_digest ledger_hash1 ledger_hash2 ledger_hash3
      next_available_token1 next_available_token2 next_available_token3
      transition12 transition23 =
    let fee_excess =
      Or_error.ok_exn
      @@ Fee_excess.combine transition12.Transition_data.fee_excess
           transition23.Transition_data.fee_excess
    in
    let supply_increase =
      Amount.add transition12.supply_increase transition23.supply_increase
      |> Option.value_exn
    in
    let statement : Statement.With_sok.t =
      { source= ledger_hash1
      ; target= ledger_hash3
      ; supply_increase
      ; pending_coinbase_stack_state=
          { source= transition12.pending_coinbase_stack_state.source
          ; target= transition23.pending_coinbase_stack_state.target }
      ; fee_excess
      ; next_available_token_before= next_available_token1
      ; next_available_token_after= next_available_token2
      ; proof_type= ()
      ; sok_digest }
    in
    let top_hash = merge_top_hash wrap_vk_state statement in
    let prover_state =
      { Merge.Prover_state.sok_digest
      ; ledger_hash1
      ; ledger_hash2
      ; ledger_hash3
      ; next_available_token1
      ; next_available_token2
      ; next_available_token3
      ; pending_coinbase_stack1=
          transition12.pending_coinbase_stack_state.source
      ; pending_coinbase_stack2=
          transition12.pending_coinbase_stack_state.target
      ; pending_coinbase_stack3=
          transition23.pending_coinbase_stack_state.source
      ; pending_coinbase_stack4=
          transition23.pending_coinbase_stack_state.target
      ; transition12
      ; transition23
      ; tock_vk= keys.verification.wrap }
    in
    ( top_hash
    , Tick.prove keys.proving.merge (tick_input ()) prover_state Merge.main
        top_hash )

  let of_transaction_union ?preeval ~constraint_constants sok_digest source
      target ~init_stack ~pending_coinbase_stack_state
      ~next_available_token_before ~next_available_token_after transaction
      state_body handler =
    let top_hash, proof =
      Base.transaction_union_proof ?preeval ~constraint_constants sok_digest
        ~proving_key:keys.proving.base source target init_stack
        pending_coinbase_stack_state next_available_token_before
        next_available_token_after transaction state_body handler
    in
    { source
    ; sok_digest
    ; target
    ; proof_type= `Base
    ; fee_excess= Transaction_union.fee_excess transaction
    ; next_available_token_before
    ; next_available_token_after
    ; pending_coinbase_stack_state
    ; supply_increase= Transaction_union.supply_increase transaction
    ; proof= wrap `Base proof top_hash }

  let of_transaction ?preeval ~constraint_constants ~sok_digest ~source ~target
      ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after transaction_in_block handler =
    let transaction =
      Transaction_protocol_state.transaction transaction_in_block
    in
    let state_body =
      Transaction_protocol_state.block_data transaction_in_block
    in
    of_transaction_union ?preeval ~constraint_constants sok_digest source
      target ~init_stack ~pending_coinbase_stack_state
      ~next_available_token_before ~next_available_token_after
      (Transaction_union.of_transaction transaction)
      state_body handler

  let of_user_command ~constraint_constants ~sok_digest ~source ~target
      ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after user_command_in_block handler =
    of_transaction ~constraint_constants ~sok_digest ~source ~target
      ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after
      { user_command_in_block with
        transaction=
          User_command
            (Transaction_protocol_state.transaction user_command_in_block) }
      handler

  let of_fee_transfer ~constraint_constants ~sok_digest ~source ~target
      ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after transfer_in_block handler =
    of_transaction ~constraint_constants ~sok_digest ~source ~target
      ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after
      { transfer_in_block with
        transaction=
          Fee_transfer
            (Transaction_protocol_state.transaction transfer_in_block) }
      handler

  let merge t1 t2 ~sok_digest =
    if not (Frozen_ledger_hash.( = ) t1.target t2.source) then
      failwithf
        !"Transaction_snark.merge: t1.target <> t2.source \
          (%{sexp:Frozen_ledger_hash.t} vs %{sexp:Frozen_ledger_hash.t})"
        t1.target t2.source () ;
    if
      not
        (Token_id.( = ) t1.next_available_token_after
           t2.next_available_token_before)
    then
      failwithf
        !"Transaction_snark.merge: t1.next_available_token_befre <> \
          t2.next_available_token_after (%{sexp:Token_id.t} vs \
          %{sexp:Token_id.t})"
        t1.next_available_token_after t2.next_available_token_before () ;
    let input, proof =
      merge_proof sok_digest t1.source t1.target t2.target
        t1.next_available_token_before t1.next_available_token_after
        t2.next_available_token_after
        { Transition_data.proof= (t1.proof_type, t1.proof)
        ; fee_excess= t1.fee_excess
        ; supply_increase= t1.supply_increase
        ; sok_digest= t1.sok_digest
        ; pending_coinbase_stack_state= t1.pending_coinbase_stack_state }
        { Transition_data.proof= (t2.proof_type, t2.proof)
        ; fee_excess= t2.fee_excess
        ; supply_increase= t2.supply_increase
        ; sok_digest= t2.sok_digest
        ; pending_coinbase_stack_state= t2.pending_coinbase_stack_state }
    in
    let open Or_error.Let_syntax in
    let%map fee_excess = Fee_excess.combine t1.fee_excess t2.fee_excess
    and supply_increase =
      Amount.add t1.supply_increase t2.supply_increase
      |> Option.value_map ~f:Or_error.return
           ~default:
             (Or_error.errorf
                "Transaction_snark.merge: Supply change amount overflow")
    in
    { source= t1.source
    ; target= t2.target
    ; sok_digest
    ; fee_excess
    ; next_available_token_before= t1.next_available_token_before
    ; next_available_token_after= t2.next_available_token_after
    ; supply_increase
    ; pending_coinbase_stack_state=
        { source= t1.pending_coinbase_stack_state.source
        ; target= t2.pending_coinbase_stack_state.target }
    ; proof_type= `Merge
    ; proof= wrap `Merge proof input }
end

module Keys = struct
  module Storage = Storage.List.Make (Storage.Disk)

  module Per_snark_location = struct
    module T = struct
      type t =
        { base: Storage.location
        ; merge: Storage.location
        ; wrap: Storage.location }
      [@@deriving sexp]
    end

    include T
    include Sexpable.To_stringable (T)
  end

  let checksum ~prefix ~base ~merge ~wrap =
    Md5.digest_string
      ( "Transaction_snark_" ^ prefix ^ Md5.to_hex base ^ Md5.to_hex merge
      ^ Md5.to_hex wrap )

  module Verification = struct
    include Keys0.Verification
    module Location = Per_snark_location

    let checksum ~base ~merge ~wrap =
      checksum ~prefix:"transaction_snark_verification" ~base ~merge ~wrap

    let load ({merge; base; wrap} : Location.t) =
      let open Storage in
      let logger = Logger.create () in
      let tick_controller =
        Controller.create ~logger (module Tick.Verification_key)
      in
      let tock_controller =
        Controller.create ~logger (module Tock.Verification_key)
      in
      let open Async in
      let load c p =
        match%map load_with_checksum c p with
        | Ok x ->
            x
        | Error _e ->
            failwithf
              !"Transaction_snark: load failed on %{sexp:Storage.location}"
              p ()
      in
      let%map base = load tick_controller base
      and merge = load tick_controller merge
      and wrap = load tock_controller wrap in
      let t = {base= base.data; merge= merge.data; wrap= wrap.data} in
      ( t
      , checksum ~base:base.checksum ~merge:merge.checksum ~wrap:wrap.checksum
      )
  end

  module Proving = struct
    include Keys0.Proving
    module Location = Per_snark_location

    let checksum ~base ~merge ~wrap =
      checksum ~prefix:"transaction_snark_proving" ~base ~merge ~wrap

    let load ({merge; base; wrap} : Location.t) =
      let open Storage in
      let logger = Logger.create () in
      let tick_controller =
        Controller.create ~logger (module Tick.Proving_key)
      in
      let tock_controller =
        Controller.create ~logger (module Tock.Proving_key)
      in
      let open Async in
      let load c p =
        match%map load_with_checksum c p with
        | Ok x ->
            x
        | Error _e ->
            failwithf
              !"Transaction_snark: load failed on %{sexp:Storage.location}"
              p ()
      in
      let%map base = load tick_controller base
      and merge = load tick_controller merge
      and wrap = load tock_controller wrap in
      let t = {base= base.data; merge= merge.data; wrap= wrap.data} in
      ( t
      , checksum ~base:base.checksum ~merge:merge.checksum ~wrap:wrap.checksum
      )
  end

  module Location = struct
    module T = struct
      type t =
        {proving: Proving.Location.t; verification: Verification.Location.t}
      [@@deriving sexp]
    end

    include T
    include Sexpable.To_stringable (T)
  end

  include Keys0.T

  module Checksum = struct
    type t = {proving: Md5.t; verification: Md5.t}
  end

  let create () =
    let base = Base.create_keys () in
    let merge = Merge.create_keys () in
    let wrap =
      let module Wrap = Wrap (struct
        let base = Tick.Keypair.vk base

        let merge = Tick.Keypair.vk merge
      end) in
      Wrap.create_keys ()
    in
    { proving=
        { base= Tick.Keypair.pk base
        ; merge= Tick.Keypair.pk merge
        ; wrap= Tock.Keypair.pk wrap }
    ; verification=
        { base= Tick.Keypair.vk base
        ; merge= Tick.Keypair.vk merge
        ; wrap= Tock.Keypair.vk wrap } }

  let cached () =
    let paths path = Cache_dir.possible_paths (Filename.basename path) in
    let open Cached.Deferred_with_track_generated.Let_syntax in
    let%bind base_vk, base_pk = Cached.run Base.cached in
    let%bind merge_vk, merge_pk = Cached.run Merge.cached in
    let%map wrap_vk, wrap_pk =
      let module Wrap = Wrap (struct
        let base = base_vk.value

        let merge = merge_vk.value
      end) in
      Cached.run Wrap.cached
    in
    let t : Verification.t =
      {base= base_vk.value; merge= merge_vk.value; wrap= wrap_vk.value}
    in
    let location : Location.t =
      { proving=
          { base= paths base_pk.path
          ; merge= paths merge_pk.path
          ; wrap= paths wrap_pk.path }
      ; verification=
          { base= paths base_vk.path
          ; merge= paths merge_vk.path
          ; wrap= paths wrap_vk.path } }
    in
    let checksum =
      { Checksum.proving=
          Proving.checksum ~base:base_pk.checksum ~merge:merge_pk.checksum
            ~wrap:wrap_pk.checksum
      ; verification=
          Verification.checksum ~base:base_vk.checksum ~merge:merge_vk.checksum
            ~wrap:wrap_vk.checksum }
    in
    (location, t, checksum)
end

let%test_module "transaction_snark" =
  ( module struct
    let constraint_constants =
      Genesis_constants.Constraint_constants.for_unit_tests

    let genesis_constants = Genesis_constants.for_unit_tests

    let consensus_constants =
      Consensus.Constants.create ~constraint_constants
        ~protocol_constants:genesis_constants.protocol

    (* For tests let's just monkey patch ledger and sparse ledger to freeze their
     * ledger_hashes. The nominal type is just so we don't mix this up in our
     * real code. *)
    module Ledger = struct
      include Ledger

      let merkle_root t = Frozen_ledger_hash.of_ledger_hash @@ merkle_root t

      let merkle_root_after_user_command_exn t ~txn_global_slot txn =
        let hash, `Next_available_token tid =
          merkle_root_after_user_command_exn ~constraint_constants
            ~txn_global_slot t txn
        in
        (Frozen_ledger_hash.of_ledger_hash hash, `Next_available_token tid)
    end

    module Sparse_ledger = struct
      include Sparse_ledger

      let merkle_root t = Frozen_ledger_hash.of_ledger_hash @@ merkle_root t
    end

    type wallet = {private_key: Private_key.t; account: Account.t}

    let ledger_depth = constraint_constants.ledger_depth

    let random_wallets ?(n = min (Int.pow 2 ledger_depth) (1 lsl 10)) () =
      let random_wallet () : wallet =
        let private_key = Private_key.create () in
        let public_key =
          Public_key.compress (Public_key.of_private_key_exn private_key)
        in
        let account_id = Account_id.create public_key Token_id.default in
        { private_key
        ; account=
            Account.create account_id
              (Balance.of_int ((50 + Random.int 100) * 1_000_000_000)) }
      in
      Array.init n ~f:(fun _ -> random_wallet ())

    let user_command ~fee_payer ~source_pk ~receiver_pk ~fee_token ~token amt
        fee nonce memo =
      let payload : User_command.Payload.t =
        User_command.Payload.create ~fee ~fee_token
          ~fee_payer_pk:(Account.public_key fee_payer.account)
          ~nonce ~memo ~valid_until:Global_slot.max_value
          ~body:
            (Payment
               { source_pk
               ; receiver_pk
               ; token_id= token
               ; amount= Amount.of_int amt })
      in
      let signature =
        User_command.sign_payload fee_payer.private_key payload
      in
      User_command.check
        User_command.Poly.Stable.Latest.
          { payload
          ; signer= Public_key.of_private_key_exn fee_payer.private_key
          ; signature }
      |> Option.value_exn

    let user_command_with_wallet wallets ~sender:i ~receiver:j amt fee
        ~fee_token ~token nonce memo =
      let fee_payer = wallets.(i) in
      let receiver = wallets.(j) in
      user_command ~fee_payer
        ~source_pk:(Account.public_key fee_payer.account)
        ~receiver_pk:(Account.public_key receiver.account)
        ~fee_token ~token amt fee nonce memo

    let keys = Keys.create ()

    include Make (struct
      let keys = keys
    end)

    let state_body =
      let compile_time_genesis =
        (*not using Precomputed_values.for_unit_test because of dependency cycle*)
        Coda_state.Genesis_protocol_state.t
          ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
          ~constraint_constants ~consensus_constants
      in
      compile_time_genesis.data |> Coda_state.Protocol_state.body

    let state_body_hash = Coda_state.Protocol_state.Body.hash state_body

    let pending_coinbase_stack_target (t : Transaction.t) state_body_hash stack
        =
      let stack_with_state =
        Pending_coinbase.Stack.(push_state state_body_hash stack)
      in
      match t with
      | Coinbase c ->
          Pending_coinbase.(Stack.push_coinbase c stack_with_state)
      | _ ->
          stack_with_state

    let check_balance pk balance ledger =
      let loc = Ledger.location_of_account ledger pk |> Option.value_exn in
      let acc = Ledger.get ledger loc |> Option.value_exn in
      [%test_eq: Balance.t] acc.balance (Balance.of_int balance)

    let of_user_command' sok_digest ledger user_command init_stack
        pending_coinbase_stack_state state_body handler =
      let source = Ledger.merkle_root ledger in
      let current_global_slot =
        Coda_state.Protocol_state.Body.consensus_state state_body
        |> Consensus.Data.Consensus_state.curr_slot
      in
      let next_available_token_before = Ledger.next_available_token ledger in
      let target, `Next_available_token next_available_token_after =
        Ledger.merkle_root_after_user_command_exn ledger
          ~txn_global_slot:current_global_slot user_command
      in
      let user_command_in_block =
        { Transaction_protocol_state.Poly.transaction= user_command
        ; block_data= state_body }
      in
      of_user_command ~constraint_constants ~sok_digest ~source ~target
        ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
        ~next_available_token_after user_command_in_block handler

    (*
                ~proposer:
                  { x=
                      Snark_params.Tick.Field.of_string
                        "39876046544032071884326965137489542106804584544160987424424979200505499184903744868114140"
                  ; is_odd= true }
                ~fee_transfer:
                  (Some
                     ( { x=
                           Snark_params.Tick.Field.of_string
                             "221715137372156378645114069225806158618712943627692160064142985953895666487801880947288786"
                       ; is_odd= true }
       *)

    let coinbase_test state_body ~carryforward =
      let mk_pubkey () =
        Public_key.(compress (of_private_key_exn (Private_key.create ())))
      in
      let state_body_hash = Coda_state.Protocol_state.Body.hash state_body in
      let producer = mk_pubkey () in
      let producer_id = Account_id.create producer Token_id.default in
      let receiver = mk_pubkey () in
      let receiver_id = Account_id.create receiver Token_id.default in
      let other = mk_pubkey () in
      let other_id = Account_id.create other Token_id.default in
      let pending_coinbase_init = Pending_coinbase.Stack.empty in
      let cb =
        Coinbase.create
          ~amount:(Currency.Amount.of_int 10_000_000_000)
          ~receiver
          ~fee_transfer:
            (Some
               (Coinbase.Fee_transfer.create ~receiver_pk:other
                  ~fee:constraint_constants.account_creation_fee))
        |> Or_error.ok_exn
      in
      let transaction = Transaction.Coinbase cb in
      let source_stack =
        if carryforward then
          Pending_coinbase.Stack.(
            push_state state_body_hash pending_coinbase_init)
        else pending_coinbase_init
      in
      let pending_coinbase_stack_target =
        pending_coinbase_stack_target transaction state_body_hash
          pending_coinbase_init
      in
      let txn_in_block =
        {Transaction_protocol_state.Poly.transaction; block_data= state_body}
      in
      Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
          Ledger.create_new_account_exn ledger producer_id
            (Account.create receiver_id Balance.zero) ;
          let sparse_ledger =
            Sparse_ledger.of_ledger_subset_exn ledger
              [producer_id; receiver_id; other_id]
          in
          let sparse_ledger_after =
            Sparse_ledger.apply_transaction_exn ~constraint_constants
              sparse_ledger
              ~txn_global_slot:
                ( txn_in_block.block_data
                |> Coda_state.Protocol_state.Body.consensus_state
                |> Consensus.Data.Consensus_state.curr_global_slot )
              txn_in_block.transaction
          in
          check_transaction txn_in_block
            (unstage (Sparse_ledger.handler sparse_ledger))
            ~constraint_constants
            ~sok_message:
              (Coda_base.Sok_message.create ~fee:Currency.Fee.zero
                 ~prover:Public_key.Compressed.empty)
            ~source:(Sparse_ledger.merkle_root sparse_ledger)
            ~target:(Sparse_ledger.merkle_root sparse_ledger_after)
            ~next_available_token_before:(Ledger.next_available_token ledger)
            ~next_available_token_after:
              (Sparse_ledger.next_available_token sparse_ledger_after)
            ~init_stack:pending_coinbase_init
            ~pending_coinbase_stack_state:
              {source= source_stack; target= pending_coinbase_stack_target} )

    let%test_unit "coinbase with new state body hash" =
      Test_util.with_randomness 123456789 (fun () ->
          coinbase_test state_body ~carryforward:false )

    let%test_unit "coinbase with carry-forward state body hash" =
      Test_util.with_randomness 123456789 (fun () ->
          coinbase_test state_body ~carryforward:true )

    let%test_unit "new_account" =
      Test_util.with_randomness 123456789 (fun () ->
          let wallets = random_wallets () in
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              Array.iter
                (Array.sub wallets ~pos:1 ~len:(Array.length wallets - 1))
                ~f:(fun {account; private_key= _} ->
                  Ledger.create_new_account_exn ledger
                    (Account.identifier account)
                    account ) ;
              let t1 =
                user_command_with_wallet wallets ~sender:1 ~receiver:0
                  8_000_000_000
                  (Fee.of_int (Random.int 20 * 1_000_000_000))
                  ~fee_token:Token_id.default ~token:Token_id.default
                  Account.Nonce.zero
                  (User_command_memo.create_by_digesting_string_exn
                     (Test_util.arbitrary_string
                        ~len:User_command_memo.max_digestible_string_length))
              in
              let current_global_slot =
                Coda_state.Protocol_state.Body.consensus_state state_body
                |> Consensus.Data.Consensus_state.curr_slot
              in
              let next_available_token_before =
                Ledger.next_available_token ledger
              in
              let target, `Next_available_token next_available_token_after =
                Ledger.merkle_root_after_user_command_exn ledger
                  ~txn_global_slot:current_global_slot t1
              in
              let mentioned_keys =
                User_command.accounts_accessed
                  ~next_available_token:next_available_token_before
                  (User_command.forget_check t1)
              in
              let sparse_ledger =
                Sparse_ledger.of_ledger_subset_exn ledger mentioned_keys
              in
              let sok_message =
                Sok_message.create ~fee:Fee.zero
                  ~prover:wallets.(1).account.public_key
              in
              let pending_coinbase_stack = Pending_coinbase.Stack.empty in
              let pending_coinbase_stack_target =
                pending_coinbase_stack_target (User_command t1) state_body_hash
                  pending_coinbase_stack
              in
              let pending_coinbase_stack_state =
                { Pending_coinbase_stack_state.source= pending_coinbase_stack
                ; target= pending_coinbase_stack_target }
              in
              check_user_command ~constraint_constants ~sok_message
                ~source:(Ledger.merkle_root ledger)
                ~target ~init_stack:pending_coinbase_stack
                ~pending_coinbase_stack_state ~next_available_token_before
                ~next_available_token_after
                {transaction= t1; block_data= state_body}
                (unstage @@ Sparse_ledger.handler sparse_ledger) ) )

    let account_fee = Fee.to_int constraint_constants.account_creation_fee

    let test_transaction ~constraint_constants ?txn_global_slot ledger txn =
      let source = Ledger.merkle_root ledger in
      let pending_coinbase_stack = Pending_coinbase.Stack.empty in
      let next_available_token = Ledger.next_available_token ledger in
      let state_body, state_body_hash, txn_global_slot =
        match txn_global_slot with
        | None ->
            let txn_global_slot =
              state_body |> Coda_state.Protocol_state.Body.consensus_state
              |> Consensus.Data.Consensus_state.curr_slot
            in
            (state_body, state_body_hash, txn_global_slot)
        | Some txn_global_slot ->
            let state_body =
              let state =
                (* NB: The [previous_state_hash] is a dummy, do not use. *)
                Coda_state.Protocol_state.create
                  ~previous_state_hash:Tick0.Field.zero ~body:state_body
              in
              let consensus_state_at_slot =
                Consensus.Data.Consensus_state.Value.For_tests
                .with_curr_global_slot
                  (Coda_state.Protocol_state.consensus_state state)
                  txn_global_slot
              in
              Coda_state.Protocol_state.(
                create_value
                  ~previous_state_hash:(previous_state_hash state)
                  ~genesis_state_hash:(genesis_state_hash state)
                  ~blockchain_state:(blockchain_state state)
                  ~consensus_state:consensus_state_at_slot
                  ~constants:
                    (Protocol_constants_checked.value_of_t
                       Genesis_constants.compiled.protocol))
                .body
            in
            let state_body_hash =
              Coda_state.Protocol_state.Body.hash state_body
            in
            (state_body, state_body_hash, txn_global_slot)
      in
      let mentioned_keys, pending_coinbase_stack_target =
        let pending_coinbase_stack =
          Pending_coinbase.Stack.push_state state_body_hash
            pending_coinbase_stack
        in
        match txn with
        | Transaction.User_command uc ->
            ( User_command.accounts_accessed ~next_available_token
                (uc :> User_command.t)
            , pending_coinbase_stack )
        | Fee_transfer ft ->
            (Fee_transfer.receivers ft, pending_coinbase_stack)
        | Coinbase cb ->
            ( Coinbase.accounts_accessed cb
            , Pending_coinbase.Stack.push_coinbase cb pending_coinbase_stack )
      in
      let signer =
        let txn_union = Transaction_union.of_transaction txn in
        txn_union.signer |> Public_key.compress
      in
      let sparse_ledger =
        Sparse_ledger.of_ledger_subset_exn ledger mentioned_keys
      in
      let _undo =
        Or_error.ok_exn
        @@ Ledger.apply_transaction ledger ~constraint_constants
             ~txn_global_slot txn
      in
      let target = Ledger.merkle_root ledger in
      let sok_message = Sok_message.create ~fee:Fee.zero ~prover:signer in
      check_transaction ~constraint_constants ~sok_message ~source ~target
        ~init_stack:pending_coinbase_stack
        ~pending_coinbase_stack_state:
          { Pending_coinbase_stack_state.source= pending_coinbase_stack
          ; target= pending_coinbase_stack_target }
        ~next_available_token_before:next_available_token
        ~next_available_token_after:(Ledger.next_available_token ledger)
        {transaction= txn; block_data= state_body}
        (unstage @@ Sparse_ledger.handler sparse_ledger)

    let%test_unit "account creation fee - user commands" =
      Test_util.with_randomness 123456789 (fun () ->
          let wallets = random_wallets ~n:3 () |> Array.to_list in
          let sender = List.hd_exn wallets in
          let receivers = List.tl_exn wallets in
          let txns_per_receiver = 2 in
          let amount = 8_000_000_000 in
          let txn_fee = 2_000_000_000 in
          let memo =
            User_command_memo.create_by_digesting_string_exn
              (Test_util.arbitrary_string
                 ~len:User_command_memo.max_digestible_string_length)
          in
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let _, ucs =
                let receivers =
                  List.fold ~init:receivers
                    (List.init (txns_per_receiver - 1) ~f:Fn.id)
                    ~f:(fun acc _ -> receivers @ acc)
                in
                List.fold receivers ~init:(Account.Nonce.zero, [])
                  ~f:(fun (nonce, txns) receiver ->
                    let uc =
                      user_command ~fee_payer:sender
                        ~source_pk:(Account.public_key sender.account)
                        ~receiver_pk:(Account.public_key receiver.account)
                        ~fee_token:Token_id.default ~token:Token_id.default
                        amount (Fee.of_int txn_fee) nonce memo
                    in
                    (Account.Nonce.succ nonce, txns @ [uc]) )
              in
              Ledger.create_new_account_exn ledger
                (Account.identifier sender.account)
                sender.account ;
              let () =
                List.iter ucs ~f:(fun uc ->
                    test_transaction ~constraint_constants ledger
                      (Transaction.User_command uc) )
              in
              List.iter receivers ~f:(fun receiver ->
                  check_balance
                    (Account.identifier receiver.account)
                    ((amount * txns_per_receiver) - account_fee)
                    ledger ) ;
              check_balance
                (Account.identifier sender.account)
                ( Balance.to_int sender.account.balance
                - (amount + txn_fee) * txns_per_receiver
                  * List.length receivers )
                ledger ) )

    let%test_unit "account creation fee - fee transfers" =
      Test_util.with_randomness 123456789 (fun () ->
          let receivers = random_wallets ~n:3 () |> Array.to_list in
          let txns_per_receiver = 3 in
          let fee = 8_000_000_000 in
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let fts =
                let receivers =
                  List.fold ~init:receivers
                    (List.init (txns_per_receiver - 1) ~f:Fn.id)
                    ~f:(fun acc _ -> receivers @ acc)
                  |> One_or_two.group_list
                in
                List.fold receivers ~init:[] ~f:(fun txns receiver ->
                    let ft : Fee_transfer.t =
                      Or_error.ok_exn @@ Fee_transfer.of_singles
                      @@ One_or_two.map receiver ~f:(fun receiver ->
                             Fee_transfer.Single.create
                               ~receiver_pk:receiver.account.public_key
                               ~fee:(Currency.Fee.of_int fee)
                               ~fee_token:receiver.account.token_id )
                    in
                    txns @ [ft] )
              in
              let () =
                List.iter fts ~f:(fun ft ->
                    let txn = Transaction.Fee_transfer ft in
                    test_transaction ~constraint_constants ledger txn )
              in
              List.iter receivers ~f:(fun receiver ->
                  check_balance
                    (Account.identifier receiver.account)
                    ((fee * txns_per_receiver) - account_fee)
                    ledger ) ) )

    let%test_unit "account creation fee - coinbase" =
      Test_util.with_randomness 123456789 (fun () ->
          let wallets = random_wallets ~n:3 () in
          let receiver = wallets.(0) in
          let other = wallets.(1) in
          let dummy_account = wallets.(2) in
          let reward = 10_000_000_000 in
          let fee = Fee.to_int constraint_constants.account_creation_fee in
          let coinbase_count = 3 in
          let ft_count = 2 in
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let _, cbs =
                let fts =
                  List.map (List.init ft_count ~f:Fn.id) ~f:(fun _ ->
                      Coinbase.Fee_transfer.create
                        ~receiver_pk:other.account.public_key
                        ~fee:constraint_constants.account_creation_fee )
                in
                List.fold ~init:(fts, []) (List.init coinbase_count ~f:Fn.id)
                  ~f:(fun (fts, cbs) _ ->
                    let cb =
                      Coinbase.create
                        ~amount:(Currency.Amount.of_int reward)
                        ~receiver:receiver.account.public_key
                        ~fee_transfer:(List.hd fts)
                      |> Or_error.ok_exn
                    in
                    (Option.value ~default:[] (List.tl fts), cb :: cbs) )
              in
              Ledger.create_new_account_exn ledger
                (Account.identifier dummy_account.account)
                dummy_account.account ;
              let () =
                List.iter cbs ~f:(fun cb ->
                    let txn = Transaction.Coinbase cb in
                    test_transaction ~constraint_constants ledger txn )
              in
              let fees = fee * ft_count in
              check_balance
                (Account.identifier receiver.account)
                ((reward * coinbase_count) - account_fee - fees)
                ledger ;
              check_balance
                (Account.identifier other.account)
                (fees - account_fee) ledger ) )

    let test_base_and_merge ~state_hash_and_body1 ~state_hash_and_body2
        ~carryforward1 ~carryforward2 =
      Test_util.with_randomness 123456789 (fun () ->
          let wallets = random_wallets () in
          (*let state_body = Lazy.force state_body in
      let state_body_hash = Lazy.force state_body_hash in*)
          let state_body_hash1, state_body1 = state_hash_and_body1 in
          let state_body_hash2, state_body2 = state_hash_and_body2 in
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              Array.iter wallets ~f:(fun {account; private_key= _} ->
                  Ledger.create_new_account_exn ledger
                    (Account.identifier account)
                    account ) ;
              let memo =
                User_command_memo.create_by_digesting_string_exn
                  (Test_util.arbitrary_string
                     ~len:User_command_memo.max_digestible_string_length)
              in
              let t1 =
                user_command_with_wallet wallets ~sender:0 ~receiver:1
                  8_000_000_000
                  (Fee.of_int (Random.int 20 * 1_000_000_000))
                  ~fee_token:Token_id.default ~token:Token_id.default
                  Account.Nonce.zero memo
              in
              let t2 =
                user_command_with_wallet wallets ~sender:1 ~receiver:2
                  8_000_000_000
                  (Fee.of_int (Random.int 20 * 1_000_000_000))
                  ~fee_token:Token_id.default ~token:Token_id.default
                  Account.Nonce.zero memo
              in
              let sok_digest =
                Sok_message.create ~fee:Fee.zero
                  ~prover:wallets.(0).account.public_key
                |> Sok_message.digest
              in
              let state1 = Ledger.merkle_root ledger in
              let next_available_token1 = Ledger.next_available_token ledger in
              let sparse_ledger =
                Sparse_ledger.of_ledger_subset_exn ledger
                  (List.concat_map
                     ~f:(fun t ->
                       (* NB: Shouldn't assume the same next_available_token
                          for each command normally, but we know statically
                          that these are payments in this test.
                       *)
                       User_command.accounts_accessed
                         ~next_available_token:next_available_token1
                         (User_command.forget_check t) )
                     [t1; t2])
              in
              let init_stack1 = Pending_coinbase.Stack.empty in
              let pending_coinbase_stack_state1 =
                (* No coinbase to add to the stack. *)
                let stack_with_state =
                  Pending_coinbase.Stack.push_state state_body_hash1
                    init_stack1
                in
                (* Since protocol state body is added once per block, the
                   source would already have the state if [carryforward=true]
                   from the previous transaction in the sequence of
                   transactions in a block. We add state to [init_stack] and
                   then check that it is equal to the target.
                *)
                let source_stack, target_stack =
                  if carryforward1 then (stack_with_state, stack_with_state)
                  else (init_stack1, stack_with_state)
                in
                { Pending_coinbase_stack_state.source= source_stack
                ; target= target_stack }
              in
              let proof12 =
                of_user_command' sok_digest ledger t1 init_stack1
                  pending_coinbase_stack_state1 state_body1
                  (unstage @@ Sparse_ledger.handler sparse_ledger)
              in
              let current_global_slot =
                Coda_state.Protocol_state.Body.consensus_state state_body1
                |> Consensus.Data.Consensus_state.curr_slot
              in
              let sparse_ledger =
                Sparse_ledger.apply_user_command_exn ~constraint_constants
                  ~txn_global_slot:current_global_slot sparse_ledger
                  (t1 :> User_command.t)
              in
              let pending_coinbase_stack_state2, state_body2, init_stack2 =
                let previous_stack = pending_coinbase_stack_state1.target in
                let stack_with_state2 =
                  Pending_coinbase.Stack.(
                    push_state state_body_hash2 previous_stack)
                in
                (* No coinbase to add. *)
                let source_stack, target_stack, init_stack, state_body2 =
                  if carryforward2 then
                    (* Source and target already have the protocol state,
                       init_stack will be such that
                       [init_stack + state_body_hash1 = target = source].
                    *)
                    (previous_stack, previous_stack, init_stack1, state_body1)
                  else
                    (* Add the new state such that
                       [previous_stack + state_body_hash2
                        = init_stack + state_body_hash2
                        = target].
                    *)
                    ( previous_stack
                    , stack_with_state2
                    , previous_stack
                    , state_body2 )
                in
                ( { Pending_coinbase_stack_state.source= source_stack
                  ; target= target_stack }
                , state_body2
                , init_stack )
              in
              Ledger.apply_user_command ~constraint_constants ledger
                ~txn_global_slot:current_global_slot t1
              |> Or_error.ok_exn |> ignore ;
              [%test_eq: Frozen_ledger_hash.t]
                (Ledger.merkle_root ledger)
                (Sparse_ledger.merkle_root sparse_ledger) ;
              let proof23 =
                of_user_command' sok_digest ledger t2 init_stack2
                  pending_coinbase_stack_state2 state_body2
                  (unstage @@ Sparse_ledger.handler sparse_ledger)
              in
              let current_global_slot =
                Coda_state.Protocol_state.Body.consensus_state state_body2
                |> Consensus.Data.Consensus_state.curr_slot
              in
              let sparse_ledger =
                Sparse_ledger.apply_user_command_exn ~constraint_constants
                  ~txn_global_slot:current_global_slot sparse_ledger
                  (t2 :> User_command.t)
              in
              let pending_coinbase_stack_state_merge =
                Pending_coinbase_stack_state.
                  { source= pending_coinbase_stack_state1.source
                  ; target= pending_coinbase_stack_state2.target }
              in
              Ledger.apply_user_command ledger ~constraint_constants
                ~txn_global_slot:current_global_slot t2
              |> Or_error.ok_exn |> ignore ;
              [%test_eq: Frozen_ledger_hash.t]
                (Ledger.merkle_root ledger)
                (Sparse_ledger.merkle_root sparse_ledger) ;
              let total_fees =
                let open Fee in
                let magnitude =
                  User_command_payload.fee (t1 :> User_command.t).payload
                  + User_command_payload.fee (t2 :> User_command.t).payload
                  |> Option.value_exn
                in
                Signed.create ~magnitude ~sgn:Sgn.Pos
              in
              let state3 = Sparse_ledger.merkle_root sparse_ledger in
              let next_available_token3 = Ledger.next_available_token ledger in
              let proof13 =
                merge ~sok_digest proof12 proof23 |> Or_error.ok_exn
              in
              let statement : Statement.With_sok.t =
                { source= state1
                ; target= state3
                ; supply_increase= Amount.zero
                ; pending_coinbase_stack_state=
                    pending_coinbase_stack_state_merge
                ; fee_excess=
                    Fee_excess.of_single (Token_id.default, total_fees)
                ; next_available_token_before= next_available_token1
                ; next_available_token_after= next_available_token3
                ; proof_type= ()
                ; sok_digest }
              in
              Tock.verify proof13.proof keys.verification.wrap wrap_input
                (Wrap_input.of_tick_field
                   (merge_top_hash wrap_vk_state statement)) ) )

    let%test "base_and_merge: transactions in one block (t1,t2 in b1), \
              carryforward the state from a previous transaction t0 in b1" =
      let state_hash_and_body1 = (state_body_hash, state_body) in
      test_base_and_merge ~state_hash_and_body1
        ~state_hash_and_body2:state_hash_and_body1 ~carryforward1:true
        ~carryforward2:true

    (* No new state body, carryforward the stack from the previous transaction*)

    let%test "base_and_merge: transactions in one block (t1,t2 in b1), don't \
              carryforward the state from a previous transaction t0 in b1" =
      let state_hash_and_body1 = (state_body_hash, state_body) in
      test_base_and_merge ~state_hash_and_body1
        ~state_hash_and_body2:state_hash_and_body1 ~carryforward1:false
        ~carryforward2:true

    let%test "base_and_merge: transactions in two different blocks (t1,t2 in \
              b1, b2 resp.), carryforward the state from a previous \
              transaction t0 in b1" =
      let state_hash_and_body1 =
        let state_body0 =
          Coda_state.Protocol_state.negative_one
            ~genesis_ledger:Test_genesis_ledger.t ~constraint_constants
            ~consensus_constants
          |> Coda_state.Protocol_state.body
        in
        let state_body_hash0 =
          Coda_state.Protocol_state.Body.hash state_body0
        in
        (state_body_hash0, state_body0)
      in
      let state_hash_and_body2 = (state_body_hash, state_body) in
      test_base_and_merge ~state_hash_and_body1 ~state_hash_and_body2
        ~carryforward1:true ~carryforward2:false

    (*t2 is in a new state, therefore do not carryforward the previous state*)

    let%test "base_and_merge: transactions in two different blocks (t1,t2 in \
              b1, b2 resp.), don't carryforward the state from a previous \
              transaction t0 in b1" =
      let state_hash_and_body1 =
        let state_body0 =
          Coda_state.Protocol_state.negative_one
            ~genesis_ledger:Test_genesis_ledger.t ~constraint_constants
            ~consensus_constants
          |> Coda_state.Protocol_state.body
        in
        let state_body_hash0 =
          Coda_state.Protocol_state.Body.hash state_body0
        in
        (state_body_hash0, state_body0)
      in
      let state_hash_and_body2 = (state_body_hash, state_body) in
      test_base_and_merge ~state_hash_and_body1 ~state_hash_and_body2
        ~carryforward1:false ~carryforward2:false

    let create_account pk token balance =
      Account.create (Account_id.create pk token) (Balance.of_int balance)

    let test_user_command_with_accounts ~constraint_constants ~ledger ~accounts
        ~signer ~fee ~fee_payer_pk ~fee_token ?memo
        ?(valid_until = Global_slot.max_value) ?nonce body =
      let memo =
        match memo with
        | Some memo ->
            memo
        | None ->
            User_command_memo.create_by_digesting_string_exn
              (Test_util.arbitrary_string
                 ~len:User_command_memo.max_digestible_string_length)
      in
      Array.iter accounts ~f:(fun account ->
          Ledger.create_new_account_exn ledger
            (Account.identifier account)
            account ) ;
      let get_account aid =
        Option.bind
          (Ledger.location_of_account ledger aid)
          ~f:(Ledger.get ledger)
      in
      let nonce =
        match nonce with
        | Some nonce ->
            nonce
        | None -> (
          match get_account (Account_id.create fee_payer_pk fee_token) with
          | Some {nonce; _} ->
              nonce
          | None ->
              failwith
                "Could not infer a valid nonce for this test. Provide one \
                 explicitly" )
      in
      let payload =
        User_command.Payload.create ~fee ~fee_payer_pk ~fee_token ~nonce
          ~valid_until ~memo ~body
      in
      let signer = Keypair.of_private_key_exn signer in
      let user_command = User_command.sign signer payload in
      let next_available_token = Ledger.next_available_token ledger in
      test_transaction ~constraint_constants ledger (User_command user_command) ;
      let fee_payer = User_command.Payload.fee_payer payload in
      let source = User_command.Payload.source ~next_available_token payload in
      let receiver =
        User_command.Payload.receiver ~next_available_token payload
      in
      let fee_payer_account = get_account fee_payer in
      let source_account = get_account source in
      let receiver_account = get_account receiver in
      ( `Fee_payer_account fee_payer_account
      , `Source_account source_account
      , `Receiver_account receiver_account )

    let random_int_incl l u = Quickcheck.random_value (Int.gen_incl l u)

    let sub_amount amt bal = Option.value_exn (Balance.sub_amount bal amt)

    let add_amount amt bal = Option.value_exn (Balance.add_amount bal amt)

    let sub_fee fee = sub_amount (Amount.of_fee fee)

    let%test_unit "transfer non-default tokens to a new account: fails but \
                   charges fee" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account source_pk token_id 30_000_000_000 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let amount =
                Amount.of_int (random_int_incl 0 30 * 1_000_000_000)
              in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Payment {source_pk; receiver_pk; token_id; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let source_account = Option.value_exn source_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.equal accounts.(1).balance source_account.balance) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "transfer non-default tokens to an existing account" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account source_pk token_id 30_000_000_000
                 ; create_account receiver_pk token_id 0 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let amount =
                Amount.of_int (random_int_incl 0 30 * 1_000_000_000)
              in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Payment {source_pk; receiver_pk; token_id; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let source_account = Option.value_exn source_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              let expected_source_balance =
                accounts.(1).balance |> sub_amount amount
              in
              assert (
                Balance.equal source_account.balance expected_source_balance ) ;
              let expected_receiver_balance =
                accounts.(2).balance |> add_amount amount
              in
              assert (
                Balance.equal receiver_account.balance
                  expected_receiver_balance ) ) )

    let%test_unit "insufficient account creation fee for non-default token \
                   transfer" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account source_pk token_id 30_000_000_000 |]
              in
              let fee = Fee.of_int 20_000_000_000 in
              let amount =
                Amount.of_int (random_int_incl 0 30 * 1_000_000_000)
              in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Payment {source_pk; receiver_pk; token_id; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let source_account = Option.value_exn source_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              let expected_source_balance = accounts.(1).balance in
              assert (
                Balance.equal source_account.balance expected_source_balance ) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "insufficient source balance for non-default token transfer"
        =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account source_pk token_id 30_000_000_000 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let amount = Amount.of_int 40_000_000_000 in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Payment {source_pk; receiver_pk; token_id; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let source_account = Option.value_exn source_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              let expected_source_balance = accounts.(1).balance in
              assert (
                Balance.equal source_account.balance expected_source_balance ) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "transfer non-existing source" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [|create_account fee_payer_pk fee_token 20_000_000_000|]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let amount = Amount.of_int 20_000_000_000 in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Payment {source_pk; receiver_pk; token_id; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Option.is_none source_account) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "payment predicate failure" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = wallets.(1).account.public_key in
              let receiver_pk = wallets.(2).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account source_pk token_id 30_000_000_000 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let amount = Amount.of_int 20_000_000_000 in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Payment {source_pk; receiver_pk; token_id; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let source_account = Option.value_exn source_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              let expected_source_balance = accounts.(1).balance in
              assert (
                Balance.equal source_account.balance expected_source_balance ) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "delegation predicate failure" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = wallets.(1).account.public_key in
              let receiver_pk = wallets.(2).account.public_key in
              let fee_token = Token_id.default in
              let token_id = Token_id.default in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account source_pk token_id 30_000_000_000
                 ; create_account receiver_pk token_id 30_000_000_000 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Stake_delegation
                     (Set_delegate
                        {delegator= source_pk; new_delegate= receiver_pk}))
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let source_account = Option.value_exn source_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Public_key.Compressed.equal source_account.delegate source_pk
              ) ;
              assert (Option.is_some receiver_account) ) )

    let%test_unit "delegation delegatee does not exist" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let accounts =
                [|create_account fee_payer_pk fee_token 20_000_000_000|]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Stake_delegation
                     (Set_delegate
                        {delegator= source_pk; new_delegate= receiver_pk}))
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let source_account = Option.value_exn source_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Public_key.Compressed.equal source_account.delegate source_pk
              ) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "delegation delegator does not exist" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = wallets.(1).account.public_key in
              let receiver_pk = wallets.(2).account.public_key in
              let fee_token = Token_id.default in
              let token_id = Token_id.default in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account receiver_pk token_id 30_000_000_000 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Stake_delegation
                     (Set_delegate
                        {delegator= source_pk; new_delegate= receiver_pk}))
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Option.is_none source_account) ;
              assert (Option.is_some receiver_account) ) )

    let%test_unit "timed account - transactions" =
      Test_util.with_randomness 123456789 (fun () ->
          let wallets = random_wallets ~n:3 () in
          let sender = wallets.(0) in
          let receivers = Array.to_list wallets |> List.tl_exn in
          let txns_per_receiver = 2 in
          let amount = 8_000_000_000 in
          let txn_fee = 2_000_000_000 in
          let memo =
            User_command_memo.create_by_digesting_string_exn
              (Test_util.arbitrary_string
                 ~len:User_command_memo.max_digestible_string_length)
          in
          let balance = Balance.of_int 100_000_000_000_000 in
          let initial_minimum_balance = Balance.of_int 80_000_000_000_000 in
          let cliff_time = Global_slot.of_int 1000 in
          let vesting_period = Global_slot.of_int 10 in
          let vesting_increment = Amount.of_int 1 in
          let txn_global_slot = Global_slot.of_int 1002 in
          let sender =
            { sender with
              account=
                Or_error.ok_exn
                @@ Account.create_timed
                     (Account.identifier sender.account)
                     balance ~initial_minimum_balance ~cliff_time
                     ~vesting_period ~vesting_increment }
          in
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let _, ucs =
                let receiver_ids =
                  List.init (List.length receivers) ~f:(( + ) 1)
                in
                let receivers =
                  List.fold ~init:receiver_ids
                    (List.init (txns_per_receiver - 1) ~f:Fn.id)
                    ~f:(fun acc _ -> receiver_ids @ acc)
                in
                List.fold receivers ~init:(Account.Nonce.zero, [])
                  ~f:(fun (nonce, txns) receiver ->
                    let uc =
                      user_command_with_wallet wallets ~sender:0 ~receiver
                        amount (Fee.of_int txn_fee) ~fee_token:Token_id.default
                        ~token:Token_id.default nonce memo
                    in
                    (Account.Nonce.succ nonce, txns @ [uc]) )
              in
              Ledger.create_new_account_exn ledger
                (Account.identifier sender.account)
                sender.account ;
              let () =
                List.iter ucs ~f:(fun uc ->
                    test_transaction ~constraint_constants ~txn_global_slot
                      ledger (Transaction.User_command uc) )
              in
              List.iter receivers ~f:(fun receiver ->
                  check_balance
                    (Account.identifier receiver.account)
                    ((amount * txns_per_receiver) - account_fee)
                    ledger ) ;
              check_balance
                (Account.identifier sender.account)
                ( Balance.to_int sender.account.balance
                - (amount + txn_fee) * txns_per_receiver
                  * List.length receivers )
                ledger ) )

    let%test_unit "create own new token" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:1 () in
              let signer = wallets.(0).private_key in
              (* Fee payer is the new token owner. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let fee_token = Token_id.default in
              let accounts =
                [|create_account fee_payer_pk fee_token 20_000_000_000|]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account _also_token_owner_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_new_token
                     {token_owner_pk; disable_new_accounts= false})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
                |> sub_fee constraint_constants.account_creation_fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (
                Public_key.Compressed.(equal empty)
                  token_owner_account.delegate ) ;
              assert (
                token_owner_account.token_permissions
                = Token_owned {disable_new_accounts= false} ) ) )

    let%test_unit "create new token for a different pk" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee payer and new token owner are distinct. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let accounts =
                [|create_account fee_payer_pk fee_token 20_000_000_000|]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account _also_token_owner_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_new_token
                     {token_owner_pk; disable_new_accounts= false})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
                |> sub_fee constraint_constants.account_creation_fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (
                Public_key.Compressed.(equal empty)
                  token_owner_account.delegate ) ;
              assert (
                token_owner_account.token_permissions
                = Token_owned {disable_new_accounts= false} ) ) )

    let%test_unit "create new token for a different pk new accounts disabled" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee payer and new token owner are distinct. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let accounts =
                [|create_account fee_payer_pk fee_token 20_000_000_000|]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account _also_token_owner_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_new_token {token_owner_pk; disable_new_accounts= true})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
                |> sub_fee constraint_constants.account_creation_fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (
                Public_key.Compressed.(equal empty)
                  token_owner_account.delegate ) ;
              assert (
                token_owner_account.token_permissions
                = Token_owned {disable_new_accounts= true} ) ) )

    let%test_unit "create own new token account" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and receiver are the same, token owner differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = fee_payer_pk in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
                |> sub_fee constraint_constants.account_creation_fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Balance.(equal zero) receiver_account.balance) ;
              assert (
                Public_key.Compressed.(equal empty) receiver_account.delegate
              ) ;
              assert (
                receiver_account.token_permissions
                = Not_owned {account_disabled= false} ) ) )

    let%test_unit "create new token account for a different pk" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer, receiver, and token owner differ. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = wallets.(2).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
                |> sub_fee constraint_constants.account_creation_fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Balance.(equal zero) receiver_account.balance) ;
              assert (
                Public_key.Compressed.(equal empty) receiver_account.delegate
              ) ;
              assert (
                receiver_account.token_permissions
                = Not_owned {account_disabled= false} ) ) )

    let%test_unit "create new token account for a different pk in a locked \
                   token" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and token owner are the same, receiver differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions= Token_owned {disable_new_accounts= true}
                   } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
                |> sub_fee constraint_constants.account_creation_fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Balance.(equal zero) receiver_account.balance) ;
              assert (
                Public_key.Compressed.(equal empty) receiver_account.delegate
              ) ;
              assert (
                receiver_account.token_permissions
                = Not_owned {account_disabled= false} ) ) )

    let%test_unit "create new own locked token account in a locked token" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and receiver are the same, token owner differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = fee_payer_pk in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions= Token_owned {disable_new_accounts= true}
                   } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= true })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
                |> sub_fee constraint_constants.account_creation_fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Balance.(equal zero) receiver_account.balance) ;
              assert (
                Public_key.Compressed.(equal empty) receiver_account.delegate
              ) ;
              assert (
                receiver_account.token_permissions
                = Not_owned {account_disabled= true} ) ) )

    let%test_unit "create new token account fails for locked token, non-owner \
                   fee-payer" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer, receiver, and token owner differ. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = wallets.(2).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions= Token_owned {disable_new_accounts= true}
                   } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "create new locked token account fails for unlocked token, \
                   non-owner fee-payer" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer, receiver, and token owner differ. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = wallets.(2).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= true })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "create new token account fails if account exists" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer, receiver, and token owner differ. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = wallets.(2).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} }
                 ; create_account receiver_pk token_id 0 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let receiver_account = Option.value_exn receiver_account in
              (* No account creation fee: the command fails. *)
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Balance.(equal zero) receiver_account.balance) ) )

    let%test_unit "create new token account fails if receiver is token owner" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Receiver and token owner are the same, fee-payer differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = token_owner_pk in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let receiver_account = Option.value_exn receiver_account in
              (* No account creation fee: the command fails. *)
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Balance.(equal zero) receiver_account.balance) ) )

    let%test_unit "create new token account fails if claimed token owner \
                   doesn't own the token" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer, receiver, and token owner differ. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = wallets.(2).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account token_owner_pk token_id 0 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              (* No account creation fee: the command fails. *)
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) token_owner_account.balance) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "create new token account works for default token" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and receiver are the same, token owner differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id = Token_id.default in
              let accounts =
                [|create_account fee_payer_pk fee_token 20_000_000_000|]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account _token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk
                     ; token_id
                     ; receiver_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
                |> sub_fee constraint_constants.account_creation_fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.(equal zero) receiver_account.balance) ;
              assert (
                Public_key.Compressed.equal receiver_pk
                  receiver_account.delegate ) ;
              assert (
                receiver_account.token_permissions
                = Not_owned {account_disabled= false} ) ) )

    let%test_unit "mint tokens in owner's account" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:1 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer, receiver, and token owner are the same. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let receiver_pk = fee_payer_pk in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let amount =
                Amount.of_int (random_int_incl 2 15 * 1_000_000_000)
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account _token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Mint_tokens {token_owner_pk; token_id; receiver_pk; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              let expected_receiver_balance =
                accounts.(1).balance |> add_amount amount
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal expected_receiver_balance
                  receiver_account.balance ) ) )

    let%test_unit "mint tokens in another pk's account" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and token owner are the same, receiver differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let amount =
                Amount.of_int (random_int_incl 2 15 * 1_000_000_000)
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} }
                 ; create_account receiver_pk token_id 0 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Mint_tokens {token_owner_pk; token_id; receiver_pk; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let receiver_account = Option.value_exn receiver_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              let expected_receiver_balance =
                accounts.(2).balance |> add_amount amount
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal accounts.(1).balance token_owner_account.balance
              ) ;
              assert (
                Balance.equal expected_receiver_balance
                  receiver_account.balance ) ) )

    let%test_unit "mint tokens fails if the claimed token owner is not the \
                   token owner" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and token owner are the same, receiver differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let amount =
                Amount.of_int (random_int_incl 2 15 * 1_000_000_000)
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account token_owner_pk token_id 0
                 ; create_account receiver_pk token_id 0 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Mint_tokens {token_owner_pk; token_id; receiver_pk; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let receiver_account = Option.value_exn receiver_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal accounts.(1).balance token_owner_account.balance
              ) ;
              assert (
                Balance.equal accounts.(2).balance receiver_account.balance )
          ) )

    let%test_unit "mint tokens fails if the token owner account is not present"
        =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and token owner are the same, receiver differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let amount =
                Amount.of_int (random_int_incl 2 15 * 1_000_000_000)
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; create_account receiver_pk token_id 0 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Mint_tokens {token_owner_pk; token_id; receiver_pk; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Option.is_none token_owner_account) ;
              assert (
                Balance.equal accounts.(1).balance receiver_account.balance )
          ) )

    let%test_unit "mint tokens fails if the fee-payer does not have \
                   permission to mint" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and receiver are the same, token owner differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = wallets.(1).account.public_key in
              let receiver_pk = fee_payer_pk in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let amount =
                Amount.of_int (random_int_incl 2 15 * 1_000_000_000)
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} }
                 ; create_account receiver_pk token_id 0 |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Mint_tokens {token_owner_pk; token_id; receiver_pk; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let receiver_account = Option.value_exn receiver_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal accounts.(1).balance token_owner_account.balance
              ) ;
              assert (
                Balance.equal accounts.(2).balance receiver_account.balance )
          ) )

    let%test_unit "mint tokens fails if the receiver account is not present" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and fee payer are the same, receiver differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let amount =
                Amount.of_int (random_int_incl 2 15 * 1_000_000_000)
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Mint_tokens {token_owner_pk; token_id; receiver_pk; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal accounts.(1).balance token_owner_account.balance
              ) ;
              assert (Option.is_none receiver_account) ) )

    let%test_unit "set token permissions" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:1 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} } |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account _also_token_owner_account
                  , `Receiver_account token_owner_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Set_token_permissions
                     {token_owner_pk; token_id; disable_new_accounts= true})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal accounts.(1).balance token_owner_account.balance
              ) ;
              assert (
                Token_permissions.equal
                  (Token_owned {disable_new_accounts= true})
                  token_owner_account.token_permissions ) ) )

    let%test_unit "set token permissions fails if not owner" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:1 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and fee payer are the same, receiver differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions= Not_owned {account_disabled= false} }
                |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account _also_token_owner_account
                  , `Receiver_account token_owner_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Set_token_permissions
                     {token_owner_pk; token_id; disable_new_accounts= true})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal accounts.(1).balance token_owner_account.balance
              ) ;
              assert (
                Token_permissions.equal
                  (Not_owned {account_disabled= false})
                  token_owner_account.token_permissions ) ) )

    let%test_unit "set account permissions" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and fee payer are the same, receiver differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let target_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions=
                       Token_owned {disable_new_accounts= false} }
                 ; { (create_account target_pk token_id 0) with
                     token_permissions= Not_owned {account_disabled= true} }
                |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account target_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Set_account_permissions
                     {token_owner_pk; token_id; target_pk; disabled= false})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let target_account = Option.value_exn target_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal accounts.(1).balance token_owner_account.balance
              ) ;
              assert (Balance.equal accounts.(2).balance target_account.balance) ;
              assert (
                Token_permissions.equal
                  (Not_owned {account_disabled= false})
                  target_account.token_permissions ) ) )

    let%test_unit "set account permissions fails if not token owner" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer and fee payer are the same, receiver differs. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let token_owner_pk = fee_payer_pk in
              let target_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account token_owner_pk token_id 0) with
                     token_permissions= Not_owned {account_disabled= false} }
                 ; { (create_account target_pk token_id 0) with
                     token_permissions= Not_owned {account_disabled= true} }
                |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account target_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Set_account_permissions
                     {token_owner_pk; token_id; target_pk; disabled= false})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let token_owner_account = Option.value_exn token_owner_account in
              let target_account = Option.value_exn target_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (
                Balance.equal accounts.(1).balance token_owner_account.balance
              ) ;
              assert (Balance.equal accounts.(2).balance target_account.balance) ;
              assert (
                Token_permissions.equal
                  (Not_owned {account_disabled= true})
                  target_account.token_permissions ) ) )

    let%test_unit "disabled accounts cannot send transactions" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:2 () in
              let signer = wallets.(0).private_key in
              let fee_payer_pk = wallets.(0).account.public_key in
              let source_pk = fee_payer_pk in
              let receiver_pk = wallets.(1).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [| create_account fee_payer_pk fee_token 20_000_000_000
                 ; { (create_account source_pk token_id 20_000_000_000) with
                     token_permissions= Not_owned {account_disabled= true} }
                 ; { (create_account receiver_pk token_id 0) with
                     token_permissions= Not_owned {account_disabled= false} }
                |]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let amount =
                Amount.of_int (random_int_incl 2 15 * 1_000_000_000)
              in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account source_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Payment {source_pk; receiver_pk; token_id; amount})
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              let source_account = Option.value_exn source_account in
              let receiver_account = Option.value_exn receiver_account in
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Balance.equal accounts.(1).balance source_account.balance) ;
              assert (
                Balance.equal accounts.(2).balance receiver_account.balance )
          ) )
  end )

let%test_module "account timing check" =
  ( module struct
    open Core_kernel
    open Coda_numbers
    open Currency
    open Transaction_validator.For_tests

    (* test that unchecked and checked calculations for timing agree *)

    let make_checked_computation account txn_amount txn_global_slot =
      let account = Account.var_of_t account in
      let txn_amount = Amount.var_of_t txn_amount in
      let txn_global_slot = Global_slot.Checked.constant txn_global_slot in
      let open Snarky.Checked.Let_syntax in
      let%map _, timing =
        Base.check_timing ~balance_check:Tick.Boolean.Assert.is_true
          ~timed_balance_check:Tick.Boolean.Assert.is_true ~account ~txn_amount
          ~txn_global_slot
      in
      Snarky.As_prover.read Account.Timing.typ timing

    let run_checked_timing_and_compare account txn_amount txn_global_slot
        unchecked_timing =
      let checked_computation =
        make_checked_computation account txn_amount txn_global_slot
      in
      let (), checked_timing =
        Or_error.ok_exn
        @@ Snark_params.Tick.run_and_check checked_computation ()
      in
      Account.Timing.equal checked_timing unchecked_timing

    (* confirm the checked computation fails *)
    let checked_timing_should_fail account txn_amount txn_global_slot =
      let checked_computation =
        make_checked_computation account txn_amount txn_global_slot
      in
      Or_error.is_error
      @@ Snark_params.Tick.run_and_check checked_computation ()

    let%test "before_cliff_time" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 80_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 1_000_000_000 in
      let txn_amount = Currency.Amount.of_int 100_000_000_000 in
      let txn_global_slot = Global_slot.of_int 45 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~vesting_period ~vesting_increment
      in
      let timing = validate_timing ~txn_amount ~txn_global_slot ~account in
      match timing with
      | Ok (Timed _ as unchecked_timing) ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing
      | _ ->
          false

    let%test "positive min balance" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 100_000_000_000 in
      let txn_global_slot = Coda_numbers.Global_slot.of_int 1_900 in
      let timing =
        validate_timing ~account
          ~txn_amount:(Currency.Amount.of_int 100_000_000_000)
          ~txn_global_slot:(Coda_numbers.Global_slot.of_int 1_900)
      in
      (* we're 900 slots past the cliff, which is 90 vesting periods
          subtract 90 * 100 = 9,000 from init min balance of 10,000 to get 1000
          so we should still be timed
        *)
      match timing with
      | Ok (Timed _ as unchecked_timing) ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing
      | _ ->
          false

    let%test "curr min balance of zero" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1_000 in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 100_000_000_000 in
      let txn_global_slot = Global_slot.of_int 2_000 in
      let timing = validate_timing ~txn_amount ~txn_global_slot ~account in
      (* we're 2_000 - 1_000 = 1_000 slots past the cliff, which is 100 vesting periods
          subtract 100 * 100_000_000_000 = 10_000_000_000_000 from init min balance
          of 10_000_000_000 to get zero, so we should be untimed now
        *)
      match timing with
      | Ok (Untimed as unchecked_timing) ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing
      | _ ->
          false

    let%test "below calculated min balance" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 10_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1_000 in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 101_000_000_000 in
      let txn_global_slot = Coda_numbers.Global_slot.of_int 1_010 in
      let timing = validate_timing ~txn_amount ~txn_global_slot ~account in
      match timing with
      | Error _ ->
          checked_timing_should_fail account txn_amount txn_global_slot
      | _ ->
          false

    let%test "insufficient balance" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 100_001_000_000_000 in
      let txn_global_slot = Global_slot.of_int 2000_000_000_000 in
      let timing = validate_timing ~txn_amount ~txn_global_slot ~account in
      match timing with
      | Error _ ->
          checked_timing_should_fail account txn_amount txn_global_slot
      | _ ->
          false

    let%test "past full vesting" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~vesting_period ~vesting_increment
      in
      (* fully vested, curr min balance = 0, so we can spend the whole balance *)
      let txn_amount = Currency.Amount.of_int 100_000_000_000_000 in
      let txn_global_slot = Global_slot.of_int 3000 in
      let timing = validate_timing ~txn_amount ~txn_global_slot ~account in
      match timing with
      | Ok (Untimed as unchecked_timing) ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing
      | _ ->
          false
  end )

let constraint_system_digests () =
  let module W = Wrap (struct
    let merge = Verification_keys.dummy.merge

    let base = Verification_keys.dummy.base
  end) in
  let digest = Tick.R1CS_constraint_system.digest in
  let digest' = Tock.R1CS_constraint_system.digest in
  [ ( "transaction-merge"
    , digest Merge.(Tick.constraint_system ~exposing:(input ()) main) )
  ; ( "transaction-base"
    , digest
        Base.(
          Tick.constraint_system ~exposing:(tick_input ())
            (main
               ~constraint_constants:
                 Genesis_constants.Constraint_constants.compiled)) )
  ; ( "transaction-wrap"
    , digest' W.(Tock.constraint_system ~exposing:wrap_input main) ) ]
