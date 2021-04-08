open Core
open Signature_lib
open Mina_base
open Snark_params
module Global_slot = Mina_numbers.Global_slot
open Currency
open Pickles_types
module Impl = Pickles.Impls.Step

let top_hash_logging_enabled = ref false

let to_preunion (t : Transaction.t) =
  match t with
  | Command (Signed_command x) ->
      `Transaction (Transaction.Command x)
  | Fee_transfer x ->
      `Transaction (Fee_transfer x)
  | Coinbase x ->
      `Transaction (Coinbase x)
  | Command (Snapp_command x) ->
      `Snapp_command x

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
  end

  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type 'pending_coinbase t =
          {source: 'pending_coinbase; target: 'pending_coinbase}
        [@@deriving sexp, hash, compare, eq, fields, yojson, hlist]

        let to_latest pending_coinbase {source; target} =
          {source= pending_coinbase source; target= pending_coinbase target}
      end
    end]

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
             , 'sok_digest )
             t =
          { source: 'ledger_hash
          ; target: 'ledger_hash
          ; supply_increase: 'amount
          ; pending_coinbase_stack_state: 'pending_coinbase
          ; fee_excess: 'fee_excess
          ; next_available_token_before: 'token_id
          ; next_available_token_after: 'token_id
          ; sok_digest: 'sok_digest }
        [@@deriving compare, equal, hash, sexp, yojson, hlist]

        let to_latest ledger_hash amount pending_coinbase fee_excess' token_id
            sok_digest'
            { source
            ; target
            ; supply_increase
            ; pending_coinbase_stack_state
            ; fee_excess
            ; next_available_token_before
            ; next_available_token_after
            ; sok_digest } =
          { source= ledger_hash source
          ; target= ledger_hash target
          ; supply_increase= amount supply_increase
          ; pending_coinbase_stack_state=
              pending_coinbase pending_coinbase_stack_state
          ; fee_excess= fee_excess' fee_excess
          ; next_available_token_before= token_id next_available_token_before
          ; next_available_token_after= token_id next_available_token_after
          ; sok_digest= sok_digest' sok_digest }
      end
    end]

    let typ ledger_hash amount pending_coinbase fee_excess token_id sok_digest
        =
      Tick.Typ.of_hlistable
        [ ledger_hash
        ; ledger_hash
        ; amount
        ; pending_coinbase
        ; fee_excess
        ; token_id
        ; token_id
        ; sok_digest ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
  end

  type ( 'ledger_hash
       , 'amount
       , 'pending_coinbase
       , 'fee_excess
       , 'token_id
       , 'sok_digest )
       poly =
        ( 'ledger_hash
        , 'amount
        , 'pending_coinbase
        , 'fee_excess
        , 'token_id
        , 'sok_digest )
        Poly.t =
    { source: 'ledger_hash
    ; target: 'ledger_hash
    ; supply_increase: 'amount
    ; pending_coinbase_stack_state: 'pending_coinbase
    ; fee_excess: 'fee_excess
    ; next_available_token_before: 'token_id
    ; next_available_token_after: 'token_id
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
        , unit )
        Poly.Stable.V1.t
      [@@deriving compare, equal, hash, sexp, yojson]

      let to_latest = Fn.id
    end
  end]

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
          , Sok_message.Digest.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving compare, equal, hash, sexp, yojson]

        let to_latest = Fn.id
      end
    end]

    type var =
      ( Frozen_ledger_hash.var
      , Currency.Amount.var
      , Pending_coinbase_stack_state.var
      , Fee_excess.var
      , Token_id.var
      , Sok_message.Digest.Checked.t )
      Poly.t

    let typ : (var, t) Tick.Typ.t =
      Poly.typ Frozen_ledger_hash.typ Currency.Amount.typ
        Pending_coinbase_stack_state.typ Fee_excess.typ Token_id.typ
        Sok_message.Digest.typ

    let to_input
        { source
        ; target
        ; supply_increase
        ; pending_coinbase_stack_state
        ; fee_excess
        ; next_available_token_before
        ; next_available_token_after
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
      type t = var

      let to_input
          { source
          ; target
          ; supply_increase
          ; pending_coinbase_stack_state
          ; fee_excess
          ; next_available_token_before
          ; next_available_token_after
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
        Tick.Run.run_checked (to_input t >>| Random_oracle.Checked.pack_input)
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
    and () =
      if Frozen_ledger_hash.equal s1.target s2.source then return ()
      else
        Or_error.errorf
          !"Target ledger hash of statement 1 (%{sexp: Frozen_ledger_hash.t}) \
            does not match source ledger hash of statement 2 (%{sexp: \
            Frozen_ledger_hash.t})"
          s1.target s2.source
    in
    ( { source= s1.source
      ; target= s2.target
      ; fee_excess
      ; next_available_token_before= s1.next_available_token_before
      ; next_available_token_after= s2.next_available_token_after
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
    in
    ( { source
      ; target
      ; fee_excess
      ; next_available_token_before
      ; next_available_token_after
      ; supply_increase
      ; pending_coinbase_stack_state=
          {source= pending_coinbase_before; target= pending_coinbase_after}
      ; sok_digest= () }
      : t )
end

module Proof = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Pickles.Proof.Branching_2.Stable.V1.t
      [@@deriving version {asserted}, yojson, bin_io, compare, sexp]

      let to_latest = Fn.id
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      {statement: Statement.With_sok.Stable.V1.t; proof: Proof.Stable.V1.t}
    [@@deriving compare, fields, sexp, version, yojson]

    let to_latest = Fn.id
  end
end]

let proof t = t.proof

let statement t = {t.statement with sok_digest= ()}

let sok_digest t = t.statement.sok_digest

let to_yojson = Stable.Latest.to_yojson

let create ~source ~target ~supply_increase ~pending_coinbase_stack_state
    ~fee_excess ~next_available_token_before ~next_available_token_after
    ~sok_digest ~proof =
  { statement=
      { source
      ; target
      ; next_available_token_before
      ; next_available_token_after
      ; supply_increase
      ; pending_coinbase_stack_state
      ; fee_excess
      ; sok_digest }
  ; proof }

open Tick
open Let_syntax

let chain if_ b ~then_ ~else_ =
  let%bind then_ = then_ and else_ = else_ in
  if_ b ~then_ ~else_

module Base = struct
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
      ; source_minimum_balance_violation: 'bool (* Payment only *)
      ; source_bad_timing: 'bool (* Payment only *)
      ; receiver_exists: 'bool (* Create_account only *)
      ; not_token_owner: 'bool (* Create_account, Mint_tokens *)
      ; token_auth: 'bool (* Create_account *) }

    let num_fields = 11

    let to_list
        { predicate_failed
        ; source_not_present
        ; receiver_not_present
        ; amount_insufficient_to_create
        ; token_cannot_create
        ; source_insufficient_balance
        ; source_minimum_balance_violation
        ; source_bad_timing
        ; receiver_exists
        ; not_token_owner
        ; token_auth } =
      [ predicate_failed
      ; source_not_present
      ; receiver_not_present
      ; amount_insufficient_to_create
      ; token_cannot_create
      ; source_insufficient_balance
      ; source_minimum_balance_violation
      ; source_bad_timing
      ; receiver_exists
      ; not_token_owner
      ; token_auth ]

    let of_list = function
      | [ predicate_failed
        ; source_not_present
        ; receiver_not_present
        ; amount_insufficient_to_create
        ; token_cannot_create
        ; source_insufficient_balance
        ; source_minimum_balance_violation
        ; source_bad_timing
        ; receiver_exists
        ; not_token_owner
        ; token_auth ] ->
          { predicate_failed
          ; source_not_present
          ; receiver_not_present
          ; amount_insufficient_to_create
          ; token_cannot_create
          ; source_insufficient_balance
          ; source_minimum_balance_violation
          ; source_bad_timing
          ; receiver_exists
          ; not_token_owner
          ; token_auth }
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
              | Payment | Stake_delegation | Mint_tokens ->
                  (* TODO(#4554): Hook predicate evaluation in here once
                     implemented.
                  *)
                  (true, false)
              | Fee_transfer | Coinbase ->
                  assert false
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
              ; source_minimum_balance_violation= false
              ; source_bad_timing= false
              ; receiver_exists= false
              ; not_token_owner= false
              ; token_auth= false }
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
              let timing_or_error =
                Transaction_logic.validate_timing
                  ~txn_amount:payload.body.amount ~txn_global_slot
                  ~account:source_account
              in
              let source_minimum_balance_violation =
                match timing_or_error with
                | Ok _ ->
                    false
                | Error err ->
                    let open Mina_base in
                    Transaction_status.Failure.equal
                      (Transaction_logic.timing_error_to_user_command_status
                         err)
                      Transaction_status.Failure
                      .Source_minimum_balance_violation
              in
              let source_bad_timing =
                (* This failure is fatal if fee-payer and source account are
                   the same. This is checked in the transaction pool.
                *)
                (not fee_payer_is_source)
                && (not source_insufficient_balance)
                && Or_error.is_error timing_or_error
              in
              { predicate_failed
              ; source_not_present
              ; receiver_not_present= false
              ; amount_insufficient_to_create
              ; token_cannot_create
              ; source_insufficient_balance
              ; source_minimum_balance_violation
              ; source_bad_timing
              ; receiver_exists= false
              ; not_token_owner= false
              ; token_auth= false }
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
              let ret =
                { predicate_failed= false
                ; source_not_present
                ; receiver_not_present= false
                ; amount_insufficient_to_create= false
                ; token_cannot_create= false
                ; source_insufficient_balance= false
                ; source_minimum_balance_violation= false
                ; source_bad_timing= false
                ; receiver_exists
                ; not_token_owner
                ; token_auth }
              in
              (* Note: This logic is dependent upon all failures above, so we
                 have to calculate it separately here. *)
              if
                (* If we think the source exists *)
                (not source_not_present)
                (* and there is a failure *)
                && List.exists ~f:Fn.id (to_list ret)
                (* and the receiver account did not exist *)
                && (not receiver_exists)
                (* and the source account was the receiver account *)
                && Account_id.equal source receiver
              then
                (* then the receiver account will not be initialized, and so
                   the source (=receiver) account will not be present.
                *)
                { ret with
                  source_not_present= true
                ; not_token_owner=
                    not Token_id.(equal default (Account_id.token_id receiver))
                ; token_auth=
                    not ((not payload.body.token_locked) || predicate_result)
                }
              else ret
          | Mint_tokens ->
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
              ; source_minimum_balance_violation= false
              ; source_bad_timing= false
              ; receiver_exists= false
              ; not_token_owner
              ; token_auth= false } )

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

  (* Currently, a circuit must have at least 1 of every type of constraint. *)
  let dummy_constraints () =
    make_checked
      Impl.(
        fun () ->
          let b = exists Boolean.typ_unchecked ~compute:(fun _ -> true) in
          let g = exists Inner_curve.typ ~compute:(fun _ -> Inner_curve.one) in
          let _ =
            Pickles.Step_main_inputs.Ops.scale_fast g
              (`Plus_two_to_len [|b; b|])
          in
          let _ =
            Pickles.Pairing_main.Scalar_challenge.endo g (Scalar_challenge [b])
          in
          ())

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
        ; cliff_amount
        ; vesting_period
        ; vesting_increment } =
      account.timing
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
      Account.Checked.min_balance_at_slot ~global_slot:txn_global_slot
        ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
        ~initial_minimum_balance
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
    let%bind is_untimed = Boolean.((not is_timed) ||| is_timed_balance_zero) in
    let%map timing =
      Account.Timing.if_ is_untimed ~then_:Account.Timing.untimed_var
        ~else_:account.timing
    in
    (`Min_balance curr_min_balance, timing)

  let side_loaded i =
    let open Snapp_statement in
    Pickles.Side_loaded.create ~typ ~name:(sprintf "snapp_%d" i)
      ~max_num_input_proofs:
        (module Pickles.Side_loaded.Verification_key.Max_num_input_proofs)
      ~value_to_field_elements:to_field_elements
      ~var_to_field_elements:Checked.to_field_elements

  module Snapp_command = struct
    include struct
      open Snarky_backendless.Request

      type _ t +=
        | State_body : Mina_state.Protocol_state.Body.Value.t t
        | Snapp_account : [`One | `Two] -> Snapp_account.t t
        | Fee_payer_signature : Signature.t t
        | Account_signature : [`One | `Two] -> Signature.t t
        | Zero_complement : Snapp_command.Payload.Zero_proved.t t
        | One_complement : Snapp_statement.Complement.One_proved.t t
        | Two_complement : Snapp_statement.Complement.Two_proved.t t
    end

    let handler ~(state_body : Mina_state.Protocol_state.Body.Value.t)
        ~(snapp_account1 : Snapp_account.t option)
        ~(snapp_account2 : Snapp_account.t option) (c : Snapp_command.t)
        handler : request -> response =
     fun (With {request; respond} as r) ->
      let Vector.[snapp_account1; snapp_account2] =
        Vector.map
          ~f:(Option.value ~default:Snapp_account.default)
          [snapp_account1; snapp_account2]
      in
      let sig1, sig2 =
        let control : Control.t -> Signature.t = function
          | Signature x ->
              x
          | Both {signature; _} ->
              signature
          | Proof _ | None_given ->
              Signature.dummy
        in
        let opt_dummy f x = Option.value_map x ~f ~default:Signature.dummy in
        let empty (p : Snapp_command.Party.Authorized.Empty.t) =
          let () = p.authorization in
          Signature.dummy
        in
        let signed (p : Snapp_command.Party.Authorized.Signed.t) =
          p.authorization
        in
        match c with
        | Proved_empty r ->
            (control r.one.authorization, opt_dummy empty r.two)
        | Proved_signed r ->
            (control r.one.authorization, signed r.two)
        | Proved_proved r ->
            (control r.one.authorization, control r.two.authorization)
        | Signed_signed r ->
            (signed r.one, signed r.two)
        | Signed_empty r ->
            (signed r.one, opt_dummy empty r.two)
      in
      let payload = Snapp_command.to_payload c in
      match request with
      | State_body ->
          respond (Provide state_body)
      | Snapp_account `One ->
          respond (Provide snapp_account1)
      | Snapp_account `Two ->
          respond (Provide snapp_account2)
      | Fee_payer_signature ->
          respond
            (Provide
               (Option.value_map (Snapp_command.fee_payment c)
                  ~default:Signature.dummy ~f:(fun p -> p.signature)))
      | Account_signature `One ->
          respond (Provide sig1)
      | Account_signature `Two ->
          respond (Provide sig2)
      | Zero_complement -> (
        match payload with
        | Zero_proved x ->
            respond (Provide x)
        | _ ->
            unhandled )
      | One_complement -> (
        match payload with
        | One_proved x ->
            respond (Provide (Snapp_statement.Complement.One_proved.create x))
        | _ ->
            unhandled )
      | Two_complement -> (
        match payload with
        | Two_proved x ->
            respond (Provide (Snapp_statement.Complement.Two_proved.create x))
        | _ ->
            unhandled )
      | _ ->
          handler r

    open Snapp_basic

    let check_fee ~(excess : Amount.Signed.var) ~token_id
        ~(other_fee_payer_opt :
           (_, Other_fee_payer.Payload.Checked.t) Flagged_option.t) =
      let open Impl in
      let ( ! ) = run_checked in
      (* Either
          (other_fee_payment_opt = None AND token_id = default AND excess <= 0)
          OR
          (other_fee_payment_opt = Some AND fee_token_id = default AND excess = 0)
      *)
      let is_default x = !Token_id.(Checked.equal (var_of_t default) x) in
      let token_is_default = is_default token_id in
      let fee_token_is_default =
        is_default other_fee_payer_opt.data.token_id
      in
      let open Boolean in
      let excess_is_zero =
        !(Amount.(Checked.equal (var_of_t zero)) excess.magnitude)
      in
      Assert.any
        [ all
            [ not other_fee_payer_opt.is_some
            ; token_is_default
            ; any [Sgn.Checked.is_neg excess.sgn; excess_is_zero] ]
        ; all
            [other_fee_payer_opt.is_some; fee_token_is_default; excess_is_zero]
        ]

    let snapp1_tag = side_loaded 1

    let snapp2_tag = side_loaded 2

    let unhash_snapp_account ~which (a : Account.var) :
        Account.Checked.Unhashed.t =
      let open Impl in
      let s =
        exists Snapp_account.typ ~request:(fun () -> Snapp_account which)
      in
      with_label __LOC__ (fun () ->
          Field.Assert.equal (fst a.snapp) (Snapp_account.Checked.digest s) ) ;
      {a with snapp= s}

    let apply_body
        ~(constraint_constants : Genesis_constants.Constraint_constants.t)
        ~(is_new : [`No | `Maybe of Boolean.var]) ?tag ~txn_global_slot
        ~add_check ~check_auth
        ({ pk= _
         ; update= {app_state; delegate; verification_key; permissions}
         ; delta } :
          Snapp_command.Party.Body.Checked.t) (a : Account.Checked.Unhashed.t)
        : Account.var * _ =
      let open Impl in
      let r = ref [] in
      let update_authorized (type a) perm ~is_keep
          ~(updated : [`Ok of a | `Flagged of a * Boolean.var]) =
        let speculative_success, `proof_must_verify x = check_auth perm in
        r := lazy Boolean.((not is_keep) &&& x) :: !r ;
        match updated with
        | `Ok res ->
            add_check ?label:(Some __LOC__)
              Boolean.(speculative_success ||| is_keep) ;
            res
        | `Flagged (res, failed) ->
            add_check ?label:(Some __LOC__)
              Boolean.((not failed) &&& speculative_success ||| is_keep) ;
            res
      in
      let proof_must_verify () = Boolean.any (List.map !r ~f:Lazy.force) in
      let ( ! ) = run_checked in
      let is_receiver = Sgn.Checked.is_pos delta.sgn in
      let `Min_balance _, timing =
        !([%with_label "Check snapp timing"]
            (let open Tick in
            let balance_check ok =
              [%with_label "Check snapp balance"]
                (Boolean.Assert.any [ok; is_receiver])
            in
            let timed_balance_check ok =
              [%with_label "Check snapp timed balance"]
                (Boolean.Assert.any [ok; is_receiver])
            in
            check_timing ~balance_check ~timed_balance_check ~account:a
              ~txn_amount:delta.magnitude ~txn_global_slot))
      in
      let timing =
        !(Account.Timing.if_ is_receiver ~then_:a.timing ~else_:timing)
      in
      (* Check send/receive permissions *)
      let balance =
        with_label __LOC__ (fun () ->
            update_authorized
              (Permissions.Auth_required.Checked.if_ is_receiver
                 ~then_:a.permissions.receive ~else_:a.permissions.send)
              ~is_keep:!Amount.Signed.(Checked.(equal (constant zero) delta))
              ~updated:
                (let balance, `Overflow failed1 =
                   !(Balance.Checked.add_signed_amount_flagged a.balance delta)
                 in
                 match is_new with
                 | `No ->
                     `Flagged (balance, failed1)
                 | `Maybe is_new ->
                     let fee =
                       Amount.Checked.of_fee
                         (Fee.var_of_t
                            constraint_constants.account_creation_fee)
                     in
                     let balance_when_new, `Underflow failed2 =
                       !(Balance.Checked.sub_amount_flagged balance fee)
                     in
                     let res =
                       !(Balance.Checked.if_ is_new ~then_:balance_when_new
                           ~else_:balance)
                     in
                     let failed = Boolean.(failed1 ||| (is_new &&& failed2)) in
                     `Flagged (res, failed)) )
      in
      let snapp =
        let app_state =
          with_label __LOC__ (fun () ->
              update_authorized a.permissions.edit_state
                ~is_keep:
                  (Boolean.all
                     (List.map (Vector.to_list app_state)
                        ~f:Set_or_keep.Checked.is_keep))
                ~updated:
                  (`Ok
                    (Vector.map2 app_state a.snapp.app_state
                       ~f:(Set_or_keep.Checked.set_or_keep ~if_:Field.if_))) )
        in
        Option.iter tag ~f:(fun t ->
            Pickles.Side_loaded.in_circuit t a.snapp.verification_key.data ) ;
        let verification_key =
          update_authorized a.permissions.set_verification_key
            ~is_keep:(Set_or_keep.Checked.is_keep verification_key)
            ~updated:
              (`Ok
                (Set_or_keep.Checked.set_or_keep ~if_:Field.if_
                   verification_key
                   (Lazy.force a.snapp.verification_key.hash)))
        in
        let snapp' = {Snapp_account.verification_key; app_state} in
        let r =
          As_prover.Ref.create
            As_prover.(
              fun () ->
                Some
                  ( { verification_key=
                        (* Huge hack. This relies on the fact that the "data" is not
                      used for computing the hash of the snapp account. We can't
                      provide the verification key since it's not available here. *)
                        Some
                          { With_hash.data= Side_loaded_verification_key.dummy
                          ; hash= read_var snapp'.verification_key }
                    ; app_state=
                        read (Snapp_state.typ Field.typ) snapp'.app_state }
                    : Snapp_account.t ))
        in
        (Snapp_account.Checked.digest' snapp', r)
      in
      let delegate =
        update_authorized a.permissions.set_delegate
          ~is_keep:(Set_or_keep.Checked.is_keep delegate)
          ~updated:
            (`Ok
              (Set_or_keep.Checked.set_or_keep
                 ~if_:(fun b ~then_ ~else_ ->
                   !(Public_key.Compressed.Checked.if_ b ~then_ ~else_) )
                 delegate a.delegate))
      in
      let permissions =
        update_authorized a.permissions.set_permissions
          ~is_keep:(Set_or_keep.Checked.is_keep permissions)
          ~updated:
            (`Ok
              (Set_or_keep.Checked.set_or_keep ~if_:Permissions.Checked.if_
                 permissions a.permissions))
      in
      ( {a with balance; snapp; delegate; permissions; timing}
      , `proof_must_verify proof_must_verify )

    let assert_account_present public_key (acct : Account.var) ~is_new =
      let%bind account_there =
        Public_key.Compressed.Checked.equal acct.public_key public_key
      in
      let open Boolean in
      match is_new with
      | `Maybe is_new ->
          let%bind is_empty =
            Public_key.Compressed.Checked.equal acct.public_key
              Public_key.Compressed.(var_of_t empty)
          in
          let%bind there_ok = (not is_new) &&& account_there in
          let%bind empty_ok = is_new &&& is_empty in
          with_label __LOC__ (Assert.any [there_ok; empty_ok])
      | `No ->
          Assert.is_true account_there

    let signature_verifies ~shifted ~payload_digest req pk =
      let%bind signature =
        exists Schnorr.Signature.typ ~request:(As_prover.return req)
      in
      let%bind pk =
        Public_key.decompress_var pk
        (*           (Account_id.Checked.public_key fee_payer_id) *)
      in
      Schnorr.Checked.verifies shifted signature pk
        (Random_oracle.Input.field payload_digest)

    let pay_fee
        ~(constraint_constants : Genesis_constants.Constraint_constants.t)
        ~shifted ~root ~fee ~fee_payer_is_other ~fee_payer_id ~fee_payer_nonce
        ~payload_digest ~txn_global_slot =
      let open Tick in
      let actual_fee_payer_nonce_and_rch = Set_once.create () in
      let%bind signature_verifies =
        signature_verifies Fee_payer_signature
          (Account_id.Checked.public_key fee_payer_id)
          ~payload_digest ~shifted
      in
      let%map root =
        Frozen_ledger_hash.modify_account root fee_payer_id
          ~depth:constraint_constants.ledger_depth
          ~filter:(fun acct ->
            Account_id.Checked.(
              equal fee_payer_id (create acct.public_key acct.token_id))
            >>= Boolean.Assert.is_true )
          ~f:(fun () account ->
            Set_once.set_exn actual_fee_payer_nonce_and_rch [%here]
              (account.nonce, account.receipt_chain_hash) ;
            let%bind () =
              let%bind authorized =
                make_checked (fun () ->
                    Permissions.Auth_required.Checked.eval_no_proof
                      ~signature_verifies account.permissions.send )
              in
              (* It's ok for this signature to fail if there is no separate fee payer.
                Their control will be checked independently. *)
              Boolean.(Assert.any [authorized; not fee_payer_is_other])
            in
            let%bind () =
              [%with_label "Check fee nonce"]
                (let%bind nonce_matches =
                   Account.Nonce.Checked.equal fee_payer_nonce account.nonce
                 in
                 (* If there is not a separate fee payer, its nonce is checked elsewhere *)
                 Boolean.(Assert.any [nonce_matches; not fee_payer_is_other]))
            in
            let%bind next_nonce = Account.Nonce.Checked.succ account.nonce in
            let%bind receipt_chain_hash =
              let current = account.receipt_chain_hash in
              Receipt.Chain_hash.Checked.cons (Snapp_command payload_digest)
                current
            in
            let txn_amount = Amount.Checked.of_fee fee in
            let%bind `Min_balance _, timing =
              [%with_label "Check fee payer timing"]
                (let balance_check ok =
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
            let%map balance =
              [%with_label "Check payer balance"]
                (Balance.Checked.sub_amount account.balance txn_amount)
            in
            { Account.Poly.balance
            ; public_key= account.public_key
            ; token_id= account.token_id
            ; token_permissions= account.token_permissions
            ; nonce= next_nonce
            ; receipt_chain_hash
            ; delegate= account.delegate
            ; voting_for= account.voting_for
            ; timing
            ; permissions= account.permissions
            ; snapp= account.snapp } )
      in
      (root, Set_once.get_exn actual_fee_payer_nonce_and_rch [%here])

    let shouldn't_update_nonce_and_rch ~is_fee_payer ~should_step =
      match should_step with
      | `Yes ->
          is_fee_payer
      | `Maybe should_step ->
          Impl.Boolean.(any [is_fee_payer; not should_step])

    let update_nonce_and_rch ~payload_digest ~is_fee_payer ~should_step
        ~(account : Account.var) =
      let ( ! ) = Impl.run_checked in
      let updated =
        !(Receipt.Chain_hash.Checked.cons (Snapp_command payload_digest)
            account.receipt_chain_hash)
      in
      let shouldn't_update =
        shouldn't_update_nonce_and_rch ~is_fee_payer ~should_step
      in
      let should_update = Boolean.not shouldn't_update in
      { account with
        nonce= !(Account.Nonce.Checked.succ_if account.nonce should_update)
      ; receipt_chain_hash=
          !(Receipt.Chain_hash.Checked.if_ shouldn't_update
              ~then_:account.receipt_chain_hash ~else_:updated) }

    module Check_predicate = struct
      let snapp_self p (a : Account.Checked.Unhashed.t) =
        [ Snapp_predicate.Account.Checked.check_nonsnapp p a
        ; Snapp_predicate.Account.Checked.check_snapp p a.snapp ]

      let snapp_other (o : Snapp_predicate.Other.Checked.t)
          (a : Account.Checked.Unhashed.t) =
        [ Snapp_predicate.Account.Checked.check_nonsnapp o.predicate a
        ; Snapp_predicate.Account.Checked.check_snapp o.predicate a.snapp
        ; Snapp_predicate.Hash.(check_checked Tc.field)
            o.account_vk
            (Lazy.force a.snapp.verification_key.hash)
        ; Snapp_basic.Account_state.Checked.check o.account_transition.prev
            ~is_empty:Boolean.false_
        ; Snapp_basic.Account_state.Checked.check o.account_transition.next
            ~is_empty:Boolean.false_ ]

      let signed_self nonce (a : Account.Checked.Unhashed.t) =
        let open Impl in
        [run_checked (Account.Nonce.Checked.equal nonce a.nonce)]
    end

    let modify
        ~(constraint_constants : Genesis_constants.Constraint_constants.t)
        ~shifted ~txn_global_slot ~add_check ~root ~fee ~fee_payer_nonce
        ~fee_payer_receipt_chain_hash ~token_id ~payload_digest ~is_fee_payer
        ~is_new ~which ~tag ~(body : Snapp_command.Party.Body.Checked.t)
        ~self_predicate ~other_predicate
        ~(*         ~(other_predicate : Snapp_predicate.Other.Checked.t) *)
        check_auth =
      let open Impl in
      let ( ! ) = run_checked in
      let proof_must_verify = Set_once.create () in
      let public_key = body.pk in
      let body =
        (*
          delta = second_delta + (if is_fee_payer then -fee else 0)
          second_delta = delta - (if is_fee_payer then -fee else 0)
          second_delta = delta + (if is_fee_payer then fee else 0)
        *)
        { body with
          delta=
            !(Amount.Signed.Checked.add body.delta
                (Amount.Signed.Checked.of_unsigned
                   !(Amount.Checked.if_ is_fee_payer
                       ~then_:(Amount.Checked.of_fee fee)
                       ~else_:(Amount.var_of_t Amount.zero)))) }
      in
      let root =
        run_checked
          (let%bind signature_verifies =
             signature_verifies (Account_signature which) public_key
               ~payload_digest ~shifted
           in
           Frozen_ledger_hash.modify_account root
             (Account_id.Checked.create public_key token_id)
             ~depth:constraint_constants.ledger_depth
             ~filter:(assert_account_present public_key ~is_new)
             ~f:(fun () account ->
               make_checked (fun () ->
                   let account = unhash_snapp_account ~which account in
                   let account', `proof_must_verify must_verify =
                     apply_body body account ~constraint_constants ~is_new ~tag
                       ~txn_global_slot ~add_check ~check_auth:(fun t ->
                         with_label __LOC__ (fun () ->
                             check_auth t ~signature_verifies ) )
                   in
                   Set_once.set_exn proof_must_verify [%here] must_verify ;
                   let account =
                     !(let%map nonce =
                         Account.Nonce.Checked.if_ is_fee_payer
                           ~then_:fee_payer_nonce ~else_:account.nonce
                       and receipt_chain_hash =
                         Receipt.Chain_hash.Checked.if_ is_fee_payer
                           ~then_:fee_payer_receipt_chain_hash
                           ~else_:account.receipt_chain_hash
                       in
                       {account with nonce; receipt_chain_hash})
                   in
                   List.iter
                     ~f:(add_check ?label:(Some __LOC__))
                     (self_predicate account) ;
                   List.iter
                     ~f:(add_check ?label:(Some __LOC__))
                     (other_predicate account) ;
                   update_nonce_and_rch ~payload_digest ~is_fee_payer
                     ~should_step:`Yes ~account:account' ) ))
      in
      (root, Set_once.get_exn proof_must_verify [%here])

    let compute_fee_excess ~fee ~fee_payer_id =
      (* Use the default token for the fee excess if it is zero.
        This matches the behaviour of [Fee_excess.rebalance], which allows
        [verify_complete_merge] to verify a proof without knowledge of the
        particular fee tokens used.
      *)
      let open Impl in
      let ( ! ) = run_checked in
      let fee_excess_zero = !Fee.(equal_var fee (var_of_t zero)) in
      let fee_token = Account_id.Checked.token_id fee_payer_id in
      let fee_token_l =
        !(Token_id.Checked.if_ fee_excess_zero
            ~then_:Token_id.(var_of_t default)
            ~else_:fee_token)
      in
      { Fee_excess.fee_token_l
      ; fee_excess_l= Fee.Signed.Checked.of_unsigned fee
      ; fee_token_r= Token_id.(var_of_t default)
      ; fee_excess_r= Fee.Signed.(Checked.constant zero) }

    let determine_fee_payer ~token_id
        ~(other_fee_payer_opt :
           (_, Other_fee_payer.Payload.Checked.t) Flagged_option.t)
        ~(body1 : Snapp_command.Party.Body.Checked.t)
        ~(body2 : Snapp_command.Party.Body.Checked.t) =
      let open Impl in
      let ( ! ) = run_checked in
      let fee_payer_is_other = other_fee_payer_opt.is_some in
      let account1_is_sender = Sgn.Checked.is_neg body1.delta.sgn in
      let account1_is_fee_payer, account2_is_fee_payer =
        Boolean.
          ( (not fee_payer_is_other) &&& account1_is_sender
          , (not fee_payer_is_other) &&& not account1_is_sender )
      in
      let fee_payer_id =
        !(Account_id.Checked.if_ fee_payer_is_other
            ~then_:
              (Account_id.Checked.create other_fee_payer_opt.data.pk
                 other_fee_payer_opt.data.token_id)
            ~else_:
              (Account_id.Checked.create
                 !(Public_key.Compressed.Checked.if_ account1_is_sender
                     ~then_:body1.pk ~else_:body2.pk)
                 token_id))
      in
      ( account1_is_fee_payer
      , account2_is_fee_payer
      , fee_payer_is_other
      , fee_payer_id )

    let create_checker () =
      let r = ref [] in
      let finished = ref false in
      ( (fun ?label:_ x ->
          if finished.contents then failwith "finished"
          else r := x :: r.contents )
      , fun () ->
          finished := true ;
          Impl.Boolean.all r.contents )

    module Two_proved = struct
      let main
          ~(constraint_constants : Genesis_constants.Constraint_constants.t)
          (s1 : Snapp_statement.Checked.t) (s2 : Snapp_statement.Checked.t)
          (s : Statement.With_sok.Checked.t) =
        let open Impl in
        let ( ! ) = run_checked in
        let state_body =
          (* TODO: How to check this against the statement? *)
          exists (Mina_state.Protocol_state.Body.typ ~constraint_constants)
            ~request:(fun () -> State_body)
        in
        let curr_state =
          Mina_state.Protocol_state.Body.view_checked state_body
        in
        (* Kind of a hack...
           We must have
           s1.body2 = s2.body1
           and
           s2.body2 = s1.body1
           so to save on hashing, we just throw away the bodies in s2 and replace them. *)
        let s2 =
          let _ = Snapp_statement.Checked.to_field_elements s1 in
          (* pickles uses these values to hash the statement  *)
          let ( := ) x2 x1 =
            Set_once.set_exn x2 [%here] (Set_once.get_exn x1 [%here])
          in
          s2.body1.hash := s1.body2.hash ;
          s2.body2.hash := s1.body1.hash ;
          {s2 with body1= s1.body2; body2= s1.body1}
        in
        let excess =
          !(Amount.Signed.Checked.add s1.body1.data.delta s1.body2.data.delta)
        in
        let ({token_id; other_fee_payer_opt} as comp
              : _ Snapp_statement.Complement.Two_proved.Poly.t) =
          exists Snapp_statement.Complement.Two_proved.typ ~request:(fun () ->
              Two_complement )
        in
        (* Check fee *)
        check_fee ~excess ~token_id ~other_fee_payer_opt ;
        let ( account1_is_fee_payer
            , account2_is_fee_payer
            , fee_payer_is_other
            , fee_payer_id ) =
          determine_fee_payer ~token_id ~other_fee_payer_opt
            ~body1:s1.body1.data ~body2:s1.body2.data
        in
        (* By here, we know that excess is either zero or negative, so we can throw away the sign. *)
        let excess = excess.magnitude in
        let fee =
          !(Fee.Checked.if_ fee_payer_is_other
              ~then_:other_fee_payer_opt.data.fee
              ~else_:(Amount.Checked.to_fee excess))
        in
        let payload : Snapp_command.Payload.Digested.Checked.t =
          Two_proved
            (Snapp_statement.Complement.Two_proved.Checked.complete comp
               ~one:s1 ~two:s2)
        in
        let payload_digest =
          Snapp_command.Payload.Digested.Checked.digest payload
        in
        let (module S) = !(Tick.Inner_curve.Checked.Shifted.create ()) in
        let txn_global_slot = curr_state.global_slot_since_genesis in
        let root = s.source in
        let ( (root as root_after_fee_payer)
            , (fee_payer_nonce, fee_payer_receipt_chain_hash) ) =
          !(pay_fee ~constraint_constants
              ~shifted:(module S)
              ~root ~fee ~fee_payer_is_other ~fee_payer_id
              ~fee_payer_nonce:other_fee_payer_opt.data.nonce ~payload_digest
              ~txn_global_slot)
        in
        let add_check, checks_succeeded = create_checker () in
        add_check
          (Snapp_predicate.Protocol_state.Checked.check
             s1.predicate.data.protocol_state_predicate curr_state) ;
        add_check
          (Snapp_predicate.Protocol_state.Checked.check
             s2.predicate.data.protocol_state_predicate curr_state) ;
        add_check
          (Snapp_predicate.Eq_data.(check_checked (Tc.public_key ()))
             s1.predicate.data.fee_payer
             (Account_id.Checked.public_key fee_payer_id)) ;
        add_check
          (Snapp_predicate.Eq_data.(check_checked (Tc.public_key ()))
             s2.predicate.data.fee_payer
             (Account_id.Checked.public_key fee_payer_id)) ;
        let modify =
          modify ~constraint_constants
            ~shifted:(module S)
            ~txn_global_slot ~add_check ~fee_payer_nonce
            ~fee_payer_receipt_chain_hash ~token_id ~payload_digest ~is_new:`No
            ~check_auth:Permissions.Auth_required.Checked.spec_eval
        in
        let self_pred = Check_predicate.snapp_self in
        let other_pred = Check_predicate.snapp_other in
        let root, proof1_must_verify =
          modify ~root ~is_fee_payer:account1_is_fee_payer ~which:`One ~fee
            ~tag:snapp1_tag ~body:s1.body1.data
            ~self_predicate:(self_pred s1.predicate.data.self_predicate)
            ~other_predicate:(other_pred s2.predicate.data.other)
        in
        let root, proof2_must_verify =
          modify ~root ~is_fee_payer:account2_is_fee_payer ~which:`Two ~fee
            ~tag:snapp2_tag ~body:s1.body2.data
            ~self_predicate:(self_pred s2.predicate.data.self_predicate)
            ~other_predicate:(other_pred s1.predicate.data.other)
        in
        let root =
          !(Frozen_ledger_hash.if_ (checks_succeeded ()) ~then_:root
              ~else_:root_after_fee_payer)
        in
        let fee_excess = compute_fee_excess ~fee ~fee_payer_id in
        !((* TODO: s.pending_coinbase_stack_state assertion *)
          Checked.all_unit
            [ Frozen_ledger_hash.assert_equal root s.target
            ; Currency.Amount.Checked.assert_equal s.supply_increase
                Currency.Amount.(var_of_t zero)
            ; Fee_excess.assert_equal_checked s.fee_excess fee_excess
              (* TODO: These should maybe be able to create tokens *)
            ; Token_id.Checked.Assert.equal s.next_available_token_after
                s.next_available_token_before ]) ;
        (proof1_must_verify (), proof2_must_verify ())

      let _rule ~constraint_constants : _ Pickles.Inductive_rule.t =
        { identifier= "snapp-two-proved"
        ; prevs= [snapp1_tag; snapp2_tag]
        ; main=
            (fun [t1; t2] x ->
              let s1, s2 = main t1 t2 ~constraint_constants x in
              [s1; s2] )
        ; main_value= (fun _ _ -> [true; true]) }
    end

    module One_proved = struct
      let main
          ~(constraint_constants : Genesis_constants.Constraint_constants.t)
          (s1 : Snapp_statement.Checked.t) (s : Statement.With_sok.Checked.t) =
        let open Impl in
        let ( ! ) = run_checked in
        let state_body =
          (* TODO: How to check this against the statement? *)
          exists (Mina_state.Protocol_state.Body.typ ~constraint_constants)
            ~request:(fun () -> State_body)
        in
        let curr_state =
          Mina_state.Protocol_state.Body.view_checked state_body
        in
        let _ = Snapp_statement.Checked.to_field_elements s1 in
        let excess =
          !(Amount.Signed.Checked.add s1.body1.data.delta s1.body2.data.delta)
        in
        let ({ token_id
             ; other_fee_payer_opt
             ; second_starts_empty
             ; second_ends_empty
             ; account2_nonce } as comp
              : _ Snapp_statement.Complement.One_proved.Poly.t) =
          exists Snapp_statement.Complement.One_proved.typ ~request:(fun () ->
              One_complement )
        in
        (* Check fee *)
        check_fee ~excess ~token_id ~other_fee_payer_opt ;
        let ( account1_is_fee_payer
            , account2_is_fee_payer
            , fee_payer_is_other
            , fee_payer_id ) =
          determine_fee_payer ~token_id ~other_fee_payer_opt
            ~body1:s1.body1.data ~body2:s1.body2.data
        in
        (* By here, we know that excess is either zero or negative, so we can throw away the sign. *)
        let excess = excess.magnitude in
        let fee =
          !(Fee.Checked.if_ fee_payer_is_other
              ~then_:other_fee_payer_opt.data.fee
              ~else_:(Amount.Checked.to_fee excess))
        in
        let payload : Snapp_command.Payload.Digested.Checked.t =
          One_proved
            (Snapp_statement.Complement.One_proved.Checked.complete comp
               ~one:s1)
        in
        let payload_digest =
          Snapp_command.Payload.Digested.Checked.digest payload
        in
        let (module S) = !(Tick.Inner_curve.Checked.Shifted.create ()) in
        let txn_global_slot = curr_state.global_slot_since_genesis in
        let ( root_after_fee_payer
            , (fee_payer_nonce, fee_payer_receipt_chain_hash) ) =
          !(pay_fee ~constraint_constants
              ~shifted:(module S)
              ~root:s.source ~fee ~fee_payer_is_other ~fee_payer_id
              ~fee_payer_nonce:other_fee_payer_opt.data.nonce ~payload_digest
              ~txn_global_slot)
        in
        let add_check1, checks_succeeded1 = create_checker () in
        add_check1
          (Snapp_predicate.Protocol_state.Checked.check
             s1.predicate.data.protocol_state_predicate curr_state) ;
        add_check1
          (Snapp_predicate.Eq_data.(check_checked (Tc.public_key ()))
             s1.predicate.data.fee_payer
             (Account_id.Checked.public_key fee_payer_id)) ;
        let root_after_account1, proof1_must_verify =
          modify ~constraint_constants ~fee
            ~shifted:(module S)
            ~txn_global_slot ~add_check:add_check1 ~root:root_after_fee_payer
            ~fee_payer_nonce ~fee_payer_receipt_chain_hash ~token_id
            ~payload_digest
            ~check_auth:Permissions.Auth_required.Checked.spec_eval ~is_new:`No
            ~is_fee_payer:account1_is_fee_payer ~which:`One ~tag:snapp1_tag
            ~body:s1.body1.data
            ~self_predicate:
              (Check_predicate.snapp_self s1.predicate.data.self_predicate)
            ~other_predicate:(fun _ -> [])
        in
        let add_check2, checks_succeeded2 = create_checker () in
        let root_after_account2, _ =
          modify ~constraint_constants ~fee
            ~shifted:(module S)
            ~txn_global_slot ~add_check:add_check2 ~root:root_after_account1
            ~fee_payer_nonce ~fee_payer_receipt_chain_hash ~token_id
            ~payload_digest
            ~check_auth:(fun perm ~signature_verifies ->
              let res =
                Permissions.Auth_required.Checked.eval_no_proof perm
                  ~signature_verifies
              in
              ( Boolean.(res ||| second_starts_empty)
              , `proof_must_verify Boolean.true_ ) )
            ~is_new:(`Maybe second_starts_empty)
            ~is_fee_payer:account2_is_fee_payer ~which:`Two ~tag:snapp2_tag
            ~body:s1.body2.data
            ~self_predicate:(Check_predicate.signed_self account2_nonce)
            ~other_predicate:
              (Check_predicate.snapp_other s1.predicate.data.other)
        in
        (* No deleting accounts for now. *)
        Boolean.(
          Assert.is_true (not (second_ends_empty &&& not second_starts_empty))) ;
        let root =
          let checks_succeeded1 = checks_succeeded1 () in
          let checks_succeeded2 = checks_succeeded2 () in
          let if_ = Frozen_ledger_hash.if_ in
          !(if_ second_ends_empty
              ~then_:
                !(if_ checks_succeeded1 ~then_:root_after_account1
                    ~else_:root_after_fee_payer)
              ~else_:
                !(if_
                    Boolean.(checks_succeeded1 &&& checks_succeeded2)
                    ~then_:root_after_account2 ~else_:root_after_fee_payer))
        in
        let fee_excess = compute_fee_excess ~fee ~fee_payer_id in
        !((* TODO: s.pending_coinbase_stack_state assertion *)
          Checked.all_unit
            [ Frozen_ledger_hash.assert_equal root s.target
            ; Currency.Amount.Checked.assert_equal s.supply_increase
                Currency.Amount.(var_of_t zero)
            ; Fee_excess.assert_equal_checked s.fee_excess fee_excess
              (* TODO: These should maybe be able to create tokens *)
            ; Token_id.Checked.Assert.equal s.next_available_token_after
                s.next_available_token_before ]) ;
        proof1_must_verify ()

      let _rule ~constraint_constants : _ Pickles.Inductive_rule.t =
        { identifier= "snapp-one-proved"
        ; prevs= [snapp1_tag]
        ; main=
            (fun [t1] x ->
              let s1 = main t1 ~constraint_constants x in
              [s1] )
        ; main_value= (fun _ _ -> [true]) }
    end

    module Zero_proved = struct
      let main
          ~(constraint_constants : Genesis_constants.Constraint_constants.t)
          (s : Statement.With_sok.Checked.t) =
        let open Impl in
        let ( ! ) = run_checked in
        let payload =
          exists Snapp_command.Payload.Zero_proved.typ ~request:(fun () ->
              Zero_complement )
        in
        let ({ token_id
             ; other_fee_payer_opt
             ; one
             ; two
             ; second_starts_empty
             ; second_ends_empty }
              : Snapp_command.Payload.Zero_proved.Checked.t) =
          payload
        in
        let state_body =
          (* TODO: How to check this against the statement? *)
          exists (Mina_state.Protocol_state.Body.typ ~constraint_constants)
            ~request:(fun () -> State_body)
        in
        let curr_state =
          Mina_state.Protocol_state.Body.view_checked state_body
        in
        let excess =
          !(Amount.Signed.Checked.add one.body.delta two.body.delta)
        in
        (* Check fee *)
        check_fee ~excess ~token_id ~other_fee_payer_opt ;
        let ( account1_is_fee_payer
            , account2_is_fee_payer
            , fee_payer_is_other
            , fee_payer_id ) =
          determine_fee_payer ~token_id ~other_fee_payer_opt ~body1:one.body
            ~body2:two.body
        in
        (* By here, we know that excess is either zero or negative, so we can throw away the sign. *)
        let excess = excess.magnitude in
        let fee =
          !(Fee.Checked.if_ fee_payer_is_other
              ~then_:other_fee_payer_opt.data.fee
              ~else_:(Amount.Checked.to_fee excess))
        in
        let payload_digest =
          Snapp_command.Payload.(
            Digested.Checked.digest
              (Zero_proved (Zero_proved.Checked.digested payload)))
        in
        let (module S) = !(Tick.Inner_curve.Checked.Shifted.create ()) in
        let txn_global_slot = curr_state.global_slot_since_genesis in
        let ( root_after_fee_payer
            , (fee_payer_nonce, fee_payer_receipt_chain_hash) ) =
          !(pay_fee ~constraint_constants
              ~shifted:(module S)
              ~root:s.source ~fee ~fee_payer_is_other ~fee_payer_id
              ~fee_payer_nonce:other_fee_payer_opt.data.nonce ~payload_digest
              ~txn_global_slot)
        in
        let add_check1, checks_succeeded1 = create_checker () in
        let root_after_account1, _ =
          modify ~constraint_constants ~fee
            ~shifted:(module S)
            ~txn_global_slot ~add_check:add_check1 ~root:root_after_fee_payer
            ~fee_payer_nonce ~fee_payer_receipt_chain_hash ~token_id
            ~payload_digest
            ~check_auth:Permissions.Auth_required.Checked.spec_eval ~is_new:`No
            ~is_fee_payer:account1_is_fee_payer ~which:`One ~tag:snapp1_tag
            ~body:one.body
            ~self_predicate:(fun a ->
              Check_predicate.signed_self one.predicate
                { a with
                  nonce=
                    !(Account.Nonce.Checked.if_ account1_is_fee_payer
                        ~then_:fee_payer_nonce ~else_:a.nonce) } )
            ~other_predicate:(fun _ -> [])
        in
        let add_check2, checks_succeeded2 = create_checker () in
        let root_after_account2, _ =
          modify ~constraint_constants ~fee
            ~shifted:(module S)
            ~txn_global_slot ~add_check:add_check2 ~root:root_after_account1
            ~fee_payer_nonce ~fee_payer_receipt_chain_hash ~token_id
            ~payload_digest
            ~check_auth:(fun perm ~signature_verifies ->
              let res =
                Permissions.Auth_required.Checked.eval_no_proof perm
                  ~signature_verifies
              in
              ( Boolean.(res ||| second_starts_empty)
              , `proof_must_verify Boolean.true_ ) )
            ~is_new:(`Maybe second_starts_empty)
            ~is_fee_payer:account2_is_fee_payer ~which:`Two ~tag:snapp2_tag
            ~body:two.body
            ~self_predicate:(fun a ->
              Check_predicate.signed_self two.predicate
                { a with
                  nonce=
                    !(Account.Nonce.Checked.if_ account2_is_fee_payer
                        ~then_:fee_payer_nonce ~else_:a.nonce) } )
            ~other_predicate:(fun _ -> [])
        in
        (* No deleting accounts for now. *)
        Boolean.(
          Assert.is_true (not (second_ends_empty &&& not second_starts_empty))) ;
        let checks_succeeded1 = checks_succeeded1 () in
        let checks_succeeded2 = checks_succeeded2 () in
        let root =
          let if_ = Frozen_ledger_hash.if_ in
          !(if_ second_ends_empty
              ~then_:
                !(if_ checks_succeeded1 ~then_:root_after_account1
                    ~else_:root_after_fee_payer)
              ~else_:
                !(if_
                    Boolean.(checks_succeeded1 &&& checks_succeeded2)
                    ~then_:root_after_account2 ~else_:root_after_fee_payer))
        in
        let fee_excess = compute_fee_excess ~fee ~fee_payer_id in
        (* TODO: s.pending_coinbase_stack_state assertion *)
        !(Frozen_ledger_hash.assert_equal root s.target) ;
        !(Currency.Amount.Checked.assert_equal s.supply_increase
            Currency.Amount.(var_of_t zero)) ;
        !(Fee_excess.assert_equal_checked s.fee_excess fee_excess) ;
        (* TODO: These should maybe be able to create tokens *)
        !(Token_id.Checked.Assert.equal s.next_available_token_after
            s.next_available_token_before)

      let _rule ~constraint_constants : _ Pickles.Inductive_rule.t =
        { identifier= "snapp-zero-proved"
        ; prevs= []
        ; main=
            (fun [] x ->
              let () = main ~constraint_constants x in
              [] )
        ; main_value= (fun _ _ -> []) }
    end
  end

  type _ Snarky_backendless.Request.t +=
    | Transaction : Transaction_union.t Snarky_backendless.Request.t
    | State_body :
        Mina_state.Protocol_state.Body.Value.t Snarky_backendless.Request.t
    | Init_stack : Pending_coinbase.Stack.t Snarky_backendless.Request.t

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
               [Boolean.not payload.body.token_locked; is_create_account])
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
      Mina_state.Protocol_state.Body.consensus_state state_body
      |> Consensus.Data.Consensus_state.global_slot_since_genesis_var
    in
    let%bind creating_new_token =
      Boolean.(is_create_account &&& token_invalid)
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
        Boolean.(creating_new_token &&& not user_command_fails)
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
           Mina_state.Protocol_state.Body.hash_checked state_body
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
              Boolean.(equal_source ||| equal_source_with_state)
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
      Boolean.(is_own_account ||| predicate_result)
    in
    let%bind () =
      [%with_label "Check predicate failure against predicted"]
        (let%bind predicate_failed =
           Boolean.((not predicate_result) &&& not predicate_deferred)
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
    let is_coinbase_or_fee_transfer = Boolean.not is_user_command in
    let%bind can_create_fee_payer_account =
      (* Fee transfers and coinbases may create an account. We check the normal
         invariants to ensure that the account creation fee is paid.
      *)
      let%bind fee_may_be_charged =
        (* If the fee is zero, we do not create the account at all, so we allow
           this through. Otherwise, the fee must be the default.
        *)
        Boolean.(token_default ||| is_zero_fee)
      in
      Boolean.(is_coinbase_or_fee_transfer &&& fee_may_be_charged)
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
               let%bind r =
                 Receipt.Chain_hash.Checked.cons (Signed_command payload)
                   current
               in
               Receipt.Chain_hash.Checked.if_ is_user_command ~then_:r
                 ~else_:current
             in
             let%bind is_empty_and_writeable =
               (* If this is a coinbase with zero fee, do not create the
                  account, since the fee amount won't be enough to pay for it.
               *)
               Boolean.(is_empty_and_writeable &&& not is_zero_fee)
             in
             let%bind should_pay_to_create =
               (* Coinbases and fee transfers may create, or we may be creating
                  a new token account. These are mutually exclusive, so we can
                  encode this as a boolean.
               *)
               let%bind is_create_account =
                 Boolean.(is_create_account &&& not user_command_fails)
               in
               Boolean.(is_empty_and_writeable ||| is_create_account)
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
             ; timing
             ; permissions= account.permissions
             ; snapp= account.snapp } ))
    in
    let%bind receiver_increase =
      (* - payments:         payload.body.amount
         - stake delegation: 0
         - account creation: 0
         - token minting:    payload.body.amount
         - coinbase:         payload.body.amount - payload.common.fee
         - fee transfer:     payload.body.amount
      *)
      [%with_label "Compute receiver increase"]
        (let%bind base_amount =
           let%bind zero_transfer =
             Boolean.any [is_stake_delegation; is_create_account]
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
    let receiver_overflow = ref Boolean.false_ in
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
               - the receiver for a coinbase
               - the first receiver for a fee transfer
             *)
             let%bind is_empty_failure =
               let%bind must_not_be_empty =
                 Boolean.(is_stake_delegation ||| is_mint_tokens)
               in
               Boolean.(is_empty_and_writeable &&& must_not_be_empty)
             in
             let%bind () =
               [%with_label "Receiver existence failure matches predicted"]
                 (Boolean.Assert.( = ) is_empty_failure
                    user_command_failure.receiver_not_present)
             in
             let%bind () =
               [%with_label "Receiver creation failure matches predicted"]
                 (let%bind is_nonempty_creating =
                    Boolean.(
                      (not is_empty_and_writeable) &&& is_create_account)
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
               Boolean.(is_empty_and_writeable &&& not is_create_account)
             in
             let%bind () =
               [%with_label
                 "Check whether creation fails due to a non-default token"]
                 (let%bind token_should_not_create =
                    Boolean.(
                      should_pay_to_create &&& Boolean.not token_default)
                  in
                  let%bind token_cannot_create =
                    Boolean.(token_should_not_create &&& is_user_command)
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
               (* NOTE: Instead of capturing this as part of the user command
                  failures, we capture it inline here and bubble it out to a
                  reference. This behavior is still in line with the
                  out-of-snark transaction logic.

                  Updating [user_command_fails] to include this value from here
                  onwards will ensure that we do not update the source or
                  receiver accounts. The only places where [user_command_fails]
                  may have already affected behaviour are
                  * when the fee-payer is paying the account creation fee, and
                  * when a new token is created.
                  In both of these, this account is new, and will have a
                  balance of 0, so we can guarantee that there is no overflow.
               *)
               let%bind balance, `Overflow overflow =
                 Balance.Checked.add_amount_flagged account.balance
                   receiver_amount
               in
               let%bind () =
                 [%with_label "Overflow error only occurs in user commands"]
                   Boolean.(Assert.any [is_user_command; not overflow])
               in
               receiver_overflow := overflow ;
               Balance.Checked.if_ overflow ~then_:account.balance
                 ~else_:balance
             in
             let%bind user_command_fails =
               Boolean.(!receiver_overflow ||| user_command_fails)
             in
             let%bind is_empty_and_writeable =
               (* Do not create a new account if the user command will fail. *)
               Boolean.(is_empty_and_writeable &&& not user_command_fails)
             in
             let%bind may_delegate =
               (* Only default tokens may participate in delegation. *)
               Boolean.(is_empty_and_writeable &&& token_default)
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
               Boolean.if_ is_empty_and_writeable
                 ~then_:payload.body.token_locked
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
             ; timing= account.timing
             ; permissions= account.permissions
             ; snapp= account.snapp } ))
    in
    let%bind user_command_fails =
      Boolean.(!receiver_overflow ||| user_command_fails)
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
                      (let%bind not_ok =
                         Boolean.(
                           (not ok)
                           &&& not
                                 user_command_failure
                                   .source_insufficient_balance)
                       in
                       Boolean.Assert.( = ) not_ok
                         user_command_failure.source_bad_timing)
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
                      Boolean.(is_create_account ||| is_mint_tokens)
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
             ; timing
             ; permissions= account.permissions
             ; snapp= account.snapp } ))
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
           let%bind fee_transfer_excess, fee_transfer_excess_overflowed =
             let%map magnitude, `Overflow overflowed =
               Checked.(
                 add_flagged payload.body.amount (of_fee payload.common.fee))
             in
             (Signed.create ~magnitude ~sgn:Sgn.Checked.neg, overflowed)
           in
           let%bind () =
             (* TODO: Reject this in txn pool before fees-in-tokens. *)
             [%with_label "Fee excess does not overflow"]
               Boolean.(
                 Assert.any
                   [not is_fee_transfer; not fee_transfer_excess_overflowed])
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

  (* spec for [main statement]:
   constraints pass iff there exists
      t : Tagged_transaction.t
   such that
    - applying [t] to ledger with merkle hash [l1] results in ledger with merkle hash [l2].
    - applying [t] to [pc.source] with results in pending coinbase stack [pc.target]
    - t has fee excess equal to [fee_excess]
    - t has supply increase equal to [supply_increase]
   where statement includes
      l1 : Frozen_ledger_hash.t,
      l2 : Frozen_ledger_hash.t,
      fee_excess : Amount.Signed.t,
      supply_increase : Amount.t
      pc: Pending_coinbase_stack_state.t
  *)
  let%snarkydef main ~constraint_constants
      (statement : Statement.With_sok.Checked.t) =
    let%bind () = dummy_constraints () in
    let%bind (module Shifted) = Tick.Inner_curve.Checked.Shifted.create () in
    let%bind t =
      with_label __LOC__
        (exists Transaction_union.typ ~request:(As_prover.return Transaction))
    in
    let%bind pending_coinbase_init =
      exists Pending_coinbase.Stack.typ ~request:(As_prover.return Init_stack)
    in
    let%bind state_body =
      exists
        (Mina_state.Protocol_state.Body.typ ~constraint_constants)
        ~request:(As_prover.return State_body)
    in
    let pc = statement.pending_coinbase_stack_state in
    let%bind ( root_after
             , fee_excess
             , supply_increase
             , next_available_token_after ) =
      apply_tagged_transaction ~constraint_constants
        (module Shifted)
        statement.source pending_coinbase_init pc.source pc.target
        statement.next_available_token_before state_body t
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
    Checked.all_unit
      [ Frozen_ledger_hash.assert_equal root_after statement.target
      ; Currency.Amount.Checked.assert_equal supply_increase
          statement.supply_increase
      ; Fee_excess.assert_equal_checked fee_excess statement.fee_excess
      ; Token_id.Checked.Assert.equal next_available_token_after
          statement.next_available_token_after ]

  let rule ~constraint_constants : _ Pickles.Inductive_rule.t =
    { identifier= "transaction"
    ; prevs= []
    ; main=
        (fun [] x ->
          Run.run_checked (main ~constraint_constants x) ;
          [] )
    ; main_value= (fun [] _ -> []) }

  let transaction_union_handler handler (transaction : Transaction_union.t)
      (state_body : Mina_state.Protocol_state.Body.Value.t)
      (init_stack : Pending_coinbase.Stack.t) :
      Snarky_backendless.Request.request -> _ =
   fun (With {request; respond} as r) ->
    match request with
    | Transaction ->
        respond (Provide transaction)
    | State_body ->
        respond (Provide state_body)
    | Init_stack ->
        respond (Provide init_stack)
    | _ ->
        handler r
end

module Transition_data = struct
  type t =
    { proof: Proof_type.t
    ; supply_increase: Amount.t
    ; fee_excess: Fee_excess.t
    ; sok_digest: Sok_message.Digest.t
    ; pending_coinbase_stack_state: Pending_coinbase_stack_state.t }
  [@@deriving fields]
end

module Merge = struct
  open Tick

  (* spec for [main top_hash]:
     constraints pass iff
     there exist digest, s1, s3, fee_excess, supply_increase pending_coinbase_stack12.source, pending_coinbase_stack23.target, tock_vk such that
     H(digest,s1, s3, pending_coinbase_stack12.source, pending_coinbase_stack23.target, fee_excess, supply_increase, tock_vk) = top_hash,
     verify_transition tock_vk _ s1 s2 pending_coinbase_stack12.source, pending_coinbase_stack12.target is true
     verify_transition tock_vk _ s2 s3 pending_coinbase_stack23.source, pending_coinbase_stack23.target is true
  *)
  let%snarkydef main
      ([s1; s2] :
        (Statement.With_sok.var * (Statement.With_sok.var * _))
        Pickles_types.Hlist.HlistId.t) (s : Statement.With_sok.Checked.t) =
    let%bind fee_excess =
      Fee_excess.combine_checked s1.Statement.fee_excess
        s2.Statement.fee_excess
    in
    let%bind () =
      with_label __LOC__
        (let%bind valid_pending_coinbase_stack_transition =
           Pending_coinbase.Stack.Checked.check_merge
             ~transition1:
               ( s1.pending_coinbase_stack_state.source
               , s1.pending_coinbase_stack_state.target )
             ~transition2:
               ( s2.pending_coinbase_stack_state.source
               , s2.pending_coinbase_stack_state.target )
         in
         Boolean.Assert.is_true valid_pending_coinbase_stack_transition)
    in
    let%bind supply_increase =
      Amount.Checked.add s1.supply_increase s2.supply_increase
    in
    Checked.all_unit
      [ Fee_excess.assert_equal_checked fee_excess s.fee_excess
      ; Amount.Checked.assert_equal supply_increase s.supply_increase
      ; Frozen_ledger_hash.assert_equal s.source s1.source
      ; Frozen_ledger_hash.assert_equal s1.target s2.source
      ; Frozen_ledger_hash.assert_equal s2.target s.target
      ; Token_id.Checked.Assert.equal s.next_available_token_before
          s1.next_available_token_before
      ; Token_id.Checked.Assert.equal s1.next_available_token_after
          s2.next_available_token_before
      ; Token_id.Checked.Assert.equal s2.next_available_token_after
          s.next_available_token_after ]

  let rule self : _ Pickles.Inductive_rule.t =
    let prev_should_verify =
      match Genesis_constants.Proof_level.compiled with
      | Full ->
          true
      | _ ->
          false
    in
    let b = Boolean.var_of_value prev_should_verify in
    { identifier= "merge"
    ; prevs= [self; self]
    ; main=
        (fun ps x ->
          Run.run_checked (main ps x) ;
          [b; b] )
    ; main_value= (fun _ _ -> [prev_should_verify; prev_should_verify]) }
end

open Pickles_types

type tag =
  ( Statement.With_sok.Checked.t
  , Statement.With_sok.t
  , Nat.N2.n
  , Nat.N2.n )
  Pickles.Tag.t

let time lab f =
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "%s: %s\n%!" lab (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

let system ~constraint_constants =
  time "Transaction_snark.system" (fun () ->
      Pickles.compile ~cache:Cache_dir.cache
        (module Statement.With_sok.Checked)
        (module Statement.With_sok)
        ~typ:Statement.With_sok.typ
        ~num_rules:(module Nat.N2)
        ~max_num_input_proofs:(module Nat.N2)
        ~name:"transaction-snark"
        ~constraint_constants:
          (Genesis_constants.Constraint_constants.to_snark_keys_header
             constraint_constants)
        ~rules:(fun ~self -> [Base.rule ~constraint_constants; Merge.rule self])
  )

module Verification = struct
  module type S = sig
    val tag : tag

    val verify : (t * Sok_message.t) list -> bool

    val id : Pickles.Verification_key.Id.t Lazy.t

    val verification_key : Pickles.Verification_key.t Lazy.t

    val verify_against_digest : t -> bool
  end
end

module type S = sig
  include Verification.S

  val cache_handle : Pickles.Cache_handle.t

  val of_transaction :
       sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> snapp_account1:Snapp_account.t option
    -> snapp_account2:Snapp_account.t option
    -> Transaction.Valid.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t Async.Deferred.t

  val of_user_command :
       sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> Signed_command.With_valid_signature.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t Async.Deferred.t

  val of_fee_transfer :
       sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> Fee_transfer.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t Async.Deferred.t

  val merge :
    t -> t -> sok_digest:Sok_message.Digest.t -> t Async.Deferred.Or_error.t
end

let check_transaction_union ?(preeval = false) ~constraint_constants
    sok_message source target init_stack pending_coinbase_stack_state
    next_available_token_before next_available_token_after transaction
    state_body handler =
  if preeval then failwith "preeval currently disabled" ;
  let sok_digest = Sok_message.digest sok_message in
  let handler =
    Base.transaction_union_handler handler transaction state_body init_stack
  in
  let statement : Statement.With_sok.t =
    { source
    ; target
    ; supply_increase= Transaction_union.supply_increase transaction
    ; pending_coinbase_stack_state
    ; fee_excess= Transaction_union.fee_excess transaction
    ; next_available_token_before
    ; next_available_token_after
    ; sok_digest }
  in
  let open Tick in
  Or_error.ok_exn
    (run_and_check
       (handle
          (Checked.map ~f:As_prover.return
             (let open Checked in
             exists Statement.With_sok.typ
               ~compute:(As_prover.return statement)
             >>= Base.main ~constraint_constants))
          handler)
       ())
  |> ignore

let command_to_proofs (p : Snapp_command.t) :
    (Snapp_statement.t * Pickles.Side_loaded.Proof.t, Nat.N2.n) At_most.t =
  let proof_exn (c : Control.t) =
    match c with
    | Proof p ->
        p
    | Both {proof; _} ->
        proof
    | _ ->
        failwith "proof_exn"
  in
  let f (ps : (Snapp_command.Party.Authorized.Proved.t, _) At_most.t) =
    At_most.map ps ~f:(fun p ->
        ( { Snapp_statement.Poly.predicate= p.data.predicate
          ; body1= p.data.body
          ; body2= Snapp_command.Party.Body.dummy }
        , proof_exn p.authorization ) )
  in
  match p with
  | Signed_empty _ ->
      []
  | Signed_signed _ ->
      []
  | Proved_empty p ->
      f [p.one]
  | Proved_signed p ->
      f [p.one]
  | Proved_proved p ->
      f [p.one; p.two]

let command_to_statements c = At_most.map (command_to_proofs c) ~f:fst

(* TODO: Use init_stack. *)
let check_snapp_command ?(preeval = false) ~constraint_constants ~sok_message
    ~source ~target ~init_stack:_ ~pending_coinbase_stack_state
    ~next_available_token_before ~next_available_token_after ~state_body
    ~snapp_account1 ~snapp_account2 (t : Snapp_command.t) handler =
  if preeval then failwith "preeval currently disabled" ;
  let sok_digest = Sok_message.digest sok_message in
  let handler =
    Base.Snapp_command.handler ~state_body ~snapp_account1 ~snapp_account2 t
      handler
  in
  let statement : Statement.With_sok.t =
    { source
    ; target
    ; supply_increase= Currency.Amount.zero
    ; pending_coinbase_stack_state
    ; fee_excess= Or_error.ok_exn (Snapp_command.fee_excess t)
    ; next_available_token_before
    ; next_available_token_after
    ; sok_digest }
  in
  let open Tick in
  let comp =
    let open Checked in
    let%bind s =
      exists Statement.With_sok.typ ~compute:(As_prover.return statement)
    in
    match command_to_statements t with
    | [] ->
        Impl.make_checked (fun () ->
            Base.Snapp_command.Zero_proved.main ~constraint_constants s )
    | [s1] ->
        let%bind s1 =
          exists Snapp_statement.typ ~compute:(As_prover.return s1)
        in
        Impl.make_checked (fun () ->
            let (_ : Boolean.var) =
              Base.Snapp_command.One_proved.main ~constraint_constants s1 s
            in
            () )
    | [s1; s2] ->
        let%bind s1 = exists Snapp_statement.typ ~compute:(As_prover.return s1)
        and s2 = exists Snapp_statement.typ ~compute:(As_prover.return s2) in
        Impl.make_checked (fun () ->
            let (_ : Boolean.var * Boolean.var) =
              Base.Snapp_command.Two_proved.main ~constraint_constants s1 s2 s
            in
            () )
  in
  Or_error.ok_exn
    (run_and_check (handle (Checked.map ~f:As_prover.return comp) handler) ())
  |> ignore

let check_transaction ?preeval ~constraint_constants ~sok_message ~source
    ~target ~init_stack ~pending_coinbase_stack_state
    ~next_available_token_before ~next_available_token_after ~snapp_account1
    ~snapp_account2
    (transaction_in_block : Transaction.Valid.t Transaction_protocol_state.t)
    handler =
  let transaction =
    Transaction_protocol_state.transaction transaction_in_block
  in
  let state_body =
    Transaction_protocol_state.block_data transaction_in_block
  in
  match to_preunion (transaction :> Transaction.t) with
  | `Snapp_command c ->
      check_snapp_command ?preeval ~constraint_constants ~sok_message ~source
        ~target ~init_stack ~pending_coinbase_stack_state
        ~next_available_token_before ~next_available_token_after ~state_body
        ~snapp_account1 ~snapp_account2 c handler
  | `Transaction t ->
      check_transaction_union ?preeval ~constraint_constants sok_message source
        target init_stack pending_coinbase_stack_state
        next_available_token_before next_available_token_after
        (Transaction_union.of_transaction t)
        state_body handler

let check_user_command ~constraint_constants ~sok_message ~source ~target
    ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
    ~next_available_token_after t_in_block handler =
  let user_command = Transaction_protocol_state.transaction t_in_block in
  check_transaction ~constraint_constants ~sok_message ~source ~target
    ~init_stack ~pending_coinbase_stack_state ~next_available_token_before
    ~next_available_token_after ~snapp_account1:None ~snapp_account2:None
    {t_in_block with transaction= Command (Signed_command user_command)}
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
  let handler =
    Base.transaction_union_handler handler transaction state_body init_stack
  in
  let statement : Statement.With_sok.t =
    { source
    ; target
    ; supply_increase= Transaction_union.supply_increase transaction
    ; pending_coinbase_stack_state
    ; fee_excess= Transaction_union.fee_excess transaction
    ; next_available_token_before
    ; next_available_token_after
    ; sok_digest }
  in
  let open Tick in
  let main x = handle (Base.main ~constraint_constants x) handler in
  generate_auxiliary_input [Statement.With_sok.typ] () main statement

let generate_snapp_command_witness ?(preeval = false) ~constraint_constants
    ~sok_message ~source ~target ~init_stack:_ ~pending_coinbase_stack_state
    ~next_available_token_before ~next_available_token_after ~snapp_account1
    ~snapp_account2 transaction_in_block handler =
  if preeval then failwith "preeval currently disabled" ;
  let transaction : Snapp_command.t =
    Transaction_protocol_state.transaction transaction_in_block
  in
  let state_body =
    Transaction_protocol_state.block_data transaction_in_block
  in
  let sok_digest = Sok_message.digest sok_message in
  let handler =
    Base.Snapp_command.handler ~state_body ~snapp_account1 ~snapp_account2
      transaction handler
  in
  let statement : Statement.With_sok.t =
    { source
    ; target
    ; supply_increase= Currency.Amount.zero
    ; pending_coinbase_stack_state
    ; fee_excess= Or_error.ok_exn (Snapp_command.fee_excess transaction)
    ; next_available_token_before
    ; next_available_token_after
    ; sok_digest }
  in
  let open Tick in
  match command_to_statements transaction with
  | [] ->
      let main x =
        handle
          (make_checked (fun () ->
               Base.Snapp_command.Zero_proved.main ~constraint_constants x ))
          handler
      in
      generate_auxiliary_input [Statement.With_sok.typ] () main statement
  | [s1] ->
      let main x =
        handle
          (let%bind s1 =
             exists Snapp_statement.typ ~compute:(As_prover.return s1)
           in
           make_checked (fun () ->
               Base.Snapp_command.One_proved.main s1 ~constraint_constants x ))
          handler
      in
      generate_auxiliary_input [Statement.With_sok.typ] () main statement
  | [s1; s2] ->
      let main x =
        handle
          (let%bind s1 =
             exists Snapp_statement.typ ~compute:(As_prover.return s1)
           in
           let%bind s2 =
             exists Snapp_statement.typ ~compute:(As_prover.return s2)
           in
           make_checked (fun () ->
               Base.Snapp_command.Two_proved.main s1 s2 ~constraint_constants x
           ))
          handler
      in
      generate_auxiliary_input [Statement.With_sok.typ] () main statement

let generate_transaction_witness ?preeval ~constraint_constants ~sok_message
    ~source ~target ~init_stack ~pending_coinbase_stack_state
    ~next_available_token_before ~next_available_token_after ~snapp_account1
    ~snapp_account2
    (transaction_in_block : Transaction.Valid.t Transaction_protocol_state.t)
    handler =
  match
    to_preunion
      ( Transaction_protocol_state.transaction transaction_in_block
        :> Transaction.t )
  with
  | `Snapp_command c ->
      generate_snapp_command_witness ?preeval ~constraint_constants
        ~sok_message ~source ~target ~init_stack ~pending_coinbase_stack_state
        ~next_available_token_before ~next_available_token_after
        ~snapp_account1 ~snapp_account2
        {transaction_in_block with transaction= c}
        handler
  | `Transaction t ->
      generate_transaction_union_witness ?preeval ~constraint_constants
        sok_message source target
        { transaction_in_block with
          transaction= Transaction_union.of_transaction t }
        init_stack next_available_token_before next_available_token_after
        pending_coinbase_stack_state handler

let verify (ts : (t * _) list) ~key =
  List.for_all ts ~f:(fun ({statement; _}, message) ->
      Sok_message.Digest.equal
        (Sok_message.digest message)
        statement.sok_digest )
  && Pickles.verify
       (module Nat.N2)
       (module Statement.With_sok)
       key
       (List.map ts ~f:(fun ({statement; proof}, _) -> (statement, proof)))

module Make (Inputs : sig
  val constraint_constants : Genesis_constants.Constraint_constants.t
end) =
struct
  open Inputs

  let tag, cache_handle, p, Pickles.Provers.[base; merge] =
    system ~constraint_constants

  module Proof = (val p)

  let id = Proof.id

  let verification_key = Proof.verification_key

  let verify_against_digest {statement; proof} =
    Proof.verify [(statement, proof)]

  let verify ts =
    List.for_all ts ~f:(fun (p, m) ->
        Sok_message.Digest.equal (Sok_message.digest m) p.statement.sok_digest
    )
    && Proof.verify
         (List.map ts ~f:(fun ({statement; proof}, _) -> (statement, proof)))

  let of_transaction_union sok_digest source target ~init_stack
      ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after transaction state_body handler =
    let s =
      { Statement.source
      ; target
      ; sok_digest
      ; next_available_token_before
      ; next_available_token_after
      ; fee_excess= Transaction_union.fee_excess transaction
      ; supply_increase= Transaction_union.supply_increase transaction
      ; pending_coinbase_stack_state }
    in
    let%map.Async proof =
      base []
        ~handler:
          (Base.transaction_union_handler handler transaction state_body
             init_stack)
        s
    in
    {statement= s; proof}

  let of_snapp_command ~sok_digest ~source ~target ~init_stack:_
      ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after ~snapp_account1 ~snapp_account2 ~state_body t
      handler =
    let _handler =
      Base.Snapp_command.handler ~state_body ~snapp_account1 ~snapp_account2 t
        handler
    in
    let statement : Statement.With_sok.t =
      { source
      ; target
      ; supply_increase= Currency.Amount.zero
      ; pending_coinbase_stack_state
      ; fee_excess= Or_error.ok_exn (Snapp_command.fee_excess t)
      ; next_available_token_before
      ; next_available_token_after
      ; sok_digest }
    in
    let proof =
      match command_to_proofs t with
      | [] | [_] | [_; _] ->
          failwith "unimplemented"
    in
    {statement; proof}

  let of_transaction ~sok_digest ~source ~target ~init_stack
      ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after ~snapp_account1 ~snapp_account2
      transaction_in_block handler =
    let transaction : Transaction.t =
      Transaction.forget
        (Transaction_protocol_state.transaction transaction_in_block)
    in
    let state_body =
      Transaction_protocol_state.block_data transaction_in_block
    in
    match to_preunion transaction with
    | `Snapp_command t ->
        Async.Deferred.return
        @@ of_snapp_command ~sok_digest ~source ~target ~init_stack
             ~pending_coinbase_stack_state ~next_available_token_before
             ~next_available_token_after ~snapp_account1 ~snapp_account2
             ~state_body t handler
    | `Transaction t ->
        of_transaction_union sok_digest source target ~init_stack
          ~pending_coinbase_stack_state ~next_available_token_before
          ~next_available_token_after
          (Transaction_union.of_transaction t)
          state_body handler

  let of_user_command ~sok_digest ~source ~target ~init_stack
      ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after user_command_in_block handler =
    of_transaction ~sok_digest ~source ~target ~init_stack
      ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after ~snapp_account1:None ~snapp_account2:None
      { user_command_in_block with
        transaction=
          Command
            (Signed_command
               (Transaction_protocol_state.transaction user_command_in_block))
      }
      handler

  let of_fee_transfer ~sok_digest ~source ~target ~init_stack
      ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after transfer_in_block handler =
    of_transaction ~sok_digest ~source ~target ~init_stack
      ~pending_coinbase_stack_state ~next_available_token_before
      ~next_available_token_after ~snapp_account1:None ~snapp_account2:None
      { transfer_in_block with
        transaction=
          Fee_transfer
            (Transaction_protocol_state.transaction transfer_in_block) }
      handler

  let merge ({statement= t12; _} as x12) ({statement= t23; _} as x23)
      ~sok_digest =
    if not (Frozen_ledger_hash.( = ) t12.target t23.source) then
      failwithf
        !"Transaction_snark.merge: t12.target <> t23.source \
          (%{sexp:Frozen_ledger_hash.t} vs %{sexp:Frozen_ledger_hash.t})"
        t12.target t23.source () ;
    if
      not
        (Token_id.( = ) t12.next_available_token_after
           t23.next_available_token_before)
    then
      failwithf
        !"Transaction_snark.merge: t12.next_available_token_befre <> \
          t23.next_available_token_after (%{sexp:Token_id.t} vs \
          %{sexp:Token_id.t})"
        t12.next_available_token_after t23.next_available_token_before () ;
    let open Async.Deferred.Or_error.Let_syntax in
    let%bind fee_excess =
      Async.return @@ Fee_excess.combine t12.fee_excess t23.fee_excess
    and supply_increase =
      Amount.add t12.supply_increase t23.supply_increase
      |> Option.value_map ~f:Or_error.return
           ~default:
             (Or_error.errorf
                "Transaction_snark.merge: Supply change amount overflow")
      |> Async.return
    in
    let s : Statement.With_sok.t =
      { Statement.source= t12.source
      ; target= t23.target
      ; supply_increase
      ; fee_excess
      ; next_available_token_before= t12.next_available_token_before
      ; next_available_token_after= t23.next_available_token_after
      ; pending_coinbase_stack_state=
          { source= t12.pending_coinbase_stack_state.source
          ; target= t23.pending_coinbase_stack_state.target }
      ; sok_digest }
    in
    let%map.Async proof =
      merge [(x12.statement, x12.proof); (x23.statement, x23.proof)] s
    in
    Ok {statement= s; proof}
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

      let merkle_root_after_snapp_command_exn t ~txn_state_view txn =
        let hash, `Next_available_token tid =
          merkle_root_after_snapp_command_exn ~constraint_constants
            ~txn_state_view t txn
        in
        (Frozen_ledger_hash.of_ledger_hash hash, `Next_available_token tid)

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
      let payload : Signed_command.Payload.t =
        Signed_command.Payload.create ~fee ~fee_token
          ~fee_payer_pk:(Account.public_key fee_payer.account)
          ~nonce ~memo ~valid_until:None
          ~body:
            (Payment
               { source_pk
               ; receiver_pk
               ; token_id= token
               ; amount= Amount.of_int amt })
      in
      let signature =
        Signed_command.sign_payload fee_payer.private_key payload
      in
      Signed_command.check
        Signed_command.Poly.Stable.Latest.
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

    include Make (struct
      let constraint_constants = constraint_constants
    end)

    let state_body =
      let compile_time_genesis =
        (*not using Precomputed_values.for_unit_test because of dependency cycle*)
        Mina_state.Genesis_protocol_state.t
          ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
          ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
          ~constraint_constants ~consensus_constants
      in
      compile_time_genesis.data |> Mina_state.Protocol_state.body

    let state_body_hash = Mina_state.Protocol_state.Body.hash state_body

    let pending_coinbase_stack_target (t : Transaction.Valid.t) state_body_hash
        stack =
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

    let of_user_command' sok_digest ledger
        (user_command : Signed_command.With_valid_signature.t) init_stack
        pending_coinbase_stack_state state_body handler =
      let source = Ledger.merkle_root ledger in
      let current_global_slot =
        Mina_state.Protocol_state.Body.consensus_state state_body
        |> Consensus.Data.Consensus_state.global_slot_since_genesis
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
      Async.Thread_safe.block_on_async_exn (fun () ->
          of_user_command ~sok_digest ~source ~target ~init_stack
            ~pending_coinbase_stack_state ~next_available_token_before
            ~next_available_token_after user_command_in_block handler )

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
      let state_body_hash = Mina_state.Protocol_state.Body.hash state_body in
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
              ~txn_state_view:
                (txn_in_block.block_data |> Mina_state.Protocol_state.Body.view)
              txn_in_block.transaction
          in
          check_transaction txn_in_block
            (unstage (Sparse_ledger.handler sparse_ledger))
            ~constraint_constants
            ~sok_message:
              (Mina_base.Sok_message.create ~fee:Currency.Fee.zero
                 ~prover:Public_key.Compressed.empty)
            ~source:(Sparse_ledger.merkle_root sparse_ledger)
            ~target:(Sparse_ledger.merkle_root sparse_ledger_after)
            ~next_available_token_before:(Ledger.next_available_token ledger)
            ~next_available_token_after:
              (Sparse_ledger.next_available_token sparse_ledger_after)
            ~init_stack:pending_coinbase_init
            ~pending_coinbase_stack_state:
              {source= source_stack; target= pending_coinbase_stack_target}
            ~snapp_account1:None ~snapp_account2:None )

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
                  (Signed_command_memo.create_by_digesting_string_exn
                     (Test_util.arbitrary_string
                        ~len:Signed_command_memo.max_digestible_string_length))
              in
              let current_global_slot =
                Mina_state.Protocol_state.Body.consensus_state state_body
                |> Consensus.Data.Consensus_state.global_slot_since_genesis
              in
              let next_available_token_before =
                Ledger.next_available_token ledger
              in
              let target, `Next_available_token next_available_token_after =
                Ledger.merkle_root_after_user_command_exn ledger
                  ~txn_global_slot:current_global_slot t1
              in
              let mentioned_keys =
                Signed_command.accounts_accessed
                  ~next_available_token:next_available_token_before
                  (Signed_command.forget_check t1)
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
                pending_coinbase_stack_target (Command (Signed_command t1))
                  state_body_hash pending_coinbase_stack
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

    let signed_signed ~wallets i j =
      let full_amount = 8_000_000_000 in
      let fee = Fee.of_int (Random.int full_amount) in
      let receiver_amount =
        Amount.sub (Amount.of_int full_amount) (Amount.of_fee fee)
        |> Option.value_exn
      in
      let acct1 = wallets.(i) in
      let acct2 = wallets.(j) in
      let open Snapp_command in
      let open Snapp_basic in
      let new_state : _ Snapp_state.V.t =
        Vector.init Snapp_state.Max_state_size.n ~f:Field.of_int
      in
      let data1 : Party.Predicated.Signed.t =
        { predicate= acct1.account.nonce
        ; body=
            { pk= acct1.account.public_key
            ; update=
                { app_state=
                    Vector.map new_state ~f:(fun x -> Set_or_keep.Set x)
                ; delegate= Keep
                ; verification_key= Keep
                ; permissions= Keep }
            ; delta=
                Amount.Signed.(
                  negate (of_unsigned (Amount.of_int full_amount))) } }
      in
      let data2 : Party.Predicated.Signed.t =
        { predicate= acct2.account.nonce
        ; body=
            { pk= acct2.account.public_key
            ; update=
                { app_state= Vector.map new_state ~f:(fun _ -> Set_or_keep.Keep)
                ; delegate= Keep
                ; verification_key= Keep
                ; permissions= Keep }
            ; delta= Amount.Signed.of_unsigned receiver_amount } }
      in
      Snapp_command.signed_signed ~token_id:Token_id.default
        (acct1.private_key, data1) (acct2.private_key, data2)

    let%test_unit "merkle_root_after_snapp_command_exn_immutable" =
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
                let i, j = (1, 2) in
                signed_signed ~wallets i j
              in
              let hash_pre = Ledger.merkle_root ledger in
              let _target, `Next_available_token _next_available_token_after =
                let txn_state_view =
                  Mina_state.Protocol_state.Body.view state_body
                in
                Ledger.merkle_root_after_snapp_command_exn ledger
                  ~txn_state_view t1
              in
              let hash_post = Ledger.merkle_root ledger in
              [%test_eq: Field.t] hash_pre hash_post ) )

    let%test_unit "signed_signed" =
      Test_util.with_randomness 123456789 (fun () ->
          let wallets = random_wallets () in
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              Array.iter (Array.sub wallets ~pos:1 ~len:2)
                ~f:(fun {account; private_key= _} ->
                  Ledger.create_new_account_exn ledger
                    (Account.identifier account)
                    account ) ;
              let i, j = (1, 2) in
              let t1 = signed_signed ~wallets i j in
              let txn_state_view =
                Mina_state.Protocol_state.Body.view state_body
              in
              let next_available_token_before =
                Ledger.next_available_token ledger
              in
              let target, `Next_available_token next_available_token_after =
                Ledger.merkle_root_after_snapp_command_exn ledger
                  ~txn_state_view t1
              in
              let mentioned_keys = Snapp_command.accounts_accessed t1 in
              let sparse_ledger =
                Sparse_ledger.of_ledger_subset_exn ledger mentioned_keys
              in
              let sok_message =
                Sok_message.create ~fee:Fee.zero
                  ~prover:wallets.(1).account.public_key
              in
              let pending_coinbase_stack = Pending_coinbase.Stack.empty in
              let pending_coinbase_stack_target =
                pending_coinbase_stack_target (Command (Snapp_command t1))
                  state_body_hash pending_coinbase_stack
              in
              let pending_coinbase_stack_state =
                { Pending_coinbase_stack_state.source= pending_coinbase_stack
                ; target= pending_coinbase_stack_target }
              in
              let snapp_account1, snapp_account2 =
                Sparse_ledger.snapp_accounts sparse_ledger
                  (Command (Snapp_command t1))
              in
              check_snapp_command ~constraint_constants ~sok_message
                ~state_body
                ~source:(Ledger.merkle_root ledger)
                ~target ~init_stack:pending_coinbase_stack
                ~pending_coinbase_stack_state ~next_available_token_before
                ~next_available_token_after ~snapp_account1 ~snapp_account2 t1
                (unstage @@ Sparse_ledger.handler sparse_ledger) ) )

    let account_fee = Fee.to_int constraint_constants.account_creation_fee

    let test_transaction ~constraint_constants ?txn_global_slot ledger txn =
      let source = Ledger.merkle_root ledger in
      let pending_coinbase_stack = Pending_coinbase.Stack.empty in
      let next_available_token = Ledger.next_available_token ledger in
      let state_body, state_body_hash =
        match txn_global_slot with
        | None ->
            (state_body, state_body_hash)
        | Some txn_global_slot ->
            let state_body =
              let state =
                (* NB: The [previous_state_hash] is a dummy, do not use. *)
                Mina_state.Protocol_state.create
                  ~previous_state_hash:Tick0.Field.zero ~body:state_body
              in
              let consensus_state_at_slot =
                Consensus.Data.Consensus_state.Value.For_tests
                .with_global_slot_since_genesis
                  (Mina_state.Protocol_state.consensus_state state)
                  txn_global_slot
              in
              Mina_state.Protocol_state.(
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
              Mina_state.Protocol_state.Body.hash state_body
            in
            (state_body, state_body_hash)
      in
      let txn_state_view : Snapp_predicate.Protocol_state.View.t =
        Mina_state.Protocol_state.Body.view state_body
      in
      let mentioned_keys, pending_coinbase_stack_target =
        let pending_coinbase_stack =
          Pending_coinbase.Stack.push_state state_body_hash
            pending_coinbase_stack
        in
        match (txn : Transaction.Valid.t) with
        | Command (Signed_command uc) ->
            ( Signed_command.accounts_accessed ~next_available_token
                (uc :> Signed_command.t)
            , pending_coinbase_stack )
        | Command (Snapp_command _) ->
            failwith "Snapp_command not yet supported"
        | Fee_transfer ft ->
            (Fee_transfer.receivers ft, pending_coinbase_stack)
        | Coinbase cb ->
            ( Coinbase.accounts_accessed cb
            , Pending_coinbase.Stack.push_coinbase cb pending_coinbase_stack )
      in
      let sok_signer =
        match to_preunion (txn :> Transaction.t) with
        | `Transaction t ->
            (Transaction_union.of_transaction t).signer |> Public_key.compress
        | `Snapp_command c ->
            Account_id.public_key (Snapp_command.fee_payer c)
      in
      let sparse_ledger =
        Sparse_ledger.of_ledger_subset_exn ledger mentioned_keys
      in
      let _undo =
        Or_error.ok_exn
        @@ Ledger.apply_transaction ledger ~constraint_constants
             ~txn_state_view
             (txn :> Transaction.t)
      in
      let target = Ledger.merkle_root ledger in
      let sok_message = Sok_message.create ~fee:Fee.zero ~prover:sok_signer in
      check_transaction ~constraint_constants ~sok_message ~source ~target
        ~init_stack:pending_coinbase_stack
        ~pending_coinbase_stack_state:
          { Pending_coinbase_stack_state.source= pending_coinbase_stack
          ; target= pending_coinbase_stack_target }
        ~next_available_token_before:next_available_token
        ~next_available_token_after:(Ledger.next_available_token ledger)
        ~snapp_account1:None ~snapp_account2:None
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
            Signed_command_memo.create_by_digesting_string_exn
              (Test_util.arbitrary_string
                 ~len:Signed_command_memo.max_digestible_string_length)
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
                      (Transaction.Command (Signed_command uc)) )
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

    module Pc_with_init_stack = struct
      type t =
        { pc: Pending_coinbase_stack_state.t
        ; init_stack: Pending_coinbase.Stack.t }
    end

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
                Signed_command_memo.create_by_digesting_string_exn
                  (Test_util.arbitrary_string
                     ~len:Signed_command_memo.max_digestible_string_length)
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
              let next_available_token1 = Ledger.next_available_token ledger in
              let sparse_ledger =
                Sparse_ledger.of_ledger_subset_exn ledger
                  (List.concat_map
                     ~f:(fun t ->
                       (* NB: Shouldn't assume the same next_available_token
                          for each command normally, but we know statically
                          that these are payments in this test.
                       *)
                       Signed_command.accounts_accessed
                         ~next_available_token:next_available_token1
                         (Signed_command.forget_check t) )
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
                { Pc_with_init_stack.pc=
                    {source= source_stack; target= target_stack}
                ; init_stack= init_stack1 }
              in
              let proof12 =
                of_user_command' sok_digest ledger t1
                  pending_coinbase_stack_state1.init_stack
                  pending_coinbase_stack_state1.pc state_body1
                  (unstage @@ Sparse_ledger.handler sparse_ledger)
              in
              let current_global_slot =
                Mina_state.Protocol_state.Body.consensus_state state_body1
                |> Consensus.Data.Consensus_state.global_slot_since_genesis
              in
              let sparse_ledger =
                Sparse_ledger.apply_user_command_exn ~constraint_constants
                  ~txn_global_slot:current_global_slot sparse_ledger
                  (t1 :> Signed_command.t)
              in
              let pending_coinbase_stack_state2, state_body2 =
                let previous_stack = pending_coinbase_stack_state1.pc.target in
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
                ( { Pc_with_init_stack.pc=
                      {source= source_stack; target= target_stack}
                  ; init_stack }
                , state_body2 )
              in
              Ledger.apply_user_command ~constraint_constants ledger
                ~txn_global_slot:current_global_slot t1
              |> Or_error.ok_exn |> ignore ;
              [%test_eq: Frozen_ledger_hash.t]
                (Ledger.merkle_root ledger)
                (Sparse_ledger.merkle_root sparse_ledger) ;
              let proof23 =
                of_user_command' sok_digest ledger t2
                  pending_coinbase_stack_state2.init_stack
                  pending_coinbase_stack_state2.pc state_body2
                  (unstage @@ Sparse_ledger.handler sparse_ledger)
              in
              let current_global_slot =
                Mina_state.Protocol_state.Body.consensus_state state_body2
                |> Consensus.Data.Consensus_state.global_slot_since_genesis
              in
              let sparse_ledger =
                Sparse_ledger.apply_user_command_exn ~constraint_constants
                  ~txn_global_slot:current_global_slot sparse_ledger
                  (t2 :> Signed_command.t)
              in
              Ledger.apply_user_command ledger ~constraint_constants
                ~txn_global_slot:current_global_slot t2
              |> Or_error.ok_exn |> ignore ;
              [%test_eq: Frozen_ledger_hash.t]
                (Ledger.merkle_root ledger)
                (Sparse_ledger.merkle_root sparse_ledger) ;
              let proof13 =
                Async.Thread_safe.block_on_async_exn (fun () ->
                    merge ~sok_digest proof12 proof23 )
                |> Or_error.ok_exn
              in
              Proof.verify [(proof13.statement, proof13.proof)] ) )

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
          Mina_state.Protocol_state.negative_one
            ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
            ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
            ~constraint_constants ~consensus_constants
          |> Mina_state.Protocol_state.body
        in
        let state_body_hash0 =
          Mina_state.Protocol_state.Body.hash state_body0
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
          Mina_state.Protocol_state.negative_one
            ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
            ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
            ~constraint_constants ~consensus_constants
          |> Mina_state.Protocol_state.body
        in
        let state_body_hash0 =
          Mina_state.Protocol_state.Body.hash state_body0
        in
        (state_body_hash0, state_body0)
      in
      let state_hash_and_body2 = (state_body_hash, state_body) in
      test_base_and_merge ~state_hash_and_body1 ~state_hash_and_body2
        ~carryforward1:false ~carryforward2:false

    let create_account pk token balance =
      Account.create (Account_id.create pk token) (Balance.of_int balance)

    let test_user_command_with_accounts ~constraint_constants ~ledger ~accounts
        ~signer ~fee ~fee_payer_pk ~fee_token ?memo ?valid_until ?nonce body =
      let memo =
        match memo with
        | Some memo ->
            memo
        | None ->
            Signed_command_memo.create_by_digesting_string_exn
              (Test_util.arbitrary_string
                 ~len:Signed_command_memo.max_digestible_string_length)
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
        Signed_command.Payload.create ~fee ~fee_payer_pk ~fee_token ~nonce
          ~valid_until ~memo ~body
      in
      let signer = Signature_lib.Keypair.of_private_key_exn signer in
      let user_command = Signed_command.sign signer payload in
      let next_available_token = Ledger.next_available_token ledger in
      test_transaction ~constraint_constants ledger
        (Command (Signed_command user_command)) ;
      let fee_payer = Signed_command.Payload.fee_payer payload in
      let source =
        Signed_command.Payload.source ~next_available_token payload
      in
      let receiver =
        Signed_command.Payload.receiver ~next_available_token payload
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
                Public_key.Compressed.equal
                  (Option.value_exn source_account.delegate)
                  source_pk ) ;
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
                Public_key.Compressed.equal
                  (Option.value_exn source_account.delegate)
                  source_pk ) ;
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
            Signed_command_memo.create_by_digesting_string_exn
              (Test_util.arbitrary_string
                 ~len:Signed_command_memo.max_digestible_string_length)
          in
          let balance = Balance.of_int 100_000_000_000_000 in
          let initial_minimum_balance = Balance.of_int 80_000_000_000_000 in
          let cliff_time = Global_slot.of_int 1000 in
          let cliff_amount = Amount.of_int 10000 in
          let vesting_period = Global_slot.of_int 10 in
          let vesting_increment = Amount.of_int 1 in
          let txn_global_slot = Global_slot.of_int 1002 in
          let sender =
            { sender with
              account=
                Or_error.ok_exn
                @@ Account.create_timed
                     (Account.identifier sender.account)
                     balance ~initial_minimum_balance ~cliff_time ~cliff_amount
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
                      ledger (Transaction.Command (Signed_command uc)) )
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
              assert (Option.is_none token_owner_account.delegate) ;
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
              assert (Option.is_none token_owner_account.delegate) ;
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
              assert (Option.is_none token_owner_account.delegate) ;
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
              assert (Option.is_none receiver_account.delegate) ;
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
              assert (Option.is_none receiver_account.delegate) ;
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
              assert (Option.is_none receiver_account.delegate) ;
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
              assert (Option.is_none receiver_account.delegate) ;
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

    let%test_unit "create new token account fails if claimed token owner is \
                   also the account creation target and does not exist" =
      Test_util.with_randomness 123456789 (fun () ->
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              let wallets = random_wallets ~n:3 () in
              let signer = wallets.(0).private_key in
              (* Fee-payer, receiver, and token owner are the same. *)
              let fee_payer_pk = wallets.(0).account.public_key in
              let fee_token = Token_id.default in
              let token_id =
                Quickcheck.random_value Token_id.gen_non_default
              in
              let accounts =
                [|create_account fee_payer_pk fee_token 20_000_000_000|]
              in
              let fee = Fee.of_int (random_int_incl 2 15 * 1_000_000_000) in
              let ( `Fee_payer_account fee_payer_account
                  , `Source_account token_owner_account
                  , `Receiver_account receiver_account ) =
                test_user_command_with_accounts ~constraint_constants ~ledger
                  ~accounts ~signer ~fee ~fee_payer_pk ~fee_token
                  (Create_token_account
                     { token_owner_pk= fee_payer_pk
                     ; token_id
                     ; receiver_pk= fee_payer_pk
                     ; account_disabled= false })
              in
              let fee_payer_account = Option.value_exn fee_payer_account in
              (* No account creation fee: the command fails. *)
              let expected_fee_payer_balance =
                accounts.(0).balance |> sub_fee fee
              in
              assert (
                Balance.equal fee_payer_account.balance
                  expected_fee_payer_balance ) ;
              assert (Option.is_none token_owner_account) ;
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
                  (Option.value_exn receiver_account.delegate) ) ;
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

    let%test_unit "unchanged timings for fee transfers and coinbase" =
      Test_util.with_randomness 123456789 (fun () ->
          let receivers =
            Array.init 2 ~f:(fun _ ->
                Public_key.of_private_key_exn (Private_key.create ())
                |> Public_key.compress )
          in
          let timed_account pk =
            let account_id = Account_id.create pk Token_id.default in
            let balance = Balance.of_int 100_000_000_000_000 in
            let initial_minimum_balance = Balance.of_int 80_000_000_000 in
            let cliff_time = Global_slot.of_int 2 in
            let cliff_amount = Amount.of_int 5_000_000_000 in
            let vesting_period = Global_slot.of_int 2 in
            let vesting_increment = Amount.of_int 40_000_000_000 in
            Or_error.ok_exn
            @@ Account.create_timed account_id balance ~initial_minimum_balance
                 ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
          in
          let timed_account1 = timed_account receivers.(0) in
          let timed_account2 = timed_account receivers.(1) in
          let fee = 8_000_000_000 in
          let ft1, ft2 =
            let single1 =
              Fee_transfer.Single.create ~receiver_pk:receivers.(0)
                ~fee:(Currency.Fee.of_int fee) ~fee_token:Token_id.default
            in
            let single2 =
              Fee_transfer.Single.create ~receiver_pk:receivers.(1)
                ~fee:(Currency.Fee.of_int fee) ~fee_token:Token_id.default
            in
            ( Fee_transfer.create single1 (Some single2) |> Or_error.ok_exn
            , Fee_transfer.create single1 None |> Or_error.ok_exn )
          in
          let coinbase_with_ft, coinbase_wo_ft =
            let ft =
              Coinbase.Fee_transfer.create ~receiver_pk:receivers.(0)
                ~fee:(Currency.Fee.of_int fee)
            in
            ( Coinbase.create
                ~amount:(Currency.Amount.of_int 10_000_000_000)
                ~receiver:receivers.(1) ~fee_transfer:(Some ft)
              |> Or_error.ok_exn
            , Coinbase.create
                ~amount:(Currency.Amount.of_int 10_000_000_000)
                ~receiver:receivers.(1) ~fee_transfer:None
              |> Or_error.ok_exn )
          in
          let transactions : Transaction.Valid.t list =
            [ Fee_transfer ft1
            ; Fee_transfer ft2
            ; Coinbase coinbase_with_ft
            ; Coinbase coinbase_wo_ft ]
          in
          Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
              List.iter [timed_account1; timed_account2] ~f:(fun acc ->
                  Ledger.create_new_account_exn ledger (Account.identifier acc)
                    acc ) ;
              (* well over the vesting period, the timing field shouldn't change*)
              let txn_global_slot = Global_slot.of_int 100 in
              List.iter transactions ~f:(fun txn ->
                  test_transaction ~txn_global_slot ~constraint_constants
                    ledger txn ) ) )
  end )

let%test_module "account timing check" =
  ( module struct
    open Core_kernel
    open Mina_numbers
    open Currency
    open Transaction_validator.For_tests

    (* test that unchecked and checked calculations for timing agree *)

    let checked_min_balance_and_timing account txn_amount txn_global_slot =
      let account = Account.var_of_t account in
      let txn_amount = Amount.var_of_t txn_amount in
      let txn_global_slot = Global_slot.Checked.constant txn_global_slot in
      let%map `Min_balance min_balance, timing =
        Base.check_timing ~balance_check:Tick.Boolean.Assert.is_true
          ~timed_balance_check:Tick.Boolean.Assert.is_true ~account ~txn_amount
          ~txn_global_slot
      in
      (min_balance, timing)

    let make_checked_timing_computation account txn_amount txn_global_slot =
      let%map _min_balance, timing =
        checked_min_balance_and_timing account txn_amount txn_global_slot
      in
      timing

    let make_checked_min_balance_computation account txn_amount txn_global_slot
        =
      let%map min_balance, _timing =
        checked_min_balance_and_timing account txn_amount txn_global_slot
      in
      min_balance

    let snarky_integer_of_bools bools =
      let snarky_bools =
        List.map bools ~f:(fun b ->
            let open Tick.Boolean in
            if b then true_ else false_ )
      in
      let bitstring_lsb =
        Bitstring_lib.Bitstring.Lsb_first.of_list snarky_bools
      in
      Snarky_integer.Integer.of_bits ~m:Tick.m bitstring_lsb

    let run_checked_timing_and_compare account txn_amount txn_global_slot
        unchecked_timing unchecked_min_balance =
      let equal_balances_computation =
        let open Snarky_backendless.Checked in
        let%bind checked_timing =
          make_checked_timing_computation account txn_amount txn_global_slot
        in
        (* check agreement of timings produced by checked, unchecked validations *)
        let%bind () =
          as_prover
            As_prover.(
              let%map checked_timing =
                read Account.Timing.typ checked_timing
              in
              assert (Account.Timing.equal checked_timing unchecked_timing))
        in
        let%bind checked_min_balance =
          make_checked_min_balance_computation account txn_amount
            txn_global_slot
        in
        let%bind unchecked_min_balance_as_snarky_integer =
          Run.make_checked (fun () ->
              snarky_integer_of_bools (Balance.to_bits unchecked_min_balance)
          )
        in
        let%map equal_balances_checked =
          Run.make_checked (fun () ->
              Snarky_integer.Integer.equal ~m checked_min_balance
                unchecked_min_balance_as_snarky_integer )
        in
        Snarky_backendless.As_prover.read Tick.Boolean.typ
          equal_balances_checked
      in
      let (), equal_balances =
        Or_error.ok_exn @@ Tick.run_and_check equal_balances_computation ()
      in
      equal_balances

    (* confirm the checked computation fails *)
    let checked_timing_should_fail account txn_amount txn_global_slot =
      let checked_timing_computation =
        let%map checked_timing =
          make_checked_timing_computation account txn_amount txn_global_slot
        in
        As_prover.read Account.Timing.typ checked_timing
      in
      Or_error.is_error @@ Tick.run_and_check checked_timing_computation ()

    let%test "before_cliff_time" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 80_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let cliff_amount = Amount.of_int 500_000_000 in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 1_000_000_000 in
      let txn_amount = Currency.Amount.of_int 100_000_000_000 in
      let txn_global_slot = Global_slot.of_int 45 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
      in
      let timing_with_min_balance =
        validate_timing_with_min_balance ~txn_amount ~txn_global_slot ~account
      in
      match timing_with_min_balance with
      | Ok ((Timed _ as unchecked_timing), `Min_balance unchecked_min_balance)
        ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing unchecked_min_balance
      | _ ->
          false

    let%test "positive min balance" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let cliff_amount = Amount.zero in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 100_000_000_000 in
      let txn_global_slot = Mina_numbers.Global_slot.of_int 1_900 in
      let timing_with_min_balance =
        validate_timing_with_min_balance ~account
          ~txn_amount:(Currency.Amount.of_int 100_000_000_000)
          ~txn_global_slot:(Mina_numbers.Global_slot.of_int 1_900)
      in
      (* we're 900 slots past the cliff, which is 90 vesting periods
          subtract 90 * 100 = 9,000 from init min balance of 10,000 to get 1000
          so we should still be timed
        *)
      match timing_with_min_balance with
      | Ok ((Timed _ as unchecked_timing), `Min_balance unchecked_min_balance)
        ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing unchecked_min_balance
      | _ ->
          false

    let%test "curr min balance of zero" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1_000 in
      let cliff_amount = Amount.of_int 900_000_000 in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 100_000_000_000 in
      let txn_global_slot = Global_slot.of_int 2_000 in
      let timing_with_min_balance =
        validate_timing_with_min_balance ~txn_amount ~txn_global_slot ~account
      in
      (* we're 2_000 - 1_000 = 1_000 slots past the cliff, which is 100 vesting periods
          subtract 100 * 100_000_000_000 = 10_000_000_000_000 from init min balance
          of 10_000_000_000 to get zero, so we should be untimed now
        *)
      match timing_with_min_balance with
      | Ok ((Untimed as unchecked_timing), `Min_balance unchecked_min_balance)
        ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing unchecked_min_balance
      | _ ->
          false

    let%test "below calculated min balance" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 10_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1_000 in
      let cliff_amount = Amount.zero in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 101_000_000_000 in
      let txn_global_slot = Mina_numbers.Global_slot.of_int 1_010 in
      let timing = validate_timing ~txn_amount ~txn_global_slot ~account in
      match timing with
      | Error err ->
          assert (
            Transaction_status.Failure.equal
              (Transaction_logic.timing_error_to_user_command_status err)
              Transaction_status.Failure.Source_minimum_balance_violation ) ;
          checked_timing_should_fail account txn_amount txn_global_slot
      | _ ->
          false

    let%test "insufficient balance" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let cliff_amount = Amount.zero in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 100_001_000_000_000 in
      let txn_global_slot = Global_slot.of_int 2000_000_000_000 in
      let timing = validate_timing ~txn_amount ~txn_global_slot ~account in
      match timing with
      | Error err ->
          assert (
            Transaction_status.Failure.equal
              (Transaction_logic.timing_error_to_user_command_status err)
              Transaction_status.Failure.Source_insufficient_balance ) ;
          checked_timing_should_fail account txn_amount txn_global_slot
      | _ ->
          false

    let%test "past full vesting" =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let cliff_amount = Amount.zero in
      let vesting_period = Global_slot.of_int 10 in
      let vesting_increment = Amount.of_int 100_000_000_000 in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
      in
      (* fully vested, curr min balance = 0, so we can spend the whole balance *)
      let txn_amount = Currency.Amount.of_int 100_000_000_000_000 in
      let txn_global_slot = Global_slot.of_int 3000 in
      let timing_with_min_balance =
        validate_timing_with_min_balance ~txn_amount ~txn_global_slot ~account
      in
      match timing_with_min_balance with
      | Ok ((Untimed as unchecked_timing), `Min_balance unchecked_min_balance)
        ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing unchecked_min_balance
      | _ ->
          false

    let make_cliff_amount_test slot =
      let pk = Public_key.Compressed.empty in
      let account_id = Account_id.create pk Token_id.default in
      let balance = Balance.of_int 100_000_000_000_000 in
      let initial_minimum_balance = Balance.of_int 10_000_000_000_000 in
      let cliff_time = Global_slot.of_int 1000 in
      let cliff_amount =
        Balance.to_uint64 initial_minimum_balance |> Amount.of_uint64
      in
      let vesting_period = Global_slot.of_int 1 in
      let vesting_increment = Amount.zero in
      let account =
        Or_error.ok_exn
        @@ Account.create_timed account_id balance ~initial_minimum_balance
             ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
      in
      let txn_amount = Currency.Amount.of_int 100_000_000_000_000 in
      let txn_global_slot = Global_slot.of_int slot in
      (txn_amount, txn_global_slot, account)

    let%test "before cliff, cliff_amount doesn't affect min balance" =
      let txn_amount, txn_global_slot, account = make_cliff_amount_test 999 in
      let timing = validate_timing ~txn_amount ~txn_global_slot ~account in
      match timing with
      | Error err ->
          assert (
            Transaction_status.Failure.equal
              (Transaction_logic.timing_error_to_user_command_status err)
              Transaction_status.Failure.Source_minimum_balance_violation ) ;
          checked_timing_should_fail account txn_amount txn_global_slot
      | Ok _ ->
          false

    let%test "at exactly cliff time, cliff amount allows spending" =
      let txn_amount, txn_global_slot, account = make_cliff_amount_test 1000 in
      let timing_with_min_balance =
        validate_timing_with_min_balance ~txn_amount ~txn_global_slot ~account
      in
      match timing_with_min_balance with
      | Ok ((Untimed as unchecked_timing), `Min_balance unchecked_min_balance)
        ->
          run_checked_timing_and_compare account txn_amount txn_global_slot
            unchecked_timing unchecked_min_balance
      | _ ->
          false
  end )

let constraint_system_digests ~constraint_constants () =
  let digest = Tick.R1CS_constraint_system.digest in
  [ ( "transaction-merge"
    , digest
        Merge.(
          Tick.constraint_system ~exposing:[Statement.With_sok.typ] (fun x ->
              let open Tick in
              let%bind x1 = exists Statement.With_sok.typ in
              let%bind x2 = exists Statement.With_sok.typ in
              main [x1; x2] x )) )
  ; ( "transaction-base"
    , digest
        Base.(
          Tick.constraint_system ~exposing:[Statement.With_sok.typ]
            (main ~constraint_constants)) ) ]
