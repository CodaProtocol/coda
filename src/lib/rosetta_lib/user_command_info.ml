[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifndef
consensus_mechanism]

module Mina_base = Mina_base_nonconsensus
module Currency = Currency_nonconsensus.Currency
module Signature_lib = Signature_lib_nonconsensus
module Unsigned_extended = Unsigned_extended_nonconsensus.Unsigned_extended

[%%endif]

module Fee_currency = Currency.Fee
module Amount_currency = Currency.Amount
open Rosetta_models
module Signed_command = Mina_base.Signed_command
module Token_id = Mina_base.Token_id
module Public_key = Signature_lib.Public_key
module Signed_command_memo = Mina_base.Signed_command_memo
module Payment_payload = Mina_base.Payment_payload
module Stake_delegation = Mina_base.Stake_delegation

let pk_to_public_key ~context (`Pk pk) =
  Public_key.Compressed.of_base58_check pk
  |> Result.map_error ~f:(fun _ ->
         Errors.create ~context `Public_key_format_not_valid )

let account_id (`Pk pk) token_id =
  { Account_identifier.address= pk
  ; sub_account= None
  ; metadata= Some (Amount_of.Token_id.encode token_id) }

let token_id_of_account (account : Account_identifier.t) =
  let module Decoder = Amount_of.Token_id.T (Result) in
  Decoder.decode account.metadata
  |> Result.map ~f:(Option.value ~default:Amount_of.Token_id.default)
  |> Result.ok

module Op = struct
  type 'a t = {label: 'a; related_to: 'a option} [@@deriving equal]

  module T (M : Monad.S2) = struct
    let build ~a_eq ~plan ~f =
      let open M.Let_syntax in
      let%map _, rev_data =
        List.fold plan
          ~init:(M.return (0, []))
          ~f:(fun macc op ->
            let open M.Let_syntax in
            let%bind i, acc = macc in
            let operation_identifier i =
              { Operation_identifier.index= Int64.of_int_exn i
              ; network_index= None }
            in
            let related_operations =
              match op.related_to with
              | Some relate ->
                  List.findi plan ~f:(fun _ a -> a_eq relate a.label)
                  |> Option.map ~f:(fun (i, _) -> operation_identifier i)
                  |> Option.to_list
              | None ->
                  []
            in
            let%map a =
              f ~related_operations
                ~operation_identifier:(operation_identifier i) op
            in
            (i + 1, a :: acc) )
      in
      List.rev rev_data
  end

  module Ident2 = struct
    type ('a, 'e) t = 'a

    module T = struct
      type ('a, 'e) t = 'a

      let map = `Define_using_bind

      let return a = a

      let bind a ~f = f a
    end

    include Monad.Make2 (T)
  end

  include T (Ident2)
end

module Kind = struct
  type t =
    [ `Payment
    | `Delegation
    | `Create_token
    | `Create_token_account
    | `Mint_tokens ]
  [@@deriving yojson, equal, sexp, compare]
end

module Account_creation_fees_paid = struct
  type t =
    | By_no_one
    | By_fee_payer of Unsigned_extended.UInt64.t
    | By_receiver of Unsigned_extended.UInt64.t
  [@@deriving equal, to_yojson, sexp, compare]
end

module Failure_status = struct
  type t = [`Applied of Account_creation_fees_paid.t | `Failed of string]
  [@@deriving equal, to_yojson, sexp, compare]
end

type t =
  { kind: Kind.t
  ; fee_payer: [`Pk of string]
  ; source: [`Pk of string]
  ; receiver: [`Pk of string]
  ; fee_token: Unsigned_extended.UInt64.t
  ; token: Unsigned_extended.UInt64.t
  ; fee: Unsigned_extended.UInt64.t
  ; nonce: Unsigned_extended.UInt32.t
  ; amount: Unsigned_extended.UInt64.t option
  ; hash: string
  ; failure_status: Failure_status.t option }
[@@deriving to_yojson, equal, sexp, compare]

module Partial = struct
  type t =
    { kind: Kind.t
    ; fee_payer: [`Pk of string]
    ; source: [`Pk of string]
    ; receiver: [`Pk of string]
    ; fee_token: Unsigned_extended.UInt64.t
    ; token: Unsigned_extended.UInt64.t
    ; fee: Unsigned_extended.UInt64.t
    ; amount: Unsigned_extended.UInt64.t option }
  [@@deriving to_yojson, sexp, compare]

  module Reason = Errors.Partial_reason

  let to_user_command_payload :
         ?memo:string
      -> ?valid_until:Unsigned_extended.UInt32.t
      -> t
      -> nonce:Unsigned_extended.UInt32.t
      -> (Signed_command.Payload.t, Errors.t) Result.t =
   fun ?memo ?valid_until t ~nonce ->
    let open Result.Let_syntax in
    let%bind fee_payer_pk =
      pk_to_public_key ~context:"Fee payer" t.fee_payer
    in
    let%bind source_pk = pk_to_public_key ~context:"Source" t.source in
    let%bind receiver_pk = pk_to_public_key ~context:"Receiver" t.receiver in
    let%bind memo =
      match memo with
      | Some memo -> (
        try Ok (Signed_command_memo.create_from_string_exn memo)
        with _ -> Error (Errors.create `Memo_invalid) )
      | None ->
          Ok Signed_command_memo.empty
    in
    let%map body =
      match t.kind with
      | `Payment ->
          let%map amount =
            Result.of_option t.amount
              ~error:
                (Errors.create
                   (`Operations_not_valid
                     [Errors.Partial_reason.Amount_not_some]))
          in
          let payload =
            { Payment_payload.Poly.source_pk
            ; receiver_pk
            ; token_id= Token_id.of_uint64 t.token
            ; amount= Amount_currency.of_uint64 amount }
          in
          Signed_command.Payload.Body.Payment payload
      | `Delegation ->
          let payload =
            Stake_delegation.Set_delegate
              {delegator= source_pk; new_delegate= receiver_pk}
          in
          Result.return @@ Signed_command.Payload.Body.Stake_delegation payload
      | `Create_token ->
          let payload =
            { Mina_base.New_token_payload.token_owner_pk= receiver_pk
            ; disable_new_accounts= false }
          in
          Result.return @@ Signed_command.Payload.Body.Create_new_token payload
      | `Create_token_account ->
          let payload =
            { Mina_base.New_account_payload.token_id= Token_id.of_uint64 t.token
            ; token_owner_pk= source_pk
            ; receiver_pk
            ; account_disabled= false }
          in
          Result.return
          @@ Signed_command.Payload.Body.Create_token_account payload
      | `Mint_tokens ->
          let%map amount =
            Result.of_option t.amount
              ~error:
                (Errors.create
                   (`Operations_not_valid
                     [Errors.Partial_reason.Amount_not_some]))
          in
          let payload =
            { Mina_base.Minting_payload.token_id= Token_id.of_uint64 t.token
            ; token_owner_pk= source_pk
            ; receiver_pk
            ; amount= Amount_currency.of_uint64 amount }
          in
          Signed_command.Payload.Body.Mint_tokens payload
    in
    Signed_command.Payload.create
      ~fee:(Fee_currency.of_uint64 t.fee)
      ~fee_token:(Token_id.of_uint64 t.fee_token)
      ~fee_payer_pk ~nonce ~body ~memo ~valid_until
end

let forget (t : t) : Partial.t =
  { kind= t.kind
  ; fee_payer= t.fee_payer
  ; source= t.source
  ; receiver= t.receiver
  ; fee_token= t.fee_token
  ; token= t.token
  ; fee= t.fee
  ; amount= t.amount }

let remember ~nonce ~hash t =
  { kind= t.kind
  ; fee_payer= t.fee_payer
  ; source= t.source
  ; receiver= t.receiver
  ; fee_token= t.fee_token
  ; token= t.token
  ; fee= t.fee
  ; amount= t.amount
  ; hash
  ; nonce
  ; failure_status= None }

let of_operations (ops : Operation.t list) :
    (Partial.t, Partial.Reason.t) Validation.t =
  (* TODO: If we care about DoS attacks, break early if length too large *)
  (* Note: It's better to have nice errors with the validation than micro-optimize searching through a small list a minimal number of times. *)
  let find_kind k (ops : Operation.t list) =
    let name = Operation_types.name k in
    List.find ops ~f:(fun op -> String.equal op.Operation._type name)
    |> Result.of_option ~error:[Partial.Reason.Can't_find_kind name]
  in
  let module V = Validation in
  let open V.Let_syntax in
  let open Partial.Reason in
  (* For a payment we demand:
    *
    * ops = length exactly 3
    *
    * payment_source_dec with account 'a, some amount 'x, status="Pending"
    * fee_payer_dec with account 'a, some amount 'y, status="Pending"
    * payment_receiver_inc with account 'b, some amount 'x, status="Pending"
  *)
  let payment =
    let%map () =
      if Int.equal (List.length ops) 3 then V.return ()
      else V.fail Length_mismatch
    and account_a =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Payment_source_dec ops
      and {account= account'; _} = find_kind `Fee_payer_dec ops in
      match (account, account') with
      | Some x, Some y when Account_identifier.equal x y ->
          V.return x
      | Some _, Some _ ->
          V.fail Fee_payer_and_source_mismatch
      | None, _ | _, None ->
          V.fail Account_not_some
    and token =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Payment_source_dec ops in
      match account with
      | Some account -> (
        match token_id_of_account account with
        | None ->
            V.fail Incorrect_token_id
        | Some token ->
            V.return token )
      | None ->
          V.fail Account_not_some
    and fee_token =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      match account with
      | Some account -> (
        match token_id_of_account account with
        | Some token_id ->
            V.return token_id
        | None ->
            V.fail Incorrect_token_id )
      | None ->
          V.fail Account_not_some
    and account_b =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Payment_receiver_inc ops in
      Result.of_option account ~error:[Account_not_some]
    and () =
      if
        List.for_all ops ~f:(fun op ->
            Option.equal String.equal op.status (Some "Pending") )
      then V.return ()
      else V.fail Status_not_pending
    and payment_amount_x =
      let open Result.Let_syntax in
      let%bind {amount; _} = find_kind `Payment_source_dec ops
      and {amount= amount'; _} = find_kind `Payment_receiver_inc ops in
      match (amount, amount') with
      | Some x, Some y when Amount.equal (Amount_of.negated x) y ->
          V.return y
      | Some _, Some _ ->
          V.fail Amount_inc_dec_mismatch
      | None, _ | _, None ->
          V.fail Amount_not_some
    and payment_amount_y =
      let open Result.Let_syntax in
      let%bind {amount; _} = find_kind `Fee_payer_dec ops in
      match amount with
      | Some x ->
          V.return (Amount_of.negated x)
      | None ->
          V.fail Amount_not_some
    in
    { Partial.kind= `Payment
    ; fee_payer= `Pk account_a.address
    ; source= `Pk account_a.address
    ; receiver= `Pk account_b.address
    ; fee_token
    ; token (* TODO: Catch exception properly on these uint64 decodes *)
    ; fee= Unsigned.UInt64.of_string payment_amount_y.Amount.value
    ; amount= Some (Unsigned.UInt64.of_string payment_amount_x.Amount.value) }
  in
  (* For a delegation we demand:
    *
    * ops = length exactly 2
    *
    * fee_payer_dec with account 'a, some amount 'y, status="Pending"
    * delegate_change with account 'a, metadata:{delegate_change_target:'b}, status="Pending"
  *)
  let delegation =
    let%map () =
      if Int.equal (List.length ops) 2 then V.return ()
      else V.fail Length_mismatch
    and account_a =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      Option.value_map account ~default:(V.fail Account_not_some) ~f:V.return
    and fee_token =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      match account with
      | Some account -> (
        match token_id_of_account account with
        | Some token_id ->
            V.return token_id
        | None ->
            V.fail Incorrect_token_id )
      | None ->
          V.fail Account_not_some
    and account_b =
      let open Result.Let_syntax in
      let%bind {metadata; _} = find_kind `Delegate_change ops in
      match metadata with
      | Some metadata -> (
        match metadata with
        | `Assoc [("delegate_change_target", `String s)] ->
            return s
        | _ ->
            V.fail Invalid_metadata )
      | None ->
          V.fail Account_not_some
    and () =
      if
        List.for_all ops ~f:(fun op ->
            Option.equal String.equal op.status (Some "Pending") )
      then V.return ()
      else V.fail Status_not_pending
    and payment_amount_y =
      let open Result.Let_syntax in
      let%bind {amount; _} = find_kind `Fee_payer_dec ops in
      match amount with
      | Some x ->
          V.return (Amount_of.negated x)
      | None ->
          V.fail Amount_not_some
    in
    { Partial.kind= `Delegation
    ; fee_payer= `Pk account_a.address
    ; source= `Pk account_a.address
    ; receiver= `Pk account_b
    ; fee_token
    ; token=
        Token_id.(default |> to_uint64)
        (* only default token can be delegated *)
    ; fee= Unsigned.UInt64.of_string payment_amount_y.Amount.value
    ; amount= None }
  in
  (* For token creation, we demand:
    *
    * ops = length exactly 2
    *
    * fee_payer_dec with account 'a, some amount 'y, status="Pending"
    * create_token with account=None, status="Pending"
  *)
  let create_token =
    let%map () =
      if Int.equal (List.length ops) 2 then V.return ()
      else V.fail Length_mismatch
    and account_a =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      Option.value_map account ~default:(V.fail Account_not_some) ~f:V.return
    and fee_token =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      match account with
      | Some account -> (
        match token_id_of_account account with
        | Some token_id ->
            V.return token_id
        | None ->
            V.fail Incorrect_token_id )
      | None ->
          V.fail Account_not_some
    and () =
      if
        List.for_all ops ~f:(fun op ->
            Option.equal String.equal op.status (Some "Pending") )
      then V.return ()
      else V.fail Status_not_pending
    and payment_amount_y =
      let open Result.Let_syntax in
      let%bind {amount; _} = find_kind `Fee_payer_dec ops in
      match amount with
      | Some x ->
          V.return (Amount_of.negated x)
      | None ->
          V.fail Amount_not_some
    (* distinguish create token ops from delegation ops *)
    and () =
      match find_kind `Delegate_change ops with
      | Ok _ ->
          V.fail Invalid_metadata
      | Error _ ->
          V.return ()
    (* distinguish from mint tokens ops *)
    and () =
      match find_kind `Mint_tokens ops with
      | Ok _ ->
          V.fail Invalid_metadata
      | Error _ ->
          V.return ()
    in
    { Partial.kind= `Create_token
    ; fee_payer= `Pk account_a.address
    ; source= `Pk account_a.address
    ; receiver= `Pk account_a.address (* reviewer: is this sane? *)
    ; fee_token
    ; token= Token_id.(default |> to_uint64)
    ; fee= Unsigned.UInt64.of_string payment_amount_y.Amount.value
    ; amount= None }
  in
  (* For token account creation, we demand:
    *
    * ops = length exactly 1
    *
    * fee_payer_dec with account 'a, some amount 'y, status="Pending"
  *)
  let create_token_account =
    let%map () =
      if Int.equal (List.length ops) 1 then V.return ()
      else V.fail Length_mismatch
    and account_a =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      Option.value_map account ~default:(V.fail Account_not_some) ~f:V.return
    and fee_token =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      match account with
      | Some account -> (
        match token_id_of_account account with
        | Some token_id ->
            V.return token_id
        | None ->
            V.fail Incorrect_token_id )
      | None ->
          V.fail Account_not_some
    and () =
      if
        List.for_all ops ~f:(fun op ->
            Option.equal String.equal op.status (Some "Pending") )
      then V.return ()
      else V.fail Status_not_pending
    and payment_amount_y =
      let open Result.Let_syntax in
      let%bind {amount; _} = find_kind `Fee_payer_dec ops in
      match amount with
      | Some x ->
          V.return (Amount_of.negated x)
      | None ->
          V.fail Amount_not_some
    in
    { Partial.kind= `Create_token_account
    ; fee_payer= `Pk account_a.address
    ; source= `Pk account_a.address
    ; receiver= `Pk account_a.address
    ; fee_token
    ; token= Token_id.(default |> to_uint64)
    ; fee= Unsigned.UInt64.of_string payment_amount_y.Amount.value
    ; amount= None }
  in
  (* For token minting, we demand:
    *
    * ops = length exactly 2
    *
    * fee_payer_dec with account 'a, some amount 'y, status="Pending"
    * mint_tokens with account 'a, some amount 'y with the minted token id, metadata={token_owner_pk:'b}, status=Pending
  *)
  let mint_tokens =
    let%map () =
      if Int.equal (List.length ops) 2 then V.return ()
      else V.fail Length_mismatch
    and account_a =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      Option.value_map account ~default:(V.fail Account_not_some) ~f:V.return
    and fee_token =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Fee_payer_dec ops in
      match account with
      | Some account -> (
        match token_id_of_account account with
        | Some token_id ->
            V.return token_id
        | None ->
            V.fail Incorrect_token_id )
      | None ->
          V.fail Account_not_some
    and () =
      if
        List.for_all ops ~f:(fun op ->
            Option.equal String.equal op.status (Some "Pending") )
      then V.return ()
      else V.fail Status_not_pending
    and payment_amount_y =
      let open Result.Let_syntax in
      let%bind {amount; _} = find_kind `Fee_payer_dec ops in
      match amount with
      | Some x ->
          V.return (Amount_of.negated x)
      | None ->
          V.fail Amount_not_some
    and account_b =
      let open Result.Let_syntax in
      let%bind {account; _} = find_kind `Mint_tokens ops in
      Option.value_map account ~default:(V.fail Account_not_some) ~f:V.return
    and amount_b =
      let open Result.Let_syntax in
      let%bind {amount; _} = find_kind `Mint_tokens ops in
      Option.value_map amount ~default:(V.fail Amount_not_some) ~f:V.return
    and account_c =
      let open Result.Let_syntax in
      let%bind {metadata; _} = find_kind `Mint_tokens ops in
      match metadata with
      | Some metadata -> (
        match metadata with
        | `Assoc [("token_owner_pk", `String s)] ->
            return s
        | _ ->
            V.fail Invalid_metadata )
      | None ->
          V.fail Account_not_some
    and token =
      let open Result.Let_syntax in
      let%bind {amount; _} = find_kind `Mint_tokens ops in
      (* check for Amount_not_some already done for amount_b *)
      let Amount.{currency= {symbol; _}; _} = Option.value_exn amount in
      if String.equal symbol "CODA+" then return (Unsigned.UInt64.of_int 2)
      else V.fail Incorrect_token_id
    in
    { Partial.kind= `Mint_tokens
    ; fee_payer= `Pk account_a.address
    ; source= `Pk account_c
    ; receiver= `Pk account_b.address
    ; fee_token
    ; token
    ; fee= Unsigned.UInt64.of_string payment_amount_y.Amount.value
    ; amount= Some (amount_b.Amount.value |> Unsigned.UInt64.of_string) }
  in
  let partials =
    [payment; delegation; create_token; create_token_account; mint_tokens]
  in
  let oks, errs = List.partition_map partials ~f:Result.ok_fst in
  match (oks, errs) with
  | [], errs ->
      (* no Oks *)
      Error (List.concat errs)
  | [partial], _ ->
      (* exactly one Ok *)
      Ok partial
  | _, _ ->
      (* more than one Ok, a bug in our implementation *)
      failwith
        "A sequence of operations must represent exactly one user command"

let to_operations ~failure_status (t : Partial.t) : Operation.t list =
  (* First build a plan. The plan specifies all operations ahead of time so
     * we can later compute indices and relations when we're building the full
     * models.
     *
     * For now, relations will be defined only on the two sides of a given
     * transfer. ie. Source decreases, and receiver increases.
  *)
  let plan : 'a Op.t list =
    ( if not Unsigned.UInt64.(equal t.fee zero) then
      [{Op.label= `Fee_payer_dec; related_to= None}]
    else [] )
    @ ( match failure_status with
      | Some (`Applied (Account_creation_fees_paid.By_receiver amount)) ->
          [ { Op.label= `Account_creation_fee_via_payment amount
            ; related_to= None } ]
      | Some (`Applied (Account_creation_fees_paid.By_fee_payer amount)) ->
          [ { Op.label= `Account_creation_fee_via_fee_payer amount
            ; related_to= None } ]
      | _ ->
          [] )
    @
    match t.kind with
    | `Payment -> (
      (* When amount is not none, we move the amount from source to receiver -- unless it's a failure, we will capture that below *)
      match t.amount with
      | Some amount ->
          [ {Op.label= `Payment_source_dec amount; related_to= None}
          ; { Op.label= `Payment_receiver_inc amount
            ; related_to= Some (`Payment_source_dec amount) } ]
      | None ->
          [] )
    | `Delegation ->
        [{Op.label= `Delegate_change; related_to= None}]
    | `Create_token ->
        [{Op.label= `Create_token; related_to= None}]
    | `Create_token_account ->
        [] (* Covered by account creation fee *)
    | `Mint_tokens -> (
      (* When amount is not none, the amount goes to receiver's account *)
      match t.amount with
      | Some amount ->
          [{Op.label= `Mint_tokens amount; related_to= None}]
      | None ->
          [] )
  in
  Op.build
    ~a_eq:
      [%eq:
        [ `Fee_payer_dec
        | `Payment_source_dec of Unsigned.UInt64.t
        | `Payment_receiver_inc of Unsigned.UInt64.t ]] ~plan
    ~f:(fun ~related_operations ~operation_identifier op ->
      let status, metadata, did_fail =
        match (op.label, failure_status) with
        (* If we're looking at mempool transactions, it's always pending *)
        | _, None ->
            (`Pending, None, false)
        | _, Some (`Applied _) ->
            (`Success, None, false)
        | _, Some (`Failed reason) ->
            (`Failed, Some (`Assoc [("reason", `String reason)]), true)
      in
      let pending_or_success_only = function
        | `Pending ->
            `Pending
        | `Success | `Failed ->
            `Success
      in
      let merge_metadata m1 m2 =
        match (m1, m2) with
        | None, None ->
            None
        | Some x, None | None, Some x ->
            Some x
        | Some (`Assoc xs), Some (`Assoc ys) ->
            Some (`Assoc (xs @ ys))
        | _ ->
            failwith "Unexpected pattern"
      in
      match op.label with
      | `Fee_payer_dec ->
          { Operation.operation_identifier
          ; related_operations
          ; status=
              Some
                (status |> pending_or_success_only |> Operation_statuses.name)
          ; account= Some (account_id t.fee_payer t.fee_token)
          ; _type= Operation_types.name `Fee_payer_dec
          ; amount= Some Amount_of.(negated @@ token t.fee_token t.fee)
          ; coin_change= None
          ; metadata }
      | `Payment_source_dec amount ->
          { Operation.operation_identifier
          ; related_operations
          ; status= Some (Operation_statuses.name status)
          ; account= Some (account_id t.source t.token)
          ; _type= Operation_types.name `Payment_source_dec
          ; amount=
              ( if did_fail then None
              else Some Amount_of.(negated @@ token t.token amount) )
          ; coin_change= None
          ; metadata }
      | `Payment_receiver_inc amount ->
          { Operation.operation_identifier
          ; related_operations
          ; status= Some (Operation_statuses.name status)
          ; account= Some (account_id t.receiver t.token)
          ; _type= Operation_types.name `Payment_receiver_inc
          ; amount=
              (if did_fail then None else Some (Amount_of.token t.token amount))
          ; coin_change= None
          ; metadata }
      | `Account_creation_fee_via_payment account_creation_fee ->
          { Operation.operation_identifier
          ; related_operations
          ; status= Some (Operation_statuses.name status)
          ; account= Some (account_id t.receiver t.token)
          ; _type= Operation_types.name `Account_creation_fee_via_payment
          ; amount= Some Amount_of.(negated @@ coda account_creation_fee)
          ; coin_change= None
          ; metadata }
      | `Account_creation_fee_via_fee_payer account_creation_fee ->
          { Operation.operation_identifier
          ; related_operations
          ; status= Some (Operation_statuses.name status)
          ; account= Some (account_id t.fee_payer t.fee_token)
          ; _type= Operation_types.name `Account_creation_fee_via_fee_payer
          ; amount= Some Amount_of.(negated @@ coda account_creation_fee)
          ; coin_change= None
          ; metadata }
      | `Create_token ->
          { Operation.operation_identifier
          ; related_operations
          ; status= Some (Operation_statuses.name status)
          ; account= None
          ; _type= Operation_types.name `Create_token
          ; amount= None
          ; coin_change= None
          ; metadata }
      | `Delegate_change ->
          { Operation.operation_identifier
          ; related_operations
          ; status= Some (Operation_statuses.name status)
          ; account= Some (account_id t.source Amount_of.Token_id.default)
          ; _type= Operation_types.name `Delegate_change
          ; amount= None
          ; coin_change= None
          ; metadata=
              merge_metadata metadata
                (Some
                   (`Assoc
                     [ ( "delegate_change_target"
                       , `String
                           (let (`Pk r) = t.receiver in
                            r) ) ])) }
      | `Mint_tokens amount ->
          { Operation.operation_identifier
          ; related_operations
          ; status= Some (Operation_statuses.name status)
          ; account= Some (account_id t.receiver t.token)
          ; _type= Operation_types.name `Mint_tokens
          ; amount= Some (Amount_of.token t.token amount)
          ; coin_change= None
          ; metadata=
              merge_metadata metadata
                (Some
                   (`Assoc
                     [ ( "token_owner_pk"
                       , `String
                           (let (`Pk r) = t.source in
                            r) ) ])) } )

let to_operations' (t : t) : Operation.t list =
  to_operations ~failure_status:t.failure_status (forget t)

let%test_unit "payment_round_trip" =
  let start =
    { kind= `Payment (* default token *)
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= Some (Unsigned.UInt64.of_int 2_000_000_000)
    ; failure_status= None
    ; hash= "TXN_1_HASH" }
  in
  let ops = to_operations' start in
  match of_operations ops with
  | Ok partial ->
      [%test_eq: Partial.t] partial (forget start)
  | Error e ->
      failwithf !"Mismatch because %{sexp: Partial.Reason.t list}" e ()

let%test_unit "delegation_round_trip" =
  let start =
    { kind= `Delegation
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 1_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 42
    ; amount= None
    ; failure_status= None
    ; hash= "TXN_2_HASH" }
  in
  let ops = to_operations' start in
  match of_operations ops with
  | Ok partial ->
      [%test_eq: Partial.t] partial (forget start)
  | Error e ->
      failwithf !"Mismatch because %{sexp: Partial.Reason.t list}" e ()

let dummies =
  [ { kind= `Payment (* default token *)
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= Some (Unsigned.UInt64.of_int 2_000_000_000)
    ; failure_status= Some (`Applied Account_creation_fees_paid.By_no_one)
    ; hash= "TXN_1_HASH" }
  ; { kind= `Payment (* new account created *)
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= Some (Unsigned.UInt64.of_int 2_000_000_000)
    ; failure_status=
        Some
          (`Applied
            (Account_creation_fees_paid.By_receiver
               (Unsigned.UInt64.of_int 1_000_000)))
    ; hash= "TXN_1new_HASH" }
  ; { kind= `Payment (* failed payment *)
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= Some (Unsigned.UInt64.of_int 2_000_000_000)
    ; failure_status= Some (`Failed "Failure")
    ; hash= "TXN_1fail_HASH" }
  ; { kind= `Payment (* custom token *)
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 3
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= Some (Unsigned.UInt64.of_int 2_000_000_000)
    ; failure_status= Some (`Applied Account_creation_fees_paid.By_no_one)
    ; hash= "TXN_1a_HASH" }
  ; { kind= `Payment (* custom fee-token *)
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 3
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= Some (Unsigned.UInt64.of_int 2_000_000_000)
    ; failure_status= Some (`Applied Account_creation_fees_paid.By_no_one)
    ; hash= "TXN_1b_HASH" }
  ; { kind= `Delegation
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= None
    ; failure_status= Some (`Applied Account_creation_fees_paid.By_no_one)
    ; hash= "TXN_2_HASH" }
  ; { kind= `Create_token (* no new account *)
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= None
    ; failure_status= Some (`Applied Account_creation_fees_paid.By_no_one)
    ; hash= "TXN_3a_HASH" }
  ; { kind= `Create_token (* new account fee *)
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= None
    ; failure_status=
        Some
          (`Applied
            (Account_creation_fees_paid.By_fee_payer
               (Unsigned.UInt64.of_int 3_000)))
    ; hash= "TXN_3b_HASH" }
  ; { kind= `Create_token_account
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 1
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= None
    ; failure_status= Some (`Applied Account_creation_fees_paid.By_no_one)
    ; hash= "TXN_4_HASH" }
  ; { kind= `Mint_tokens
    ; fee_payer= `Pk "Alice"
    ; source= `Pk "Alice"
    ; token= Unsigned.UInt64.of_int 10
    ; fee= Unsigned.UInt64.of_int 2_000_000_000
    ; receiver= `Pk "Bob"
    ; fee_token= Unsigned.UInt64.of_int 1
    ; nonce= Unsigned.UInt32.of_int 3
    ; amount= Some (Unsigned.UInt64.of_int 30_000)
    ; failure_status= Some (`Applied Account_creation_fees_paid.By_no_one)
    ; hash= "TXN_5_HASH" } ]
