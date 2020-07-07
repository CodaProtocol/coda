open Core
open Import
open Snark_params.Tick

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Ledger_hash.Stable.V1.t
      , Account_id.Stable.V1.t
      , Account.Stable.V1.t
      , Token_id.Stable.V1.t )
      Sparse_ledger_lib.Sparse_ledger.T.Stable.V1.t
    [@@deriving to_yojson, sexp]

    let to_latest = Fn.id
  end
end]

type t = Stable.Latest.t [@@deriving sexp]

module Hash = struct
  include Ledger_hash

  let merge = Ledger_hash.merge
end

module Account = struct
  include Account

  let data_hash = Fn.compose Ledger_hash.of_digest Account.digest
end

module M =
  Sparse_ledger_lib.Sparse_ledger.Make (Hash) (Token_id) (Account_id) (Account)

[%%define_locally
M.
  ( of_hash
  , to_yojson
  , get_exn
  , path_exn
  , set_exn
  , find_index_exn
  , add_path
  , merkle_root
  , iteri
  , next_available_token )]

let of_root ~depth ~next_available_token (h : Ledger_hash.t) =
  of_hash ~depth ~next_available_token
    (Ledger_hash.of_digest (h :> Random_oracle.Digest.t))

let of_ledger_root ledger =
  of_root ~depth:(Ledger.depth ledger)
    ~next_available_token:(Ledger.next_available_token ledger)
    (Ledger.merkle_root ledger)

let of_any_ledger (ledger : Ledger.Any_ledger.witness) =
  Ledger.Any_ledger.M.foldi ledger
    ~init:
      (of_root
         ~depth:(Ledger.Any_ledger.M.depth ledger)
         ~next_available_token:
           (Ledger.Any_ledger.M.next_available_token ledger)
         (Ledger.Any_ledger.M.merkle_root ledger))
    ~f:(fun _addr sparse_ledger account ->
      let loc =
        Option.value_exn
          (Ledger.Any_ledger.M.location_of_account ledger
             (Account.identifier account))
      in
      add_path sparse_ledger
        (Ledger.Any_ledger.M.merkle_path ledger loc)
        (Account.identifier account)
        (Option.value_exn (Ledger.Any_ledger.M.get ledger loc)) )

let of_ledger_subset_exn (oledger : Ledger.t) keys =
  let ledger = Ledger.copy oledger in
  let _, sparse =
    List.fold keys
      ~f:(fun (new_keys, sl) key ->
        match Ledger.location_of_account ledger key with
        | Some loc ->
            ( new_keys
            , add_path sl
                (Ledger.merkle_path ledger loc)
                key
                ( Ledger.get ledger loc
                |> Option.value_exn ?here:None ?error:None ?message:None ) )
        | None ->
            let path, acct = Ledger.create_empty ledger key in
            (key :: new_keys, add_path sl path key acct) )
      ~init:([], of_ledger_root ledger)
  in
  Debug_assert.debug_assert (fun () ->
      [%test_eq: Ledger_hash.t]
        (Ledger.merkle_root ledger)
        ((merkle_root sparse :> Random_oracle.Digest.t) |> Ledger_hash.of_hash)
  ) ;
  sparse

let of_ledger_index_subset_exn (ledger : Ledger.Any_ledger.witness) indexes =
  List.fold indexes
    ~init:
      (of_root
         ~depth:(Ledger.Any_ledger.M.depth ledger)
         ~next_available_token:
           (Ledger.Any_ledger.M.next_available_token ledger)
         (Ledger.Any_ledger.M.merkle_root ledger))
    ~f:(fun acc i ->
      let account = Ledger.Any_ledger.M.get_at_index_exn ledger i in
      add_path acc
        (Ledger.Any_ledger.M.merkle_path_at_index_exn ledger i)
        (Account.identifier account)
        account )

let%test_unit "of_ledger_subset_exn with keys that don't exist works" =
  let keygen () =
    let privkey = Private_key.create () in
    (privkey, Public_key.of_private_key_exn privkey |> Public_key.compress)
  in
  Ledger.with_ledger
    ~depth:Genesis_constants.Constraint_constants.for_unit_tests.ledger_depth
    ~f:(fun ledger ->
      let _, pub1 = keygen () in
      let _, pub2 = keygen () in
      let aid1 = Account_id.create pub1 Token_id.default in
      let aid2 = Account_id.create pub2 Token_id.default in
      let sl = of_ledger_subset_exn ledger [aid1; aid2] in
      [%test_eq: Ledger_hash.t]
        (Ledger.merkle_root ledger)
        ((merkle_root sl :> Random_oracle.Digest.t) |> Ledger_hash.of_hash) )

let get_or_initialize_exn account_id t idx =
  let account = get_exn t idx in
  if Public_key.Compressed.(equal empty account.public_key) then
    let public_key = Account_id.public_key account_id in
    ( `Added
    , { account with
        delegate= public_key
      ; public_key
      ; token_id= Account_id.token_id account_id } )
  else (`Existed, account)

let sub_account_creation_fee
    ~(constraint_constants : Genesis_constants.Constraint_constants.t) action
    (amount : Currency.Amount.t) =
  if action = `Added then
    Option.value_exn
      Currency.Amount.(
        sub amount (of_fee constraint_constants.account_creation_fee))
  else amount

let apply_user_command_exn
    ~(constraint_constants : Genesis_constants.Constraint_constants.t)
    ~txn_global_slot t
    ({signer; payload; signature= _} as user_command : User_command.t) =
  let open Currency in
  let signer_pk = Public_key.compress signer in
  let current_global_slot = txn_global_slot in
  (* Fee-payer information *)
  let fee_token = User_command.fee_token user_command in
  let fee_payer = User_command.fee_payer user_command in
  let nonce = User_command.nonce user_command in
  assert (
    Public_key.Compressed.equal (Account_id.public_key fee_payer) signer_pk ) ;
  assert (Token_id.equal fee_token Token_id.default) ;
  let fee_payer_idx, fee_payer_account =
    let idx = find_index_exn t fee_payer in
    let account = get_exn t idx in
    assert (Account.Nonce.equal account.nonce nonce) ;
    let fee = User_command.fee user_command in
    let timing =
      Or_error.ok_exn
      @@ Transaction_logic.validate_timing ~txn_amount:(Amount.of_fee fee)
           ~txn_global_slot:current_global_slot ~account
    in
    ( idx
    , { account with
        nonce= Account.Nonce.succ account.nonce
      ; balance=
          Balance.sub_amount account.balance (Amount.of_fee fee)
          |> Option.value_exn ?here:None ?error:None ?message:None
      ; receipt_chain_hash=
          Receipt.Chain_hash.cons payload account.receipt_chain_hash
      ; timing } )
  in
  (* Charge the fee. *)
  let t = set_exn t fee_payer_idx fee_payer_account in
  let next_available_token = next_available_token t in
  let source = User_command.source ~next_available_token user_command in
  let receiver = User_command.receiver ~next_available_token user_command in
  let exception Reject of exn in
  let charge_account_creation_fee_exn (account : Account.t) =
    let balance =
      Option.value_exn
        (Balance.sub_amount account.balance
           (Amount.of_fee constraint_constants.account_creation_fee))
    in
    let account = {account with balance} in
    let timing =
      Or_error.ok_exn
        (Transaction_logic.validate_timing ~txn_amount:Amount.zero
           ~txn_global_slot:current_global_slot ~account)
    in
    {account with timing}
  in
  let compute_updates () =
    (* Raise an exception if any of the invariants for the user command are not
       satisfied, so that the command will not go through.

       This must re-check the conditions in Transaction_logic, to ensure that
       the failure cases are consistent.
    *)
    let predicate_passed =
      if
        Public_key.Compressed.equal
          (User_command.fee_payer_pk user_command)
          (User_command.source_pk user_command)
      then true
      else
        match payload.body with
        | Create_new_token _ ->
            (* Any account is allowed to create a new token associated with a
               public key.
            *)
            true
        | Create_token_account _ ->
            (* Predicate failure is deferred here. It will be checked later. *)
            let predicate_result =
              (* TODO(#4554): Hook predicate evaluation in here once
                 implemented.
              *)
              false
            in
            predicate_result
        | Payment _
        | Stake_delegation _
        | Mint_tokens _
        | Set_token_permissions _
        | Set_account_permissions _ ->
            (* TODO(#4554): Hook predicate evaluation in here once implemented. *)
            failwith
              "The fee-payer is not authorised to issue commands for the \
               source account"
    in
    match User_command.Payload.body payload with
    | Stake_delegation _ ->
        let receiver_account = get_exn t @@ find_index_exn t receiver in
        (* Check that receiver account exists. *)
        assert (
          not Public_key.Compressed.(equal empty receiver_account.public_key)
        ) ;
        let source_idx = find_index_exn t source in
        let source_account = get_exn t source_idx in
        (* Check that source account exists. *)
        assert (
          not Public_key.Compressed.(equal empty source_account.public_key) ) ;
        let source_account =
          (* Timing is always valid, but we need to record any switch from
             timed to untimed here to stay in sync with the snark.
          *)
          let timing =
            Or_error.ok_exn
            @@ Transaction_logic.validate_timing ~txn_amount:Amount.zero
                 ~txn_global_slot:current_global_slot ~account:source_account
          in
          {source_account with delegate= Account_id.public_key receiver; timing}
        in
        [(source_idx, source_account)]
    | Payment {amount; token_id= token; _} ->
        let receiver_idx = find_index_exn t receiver in
        let action, receiver_account =
          get_or_initialize_exn receiver t receiver_idx
        in
        let receiver_amount =
          if Token_id.(equal default) token then
            sub_account_creation_fee ~constraint_constants action amount
          else if action = `Added then
            failwith "Receiver account does not exist, and we cannot create it"
          else amount
        in
        let receiver_account =
          { receiver_account with
            balance=
              Balance.add_amount receiver_account.balance receiver_amount
              |> Option.value_exn ?here:None ?error:None ?message:None }
        in
        let source_idx = find_index_exn t source in
        let source_account =
          let account =
            if Account_id.equal source receiver then (
              assert (action = `Existed) ;
              receiver_account )
            else get_exn t source_idx
          in
          (* Check that source account exists. *)
          assert (not Public_key.Compressed.(equal empty account.public_key)) ;
          try
            { account with
              balance=
                Balance.sub_amount account.balance amount
                |> Option.value_exn ?here:None ?error:None ?message:None
            ; timing=
                Or_error.ok_exn
                @@ Transaction_logic.validate_timing ~txn_amount:amount
                     ~txn_global_slot:current_global_slot ~account }
          with exn when Account_id.equal fee_payer source ->
            (* Don't process transactions with insufficient balance from the
               fee-payer.
            *)
            raise (Reject exn)
        in
        [(receiver_idx, receiver_account); (source_idx, source_account)]
    | Create_new_token {disable_new_accounts; _} ->
        (* NOTE: source and receiver are definitionally equal here. *)
        let fee_payer_account =
          try charge_account_creation_fee_exn fee_payer_account
          with exn -> raise (Reject exn)
        in
        let receiver_idx = find_index_exn t receiver in
        let action, receiver_account =
          get_or_initialize_exn receiver t receiver_idx
        in
        if not (action = `Added) then
          raise
            (Reject
               (Failure
                  "Token owner account for newly created token already \
                   exists?!?!")) ;
        let receiver_account =
          { receiver_account with
            token_permissions=
              Token_permissions.Token_owned {disable_new_accounts} }
        in
        [(fee_payer_idx, fee_payer_account); (receiver_idx, receiver_account)]
    | Create_token_account {account_disabled; _} ->
        if
          account_disabled
          && Token_id.(equal default) (Account_id.token_id receiver)
        then
          raise
            (Reject
               (Failure "Cannot open a disabled account in the default token")) ;
        let fee_payer_account =
          try charge_account_creation_fee_exn fee_payer_account
          with exn -> raise (Reject exn)
        in
        let receiver_idx = find_index_exn t receiver in
        let action, receiver_account =
          get_or_initialize_exn receiver t receiver_idx
        in
        if action = `Existed then
          failwith "Attempted to create an account that already exists" ;
        let receiver_account =
          { receiver_account with
            token_permissions= Token_permissions.Not_owned {account_disabled}
          }
        in
        let source_idx = find_index_exn t source in
        let source_account =
          if Account_id.equal source receiver then receiver_account
          else if Account_id.equal source fee_payer then fee_payer_account
          else
            match get_or_initialize_exn receiver t source_idx with
            | `Added, _ ->
                failwith "Source account does not exist"
            | `Existed, source_account ->
                source_account
        in
        let () =
          match source_account.token_permissions with
          | Token_owned {disable_new_accounts} ->
              if
                not
                  ( Bool.equal account_disabled disable_new_accounts
                  || predicate_passed )
              then
                failwith
                  "The fee-payer is not authorised to create token accounts \
                   for this token"
          | Not_owned _ ->
              if Token_id.(equal default) (Account_id.token_id receiver) then
                ()
              else failwith "Token owner account does not own the token"
        in
        let source_account =
          let timing =
            Or_error.ok_exn
            @@ Transaction_logic.validate_timing ~txn_amount:Amount.zero
                 ~txn_global_slot:current_global_slot ~account:source_account
          in
          {source_account with timing}
        in
        if Account_id.equal source receiver then
          (* For token_id= default, we allow this *)
          [(fee_payer_idx, fee_payer_account); (source_idx, source_account)]
        else
          [ (receiver_idx, receiver_account)
          ; (fee_payer_idx, fee_payer_account)
          ; (source_idx, source_account) ]
    | Mint_tokens {token_id= token; amount; _} ->
        assert (not (Token_id.(equal default) token)) ;
        let receiver_idx = find_index_exn t receiver in
        let action, receiver_account =
          get_or_initialize_exn receiver t receiver_idx
        in
        assert (action = `Existed) ;
        let receiver_account =
          { receiver_account with
            balance=
              Balance.add_amount receiver_account.balance amount
              |> Option.value_exn ?here:None ?error:None ?message:None }
        in
        let source_idx = find_index_exn t source in
        let source_account =
          let account =
            if Account_id.equal source receiver then receiver_account
            else get_exn t source_idx
          in
          (* Check that source account exists. *)
          assert (not Public_key.Compressed.(equal empty account.public_key)) ;
          (* Check that source account owns the token. *)
          let () =
            match account.token_permissions with
            | Token_owned _ ->
                ()
            | Not_owned _ ->
                failwithf
                  !"The claimed token owner %{sexp: Account_id.t} does not \
                    own the token %{sexp: Token_id.t}"
                  source token ()
          in
          { account with
            timing=
              Or_error.ok_exn
              @@ Transaction_logic.validate_timing ~txn_amount:Amount.zero
                   ~txn_global_slot:current_global_slot ~account }
        in
        [(receiver_idx, receiver_account); (source_idx, source_account)]
    | Set_token_permissions
        {token_id= token; disable_new_accounts= token_locked; _}
    | Set_account_permissions {token_id= token; disabled= token_locked; _} ->
        assert (not (Token_id.(equal default) token)) ;
        let receiver_idx = find_index_exn t receiver in
        let action, receiver_account =
          get_or_initialize_exn receiver t receiver_idx
        in
        assert (action = `Existed) ;
        let receiver_account =
          let token_permissions =
            match receiver_account.token_permissions with
            | Token_owned _ ->
                Token_permissions.Token_owned
                  {disable_new_accounts= token_locked}
            | Not_owned _ ->
                Not_owned {account_disabled= token_locked}
          in
          {receiver_account with token_permissions}
        in
        let source_idx = find_index_exn t source in
        let source_account =
          let account =
            if Account_id.equal source receiver then receiver_account
            else get_exn t source_idx
          in
          (* Check that source account exists. *)
          assert (not Public_key.Compressed.(equal empty account.public_key)) ;
          (* Check that source account owns the token. *)
          let () =
            match account.token_permissions with
            | Token_owned _ ->
                ()
            | Not_owned _ ->
                failwithf
                  !"The claimed token owner %{sexp: Account_id.t} does not \
                    own the token %{sexp: Token_id.t}"
                  source token ()
          in
          { account with
            timing=
              Or_error.ok_exn
              @@ Transaction_logic.validate_timing ~txn_amount:Amount.zero
                   ~txn_global_slot:current_global_slot ~account }
        in
        [(receiver_idx, receiver_account); (source_idx, source_account)]
  in
  try
    let indexed_accounts = compute_updates () in
    (* User command succeeded, update accounts in the ledger. *)
    List.fold ~init:t indexed_accounts ~f:(fun t (idx, account) ->
        set_exn t idx account )
  with
  | Reject exn ->
      (* TODO: These transactions should never reach this stage, this error
         should be fatal.
      *)
      raise exn
  | _ ->
      (* Not able to apply the user command successfully, charge fee only. *)
      t

let apply_fee_transfer_exn ~constraint_constants =
  let apply_single t (ft : Fee_transfer.Single.t) =
    let account_id = Fee_transfer.Single.receiver ft in
    let index = find_index_exn t account_id in
    let action, account = get_or_initialize_exn account_id t index in
    let open Currency in
    let amount = Amount.of_fee ft.fee in
    let balance =
      let amount' =
        sub_account_creation_fee ~constraint_constants action amount
      in
      Option.value_exn (Balance.add_amount account.balance amount')
    in
    set_exn t index {account with balance}
  in
  fun t transfer -> Fee_transfer.fold transfer ~f:apply_single ~init:t

let apply_coinbase_exn ~constraint_constants t
    ({receiver; fee_transfer; amount= coinbase_amount} : Coinbase.t) =
  let open Currency in
  let add_to_balance t pk amount =
    let idx = find_index_exn t pk in
    let action, a = get_or_initialize_exn pk t idx in
    let balance =
      let amount' =
        sub_account_creation_fee ~constraint_constants action amount
      in
      Option.value_exn (Balance.add_amount a.balance amount')
    in
    set_exn t idx {a with balance}
  in
  let receiver_reward, t =
    match fee_transfer with
    | None ->
        (coinbase_amount, t)
    | Some ({receiver_pk= _; fee} as ft) ->
        let fee = Amount.of_fee fee in
        let reward =
          Amount.sub coinbase_amount fee
          |> Option.value_exn ?here:None ?message:None ?error:None
        in
        let transferee_id = Coinbase.Fee_transfer.receiver ft in
        (reward, add_to_balance t transferee_id fee)
  in
  let receiver_id = Account_id.create receiver Token_id.default in
  add_to_balance t receiver_id receiver_reward

let apply_transaction_exn ~constraint_constants ~txn_global_slot t
    (transition : Transaction.t) =
  match transition with
  | Fee_transfer tr ->
      apply_fee_transfer_exn ~constraint_constants t tr
  | User_command cmd ->
      apply_user_command_exn ~constraint_constants ~txn_global_slot t
        (cmd :> User_command.t)
  | Coinbase c ->
      apply_coinbase_exn ~constraint_constants t c

let merkle_root t =
  Ledger_hash.of_hash (merkle_root t :> Random_oracle.Digest.t)

let handler t =
  let ledger = ref t in
  let path_exn idx =
    List.map (path_exn !ledger idx) ~f:(function `Left h -> h | `Right h -> h)
  in
  stage (fun (With {request; respond}) ->
      match request with
      | Ledger_hash.Get_element idx ->
          let elt = get_exn !ledger idx in
          let path = (path_exn idx :> Random_oracle.Digest.t list) in
          respond (Provide (elt, path))
      | Ledger_hash.Get_path idx ->
          let path = (path_exn idx :> Random_oracle.Digest.t list) in
          respond (Provide path)
      | Ledger_hash.Set (idx, account) ->
          ledger := set_exn !ledger idx account ;
          respond (Provide ())
      | Ledger_hash.Find_index pk ->
          let index = find_index_exn !ledger pk in
          respond (Provide index)
      | _ ->
          unhandled )
