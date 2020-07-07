open Core
open Currency
open Signature_lib
module Global_slot = Coda_numbers.Global_slot

module type Ledger_intf = sig
  type t

  type location

  val get : t -> location -> Account.t option

  val location_of_account : t -> Account_id.t -> location option

  val set : t -> location -> Account.t -> unit

  val get_or_create :
    t -> Account_id.t -> [`Added | `Existed] * Account.t * location

  val get_or_create_account_exn :
    t -> Account_id.t -> Account.t -> [`Added | `Existed] * location

  val remove_accounts_exn : t -> Account_id.t list -> unit

  val merkle_root : t -> Ledger_hash.t

  val with_ledger : depth:int -> f:(t -> 'a) -> 'a

  val next_available_token : t -> Token_id.t

  val set_next_available_token : t -> Token_id.t -> unit
end

module Undo = struct
  module UC = User_command

  module User_command_undo = struct
    module Common = struct
      [%%versioned
      module Stable = struct
        module V1 = struct
          type t =
            { user_command: User_command.Stable.V1.t
            ; previous_receipt_chain_hash: Receipt.Chain_hash.Stable.V1.t
            ; fee_payer_timing: Account.Timing.Stable.V1.t
            ; source_timing: Account.Timing.Stable.V1.t option }
          [@@deriving sexp]

          let to_latest = Fn.id
        end
      end]

      type t = Stable.Latest.t =
        { user_command: User_command.t
        ; previous_receipt_chain_hash: Receipt.Chain_hash.t
        ; fee_payer_timing: Account.Timing.t
        ; source_timing: Account.Timing.t option }
      [@@deriving sexp]
    end

    module Body = struct
      [%%versioned
      module Stable = struct
        module V1 = struct
          type t =
            | Payment of {previous_empty_accounts: Account_id.Stable.V1.t list}
            | Stake_delegation of
                { previous_delegate: Public_key.Compressed.Stable.V1.t }
            | Create_new_token of {created_token: Token_id.Stable.V1.t}
            | Create_token_account
            | Mint_tokens
            | Set_token_permissions of {previous_token_locked: bool}
            | Failed
          [@@deriving sexp]

          let to_latest = Fn.id
        end
      end]

      type t = Stable.Latest.t =
        | Payment of {previous_empty_accounts: Account_id.t list}
        | Stake_delegation of {previous_delegate: Public_key.Compressed.t}
        | Create_new_token of {created_token: Token_id.t}
        | Create_token_account
        | Mint_tokens
        | Set_token_permissions of {previous_token_locked: bool}
        | Failed
      [@@deriving sexp]
    end

    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = {common: Common.Stable.V1.t; body: Body.Stable.V1.t}
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]

    (* bin_io omitted *)
    type t = Stable.Latest.t = {common: Common.t; body: Body.t}
    [@@deriving sexp]
  end

  module Fee_transfer_undo = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          { fee_transfer: Fee_transfer.Stable.V1.t
          ; previous_empty_accounts: Account_id.Stable.V1.t list }
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]

    type t = Stable.Latest.t =
      {fee_transfer: Fee_transfer.t; previous_empty_accounts: Account_id.t list}
    [@@deriving sexp]
  end

  module Coinbase_undo = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          { coinbase: Coinbase.Stable.V1.t
          ; previous_empty_accounts: Account_id.Stable.V1.t list }
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]

    (* bin_io omitted *)
    type t = Stable.Latest.t =
      {coinbase: Coinbase.t; previous_empty_accounts: Account_id.t list}
    [@@deriving sexp]
  end

  module Varying = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          | User_command of User_command_undo.Stable.V1.t
          | Fee_transfer of Fee_transfer_undo.Stable.V1.t
          | Coinbase of Coinbase_undo.Stable.V1.t
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]

    (* bin_io omitted *)
    type t = Stable.Latest.t =
      | User_command of User_command_undo.t
      | Fee_transfer of Fee_transfer_undo.t
      | Coinbase of Coinbase_undo.t
    [@@deriving sexp]
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        {previous_hash: Ledger_hash.Stable.V1.t; varying: Varying.Stable.V1.t}
      [@@deriving sexp]

      let to_latest = Fn.id
    end
  end]

  (* bin_io omitted *)
  type t = Stable.Latest.t = {previous_hash: Ledger_hash.t; varying: Varying.t}
  [@@deriving sexp]
end

module type S = sig
  type ledger

  module Undo : sig
    module User_command_undo : sig
      module Common : sig
        type t = Undo.User_command_undo.Common.t =
          { user_command: User_command.t
          ; previous_receipt_chain_hash: Receipt.Chain_hash.t
          ; fee_payer_timing: Account.Timing.t
          ; source_timing: Account.Timing.t option }
        [@@deriving sexp]
      end

      module Body : sig
        type t = Undo.User_command_undo.Body.t =
          | Payment of {previous_empty_accounts: Account_id.t list}
          | Stake_delegation of {previous_delegate: Public_key.Compressed.t}
          | Create_new_token of {created_token: Token_id.t}
          | Create_token_account
          | Mint_tokens
          | Set_token_permissions of {previous_token_locked: bool}
          | Failed
        [@@deriving sexp]
      end

      type t = Undo.User_command_undo.t = {common: Common.t; body: Body.t}
      [@@deriving sexp]
    end

    module Fee_transfer_undo : sig
      type t = Undo.Fee_transfer_undo.t =
        { fee_transfer: Fee_transfer.t
        ; previous_empty_accounts: Account_id.t list }
      [@@deriving sexp]
    end

    module Coinbase_undo : sig
      type t = Undo.Coinbase_undo.t =
        {coinbase: Coinbase.t; previous_empty_accounts: Account_id.t list}
      [@@deriving sexp]
    end

    module Varying : sig
      type t = Undo.Varying.t =
        | User_command of User_command_undo.t
        | Fee_transfer of Fee_transfer_undo.t
        | Coinbase of Coinbase_undo.t
      [@@deriving sexp]
    end

    type t = Undo.t = {previous_hash: Ledger_hash.t; varying: Varying.t}
    [@@deriving sexp]

    val transaction : t -> Transaction.t Or_error.t
  end

  val apply_user_command :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_global_slot:Global_slot.t
    -> ledger
    -> User_command.With_valid_signature.t
    -> Undo.User_command_undo.t Or_error.t

  val apply_transaction :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_global_slot:Global_slot.t
    -> ledger
    -> Transaction.t
    -> Undo.t Or_error.t

  val merkle_root_after_user_command_exn :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_global_slot:Global_slot.t
    -> ledger
    -> User_command.With_valid_signature.t
    -> Ledger_hash.t * [`Next_available_token of Token_id.t]

  val undo :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> ledger
    -> Undo.t
    -> unit Or_error.t

  module For_tests : sig
    val validate_timing :
         account:Account.t
      -> txn_amount:Amount.t
      -> txn_global_slot:Global_slot.t
      -> Account.Timing.t Or_error.t
  end
end

let validate_timing ~account ~txn_amount ~txn_global_slot =
  let open Account.Poly in
  let open Account.Timing.Poly in
  match account.timing with
  | Untimed ->
      (* no time restrictions *)
      Or_error.return Untimed
  | Timed
      {initial_minimum_balance; cliff_time; vesting_period; vesting_increment}
    ->
      let open Or_error.Let_syntax in
      let%map curr_min_balance =
        let account_balance = account.balance in
        let nsf_error () =
          Or_error.errorf
            !"For timed account, the requested transaction for amount %{sexp: \
              Amount.t} at global slot %{sexp: Global_slot.t}, the balance \
              %{sexp: Balance.t} is insufficient"
            txn_amount txn_global_slot account_balance
        in
        let min_balance_error min_balance =
          Or_error.errorf
            !"For timed account, the requested transaction for amount %{sexp: \
              Amount.t} at global slot %{sexp: Global_slot.t}, applying the \
              transaction would put the balance below the calculated minimum \
              balance of %{sexp: Balance.t}"
            txn_amount txn_global_slot min_balance
        in
        match Balance.(account_balance - txn_amount) with
        | None ->
            (* checking for sufficient funds may be redundant with a check elsewhere
               regardless, the transaction would put the account below any calculated minimum balance
               so don't bother with the remaining computations
            *)
            nsf_error ()
        | Some proposed_new_balance ->
            let open Unsigned in
            let curr_min_balance =
              if Global_slot.(txn_global_slot < cliff_time) then
                initial_minimum_balance
              else
                (* take advantage of fact that global slots are uint32's *)
                let num_periods =
                  UInt32.(
                    Infix.((txn_global_slot - cliff_time) / vesting_period)
                    |> to_int64 |> UInt64.of_int64)
                in
                let min_balance_decrement =
                  UInt64.Infix.(
                    num_periods * Amount.to_uint64 vesting_increment)
                  |> Amount.of_uint64
                in
                match
                  Balance.(initial_minimum_balance - min_balance_decrement)
                with
                | None ->
                    Balance.zero
                | Some amt ->
                    amt
            in
            if Balance.(proposed_new_balance < curr_min_balance) then
              min_balance_error curr_min_balance
            else Or_error.return curr_min_balance
      in
      (* once the calculated minimum balance becomes zero, the account becomes untimed *)
      if Balance.(curr_min_balance > zero) then account.timing else Untimed

module Make (L : Ledger_intf) : S with type ledger := L.t = struct
  open L

  let error s = Or_error.errorf "Ledger.apply_transaction: %s" s

  let error_opt e = Option.value_map ~default:(error e) ~f:Or_error.return

  let get_with_location ledger account_id =
    match location_of_account ledger account_id with
    | Some location -> (
      match get ledger location with
      | Some account ->
          Ok (`Existing location, account)
      | None ->
          Or_error.errorf
            !"Account %{sexp: Account_id.t} has a location in the ledger, but \
              is not present"
            account_id )
    | None ->
        Ok (`New, Account.create account_id Balance.zero)

  let set_with_location ledger location account =
    match location with
    | `Existing location ->
        set ledger location account
    | `New ->
        ignore
        @@ get_or_create_account_exn ledger
             (Account.identifier account)
             account

  let get' ledger tag location =
    error_opt (sprintf "%s account not found" tag) (get ledger location)

  let location_of_account' ledger tag key =
    error_opt
      (sprintf "%s location not found" tag)
      (location_of_account ledger key)

  let add_amount balance amount =
    error_opt "overflow" (Balance.add_amount balance amount)

  let sub_amount balance amount =
    error_opt "insufficient funds" (Balance.sub_amount balance amount)

  let sub_account_creation_fee
      ~(constraint_constants : Genesis_constants.Constraint_constants.t) action
      amount =
    let fee = constraint_constants.account_creation_fee in
    if action = `Added then
      error_opt
        (sprintf
           !"Error subtracting account creation fee %{sexp: Currency.Fee.t}; \
             transaction amount %{sexp: Currency.Amount.t} insufficient"
           fee amount)
        Amount.(sub amount (of_fee fee))
    else Ok amount

  let check b =
    ksprintf (fun s -> if b then Ok () else Or_error.error_string s)

  let validate_nonces txn_nonce account_nonce =
    check
      (Account.Nonce.equal account_nonce txn_nonce)
      !"Nonce in account %{sexp: Account.Nonce.t} different from nonce in \
        transaction %{sexp: Account.Nonce.t}"
      account_nonce txn_nonce

  let validate_time ~valid_until ~current_global_slot =
    check
      Global_slot.(current_global_slot <= valid_until)
      !"Current global slot %{sexp: Global_slot.t} greater than transaction \
        expiry slot %{sexp: Global_slot.t}"
      current_global_slot valid_until

  module Undo = struct
    include Undo

    let transaction : t -> Transaction.t Or_error.t =
     fun {varying; _} ->
      match varying with
      | User_command tr ->
          Option.value_map ~default:(Or_error.error_string "Bad signature")
            (UC.check tr.common.user_command) ~f:(fun x ->
              Ok (Transaction.User_command x) )
      | Fee_transfer f ->
          Ok (Fee_transfer f.fee_transfer)
      | Coinbase c ->
          Ok (Coinbase c.coinbase)
  end

  let previous_empty_accounts action pk = if action = `Added then [pk] else []

  (* someday: It would probably be better if we didn't modify the receipt chain hash
  in the case that the sender is equal to the receiver, but it complicates the SNARK, so
  we don't for now. *)
  let apply_user_command_unchecked
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ~txn_global_slot ledger
      ({payload; signer; signature= _} as user_command : User_command.t) =
    let open Or_error.Let_syntax in
    let signer_pk = Public_key.compress signer in
    let current_global_slot = txn_global_slot in
    let%bind () =
      validate_time
        ~valid_until:(User_command.valid_until user_command)
        ~current_global_slot
    in
    (* Fee-payer information *)
    let fee_token = User_command.fee_token user_command in
    let fee_payer = User_command.fee_payer user_command in
    let nonce = User_command.nonce user_command in
    let%bind () =
      (* TODO: Enable multi-sig. *)
      if
        Public_key.Compressed.equal (Account_id.public_key fee_payer) signer_pk
      then return ()
      else
        Or_error.errorf
          "Cannot pay fees from a public key that did not sign the transaction"
    in
    let%bind () =
      (* TODO: Remove this check and update the transaction snark once we have
         an exchange rate mechanism. See issue #4447.
      *)
      if Token_id.equal fee_token Token_id.default then return ()
      else
        Or_error.errorf
          "Cannot create transactions with fee_token different from the default"
    in
    let%bind fee_payer_location, fee_payer_account, undo_common =
      let%bind location, account = get_with_location ledger fee_payer in
      let%bind () =
        match location with
        | `Existing _ ->
            return ()
        | `New ->
            Or_error.errorf "The fee-payer account does not exist"
      in
      let fee = Amount.of_fee (User_command.fee user_command) in
      let%bind balance = sub_amount account.balance fee in
      let%bind () = validate_nonces nonce account.nonce in
      let fee_payer_timing = account.timing in
      let%map timing =
        validate_timing ~txn_amount:fee ~txn_global_slot:current_global_slot
          ~account
      in
      let undo_common : Undo.User_command_undo.Common.t =
        { user_command
        ; previous_receipt_chain_hash= account.receipt_chain_hash
        ; fee_payer_timing
        ; source_timing= None }
      in
      ( location
      , { account with
          balance
        ; nonce= Account.Nonce.succ account.nonce
        ; receipt_chain_hash=
            Receipt.Chain_hash.cons payload account.receipt_chain_hash
        ; timing }
      , undo_common )
    in
    (* Charge the fee. This must happen, whether or not the command itself
       succeeds, to ensure that the network is compensated for processing this
       command.
    *)
    set_with_location ledger fee_payer_location fee_payer_account ;
    let next_available_token = next_available_token ledger in
    let source = User_command.source ~next_available_token user_command in
    let receiver = User_command.receiver ~next_available_token user_command in
    let exception Reject of Error.t in
    let charge_account_creation_fee_exn (account : Account.t) =
      let balance =
        Option.value_exn
          (Balance.sub_amount account.balance
             (Amount.of_fee constraint_constants.account_creation_fee))
      in
      let account = {account with balance} in
      let timing =
        Or_error.ok_exn
          (validate_timing ~txn_amount:Amount.zero
             ~txn_global_slot:current_global_slot ~account)
      in
      {account with timing}
    in
    let compute_updates () =
      (* Compute the necessary changes to apply the command, failing if any of
         the conditions are not met.
      *)
      let%bind predicate_passed =
        if
          Public_key.Compressed.equal
            (User_command.fee_payer_pk user_command)
            (User_command.source_pk user_command)
        then return true
        else
          match payload.body with
          | Create_new_token _ ->
              (* Any account is allowed to create a new token associated with a
                 public key.
              *)
              return true
          | Create_token_account _ ->
              (* Predicate failure is deferred here. It will be checked later. *)
              let predicate_result =
                (* TODO(#4554): Hook predicate evaluation in here once
                   implemented.
                *)
                false
              in
              return predicate_result
          | Payment _
          | Stake_delegation _
          | Mint_tokens _
          | Set_token_permissions _
          | Set_account_permissions _ ->
              (* TODO(#4554): Hook predicate evaluation in here once implemented. *)
              Or_error.errorf
                "The fee-payer is not authorised to issue commands for the \
                 source account"
      in
      let not_disabled (account : Account.t) =
        match account.token_permissions with
        | Token_owned _ ->
            return ()
        | Not_owned {account_disabled} ->
            if account_disabled then
              Or_error.errorf "The source account is disabled"
            else return ()
      in
      match payload.body with
      | Stake_delegation _ ->
          let%bind receiver_location, _receiver_account =
            (* Check that receiver account exists. *)
            get_with_location ledger receiver
          in
          let%bind source_location, source_account =
            get_with_location ledger source
          in
          let%bind () =
            match (source_location, receiver_location) with
            | `Existing _, `Existing _ ->
                return ()
            | `New, _ ->
                Or_error.errorf "The delegating account does not exist"
            | _, `New ->
                Or_error.errorf "The delegated-to account does not exist"
          in
          let previous_delegate = source_account.delegate in
          let source_timing = source_account.timing in
          (* Timing is always valid, but we need to record any switch from
             timed to untimed here to stay in sync with the snark.
          *)
          let%map timing =
            validate_timing ~txn_amount:Amount.zero
              ~txn_global_slot:current_global_slot ~account:source_account
          in
          let source_account =
            { source_account with
              delegate= Account_id.public_key receiver
            ; timing }
          in
          ( [(source_location, source_account)]
          , `Source_timing source_timing
          , Undo.User_command_undo.Body.Stake_delegation {previous_delegate} )
      | Payment {amount; token_id= token; _} ->
          let%bind receiver_location, receiver_account =
            get_with_location ledger receiver
          in
          (* Charge the account creation fee. *)
          let%bind receiver_amount =
            match receiver_location with
            | `Existing _ ->
                return amount
            | `New ->
                if Token_id.(equal default) token then
                  (* Subtract the creation fee from the transaction amount. *)
                  sub_account_creation_fee ~constraint_constants `Added amount
                else
                  Or_error.errorf
                    !"The receiving account does not exist, and the \
                      transferred token %{sexp: Token_id.t} is not the default"
                    token
          in
          let%bind receiver_account =
            let%map balance =
              add_amount receiver_account.balance receiver_amount
            in
            {receiver_account with balance}
          in
          let%bind source_location, source_timing, source_account =
            let ret =
              let%bind location, account =
                if Account_id.equal source receiver then
                  match receiver_location with
                  | `Existing _ ->
                      return (receiver_location, receiver_account)
                  | `New ->
                      Or_error.errorf "The source account does not exist"
                else get_with_location ledger source
              in
              let%bind () =
                match location with
                | `Existing _ ->
                    return ()
                | `New ->
                    Or_error.errorf "The source account does not exist"
              in
              let source_timing = account.timing in
              let%bind timing =
                validate_timing ~txn_amount:amount
                  ~txn_global_slot:current_global_slot ~account
              in
              let%map balance = sub_amount account.balance amount in
              (location, source_timing, {account with timing; balance})
            in
            if Account_id.equal fee_payer source then
              (* Don't process transactions with insufficient balance from the
                 fee-payer.
              *)
              match ret with Ok _ -> ret | Error err -> raise (Reject err)
            else ret
          in
          let%map () = not_disabled source_account in
          let previous_empty_accounts =
            match receiver_location with
            | `Existing _ ->
                []
            | `New ->
                [receiver]
          in
          ( [ (receiver_location, receiver_account)
            ; (source_location, source_account) ]
          , `Source_timing source_timing
          , Undo.User_command_undo.Body.Payment {previous_empty_accounts} )
      | Create_new_token {disable_new_accounts; _} ->
          (* NOTE: source and receiver are definitionally equal here. *)
          let fee_payer_account =
            try charge_account_creation_fee_exn fee_payer_account
            with exn -> raise (Reject (Error.of_exn exn))
          in
          let%map receiver_location, receiver_account =
            get_with_location ledger receiver
          in
          if not (receiver_location = `New) then
            failwith
              "Token owner account for newly created token already exists?!?!" ;
          let receiver_account =
            { receiver_account with
              token_permissions=
                Token_permissions.Token_owned {disable_new_accounts} }
          in
          ( [ (fee_payer_location, fee_payer_account)
            ; (receiver_location, receiver_account) ]
          , `Source_timing receiver_account.timing
          , Undo.User_command_undo.Body.Create_new_token
              {created_token= next_available_token} )
      | Create_token_account {account_disabled; _} ->
          if
            account_disabled
            && Token_id.(equal default) (Account_id.token_id receiver)
          then
            raise
              (Reject
                 (Error.createf
                    "Cannot open a disabled account in the default token")) ;
          let fee_payer_account =
            try charge_account_creation_fee_exn fee_payer_account
            with exn -> raise (Reject (Error.of_exn exn))
          in
          let%bind receiver_location, receiver_account =
            get_with_location ledger receiver
          in
          let%bind () =
            match receiver_location with
            | `New ->
                return ()
            | `Existing _ ->
                Or_error.errorf
                  "Attempted to create an account that already exists"
          in
          let receiver_account =
            { receiver_account with
              token_permissions= Token_permissions.Not_owned {account_disabled}
            }
          in
          let%bind source_location, source_account =
            get_with_location ledger source
          in
          let%bind source_account =
            if Account_id.equal source receiver then return receiver_account
            else if Account_id.equal source fee_payer then
              return fee_payer_account
            else
              match source_location with
              | `New ->
                  Or_error.errorf "Token owner account does not exist"
              | `Existing _ ->
                  return source_account
          in
          let%bind () =
            match source_account.token_permissions with
            | Token_owned {disable_new_accounts} ->
                if
                  not
                    ( Bool.equal account_disabled disable_new_accounts
                    || predicate_passed )
                then
                  Or_error.errorf
                    "The fee-payer is not authorised to create token accounts \
                     for this token"
                else return ()
            | Not_owned _ ->
                if Token_id.(equal default) (Account_id.token_id receiver) then
                  return ()
                else
                  Or_error.errorf "Token owner account does not own the token"
          in
          let source_timing = source_account.timing in
          let%map source_account =
            let%map timing =
              validate_timing ~txn_amount:Amount.zero
                ~txn_global_slot:current_global_slot ~account:source_account
            in
            {source_account with timing}
          in
          let located_accounts =
            if Account_id.equal source receiver then
              (* For token_id= default, we allow this *)
              [ (fee_payer_location, fee_payer_account)
              ; (source_location, source_account) ]
            else
              [ (receiver_location, receiver_account)
              ; (fee_payer_location, fee_payer_account)
              ; (source_location, source_account) ]
          in
          ( located_accounts
          , `Source_timing source_timing
          , Undo.User_command_undo.Body.Create_token_account )
      | Mint_tokens {token_id= token; amount; _} ->
          let%bind () =
            if Token_id.(equal default) token then
              Or_error.errorf "Cannot mint more of the default token"
            else return ()
          in
          let%bind receiver_location, receiver_account =
            get_with_location ledger receiver
          in
          let%bind () =
            match receiver_location with
            | `Existing _ ->
                return ()
            | `New ->
                Or_error.errorf "The receiving account does not exist"
          in
          let%bind receiver_account =
            let%map balance = add_amount receiver_account.balance amount in
            {receiver_account with balance}
          in
          let%map source_location, source_timing, source_account =
            let%bind location, account =
              if Account_id.equal source receiver then
                return (receiver_location, receiver_account)
              else get_with_location ledger source
            in
            let%bind () =
              match location with
              | `Existing _ ->
                  return ()
              | `New ->
                  Or_error.errorf "The token owner account does not exist"
            in
            let%bind () =
              match account.token_permissions with
              | Token_owned _ ->
                  return ()
              | Not_owned _ ->
                  Or_error.errorf
                    !"The claimed token owner %{sexp: Account_id.t} does not \
                      own the token %{sexp: Token_id.t}"
                    source token
            in
            let source_timing = account.timing in
            let%map timing =
              validate_timing ~txn_amount:Amount.zero
                ~txn_global_slot:current_global_slot ~account
            in
            (location, source_timing, {account with timing})
          in
          ( [ (receiver_location, receiver_account)
            ; (source_location, source_account) ]
          , `Source_timing source_timing
          , Undo.User_command_undo.Body.Mint_tokens )
      | Set_token_permissions
          {token_id= token; disable_new_accounts= token_locked; _}
      | Set_account_permissions {token_id= token; disabled= token_locked; _} ->
          (* These commands do exactly the same thing under the hood, so we
             share the logic for them. We are permissive about using the
             [Set_account_permissions] command on the token owner account,
             because the commands alias in the snark and so we can't identify a
             failure there.
          *)
          let%bind () =
            if Token_id.(equal default) token then
              Or_error.errorf "Cannot set permissions for the default token"
            else return ()
          in
          let%bind receiver_location, receiver_account =
            get_with_location ledger receiver
          in
          let%bind () =
            match receiver_location with
            | `Existing _ ->
                return ()
            | `New ->
                Or_error.errorf "The receiving account does not exist"
          in
          let previous_token_locked, receiver_account =
            let previous_token_locked, token_permissions =
              match receiver_account.token_permissions with
              | Token_owned {disable_new_accounts} ->
                  ( disable_new_accounts
                  , Token_permissions.Token_owned
                      {disable_new_accounts= token_locked} )
              | Not_owned {account_disabled} ->
                  (account_disabled, Not_owned {account_disabled= token_locked})
            in
            (previous_token_locked, {receiver_account with token_permissions})
          in
          let%map source_location, source_timing, source_account =
            let%bind location, account =
              if Account_id.equal source receiver then
                return (receiver_location, receiver_account)
              else get_with_location ledger source
            in
            let%bind () =
              match location with
              | `Existing _ ->
                  return ()
              | `New ->
                  Or_error.errorf "The token owner account does not exist"
            in
            let%bind () =
              match account.token_permissions with
              | Token_owned _ ->
                  return ()
              | Not_owned _ ->
                  Or_error.errorf
                    !"The claimed token owner %{sexp: Account_id.t} does not \
                      own the token %{sexp: Token_id.t}"
                    source token
            in
            let source_timing = account.timing in
            let%map timing =
              validate_timing ~txn_amount:Amount.zero
                ~txn_global_slot:current_global_slot ~account
            in
            (location, source_timing, {account with timing})
          in
          ( [ (receiver_location, receiver_account)
            ; (source_location, source_account) ]
          , `Source_timing source_timing
          , Undo.User_command_undo.Body.Set_token_permissions
              {previous_token_locked} )
    in
    match compute_updates () with
    | Ok (located_accounts, `Source_timing source_timing, undo_body) ->
        (* Update the ledger. *)
        List.iter located_accounts ~f:(fun (location, account) ->
            set_with_location ledger location account ) ;
        let undo_common =
          {undo_common with source_timing= Some source_timing}
        in
        return
          ({common= undo_common; body= undo_body} : Undo.User_command_undo.t)
    | Error _ ->
        (* Do not update the ledger. *)
        return ({common= undo_common; body= Failed} : Undo.User_command_undo.t)
    | exception Reject err ->
        (* TODO: These transactions should never reach this stage, this error
           should be fatal.
        *)
        Error err

  let apply_user_command ~constraint_constants ~txn_global_slot ledger
      (user_command : User_command.With_valid_signature.t) =
    apply_user_command_unchecked ~constraint_constants ~txn_global_slot ledger
      (User_command.forget_check user_command)

  let process_fee_transfer t (transfer : Fee_transfer.t) ~modify_balance =
    let open Or_error.Let_syntax in
    (* TODO(#4555): Allow token_id to vary from default. *)
    let%bind () =
      if
        List.for_all
          ~f:Token_id.(equal default)
          (One_or_two.to_list (Fee_transfer.fee_tokens transfer))
      then return ()
      else Or_error.errorf "Cannot pay fees in non-default tokens."
    in
    match Fee_transfer.to_singles transfer with
    | `One ft ->
        let account_id = Fee_transfer.Single.receiver ft in
        (* TODO(#4496): Do not use get_or_create here; we should not create a
           new account before we know that the transaction will go through and
           thus the creation fee has been paid.
        *)
        let action, a, loc = get_or_create t account_id in
        let emptys = previous_empty_accounts action account_id in
        let%map balance = modify_balance action account_id a.balance ft.fee in
        set t loc {a with balance} ;
        emptys
    | `Two (ft1, ft2) ->
        let account_id1 = Fee_transfer.Single.receiver ft1 in
        (* TODO(#4496): Do not use get_or_create here; we should not create a
           new account before we know that the transaction will go through and
           thus the creation fee has been paid.
        *)
        let action1, a1, l1 = get_or_create t account_id1 in
        let emptys1 = previous_empty_accounts action1 account_id1 in
        let account_id2 = Fee_transfer.Single.receiver ft2 in
        if Account_id.equal account_id1 account_id2 then (
          let%bind fee = error_opt "overflow" (Fee.add ft1.fee ft2.fee) in
          let%map balance =
            modify_balance action1 account_id1 a1.balance fee
          in
          set t l1 {a1 with balance} ;
          emptys1 )
        else
          (* TODO(#4496): Do not use get_or_create here; we should not create a
             new account before we know that the transaction will go through
             and thus the creation fee has been paid.
          *)
          let action2, a2, l2 = get_or_create t account_id2 in
          let emptys2 = previous_empty_accounts action2 account_id2 in
          let%bind balance1 =
            modify_balance action1 account_id1 a1.balance ft1.fee
          in
          let%map balance2 =
            modify_balance action2 account_id2 a2.balance ft2.fee
          in
          set t l1 {a1 with balance= balance1} ;
          set t l2 {a2 with balance= balance2} ;
          emptys1 @ emptys2

  let apply_fee_transfer ~constraint_constants t transfer =
    let open Or_error.Let_syntax in
    let%map previous_empty_accounts =
      process_fee_transfer t transfer ~modify_balance:(fun action _ b f ->
          let%bind amount =
            let amount = Amount.of_fee f in
            sub_account_creation_fee ~constraint_constants action amount
          in
          add_amount b amount )
    in
    Undo.Fee_transfer_undo.{fee_transfer= transfer; previous_empty_accounts}

  let undo_fee_transfer ~constraint_constants t
      ({previous_empty_accounts; fee_transfer} : Undo.Fee_transfer_undo.t) =
    let open Or_error.Let_syntax in
    let%map _ =
      process_fee_transfer t fee_transfer ~modify_balance:(fun _ aid b f ->
          let action =
            if List.mem ~equal:Account_id.equal previous_empty_accounts aid
            then `Added
            else `Existed
          in
          let%bind amount =
            sub_account_creation_fee ~constraint_constants action
              (Amount.of_fee f)
          in
          sub_amount b amount )
    in
    remove_accounts_exn t previous_empty_accounts

  let apply_coinbase ~constraint_constants t
      (* TODO: Better system needed for making atomic changes. Could use a monad. *)
      ({receiver; fee_transfer; amount= coinbase_amount} as cb : Coinbase.t) =
    let open Or_error.Let_syntax in
    let%bind receiver_reward, emptys1, transferee_update =
      match fee_transfer with
      | None ->
          return (coinbase_amount, [], None)
      | Some ({receiver_pk= transferee; fee} as ft) ->
          assert (not @@ Public_key.Compressed.equal transferee receiver) ;
          let transferee_id = Coinbase.Fee_transfer.receiver ft in
          let fee = Amount.of_fee fee in
          let%bind receiver_reward =
            error_opt "Coinbase fee transfer too large"
              (Amount.sub coinbase_amount fee)
          in
          let action, transferee_account, transferee_location =
            (* TODO(#4496): Do not use get_or_create here; we should not create
               a new account before we know that the transaction will go
               through and thus the creation fee has been paid.
            *)
            get_or_create t transferee_id
          in
          let emptys = previous_empty_accounts action transferee_id in
          let%map balance =
            let%bind amount =
              sub_account_creation_fee ~constraint_constants action fee
            in
            add_amount transferee_account.balance amount
          in
          ( receiver_reward
          , emptys
          , Some (transferee_location, {transferee_account with balance}) )
    in
    let receiver_id = Account_id.create receiver Token_id.default in
    let action2, receiver_account, receiver_location =
      (* TODO(#4496): Do not use get_or_create here; we should not create a new
         account before we know that the transaction will go through and thus
         the creation fee has been paid.
      *)
      get_or_create t receiver_id
    in
    let emptys2 = previous_empty_accounts action2 receiver_id in
    let%map receiver_balance =
      let%bind amount =
        sub_account_creation_fee ~constraint_constants action2 receiver_reward
      in
      add_amount receiver_account.balance amount
    in
    set t receiver_location {receiver_account with balance= receiver_balance} ;
    Option.iter transferee_update ~f:(fun (l, a) -> set t l a) ;
    Undo.Coinbase_undo.
      {coinbase= cb; previous_empty_accounts= emptys1 @ emptys2}

  (* Don't have to be atomic here because these should never fail. In fact, none of
  the undo functions should ever return an error. This should be fixed in the types. *)
  let undo_coinbase ~constraint_constants t
      Undo.Coinbase_undo.
        { coinbase= {receiver; fee_transfer; amount= coinbase_amount}
        ; previous_empty_accounts } =
    let receiver_reward =
      match fee_transfer with
      | None ->
          coinbase_amount
      | Some ({receiver_pk= _; fee} as ft) ->
          let fee = Amount.of_fee fee in
          let transferee_id = Coinbase.Fee_transfer.receiver ft in
          let transferee_location =
            Or_error.ok_exn (location_of_account' t "transferee" transferee_id)
          in
          let transferee_account =
            Or_error.ok_exn (get' t "transferee" transferee_location)
          in
          let transferee_balance =
            let action =
              if
                List.mem previous_empty_accounts transferee_id
                  ~equal:Account_id.equal
              then `Added
              else `Existed
            in
            let amount =
              sub_account_creation_fee ~constraint_constants action fee
              |> Or_error.ok_exn
            in
            Option.value_exn
              (Balance.sub_amount transferee_account.balance amount)
          in
          set t transferee_location
            {transferee_account with balance= transferee_balance} ;
          Option.value_exn (Amount.sub coinbase_amount fee)
    in
    let receiver_id = Account_id.create receiver Token_id.default in
    let receiver_location =
      Or_error.ok_exn (location_of_account' t "receiver" receiver_id)
    in
    let receiver_account =
      Or_error.ok_exn (get' t "receiver" receiver_location)
    in
    let receiver_balance =
      let action =
        if List.mem previous_empty_accounts receiver_id ~equal:Account_id.equal
        then `Added
        else `Existed
      in
      let amount =
        sub_account_creation_fee ~constraint_constants action receiver_reward
        |> Or_error.ok_exn
      in
      Option.value_exn (Balance.sub_amount receiver_account.balance amount)
    in
    set t receiver_location {receiver_account with balance= receiver_balance} ;
    remove_accounts_exn t previous_empty_accounts

  let undo_user_command
      ~(constraint_constants : Genesis_constants.Constraint_constants.t) ledger
      { Undo.User_command_undo.common=
          { user_command= {payload; signer= _; signature= _} as user_command
          ; previous_receipt_chain_hash
          ; fee_payer_timing
          ; source_timing }
      ; body } =
    let open Or_error.Let_syntax in
    (* Fee-payer information *)
    let fee_payer = User_command.fee_payer user_command in
    let nonce = User_command.nonce user_command in
    let%bind fee_payer_location =
      location_of_account' ledger "fee payer" fee_payer
    in
    (* Refund the fee to the fee-payer. *)
    let%bind fee_payer_account =
      let%bind account = get' ledger "fee payer" fee_payer_location in
      let%bind () = validate_nonces (Account.Nonce.succ nonce) account.nonce in
      let%map balance =
        add_amount account.balance
          (Amount.of_fee (User_command.fee user_command))
      in
      { account with
        balance
      ; nonce
      ; receipt_chain_hash= previous_receipt_chain_hash
      ; timing= fee_payer_timing }
    in
    (* Update the fee-payer's account. *)
    set ledger fee_payer_location fee_payer_account ;
    let next_available_token =
      match body with
      | Create_new_token {created_token} ->
          created_token
      | _ ->
          next_available_token ledger
    in
    let source = User_command.source ~next_available_token user_command in
    let source_timing =
      (* Prefer fee-payer original timing when applicable, since it is the
         'true' original.
      *)
      if Account_id.equal fee_payer source then Some fee_payer_timing
      else source_timing
    in
    (* Reverse any other effects that the user command had. *)
    match (User_command.Payload.body payload, body) with
    | _, Failed ->
        (* The user command failed, only the fee was charged. *)
        return ()
    | Stake_delegation (Set_delegate _), Stake_delegation {previous_delegate}
      ->
        let%bind source_location =
          location_of_account' ledger "source" source
        in
        let%map source_account = get' ledger "source" source_location in
        set ledger source_location
          { source_account with
            delegate= previous_delegate
          ; timing= Option.value ~default:source_account.timing source_timing
          }
    | Payment {amount; _}, Payment {previous_empty_accounts} ->
        let receiver =
          User_command.receiver ~next_available_token user_command
        in
        let%bind receiver_location, receiver_account =
          let%bind location =
            location_of_account' ledger "receiver" receiver
          in
          let%map account = get' ledger "receiver" location in
          let balance =
            (* NOTE: [sub_amount] is only [None] if the account creation fee
               was charged, in which case this account will be deleted by
               [remove_accounts_exn] below anyway.
            *)
            Option.value ~default:Balance.zero
              (Balance.sub_amount account.balance amount)
          in
          (location, {account with balance})
        in
        let%map source_location, source_account =
          let%bind location, account =
            if Account_id.equal source receiver then
              return (receiver_location, receiver_account)
            else
              let%bind location =
                location_of_account' ledger "source" source
              in
              let%map account = get' ledger "source" location in
              (location, account)
          in
          let%map balance = add_amount account.balance amount in
          ( location
          , { account with
              balance
            ; timing= Option.value ~default:account.timing source_timing } )
        in
        set ledger receiver_location receiver_account ;
        set ledger source_location source_account ;
        remove_accounts_exn ledger previous_empty_accounts
    | Create_new_token _, Create_new_token _
    | Create_token_account _, Create_token_account ->
        (* We group these commands together because their undo behaviour is
           identical: remove the created account, and un-charge the fee payer
           for creating the account. *)
        let fee_payer_account =
          let balance =
            Option.value_exn
              (Balance.add_amount fee_payer_account.balance
                 (Amount.of_fee constraint_constants.account_creation_fee))
          in
          {fee_payer_account with balance}
        in
        let%bind source_location =
          location_of_account' ledger "source" source
        in
        let%map source_account =
          if Account_id.equal fee_payer source then return fee_payer_account
          else get' ledger "source" source_location
        in
        let receiver =
          User_command.receiver ~next_available_token user_command
        in
        set ledger fee_payer_location fee_payer_account ;
        set ledger source_location
          { source_account with
            timing= Option.value ~default:source_account.timing source_timing
          } ;
        remove_accounts_exn ledger [receiver] ;
        (* Restore to the previous [next_available_token]. This is a no-op if
           the [next_available_token] did not change.
        *)
        set_next_available_token ledger next_available_token
    | Mint_tokens {amount; _}, Mint_tokens ->
        let receiver =
          User_command.receiver ~next_available_token user_command
        in
        let%bind receiver_location, receiver_account =
          let%bind location =
            location_of_account' ledger "receiver" receiver
          in
          let%map account = get' ledger "receiver" location in
          let balance =
            Option.value_exn (Balance.sub_amount account.balance amount)
          in
          (location, {account with balance})
        in
        let%map source_location, source_account =
          let%map location, account =
            if Account_id.equal source receiver then
              return (receiver_location, receiver_account)
            else
              let%bind location =
                location_of_account' ledger "source" source
              in
              let%map account = get' ledger "source" location in
              (location, account)
          in
          ( location
          , { account with
              timing= Option.value ~default:account.timing source_timing } )
        in
        set ledger receiver_location receiver_account ;
        set ledger source_location source_account
    | ( (Set_token_permissions _ | Set_account_permissions _)
      , Set_token_permissions {previous_token_locked} ) ->
        let receiver =
          User_command.receiver ~next_available_token user_command
        in
        let%bind receiver_location, receiver_account =
          let%bind location =
            location_of_account' ledger "receiver" receiver
          in
          let%map account = get' ledger "receiver" location in
          let account =
            match account.token_permissions with
            | Token_owned _ ->
                { account with
                  token_permissions=
                    Token_permissions.Token_owned
                      {disable_new_accounts= previous_token_locked} }
            | Not_owned _ ->
                { account with
                  token_permissions=
                    Not_owned {account_disabled= previous_token_locked} }
          in
          (location, account)
        in
        let%map source_location, source_account =
          let%map location, account =
            if Account_id.equal source receiver then
              return (receiver_location, receiver_account)
            else
              let%bind location =
                location_of_account' ledger "source" source
              in
              let%map account = get' ledger "source" location in
              (location, account)
          in
          ( location
          , { account with
              timing= Option.value ~default:account.timing source_timing } )
        in
        set ledger receiver_location receiver_account ;
        set ledger source_location source_account
    | _, _ ->
        failwith "Undo/command mismatch"

  let undo :
         constraint_constants:Genesis_constants.Constraint_constants.t
      -> t
      -> Undo.t
      -> unit Or_error.t =
   fun ~constraint_constants ledger undo ->
    let open Or_error.Let_syntax in
    let%map res =
      match undo.varying with
      | Fee_transfer u ->
          undo_fee_transfer ~constraint_constants ledger u
      | User_command u ->
          undo_user_command ~constraint_constants ledger u
      | Coinbase c ->
          undo_coinbase ~constraint_constants ledger c ;
          Ok ()
    in
    Debug_assert.debug_assert (fun () ->
        [%test_eq: Ledger_hash.t] undo.previous_hash (merkle_root ledger) ) ;
    res

  let apply_transaction ~constraint_constants ~txn_global_slot ledger
      (t : Transaction.t) =
    O1trace.measure "apply_transaction" (fun () ->
        let previous_hash = merkle_root ledger in
        Or_error.map
          ( match t with
          | User_command txn ->
              Or_error.map
                (apply_user_command ~constraint_constants ~txn_global_slot
                   ledger txn) ~f:(fun undo -> Undo.Varying.User_command undo)
          | Fee_transfer t ->
              Or_error.map (apply_fee_transfer ~constraint_constants ledger t)
                ~f:(fun undo -> Undo.Varying.Fee_transfer undo)
          | Coinbase t ->
              Or_error.map (apply_coinbase ~constraint_constants ledger t)
                ~f:(fun undo -> Undo.Varying.Coinbase undo) )
          ~f:(fun varying -> {Undo.previous_hash; varying}) )

  let merkle_root_after_user_command_exn ~constraint_constants ~txn_global_slot
      ledger payment =
    let undo =
      Or_error.ok_exn
        (apply_user_command ~constraint_constants ~txn_global_slot ledger
           payment)
    in
    let root = merkle_root ledger in
    let next_available_token = next_available_token ledger in
    Or_error.ok_exn (undo_user_command ~constraint_constants ledger undo) ;
    (root, `Next_available_token next_available_token)

  module For_tests = struct
    let validate_timing = validate_timing
  end
end
