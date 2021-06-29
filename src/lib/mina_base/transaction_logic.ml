open Core
open Currency
open Signature_lib
module Global_slot = Mina_numbers.Global_slot

module type Ledger_intf = sig
  type t

  type location

  val get : t -> location -> Account.t option

  val location_of_account : t -> Account_id.t -> location option

  val set : t -> location -> Account.t -> unit

  val get_or_create :
       t
    -> Account_id.t
    -> ([`Added | `Existed] * Account.t * location) Or_error.t

  val get_or_create_account :
       t
    -> Account_id.t
    -> Account.t
    -> ([`Added | `Existed] * location) Or_error.t

  val remove_accounts_exn : t -> Account_id.t list -> unit

  val merkle_root : t -> Ledger_hash.t

  val with_ledger : depth:int -> f:(t -> 'a) -> 'a

  val next_available_token : t -> Token_id.t

  val set_next_available_token : t -> Token_id.t -> unit
end

module Transaction_applied = struct
  module UC = Signed_command

  module Signed_command_applied = struct
    module Common = struct
      [%%versioned
      module Stable = struct
        module V1 = struct
          type t =
            { user_command: Signed_command.Stable.V1.t With_status.Stable.V1.t
            ; previous_receipt_chain_hash: Receipt.Chain_hash.Stable.V1.t
            ; fee_payer_timing: Account.Timing.Stable.V1.t
            ; source_timing: Account.Timing.Stable.V1.t option }
          [@@deriving sexp]

          let to_latest = Fn.id
        end
      end]
    end

    module Body = struct
      [%%versioned
      module Stable = struct
        module V1 = struct
          type t =
            | Payment of {previous_empty_accounts: Account_id.Stable.V1.t list}
            | Stake_delegation of
                { previous_delegate: Public_key.Compressed.Stable.V1.t option
                }
            | Create_new_token of {created_token: Token_id.Stable.V1.t}
            | Create_token_account
            | Mint_tokens
            | Failed
          [@@deriving sexp]

          let to_latest = Fn.id
        end
      end]
    end

    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = {common: Common.Stable.V1.t; body: Body.Stable.V1.t}
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
  end

  module Snapp_command_applied = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          { accounts: (Account_id.Stable.V1.t * Account.Stable.V1.t option) list
          ; command: Snapp_command.Stable.V1.t With_status.Stable.V1.t }
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
  end

  module Command_applied = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          | Signed_command of Signed_command_applied.Stable.V1.t
          | Snapp_command of Snapp_command_applied.Stable.V1.t
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
  end

  module Fee_transfer_applied = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          { fee_transfer: Fee_transfer.Stable.V1.t
          ; previous_empty_accounts: Account_id.Stable.V1.t list
          ; receiver_timing: Account.Timing.Stable.V1.t
          ; balances: Transaction_status.Fee_transfer_balance_data.Stable.V1.t
          }
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
  end

  module Coinbase_applied = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          { coinbase: Coinbase.Stable.V1.t
          ; previous_empty_accounts: Account_id.Stable.V1.t list
          ; receiver_timing: Account.Timing.Stable.V1.t
          ; balances: Transaction_status.Coinbase_balance_data.Stable.V1.t }
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
  end

  module Varying = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          | Command of Command_applied.Stable.V1.t
          | Fee_transfer of Fee_transfer_applied.Stable.V1.t
          | Coinbase of Coinbase_applied.Stable.V1.t
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
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
end

module type S = sig
  type ledger

  module Transaction_applied : sig
    module Signed_command_applied : sig
      module Common : sig
        type t = Transaction_applied.Signed_command_applied.Common.t =
          { user_command: Signed_command.t With_status.t
          ; previous_receipt_chain_hash: Receipt.Chain_hash.t
          ; fee_payer_timing: Account.Timing.t
          ; source_timing: Account.Timing.t option }
        [@@deriving sexp]
      end

      module Body : sig
        type t = Transaction_applied.Signed_command_applied.Body.t =
          | Payment of {previous_empty_accounts: Account_id.t list}
          | Stake_delegation of
              { previous_delegate: Public_key.Compressed.t option }
          | Create_new_token of {created_token: Token_id.t}
          | Create_token_account
          | Mint_tokens
          | Failed
        [@@deriving sexp]
      end

      type t = Transaction_applied.Signed_command_applied.t =
        {common: Common.t; body: Body.t}
      [@@deriving sexp]
    end

    module Snapp_command_applied : sig
      type t = Transaction_applied.Snapp_command_applied.t =
        { accounts: (Account_id.t * Account.t option) list
        ; command: Snapp_command.t With_status.t }
      [@@deriving sexp]
    end

    module Command_applied : sig
      type t = Transaction_applied.Command_applied.t =
        | Signed_command of Signed_command_applied.t
        | Snapp_command of Snapp_command_applied.t
      [@@deriving sexp]
    end

    module Fee_transfer_applied : sig
      type t = Transaction_applied.Fee_transfer_applied.t =
        { fee_transfer: Fee_transfer.t
        ; previous_empty_accounts: Account_id.t list
        ; receiver_timing: Account.Timing.t
        ; balances: Transaction_status.Fee_transfer_balance_data.t }
      [@@deriving sexp]
    end

    module Coinbase_applied : sig
      type t = Transaction_applied.Coinbase_applied.t =
        { coinbase: Coinbase.t
        ; previous_empty_accounts: Account_id.t list
        ; receiver_timing: Account.Timing.t
        ; balances: Transaction_status.Coinbase_balance_data.t }
      [@@deriving sexp]
    end

    module Varying : sig
      type t = Transaction_applied.Varying.t =
        | Command of Command_applied.t
        | Fee_transfer of Fee_transfer_applied.t
        | Coinbase of Coinbase_applied.t
      [@@deriving sexp]
    end

    type t = Transaction_applied.t =
      {previous_hash: Ledger_hash.t; varying: Varying.t}
    [@@deriving sexp]

    val transaction : t -> Transaction.t With_status.t

    val user_command_status : t -> Transaction_status.t
  end

  val apply_user_command :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_global_slot:Global_slot.t
    -> ledger
    -> Signed_command.With_valid_signature.t
    -> Transaction_applied.Signed_command_applied.t Or_error.t

  val apply_fee_transfer :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_global_slot:Global_slot.t
    -> ledger
    -> Fee_transfer.t
    -> Transaction_applied.Fee_transfer_applied.t Or_error.t

  val apply_coinbase :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_global_slot:Global_slot.t
    -> ledger
    -> Coinbase.t
    -> Transaction_applied.Coinbase_applied.t Or_error.t

  val apply_transaction :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_state_view:Snapp_predicate.Protocol_state.View.t
    -> ledger
    -> Transaction.t
    -> Transaction_applied.t Or_error.t

  val merkle_root_after_snapp_command_exn :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_state_view:Snapp_predicate.Protocol_state.View.t
    -> ledger
    -> Snapp_command.Valid.t
    -> Ledger_hash.t * [`Next_available_token of Token_id.t]

  val merkle_root_after_user_command_exn :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> txn_global_slot:Global_slot.t
    -> ledger
    -> Signed_command.With_valid_signature.t
    -> Ledger_hash.t * [`Next_available_token of Token_id.t]

  val undo :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> ledger
    -> Transaction_applied.t
    -> unit Or_error.t

  val has_locked_tokens :
       global_slot:Global_slot.t
    -> account_id:Account_id.t
    -> ledger
    -> bool Or_error.t

  module For_tests : sig
    val validate_timing_with_min_balance :
         account:Account.t
      -> txn_amount:Amount.t
      -> txn_global_slot:Global_slot.t
      -> (Account.Timing.t * [> `Min_balance of Balance.t]) Or_error.t

    val validate_timing :
         account:Account.t
      -> txn_amount:Amount.t
      -> txn_global_slot:Global_slot.t
      -> Account.Timing.t Or_error.t
  end
end

(* tags for timing validation errors *)
let nsf_tag = "nsf"

let min_balance_tag = "minbal"

let timing_error_to_user_command_status err =
  match Error.Internal_repr.of_info err with
  | Tag_t (tag, _) when String.equal tag nsf_tag ->
      Transaction_status.Failure.Source_insufficient_balance
  | Tag_t (tag, _) when String.equal tag min_balance_tag ->
      Transaction_status.Failure.Source_minimum_balance_violation
  | _ ->
      failwith "Unexpected timed account validation error"

let validate_timing_with_min_balance ~account ~txn_amount ~txn_global_slot =
  let open Account.Poly in
  let open Account.Timing.Poly in
  match account.timing with
  | Untimed ->
      (* no time restrictions *)
      Or_error.return (Untimed, `Min_balance Balance.zero)
  | Timed
      { initial_minimum_balance
      ; cliff_time
      ; cliff_amount
      ; vesting_period
      ; vesting_increment } ->
      let open Or_error.Let_syntax in
      let%map curr_min_balance =
        let account_balance = account.balance in
        let nsf_error () =
          Or_error.errorf
            !"For timed account, the requested transaction for amount %{sexp: \
              Amount.t} at global slot %{sexp: Global_slot.t}, the balance \
              %{sexp: Balance.t} is insufficient"
            txn_amount txn_global_slot account_balance
          |> Or_error.tag ~tag:nsf_tag
        in
        let min_balance_error min_balance =
          Or_error.errorf
            !"For timed account, the requested transaction for amount %{sexp: \
              Amount.t} at global slot %{sexp: Global_slot.t}, applying the \
              transaction would put the balance below the calculated minimum \
              balance of %{sexp: Balance.t}"
            txn_amount txn_global_slot min_balance
          |> Or_error.tag ~tag:min_balance_tag
        in
        match Balance.(account_balance - txn_amount) with
        | None ->
            (* checking for sufficient funds may be redundant with a check elsewhere
               regardless, the transaction would put the account below any calculated minimum balance
               so don't bother with the remaining computations
            *)
            nsf_error ()
        | Some proposed_new_balance ->
            let curr_min_balance =
              Account.min_balance_at_slot ~global_slot:txn_global_slot
                ~cliff_time ~cliff_amount ~vesting_period ~vesting_increment
                ~initial_minimum_balance
            in
            if Balance.(proposed_new_balance < curr_min_balance) then
              min_balance_error curr_min_balance
            else Or_error.return curr_min_balance
      in
      (* once the calculated minimum balance becomes zero, the account becomes untimed *)
      if Balance.(curr_min_balance > zero) then
        (account.timing, `Min_balance curr_min_balance)
      else (Untimed, `Min_balance Balance.zero)

let validate_timing ~account ~txn_amount ~txn_global_slot =
  let open Result.Let_syntax in
  let%map timing, `Min_balance _ =
    validate_timing_with_min_balance ~account ~txn_amount ~txn_global_slot
  in
  timing

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
        Ok (set ledger location account)
    | `New ->
        Or_error.ignore_m
        @@ get_or_create_account ledger (Account.identifier account) account

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

  module Transaction_applied = struct
    include Transaction_applied

    let transaction : t -> Transaction.t With_status.t =
     fun {varying; _} ->
      match varying with
      | Command (Signed_command uc) ->
          With_status.map uc.common.user_command ~f:(fun cmd ->
              Transaction.Command (User_command.Signed_command cmd) )
      | Command (Snapp_command s) ->
          With_status.map s.command ~f:(fun c ->
              Transaction.Command (User_command.Snapp_command c) )
      | Fee_transfer f ->
          { data= Fee_transfer f.fee_transfer
          ; status=
              Applied
                ( Transaction_status.Auxiliary_data.empty
                , Transaction_status.Fee_transfer_balance_data.to_balance_data
                    f.balances ) }
      | Coinbase c ->
          { data= Coinbase c.coinbase
          ; status=
              Applied
                ( Transaction_status.Auxiliary_data.empty
                , Transaction_status.Coinbase_balance_data.to_balance_data
                    c.balances ) }

    let user_command_status : t -> Transaction_status.t =
     fun {varying; _} ->
      match varying with
      | Command (Signed_command {common= {user_command= {status; _}; _}; _}) ->
          status
      | Command (Snapp_command c) ->
          c.command.status
      | Fee_transfer f ->
          Applied
            ( Transaction_status.Auxiliary_data.empty
            , Transaction_status.Fee_transfer_balance_data.to_balance_data
                f.balances )
      | Coinbase c ->
          Applied
            ( Transaction_status.Auxiliary_data.empty
            , Transaction_status.Coinbase_balance_data.to_balance_data
                c.balances )
  end

  let previous_empty_accounts action pk = if action = `Added then [pk] else []

  let has_locked_tokens ~global_slot ~account_id ledger =
    let open Or_error.Let_syntax in
    let%map _, account = get_with_location ledger account_id in
    Account.has_locked_tokens ~global_slot account

  let get_user_account_with_location ledger account_id =
    let open Or_error.Let_syntax in
    let%bind ((_, acct) as r) = get_with_location ledger account_id in
    let%map () =
      check
        (Option.is_none acct.snapp)
        !"Expected account %{sexp: Account_id.t} to be a user account, got a \
          snapp account."
        account_id
    in
    r

  let check_exists t e =
    match t with `Existing _ -> Ok () | `New -> Result.fail e

  let failure (e : Transaction_status.Failure.t) = e

  let incr_balance (acct : Account.t) amt =
    match add_amount acct.balance amt with
    | Ok balance ->
        Ok {acct with balance}
    | Error _ ->
        Result.fail (failure Overflow)

  let with_err e = Result.map_error ~f:(fun _ -> failure e)

  (* Helper function for [apply_user_command_unchecked] *)
  let pay_fee' ~command ~nonce ~fee_payer ~fee ~ledger ~current_global_slot =
    let open Or_error.Let_syntax in
    (* Fee-payer information *)
    let%bind location, account =
      get_user_account_with_location ledger fee_payer
    in
    let%bind () =
      match location with
      | `Existing _ ->
          return ()
      | `New ->
          Or_error.errorf "The fee-payer account does not exist"
    in
    let fee = Amount.of_fee fee in
    let%bind balance = sub_amount account.balance fee in
    let%bind () = validate_nonces nonce account.nonce in
    let%map timing =
      validate_timing ~txn_amount:fee ~txn_global_slot:current_global_slot
        ~account
    in
    ( location
    , account
    , { account with
        balance
      ; nonce= Account.Nonce.succ account.nonce
      ; receipt_chain_hash=
          Receipt.Chain_hash.cons command account.receipt_chain_hash
      ; timing } )

  (* Helper function for [apply_user_command_unchecked] *)
  let pay_fee ~user_command ~signer_pk ~ledger ~current_global_slot =
    let open Or_error.Let_syntax in
    (* Fee-payer information *)
    let nonce = Signed_command.nonce user_command in
    let fee_payer = Signed_command.fee_payer user_command in
    let%bind () =
      let fee_token = Signed_command.fee_token user_command in
      let%bind () =
        (* TODO: Enable multi-sig. *)
        if
          Public_key.Compressed.equal
            (Account_id.public_key fee_payer)
            signer_pk
        then return ()
        else
          Or_error.errorf
            "Cannot pay fees from a public key that did not sign the \
             transaction"
      in
      let%map () =
        (* TODO: Remove this check and update the transaction snark once we have
          an exchange rate mechanism. See issue #4447.
        *)
        if Token_id.equal fee_token Token_id.default then return ()
        else
          Or_error.errorf
            "Cannot create transactions with fee_token different from the \
             default"
      in
      ()
    in
    let%map loc, account, account' =
      pay_fee' ~command:(Signed_command user_command.payload) ~nonce ~fee_payer
        ~fee:(Signed_command.fee user_command)
        ~ledger ~current_global_slot
    in
    let applied_common : Transaction_applied.Signed_command_applied.Common.t =
      { user_command=
          { data= user_command
          ; status=
              Applied
                ( Transaction_status.Auxiliary_data.empty
                , Transaction_status.Balance_data.empty ) }
      ; previous_receipt_chain_hash= account.receipt_chain_hash
      ; fee_payer_timing= account.timing
      ; source_timing= None }
    in
    (loc, account', applied_common)

  (* someday: It would probably be better if we didn't modify the receipt chain hash
  in the case that the sender is equal to the receiver, but it complicates the SNARK, so
  we don't for now. *)
  let apply_user_command_unchecked
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ~txn_global_slot ledger
      ({payload; signer; signature= _} as user_command : Signed_command.t) =
    let open Or_error.Let_syntax in
    let signer_pk = Public_key.compress signer in
    let current_global_slot = txn_global_slot in
    let%bind () =
      validate_time
        ~valid_until:(Signed_command.valid_until user_command)
        ~current_global_slot
    in
    (* Fee-payer information *)
    let fee_payer = Signed_command.fee_payer user_command in
    let%bind fee_payer_location, fee_payer_account, applied_common =
      pay_fee ~user_command ~signer_pk ~ledger ~current_global_slot
    in
    (* Charge the fee. This must happen, whether or not the command itself
       succeeds, to ensure that the network is compensated for processing this
       command.
    *)
    let%bind () =
      set_with_location ledger fee_payer_location fee_payer_account
    in
    let next_available_token = next_available_token ledger in
    let source = Signed_command.source ~next_available_token user_command in
    let receiver =
      Signed_command.receiver ~next_available_token user_command
    in
    let exception Reject of Error.t in
    let ok_or_reject = function
      | Ok x ->
          x
      | Error err ->
          raise (Reject err)
    in
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
      let open Result.Let_syntax in
      (* Compute the necessary changes to apply the command, failing if any of
         the conditions are not met.
      *)
      let%bind predicate_passed =
        if
          Public_key.Compressed.equal
            (Signed_command.fee_payer_pk user_command)
            (Signed_command.source_pk user_command)
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
          | Payment _ | Stake_delegation _ | Mint_tokens _ ->
              (* TODO(#4554): Hook predicate evaluation in here once implemented. *)
              Result.fail Transaction_status.Failure.Predicate
      in
      match payload.body with
      | Stake_delegation _ ->
          let receiver_location, _receiver_account =
            (* Check that receiver account exists. *)
            get_with_location ledger receiver |> ok_or_reject
          in
          let source_location, source_account =
            get_with_location ledger source |> ok_or_reject
          in
          let%bind () =
            match (source_location, receiver_location) with
            | `Existing _, `Existing _ ->
                return ()
            | `New, _ ->
                Result.fail Transaction_status.Failure.Source_not_present
            | _, `New ->
                Result.fail Transaction_status.Failure.Receiver_not_present
          in
          let previous_delegate = source_account.delegate in
          let source_timing = source_account.timing in
          (* Timing is always valid, but we need to record any switch from
             timed to untimed here to stay in sync with the snark.
          *)
          let%map timing =
            validate_timing ~txn_amount:Amount.zero
              ~txn_global_slot:current_global_slot ~account:source_account
            |> Result.map_error ~f:timing_error_to_user_command_status
          in
          let source_account =
            { source_account with
              delegate= Some (Account_id.public_key receiver)
            ; timing }
          in
          ( [(source_location, source_account)]
          , `Source_timing source_timing
          , Transaction_status.Auxiliary_data.empty
          , Transaction_applied.Signed_command_applied.Body.Stake_delegation
              {previous_delegate} )
      | Payment {amount; token_id= token; _} ->
          let receiver_location, receiver_account =
            get_with_location ledger receiver |> ok_or_reject
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
                  |> Result.map_error ~f:(fun _ ->
                         Transaction_status.Failure
                         .Amount_insufficient_to_create_account )
                else
                  Result.fail
                    Transaction_status.Failure.Cannot_pay_creation_fee_in_token
          in
          let%bind receiver_account =
            incr_balance receiver_account receiver_amount
          in
          let%map source_location, source_timing, source_account =
            let ret =
              let%bind location, account =
                if Account_id.equal source receiver then
                  match receiver_location with
                  | `Existing _ ->
                      return (receiver_location, receiver_account)
                  | `New ->
                      Result.fail Transaction_status.Failure.Source_not_present
                else return (get_with_location ledger source |> ok_or_reject)
              in
              let%bind () =
                match location with
                | `Existing _ ->
                    return ()
                | `New ->
                    Result.fail Transaction_status.Failure.Source_not_present
              in
              let source_timing = account.timing in
              let%bind timing =
                validate_timing ~txn_amount:amount
                  ~txn_global_slot:current_global_slot ~account
                |> Result.map_error ~f:timing_error_to_user_command_status
              in
              let%map balance =
                Result.map_error (sub_amount account.balance amount)
                  ~f:(fun _ ->
                    Transaction_status.Failure.Source_insufficient_balance )
              in
              (location, source_timing, {account with timing; balance})
            in
            if Account_id.equal fee_payer source then
              (* Don't process transactions with insufficient balance from the
                 fee-payer.
              *)
              match ret with
              | Ok x ->
                  Ok x
              | Error failure ->
                  raise
                    (Reject
                       (Error.createf "%s"
                          (Transaction_status.Failure.describe failure)))
            else ret
          in
          let previous_empty_accounts, auxiliary_data =
            match receiver_location with
            | `Existing _ ->
                ([], Transaction_status.Auxiliary_data.empty)
            | `New ->
                ( [receiver]
                , { Transaction_status.Auxiliary_data.empty with
                    receiver_account_creation_fee_paid=
                      Some
                        (Amount.of_fee
                           constraint_constants.account_creation_fee) } )
          in
          ( [ (receiver_location, receiver_account)
            ; (source_location, source_account) ]
          , `Source_timing source_timing
          , auxiliary_data
          , Transaction_applied.Signed_command_applied.Body.Payment
              {previous_empty_accounts} )
      | Create_new_token {disable_new_accounts; _} ->
          (* NOTE: source and receiver are definitionally equal here. *)
          let fee_payer_account =
            Or_error.try_with (fun () ->
                charge_account_creation_fee_exn fee_payer_account )
            |> ok_or_reject
          in
          let receiver_location, receiver_account =
            get_with_location ledger receiver |> ok_or_reject
          in
          if not (receiver_location = `New) then
            failwith
              "Token owner account for newly created token already exists?!?!" ;
          let receiver_account =
            { receiver_account with
              token_permissions=
                Token_permissions.Token_owned {disable_new_accounts} }
          in
          return
            ( [ (fee_payer_location, fee_payer_account)
              ; (receiver_location, receiver_account) ]
            , `Source_timing receiver_account.timing
            , { Transaction_status.Auxiliary_data.empty with
                fee_payer_account_creation_fee_paid=
                  Some
                    (Amount.of_fee constraint_constants.account_creation_fee)
              ; created_token= Some next_available_token }
            , Transaction_applied.Signed_command_applied.Body.Create_new_token
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
            Or_error.try_with (fun () ->
                charge_account_creation_fee_exn fee_payer_account )
            |> ok_or_reject
          in
          let receiver_location, receiver_account =
            get_with_location ledger receiver |> ok_or_reject
          in
          let%bind () =
            match receiver_location with
            | `New ->
                return ()
            | `Existing _ ->
                Result.fail Transaction_status.Failure.Receiver_already_exists
          in
          let receiver_account =
            { receiver_account with
              token_permissions= Token_permissions.Not_owned {account_disabled}
            }
          in
          let source_location, source_account =
            get_with_location ledger source |> ok_or_reject
          in
          let%bind source_account =
            if Account_id.equal source receiver then return receiver_account
            else if Account_id.equal source fee_payer then
              return fee_payer_account
            else
              match source_location with
              | `New ->
                  Result.fail Transaction_status.Failure.Source_not_present
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
                  Result.fail
                    Transaction_status.Failure.Mismatched_token_permissions
                else return ()
            | Not_owned _ ->
                if Token_id.(equal default) (Account_id.token_id receiver) then
                  return ()
                else Result.fail Transaction_status.Failure.Not_token_owner
          in
          let source_timing = source_account.timing in
          let%map source_account =
            let%map timing =
              validate_timing ~txn_amount:Amount.zero
                ~txn_global_slot:current_global_slot ~account:source_account
              |> Result.map_error ~f:timing_error_to_user_command_status
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
          , { Transaction_status.Auxiliary_data.empty with
              fee_payer_account_creation_fee_paid=
                Some (Amount.of_fee constraint_constants.account_creation_fee)
            }
          , Transaction_applied.Signed_command_applied.Body
            .Create_token_account )
      | Mint_tokens {token_id= token; amount; _} ->
          let%bind () =
            if Token_id.(equal default) token then
              Result.fail Transaction_status.Failure.Not_token_owner
            else return ()
          in
          let receiver_location, receiver_account =
            get_with_location ledger receiver |> ok_or_reject
          in
          let%bind () =
            match receiver_location with
            | `Existing _ ->
                return ()
            | `New ->
                Result.fail Transaction_status.Failure.Receiver_not_present
          in
          let%bind receiver_account = incr_balance receiver_account amount in
          let%map source_location, source_timing, source_account =
            let location, account =
              if Account_id.equal source receiver then
                (receiver_location, receiver_account)
              else get_with_location ledger source |> ok_or_reject
            in
            let%bind () =
              match location with
              | `Existing _ ->
                  return ()
              | `New ->
                  Result.fail Transaction_status.Failure.Source_not_present
            in
            let%bind () =
              match account.token_permissions with
              | Token_owned _ ->
                  return ()
              | Not_owned _ ->
                  Result.fail Transaction_status.Failure.Not_token_owner
            in
            let source_timing = account.timing in
            let%map timing =
              validate_timing ~txn_amount:Amount.zero
                ~txn_global_slot:current_global_slot ~account
              |> Result.map_error ~f:timing_error_to_user_command_status
            in
            (location, source_timing, {account with timing})
          in
          ( [ (receiver_location, receiver_account)
            ; (source_location, source_account) ]
          , `Source_timing source_timing
          , Transaction_status.Auxiliary_data.empty
          , Transaction_applied.Signed_command_applied.Body.Mint_tokens )
    in
    let compute_balances () =
      let compute_balance account_id =
        match get_user_account_with_location ledger account_id with
        | Ok (`Existing _, account) ->
            Some account.balance
        | _ ->
            None
      in
      { Transaction_status.Balance_data.fee_payer_balance=
          compute_balance fee_payer
      ; source_balance= compute_balance source
      ; receiver_balance= compute_balance receiver }
    in
    match compute_updates () with
    | Ok
        ( located_accounts
        , `Source_timing source_timing
        , auxiliary_data
        , applied_body ) ->
        (* Update the ledger. *)
        let%bind () =
          List.fold located_accounts ~init:(Ok ())
            ~f:(fun acc (location, account) ->
              let%bind () = acc in
              set_with_location ledger location account )
        in
        let applied_common =
          { applied_common with
            source_timing= Some source_timing
          ; user_command=
              { data= user_command
              ; status= Applied (auxiliary_data, compute_balances ()) } }
        in
        return
          ( {common= applied_common; body= applied_body}
            : Transaction_applied.Signed_command_applied.t )
    | Error failure ->
        (* Do not update the ledger. *)
        let applied_common =
          { applied_common with
            user_command=
              { data= user_command
              ; status= Failed (failure, compute_balances ()) } }
        in
        return
          ( {common= applied_common; body= Failed}
            : Transaction_applied.Signed_command_applied.t )
    | exception Reject err ->
        (* TODO: These transactions should never reach this stage, this error
           should be fatal.
        *)
        Error err

  let apply_user_command ~constraint_constants ~txn_global_slot ledger
      (user_command : Signed_command.With_valid_signature.t) =
    apply_user_command_unchecked ~constraint_constants ~txn_global_slot ledger
      (Signed_command.forget_check user_command)

  let opt_fail e = function Some x -> Ok x | None -> Error (failure e)

  let add_signed_amount b (a : Amount.Signed.t) =
    ( match a.sgn with
    | Pos ->
        Balance.add_amount b a.magnitude
    | Neg ->
        Balance.sub_amount b a.magnitude )
    |> opt_fail Overflow

  let check e b = if b then Ok () else Error (failure e)

  open Pickles_types

  let apply_body
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ~(state_view : Snapp_predicate.Protocol_state.View.t) ~check_auth ~is_new
      ({ pk= _
       ; update= {app_state; delegate; verification_key; permissions}
       ; delta } :
        Snapp_command.Party.Body.t) (a : Account.t) : (Account.t, _) Result.t =
    let open Snapp_basic in
    let open Result.Let_syntax in
    let%bind balance =
      let%bind b = add_signed_amount a.balance delta in
      let fee = constraint_constants.account_creation_fee in
      if is_new then
        Balance.sub_amount b (Amount.of_fee fee)
        |> opt_fail Amount_insufficient_to_create_account
      else Ok b
    in
    (* Check send/receive permissions *)
    let%bind () =
      check Update_not_permitted
        (check_auth
           ( match delta.sgn with
           | Pos ->
               a.permissions.receive
           | Neg ->
               a.permissions.send ))
    in
    (* Check timing. *)
    let%bind timing =
      match delta.sgn with
      | Pos ->
          Ok a.timing
      | Neg ->
          validate_timing ~txn_amount:delta.magnitude
            ~txn_global_slot:state_view.global_slot_since_genesis ~account:a
          |> Result.map_error ~f:timing_error_to_user_command_status
    in
    let init =
      match a.snapp with None -> Snapp_account.default | Some a -> a
    in
    let update perm u curr ~is_keep ~update =
      match check_auth perm with
      | false ->
          let%map () = check Update_not_permitted (is_keep u) in
          curr
      | true ->
          Ok (update u curr)
    in
    let%bind delegate =
      if Token_id.(equal default) a.token_id then
        update a.permissions.set_delegate delegate a.delegate
          ~is_keep:(( = ) Set_or_keep.Keep) ~update:(fun u x ->
            match u with Keep -> x | Set y -> Some y )
      else return a.delegate
    in
    let%bind snapp =
      let%map app_state =
        update a.permissions.edit_state app_state init.app_state
          ~is_keep:(Vector.for_all ~f:(( = ) Set_or_keep.Keep))
          ~update:(Vector.map2 ~f:Set_or_keep.set_or_keep)
      and verification_key =
        update a.permissions.set_verification_key verification_key
          init.verification_key ~is_keep:(( = ) Set_or_keep.Keep)
          ~update:(fun u x ->
            match (u, x) with Keep, _ -> x | Set x, _ -> Some x )
      in
      let t : Snapp_account.t = {app_state; verification_key} in
      if Snapp_account.(equal default t) then None else Some t
    in
    let%bind permissions =
      update a.permissions.set_delegate permissions a.permissions
        ~is_keep:(( = ) Set_or_keep.Keep) ~update:Set_or_keep.set_or_keep
    in
    Ok {a with balance; snapp; delegate; permissions; timing}

  let apply_snapp_command_unchecked ledger
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ~(state_view : Snapp_predicate.Protocol_state.View.t)
      (c : Snapp_command.t) =
    let open Snapp_command in
    let current_global_slot = state_view.global_slot_since_genesis in
    let open Result.Let_syntax in
    with_return (fun ({return} : _ Result.t return) ->
        let ok_or_reject = function
          | Ok x ->
              x
          | Error err ->
              return (Error err)
        in
        let opt err = function
          | Some x ->
              x
          | None ->
              return (Or_error.error_string err)
        in
        let fee_token_id = Snapp_command.fee_token c in
        let fee_payer_account = Set_once.create () in
        let finish res =
          match res with
          | Error failure ->
              Ok
                { Transaction_applied.Snapp_command_applied.command=
                    { data= c
                    ; status=
                        Failed
                          ( failure
                          , (* TODO: This needs to contain the correct data when
                                we update the archive db to handle snapp
                                commands.
                            *)
                            Transaction_status.Balance_data.empty ) }
                ; accounts= [Set_once.get_exn fee_payer_account [%here]] }
          | Ok (accts, applied) ->
              let%bind () =
                List.fold accts ~init:(Ok ())
                  ~f:(fun acc (location, account) ->
                    let%bind () = acc in
                    set_with_location ledger location account )
              in
              Ok applied
        in
        let payload =
          Receipt.Elt.Snapp_command
            (Payload.Digested.digest (Payload.digested (to_payload c)))
        in
        let validate_timing a txn_amount =
          validate_timing ~txn_amount ~txn_global_slot:current_global_slot
            ~account:a
        in
        let step (a : Account.t) =
          { a with
            nonce= Account.Nonce.succ a.nonce
          ; receipt_chain_hash=
              Receipt.Chain_hash.cons payload a.receipt_chain_hash }
        in
        let step_amount ~amount (a : Account.t) =
          let%map timing = validate_timing a amount in
          {(step a) with timing}
        in
        let step_fee_payer a fee =
          let f = Amount.of_fee fee in
          { (Or_error.ok_exn (step_amount a ~amount:f)) with
            balance= opt "Cannot pay fee" (Balance.sub_amount a.balance f) }
        in
        let applied accounts =
          { Transaction_applied.Snapp_command_applied.command=
              { data= c
              ; status=
                  Applied
                    ( Transaction_status.Auxiliary_data.empty
                    , (* TODO: This needs to contain the correct data when we
                         update the archive db to handle snapp commands.
                      *)
                      Transaction_status.Balance_data.empty ) }
          ; accounts }
        in
        let pay_fee ({pk; nonce; fee; _} : Other_fee_payer.Payload.t) =
          let ((loc, acct, acct') as info) =
            pay_fee' ~command:payload ~nonce
              ~fee_payer:(Account_id.create pk fee_token_id)
              ~fee ~ledger ~current_global_slot
            |> ok_or_reject
          in
          Set_once.set_exn fee_payer_account [%here]
            (Account_id.create pk fee_token_id, Some acct) ;
          (* Charge the fee. This must happen, whether or not the command itself
            succeeds, to ensure that the network is compensated for processing this
            command.
          *)
          ok_or_reject @@ set_with_location ledger loc acct' ;
          info
        in
        let apply_body = apply_body ~constraint_constants ~state_view in
        let open Party in
        let set_delta
            (r : ((Body.t, _) Predicated.Poly.t, _) Authorized.Poly.t) delta =
          {r with data= {r.data with body= {r.data.body with delta}}}
        in
        let get_delta
            (r : ((Body.t, _) Predicated.Poly.t, _) Authorized.Poly.t) =
          r.data.body.delta
        in
        (* TODO:
           If there is an excess for the native token of the transaction, currently
           this just burns it. Probably we should assert that if another fee payer is
           present, the excess is 0. *)
        let f
            ({token_id; fee_payment; one; two} :
              ( ((Body.t, _) Predicated.Poly.t, _) Authorized.Poly.t
              , ((Body.t, _) Predicated.Poly.t, _) Authorized.Poly.t option )
              Inner.t) ~check_predicate1 ~check_predicate2
            ~account2_should_step =
          let account_id1 = Account_id.create one.data.body.pk token_id in
          let loc1, acct1 =
            get_with_location ledger account_id1 |> ok_or_reject
          in
          let%bind () = check_exists loc1 (failure Source_not_present) in
          let lacct2 =
            Option.map two ~f:(fun two ->
                get_with_location ledger
                  (Account_id.create two.data.body.pk token_id)
                |> ok_or_reject )
          in
          (* Pay the fee, step both accounts. *)
          let acct1', acct2', one, two, fee_payer_info, fee_payer_pk =
            let party_fee_payer loc acct p =
              let _, fee = native_excess_exn c in
              let acct = step_fee_payer acct (Amount.to_fee fee) in
              ok_or_reject @@ set_with_location ledger loc acct ;
              let delta =
                (* delta = delta_remaining + (-fee)
                  delta_remaining = delta + fee
                *)
                opt "Transaction overflow"
                  Amount.Signed.(add (get_delta p) (of_unsigned fee))
              in
              (acct, set_delta p delta)
            in
            match fee_payment with
            | None -> (
              (* TODO: Assert that the two have opposite signs *)
              match one.data.body.delta.sgn with
              | Neg ->
                  (* Account 1 is the sender. *)
                  Set_once.set_exn fee_payer_account [%here]
                    (account_id1, Some acct1) ;
                  let acct1, one = party_fee_payer loc1 acct1 one in
                  ( acct1
                  , Option.map lacct2 ~f:(fun (_, a) ->
                        if account2_should_step then step a else a )
                  , one
                  , two
                  , None
                  , one.data.body.pk )
              | Pos ->
                  (* Account 2 is the sender. *)
                  let err x = opt "account 2 is the sender, but is None" x in
                  let loc2, acct2 = err lacct2 in
                  Set_once.set_exn fee_payer_account [%here]
                    (Account_id.create acct2.public_key token_id, Some acct2) ;
                  let two = err two in
                  let acct2, two = party_fee_payer loc2 acct2 two in
                  ( step acct1 (* Account 1 always steps. *)
                  , Some acct2
                  , one
                  , Some two
                  , None
                  , two.data.body.pk ) )
            | Some {payload; signature= _} ->
                ( step acct1
                , Option.map lacct2 ~f:(fun (_, a) ->
                      if account2_should_step then step a else a )
                , one
                , two
                , Some (pay_fee payload)
                , payload.pk )
          in
          let%bind lacct2' =
            match (lacct2, acct2', two) with
            | None, None, None ->
                Ok None
            | ( Some (loc2, acct2)
              , Some acct2'
              , Some {data= {body; predicate}; authorization} ) ->
                (* TODO: Make sure that body.delta is positive. I think
                 the Snapp_command.check function does this. *)
                (* Check the predicate *)
                let%bind () =
                  check_predicate2 predicate ~state_view ~self:acct2
                    ~other_prev:(Some acct1) ~other_next:(Some ())
                    ~fee_payer_pk
                  |> with_err Predicate
                in
                (* Update *)
                let%map res =
                  let check_auth =
                    match loc2 with
                    | `New ->
                        (* If the account2 is new, we don't have to check permission. *)
                        fun _ -> true
                    | `Existing _ ->
                        Fn.flip Permissions.Auth_required.check
                          (Control.tag authorization)
                  in
                  apply_body ~check_auth body acct2' ~is_new:(loc2 = `New)
                in
                Some (loc2, res)
            | _ ->
                assert false
          in
          (* Check the predicate *)
          let%bind () =
            check_predicate1 one.data.predicate ~state_view ~self:acct1
              ~other_prev:
                ( match lacct2 with
                | None ->
                    None
                | Some (`New, _) ->
                    None
                | Some (`Existing _, a) ->
                    Some a )
              ~other_next:(Option.map lacct2' ~f:ignore)
              ~fee_payer_pk
            |> with_err Predicate
          in
          (* Update *)
          let%map acct1' =
            apply_body ~is_new:false
              ~check_auth:
                (Fn.flip Permissions.Auth_required.check
                   (Control.tag one.authorization))
              one.data.body acct1'
          in
          let applied_per_account loc (a : Account.t) =
            ( Account_id.create a.public_key fee_token_id
            , match loc with `New -> None | `Existing _ -> Some a )
          in
          ( ( [(loc1, acct1')] @ Option.to_list lacct2'
            @ Option.(
                to_list
                  (map fee_payer_info ~f:(fun (loc, _, fp_acct') ->
                       (loc, fp_acct') ))) )
          , applied
              ( (account_id1, Some acct1)
                :: Option.(
                     to_list
                       (map lacct2 ~f:(fun (loc, a) ->
                            applied_per_account loc a )))
              @ Option.(
                  to_list
                    (map fee_payer_info ~f:(fun (loc, a, _) ->
                         applied_per_account loc a ))) ) )
        in
        let check_nonce nonce ~state_view:_ ~(self : Account.t) ~other_prev:_
            ~other_next:_ ~fee_payer_pk:_ =
          validate_nonces nonce self.nonce
        in
        let no_check _auth ~state_view:_ ~self:_ ~other_prev:_ ~other_next:_
            ~fee_payer_pk:_ =
          Ok ()
        in
        let wrap_two (r : _ Inner.t) = {r with two= Some r.two} in
        let signed_one (r : (_ Party.Authorized.Poly.t, _) Inner.t) =
          { r with
            one=
              {r.one with authorization= Control.Signature r.one.authorization}
          }
        in
        let signed_two (r : (_, _ Party.Authorized.Poly.t) Inner.t) =
          { r with
            two=
              {r.two with authorization= Control.Signature r.two.authorization}
          }
        in
        ( match c with
        | Proved_empty r ->
            f
              { r with
                two=
                  Option.map r.two ~f:(fun two ->
                      {two with authorization= Control.None_given} ) }
              ~check_predicate1:Predicate.check ~check_predicate2:no_check
              ~account2_should_step:false
        | Signed_empty r ->
            f
              { r with
                two=
                  Option.map r.two ~f:(fun two ->
                      {two with authorization= Control.None_given} )
              ; one= {r.one with authorization= Signature r.one.authorization}
              }
              ~check_predicate1:check_nonce ~check_predicate2:no_check
              ~account2_should_step:false
        | Proved_proved r ->
            f (wrap_two r) ~check_predicate1:Predicate.check
              ~check_predicate2:Predicate.check ~account2_should_step:true
        | Proved_signed r ->
            f
              (wrap_two (signed_two r))
              ~check_predicate1:Predicate.check ~check_predicate2:check_nonce
              ~account2_should_step:true
        | Signed_signed r ->
            f
              (wrap_two (signed_one (signed_two r)))
              ~check_predicate1:check_nonce ~check_predicate2:check_nonce
              ~account2_should_step:true )
        |> finish )

  let update_timing_when_no_deduction ~txn_global_slot account =
    validate_timing ~txn_amount:Amount.zero ~txn_global_slot ~account

  let process_fee_transfer t (transfer : Fee_transfer.t) ~modify_balance
      ~modify_timing =
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
        let%bind action, a, loc = get_or_create t account_id in
        let emptys = previous_empty_accounts action account_id in
        let%bind timing = modify_timing a in
        let%map balance = modify_balance action account_id a.balance ft.fee in
        set t loc {a with balance; timing} ;
        (emptys, a.timing)
    | `Two (ft1, ft2) ->
        let account_id1 = Fee_transfer.Single.receiver ft1 in
        (* TODO(#4496): Do not use get_or_create here; we should not create a
           new account before we know that the transaction will go through and
           thus the creation fee has been paid.
        *)
        let%bind action1, a1, l1 = get_or_create t account_id1 in
        let emptys1 = previous_empty_accounts action1 account_id1 in
        let account_id2 = Fee_transfer.Single.receiver ft2 in
        if Account_id.equal account_id1 account_id2 then (
          let%bind fee = error_opt "overflow" (Fee.add ft1.fee ft2.fee) in
          let%bind timing = modify_timing a1 in
          let%map balance =
            modify_balance action1 account_id1 a1.balance fee
          in
          set t l1 {a1 with balance; timing} ;
          (emptys1, a1.timing) )
        else
          (* TODO(#4496): Do not use get_or_create here; we should not create a
             new account before we know that the transaction will go through
             and thus the creation fee has been paid.
          *)
          let%bind action2, a2, l2 = get_or_create t account_id2 in
          let emptys2 = previous_empty_accounts action2 account_id2 in
          let%bind balance1 =
            modify_balance action1 account_id1 a1.balance ft1.fee
          in
          (*Note: Not updating the timing field of a1 to avoid additional check in transactions snark (check_timing for "receiver"). This is OK because timing rules will not be violated when balance increases and will be checked whenever an amount is deducted from the account. (#5973)*)
          let%bind timing2 = modify_timing a2 in
          let%map balance2 =
            modify_balance action2 account_id2 a2.balance ft2.fee
          in
          set t l1 {a1 with balance= balance1} ;
          set t l2 {a2 with balance= balance2; timing= timing2} ;
          (emptys1 @ emptys2, a2.timing)

  let apply_fee_transfer ~constraint_constants ~txn_global_slot t transfer =
    let open Or_error.Let_syntax in
    let%map previous_empty_accounts, receiver_timing =
      process_fee_transfer t transfer
        ~modify_balance:(fun action _ b f ->
          let%bind amount =
            let amount = Amount.of_fee f in
            sub_account_creation_fee ~constraint_constants action amount
          in
          add_amount b amount )
        ~modify_timing:(fun acc ->
          update_timing_when_no_deduction ~txn_global_slot acc )
    in
    let compute_balance account_id =
      match get_user_account_with_location t account_id with
      | Ok (`Existing _, account) ->
          Some account.balance
      | _ ->
          None
    in
    let balances =
      match Fee_transfer.to_singles transfer with
      | `One ft ->
          { Transaction_status.Fee_transfer_balance_data.receiver1_balance=
              Option.value_exn
                (compute_balance (Fee_transfer.Single.receiver ft))
          ; receiver2_balance= None }
      | `Two (ft1, ft2) ->
          { Transaction_status.Fee_transfer_balance_data.receiver1_balance=
              Option.value_exn
                (compute_balance (Fee_transfer.Single.receiver ft1))
          ; receiver2_balance=
              compute_balance (Fee_transfer.Single.receiver ft2) }
    in
    Transaction_applied.Fee_transfer_applied.
      { fee_transfer= transfer
      ; previous_empty_accounts
      ; receiver_timing
      ; balances }

  let undo_fee_transfer ~constraint_constants t
      ({previous_empty_accounts; fee_transfer; receiver_timing; balances= _} :
        Transaction_applied.Fee_transfer_applied.t) =
    let open Or_error.Let_syntax in
    let%map _ =
      process_fee_transfer t fee_transfer
        ~modify_balance:(fun _ aid b f ->
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
        ~modify_timing:(fun _ -> Ok receiver_timing)
    in
    remove_accounts_exn t previous_empty_accounts

  let apply_coinbase ~constraint_constants ~txn_global_slot t
      (* TODO: Better system needed for making atomic changes. Could use a monad. *)
      ({receiver; fee_transfer; amount= coinbase_amount} as cb : Coinbase.t) =
    let open Or_error.Let_syntax in
    let%bind ( receiver_reward
             , emptys1
             , transferee_update
             , transferee_timing_prev ) =
      match fee_transfer with
      | None ->
          return (coinbase_amount, [], None, None)
      | Some ({receiver_pk= transferee; fee} as ft) ->
          assert (not @@ Public_key.Compressed.equal transferee receiver) ;
          let transferee_id = Coinbase.Fee_transfer.receiver ft in
          let fee = Amount.of_fee fee in
          let%bind receiver_reward =
            error_opt "Coinbase fee transfer too large"
              (Amount.sub coinbase_amount fee)
          in
          let%bind action, transferee_account, transferee_location =
            (* TODO(#4496): Do not use get_or_create here; we should not create
               a new account before we know that the transaction will go
               through and thus the creation fee has been paid.
            *)
            get_or_create t transferee_id
          in
          let emptys = previous_empty_accounts action transferee_id in
          let%bind timing =
            update_timing_when_no_deduction ~txn_global_slot transferee_account
          in
          let%map balance =
            let%bind amount =
              sub_account_creation_fee ~constraint_constants action fee
            in
            add_amount transferee_account.balance amount
          in
          ( receiver_reward
          , emptys
          , Some
              (transferee_location, {transferee_account with balance; timing})
          , Some transferee_account.timing )
    in
    let receiver_id = Account_id.create receiver Token_id.default in
    let%bind action2, receiver_account, receiver_location =
      (* TODO(#4496): Do not use get_or_create here; we should not create a new
         account before we know that the transaction will go through and thus
         the creation fee has been paid.
      *)
      get_or_create t receiver_id
    in
    let emptys2 = previous_empty_accounts action2 receiver_id in
    (* Note: Updating coinbase receiver timing only if there is no fee transfer. This is so as to not add any extra constraints in transaction snark for checking "receiver" timings. This is OK because timing rules will not be violated when balance increases and will be checked whenever an amount is deducted from the account(#5973)*)
    let%bind receiver_timing_for_applied, coinbase_receiver_timing =
      match transferee_timing_prev with
      | None ->
          let%map new_receiver_timing =
            update_timing_when_no_deduction ~txn_global_slot receiver_account
          in
          (receiver_account.timing, new_receiver_timing)
      | Some timing ->
          Ok (timing, receiver_account.timing)
    in
    let%map receiver_balance =
      let%bind amount =
        sub_account_creation_fee ~constraint_constants action2 receiver_reward
      in
      add_amount receiver_account.balance amount
    in
    set t receiver_location
      { receiver_account with
        balance= receiver_balance
      ; timing= coinbase_receiver_timing } ;
    Option.iter transferee_update ~f:(fun (l, a) -> set t l a) ;
    Transaction_applied.Coinbase_applied.
      { coinbase= cb
      ; previous_empty_accounts= emptys1 @ emptys2
      ; receiver_timing= receiver_timing_for_applied
      ; balances=
          { Transaction_status.Coinbase_balance_data.coinbase_receiver_balance=
              receiver_balance
          ; fee_transfer_receiver_balance=
              Option.map transferee_update ~f:(fun (_, a) -> a.balance) } }

  (* Don't have to be atomic here because these should never fail. In fact, none of
  the undo functions should ever return an error. This should be fixed in the types. *)
  let undo_coinbase ~constraint_constants t
      Transaction_applied.Coinbase_applied.
        { coinbase= {receiver; fee_transfer; amount= coinbase_amount}
        ; previous_empty_accounts
        ; receiver_timing
        ; balances= _ } =
    let receiver_reward, receiver_timing =
      match fee_transfer with
      | None ->
          (coinbase_amount, Some receiver_timing)
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
            { transferee_account with
              balance= transferee_balance
            ; timing= receiver_timing } ;
          (Option.value_exn (Amount.sub coinbase_amount fee), None)
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
    let timing =
      Option.value ~default:receiver_account.timing receiver_timing
    in
    set t receiver_location
      {receiver_account with balance= receiver_balance; timing} ;
    remove_accounts_exn t previous_empty_accounts

  let undo_user_command
      ~(constraint_constants : Genesis_constants.Constraint_constants.t) ledger
      { Transaction_applied.Signed_command_applied.common=
          { user_command=
              { data= {payload; signer= _; signature= _} as user_command
              ; status= _ }
          ; previous_receipt_chain_hash
          ; fee_payer_timing
          ; source_timing }
      ; body } =
    let open Or_error.Let_syntax in
    (* Fee-payer information *)
    let fee_payer = Signed_command.fee_payer user_command in
    let nonce = Signed_command.nonce user_command in
    let%bind fee_payer_location =
      location_of_account' ledger "fee payer" fee_payer
    in
    (* Refund the fee to the fee-payer. *)
    let%bind fee_payer_account =
      let%bind account = get' ledger "fee payer" fee_payer_location in
      let%bind () = validate_nonces (Account.Nonce.succ nonce) account.nonce in
      let%map balance =
        add_amount account.balance
          (Amount.of_fee (Signed_command.fee user_command))
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
    let source = Signed_command.source ~next_available_token user_command in
    let source_timing =
      (* Prefer fee-payer original timing when applicable, since it is the
         'true' original.
      *)
      if Account_id.equal fee_payer source then Some fee_payer_timing
      else source_timing
    in
    (* Reverse any other effects that the user command had. *)
    match (Signed_command.Payload.body payload, body) with
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
          Signed_command.receiver ~next_available_token user_command
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
          Signed_command.receiver ~next_available_token user_command
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
          Signed_command.receiver ~next_available_token user_command
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
    | _, _ ->
        failwith "Transaction_applied/command mismatch"

  let undo_snapp_command ~constraint_constants:_ ledger
      {Transaction_applied.Snapp_command_applied.accounts; command= _} =
    let to_update, to_delete =
      List.partition_map accounts ~f:(fun (id, a) ->
          match a with Some a -> `Fst (id, a) | None -> `Snd id )
    in
    let to_update =
      List.dedup_and_sort
        ~compare:(fun (x, _) (y, _) -> Account_id.compare x y)
        to_update
    in
    let open Or_error.Let_syntax in
    let%map to_update =
      List.map to_update ~f:(fun (id, a) ->
          let%map loc =
            location_of_account' ledger (sprintf !"%{sexp:Account_id.t}" id) id
          in
          (`Existing loc, a) )
      |> Or_error.all
    in
    remove_accounts_exn ledger to_delete ;
    List.iter to_update ~f:(fun (location, account) ->
        ignore @@ set_with_location ledger location account )

  let undo :
         constraint_constants:Genesis_constants.Constraint_constants.t
      -> t
      -> Transaction_applied.t
      -> unit Or_error.t =
   fun ~constraint_constants ledger applied ->
    let open Or_error.Let_syntax in
    let%map res =
      match applied.varying with
      | Fee_transfer u ->
          undo_fee_transfer ~constraint_constants ledger u
      | Command (Signed_command u) ->
          undo_user_command ~constraint_constants ledger u
      | Command (Snapp_command _u) ->
          failwith "Not yet implemented"
      | Coinbase c ->
          undo_coinbase ~constraint_constants ledger c ;
          Ok ()
    in
    Debug_assert.debug_assert (fun () ->
        [%test_eq: Ledger_hash.t] applied.previous_hash (merkle_root ledger) ) ;
    res

  let apply_transaction ~constraint_constants
      ~(txn_state_view : Snapp_predicate.Protocol_state.View.t) ledger
      (t : Transaction.t) =
    O1trace.measure "apply_transaction" (fun () ->
        let previous_hash = merkle_root ledger in
        let txn_global_slot = txn_state_view.global_slot_since_genesis in
        Or_error.map
          ( match t with
          | Command (Signed_command txn) ->
              Or_error.map
                (apply_user_command_unchecked ~constraint_constants
                   ~txn_global_slot ledger txn) ~f:(fun applied ->
                  Transaction_applied.Varying.Command (Signed_command applied)
              )
          | Command (Snapp_command txn) ->
              Or_error.map
                (apply_snapp_command_unchecked ~state_view:txn_state_view
                   ~constraint_constants ledger txn) ~f:(fun applied ->
                  Transaction_applied.Varying.Command (Snapp_command applied)
              )
          | Fee_transfer t ->
              Or_error.map
                (apply_fee_transfer ~constraint_constants ~txn_global_slot
                   ledger t) ~f:(fun applied ->
                  Transaction_applied.Varying.Fee_transfer applied )
          | Coinbase t ->
              Or_error.map
                (apply_coinbase ~constraint_constants ~txn_global_slot ledger t)
                ~f:(fun applied -> Transaction_applied.Varying.Coinbase applied)
          )
          ~f:(fun varying -> {Transaction_applied.previous_hash; varying}) )

  let merkle_root_after_snapp_command_exn ~constraint_constants ~txn_state_view
      ledger payment =
    let applied =
      Or_error.ok_exn
        (apply_snapp_command_unchecked ~constraint_constants
           ~state_view:txn_state_view ledger payment)
    in
    let root = merkle_root ledger in
    let next_available_token = next_available_token ledger in
    Or_error.ok_exn (undo_snapp_command ~constraint_constants ledger applied) ;
    (root, `Next_available_token next_available_token)

  let merkle_root_after_user_command_exn ~constraint_constants ~txn_global_slot
      ledger payment =
    let applied =
      Or_error.ok_exn
        (apply_user_command ~constraint_constants ~txn_global_slot ledger
           payment)
    in
    let root = merkle_root ledger in
    let next_available_token = next_available_token ledger in
    Or_error.ok_exn (undo_user_command ~constraint_constants ledger applied) ;
    (root, `Next_available_token next_available_token)

  module For_tests = struct
    let validate_timing_with_min_balance = validate_timing_with_min_balance

    let validate_timing = validate_timing
  end
end
