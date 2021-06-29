[%%import
"/src/config.mlh"]

open Core_kernel

(* if these items change, please also change
   Transaction_snark.Base.User_command_failure.t
   and update the code following it
*)
module Failure = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        | Predicate [@value 1]
        | Source_not_present
        | Receiver_not_present
        | Amount_insufficient_to_create_account
        | Cannot_pay_creation_fee_in_token
        | Source_insufficient_balance
        | Source_minimum_balance_violation
        | Receiver_already_exists
        | Not_token_owner
        | Mismatched_token_permissions
        | Overflow
        | Signed_command_on_snapp_account
        | Snapp_account_not_present
        | Update_not_permitted
        | Incorrect_nonce
      [@@deriving sexp, yojson, equal, compare, enum]

      let to_latest = Fn.id
    end
  end]

  type failure = t

  let failure_min = min

  let failure_max = max

  let to_latest = Fn.id

  let to_string = function
    | Predicate ->
        "Predicate"
    | Source_not_present ->
        "Source_not_present"
    | Receiver_not_present ->
        "Receiver_not_present"
    | Amount_insufficient_to_create_account ->
        "Amount_insufficient_to_create_account"
    | Cannot_pay_creation_fee_in_token ->
        "Cannot_pay_creation_fee_in_token"
    | Source_insufficient_balance ->
        "Source_insufficient_balance"
    | Source_minimum_balance_violation ->
        "Source_minimum_balance_violation"
    | Receiver_already_exists ->
        "Receiver_already_exists"
    | Not_token_owner ->
        "Not_token_owner"
    | Mismatched_token_permissions ->
        "Mismatched_token_permissions"
    | Overflow ->
        "Overflow"
    | Signed_command_on_snapp_account ->
        "Signed_command_on_snapp_account"
    | Snapp_account_not_present ->
        "Snapp_account_not_present"
    | Update_not_permitted ->
        "Update_not_permitted"
    | Incorrect_nonce ->
        "Incorrect_nonce"

  let of_string = function
    | "Predicate" ->
        Ok Predicate
    | "Source_not_present" ->
        Ok Source_not_present
    | "Receiver_not_present" ->
        Ok Receiver_not_present
    | "Amount_insufficient_to_create_account" ->
        Ok Amount_insufficient_to_create_account
    | "Cannot_pay_creation_fee_in_token" ->
        Ok Cannot_pay_creation_fee_in_token
    | "Source_insufficient_balance" ->
        Ok Source_insufficient_balance
    | "Source_minimum_balance_violation" ->
        Ok Source_minimum_balance_violation
    | "Receiver_already_exists" ->
        Ok Receiver_already_exists
    | "Not_token_owner" ->
        Ok Not_token_owner
    | "Mismatched_token_permissions" ->
        Ok Mismatched_token_permissions
    | "Overflow" ->
        Ok Overflow
    | "Signed_command_on_snapp_account" ->
        Ok Signed_command_on_snapp_account
    | "Snapp_account_not_present" ->
        Ok Snapp_account_not_present
    | "Update_not_permitted" ->
        Ok Update_not_permitted
    | "Incorrect_nonce" ->
        Ok Incorrect_nonce
    | _ ->
        Error "Signed_command_status.Failure.of_string: Unknown value"

  let%test_unit "of_string(to_string) roundtrip" =
    for i = failure_min to failure_max do
      let failure = Option.value_exn (of_enum i) in
      [%test_eq: (t, string) Result.t]
        (of_string (to_string failure))
        (Ok failure)
    done

  let describe = function
    | Predicate ->
        "A predicate failed"
    | Source_not_present ->
        "The source account does not exist"
    | Receiver_not_present ->
        "The receiver account does not exist"
    | Amount_insufficient_to_create_account ->
        "Cannot create account: transaction amount is smaller than the \
         account creation fee"
    | Cannot_pay_creation_fee_in_token ->
        "Cannot create account: account creation fees cannot be paid in \
         non-default tokens"
    | Source_insufficient_balance ->
        "The source account has an insufficient balance"
    | Source_minimum_balance_violation ->
        "The source account requires a minimum balance"
    | Receiver_already_exists ->
        "Attempted to create an account that already exists"
    | Not_token_owner ->
        "The source account does not own the token"
    | Mismatched_token_permissions ->
        "The permissions for this token do not match those in the command"
    | Overflow ->
        "The resulting balance is too large to store"
    | Signed_command_on_snapp_account ->
        "The source of a signed command cannot be a snapp account"
    | Snapp_account_not_present ->
        "A snapp account does not exist"
    | Update_not_permitted ->
        "An account is not permitted to make the given update"
    | Incorrect_nonce ->
        "Incorrect nonce"

  [%%ifdef
  consensus_mechanism]

  open Snark_params.Tick

  module As_record = struct
    (** Representation of a user command failure as a record, so that it may be
        consumed by a snarky computation.
    *)

    module Poly = struct
      type 'bool t =
        { predicate: 'bool
        ; source_not_present: 'bool
        ; receiver_not_present: 'bool
        ; amount_insufficient_to_create_account: 'bool
        ; cannot_pay_creation_fee_in_token: 'bool
        ; source_insufficient_balance: 'bool
        ; source_minimum_balance_violation: 'bool
        ; receiver_already_exists: 'bool
        ; not_token_owner: 'bool
        ; mismatched_token_permissions: 'bool
        ; overflow: 'bool
        ; signed_command_on_snapp_account: 'bool
        ; snapp_account_not_present: 'bool
        ; update_not_permitted: 'bool
        ; incorrect_nonce: 'bool }
      [@@deriving hlist, equal, sexp, compare]

      let map ~f
          { predicate
          ; source_not_present
          ; receiver_not_present
          ; amount_insufficient_to_create_account
          ; cannot_pay_creation_fee_in_token
          ; source_insufficient_balance
          ; source_minimum_balance_violation
          ; receiver_already_exists
          ; not_token_owner
          ; mismatched_token_permissions
          ; overflow
          ; signed_command_on_snapp_account
          ; snapp_account_not_present
          ; update_not_permitted
          ; incorrect_nonce } =
        { predicate= f predicate
        ; source_not_present= f source_not_present
        ; receiver_not_present= f receiver_not_present
        ; amount_insufficient_to_create_account=
            f amount_insufficient_to_create_account
        ; cannot_pay_creation_fee_in_token= f cannot_pay_creation_fee_in_token
        ; source_insufficient_balance= f source_insufficient_balance
        ; source_minimum_balance_violation= f source_minimum_balance_violation
        ; receiver_already_exists= f receiver_already_exists
        ; not_token_owner= f not_token_owner
        ; mismatched_token_permissions= f mismatched_token_permissions
        ; overflow= f overflow
        ; signed_command_on_snapp_account= f signed_command_on_snapp_account
        ; snapp_account_not_present= f snapp_account_not_present
        ; update_not_permitted= f update_not_permitted
        ; incorrect_nonce= f incorrect_nonce }
    end

    type 'bool poly = 'bool Poly.t =
      { predicate: 'bool
      ; source_not_present: 'bool
      ; receiver_not_present: 'bool
      ; amount_insufficient_to_create_account: 'bool
      ; cannot_pay_creation_fee_in_token: 'bool
      ; source_insufficient_balance: 'bool
      ; source_minimum_balance_violation: 'bool
      ; receiver_already_exists: 'bool
      ; not_token_owner: 'bool
      ; mismatched_token_permissions: 'bool
      ; overflow: 'bool
      ; signed_command_on_snapp_account: 'bool
      ; snapp_account_not_present: 'bool
      ; update_not_permitted: 'bool
      ; incorrect_nonce: 'bool }
    [@@deriving equal, sexp, compare]

    type t = bool poly [@@deriving equal, sexp, compare]

    let get t = function
      | Predicate ->
          t.predicate
      | Source_not_present ->
          t.source_not_present
      | Receiver_not_present ->
          t.receiver_not_present
      | Amount_insufficient_to_create_account ->
          t.amount_insufficient_to_create_account
      | Cannot_pay_creation_fee_in_token ->
          t.cannot_pay_creation_fee_in_token
      | Source_insufficient_balance ->
          t.source_insufficient_balance
      | Source_minimum_balance_violation ->
          t.source_minimum_balance_violation
      | Receiver_already_exists ->
          t.receiver_already_exists
      | Not_token_owner ->
          t.not_token_owner
      | Mismatched_token_permissions ->
          t.mismatched_token_permissions
      | Overflow ->
          t.overflow
      | Signed_command_on_snapp_account ->
          t.signed_command_on_snapp_account
      | Snapp_account_not_present ->
          t.snapp_account_not_present
      | Update_not_permitted ->
          t.update_not_permitted
      | Incorrect_nonce ->
          t.incorrect_nonce

    type var = Boolean.var poly

    let var_of_t = Poly.map ~f:Boolean.var_of_value

    let check_invariants
        { predicate
        ; source_not_present
        ; receiver_not_present
        ; amount_insufficient_to_create_account
        ; cannot_pay_creation_fee_in_token
        ; source_insufficient_balance
        ; source_minimum_balance_violation
        ; receiver_already_exists
        ; not_token_owner
        ; mismatched_token_permissions
        ; overflow
        ; signed_command_on_snapp_account
        ; snapp_account_not_present
        ; update_not_permitted
        ; incorrect_nonce } =
      let bool_to_int b = if b then 1 else 0 in
      let failures =
        bool_to_int predicate
        + bool_to_int source_not_present
        + bool_to_int receiver_not_present
        + bool_to_int amount_insufficient_to_create_account
        + bool_to_int cannot_pay_creation_fee_in_token
        + bool_to_int source_insufficient_balance
        + bool_to_int source_minimum_balance_violation
        + bool_to_int receiver_already_exists
        + bool_to_int not_token_owner
        + bool_to_int mismatched_token_permissions
        + bool_to_int overflow
        + bool_to_int signed_command_on_snapp_account
        + bool_to_int snapp_account_not_present
        + bool_to_int update_not_permitted
        + bool_to_int incorrect_nonce
      in
      failures = 0 || failures = 1

    let typ : (var, t) Typ.t =
      let bt = Boolean.typ in
      Typ.of_hlistable
        [bt; bt; bt; bt; bt; bt; bt; bt; bt; bt; bt; bt; bt; bt; bt]
        ~value_to_hlist:Poly.to_hlist ~value_of_hlist:Poly.of_hlist
        ~var_to_hlist:Poly.to_hlist ~var_of_hlist:Poly.of_hlist

    let none =
      { predicate= false
      ; source_not_present= false
      ; receiver_not_present= false
      ; amount_insufficient_to_create_account= false
      ; cannot_pay_creation_fee_in_token= false
      ; source_insufficient_balance= false
      ; source_minimum_balance_violation= false
      ; receiver_already_exists= false
      ; not_token_owner= false
      ; mismatched_token_permissions= false
      ; overflow= false
      ; signed_command_on_snapp_account= false
      ; snapp_account_not_present= false
      ; update_not_permitted= false
      ; incorrect_nonce= false }

    let predicate = {none with predicate= true}

    let source_not_present = {none with source_not_present= true}

    let receiver_not_present = {none with receiver_not_present= true}

    let amount_insufficient_to_create_account =
      {none with amount_insufficient_to_create_account= true}

    let cannot_pay_creation_fee_in_token =
      {none with cannot_pay_creation_fee_in_token= true}

    let source_insufficient_balance =
      {none with source_insufficient_balance= true}

    let source_minimum_balance_violation =
      {none with source_minimum_balance_violation= true}

    let receiver_already_exists = {none with receiver_already_exists= true}

    let not_token_owner = {none with not_token_owner= true}

    let mismatched_token_permissions =
      {none with mismatched_token_permissions= true}

    let overflow = {none with overflow= true}

    let signed_command_on_snapp_account =
      {none with signed_command_on_snapp_account= true}

    let snapp_account_not_present = {none with snapp_account_not_present= true}

    let update_not_permitted = {none with update_not_permitted= true}

    let incorrect_nonce = {none with incorrect_nonce= true}

    let to_enum = function
      | {predicate= true; _} ->
          to_enum Predicate
      | {source_not_present= true; _} ->
          to_enum Source_not_present
      | {receiver_not_present= true; _} ->
          to_enum Receiver_not_present
      | {amount_insufficient_to_create_account= true; _} ->
          to_enum Amount_insufficient_to_create_account
      | {cannot_pay_creation_fee_in_token= true; _} ->
          to_enum Cannot_pay_creation_fee_in_token
      | {source_insufficient_balance= true; _} ->
          to_enum Source_insufficient_balance
      | {source_minimum_balance_violation= true; _} ->
          to_enum Source_minimum_balance_violation
      | {receiver_already_exists= true; _} ->
          to_enum Receiver_already_exists
      | {not_token_owner= true; _} ->
          to_enum Not_token_owner
      | {mismatched_token_permissions= true; _} ->
          to_enum Mismatched_token_permissions
      | {overflow= true; _} ->
          to_enum Overflow
      | {signed_command_on_snapp_account= true; _} ->
          to_enum Signed_command_on_snapp_account
      | {snapp_account_not_present= true; _} ->
          to_enum Snapp_account_not_present
      | {update_not_permitted= true; _} ->
          to_enum Update_not_permitted
      | {incorrect_nonce= true; _} ->
          to_enum Incorrect_nonce
      | _ ->
          0

    let of_enum enum =
      match enum with
      | 0 ->
          Some none
      | _ -> (
        match of_enum enum with
        | Some failure ->
            Some
              ( match failure with
              | Predicate ->
                  predicate
              | Source_not_present ->
                  source_not_present
              | Receiver_not_present ->
                  receiver_not_present
              | Amount_insufficient_to_create_account ->
                  amount_insufficient_to_create_account
              | Cannot_pay_creation_fee_in_token ->
                  cannot_pay_creation_fee_in_token
              | Source_insufficient_balance ->
                  source_insufficient_balance
              | Source_minimum_balance_violation ->
                  source_minimum_balance_violation
              | Receiver_already_exists ->
                  receiver_already_exists
              | Not_token_owner ->
                  not_token_owner
              | Mismatched_token_permissions ->
                  mismatched_token_permissions
              | Overflow ->
                  overflow
              | Signed_command_on_snapp_account ->
                  signed_command_on_snapp_account
              | Snapp_account_not_present ->
                  snapp_account_not_present
              | Update_not_permitted ->
                  update_not_permitted
              | Incorrect_nonce ->
                  incorrect_nonce )
        | None ->
            None )

    let min = 0

    let max = failure_max

    let%test_unit "of_enum obeys invariants" =
      for i = min to max do
        assert (check_invariants (Option.value_exn (of_enum i)))
      done
  end

  module Var : sig
    module Accumulators : sig
      type t = private {user_command_failure: Boolean.var}
    end

    (** Canonical representation for user command failures in snarky.
    
        This bundles some useful accumulators with the underlying record to
        enable us to do a cheap checking operation. The type is private to
        ensure that the invariants of this check are always satisfied.
    *)
    type t = private {data: As_record.var; accumulators: Accumulators.t}

    val min : int

    val max : int

    val of_enum : int -> t option

    val typ : (t, As_record.t) Typ.t

    val none : t

    val predicate : t

    val source_not_present : t

    val receiver_not_present : t

    val amount_insufficient_to_create_account : t

    val cannot_pay_creation_fee_in_token : t

    val source_insufficient_balance : t

    val source_minimum_balance_violation : t

    val receiver_already_exists : t

    val not_token_owner : t

    val mismatched_token_permissions : t

    val overflow : t

    val signed_command_on_snapp_account : t

    val snapp_account_not_present : t

    val update_not_permitted : t

    val incorrect_nonce : t

    val get : t -> failure -> Boolean.var
  end = struct
    module Accumulators = struct
      (* TODO: receiver, source accumulators *)
      type t = {user_command_failure: Boolean.var}

      let make_unsafe
          ({ predicate
           ; source_not_present
           ; receiver_not_present
           ; amount_insufficient_to_create_account
           ; cannot_pay_creation_fee_in_token
           ; source_insufficient_balance
           ; source_minimum_balance_violation
           ; receiver_already_exists
           ; not_token_owner
           ; mismatched_token_permissions
           ; overflow
           ; signed_command_on_snapp_account
           ; snapp_account_not_present
           ; update_not_permitted
           ; incorrect_nonce } :
            As_record.var) : t =
        let user_command_failure =
          Boolean.Unsafe.of_cvar
            (Field.Var.sum
               [ (predicate :> Field.Var.t)
               ; (source_not_present :> Field.Var.t)
               ; (receiver_not_present :> Field.Var.t)
               ; (amount_insufficient_to_create_account :> Field.Var.t)
               ; (cannot_pay_creation_fee_in_token :> Field.Var.t)
               ; (source_insufficient_balance :> Field.Var.t)
               ; (source_minimum_balance_violation :> Field.Var.t)
               ; (receiver_already_exists :> Field.Var.t)
               ; (not_token_owner :> Field.Var.t)
               ; (mismatched_token_permissions :> Field.Var.t)
               ; (overflow :> Field.Var.t)
               ; (signed_command_on_snapp_account :> Field.Var.t)
               ; (snapp_account_not_present :> Field.Var.t)
               ; (update_not_permitted :> Field.Var.t)
               ; (incorrect_nonce :> Field.Var.t) ])
        in
        {user_command_failure}

      let check {user_command_failure} =
        Checked.ignore_m
        @@ Checked.all [Boolean.of_field (user_command_failure :> Field.Var.t)]
    end

    type t = {data: As_record.var; accumulators: Accumulators.t}

    let of_record data = {data; accumulators= Accumulators.make_unsafe data}

    let typ : (t, As_record.t) Typ.t =
      let typ = As_record.typ in
      { store= (fun data -> Typ.Store.map ~f:of_record (typ.store data))
      ; read= (fun {data; _} -> typ.read data)
      ; alloc= Typ.Alloc.map ~f:of_record typ.alloc
      ; check=
          Checked.(
            fun {data; accumulators} ->
              let%bind () = typ.check data in
              Accumulators.check accumulators) }

    let mk_var = Fn.compose of_record As_record.var_of_t

    let none = mk_var As_record.none

    let predicate = mk_var As_record.predicate

    let source_not_present = mk_var As_record.source_not_present

    let receiver_not_present = mk_var As_record.receiver_not_present

    let amount_insufficient_to_create_account =
      mk_var As_record.amount_insufficient_to_create_account

    let cannot_pay_creation_fee_in_token =
      mk_var As_record.cannot_pay_creation_fee_in_token

    let source_insufficient_balance =
      mk_var As_record.source_insufficient_balance

    let source_minimum_balance_violation =
      mk_var As_record.source_minimum_balance_violation

    let receiver_already_exists = mk_var As_record.receiver_already_exists

    let not_token_owner = mk_var As_record.not_token_owner

    let mismatched_token_permissions =
      mk_var As_record.mismatched_token_permissions

    let overflow = mk_var As_record.overflow

    let signed_command_on_snapp_account =
      mk_var As_record.signed_command_on_snapp_account

    let snapp_account_not_present = mk_var As_record.snapp_account_not_present

    let update_not_permitted = mk_var As_record.update_not_permitted

    let incorrect_nonce = mk_var As_record.incorrect_nonce

    let get {data; _} failure = As_record.get data failure

    let min = As_record.min

    let max = As_record.max

    let of_enum i = Option.map ~f:mk_var (As_record.of_enum i)
  end

  let to_record t =
    match As_record.of_enum (to_enum t) with
    | Some t ->
        t
    | None ->
        failwith
          "Internal error: Could not convert User_command.Status.Failure.t to \
           Transaction_status.Failure.As_record.t"

  let to_record_opt t =
    match t with None -> As_record.none | Some t -> to_record t

  let of_record_opt t = of_enum (As_record.to_enum t)

  let%test_unit "Minimum bound matches" =
    (* NB: +1 is for the [user_command_failure] accumulator. *)
    [%test_eq: int] failure_min (As_record.min + 1)

  let%test_unit "Maximum bound matches" = [%test_eq: int] max As_record.max

  let%test_unit "of_record_opt(to_record) roundtrip" =
    for i = failure_min to failure_max do
      let failure = Option.value_exn (of_enum i) in
      [%test_eq: t option] (of_record_opt (to_record failure)) (Some failure)
    done

  let%test_unit "to_record_opt(of_record_opt) roundtrip" =
    for i = As_record.min to As_record.max do
      let record = Option.value_exn (As_record.of_enum i) in
      [%test_eq: As_record.t] (to_record_opt (of_record_opt record)) record
    done

  let%test_unit "As_record.get is consistent" =
    for i = failure_min to failure_max do
      let failure = Option.value_exn (of_enum i) in
      let record = to_record failure in
      for j = failure_min to failure_max do
        let get_failure = Option.value_exn (of_enum j) in
        [%test_eq: bool]
          (As_record.get record get_failure)
          (equal failure get_failure)
      done
    done

  type var = Var.t

  let typ : (var, t) Typ.t =
    Typ.transport Var.typ ~there:to_record ~back:(fun x ->
        Option.value_exn (of_record_opt x) )

  let typ_opt : (var, t option) Typ.t =
    Typ.transport Var.typ ~there:to_record_opt ~back:of_record_opt

  let var_of_t t = Option.value_exn (Var.of_enum (to_enum t))

  let var_of_t_opt t = match t with Some t -> var_of_t t | None -> Var.none

  [%%endif]
end

module Balance_data = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { fee_payer_balance: Currency.Balance.Stable.V1.t option
        ; source_balance: Currency.Balance.Stable.V1.t option
        ; receiver_balance: Currency.Balance.Stable.V1.t option }
      [@@deriving sexp, yojson, equal, compare]

      let to_latest = Fn.id
    end
  end]

  let empty =
    {fee_payer_balance= None; source_balance= None; receiver_balance= None}
end

module Coinbase_balance_data = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { coinbase_receiver_balance: Currency.Balance.Stable.V1.t
        ; fee_transfer_receiver_balance: Currency.Balance.Stable.V1.t option }
      [@@deriving sexp, yojson, equal, compare]

      let to_latest = Fn.id
    end
  end]

  let of_balance_data_exn
      {Balance_data.fee_payer_balance; source_balance; receiver_balance} =
    ( match source_balance with
    | Some _ ->
        failwith
          "Unexpected source balance for Coinbase_balance_data.of_balance_data"
    | None ->
        () ) ;
    let coinbase_receiver_balance =
      match fee_payer_balance with
      | Some balance ->
          balance
      | None ->
          failwith
            "Missing fee-payer balance for \
             Coinbase_balance_data.of_balance_data"
    in
    {coinbase_receiver_balance; fee_transfer_receiver_balance= receiver_balance}

  let to_balance_data {coinbase_receiver_balance; fee_transfer_receiver_balance}
      =
    { Balance_data.fee_payer_balance= Some coinbase_receiver_balance
    ; source_balance= None
    ; receiver_balance= fee_transfer_receiver_balance }
end

module Fee_transfer_balance_data = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { receiver1_balance: Currency.Balance.Stable.V1.t
        ; receiver2_balance: Currency.Balance.Stable.V1.t option }
      [@@deriving sexp, yojson, equal, compare]

      let to_latest = Fn.id
    end
  end]

  let of_balance_data_exn
      {Balance_data.fee_payer_balance; source_balance; receiver_balance} =
    ( match source_balance with
    | Some _ ->
        failwith
          "Unexpected source balance for \
           Fee_transfer_balance_data.of_balance_data"
    | None ->
        () ) ;
    let receiver1_balance =
      match fee_payer_balance with
      | Some balance ->
          balance
      | None ->
          failwith
            "Missing fee-payer balance for \
             Fee_transfer_balance_data.of_balance_data"
    in
    {receiver1_balance; receiver2_balance= receiver_balance}

  let to_balance_data {receiver1_balance; receiver2_balance} =
    { Balance_data.fee_payer_balance= Some receiver1_balance
    ; source_balance= None
    ; receiver_balance= receiver2_balance }
end

module Internal_command_balance_data = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        | Coinbase of Coinbase_balance_data.Stable.V1.t
        | Fee_transfer of Fee_transfer_balance_data.Stable.V1.t
      [@@deriving sexp, yojson, equal, compare]

      let to_latest = Fn.id
    end
  end]
end

module Auxiliary_data = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { fee_payer_account_creation_fee_paid:
            Currency.Amount.Stable.V1.t option
        ; receiver_account_creation_fee_paid:
            Currency.Amount.Stable.V1.t option
        ; created_token: Token_id.Stable.V1.t option }
      [@@deriving sexp, yojson, equal, compare]

      let to_latest = Fn.id
    end
  end]

  let empty =
    { fee_payer_account_creation_fee_paid= None
    ; receiver_account_creation_fee_paid= None
    ; created_token= None }
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      | Applied of Auxiliary_data.Stable.V1.t * Balance_data.Stable.V1.t
      | Failed of Failure.Stable.V1.t * Balance_data.Stable.V1.t
    [@@deriving sexp, yojson, equal, compare]

    let to_latest = Fn.id
  end
end]

let balance_data = function
  | Applied (_, balances) | Failed (_, balances) ->
      balances
