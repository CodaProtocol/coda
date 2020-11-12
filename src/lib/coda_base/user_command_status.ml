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
      [@@deriving sexp, yojson, eq, compare, enum]

      let to_latest = Fn.id
    end
  end]

  type failure = t

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
    for i = min to max do
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
      [@@deriving hlist, eq, sexp, compare]

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
    [@@deriving eq, sexp, compare]

    type t = bool poly [@@deriving eq, sexp, compare]

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
          1
      | {source_not_present= true; _} ->
          2
      | {receiver_not_present= true; _} ->
          3
      | {amount_insufficient_to_create_account= true; _} ->
          4
      | {cannot_pay_creation_fee_in_token= true; _} ->
          5
      | {source_insufficient_balance= true; _} ->
          6
      | {source_minimum_balance_violation= true; _} ->
          7
      | {receiver_already_exists= true; _} ->
          8
      | {not_token_owner= true; _} ->
          9
      | {mismatched_token_permissions= true; _} ->
          10
      | {overflow= true; _} ->
          11
      | {signed_command_on_snapp_account= true; _} ->
          12
      | {snapp_account_not_present= true; _} ->
          13
      | {update_not_permitted= true; _} ->
          14
      | {incorrect_nonce= true; _} ->
          15
      | _ ->
          0

    let of_enum = function
      | 0 ->
          Some none
      | 1 ->
          Some predicate
      | 2 ->
          Some source_not_present
      | 3 ->
          Some receiver_not_present
      | 4 ->
          Some amount_insufficient_to_create_account
      | 5 ->
          Some cannot_pay_creation_fee_in_token
      | 6 ->
          Some source_insufficient_balance
      | 7 ->
          Some source_minimum_balance_violation
      | 8 ->
          Some receiver_already_exists
      | 9 ->
          Some not_token_owner
      | 10 ->
          Some mismatched_token_permissions
      | 11 ->
          Some overflow
      | 12 ->
          Some signed_command_on_snapp_account
      | 13 ->
          Some snapp_account_not_present
      | 14 ->
          Some update_not_permitted
      | 15 ->
          Some incorrect_nonce
      | _ ->
          None

    let min = 0

    let max = 15

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

    val check_failure : t -> failure -> Boolean.var -> (unit, _) Checked.t
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

    let none = of_record @@ As_record.var_of_t As_record.none

    let predicate = of_record @@ As_record.var_of_t As_record.predicate

    let source_not_present =
      of_record @@ As_record.var_of_t As_record.source_not_present

    let receiver_not_present =
      of_record @@ As_record.var_of_t As_record.receiver_not_present

    let amount_insufficient_to_create_account =
      of_record
      @@ As_record.var_of_t As_record.amount_insufficient_to_create_account

    let cannot_pay_creation_fee_in_token =
      of_record
      @@ As_record.var_of_t As_record.cannot_pay_creation_fee_in_token

    let source_insufficient_balance =
      of_record @@ As_record.var_of_t As_record.source_insufficient_balance

    let source_minimum_balance_violation =
      of_record
      @@ As_record.var_of_t As_record.source_minimum_balance_violation

    let receiver_already_exists =
      of_record @@ As_record.var_of_t As_record.receiver_already_exists

    let not_token_owner =
      of_record @@ As_record.var_of_t As_record.not_token_owner

    let mismatched_token_permissions =
      of_record @@ As_record.var_of_t As_record.mismatched_token_permissions

    let overflow = of_record @@ As_record.var_of_t As_record.overflow

    let signed_command_on_snapp_account =
      of_record @@ As_record.var_of_t As_record.signed_command_on_snapp_account

    let snapp_account_not_present =
      of_record @@ As_record.var_of_t As_record.snapp_account_not_present

    let update_not_permitted =
      of_record @@ As_record.var_of_t As_record.update_not_permitted

    let incorrect_nonce =
      of_record @@ As_record.var_of_t As_record.incorrect_nonce

    let get {data; _} failure = As_record.get data failure

    let min = As_record.min

    let max = As_record.max

    let of_enum i =
      Option.map
        ~f:(fun t -> of_record (As_record.var_of_t t))
        (As_record.of_enum i)

    let check_failure t failure (failure_var : Boolean.var) =
      let predicted_failure = (get t failure :> Field.Var.t) in
      let actual_failure = (failure_var :> Field.Var.t) in
      let any_failure = (t.accumulators.user_command_failure :> Field.Var.t) in
      (* We want the constraint to satisfy the following properties:
         * if a failure is predicted, it must be actual
         * if a failure is not predicted but is actual, there must be some
           other failure indicated by any_failure
         We can encode this as a truth table. Note that some combinations are
         impossible because of the construction of our any_failure: we can
         never have a predicted_failure when we have not also set any_failure.

         Encoding this as a truth table, we get:
         let P = predicted_failure
         let A = actual_failure
         let S = any_failure
         P | A | S | May build proof
         --|---|---|----------------
         0 | 0 | 0 | Yes
         0 | 0 | 1 | Yes
         0 | 1 | 0 | No
         0 | 1 | 1 | Yes
         1 | 0 | 0 | Impossible
         1 | 0 | 1 | No
         1 | 1 | 0 | Impossible
         1 | 1 | 1 | Yes

         A candidate constraint takes the form
         (a*P + b*A + c*S) * (d*P + e*A + f*S) = (w*P + x*A + y*S + z)
         We arbitrarily set a = b = c = d = e = f = 1.
         Substituting in the valid combinations, we get
         P = 0, A = 0, S = 0 => 0 * 0 = z             => z = 0
         P = 0, A = 0, S = 1 => 1 * 1 = y + z         => y = 1 (= 1 - 0)
         P = 0, A = 1, S = 1 => 2 * 2 = x + y + z     => x = 3 (= 4 - 1)
         P = 1, A = 1, S = 1 => 3 * 3 = w + x + y + z => w = 5 (= 9 - 4)

         Checking the invalid combinations, we get
         P = 0, A = 1, S = 0 => 1 * 1 ?= x + z = 3         (1 != 3)
         P = 1, A = 0, S = 1 => 2 * 2 ?= w + y + z = 6     (4 != 6)

         Thus, the following constraint encodes our requirements:
         (P + A + S) * (P + A + S) = (5*P + 3*A + 1*S)
      *)
      let open Field.Var in
      let lhs = sum [predicted_failure; actual_failure; any_failure] in
      let rhs =
        sum
          [ scale predicted_failure (Field.of_int 5)
          ; scale actual_failure (Field.of_int 3)
          ; any_failure ]
      in
      assert_square lhs rhs
  end

  let to_record t =
    match As_record.of_enum (to_enum t) with
    | Some t ->
        t
    | None ->
        failwith
          "Internal error: Could not convert User_command.Status.Failure.t to \
           User_command_status.Failure.As_record.t"

  let to_record_opt t =
    match t with None -> As_record.none | Some t -> to_record t

  let of_record_opt t = of_enum (As_record.to_enum t)

  let%test_unit "Minimum bound matches" =
    (* NB: +1 is for the [user_command_failure] accumulator. *)
    [%test_eq: int] min (As_record.min + 1)

  let%test_unit "Maximum bound matches" = [%test_eq: int] max As_record.max

  let%test_unit "of_record_opt(to_record) roundtrip" =
    for i = min to max do
      let failure = Option.value_exn (of_enum i) in
      [%test_eq: t option] (of_record_opt (to_record failure)) (Some failure)
    done

  let%test_unit "to_record_opt(of_record_opt) roundtrip" =
    for i = As_record.min to As_record.max do
      let record = Option.value_exn (As_record.of_enum i) in
      [%test_eq: As_record.t] (to_record_opt (of_record_opt record)) record
    done

  let%test_unit "As_record.get is consistent" =
    for i = min to max do
      let failure = Option.value_exn (of_enum i) in
      let record = to_record failure in
      for j = min to max do
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

  let%test_module "Var.check_failure tests" =
    ( module struct
      let check var failure boolean =
        Fn.flip run_and_check ()
        @@ Checked.map ~f:As_prover.return
        @@ Var.check_failure var failure boolean

      let%test_unit "Var.check_failure rejects failures when it is none" =
        for i = min to max do
          let failure = Option.value_exn (of_enum i) in
          (* Failures are not allowed. *)
          ( match check Var.none failure Boolean.true_ with
          | Ok _ ->
              failwithf !"check_failure none %{sexp: t} true = Ok" failure ()
          | Error _ ->
              () ) ;
          (* Succeeds when no failure. *)
          match check Var.none failure Boolean.false_ with
          | Error _ ->
              failwithf
                !"check_failure none %{sexp: t} false = Error"
                failure ()
          | Ok _ ->
              ()
        done

      let%test_unit "Var.check_failure accepts any failure when it describes \
                     a failure" =
        for i = min to max do
          let failure = Option.value_exn (of_enum i) in
          let var = var_of_t failure in
          for j = min to max do
            let failure_to_check = Option.value_exn (of_enum j) in
            match check var failure_to_check Boolean.true_ with
            | Error _ ->
                failwithf
                  !"check_failure %{sexp: t} %{sexp: t} true = Error"
                  failure failure_to_check ()
            | Ok _ ->
                ()
          done
        done

      let%test_unit "Var.check_failure requires only the failure that it \
                     describes" =
        for i = min to max do
          let failure = Option.value_exn (of_enum i) in
          let var = var_of_t failure in
          for j = min to max do
            let failure_to_check = Option.value_exn (of_enum j) in
            match check var failure_to_check Boolean.false_ with
            | Ok _ ->
                if equal failure failure_to_check then
                  failwithf
                    !"check_failure %{sexp: t} %{sexp: t} true = Ok"
                    failure failure_to_check ()
            | Error _ ->
                if not (equal failure failure_to_check) then
                  failwithf
                    !"check_failure %{sexp: t} %{sexp: t} true = Error"
                    failure failure_to_check ()
          done
        done
    end )

  [%%endif]
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
      [@@deriving sexp, yojson, eq, compare]

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
      | Applied of Auxiliary_data.Stable.V1.t
      | Failed of Failure.Stable.V1.t
    [@@deriving sexp, yojson, eq, compare]

    let to_latest = Fn.id
  end
end]
