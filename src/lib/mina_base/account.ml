(* account.ml *)

[%%import "/src/config.mlh"]

open Core_kernel

[%%ifdef consensus_mechanism]

open Snark_params
open Tick

[%%else]

module Currency = Currency_nonconsensus.Currency
module Mina_numbers = Mina_numbers_nonconsensus.Mina_numbers
module Random_oracle = Random_oracle_nonconsensus.Random_oracle
module Mina_compile_config =
  Mina_compile_config_nonconsensus.Mina_compile_config

[%%endif]

open Currency
open Mina_numbers
open Fold_lib
open Import

module Index = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = int [@@deriving to_yojson, sexp]

      let to_latest = Fn.id
    end
  end]

  let to_int = Int.to_int

  let gen ~ledger_depth = Int.gen_incl 0 ((1 lsl ledger_depth) - 1)

  module Table = Int.Table

  module Vector = struct
    include Int

    let empty = zero

    let get t i = (t lsr i) land 1 = 1

    let set v i b = if b then v lor (one lsl i) else v land lnot (one lsl i)
  end

  let to_bits ~ledger_depth t = List.init ledger_depth ~f:(Vector.get t)

  let of_bits = List.foldi ~init:Vector.empty ~f:(fun i t b -> Vector.set t i b)

  let fold_bits ~ledger_depth t =
    { Fold.fold =
        (fun ~init ~f ->
          let rec go acc i =
            if i = ledger_depth then acc else go (f acc (Vector.get t i)) (i + 1)
          in
          go init 0)
    }

  let fold ~ledger_depth t =
    Fold.group3 ~default:false (fold_bits ~ledger_depth t)

  [%%ifdef consensus_mechanism]

  module Unpacked = struct
    type var = Tick.Boolean.var list

    type value = Vector.t

    let typ ~ledger_depth : (var, value) Tick.Typ.t =
      Typ.transport
        (Typ.list ~length:ledger_depth Boolean.typ)
        ~there:(to_bits ~ledger_depth) ~back:of_bits
  end

  [%%endif]
end

module Nonce = Account_nonce

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ( 'pk
           , 'tid
           , 'token_permissions
           , 'amount
           , 'nonce
           , 'receipt_chain_hash
           , 'delegate
           , 'state_hash
           , 'timing
           , 'permissions
           , 'snapp_opt )
           t =
        { public_key : 'pk
        ; token_id : 'tid
        ; token_permissions : 'token_permissions
        ; balance : 'amount
        ; nonce : 'nonce
        ; receipt_chain_hash : 'receipt_chain_hash
        ; delegate : 'delegate
        ; voting_for : 'state_hash
        ; timing : 'timing
        ; permissions : 'permissions
        ; snapp : 'snapp_opt
        }
      [@@deriving sexp, equal, compare, hash, yojson, fields, hlist]
    end
  end]
end

module Key = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Public_key.Compressed.Stable.V1.t
      [@@deriving sexp, equal, hash, compare, yojson]

      let to_latest = Fn.id
    end
  end]
end

module Identifier = Account_id

type key = Key.t [@@deriving sexp, equal, hash, compare, yojson]

module Timing = Account_timing

module Binable_arg = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Public_key.Compressed.Stable.V1.t
        , Token_id.Stable.V1.t
        , Token_permissions.Stable.V1.t
        , Balance.Stable.V1.t
        , Nonce.Stable.V1.t
        , Receipt.Chain_hash.Stable.V1.t
        , Public_key.Compressed.Stable.V1.t option
        , State_hash.Stable.V1.t
        , Timing.Stable.V1.t
        , Permissions.Stable.V1.t
        , Snapp_account.Stable.V1.t option )
        (* TODO: Cache the digest of this? *)
        Poly.Stable.V1.t
      [@@deriving sexp, equal, hash, compare, yojson]

      let to_latest = Fn.id

      let public_key (t : t) : key = t.public_key
    end
  end]
end

let check = Fn.id

[%%if not feature_snapps]

let check (t : Binable_arg.t) =
  let t = check t in
  match t.snapp with
  | None ->
      t
  | Some _ ->
      failwith "Snapp accounts not supported"

[%%endif]

[%%if not feature_tokens]

let check (t : Binable_arg.t) =
  let t = check t in
  if Token_id.equal Token_id.default t.token_id then t
  else failwith "Token accounts not supported"

[%%endif]

[%%versioned_binable
module Stable = struct
  module V1 = struct
    type t = Binable_arg.Stable.V1.t
    [@@deriving sexp, equal, hash, compare, yojson]

    include Binable.Of_binable
              (Binable_arg.Stable.V1)
              (struct
                type nonrec t = t

                let to_binable = check

                let of_binable = check
              end)

    let to_latest = Fn.id

    let public_key (t : t) : key = t.public_key
  end
end]

[%%define_locally Stable.Latest.(public_key)]

let token { Poly.token_id; _ } = token_id

let identifier ({ public_key; token_id; _ } : t) =
  Account_id.create public_key token_id

type value =
  ( Public_key.Compressed.t
  , Token_id.t
  , Token_permissions.t
  , Balance.t
  , Nonce.t
  , Receipt.Chain_hash.t
  , Public_key.Compressed.t option
  , State_hash.t
  , Timing.t
  , Permissions.t
  , Snapp_account.t option )
  Poly.t
[@@deriving sexp]

let key_gen = Public_key.Compressed.gen

let initialize account_id : t =
  let public_key = Account_id.public_key account_id in
  let token_id = Account_id.token_id account_id in
  let delegate =
    (* Only allow delegation if this account is for the default token. *)
    if Token_id.(equal default token_id) then Some public_key else None
  in
  { public_key
  ; token_id
  ; token_permissions = Token_permissions.default
  ; balance = Balance.zero
  ; nonce = Nonce.zero
  ; receipt_chain_hash = Receipt.Chain_hash.empty
  ; delegate
  ; voting_for = State_hash.dummy
  ; timing = Timing.Untimed
  ; permissions = Permissions.user_default
  ; snapp = None
  }

let hash_snapp_account_opt = function
  | None ->
      Lazy.force Snapp_account.default_digest
  | Some (a : Snapp_account.t) ->
      Snapp_account.digest a

let delegate_opt = Option.value ~default:Public_key.Compressed.empty

let to_input (t : t) =
  let open Random_oracle.Input in
  let f mk acc field = mk (Core_kernel.Field.get field t) :: acc in
  let bits conv = f (Fn.compose bitstring conv) in
  Poly.Fields.fold ~init:[]
    ~public_key:(f Public_key.Compressed.to_input)
    ~token_id:(f Token_id.to_input) ~balance:(bits Balance.to_bits)
    ~token_permissions:(f Token_permissions.to_input)
    ~nonce:(bits Nonce.Bits.to_bits)
    ~receipt_chain_hash:(f Receipt.Chain_hash.to_input)
    ~delegate:(f (Fn.compose Public_key.Compressed.to_input delegate_opt))
    ~voting_for:(f State_hash.to_input) ~timing:(bits Timing.to_bits)
    ~snapp:(f (Fn.compose field hash_snapp_account_opt))
    ~permissions:(f Permissions.to_input)
  |> List.reduce_exn ~f:append

let crypto_hash_prefix = Hash_prefix.account

let crypto_hash t =
  Random_oracle.hash ~init:crypto_hash_prefix
    (Random_oracle.pack_input (to_input t))

[%%ifdef consensus_mechanism]

type var =
  ( Public_key.Compressed.var
  , Token_id.var
  , Token_permissions.var
  , Balance.var
  , Nonce.Checked.t
  , Receipt.Chain_hash.var
  , Public_key.Compressed.var
  , State_hash.var
  , Timing.var
  , Permissions.Checked.t
  , Field.Var.t * Snapp_account.t option As_prover.Ref.t
  (* TODO: This is a hack that lets us avoid unhashing snapp accounts when we don't need to *)
  )
  Poly.t

let identifier_of_var ({ public_key; token_id; _ } : var) =
  Account_id.Checked.create public_key token_id

let typ' snapp =
  let spec =
    Data_spec.
      [ Public_key.Compressed.typ
      ; Token_id.typ
      ; Token_permissions.typ
      ; Balance.typ
      ; Nonce.typ
      ; Receipt.Chain_hash.typ
      ; Typ.transport Public_key.Compressed.typ ~there:delegate_opt
          ~back:(fun delegate ->
            if Public_key.Compressed.(equal empty) delegate then None
            else Some delegate)
      ; State_hash.typ
      ; Timing.typ
      ; Permissions.typ
      ; snapp
      ]
  in
  Typ.of_hlistable spec ~var_to_hlist:Poly.to_hlist ~var_of_hlist:Poly.of_hlist
    ~value_to_hlist:Poly.to_hlist ~value_of_hlist:Poly.of_hlist

let typ : (var, value) Typ.t =
  let snapp :
      ( Field.Var.t * Snapp_account.t option As_prover.Ref.t
      , Snapp_account.t option )
      Typ.t =
    let account :
        (Snapp_account.t option As_prover.Ref.t, Snapp_account.t option) Typ.t =
      Typ.Internal.ref ()
    in
    let alloc =
      let open Typ.Alloc in
      let%map x = Typ.field.alloc and y = account.alloc in
      (x, y)
    in
    let read (_, y) = account.read y in
    let store y =
      let open Typ.Store in
      let x = hash_snapp_account_opt y in
      let%map x = Typ.field.store x and y = account.store y in
      (x, y)
    in
    let check (x, _) = Typ.field.check x in
    { alloc; read; store; check }
  in
  typ' snapp

let var_of_t
    ({ public_key
     ; token_id
     ; token_permissions
     ; balance
     ; nonce
     ; receipt_chain_hash
     ; delegate
     ; voting_for
     ; timing
     ; permissions
     ; snapp
     } :
      value) =
  { Poly.public_key = Public_key.Compressed.var_of_t public_key
  ; token_id = Token_id.var_of_t token_id
  ; token_permissions = Token_permissions.var_of_t token_permissions
  ; balance = Balance.var_of_t balance
  ; nonce = Nonce.Checked.constant nonce
  ; receipt_chain_hash = Receipt.Chain_hash.var_of_t receipt_chain_hash
  ; delegate = Public_key.Compressed.var_of_t (delegate_opt delegate)
  ; voting_for = State_hash.var_of_t voting_for
  ; timing = Timing.var_of_t timing
  ; permissions = Permissions.Checked.constant permissions
  ; snapp = Field.Var.constant (hash_snapp_account_opt snapp)
  }

module Checked = struct
  module Unhashed = struct
    type t =
      ( Public_key.Compressed.var
      , Token_id.var
      , Token_permissions.var
      , Balance.var
      , Nonce.Checked.t
      , Receipt.Chain_hash.var
      , Public_key.Compressed.var
      , State_hash.var
      , Timing.var
      , Permissions.Checked.t
      , Snapp_account.Checked.t )
      Poly.t

    let typ : (t, Stable.Latest.t) Typ.t =
      typ'
        (Typ.transport Snapp_account.typ
           ~there:(fun t -> Option.value t ~default:Snapp_account.default)
           ~back:(fun t -> Some t))
  end

  let to_input (t : var) =
    let ( ! ) f x = Run.run_checked (f x) in
    let f mk acc field = mk (Core_kernel.Field.get field t) :: acc in
    let open Random_oracle.Input in
    let bits conv =
      f (fun x ->
          bitstring (Bitstring_lib.Bitstring.Lsb_first.to_list (conv x)))
    in
    make_checked (fun () ->
        List.reduce_exn ~f:append
          (Poly.Fields.fold ~init:[]
             ~snapp:(f (fun (x, _) -> field x))
             ~permissions:(f Permissions.Checked.to_input)
             ~public_key:(f Public_key.Compressed.Checked.to_input)
             ~token_id:
               (* We use [run_checked] here to avoid routing the [Checked.t]
                  monad throughout this calculation.
               *)
               (f (fun x -> Run.run_checked (Token_id.Checked.to_input x)))
             ~token_permissions:(f Token_permissions.var_to_input)
             ~balance:(bits Balance.var_to_bits)
             ~nonce:(bits !Nonce.Checked.to_bits)
             ~receipt_chain_hash:(f Receipt.Chain_hash.var_to_input)
             ~delegate:(f Public_key.Compressed.Checked.to_input)
             ~voting_for:(f State_hash.var_to_input)
             ~timing:(bits Timing.var_to_bits)))

  let digest t =
    make_checked (fun () ->
        Random_oracle.Checked.(
          hash ~init:crypto_hash_prefix
            (pack_input (Run.run_checked (to_input t)))))

  let min_balance_at_slot ~global_slot ~cliff_time ~cliff_amount ~vesting_period
      ~vesting_increment ~initial_minimum_balance =
    let%bind before_cliff = Global_slot.Checked.(global_slot < cliff_time) in
    let balance_to_int balance =
      Snarky_integer.Integer.of_bits ~m @@ Balance.var_to_bits balance
    in
    let open Snarky_integer.Integer in
    let initial_minimum_balance_int = balance_to_int initial_minimum_balance in
    make_checked (fun () ->
        if_ ~m before_cliff ~then_:initial_minimum_balance_int
          ~else_:
            (let global_slot_int = Global_slot.Checked.to_integer global_slot in
             let cliff_time_int = Global_slot.Checked.to_integer cliff_time in
             let _, slot_diff =
               subtract_unpacking_or_zero ~m global_slot_int cliff_time_int
             in
             let cliff_decrement_int =
               Amount.var_to_bits cliff_amount |> of_bits ~m
             in
             let _, min_balance_less_cliff_decrement =
               subtract_unpacking_or_zero ~m initial_minimum_balance_int
                 cliff_decrement_int
             in
             let vesting_period_int =
               Global_slot.Checked.to_integer vesting_period
             in
             let num_periods, _ = div_mod ~m slot_diff vesting_period_int in
             let vesting_increment_int =
               Amount.var_to_bits vesting_increment |> of_bits ~m
             in
             let vesting_decrement = mul ~m num_periods vesting_increment_int in
             let _, min_balance_less_cliff_and_vesting_decrements =
               subtract_unpacking_or_zero ~m min_balance_less_cliff_decrement
                 vesting_decrement
             in
             min_balance_less_cliff_and_vesting_decrements))

  let has_locked_tokens ~global_slot (t : var) =
    let open Timing.As_record in
    let { is_timed = _
        ; initial_minimum_balance
        ; cliff_time
        ; cliff_amount
        ; vesting_period
        ; vesting_increment
        } =
      t.timing
    in
    let%bind cur_min_balance =
      min_balance_at_slot ~global_slot ~initial_minimum_balance ~cliff_time
        ~cliff_amount ~vesting_period ~vesting_increment
    in
    let%map zero_min_balance =
      let zero_int =
        Snarky_integer.Integer.constant ~m
          (Bigint.of_field Field.zero |> Bigint.to_bignum_bigint)
      in
      make_checked (fun () ->
          Snarky_integer.Integer.equal ~m cur_min_balance zero_int)
    in
    (*Note: Untimed accounts will always have zero min balance*)
    Boolean.not zero_min_balance
end

[%%endif]

let digest = crypto_hash

let empty =
  { Poly.public_key = Public_key.Compressed.empty
  ; token_id = Token_id.default
  ; token_permissions = Token_permissions.default
  ; balance = Balance.zero
  ; nonce = Nonce.zero
  ; receipt_chain_hash = Receipt.Chain_hash.empty
  ; delegate = None
  ; voting_for = State_hash.dummy
  ; timing = Timing.Untimed
  ; permissions =
      Permissions.user_default
      (* TODO: This should maybe be Permissions.empty *)
  ; snapp = None
  }

let empty_digest = digest empty

let create account_id balance =
  let public_key = Account_id.public_key account_id in
  let token_id = Account_id.token_id account_id in
  let delegate =
    (* Only allow delegation if this account is for the default token. *)
    if Token_id.(equal default) token_id then Some public_key else None
  in
  { Poly.public_key
  ; token_id
  ; token_permissions = Token_permissions.default
  ; balance
  ; nonce = Nonce.zero
  ; receipt_chain_hash = Receipt.Chain_hash.empty
  ; delegate
  ; voting_for = State_hash.dummy
  ; timing = Timing.Untimed
  ; permissions = Permissions.user_default
  ; snapp = None
  }

let create_timed account_id balance ~initial_minimum_balance ~cliff_time
    ~cliff_amount ~vesting_period ~vesting_increment =
  if Global_slot.(equal vesting_period zero) then
    Or_error.errorf
      !"Error creating timed account for account id %{sexp: Account_id.t}: \
        vesting period must be greater than zero"
      account_id
  else
    let public_key = Account_id.public_key account_id in
    let token_id = Account_id.token_id account_id in
    let delegate =
      (* Only allow delegation if this account is for the default token. *)
      if Token_id.(equal default) token_id then Some public_key else None
    in
    Or_error.return
      { Poly.public_key
      ; token_id
      ; token_permissions = Token_permissions.default
      ; balance
      ; nonce = Nonce.zero
      ; receipt_chain_hash = Receipt.Chain_hash.empty
      ; delegate
      ; voting_for = State_hash.dummy
      ; snapp = None
      ; permissions = Permissions.user_default
      ; timing =
          Timing.Timed
            { initial_minimum_balance
            ; cliff_time
            ; cliff_amount
            ; vesting_period
            ; vesting_increment
            }
      }

(* no vesting after cliff time + 1 slot *)
let create_time_locked public_key balance ~initial_minimum_balance ~cliff_time =
  create_timed public_key balance ~initial_minimum_balance ~cliff_time
    ~vesting_period:Global_slot.(succ zero)
    ~vesting_increment:initial_minimum_balance

let min_balance_at_slot ~global_slot ~cliff_time ~cliff_amount ~vesting_period
    ~vesting_increment ~initial_minimum_balance =
  let open Unsigned in
  if Global_slot.(global_slot < cliff_time) then initial_minimum_balance
    (* If vesting period is zero then everything vests immediately at the cliff *)
  else if Global_slot.(equal vesting_period zero) then Balance.zero
  else
    match Balance.(initial_minimum_balance - cliff_amount) with
    | None ->
        Balance.zero
    | Some min_balance_past_cliff -> (
        (* take advantage of fact that global slots are uint32's *)
        let num_periods =
          UInt32.(
            Infix.((global_slot - cliff_time) / vesting_period)
            |> to_int64 |> UInt64.of_int64)
        in
        let vesting_decrement =
          UInt64.Infix.(num_periods * Amount.to_uint64 vesting_increment)
          |> Amount.of_uint64
        in
        match Balance.(min_balance_past_cliff - vesting_decrement) with
        | None ->
            Balance.zero
        | Some amt ->
            amt )

let incremental_balance_between_slots ~start_slot ~end_slot ~cliff_time
    ~cliff_amount ~vesting_period ~vesting_increment ~initial_minimum_balance :
    Unsigned.UInt64.t =
  let open Unsigned in
  let min_balance_at_start_slot =
    min_balance_at_slot ~global_slot:start_slot ~cliff_time ~cliff_amount
      ~vesting_period ~vesting_increment ~initial_minimum_balance
    |> Balance.to_amount |> Amount.to_uint64
  in
  let min_balance_at_end_slot =
    min_balance_at_slot ~global_slot:end_slot ~cliff_time ~cliff_amount
      ~vesting_period ~vesting_increment ~initial_minimum_balance
    |> Balance.to_amount |> Amount.to_uint64
  in
  UInt64.Infix.(min_balance_at_start_slot - min_balance_at_end_slot)

let has_locked_tokens ~global_slot (account : t) =
  match account.timing with
  | Untimed ->
      false
  | Timed
      { initial_minimum_balance
      ; cliff_time
      ; cliff_amount
      ; vesting_period
      ; vesting_increment
      } ->
      let curr_min_balance =
        min_balance_at_slot ~global_slot ~cliff_time ~cliff_amount
          ~vesting_period ~vesting_increment ~initial_minimum_balance
      in
      Balance.(curr_min_balance > zero)

let gen =
  let open Quickcheck.Let_syntax in
  let%bind public_key = Public_key.Compressed.gen in
  let%bind token_id = Token_id.gen in
  let%map balance = Currency.Balance.gen in
  create (Account_id.create public_key token_id) balance

let gen_timed =
  let open Quickcheck.Let_syntax in
  let%bind public_key = Public_key.Compressed.gen in
  let%bind token_id = Token_id.gen in
  let account_id = Account_id.create public_key token_id in
  let%bind balance = Currency.Balance.gen in
  let%bind initial_minimum_balance = Currency.Balance.gen in
  let%bind cliff_time = Global_slot.gen in
  let%bind cliff_amount = Amount.gen in
  (* vesting period must be at least one to avoid division by zero *)
  let%bind vesting_period =
    Int.gen_incl 1 100 >>= Fn.compose return Global_slot.of_int
  in
  let%map vesting_increment = Amount.gen in
  create_timed account_id balance ~initial_minimum_balance ~cliff_time
    ~cliff_amount ~vesting_period ~vesting_increment
