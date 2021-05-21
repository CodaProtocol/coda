[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick
open Signature_lib
module Mina_numbers = Mina_numbers

[%%else]

open Signature_lib_nonconsensus
module Mina_numbers = Mina_numbers_nonconsensus.Mina_numbers
module Currency = Currency_nonconsensus.Currency
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module A = Account
open Mina_numbers
open Currency
open Snapp_basic
open Pickles_types
module Impl = Pickles.Impls.Step

module Closed_interval = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = {lower: 'a; upper: 'a}
      [@@deriving sexp, eq, compare, hash, yojson, hlist]
    end
  end]

  let to_input {lower; upper} ~f =
    Random_oracle_input.append (f lower) (f upper)

  let typ x =
    Typ.of_hlistable [x; x] ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
      ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

  let map {lower; upper} ~f = {lower= f lower; upper= f upper}
end

let assert_ b e = if b then Ok () else Or_error.error_string e

(* Proofs are produced against a predicate on the protocol state. For the
   transaction to go through, the predicate must be satisfied of the protocol
   state at the time of transaction application. *)
module Numeric = struct
  module Tc = struct
    type ('var, 'a) t =
      { zero: 'a
      ; max_value: 'a
      ; compare: 'a -> 'a -> int
      ; equal: 'a -> 'a -> bool
      ; typ: ('var, 'a) Typ.t
      ; to_input: 'a -> (F.t, bool) Random_oracle_input.t
      ; to_input_checked:
          'var -> (Field.Var.t, Boolean.var) Random_oracle_input.t
      ; lte_checked: 'var -> 'var -> Boolean.var }

    let run f x y = Impl.run_checked (f x y)

    let length =
      Length.
        { zero
        ; max_value
        ; compare
        ; lte_checked= run Checked.( <= )
        ; equal
        ; typ
        ; to_input
        ; to_input_checked= Fn.compose Impl.run_checked Checked.to_input }

    let amount =
      Currency.Amount.
        { zero
        ; max_value= max_int
        ; compare
        ; lte_checked= run Checked.( <= )
        ; equal
        ; typ
        ; to_input
        ; to_input_checked= var_to_input }

    let balance =
      Currency.Balance.
        { zero
        ; max_value= max_int
        ; compare
        ; lte_checked= run Checked.( <= )
        ; equal
        ; typ
        ; to_input
        ; to_input_checked= var_to_input }

    let nonce =
      Account_nonce.
        { zero
        ; max_value
        ; compare
        ; lte_checked= run Checked.( <= )
        ; equal
        ; typ
        ; to_input
        ; to_input_checked= Fn.compose Impl.run_checked Checked.to_input }

    let global_slot =
      Global_slot.
        { zero
        ; max_value
        ; compare
        ; lte_checked= run Checked.( <= )
        ; equal
        ; typ
        ; to_input
        ; to_input_checked= Fn.compose Impl.run_checked Checked.to_input }

    let token_id =
      Token_id.
        { zero= of_uint64 Unsigned.UInt64.zero
        ; max_value= of_uint64 Unsigned.UInt64.max_int
        ; equal
        ; compare
        ; lte_checked= run Checked.( <= )
        ; typ
        ; to_input
        ; to_input_checked= Fn.compose Impl.run_checked Checked.to_input }

    let time =
      Block_time.
        { equal
        ; compare
        ; lte_checked= run Checked.( <= )
        ; zero
        ; max_value
        ; typ= Unpacked.typ
        ; to_input= Fn.compose Random_oracle_input.bitstring Bits.to_bits
        ; to_input_checked=
            (fun x ->
              Random_oracle_input.bitstring
                (Unpacked.var_to_bits x :> Boolean.var list) ) }
  end

  open Tc

  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = 'a Closed_interval.Stable.V1.t Or_ignore.Stable.V1.t
      [@@deriving sexp, eq, yojson, hash, compare]
    end
  end]

  let to_input {zero; max_value; to_input; _} (t : 'a t) =
    Closed_interval.to_input ~f:to_input
      (match t with Check x -> x | Ignore -> {lower= zero; upper= max_value})

  module Checked = struct
    type 'a t = 'a Closed_interval.t Or_ignore.Checked.t

    let to_input {to_input_checked; _} (t : 'a t) =
      Or_ignore.Checked.to_input ~explicit:false t
        ~f:(Closed_interval.to_input ~f:to_input_checked)

    open Impl

    let check {lte_checked= ( <= ); _} (t : 'a t) (x : 'a) =
      Or_ignore.Checked.check t ~f:(fun {lower; upper} ->
          Boolean.all [lower <= x; x <= upper] )
  end

  let typ {equal= eq; zero; max_value; typ; _} =
    Or_ignore.typ_implicit (Closed_interval.typ typ)
      ~equal:(Closed_interval.equal eq)
      ~ignore:{Closed_interval.lower= zero; upper= max_value}

  let check ~label {compare; _} (t : 'a t) (x : 'a) =
    match t with
    | Ignore ->
        Ok ()
    | Check {lower; upper} ->
        if compare lower x <= 0 && compare x upper <= 0 then Ok ()
        else Or_error.errorf "Bounds check failed: %s" label
end

module Eq_data = struct
  include Or_ignore

  module Tc = struct
    type ('var, 'a) t =
      { equal: 'a -> 'a -> bool
      ; equal_checked: 'var -> 'var -> Boolean.var
      ; default: 'a
      ; typ: ('var, 'a) Typ.t
      ; to_input: 'a -> (F.t, bool) Random_oracle_input.t
      ; to_input_checked:
          'var -> (Field.Var.t, Boolean.var) Random_oracle_input.t }

    let run f x y = Impl.run_checked (f x y)

    let field =
      let open Random_oracle_input in
      Field.
        { typ
        ; equal
        ; equal_checked= run Checked.equal
        ; default= zero
        ; to_input= field
        ; to_input_checked= field }

    let receipt_chain_hash =
      Receipt.Chain_hash.
        { field with
          to_input_checked= var_to_input
        ; typ
        ; equal
        ; equal_checked= run equal_var }

    let ledger_hash =
      Ledger_hash.
        { field with
          to_input_checked= var_to_input
        ; typ
        ; equal
        ; equal_checked= run equal_var }

    let frozen_ledger_hash =
      Frozen_ledger_hash.
        { field with
          to_input_checked= var_to_input
        ; typ
        ; equal
        ; equal_checked= run equal_var }

    let state_hash =
      State_hash.
        { field with
          to_input_checked= var_to_input
        ; typ
        ; equal
        ; equal_checked= run equal_var }

    let epoch_seed =
      Epoch_seed.
        { field with
          to_input_checked= var_to_input
        ; typ
        ; equal
        ; equal_checked= run equal_var }

    let public_key () =
      Public_key.Compressed.
        { default= Lazy.force invalid_public_key
        ; to_input
        ; to_input_checked= Checked.to_input
        ; equal_checked= run Checked.equal
        ; typ
        ; equal }
  end

  let to_input ~explicit {Tc.default; to_input; _} (t : _ t) =
    if explicit then
      Flagged_option.to_input' ~f:to_input
        ( match t with
        | Ignore ->
            {is_some= false; data= default}
        | Check data ->
            {is_some= true; data} )
    else to_input (match t with Ignore -> default | Check x -> x)

  let to_input_explicit tc = to_input ~explicit:true tc

  let to_input_checked ~explicit {Tc.to_input_checked; _} (t : _ Checked.t) =
    Checked.to_input ~explicit t ~f:to_input_checked

  let check_checked {Tc.equal_checked; _} (t : 'a Checked.t) (x : 'a) =
    Checked.check t ~f:(equal_checked x)

  let check ~label {Tc.equal; _} (t : 'a t) (x : 'a) =
    match t with
    | Ignore ->
        Ok ()
    | Check y ->
        if equal x y then Ok ()
        else Or_error.errorf "Equality check failed: %s" label

  let typ_implicit {Tc.equal; default= ignore; typ; _} =
    typ_implicit ~equal ~ignore typ

  let typ_explicit {Tc.default= ignore; typ; _} = typ_explicit ~ignore typ
end

module Hash = struct
  include Eq_data

  let to_input tc = to_input ~explicit:true tc

  let to_input_checked tc = to_input_checked ~explicit:true tc

  let typ = typ_explicit
end

module Leaf_typs = struct
  let public_key () =
    Public_key.Compressed.(
      Or_ignore.typ_explicit ~ignore:(Lazy.force invalid_public_key) typ)

  open Eq_data.Tc

  let field = Eq_data.typ_explicit field

  let receipt_chain_hash = Hash.typ receipt_chain_hash

  let ledger_hash = Hash.typ ledger_hash

  let frozen_ledger_hash = Hash.typ frozen_ledger_hash

  let state_hash = Hash.typ state_hash

  open Numeric.Tc

  let length = Numeric.typ length

  let time = Numeric.typ time

  let amount = Numeric.typ amount

  let balance = Numeric.typ balance

  let nonce = Numeric.typ nonce

  let global_slot = Numeric.typ global_slot

  let token_id = Numeric.typ token_id
end

module Account = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('balance, 'nonce, 'receipt_chain_hash, 'pk, 'field) t =
          { balance: 'balance
          ; nonce: 'nonce
          ; receipt_chain_hash: 'receipt_chain_hash
          ; public_key: 'pk
          ; delegate: 'pk
          ; state: 'field Snapp_state.V.Stable.V1.t }
        [@@deriving hlist, sexp, eq, yojson, hash, compare]
      end
    end]
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Balance.Stable.V1.t Numeric.Stable.V1.t
        , Account_nonce.Stable.V1.t Numeric.Stable.V1.t
        , Receipt.Chain_hash.Stable.V1.t Hash.Stable.V1.t
        , Public_key.Compressed.Stable.V1.t Eq_data.Stable.V1.t
        , F.Stable.V1.t Eq_data.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, eq, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  let accept : t =
    { balance= Ignore
    ; nonce= Ignore
    ; receipt_chain_hash= Ignore
    ; public_key= Ignore
    ; delegate= Ignore
    ; state=
        Vector.init Snapp_state.Max_state_size.n ~f:(fun _ -> Or_ignore.Ignore)
    }

  let to_input
      ({balance; nonce; receipt_chain_hash; public_key; delegate; state} : t) =
    let open Random_oracle_input in
    List.reduce_exn ~f:append
      [ Numeric.(to_input Tc.balance balance)
      ; Numeric.(to_input Tc.nonce nonce)
      ; Hash.(to_input Tc.receipt_chain_hash receipt_chain_hash)
      ; Eq_data.(to_input_explicit (Tc.public_key ()) public_key)
      ; Eq_data.(to_input_explicit (Tc.public_key ()) delegate)
      ; Vector.reduce_exn ~f:append
          (Vector.map state ~f:Eq_data.(to_input_explicit Tc.field)) ]

  let digest t =
    Random_oracle.(
      hash ~init:Hash_prefix.snapp_predicate_account (pack_input (to_input t)))

  module Checked = struct
    type t =
      ( Balance.var Numeric.Checked.t
      , Account_nonce.Checked.t Numeric.Checked.t
      , Receipt.Chain_hash.var Hash.Checked.t
      , Public_key.Compressed.var Eq_data.Checked.t
      , Field.Var.t Eq_data.Checked.t )
      Poly.Stable.Latest.t

    let to_input
        ({balance; nonce; receipt_chain_hash; public_key; delegate; state} : t)
        =
      let open Random_oracle_input in
      List.reduce_exn ~f:append
        [ Numeric.(Checked.to_input Tc.balance balance)
        ; Numeric.(Checked.to_input Tc.nonce nonce)
        ; Hash.(to_input_checked Tc.receipt_chain_hash receipt_chain_hash)
        ; Eq_data.(
            to_input_checked ~explicit:true (Tc.public_key ()) public_key)
        ; Eq_data.(to_input_checked ~explicit:true (Tc.public_key ()) delegate)
        ; Vector.reduce_exn ~f:append
            (Vector.map state
               ~f:Eq_data.(to_input_checked ~explicit:true Tc.field)) ]

    open Impl

    let check_nonsnapp
        ({balance; nonce; receipt_chain_hash; public_key; delegate; state= _} :
          t) (a : Account.Checked.Unhashed.t) =
      Boolean.all
        [ Numeric.(Checked.check Tc.balance balance a.balance)
        ; Numeric.(Checked.check Tc.nonce nonce a.nonce)
        ; Eq_data.(
            check_checked Tc.receipt_chain_hash receipt_chain_hash
              a.receipt_chain_hash)
        ; Eq_data.(check_checked (Tc.public_key ()) delegate a.delegate)
        ; Eq_data.(check_checked (Tc.public_key ()) public_key a.public_key) ]

    let check_snapp
        ({ balance= _
         ; nonce= _
         ; receipt_chain_hash= _
         ; public_key= _
         ; delegate= _
         ; state } :
          t) (snapp : Snapp_account.Checked.t) =
      Boolean.all
        Vector.(
          to_list
            (map2 state snapp.app_state ~f:Eq_data.(check_checked Tc.field)))

    let digest (t : t) =
      Random_oracle.Checked.(
        hash ~init:Hash_prefix.snapp_predicate_account
          (pack_input (to_input t)))
  end

  let typ () : (Checked.t, Stable.Latest.t) Typ.t =
    let open Poly.Stable.Latest in
    let open Leaf_typs in
    Typ.of_hlistable
      [ balance
      ; nonce
      ; receipt_chain_hash
      ; public_key ()
      ; public_key ()
      ; Snapp_state.typ (Or_ignore.typ_explicit Field.typ ~ignore:Field.zero)
      ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  let check
      ({balance; nonce; receipt_chain_hash; public_key; delegate; state} : t)
      (a : Account.t) =
    let open Or_error.Let_syntax in
    let%bind () =
      Numeric.(check ~label:"balance" Tc.balance balance a.balance)
    in
    let%bind () = Numeric.(check ~label:"nonce" Tc.nonce nonce a.nonce) in
    let%bind () =
      Eq_data.(
        check ~label:"receipt_chain_hash" Tc.receipt_chain_hash
          receipt_chain_hash a.receipt_chain_hash)
    in
    let%bind () =
      let tc = Eq_data.Tc.public_key () in
      Eq_data.(
        check ~label:"delegate" tc delegate
          (Option.value ~default:tc.default a.delegate))
    in
    let%bind () =
      Eq_data.(
        check ~label:"public_key" (Tc.public_key ()) public_key a.public_key)
    in
    let%bind () =
      match a.snapp with
      | None ->
          return ()
      | Some snapp ->
          List.fold_result ~init:0
            Vector.(to_list (zip state snapp.app_state))
            ~f:(fun i (c, v) ->
              let%map () =
                Eq_data.(check Tc.field ~label:(sprintf "state[%d]" i) c v)
              in
              i + 1 )
          >>| ignore
    in
    return ()
end

module Protocol_state = struct
  (* On each numeric field, you may assert a range
      On each hash field, you may assert an equality
    *)

  module Epoch_data = struct
    module Poly = Epoch_data.Poly

    [%%versioned
    module Stable = struct
      module V1 = struct
        (* TODO: Not sure if this should be frozen ledger hash or not *)
        type t =
          ( ( Frozen_ledger_hash.Stable.V1.t Hash.Stable.V1.t
            , Currency.Amount.Stable.V1.t Numeric.Stable.V1.t )
            Epoch_ledger.Poly.Stable.V1.t
          , Epoch_seed.Stable.V1.t Hash.Stable.V1.t
          , State_hash.Stable.V1.t Hash.Stable.V1.t
          , State_hash.Stable.V1.t Hash.Stable.V1.t
          , Length.Stable.V1.t Numeric.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving sexp, eq, yojson, hash, compare]

        let to_latest = Fn.id
      end
    end]

    let to_input
        ({ ledger= {hash; total_currency}
         ; seed
         ; start_checkpoint
         ; lock_checkpoint
         ; epoch_length } :
          t) =
      let open Random_oracle.Input in
      List.reduce_exn ~f:append
        [ Hash.(to_input Tc.frozen_ledger_hash hash)
        ; Numeric.(to_input Tc.amount total_currency)
        ; Hash.(to_input Tc.epoch_seed seed)
        ; Hash.(to_input Tc.state_hash start_checkpoint)
        ; Hash.(to_input Tc.state_hash lock_checkpoint)
        ; Numeric.(to_input Tc.length epoch_length) ]

    module Checked = struct
      type t =
        ( ( Frozen_ledger_hash.var Hash.Checked.t
          , Currency.Amount.var Numeric.Checked.t )
          Epoch_ledger.Poly.t
        , Epoch_seed.var Hash.Checked.t
        , State_hash.var Hash.Checked.t
        , State_hash.var Hash.Checked.t
        , Length.Checked.t Numeric.Checked.t )
        Poly.t

      let to_input
          ({ ledger= {hash; total_currency}
           ; seed
           ; start_checkpoint
           ; lock_checkpoint
           ; epoch_length } :
            t) =
        let open Random_oracle.Input in
        List.reduce_exn ~f:append
          [ Hash.(to_input_checked Tc.frozen_ledger_hash hash)
          ; Numeric.(Checked.to_input Tc.amount total_currency)
          ; Hash.(to_input_checked Tc.epoch_seed seed)
          ; Hash.(to_input_checked Tc.state_hash start_checkpoint)
          ; Hash.(to_input_checked Tc.state_hash lock_checkpoint)
          ; Numeric.(Checked.to_input Tc.length epoch_length) ]
    end
  end

  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ( 'snarked_ledger_hash
             , 'token_id
             , 'time
             , 'length
             , 'vrf_output
             , 'global_slot
             , 'amount
             , 'epoch_data )
             t =
          { (* TODO: 
             We should include staged ledger hash again! It only changes once per
             block. *)
            snarked_ledger_hash: 'snarked_ledger_hash
          ; snarked_next_available_token: 'token_id
          ; timestamp: 'time
          ; blockchain_length: 'length
                (* TODO: This previously had epoch_count but I removed it as I believe it is redundant
   with curr_global_slot.

   epoch_count in [a, b]

   should be equivalent to

   curr_global_slot in [slots_per_epoch * a, slots_per_epoch * b]
*)
          ; min_window_density: 'length
          ; last_vrf_output: 'vrf_output
          ; total_currency: 'amount
          ; curr_global_slot: 'global_slot
          ; global_slot_since_genesis: 'global_slot
          ; staking_epoch_data: 'epoch_data
          ; next_epoch_data: 'epoch_data }
        [@@deriving hlist, sexp, eq, yojson, hash, compare, fields]
      end
    end]
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Frozen_ledger_hash.Stable.V1.t Hash.Stable.V1.t
        , Token_id.Stable.V1.t Numeric.Stable.V1.t
        , Block_time.Stable.V1.t Numeric.Stable.V1.t
        , Length.Stable.V1.t Numeric.Stable.V1.t
        , unit (* TODO *)
        , Global_slot.Stable.V1.t Numeric.Stable.V1.t
        , Currency.Amount.Stable.V1.t Numeric.Stable.V1.t
        , Epoch_data.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, eq, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  let to_input
      ({ snarked_ledger_hash
       ; snarked_next_available_token
       ; timestamp
       ; blockchain_length
       ; min_window_density
       ; last_vrf_output
       ; total_currency
       ; curr_global_slot
       ; global_slot_since_genesis
       ; staking_epoch_data
       ; next_epoch_data } :
        t) =
    let open Random_oracle.Input in
    let () = last_vrf_output in
    let length = Numeric.(to_input Tc.length) in
    List.reduce_exn ~f:append
      [ Hash.(to_input Tc.field snarked_ledger_hash)
      ; Numeric.(to_input Tc.token_id snarked_next_available_token)
      ; Numeric.(to_input Tc.time timestamp)
      ; length blockchain_length
      ; length min_window_density
      ; Numeric.(to_input Tc.amount total_currency)
      ; Numeric.(to_input Tc.global_slot curr_global_slot)
      ; Numeric.(to_input Tc.global_slot global_slot_since_genesis)
      ; Epoch_data.to_input staking_epoch_data
      ; Epoch_data.to_input next_epoch_data ]

  let digest t =
    Random_oracle.(
      hash ~init:Hash_prefix.snapp_predicate_protocol_state
        (pack_input (to_input t)))

  module View = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          ( Frozen_ledger_hash.Stable.V1.t
          , Token_id.Stable.V1.t
          , Block_time.Stable.V1.t
          , Length.Stable.V1.t
          , unit (* TODO *)
          , Global_slot.Stable.V1.t
          , Currency.Amount.Stable.V1.t
          , ( ( Frozen_ledger_hash.Stable.V1.t
              , Currency.Amount.Stable.V1.t )
              Epoch_ledger.Poly.Stable.V1.t
            , Epoch_seed.Stable.V1.t
            , State_hash.Stable.V1.t
            , State_hash.Stable.V1.t
            , Length.Stable.V1.t )
            Epoch_data.Poly.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving sexp, eq, yojson, hash, compare]

        let to_latest = Fn.id
      end
    end]

    module Checked = struct
      type t =
        ( Frozen_ledger_hash.var
        , Token_id.var
        , Block_time.Unpacked.var
        , Length.Checked.t
        , unit (* TODO *)
        , Global_slot.Checked.t
        , Currency.Amount.var
        , ( (Frozen_ledger_hash.var, Currency.Amount.var) Epoch_ledger.Poly.t
          , Epoch_seed.var
          , State_hash.var
          , State_hash.var
          , Length.Checked.t )
          Epoch_data.Poly.t )
        Poly.t
    end
  end

  module Checked = struct
    type t =
      ( Frozen_ledger_hash.var Hash.Checked.t
      , Token_id.var Numeric.Checked.t
      , Block_time.Unpacked.var Numeric.Checked.t
      , Length.Checked.t Numeric.Checked.t
      , unit (* TODO *)
      , Global_slot.Checked.t Numeric.Checked.t
      , Currency.Amount.var Numeric.Checked.t
      , Epoch_data.Checked.t )
      Poly.Stable.Latest.t

    let to_input
        ({ snarked_ledger_hash
         ; snarked_next_available_token
         ; timestamp
         ; blockchain_length
         ; min_window_density
         ; last_vrf_output
         ; total_currency
         ; curr_global_slot
         ; global_slot_since_genesis
         ; staking_epoch_data
         ; next_epoch_data } :
          t) =
      let open Random_oracle.Input in
      let () = last_vrf_output in
      let length = Numeric.(Checked.to_input Tc.length) in
      List.reduce_exn ~f:append
        [ Hash.(to_input_checked Tc.frozen_ledger_hash snarked_ledger_hash)
        ; Numeric.(Checked.to_input Tc.token_id snarked_next_available_token)
        ; Numeric.(Checked.to_input Tc.time timestamp)
        ; length blockchain_length
        ; length min_window_density
        ; Numeric.(Checked.to_input Tc.amount total_currency)
        ; Numeric.(Checked.to_input Tc.global_slot curr_global_slot)
        ; Numeric.(Checked.to_input Tc.global_slot global_slot_since_genesis)
        ; Epoch_data.Checked.to_input staking_epoch_data
        ; Epoch_data.Checked.to_input next_epoch_data ]

    let digest t =
      Random_oracle.Checked.(
        hash ~init:Hash_prefix.snapp_predicate_protocol_state
          (pack_input (to_input t)))

    let check
        (* Bind all the fields explicity so we make sure they are all used. *)
        ({ snarked_ledger_hash
         ; snarked_next_available_token
         ; timestamp
         ; blockchain_length
         ; min_window_density
         ; last_vrf_output
         ; total_currency
         ; curr_global_slot
         ; global_slot_since_genesis
         ; staking_epoch_data
         ; next_epoch_data } :
          t) (s : View.Checked.t) =
      let open Impl in
      let epoch_ledger ({hash; total_currency} : _ Epoch_ledger.Poly.t)
          (t : Epoch_ledger.var) =
        [ Hash.(check_checked Tc.frozen_ledger_hash) hash t.hash
        ; Numeric.(Checked.check Tc.amount) total_currency t.total_currency ]
      in
      let epoch_data
          ({ledger; seed; start_checkpoint; lock_checkpoint; epoch_length} :
            _ Epoch_data.Poly.t) (t : _ Epoch_data.Poly.t) =
        ignore seed ;
        epoch_ledger ledger t.ledger
        @ [ Hash.(check_checked Tc.state_hash)
              start_checkpoint t.start_checkpoint
          ; Hash.(check_checked Tc.state_hash)
              lock_checkpoint t.lock_checkpoint
          ; Numeric.(Checked.check Tc.length) epoch_length t.epoch_length ]
      in
      ignore last_vrf_output ;
      Boolean.all
        ( [ Hash.(check_checked Tc.ledger_hash)
              snarked_ledger_hash s.snarked_ledger_hash
          ; Numeric.(Checked.check Tc.token_id)
              snarked_next_available_token s.snarked_next_available_token
          ; Numeric.(Checked.check Tc.time) timestamp s.timestamp
          ; Numeric.(Checked.check Tc.length)
              blockchain_length s.blockchain_length
          ; Numeric.(Checked.check Tc.length)
              min_window_density s.min_window_density
          ; Numeric.(Checked.check Tc.amount) total_currency s.total_currency
          ; Numeric.(Checked.check Tc.global_slot)
              curr_global_slot s.curr_global_slot
          ; Numeric.(Checked.check Tc.global_slot)
              global_slot_since_genesis s.global_slot_since_genesis ]
        @ epoch_data staking_epoch_data s.staking_epoch_data
        @ epoch_data next_epoch_data s.next_epoch_data )
  end

  let typ : (Checked.t, Stable.Latest.t) Typ.t =
    let open Poly.Stable.Latest in
    let frozen_ledger_hash = Hash.(typ Tc.frozen_ledger_hash) in
    let state_hash = Hash.(typ Tc.state_hash) in
    let epoch_seed = Hash.(typ Tc.epoch_seed) in
    let length = Numeric.(typ Tc.length) in
    let time = Numeric.(typ Tc.time) in
    let amount = Numeric.(typ Tc.amount) in
    let global_slot = Numeric.(typ Tc.global_slot) in
    let token_id = Numeric.(typ Tc.token_id) in
    let epoch_data =
      let epoch_ledger =
        let open Epoch_ledger.Poly in
        Typ.of_hlistable
          [frozen_ledger_hash; amount]
          ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
          ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
      in
      let open Epoch_data.Poly in
      Typ.of_hlistable
        [epoch_ledger; epoch_seed; state_hash; state_hash; length]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
    in
    Typ.of_hlistable
      [ frozen_ledger_hash
      ; token_id
      ; time
      ; length
      ; length
      ; Typ.unit
      ; amount
      ; global_slot
      ; global_slot
      ; epoch_data
      ; epoch_data ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  let accept : t =
    let epoch_data : Epoch_data.t =
      { ledger= {hash= Ignore; total_currency= Ignore}
      ; seed= Ignore
      ; start_checkpoint= Ignore
      ; lock_checkpoint= Ignore
      ; epoch_length= Ignore }
    in
    { snarked_ledger_hash= Ignore
    ; snarked_next_available_token= Ignore
    ; timestamp= Ignore
    ; blockchain_length= Ignore
    ; min_window_density= Ignore
    ; last_vrf_output= ()
    ; total_currency= Ignore
    ; curr_global_slot= Ignore
    ; global_slot_since_genesis= Ignore
    ; staking_epoch_data= epoch_data
    ; next_epoch_data= epoch_data }

  let check
      (* Bind all the fields explicity so we make sure they are all used. *)
      ({ snarked_ledger_hash
       ; snarked_next_available_token
       ; timestamp
       ; blockchain_length
       ; min_window_density
       ; last_vrf_output
       ; total_currency
       ; curr_global_slot
       ; global_slot_since_genesis
       ; staking_epoch_data
       ; next_epoch_data } :
        t) (s : View.t) =
    let open Or_error.Let_syntax in
    let epoch_ledger ({hash; total_currency} : _ Epoch_ledger.Poly.t)
        (t : Epoch_ledger.Value.t) =
      let%bind () =
        Hash.(check ~label:"epoch_ledger_hash" Tc.frozen_ledger_hash)
          hash t.hash
      in
      let%map () =
        Numeric.(check ~label:"epoch_ledger_total_currency" Tc.amount)
          total_currency t.total_currency
      in
      ()
    in
    let epoch_data label
        ({ledger; seed; start_checkpoint; lock_checkpoint; epoch_length} :
          _ Epoch_data.Poly.t) (t : _ Epoch_data.Poly.t) =
      let l s = sprintf "%s_%s" label s in
      let%bind () = epoch_ledger ledger t.ledger in
      ignore seed ;
      let%bind () =
        Hash.(check ~label:(l "start_check_point") Tc.state_hash)
          start_checkpoint t.start_checkpoint
      in
      let%bind () =
        Hash.(check ~label:(l "lock_check_point") Tc.state_hash)
          lock_checkpoint t.lock_checkpoint
      in
      let%map () =
        Numeric.(check ~label:"epoch_length" Tc.length)
          epoch_length t.epoch_length
      in
      ()
    in
    let%bind () =
      Hash.(check ~label:"snarked_ledger_hash" Tc.ledger_hash)
        snarked_ledger_hash s.snarked_ledger_hash
    in
    let%bind () =
      Numeric.(check ~label:"snarked_next_available_token" Tc.token_id)
        snarked_next_available_token s.snarked_next_available_token
    in
    let%bind () =
      Numeric.(check ~label:"timestamp" Tc.time) timestamp s.timestamp
    in
    let%bind () =
      Numeric.(check ~label:"blockchain_length" Tc.length)
        blockchain_length s.blockchain_length
    in
    let%bind () =
      Numeric.(check ~label:"min_window_density" Tc.length)
        min_window_density s.min_window_density
    in
    ignore last_vrf_output ;
    (* TODO: Decide whether to expose this *)
    let%bind () =
      Numeric.(check ~label:"total_currency" Tc.amount)
        total_currency s.total_currency
    in
    let%bind () =
      Numeric.(check ~label:"curr_global_slot" Tc.global_slot)
        curr_global_slot s.curr_global_slot
    in
    let%bind () =
      Numeric.(check ~label:"global_slot_since_genesis" Tc.global_slot)
        global_slot_since_genesis s.global_slot_since_genesis
    in
    let%bind () =
      epoch_data "staking_epoch_data" staking_epoch_data s.staking_epoch_data
    in
    let%map () =
      epoch_data "next_epoch_data" next_epoch_data s.next_epoch_data
    in
    ()
end

module Account_type = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = User | Snapp | None | Any
      [@@deriving sexp, eq, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  let check (t : t) (a : A.t option) =
    match (a, t) with
    | _, Any ->
        Ok ()
    | None, None ->
        Ok ()
    | None, _ ->
        Or_error.error_string "expected account_type = None"
    | Some a, User ->
        assert_ (Option.is_none a.snapp) "expected account_type = User"
    | Some a, Snapp ->
        assert_ (Option.is_some a.snapp) "expected account_type = Snapp"
    | Some _, None ->
        Or_error.error_string "no second account allowed"

  let to_bits = function
    | User ->
        [true; false]
    | Snapp ->
        [false; true]
    | None ->
        [false; false]
    | Any ->
        [true; true]

  let of_bits = function
    | [user; snapp] -> (
      match (user, snapp) with
      | true, false ->
          User
      | false, true ->
          Snapp
      | false, false ->
          None
      | true, true ->
          Any )
    | _ ->
        assert false

  let to_input x = Random_oracle_input.bitstring (to_bits x)

  module Checked = struct
    type t = {user: Boolean.var; snapp: Boolean.var} [@@deriving hlist]

    let to_input {user; snapp} = Random_oracle_input.bitstring [user; snapp]

    let constant =
      let open Boolean in
      function
      | User ->
          {user= true_; snapp= false_}
      | Snapp ->
          {user= false_; snapp= true_}
      | None ->
          {user= false_; snapp= false_}
      | Any ->
          {user= true_; snapp= true_}

    (* TODO: Write a unit test for these. *)
    let snapp_allowed t = t.snapp

    let user_allowed t = t.user
  end

  let typ =
    let open Checked in
    Typ.of_hlistable [Boolean.typ; Boolean.typ] ~var_to_hlist:to_hlist
      ~var_of_hlist:of_hlist
      ~value_to_hlist:(function
        | User ->
            [true; false]
        | Snapp ->
            [false; true]
        | None ->
            [false; false]
        | Any ->
            [true; true] )
      ~value_of_hlist:(fun [user; snapp] ->
        match (user, snapp) with
        | true, false ->
            User
        | false, true ->
            Snapp
        | false, false ->
            None
        | true, true ->
            Any )
end

module Other = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('account, 'account_transition, 'vk) t =
          { predicate: 'account
          ; account_transition: 'account_transition
          ; account_vk: 'vk }
        [@@deriving hlist, sexp, eq, yojson, hash, compare]
      end
    end]
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Account.Stable.V1.t
        , Account_state.Stable.V1.t Transition.Stable.V1.t
        , F.Stable.V1.t Hash.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, eq, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  module Checked = struct
    type t =
      ( Account.Checked.t
      , Account_state.Checked.t Transition.t
      , Field.Var.t Or_ignore.Checked.t )
      Poly.Stable.Latest.t

    let to_input ({predicate; account_transition; account_vk} : t) =
      let open Random_oracle_input in
      List.reduce_exn ~f:append
        [ Account.Checked.to_input predicate
        ; Transition.to_input ~f:Account_state.Checked.to_input
            account_transition
        ; Hash.(to_input_checked Tc.field) account_vk ]
  end

  let to_input ({predicate; account_transition; account_vk} : t) =
    let open Random_oracle_input in
    List.reduce_exn ~f:append
      [ Account.to_input predicate
      ; Transition.to_input ~f:Account_state.to_input account_transition
      ; Hash.(to_input Tc.field) account_vk ]

  let typ () =
    let open Poly in
    Typ.of_hlistable
      [Account.typ (); Transition.typ Account_state.typ; Hash.(typ Tc.field)]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  let accept : t =
    { predicate= Account.accept
    ; account_transition= {prev= Any; next= Any}
    ; account_vk= Ignore }
end

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('account, 'protocol_state, 'other, 'pk) t =
        { self_predicate: 'account
        ; other: 'other
        ; fee_payer: 'pk
        ; protocol_state_predicate: 'protocol_state }
      [@@deriving hlist, sexp, eq, yojson, hash, compare]

      let to_latest = Fn.id
    end
  end]

  let typ spec =
    let open Stable.Latest in
    Typ.of_hlistable spec ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
      ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Account.Stable.V1.t
      , Protocol_state.Stable.V1.t
      , Other.Stable.V1.t
      , Public_key.Compressed.Stable.V1.t Eq_data.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving sexp, eq, yojson, hash, compare]

    let to_latest = Fn.id
  end
end]

module Digested = F

let to_input ({self_predicate; other; fee_payer; protocol_state_predicate} : t)
    =
  let open Random_oracle_input in
  List.reduce_exn ~f:append
    [ Account.to_input self_predicate
    ; Other.to_input other
    ; Eq_data.(to_input_explicit (Tc.public_key ())) fee_payer
    ; Protocol_state.to_input protocol_state_predicate ]

let digest t =
  Random_oracle.(
    hash ~init:Hash_prefix.snapp_predicate (pack_input (to_input t)))

let check ({self_predicate; other; fee_payer; protocol_state_predicate} : t)
    ~state_view ~self ~(other_prev : A.t option) ~(other_next : unit option)
    ~fee_payer_pk =
  let open Or_error.Let_syntax in
  let%bind () = Protocol_state.check protocol_state_predicate state_view in
  let%bind () = Account.check self_predicate self in
  let%bind () =
    Eq_data.(check (Tc.public_key ()))
      ~label:"fee_payer" fee_payer fee_payer_pk
  in
  let%bind () =
    let check (s : Account_state.t) (a : _ option) =
      match (s, a) with
      | Any, _ | Empty, None | Non_empty, Some _ ->
          return ()
      | _ ->
          Or_error.error_string "Bad account state"
    in
    let%bind () = check other.account_transition.prev other_prev
    and () = check other.account_transition.next other_next in
    match other_prev with
    | None ->
        return ()
    | Some other_account -> (
        let%bind () = Account.check other.predicate other_account in
        match other_account.snapp with
        | None ->
            assert_
              (other.account_vk = Ignore)
              "other_account_vk must be ignore for user account"
        | Some snapp ->
            Hash.(check ~label:"other_account_vk" Tc.field)
              other.account_vk
              (Option.value_map ~f:With_hash.hash snapp.verification_key
                 ~default:Field.zero) )
  in
  return ()

let accept : t =
  { self_predicate= Account.accept
  ; other= Other.accept
  ; fee_payer= Ignore
  ; protocol_state_predicate= Protocol_state.accept }

module Checked = struct
  type t =
    ( Account.Checked.t
    , Protocol_state.Checked.t
    , Other.Checked.t
    , Public_key.Compressed.var Or_ignore.Checked.t )
    Poly.Stable.Latest.t

  let to_input
      ({self_predicate; other; fee_payer; protocol_state_predicate} : t) =
    let open Random_oracle_input in
    List.reduce_exn ~f:append
      [ Account.Checked.to_input self_predicate
      ; Other.Checked.to_input other
      ; Eq_data.(to_input_checked ~explicit:true (Tc.public_key ())) fee_payer
      ; Protocol_state.Checked.to_input protocol_state_predicate ]

  let digest t =
    Random_oracle.Checked.(
      hash ~init:Hash_prefix.snapp_predicate (pack_input (to_input t)))
end

let typ () : (Checked.t, Stable.Latest.t) Typ.t =
  Poly.typ
    [ Account.typ ()
    ; Other.typ ()
    ; Eq_data.(typ_explicit (Tc.public_key ()))
    ; Protocol_state.typ ]
