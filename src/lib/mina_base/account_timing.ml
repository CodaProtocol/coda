[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

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

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('slot, 'balance, 'amount) t =
        | Untimed
        | Timed of
            { initial_minimum_balance: 'balance
            ; cliff_time: 'slot
            ; cliff_amount: 'amount
            ; vesting_period: 'slot
            ; vesting_increment: 'amount }
      [@@deriving sexp, equal, hash, compare, yojson]
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Global_slot.Stable.V1.t
      , Balance.Stable.V1.t
      , Amount.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving sexp, equal, hash, compare, yojson]

    let to_latest = Fn.id
  end
end]

type ('slot, 'balance, 'amount) tt = ('slot, 'balance, 'amount) Poly.t =
  | Untimed
  | Timed of
      { initial_minimum_balance: 'balance
      ; cliff_time: 'slot
      ; cliff_amount: 'amount
      ; vesting_period: 'slot
      ; vesting_increment: 'amount }
[@@deriving sexp, equal, hash, compare, yojson]

module As_record = struct
  type ('bool, 'slot, 'balance, 'amount) t =
    { is_timed: 'bool
    ; initial_minimum_balance: 'balance
    ; cliff_time: 'slot
    ; cliff_amount: 'amount
    ; vesting_period: 'slot
    ; vesting_increment: 'amount }
  [@@deriving hlist]
end

(* convert sum type to record format, useful for to_bits and typ *)
let to_record t =
  match t with
  | Untimed ->
      let slot_unused = Global_slot.zero in
      let slot_one = Global_slot.(succ zero) in
      let balance_unused = Balance.zero in
      let amount_unused = Amount.zero in
      As_record.
        { is_timed= false
        ; initial_minimum_balance= balance_unused
        ; cliff_time= slot_unused
        ; cliff_amount= amount_unused
        ; vesting_period= slot_one (* avoid division by zero *)
        ; vesting_increment= amount_unused }
  | Timed
      { initial_minimum_balance
      ; cliff_time
      ; cliff_amount
      ; vesting_period
      ; vesting_increment } ->
      As_record.
        { is_timed= true
        ; initial_minimum_balance
        ; cliff_time
        ; cliff_amount
        ; vesting_period
        ; vesting_increment }

let to_bits t =
  let As_record.
        { is_timed
        ; initial_minimum_balance
        ; cliff_time
        ; cliff_amount
        ; vesting_period
        ; vesting_increment } =
    to_record t
  in
  is_timed
  :: ( Balance.to_bits initial_minimum_balance
     @ Global_slot.to_bits cliff_time
     @ Amount.to_bits cliff_amount
     @ Global_slot.to_bits vesting_period
     @ Amount.to_bits vesting_increment )

[%%ifdef
consensus_mechanism]

type var =
  (Boolean.var, Global_slot.Checked.var, Balance.var, Amount.var) As_record.t

let var_to_bits
    As_record.
      { is_timed
      ; initial_minimum_balance
      ; cliff_time
      ; cliff_amount
      ; vesting_period
      ; vesting_increment } =
  let open Bitstring_lib.Bitstring.Lsb_first in
  let initial_minimum_balance =
    to_list @@ Balance.var_to_bits initial_minimum_balance
  in
  let cliff_time = to_list @@ Global_slot.var_to_bits cliff_time in
  let cliff_amount = to_list @@ Amount.var_to_bits cliff_amount in
  let vesting_period = to_list @@ Global_slot.var_to_bits vesting_period in
  let vesting_increment = to_list @@ Amount.var_to_bits vesting_increment in
  of_list
    ( is_timed
    :: ( initial_minimum_balance @ cliff_time @ cliff_amount @ vesting_period
       @ vesting_increment ) )

let var_of_t (t : t) : var =
  let As_record.
        { is_timed
        ; initial_minimum_balance
        ; cliff_time
        ; cliff_amount
        ; vesting_period
        ; vesting_increment } =
    to_record t
  in
  As_record.
    { is_timed= Boolean.var_of_value is_timed
    ; initial_minimum_balance= Balance.var_of_t initial_minimum_balance
    ; cliff_time= Global_slot.Checked.constant cliff_time
    ; cliff_amount= Amount.var_of_t cliff_amount
    ; vesting_period= Global_slot.Checked.constant vesting_period
    ; vesting_increment= Amount.var_of_t vesting_increment }

let untimed_var = var_of_t Untimed

let typ : (var, t) Typ.t =
  let spec =
    let open Data_spec in
    [ Boolean.typ
    ; Balance.typ
    ; Global_slot.typ
    ; Amount.typ
    ; Global_slot.typ
    ; Amount.typ ]
  in
  (* because we represent the types t (a sum type) and var (a record) differently,
      we can't use the trick, used elsewhere, of polymorphic to_hlist and of_hlist
      functions to handle both types
  *)
  let value_of_hlist :
         ( unit
         ,    Boolean.value
           -> Balance.t
           -> Global_slot.t
           -> Amount.t
           -> Global_slot.t
           -> Amount.t
           -> unit )
         H_list.t
      -> t =
    let open H_list in
    fun [ is_timed
        ; initial_minimum_balance
        ; cliff_time
        ; cliff_amount
        ; vesting_period
        ; vesting_increment ] ->
      if is_timed then
        Timed
          { initial_minimum_balance
          ; cliff_time
          ; cliff_amount
          ; vesting_period
          ; vesting_increment }
      else Untimed
  in
  let value_to_hlist (t : t) =
    let As_record.
          { is_timed
          ; initial_minimum_balance
          ; cliff_time
          ; cliff_amount
          ; vesting_period
          ; vesting_increment } =
      to_record t
    in
    H_list.
      [ is_timed
      ; initial_minimum_balance
      ; cliff_time
      ; cliff_amount
      ; vesting_period
      ; vesting_increment ]
  in
  let var_of_hlist = As_record.of_hlist in
  let var_to_hlist = As_record.to_hlist in
  Typ.of_hlistable spec ~var_to_hlist ~var_of_hlist ~value_to_hlist
    ~value_of_hlist

(* we can't use the generic if_ with the above typ, because Global_slot.typ doesn't work correctly with it
    so we define a custom if_
*)
let if_ b ~(then_ : var) ~(else_ : var) =
  let%bind is_timed =
    Boolean.if_ b ~then_:then_.is_timed ~else_:else_.is_timed
  in
  let%bind initial_minimum_balance =
    Balance.Checked.if_ b ~then_:then_.initial_minimum_balance
      ~else_:else_.initial_minimum_balance
  in
  let%bind cliff_time =
    Global_slot.Checked.if_ b ~then_:then_.cliff_time ~else_:else_.cliff_time
  in
  let%bind cliff_amount =
    Amount.Checked.if_ b ~then_:then_.cliff_amount ~else_:else_.cliff_amount
  in
  let%bind vesting_period =
    Global_slot.Checked.if_ b ~then_:then_.vesting_period
      ~else_:else_.vesting_period
  in
  let%map vesting_increment =
    Amount.Checked.if_ b ~then_:then_.vesting_increment
      ~else_:else_.vesting_increment
  in
  As_record.
    { is_timed
    ; initial_minimum_balance
    ; cliff_time
    ; cliff_amount
    ; vesting_period
    ; vesting_increment }

[%%endif]
