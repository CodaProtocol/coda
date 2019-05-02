open Core_kernel
open Fold_lib
open Tuple_lib
open Snark_params.Tick

type t = Payment | Stake_delegation | Fee_transfer | Coinbase | Chain_voting
[@@deriving enum, eq, sexp]

let gen =
  Quickcheck.Generator.map (Int.gen_incl min max) ~f:(fun i ->
      Option.value_exn (of_enum i) )

type var = Boolean.var * Boolean.var * Boolean.var

let to_bits = function
  | Payment ->
      (false, false, false)
  | Stake_delegation ->
      (true, false, false)
  | Fee_transfer ->
      (false, true, false)
  | Coinbase ->
      (true, true, false)
  | Chain_voting ->
      (false, false, true)

let of_bits = function
  | false, false, false ->
      Payment
  | true, false, false ->
      Stake_delegation
  | false, true, false ->
      Fee_transfer
  | true, true, false ->
      Coinbase
  | false, false, true ->
      Chain_voting
  | _ ->
      failwith "unrecognized bits"

let%test_unit "to_bool of_bool inv" =
  let open Quickcheck in
  test
    (Generator.tuple3 Bool.quickcheck_generator Bool.quickcheck_generator
       Bool.quickcheck_generator) ~f:(fun b -> assert (b = to_bits (of_bits b)))

let typ =
  Typ.transport
    Typ.(tuple3 Boolean.typ Boolean.typ Boolean.typ)
    ~there:to_bits ~back:of_bits

let fold (t : t) : bool Triple.t Fold.t =
  { fold=
      (fun ~init ~f ->
        let b0, b1, b2 = to_bits t in
        f init (b0, b1, b2) ) }

let length_in_triples = 1

module Checked = struct
  let constant t =
    let x, y, z = to_bits t in
    Boolean.(var_of_value x, var_of_value y, var_of_value z)

  let to_triples ((x, y, z) : var) = [(x, y, z)]

  (* someday: Make these all cached *)

  let is_payment (b0, b1, b2) = Boolean.(all [not b0; not b1; not b2])

  let is_fee_transfer (b0, b1, b2) = Boolean.(all [not b0; b1; not b2])

  let is_stake_delegation (b0, b1, b2) = Boolean.(all [b0; not b1; not b2])

  let is_coinbase (b0, b1, b2) = Boolean.(all [b0; b1; not b2])

  let is_chain_voting (b0, b1, b2) = Boolean.(all [not b0; not b1; b2])

  let is_user_command bs =
    let%bind payment = is_payment bs
    and fee_transfer = is_stake_delegation bs
    and chain_voting = is_chain_voting bs in
    Boolean.any [payment; fee_transfer; chain_voting]

  let%test_module "predicates" =
    ( module struct
      let test_predicate checked unchecked =
        for i = min to max do
          Test_util.test_equal typ Boolean.typ checked unchecked
            (Option.value_exn (of_enum i))
        done

      let one_of xs t = List.mem xs ~equal t

      let%test_unit "is_payment" = test_predicate is_payment (( = ) Payment)

      let%test_unit "is_fee_transfer" =
        test_predicate is_fee_transfer (( = ) Fee_transfer)

      let%test_unit "is_coinbase" = test_predicate is_coinbase (( = ) Coinbase)

      let%test_unit "is_user_command" =
        test_predicate is_user_command (one_of [Payment; Stake_delegation])
    end )
end
