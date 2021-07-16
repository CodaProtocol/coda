open Core_kernel

let bit_length_to_triple_length n =
  let r = n mod 3 in
  let k = n / 3 in
  if r = 0 then k else k + 1

let split_last_exn =
  let rec go acc x xs =
    match xs with [] -> (List.rev acc, x) | x' :: xs -> go (x :: acc) x' xs
  in
  function [] -> failwith "split_last: Empty list" | x :: xs -> go [] x xs

let two_to_the i = Bignum_bigint.(pow (of_int 2) (of_int i))

let todo_snapps = `Needs_some_work_for_snapps_on_mainnet

let todo_separate_fee = `Update_when_we_add_a_separate_fee

let todo_multiple_slots_per_transaction =
  `Needs_update_for_multiple_slots_per_txn
