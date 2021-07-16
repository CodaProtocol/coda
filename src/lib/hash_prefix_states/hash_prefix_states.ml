[%%import "/src/config.mlh"]

[%%ifdef consensus_mechanism]

[%%else]

module Random_oracle = Random_oracle_nonconsensus.Random_oracle
module Mina_compile_config =
  Mina_compile_config_nonconsensus.Mina_compile_config

[%%endif]

open Core_kernel
open Hash_prefixes

let salt (s : Hash_prefixes.t) = Random_oracle.salt (s :> string)

let receipt_chain_user_command = salt receipt_chain_user_command

let receipt_chain_snapp = salt receipt_chain_snapp

let coinbase = salt coinbase

let pending_coinbases = salt pending_coinbases

let coinbase_stack_data = salt coinbase_stack_data

let coinbase_stack_state_hash = salt coinbase_stack_state_hash

let coinbase_stack = salt coinbase_stack

let checkpoint_list = salt checkpoint_list

let merge_snark = salt merge_snark

let base_snark = salt base_snark

let protocol_state = salt protocol_state

let protocol_state_body = salt protocol_state_body

let merkle_tree =
  let f i = salt (merkle_tree i) in
  (* Cache up to the compiled ledger depth. *)
  let cached = ref [||] in
  fun i ->
    let len = Array.length !cached in
    if i >= len then
      cached :=
        Array.append !cached
          (Array.init (i + 1 - len) ~f:(fun i -> f (i + len))) ;
    !cached.(i)

let coinbase_merkle_tree =
  let f i = salt (coinbase_merkle_tree i) in
  let cached = ref [||] in
  fun i ->
    let len = Array.length !cached in
    if i >= len then
      cached :=
        Array.append !cached
          (Array.init (i + 1 - len) ~f:(fun i -> f (i + len))) ;
    !cached.(i)

let vrf_message = salt vrf_message

let signature =
  match Mina_signature_kind.t with
  | Mainnet ->
      salt signature_mainnet
  | Testnet ->
      salt signature_testnet

let vrf_output = salt vrf_output

let vrf_evaluation = salt vrf_evaluation

let epoch_seed = salt epoch_seed

let transition_system_snark = salt transition_system_snark

let account = salt account

let side_loaded_vk = salt side_loaded_vk

let snapp_account = salt snapp_account

let snapp_payload = salt snapp_payload

let snapp_body = salt snapp_body

let snapp_predicate = salt snapp_predicate

let snapp_predicate_account = salt snapp_predicate_account

let snapp_predicate_protocol_state = salt snapp_predicate_protocol_state

let party = salt party

let party_predicate = salt party_predicate

let party_cons = salt party_cons

let party_with_protocol_state_predicate =
  salt party_with_protocol_state_predicate
