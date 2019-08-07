open Core
open Coda_base
open Coda_state

module Merkle_list_verifier = Merkle_list_verifier.Make (struct
  type proof_elem = State_body_hash.t

  type hash = State_hash.t [@@deriving eq]

  let hash previous_state_hash state_body_hash =
    Protocol_state.hash_abstract ~hash_body:Fn.id
      {previous_state_hash; body= state_body_hash}
end)

let verify ~target_hash ~transition_chain_witness:(init_state_hash, merkle_list)
    =
  Merkle_list_verifier.verify ~init:init_state_hash merkle_list target_hash
