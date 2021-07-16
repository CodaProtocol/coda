module Rel_insert_input = struct
  type ('data, 'on_conflict) t =
    < data : 'data ; on_conflict : 'on_conflict option >

  type nonrec ('data, 'on_conflict) array = ('data array, 'on_conflict) t
end

type void

module On_conflict = struct
  type ('constraint_, 'update_columns) t =
    < constraint_ : 'constraint_ ; update_columns : 'update_columns array >

  let create constraint_ update_columns =
    object
      method constraint_ = constraint_

      method update_columns = Array.of_list update_columns
    end

  type public_keys = ([ `public_keys_value_key ], [ `value ]) t

  type fee_transfers = ([ `fee_transfers_hash_key ], [ `first_seen ]) t

  type user_commands = ([ `user_commands_hash_key ], [ `first_seen ]) t

  type receipt_chain_hash = ([ `receipt_chain_hashes_hash_key ], [ `hash ]) t

  type snark_jobs = ([ `snark_jobs_job1_job2_key ], [ `job1 | `job2 ]) t

  type blocks_user_commands =
    ( [ `blocks_user_commands_block_id_user_command_id_receipt_chain_has ]
    , void )
    t

  type blocks_fee_transfers =
    ([ `blocks_fee_transfers_block_id_fee_transfer_id_key ], void) t

  type block_snark_jobs =
    ([ `blocks_snark_jobs_block_id_snark_job_id_key ], void) t

  type state_hashes = ([ `state_hashes_value_key ], [ `value ]) t

  type blocks = ([ `blocks_state_hash_key ], [ `block_length ]) t

  let public_keys : public_keys = create `public_keys_value_key [ `value ]

  let state_hashes : state_hashes = create `state_hashes_value_key [ `value ]

  let user_commands : user_commands =
    create `user_commands_hash_key [ `first_seen ]

  let fee_transfers : fee_transfers =
    create `fee_transfers_hash_key [ `first_seen ]

  let snark_jobs : snark_jobs =
    create `snark_jobs_job1_job2_key [ `job1; `job2 ]

  let receipt_chain_hash : receipt_chain_hash =
    create `receipt_chain_hashes_hash_key [ `hash ]

  let blocks_user_commands : blocks_user_commands =
    create `blocks_user_commands_block_id_user_command_id_receipt_chain_has []

  let blocks_fee_transfers : blocks_fee_transfers =
    create `blocks_fee_transfers_block_id_fee_transfer_id_key []

  let blocks_snark_jobs : block_snark_jobs =
    create `blocks_snark_jobs_block_id_snark_job_id_key []
end

type bit = Yojson.Basic.t

type user_command_type = Yojson.Basic.t

type public_keys_insert_input =
  < blocks : blocks_arr_rel_insert_input option
  ; fee_transfers : fee_transfers_arr_rel_insert_input option
  ; snark_jobs : snark_jobs_arr_rel_insert_input option
  ; userCommandsByReceiver : user_commands_arr_rel_insert_input option
  ; user_commands : user_commands_arr_rel_insert_input option
  ; value : string option >

and public_keys_obj_rel_insert_input =
  (public_keys_insert_input, On_conflict.public_keys) Rel_insert_input.t

and fee_transfers_insert_input =
  < blocks_fee_transfers : blocks_fee_transfers_arr_rel_insert_input option
  ; fee : bit option
  ; first_seen : bit option
  ; hash : string option
  ; public_key : public_keys_obj_rel_insert_input option
  ; receiver : int option >

and fee_transfers_obj_rel_insert_input =
  (fee_transfers_insert_input, On_conflict.fee_transfers) Rel_insert_input.t

and fee_transfers_arr_rel_insert_input =
  (fee_transfers_insert_input, On_conflict.fee_transfers) Rel_insert_input.array

and receipt_chain_hashes_insert_input =
  < blocks_user_commands : blocks_user_commands_arr_rel_insert_input option
  ; hash : string option
  ; receipt_chain_hash : receipt_chain_hashes_obj_rel_insert_input option
  ; receipt_chain_hashes : receipt_chain_hashes_arr_rel_insert_input option >

and receipt_chain_hashes_obj_rel_insert_input =
  ( receipt_chain_hashes_insert_input
  , On_conflict.receipt_chain_hash )
  Rel_insert_input.t

and receipt_chain_hashes_arr_rel_insert_input =
  ( receipt_chain_hashes_insert_input
  , On_conflict.receipt_chain_hash )
  Rel_insert_input.array

and user_commands_insert_input =
  < amount : bit option
  ; blocks_user_commands : blocks_user_commands_arr_rel_insert_input option
  ; fee : bit option
  ; first_seen : bit option
  ; hash : string option
  ; memo : string option
  ; nonce : bit option
  ; publicKeyByReceiver : public_keys_obj_rel_insert_input option
  ; public_key : public_keys_obj_rel_insert_input option
  ; typ : user_command_type option >

and state_hashes_insert_input =
  < block : blocks_obj_rel_insert_input option
  ; blocks : blocks_arr_rel_insert_input option
  ; value : string option >

and state_hashes_obj_rel_insert_input =
  (state_hashes_insert_input, On_conflict.state_hashes) Rel_insert_input.t

and user_commands_obj_rel_insert_input =
  (user_commands_insert_input, On_conflict.user_commands) Rel_insert_input.t

and user_commands_arr_rel_insert_input =
  (user_commands_insert_input, On_conflict.user_commands) Rel_insert_input.array

and snark_jobs_insert_input =
  < blocks_snark_jobs : blocks_snark_jobs_arr_rel_insert_input option
  ; fee : bit option
  ; job1 : int option
  ; job2 : int option
  ; prover : int option
  ; public_key : public_keys_obj_rel_insert_input option >

and snark_jobs_obj_rel_insert_input =
  (snark_jobs_insert_input, On_conflict.snark_jobs) Rel_insert_input.t

and snark_jobs_arr_rel_insert_input =
  (snark_jobs_insert_input, On_conflict.snark_jobs) Rel_insert_input.array

and blocks_fee_transfers_insert_input =
  < block : blocks_obj_rel_insert_input option
  ; block_id : int option
  ; fee_transfer : fee_transfers_obj_rel_insert_input option
  ; fee_transfer_id : int option >

and blocks_fee_transfers_obj_rel_insert_input =
  ( blocks_fee_transfers_insert_input
  , On_conflict.blocks_fee_transfers )
  Rel_insert_input.t

and blocks_fee_transfers_arr_rel_insert_input =
  ( blocks_fee_transfers_insert_input
  , On_conflict.blocks_fee_transfers )
  Rel_insert_input.array

and blocks_user_commands_insert =
  < block : blocks_obj_rel_insert_input option
  ; receipt_chain_hash : receipt_chain_hashes_obj_rel_insert_input option
  ; user_command : user_commands_obj_rel_insert_input option >

and blocks_user_commands_arr_rel_insert_input =
  ( blocks_user_commands_insert
  , On_conflict.blocks_user_commands )
  Rel_insert_input.array

and blocks_snark_jobs_insert_input =
  < block : blocks_obj_rel_insert_input option
  ; block_id : int option
  ; snark_job : snark_jobs_obj_rel_insert_input option
  ; snark_job_id : int option >

and blocks_snark_jobs_arr_rel_insert_input =
  ( blocks_snark_jobs_insert_input
  , On_conflict.block_snark_jobs )
  Rel_insert_input.array

and blocks_insert_input =
  < block_length : bit option
  ; block_time : bit option
  ; blocks_fee_transfers : blocks_fee_transfers_arr_rel_insert_input option
  ; blocks_snark_jobs : blocks_snark_jobs_arr_rel_insert_input option
  ; blocks_user_commands : blocks_user_commands_arr_rel_insert_input option
  ; global_slot : int option
  ; ledger_hash : string option
  ; ledger_proof_nonce : int option
  ; public_key : public_keys_obj_rel_insert_input option
  ; stateHashByParentHash : state_hashes_obj_rel_insert_input option
  ; stateHashByStateHash : state_hashes_obj_rel_insert_input option
  ; status : int option >

and blocks_obj_rel_insert_input =
  (blocks_insert_input, On_conflict.blocks) Rel_insert_input.t

and blocks_arr_rel_insert_input =
  (blocks_insert_input, On_conflict.blocks) Rel_insert_input.array
