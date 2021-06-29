(* sql.ml -- (Postgresql) SQL queries for validate_delegation app *)

open Core_kernel

module Block_info = struct
  type t =
    {id: int; global_slot: int64; state_hash: string; ledger_hash: string}
  [@@deriving hlist]

  let typ =
    let open Archive_lib.Processor.Caqti_type_spec in
    let spec = Caqti_type.[int; int64; string; string] in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  (* find all blocks, working back from block with given state hash *)
  let query =
    Caqti_request.collect Caqti_type.string typ
      {sql| WITH RECURSIVE chain AS (

              SELECT id,parent_id,global_slot,state_hash,ledger_hash FROM blocks b WHERE b.state_hash = ?

              UNION ALL

              SELECT b.id,b.parent_id,b.global_slot,b.state_hash,b.ledger_hash FROM blocks b

              INNER JOIN chain

              ON b.id = chain.parent_id AND NOT chain.parent_id IS NULL
           )

           SELECT id,global_slot,state_hash,ledger_hash FROM chain c

      |sql}

  let run (module Conn : Caqti_async.CONNECTION) state_hash =
    Conn.collect_list query state_hash
end

(* build query to find all blocks back to genesis block, starting with the block containing the
   specified state hash; for each such block, find ids of all (user or internal) commands in that block
*)

let find_command_ids_query s =
  sprintf
    {sql| WITH RECURSIVE chain AS (

            SELECT id,parent_id FROM blocks b WHERE b.state_hash = ?

            UNION ALL

            SELECT b.id,b.parent_id FROM blocks b

            INNER JOIN chain

            ON b.id = chain.parent_id AND NOT chain.id IS NULL
          )

          SELECT DISTINCT %s_command_id FROM chain c

          INNER JOIN blocks_%s_commands bc

          ON bc.block_id = c.id

     |sql}
    s s

module User_command = struct
  type t =
    { type_: string
    ; fee_payer_id: int
    ; source_id: int
    ; receiver_id: int
    ; fee: int64
    ; fee_token: int64
    ; token: int64
    ; amount: int64 option
    ; valid_until: int64 option
    ; memo: string
    ; nonce: int64
    ; block_id: int
    ; global_slot: int64
    ; txn_global_slot: int64
    ; sequence_no: int
    ; status: string
    ; created_token: int64 option
    ; fee_payer_balance: int
    ; source_balance: int option
    ; receiver_balance: int option }
  [@@deriving yojson, hlist, equal]

  let typ =
    let open Archive_lib.Processor.Caqti_type_spec in
    let spec =
      Caqti_type.
        [ string
        ; int
        ; int
        ; int
        ; int64
        ; int64
        ; int64
        ; option int64
        ; option int64
        ; string
        ; int64
        ; int
        ; int64
        ; int64
        ; int
        ; string
        ; option int64
        ; int
        ; option int
        ; option int ]
    in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  let query =
    Caqti_request.collect Caqti_type.int typ
      {sql| SELECT type,fee_payer_id, source_id,receiver_id,fee,fee_token,token,amount,valid_until,memo,nonce,
                   blocks.id,blocks.global_slot,parent.global_slot_since_genesis,
                   sequence_no,status,created_token,
                   fee_payer_balance, source_balance, receiver_balance

            FROM (SELECT * FROM user_commands WHERE id = ?) AS uc

            INNER JOIN blocks_user_commands AS buc

            ON uc.id = buc.user_command_id

            INNER JOIN blocks

            ON blocks.id = buc.block_id

            INNER JOIN blocks as parent

            ON parent.id = blocks.parent_id

       |sql}

  let run (module Conn : Caqti_async.CONNECTION) user_cmd_id =
    Conn.collect_list query user_cmd_id

  let query_payments_by_source_and_receiver =
    Caqti_request.collect
      Caqti_type.(tup2 int int)
      typ
      {sql| SELECT type,fee_payer_id, source_id, receiver_id, fee,fee_token,
               token, amount, valid_until, memo, nonce, blocks.id, blocks.global_slot,
               parent.global_slot_since_genesis, sequence_no, status, created_token,
               fee_payer_balance, source_balance, receiver_balance

            FROM (SELECT * FROM user_commands WHERE source_id = $1
                                              AND receiver_id = $2
                                              AND type = 'payment') AS uc

            INNER JOIN blocks_user_commands AS buc

            ON uc.id = buc.user_command_id

            INNER JOIN blocks

            ON blocks.id = buc.block_id

            INNER JOIN blocks as parent

            ON parent.id = blocks.parent_id

            WHERE buc.status = 'applied'

       |sql}

  let run_payments_by_source_and_receiver
      (module Conn : Caqti_async.CONNECTION) ~source_id ~receiver_id =
    Conn.collect_list query_payments_by_source_and_receiver
      (source_id, receiver_id)

  let query_payments_by_receiver =
    Caqti_request.collect Caqti_type.int typ
      {sql| SELECT type,fee_payer_id, source_id, receiver_id, fee,fee_token,
               token, amount, valid_until, memo, nonce, blocks.id, blocks.global_slot,
               parent.global_slot_since_genesis, sequence_no, status, created_token,
               fee_payer_balance, source_balance, receiver_balance

            FROM (SELECT * FROM user_commands WHERE receiver_id = $1
                                              AND type = 'payment') AS uc

            INNER JOIN blocks_user_commands AS buc

            ON uc.id = buc.user_command_id

            INNER JOIN blocks

            ON blocks.id = buc.block_id

            INNER JOIN blocks as parent

            ON parent.id = blocks.parent_id

            WHERE buc.status = 'applied'

       |sql}

  let run_payments_by_receiver (module Conn : Caqti_async.CONNECTION)
      ~receiver_id =
    Conn.collect_list query_payments_by_receiver receiver_id
end

module Public_key = struct
  let query =
    Caqti_request.find_opt Caqti_type.int Caqti_type.string
      {sql| SELECT value FROM public_keys
            WHERE id = ?
      |sql}

  let run (module Conn : Caqti_async.CONNECTION) pk_id =
    Conn.find_opt query pk_id

  let query_for_id =
    Caqti_request.find_opt Caqti_type.string Caqti_type.int
      {sql| SELECT id FROM public_keys
            WHERE value = ?
      |sql}

  let run_for_id (module Conn : Caqti_async.CONNECTION) pk =
    Conn.find_opt query_for_id pk
end

module Block = struct
  let max_slot_query =
    Caqti_request.find Caqti_type.unit Caqti_type.int
      {sql| SELECT MAX(global_slot) FROM blocks
      |sql}

  let get_max_slot (module Conn : Caqti_async.CONNECTION) () =
    Conn.find max_slot_query ()

  let state_hashes_by_slot_query =
    Caqti_request.collect Caqti_type.int Caqti_type.string
      {sql| SELECT state_hash FROM blocks WHERE global_slot = $1
      |sql}

  let get_state_hashes_by_slot (module Conn : Caqti_async.CONNECTION) slot =
    Conn.collect_list state_hashes_by_slot_query slot

  let creator_slot_bounds_query =
    Caqti_request.collect
      Caqti_type.(tup3 int int64 int64)
      Caqti_type.int
      {sql| SELECT id FROM blocks
            WHERE creator_id = $1
            AND global_slot >= $2 AND global_slot <= $3
      |sql}

  let get_block_ids_for_creator_in_slot_bounds
      (module Conn : Caqti_async.CONNECTION) ~creator ~low_slot ~high_slot =
    Conn.collect_list creator_slot_bounds_query (creator, low_slot, high_slot)
end

module Coinbase_receivers_for_block_creator = struct
  (* all receivers of coinbase internal commands contained in blocks with
     with given creator_id, where the receiver distinct from the creator_id
  *)
  let query =
    Caqti_request.collect Caqti_type.int Caqti_type.int
      {sql| SELECT DISTINCT ic.receiver_id

            FROM blocks b

            INNER JOIN

            blocks_internal_commands bic

            ON bic.block_id = b.id

            INNER JOIN

            internal_commands ic

            ON bic.internal_command_id = ic.id

            WHERE b.creator_id = ?
              AND ic.type = 'coinbase'
              AND ic.receiver_id <> b.creator_id
      |sql}

  let run (module Conn : Caqti_async.CONNECTION) ~block_creator_id =
    Conn.collect_list query block_creator_id
end
