(* sql.ml -- (Postgresql) SQL queries for replayer *)

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

              ON b.id = chain.parent_id AND chain.id <> chain.parent_id
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

            ON b.id = chain.parent_id AND chain.id <> chain.parent_id
          )

          SELECT DISTINCT %s_command_id FROM chain c

          INNER JOIN blocks_%s_commands bc

          ON bc.block_id = c.id

     |sql}
    s s

module Block = struct
  let state_hash_query =
    Caqti_request.find Caqti_type.int Caqti_type.string
      {sql| SELECT state_hash FROM blocks
            WHERE id = ?
      |sql}

  let get_state_hash (module Conn : Caqti_async.CONNECTION) id =
    Conn.find state_hash_query id

  let unparented_query =
    Caqti_request.collect Caqti_type.unit Caqti_type.int
      {sql| SELECT id FROM blocks
            WHERE parent_id IS NULL
      |sql}

  let get_unparented (module Conn : Caqti_async.CONNECTION) () =
    Conn.collect_list unparented_query ()
end

module User_command_ids = struct
  let query =
    Caqti_request.collect Caqti_type.string Caqti_type.int
      (find_command_ids_query "user")

  let run (module Conn : Caqti_async.CONNECTION) state_hash =
    Conn.collect_list query state_hash
end

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
  [@@deriving hlist]

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
end

module Internal_command_ids = struct
  let query =
    Caqti_request.collect Caqti_type.string Caqti_type.int
      (find_command_ids_query "internal")

  let run (module Conn : Caqti_async.CONNECTION) state_hash =
    Conn.collect_list query state_hash
end

module Internal_command = struct
  type t =
    { type_: string
    ; receiver_id: int
    ; receiver_balance: int
    ; fee: int64
    ; token: int64
    ; block_id: int
    ; global_slot: int64
    ; txn_global_slot: int64
    ; sequence_no: int
    ; secondary_sequence_no: int }
  [@@deriving hlist]

  let typ =
    let open Archive_lib.Processor.Caqti_type_spec in
    let spec =
      Caqti_type.[string; int; int; int64; int64; int; int64; int64; int; int]
    in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  (* the transaction global slot is taken from the internal command's parent block, mirroring
     the call to Staged_ledger.apply in Block_producer
  *)
  let query =
    Caqti_request.collect Caqti_type.int typ
      {sql| SELECT type,receiver_id,receiver_balance,fee,token,
                   blocks.id,blocks.global_slot,parent.global_slot,sequence_no,secondary_sequence_no

            FROM (SELECT * FROM internal_commands WHERE id = ?) AS ic

            INNER JOIN blocks_internal_commands AS bic

            ON  ic.id = bic.internal_command_id

            INNER JOIN blocks

            ON blocks.id = bic.block_id

            INNER JOIN blocks as parent

            ON parent.id = blocks.parent_id

       |sql}

  let run (module Conn : Caqti_async.CONNECTION) internal_cmd_id =
    Conn.collect_list query internal_cmd_id
end

module Public_key = struct
  let query =
    Caqti_request.find_opt Caqti_type.int Caqti_type.string
      {sql| SELECT value FROM public_keys
            WHERE id = ?
      |sql}

  let run (module Conn : Caqti_async.CONNECTION) pk_id =
    Conn.find_opt query pk_id
end

module Epoch_data = struct
  type epoch_data = {epoch_ledger_hash: string; epoch_data_seed: string}

  let epoch_data_typ =
    let encode t = Ok (t.epoch_ledger_hash, t.epoch_data_seed) in
    let decode (epoch_ledger_hash, epoch_data_seed) =
      Ok {epoch_ledger_hash; epoch_data_seed}
    in
    let rep = Caqti_type.(tup2 string string) in
    Caqti_type.custom ~encode ~decode rep

  let query_epoch_data =
    Caqti_request.find Caqti_type.int epoch_data_typ
      {sql| SELECT slh.value, ed.seed FROM snarked_ledger_hashes AS slh

       INNER JOIN

       epoch_data AS ed

       ON slh.id = ed.ledger_hash_id

       WHERE ed.id = ?

      |sql}

  let get_epoch_data (module Conn : Caqti_async.CONNECTION) epoch_ledger_id =
    Conn.find query_epoch_data epoch_ledger_id

  let query_staking_epoch_data_id =
    Caqti_request.find Caqti_type.string Caqti_type.int
      {sql| SELECT staking_epoch_data_id FROM blocks

            WHERE state_hash = ?

      |sql}

  let get_staking_epoch_data_id (module Conn : Caqti_async.CONNECTION)
      state_hash =
    Conn.find query_staking_epoch_data_id state_hash

  let query_next_epoch_data_id =
    Caqti_request.find Caqti_type.string Caqti_type.int
      {sql| SELECT next_epoch_data_id FROM blocks

            WHERE state_hash = ?
      |sql}

  let get_next_epoch_data_id (module Conn : Caqti_async.CONNECTION) state_hash
      =
    Conn.find query_next_epoch_data_id state_hash
end

module Parent_block = struct
  (* fork block is parent of block with the given state hash *)
  let query_parent_state_hash =
    Caqti_request.find Caqti_type.string Caqti_type.string
      {sql| SELECT parent.state_hash FROM blocks AS parent

            INNER JOIN

            (SELECT parent_id FROM blocks WHERE state_hash = ?) AS epoch_ledgers_block

            ON epoch_ledgers_block.parent_id = parent.id
      |sql}

  let get_parent_state_hash (module Conn : Caqti_async.CONNECTION)
      epoch_ledgers_state_hash =
    Conn.find query_parent_state_hash epoch_ledgers_state_hash
end

module Balance = struct
  let query =
    Caqti_request.find_opt
      Caqti_type.(tup2 int int)
      Caqti_type.int64
      {sql| SELECT balance FROM balances
            WHERE id = $1
            AND public_key_id = $2
      |sql}

  let run (module Conn : Caqti_async.CONNECTION) ~id ~pk_id =
    Conn.find_opt query (id, pk_id)
end
