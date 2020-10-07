(* sql.ml -- (Postgresql) SQL queries for replayer *)

open Core_kernel

module Global_slots_and_ledger_hashes = struct
  (* find all global slots in blocks, working back from block with given state hash *)
  let query =
    Caqti_request.collect Caqti_type.string
      Caqti_type.(tup2 int64 string)
      {|
         WITH RECURSIVE chain AS (

           SELECT id,parent_id,global_slot,ledger_hash FROM blocks b WHERE b.state_hash = ?

           UNION ALL

           SELECT b.id,b.parent_id,b.global_slot,b.ledger_hash FROM blocks b

           INNER JOIN chain

           ON b.id = chain.parent_id
        )

        SELECT global_slot,ledger_hash FROM chain c
   |}

  let run (module Conn : Caqti_async.CONNECTION) state_hash =
    Conn.collect_list query state_hash
end

(* build query to find all blocks back to genesis block, starting with the block containing the
   specified state hash; for each such block, find ids of all (user or internal) commands in that block
*)

let find_command_ids_query s =
  sprintf
    {|
      WITH RECURSIVE chain AS (

        SELECT id,parent_id FROM blocks b WHERE b.state_hash = ?

        UNION ALL

        SELECT b.id,b.parent_id FROM blocks b

        INNER JOIN chain

        ON b.id = chain.parent_id
      )

      SELECT DISTINCT %s_command_id FROM chain c

      INNER JOIN

      blocks_%s_commands bc

      ON bc.block_id = c.id

     |}
    s s

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
    ; memo: string
    ; nonce: int64
    ; global_slot: int64
    ; sequence_no: int }

  let typ =
    (* chunk into groups so we can use tuple combinators *)
    let encode t =
      Ok
        ( (t.type_, t.fee_payer_id, t.source_id, t.receiver_id)
        , (t.fee, t.fee_token, t.token, t.amount)
        , (t.memo, t.nonce, t.global_slot, t.sequence_no) )
    in
    let decode
        ( (type_, fee_payer_id, source_id, receiver_id)
        , (fee, fee_token, token, amount)
        , (memo, nonce, global_slot, sequence_no) ) =
      Ok
        { type_
        ; fee_payer_id
        ; source_id
        ; receiver_id
        ; fee
        ; fee_token
        ; token
        ; amount
        ; memo
        ; nonce
        ; global_slot
        ; sequence_no }
    in
    let rep =
      Caqti_type.(
        tup3 (tup4 string int int int)
          (tup4 int64 int64 int64 (option int64))
          (tup4 string int64 int64 int))
    in
    Caqti_type.custom ~encode ~decode rep

  let query =
    Caqti_request.collect Caqti_type.int typ
      {|
         SELECT type,fee_payer_id, source_id,receiver_id,fee,fee_token,token,amount,memo,nonce,global_slot,sequence_no,status FROM

         (SELECT * FROM user_commands WHERE id = ?) AS uc

         INNER JOIN

         blocks_user_commands AS buc

         ON

         uc.id = buc.user_command_id

         INNER JOIN blocks

         ON

         blocks.id = buc.block_id

       |}

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
    ; fee: int64
    ; token: int64
    ; global_slot: int64
    ; sequence_no: int
    ; secondary_sequence_no: int }

  let typ =
    (* chunk into groups so we can use tuple combinators *)
    let encode t =
      Ok
        ( (t.type_, t.receiver_id)
        , (t.fee, t.token)
        , (t.global_slot, t.sequence_no, t.secondary_sequence_no) )
    in
    let decode
        ( (type_, receiver_id)
        , (fee, token)
        , (global_slot, sequence_no, secondary_sequence_no) ) =
      Ok
        { type_
        ; receiver_id
        ; fee
        ; token
        ; global_slot
        ; sequence_no
        ; secondary_sequence_no }
    in
    let rep =
      Caqti_type.(
        tup3 (tup2 string int) (tup2 int64 int64) (tup3 int64 int int))
    in
    Caqti_type.custom ~encode ~decode rep

  let query =
    Caqti_request.collect Caqti_type.int typ
      {|
         SELECT type,receiver_id,fee,token,global_slot,sequence_no,secondary_sequence_no FROM

         (SELECT * FROM internal_commands WHERE id = ?) AS ic

         INNER JOIN

         blocks_internal_commands AS bic

         ON

         ic.id = bic.internal_command_id

         INNER JOIN

         blocks

         ON blocks.id = bic.block_id

       |}

  let run (module Conn : Caqti_async.CONNECTION) internal_cmd_id =
    Conn.collect_list query internal_cmd_id
end

module Public_key = struct
  let query =
    Caqti_request.find_opt Caqti_type.int Caqti_type.string
      {|
         SELECT value FROM public_keys WHERE id = ?
       |}

  let run (module Conn : Caqti_async.CONNECTION) pk_id =
    Conn.find_opt query pk_id
end
