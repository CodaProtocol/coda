(* swap_bad_balances.ml -- swap bad balances for combined fee transfers *)

open Core_kernel
open Async

let query_db pool ~f ~item =
  match%bind Caqti_async.Pool.use f pool with
  | Ok v ->
      return v
  | Error msg ->
      failwithf "Error getting %s from db, error: %s" item
        (Caqti_error.show msg) ()

let main ~archive_uri ~state_hash ~sequence_no () =
  let archive_uri = Uri.of_string archive_uri in
  let logger = Logger.create () in
  match Caqti_async.connect_pool archive_uri with
  | Error e ->
      [%log fatal]
        ~metadata:[("error", `String (Caqti_error.show e))]
        "Failed to create a Caqti connection to Postgresql" ;
      exit 1
  | Ok pool ->
      [%log info] "Successfully created Caqti connection to Postgresql" ;
      let query_db ~item ~f = query_db pool ~item ~f in
      let%bind receiver_balance_ids =
        query_db
          ~f:(fun db ->
            Sql.Receiver_balances.run_ids_from_fee_transfer db state_hash
              sequence_no )
          ~item:"receiver balance ids"
      in
      if List.length receiver_balance_ids <> 2 then (
        [%log fatal]
          "Expected two receiver balance ids, one for each fee transfer" ;
        Core_kernel.exit 1 ) ;
      let balance_1_id, balance_2_id =
        match receiver_balance_ids with
        | [id1; id2] ->
            (id1, id2)
        | _ ->
            failwith "Wrong number of balance ids"
      in
      let%bind balance_1 =
        query_db
          ~f:(fun db -> Sql.Receiver_balances.load db balance_1_id)
          ~item:"receiver balance 1"
      in
      let%bind balance_2 =
        query_db
          ~f:(fun db -> Sql.Receiver_balances.load db balance_2_id)
          ~item:"receiver balance 2"
      in
      let balance_to_yojson (pk_id, bal_int64) =
        let bal_json =
          Unsigned.UInt64.of_int64 bal_int64
          |> Currency.Balance.of_uint64 |> Currency.Balance.to_yojson
        in
        `Assoc [("public_key_id", `Int pk_id); ("balance", bal_json)]
      in
      [%log info] "Found balances to be swapped"
        ~metadata:
          [ ("balance_1", balance_to_yojson balance_1)
          ; ("balance_2", balance_to_yojson balance_2) ] ;
      let balance_1_swapped, balance_2_swapped =
        match (balance_1, balance_2) with
        | (pk1, bal1), (pk2, bal2) ->
            ((pk1, bal2), (pk2, bal1))
      in
      let%bind new_balance_id_1 =
        query_db
          ~f:(fun db ->
            Sql.Receiver_balances.add_if_doesn't_exist db balance_1_swapped )
          ~item:"receiver balance 1 swapped"
      in
      [%log info] "New balance id for balance 1: %d" new_balance_id_1 ;
      let%bind new_balance_id_2 =
        query_db
          ~f:(fun db ->
            Sql.Receiver_balances.add_if_doesn't_exist db balance_2_swapped )
          ~item:"receiver balance 2 swapped"
      in
      [%log info] "New balance id for balance 2: %d" new_balance_id_2 ;
      [%log info] "Swapping in new balance 1" ;
      let%bind () =
        query_db
          ~f:(fun db ->
            Sql.Receiver_balances.swap_in_new_balance db state_hash sequence_no
              balance_1_id new_balance_id_1 )
          ~item:"balance 1 swap"
      in
      [%log info] "Swapping in new balance 2" ;
      let%bind () =
        query_db
          ~f:(fun db ->
            Sql.Receiver_balances.swap_in_new_balance db state_hash sequence_no
              balance_2_id new_balance_id_2 )
          ~item:"balance 2 swap"
      in
      Deferred.unit

let () =
  Command.(
    run
      (let open Let_syntax in
      async ~summary:"Swap bad balances for combined fee transfers"
        (let%map archive_uri =
           Param.flag "--archive-uri" ~aliases:["archive-uri"]
             ~doc:
               "URI URI for connecting to the archive database (e.g., \
                postgres://$USER@localhost:5432/archiver)"
             Param.(required string)
         and state_hash =
           Param.(flag "--state-hash" ~aliases:["state-hash"])
             ~doc:
               "STATE-HASH State hash of the block containing the combined \
                fee transfer"
             Param.(required string)
         and sequence_no =
           Param.(flag "--sequence-no" ~aliases:["sequence-no"])
             ~doc:"NN Sequence number of the two fee transfers"
             Param.(required int)
         in
         main ~archive_uri ~state_hash ~sequence_no)))
