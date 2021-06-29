(** An agent that pokes at Coda and peeks at Rosetta to see if things look alright *)

open Core_kernel
open Async
open Rosetta_lib
open Lib

(* Rosetta_models.Currency shadows our Currency so we "save" it as MinaCurrency first *)
module MinaCurrency = Currency
open Rosetta_models

module Error = struct
  include Error

  let equal e1 e2 = Yojson.Safe.equal (Error.to_yojson e1) (Error.to_yojson e2)
end

let other_pk = "B62qoDWfBZUxKpaoQCoFqr12wkaY84FrhxXNXzgBkMUi2Tz4K8kBDiv"

let snark_pk = "B62qjnkjj3zDxhEfxbn1qZhUawVeLsUr2GCzEz8m1MDztiBouNsiMUL"

let timelocked_pk = "B62qpJDprqj1zjNLf4wSpFC6dqmLzyokMy6KtMLSvkU8wfdL1midEb4"

let wait span = Async.after span |> Deferred.map ~f:Result.return

(* Keep trying to run `step` `retry_count` many times initially waiting for `initial_delay` and each time waiting `each_delay` *)
let keep_trying ~step ~retry_count ~initial_delay ~each_delay ~failure_reason =
  let open Deferred.Result.Let_syntax in
  let rec go = function
    | 0 ->
        Deferred.Result.fail
          (Errors.create ~context:failure_reason `Invariant_violation)
    | i -> (
        match%bind step () with
        | `Succeeded a ->
            return a
        | `Failed ->
            let%bind () = wait each_delay in
            go (i - 1) )
  in
  let%bind () = wait initial_delay in
  go retry_count

let get_last_block_index ~rosetta_uri ~network_response ~logger =
  let open Core.Time in
  let open Deferred.Result.Let_syntax in
  keep_trying
    ~step:(fun () ->
      let%map block_r =
        Peek.Block.newest_block ~rosetta_uri ~network_response ~logger
      in
      match
        Result.map block_r ~f:(fun block ->
            (Option.value_exn block.Block_response.block).block_identifier
              .index )
      with
      | Error _ ->
          `Failed
      | Ok index ->
          `Succeeded index )
    ~retry_count:10 ~initial_delay:(Span.of_ms 0.0)
    ~each_delay:(Span.of_ms 250.0)
    ~failure_reason:"Took too long for the last block to be fetched"

let verify_in_mempool_and_block ~logger ~rosetta_uri ~graphql_uri
    ~network_response ~txn_hash ~operation_expectations =
  let open Core.Time in
  let open Deferred.Result.Let_syntax in
  let%bind () = wait (Span.of_sec 1.0) in
  (* Grab the mempool and find the user command inside *)
  let%bind () =
    keep_trying
      ~step:(fun () ->
        let%map mempool_r =
          Peek.Mempool.mempool ~rosetta_uri ~network_response ~logger
        in
        match
          Result.map mempool_r ~f:(fun mempool ->
              List.find mempool.Mempool_response.transaction_identifiers
                ~f:(fun ident ->
                  String.equal ident.Transaction_identifier.hash txn_hash ) )
        with
        | Error _ ->
            `Failed
        | Ok None ->
            `Failed
        | Ok (Some _) ->
            `Succeeded () )
      ~retry_count:5 ~initial_delay:(Span.of_ms 100.0)
      ~each_delay:(Span.of_sec 3.0)
      ~failure_reason:"Took too long to appear in mempool"
  in
  (* Pull specific account out of mempool *)
  let%bind mempool_res =
    Peek.Mempool.transaction ~rosetta_uri ~network_response ~logger
      ~hash:txn_hash
  in
  [%log debug]
    ~metadata:
      [ ( "operations"
        , mempool_res.transaction.operations |> [%to_yojson: Operation.t list]
        ) ]
    "Mempool operations: $operations" ;
  let%bind () =
    Operation_expectation.assert_similar_operations ~logger
      ~expected:
        ( operation_expectations
        |> List.filter ~f:(fun op ->
               not
                 (String.equal op.Operation_expectation._type
                    "fee_receiver_inc") ) )
      ~actual:mempool_res.transaction.operations ~situation:"mempool"
  in
  [%log info] "Verified mempool operations" ;
  let%bind last_block_index =
    get_last_block_index ~rosetta_uri ~network_response ~logger
  in
  [%log debug]
    ~metadata:[("index", `Intlit (Int64.to_string last_block_index))]
    "Found block index $index" ;
  (* Start staking so we get blocks *)
  let%bind _res = Poke.Staking.enable ~graphql_uri in
  (* Wait until the newest block has index > last_block_index and has at least one user command *)
  let%bind block =
    keep_trying
      ~step:(fun () ->
        let%map block_r =
          Peek.Block.newest_block ~rosetta_uri ~network_response ~logger
        in
        match
          Result.map block_r ~f:(fun block ->
              let newer_block : bool =
                Int64.(
                  (Option.value_exn block.Block_response.block)
                    .block_identifier
                    .index > last_block_index)
              in
              let has_user_command : bool =
                (* HACK: First transaction is always an internal command and second, if present, is always a user
                 * command, so we can just check that length > 1 *)
                Int.(
                  List.length
                    (Option.value_exn block.Block_response.block).transactions
                  > 1)
              in
              if newer_block && has_user_command then Some block else None )
        with
        | Error _ ->
            `Failed
        | Ok None ->
            `Failed
        | Ok (Some block) ->
            `Succeeded block )
      ~retry_count:20 ~initial_delay:(Span.of_ms 250.0)
      ~each_delay:(Span.of_ms 500.0)
      ~failure_reason:"Took too long for a block to be created"
  in
  [%log debug]
    ~metadata:
      [ ( "index"
        , `Intlit
            (Int64.to_string
               (Option.value_exn block.Block_response.block).block_identifier
                 .index) ) ]
    "Waited for the next block index $index" ;
  (* Stop noisy block production *)
  let%bind _res = Poke.Staking.disable ~graphql_uri in
  let successful (x : Operation_expectation.t) = {x with status= "Success"} in
  [%log debug]
    ~metadata:
      [ ( "transactions"
        , [%to_yojson: Rosetta_models.Transaction.t list]
            (Option.value_exn block.block).transactions ) ]
    "Asserting that operations are similar in block. Transactions $transactions" ;
  Operation_expectation.assert_similar_operations ~logger
    ~expected:
      ( List.map ~f:successful operation_expectations
      @ Operation_expectation.
          [ { amount= Some 40_000_000_000
            ; account=
                Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
            ; status= "Success"
            ; _type= "coinbase_inc"
            ; target= `Check None } ] )
    ~actual:
      ( List.map (Option.value_exn block.block).transactions ~f:(fun txn ->
            txn.operations )
      |> List.join )
    ~situation:"block"

let direct_graphql_payment_through_block ~logger ~rosetta_uri ~graphql_uri
    ~network_response =
  let open Deferred.Result.Let_syntax in
  (* Unlock the account *)
  let%bind _ = Poke.Account.unlock ~graphql_uri in
  (* Send a payment *)
  let%bind hash =
    Poke.SendTransaction.payment ~fee:(`Int 2_000_000_000)
      ~amount:(`Int 5_000_000_000) ~to_:(`String other_pk) ~graphql_uri ()
  in
  verify_in_mempool_and_block ~logger ~rosetta_uri ~graphql_uri ~txn_hash:hash
    ~network_response
    ~operation_expectations:
      Operation_expectation.
        [ { amount= Some (-5_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "payment_source_dec"
          ; target= `Check None }
        ; { amount= Some 5_000_000_000
          ; account=
              Some {Account.pk= other_pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "payment_receiver_inc"
          ; target= `Check None }
        ; { amount= Some (-2_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Check None }
        ; { amount= Some 2_000_000_000
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_receiver_inc"
          ; target= `Check None } ]

let direct_graphql_no_account_fee_through_block ~logger ~rosetta_uri
    ~graphql_uri ~network_response =
  let open Deferred.Result.Let_syntax in
  (* Unlock the account *)
  let%bind _ = Poke.Account.unlock ~graphql_uri in
  let fresh_pk = "B62qokqG3ueJmkj7zXaycV31tnG6Bbg3E8tDS5vkukiFic57rgstTbb" in
  (* Send a payment *)
  let%bind hash =
    Poke.SendTransaction.payment ~fee:(`Int 7_000_000_000) ~amount:(`Int 1_000)
      ~to_:(`String fresh_pk) ~graphql_uri ()
  in
  verify_in_mempool_and_block ~logger ~rosetta_uri ~graphql_uri ~txn_hash:hash
    ~network_response
    ~operation_expectations:
      Operation_expectation.
        [ { amount= Some (-7_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Ignore } ]

let direct_graphql_delegation_through_block ~logger ~rosetta_uri ~graphql_uri
    ~network_response =
  let open Deferred.Result.Let_syntax in
  (* Unlock the account *)
  let%bind _ = Poke.Account.unlock ~graphql_uri in
  (* Delegate stake *)
  let%bind hash =
    Poke.SendTransaction.delegation ~fee:(`Int 2_000_000_000)
      ~to_:(`String other_pk) ~graphql_uri ()
  in
  verify_in_mempool_and_block ~logger ~rosetta_uri ~graphql_uri ~txn_hash:hash
    ~network_response
    ~operation_expectations:
      Operation_expectation.
        [ { amount= None
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "delegate_change"
          ; target= `Check (Some other_pk) }
        ; { amount= Some (-2_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Check None } ]

let direct_graphql_create_token_through_block ~logger ~rosetta_uri ~graphql_uri
    ~network_response =
  let open Deferred.Result.Let_syntax in
  (* Unlock the sender account *)
  let%bind _ = Poke.Account.unlock ~graphql_uri in
  (* create token *)
  let%bind hash =
    Poke.SendTransaction.create_token ~fee:(`Int 2_000_000_000)
      ~receiver:(`String other_pk) ~graphql_uri ()
  in
  verify_in_mempool_and_block ~logger ~rosetta_uri ~graphql_uri ~txn_hash:hash
    ~network_response
    ~operation_expectations:
      Operation_expectation.
        [ { amount= Some (-2_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Check None }
        ; { amount= None
          ; account= None
          ; status= "Pending"
          ; _type= "create_token"
          ; target= `Check None } ]

let direct_graphql_create_token_account_through_block ~logger ~rosetta_uri
    ~graphql_uri ~network_response =
  let open Deferred.Result.Let_syntax in
  (* Unlock the account *)
  let%bind _ = Poke.Account.unlock ~graphql_uri in
  (* Create token account *)
  let%bind hash =
    Poke.SendTransaction.create_token_account ~fee:(`Int 2_000_000_000)
      ~receiver:other_pk ~token:(`String "2") ~graphql_uri ()
  in
  verify_in_mempool_and_block ~logger ~rosetta_uri ~graphql_uri ~txn_hash:hash
    ~network_response
    ~operation_expectations:
      Operation_expectation.
        [ { amount= Some (-2_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Check None } ]

let construction_api_transaction_through_mempool ~logger ~rosetta_uri
    ~graphql_uri ~network_response ~operation_expectations ~operations =
  let open Deferred.Result.Let_syntax in
  let keys =
    Signer.Keys.of_private_key_box
      {| {"box_primitive":"xsalsa20poly1305","pw_primitive":"argon2i","nonce":"8jGuTAxw3zxtWasVqcD1H6rEojHLS1yJmG3aHHd","pwsalt":"AiUCrMJ6243h3TBmZ2rqt3Voim1Y","pwdiff":[134217728,6],"ciphertext":"DbAy736GqEKWe9NQWT4yaejiZUo9dJ6rsK7cpS43APuEf5AH1Qw6xb1s35z8D2akyLJBrUr6m"} |}
  in
  let%bind derive_res =
    Offline.Derive.req ~logger ~rosetta_uri ~network_response
      ~public_key_hex_bytes:keys.public_key_hex_bytes
  in
  let operations =
    operations (Option.value_exn derive_res.account_identifier)
  in
  let%bind preprocess_res =
    Offline.Preprocess.req ~logger ~rosetta_uri ~network_response
      ~max_fee:(Unsigned.UInt64.of_int 100_000_000_000)
      ~operations
  in
  let%bind metadata_res =
    Peek.Construction.metadata ~rosetta_uri ~network_response ~logger
      ~options:preprocess_res.options
  in
  [%log debug]
    ~metadata:[("res", Construction_metadata_response.to_yojson metadata_res)]
    "Construction_metadata result $res" ;
  let%bind payloads_res =
    Offline.Payloads.req ~logger ~rosetta_uri ~network_response ~operations
      ~metadata:metadata_res.metadata
  in
  let%bind payloads_parse_res =
    Offline.Parse.req ~logger ~rosetta_uri ~network_response
      ~transaction:
        (`Unsigned
          payloads_res.Construction_payloads_response.unsigned_transaction)
  in
  if not ([%equal: Operation.t list] operations payloads_parse_res.operations)
  then (
    [%log debug]
      ~metadata:
        [ ("expected", [%to_yojson: Operation.t list] operations)
        ; ( "actual"
          , [%to_yojson: Operation.t list] payloads_parse_res.operations ) ]
      "Construction_parse : Expected $expected, after payloads+parse $actual" ;
    failwith "Operations are not equal before and after payloads+parse" ) ;
  let%bind signature =
    Signer.sign ~keys
      ~unsigned_transaction_string:payloads_res.unsigned_transaction
    |> Deferred.return
  in
  let%bind combine_res =
    Offline.Combine.req ~logger ~rosetta_uri ~network_response ~signature
      ~unsigned_transaction:payloads_res.unsigned_transaction
      ~public_key_hex_bytes:keys.public_key_hex_bytes
      ~account_id:(Option.value_exn derive_res.account_identifier)
  in
  let%bind combine_parse_res =
    Offline.Parse.req ~logger ~rosetta_uri ~network_response
      ~transaction:
        (`Signed combine_res.Construction_combine_response.signed_transaction)
  in
  if not ([%equal: Operation.t list] operations combine_parse_res.operations)
  then (
    [%log debug]
      ~metadata:
        [ ("expected", [%to_yojson: Operation.t list] operations)
        ; ( "actual"
          , [%to_yojson: Operation.t list] combine_parse_res.operations ) ]
      "Construction_combine : Expected $expected, after combine+parse $actual" ;
    failwith "Operations are not equal before and after combine+parse" ) ;
  let%bind hash_res =
    Offline.Hash.req ~logger ~rosetta_uri ~network_response
      ~signed_transaction:combine_res.signed_transaction
  in
  let%bind verified_bool =
    Signer.verify ~public_key_hex_bytes:keys.public_key_hex_bytes
      ~signed_transaction_string:combine_res.signed_transaction
    |> Deferred.return
  in
  let%bind () =
    if verified_bool then return ()
    else
      Deferred.Result.fail
        (Errors.create ~context:"Bad signature created during construction"
           `Invariant_violation)
  in
  let%bind submit_res =
    Peek.Construction.submit ~logger ~rosetta_uri ~network_response
      ~signed_transaction:combine_res.signed_transaction
  in
  assert (
    String.equal hash_res.Construction_hash_response.transaction_hash
      submit_res.transaction_identifier.hash ) ;
  [%log debug] "Construction_submit is finalized" ;
  verify_in_mempool_and_block ~logger ~rosetta_uri ~graphql_uri
    ~txn_hash:hash_res.transaction_hash ~network_response
    ~operation_expectations

let construction_api_payment_through_mempool =
  construction_api_transaction_through_mempool
    ~operations:(fun account_id ->
      Poke.SendTransaction.payment_operations ~from:account_id.address
        ~fee:(Unsigned.UInt64.of_int 3_000_000_000)
        ~amount:(Unsigned.UInt64.of_int 10_000_000_000)
        ~to_:other_pk )
    ~operation_expectations:
      Operation_expectation.
        [ { amount= Some (-10_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "payment_source_dec"
          ; target= `Check None }
        ; { amount= Some 10_000_000_000
          ; account=
              Some {Account.pk= other_pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "payment_receiver_inc"
          ; target= `Check None }
        ; { amount= Some (-3_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Check None } ]

let construction_api_delegation_through_mempool =
  construction_api_transaction_through_mempool
    ~operations:(fun account_id ->
      Poke.SendTransaction.delegation_operations ~from:account_id.address
        ~fee:(Unsigned.UInt64.of_int 5_000_000_000)
        ~to_:other_pk )
    ~operation_expectations:
      Operation_expectation.
        [ { amount= None
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "delegate_change"
          ; target= `Check (Some other_pk) }
        ; { amount= Some (-5_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Check None } ]

let construction_api_create_token_through_mempool =
  construction_api_transaction_through_mempool
    ~operations:(fun account_id ->
      Poke.SendTransaction.create_token_operations ~sender:account_id.address
        ~fee:(Unsigned.UInt64.of_int 5_000_000_000) )
    ~operation_expectations:
      Operation_expectation.
        [ { amount= Some (-5_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Check None }
        ; { amount= None
          ; account= None
          ; status= "Pending"
          ; _type= "create_token"
          ; target= `Check None } ]

let construction_api_create_token_account_through_mempool =
  construction_api_transaction_through_mempool
    ~operations:(fun account_id ->
      Poke.SendTransaction.create_token_operations ~sender:account_id.address
        ~fee:(Unsigned.UInt64.of_int 5_000_000_000) )
    ~operation_expectations:
      Operation_expectation.
        [ { amount= Some (-5_000_000_000)
          ; account=
              Some {Account.pk= Poke.pk; token_id= Unsigned.UInt64.of_int 1}
          ; status= "Pending"
          ; _type= "fee_payer_dec"
          ; target= `Check None } ]

let get_consensus_constants ~logger :
    Consensus.Constants.t Or_error.t Deferred.t =
  let open Deferred.Or_error.Let_syntax in
  let conf_dir = "/tmp" in
  let genesis_dir =
    let home = Core.Sys.home_directory () in
    Filename.concat home ".coda-config"
  in
  let config_file =
    match Sys.getenv "CODA_CONFIG_FILE" with
    | Some config_file ->
        config_file
    | None ->
        Filename.concat conf_dir "config.json"
  in
  let%bind config =
    let%map config_json = Genesis_ledger_helper.load_config_json config_file in
    match Runtime_config.of_yojson config_json with
    | Ok config ->
        config
    | Error err ->
        failwithf "Could not parse configuration: %s" err ()
  in
  let%map proof, _ =
    Genesis_ledger_helper.init_from_config_file ~genesis_dir ~logger
      ~proof_level:None config
  in
  Precomputed_values.consensus_constants proof

let historical_balance_check ~logger ~rosetta_uri ~network_response =
  let open Core.Time in
  let open Deferred.Result.Let_syntax in
  let%bind consensus_constants =
    Deferred.Result.map_error (get_consensus_constants ~logger) ~f:(fun _ ->
        Errors.create ~context:"Failed to get consensus constants"
          `Invariant_violation )
  in
  let%bind last_block_index =
    get_last_block_index ~rosetta_uri ~network_response ~logger
  in
  (* TODO(omerzach): We need to test for more complex accounts that involve
   *   internal and user commands too *)
  let account_identifier = Account_identifier.create timelocked_pk in
  let index_to_slot (index : int64) =
    keep_trying
      ~step:(fun () ->
        let%map block_r =
          Peek.Block.block_at_index ~index ~rosetta_uri ~network_response
            ~logger
        in
        let block_r : (Block_response.t, Rosetta_models.Error.t) result =
          block_r
        in
        match
          Result.map block_r ~f:(fun block ->
              let block = Option.value_exn block.Block_response.block in
              let block_time : Block_time.t =
                Block_time.of_int64 block.timestamp
              in
              let consensus_time : Consensus.Data.Consensus_time.t =
                Consensus.Data.Consensus_time.of_time_exn
                  ~constants:consensus_constants block_time
              in
              Consensus.Data.Consensus_time.to_global_slot consensus_time )
        with
        | Error _ ->
            `Failed
        | Ok slot ->
            `Succeeded slot )
      ~retry_count:10 ~initial_delay:(Span.of_ms 0.0)
      ~each_delay:(Span.of_ms 250.0)
      ~failure_reason:
        (sprintf "Took too long for block %s to be fetched"
           (Int64.to_string index))
  in
  let expected_balance_at_slot (slot : int64) =
    let open Unsigned in
    let global_slot = UInt32.of_int (Int.of_int64_exn slot) in
    let cliff_time = UInt32.of_int 20 in
    let cliff_amount = MinaCurrency.Amount.of_int 2_000_000_000_000 in
    let vesting_period = Unsigned.UInt32.of_int 5 in
    let vesting_increment = MinaCurrency.Amount.of_int 10_000_000_000 in
    let initial_minimum_balance =
      MinaCurrency.Balance.of_int 5_000_000_000_000
    in
    let total_balance = 10_000_000_000_000 in
    let min_balance_at_slot =
      Mina_base.Account.min_balance_at_slot ~global_slot ~cliff_time
        ~cliff_amount ~vesting_period ~vesting_increment
        ~initial_minimum_balance
      |> MinaCurrency.Balance.to_int
    in
    total_balance - min_balance_at_slot
  in
  let check_balance_at_index (index : int64) ~logger =
    let open Deferred.Result.Let_syntax in
    let%bind slot = index_to_slot index in
    let slot = slot |> Unsigned.UInt32.to_int64 |> Int64.of_int64 in
    let expected_balance = expected_balance_at_slot slot in
    let%bind actual_balance =
      keep_trying
        ~step:(fun () ->
          let%map balance_r =
            Peek.Account_balance.balance_at_index ~account_identifier ~index
              ~rosetta_uri ~network_response ~logger
          in
          match balance_r with
          | Ok {balances= [{value; _}]; _} ->
              `Succeeded
                ( value |> MinaCurrency.Balance.of_string
                |> MinaCurrency.Balance.to_int )
          | _ ->
              `Failed )
        ~retry_count:5 ~initial_delay:(Span.of_ms 100.0)
        ~each_delay:(Span.of_sec 3.0)
        ~failure_reason:
          (sprintf "Took too long to look up balance for index %s"
             (Int64.to_string index))
    in
    assert (Int.(expected_balance = actual_balance)) ;
    Deferred.Result.return ()
  in
  let rec check_balances_until ~(until_index : int64)
      ~(last_index_checked : int64) =
    if Int64.(last_index_checked >= until_index) then Deferred.Result.return ()
    else
      let next_index = Int64.(last_index_checked + of_int 1) in
      let%bind () = check_balance_at_index next_index ~logger in
      let%bind () =
        check_balances_until ~until_index ~last_index_checked:next_index
      in
      Deferred.Result.return ()
  in
  check_balances_until ~until_index:last_block_index
    ~last_index_checked:(Int64.of_int 0)

(* for each possible user command, run the command via GraphQL, check that
    the command is in the transaction pool
*)
let check_new_account_user_commands ~logger ~rosetta_uri ~graphql_uri =
  let open Core.Time in
  let open Deferred.Result.Let_syntax in
  (* Stop staking so we can rely on things being in the mempool *)
  let%bind _res = Poke.Staking.disable ~graphql_uri in
  (* Figure out our network identifier *)
  let%bind network_response = Peek.Network.list ~rosetta_uri ~logger in
  (* Wait until we are "synced" -- on debug nets this is when block production begins *)
  let%bind () =
    keep_trying
      ~step:(fun () ->
        let status_r_dr =
          Peek.Network.status ~rosetta_uri ~network_response ~logger
        in
        let%map status_r = status_r_dr in
        if
          [%equal: (string option, Error.t) result]
            (Result.map status_r ~f:(fun status ->
                 Option.bind status.Network_status_response.sync_status
                   ~f:(fun sync_status -> sync_status.stage) ))
            (Ok (Some "Synced"))
        then `Succeeded ()
        else `Failed )
      ~retry_count:15 ~initial_delay:(Span.of_sec 0.5)
      ~each_delay:(Span.of_sec 1.0) ~failure_reason:"Took too long to sync"
  in
  (* Directly create a payment in graphql and make sure it's in the mempool
       * properly, and then in a block properly *)
  [%log info] "Starting payment check" ;
  let%bind () =
    direct_graphql_payment_through_block ~logger ~rosetta_uri ~graphql_uri
      ~network_response
  in
  [%log info] "Created payment and waited" ;
  (* Stop staking so we can rely on things being in the mempool again *)
  let%bind _res = Poke.Staking.disable ~graphql_uri in
  [%log info] "Starting payment (no account fee) check" ;
  let%bind () =
    direct_graphql_no_account_fee_through_block ~logger ~rosetta_uri
      ~graphql_uri ~network_response
  in
  [%log info] "Created payment (no account fee) and waited" ;
  (* Stop staking so we can rely on things being in the mempool again *)
  let%bind _res = Poke.Staking.disable ~graphql_uri in
  (* Follow the full construction API flow and make sure the submitted
   * transaction appears in the mempool *)
  [%log info] "Starting construction payment check" ;
  let%bind () =
    construction_api_payment_through_mempool ~logger ~rosetta_uri ~graphql_uri
      ~network_response
  in
  [%log info] "Created construction payment and waited" ;
  (* Stop staking *)
  [%log info] "Starting delegation check" ;
  let%bind _res = Poke.Staking.disable ~graphql_uri in
  let%bind () =
    direct_graphql_delegation_through_block ~logger ~rosetta_uri ~graphql_uri
      ~network_response
  in
  [%log info] "Created graphql delegation and waited" ;
  (* Stop staking *)
  [%log info] "Starting construction delegation check" ;
  let%bind _res = Poke.Staking.disable ~graphql_uri in
  let%bind () =
    construction_api_delegation_through_mempool ~logger ~rosetta_uri
      ~graphql_uri ~network_response
  in
  [%log info] "Created construction delegation and waited" ;
  (* Stop staking *)
  [%log info] "Starting create token check" ;
  let%bind _res = Poke.Staking.disable ~graphql_uri in
  let%bind () =
    direct_graphql_create_token_through_block ~logger ~rosetta_uri ~graphql_uri
      ~network_response
  in
  [%log info] "Created token via graphql and waited" ;
  [%log info] "Starting create token construction check" ;
  let%bind () =
    construction_api_create_token_through_mempool ~logger ~rosetta_uri
      ~graphql_uri ~network_response
  in
  [%log info] "Created token using construction and waited" ;
  (* Stop staking *)
  [%log info] "Starting create token account check" ;
  let%bind _res = Poke.Staking.disable ~graphql_uri in
  let%bind () =
    direct_graphql_create_token_account_through_block ~logger ~rosetta_uri
      ~graphql_uri ~network_response
  in
  [%log info] "Created token account and waited" ;
  [%log info] "Starting construction create token account check" ;
  let%bind () =
    construction_api_create_token_account_through_mempool ~logger ~rosetta_uri
      ~graphql_uri ~network_response
  in
  [%log info] "Created token account using construction and waited" ;
  [%log info] "Starting historical balance check" ;
  let%bind _ =
    historical_balance_check ~logger ~rosetta_uri ~network_response
  in
  [%log info] "Finished historical balance check" ;
  (* Stop staking *)
  (* Success *)
  return ()

let run ~logger ~rosetta_uri ~graphql_uri ~don't_exit =
  let open Core.Time in
  let open Deferred.Result.Let_syntax in
  let%bind () =
    check_new_account_user_commands ~logger ~rosetta_uri ~graphql_uri
  in
  [%log info] "Finished running test-agent" ;
  if don't_exit then (
    let%bind _res = Poke.Staking.enable ~graphql_uri in
    [%log info] "Running forever with more blocks" ;
    let rec go () =
      let%bind () = wait (Span.of_sec 1.0) in
      go ()
    in
    go () )
  else (
    [%log info] "Exiting" ;
    return () )

let command =
  let open Command.Let_syntax in
  let%map_open rosetta_uri =
    flag "--rosetta-uri" ~aliases:["rosetta-uri"]
      ~doc:"URI of Rosetta endpoint to connect to" Cli.required_uri
  and graphql_uri =
    flag "--graphql-uri" ~aliases:["graphql-uri"]
      ~doc:"URI of Coda GraphQL endpoint to connect to" Cli.required_uri
  and log_json =
    flag "--log-json" ~aliases:["log-json"]
      ~doc:"Print log output as JSON (default: plain text)" no_arg
  and log_level =
    flag "--log-level" ~aliases:["log-level"]
      ~doc:"Set log level (default: Info)" Cli.log_level
  and don't_exit =
    flag "--dont-exit" ~aliases:["dont-exit"]
      ~doc:"Don't exit after tests finish (default: do exit)" no_arg
  in
  let open Deferred.Let_syntax in
  fun () ->
    let logger = Logger.create () in
    Cli.logger_setup log_json log_level ;
    [%log info] "Rosetta test-agent starting" ;
    match%bind run ~logger ~rosetta_uri ~graphql_uri ~don't_exit with
    | Ok () ->
        [%log info] "Rosetta test-agent stopping successfully" ;
        return ()
    | Error e ->
        [%log error] "Rosetta test-agent stopping with a failure: %s"
          (Errors.show e) ;
        exit 1

let () =
  Command.run
    (Command.async ~summary:"Run agent to poke at Coda and peek at Rosetta"
       command)
