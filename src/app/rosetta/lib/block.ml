open Core_kernel
open Async
open Rosetta_lib
open Rosetta_models

module Get_coinbase_and_genesis =
[%graphql
{|
  query {
    genesisBlock {
      creatorAccount {
        publicKey @bsDecoder(fn: "Decoders.public_key")
      }
      winnerAccount {
        publicKey @bsDecoder(fn: "Decoders.public_key")
      }
      protocolState {
        blockchainState {
          date
        }
      }
      stateHash
    }
    daemonStatus {
      chainId
    }
    initialPeers
  }
|}]

let account_id = User_command_info.account_id

module Block_query = struct
  type t = ([`Height of int64], [`Hash of string]) These.t option

  module T (M : Monad_fail.S) = struct
    let of_partial_identifier (identifier : Partial_block_identifier.t) =
      match (identifier.index, identifier.hash) with
      | None, None ->
          M.return None
      | Some index, None ->
          M.return (Some (`This (`Height index)))
      | None, Some hash ->
          M.return (Some (`That (`Hash hash)))
      | Some index, Some hash ->
          M.return (Some (`Those (`Height index, `Hash hash)))

    let is_genesis ~hash = function
      | Some (`This (`Height index)) ->
          Int64.equal index Network.genesis_block_height
      | Some (`That (`Hash hash')) ->
          String.equal hash hash'
      | Some (`Those (`Height index, `Hash hash')) ->
          Int64.equal index Network.genesis_block_height
          && String.equal hash hash'
      | None ->
          false
  end
end

module Op = User_command_info.Op

(* TODO: Populate postgres DB with at least one of each kind of transaction and then make sure ops make sense: #5501 *)

module Internal_command_info = struct
  module Kind = struct
    type t = [`Coinbase | `Fee_transfer | `Fee_transfer_via_coinbase]
    [@@deriving eq, to_yojson]
  end

  type t =
    { kind: Kind.t
    ; receiver: [`Pk of string]
    ; fee: Unsigned_extended.UInt64.t
    ; token: Unsigned_extended.UInt64.t
    ; hash: string }
  [@@deriving to_yojson]

  module T (M : Monad_fail.S) = struct
    module Op_build = Op.T (M)

    let to_operations ~coinbase_receiver (t : t) :
        (Operation.t list, Errors.t) M.t =
      (* We choose to represent the dec-side of fee transfers from txns from the
       * canonical user command that created them so we are able consistently
       * produce more balance changing operations in the mempool or a block.
       * *)
      let plan : 'a Op.t list =
        match t.kind with
        | `Coinbase ->
            (* The coinbase transaction is really incrementing by the coinbase
           * amount  *)
            [{Op.label= `Coinbase_inc; related_to= None}]
        | `Fee_transfer ->
            [{Op.label= `Fee_receiver_inc; related_to= None}]
        | `Fee_transfer_via_coinbase ->
            [ {Op.label= `Fee_receiver_inc; related_to= None}
            ; {Op.label= `Fee_payer_dec; related_to= Some `Fee_receiver_inc} ]
      in
      Op_build.build
        ~a_eq:[%eq: [`Coinbase_inc | `Fee_payer_dec | `Fee_receiver_inc]] ~plan
        ~f:(fun ~related_operations ~operation_identifier op ->
          (* All internal commands succeed if they're in blocks *)
          let status = Some (Operation_statuses.name `Success) in
          match op.label with
          | `Coinbase_inc ->
              M.return
                { Operation.operation_identifier
                ; related_operations
                ; status
                ; account=
                    Some (account_id t.receiver Amount_of.Token_id.default)
                ; _type= Operation_types.name `Coinbase_inc
                ; amount= Some (Amount_of.token t.token t.fee)
                ; coin_change= None
                ; metadata= None }
          | `Fee_receiver_inc ->
              M.return
                { Operation.operation_identifier
                ; related_operations
                ; status
                ; account= Some (account_id t.receiver t.token)
                ; _type= Operation_types.name `Fee_receiver_inc
                ; amount= Some (Amount_of.token t.token t.fee)
                ; coin_change= None
                ; metadata= None }
          | `Fee_payer_dec ->
              let open M.Let_syntax in
              let%map coinbase_receiver =
                match coinbase_receiver with
                | Some r ->
                    M.return r
                | None ->
                    M.fail
                      (Errors.create
                         ~context:
                           "This operation existing (fee payer dec within \
                            Internal_command) demands a coinbase receiver to \
                            exist. Please report this bug."
                         `Invariant_violation)
              in
              { Operation.operation_identifier
              ; related_operations
              ; status
              ; account=
                  Some
                    (account_id coinbase_receiver Amount_of.Token_id.default)
              ; _type= Operation_types.name `Fee_payer_dec
              ; amount= Some Amount_of.(negated (coda t.fee))
              ; coin_change= None
              ; metadata= None } )
  end

  let dummies =
    [ { kind= `Coinbase
      ; receiver= `Pk "Eve"
      ; fee= Unsigned.UInt64.of_int 20_000_000_000
      ; token= Unsigned.UInt64.of_int 1
      ; hash= "COINBASE_1" }
    ; { kind= `Fee_transfer
      ; receiver= `Pk "Alice"
      ; fee= Unsigned.UInt64.of_int 30_000_000_000
      ; token= Unsigned.UInt64.of_int 1
      ; hash= "FEE_TRANSFER" } ]
end

module Block_info = struct
  type t =
    { block_identifier: Block_identifier.t
    ; parent_block_identifier: Block_identifier.t
    ; creator: [`Pk of string]
    ; winner: [`Pk of string]
    ; timestamp: int64
    ; internal_info: Internal_command_info.t list
    ; user_commands: User_command_info.t list }

  let creator_metadata {creator= `Pk pk; _} = `Assoc [("creator", `String pk)]

  let block_winner_metadata {winner= `Pk pk; _} =
    `Assoc [("winner", `String pk)]

  let dummy =
    { block_identifier=
        Block_identifier.create (Int64.of_int_exn 4) "STATE_HASH_BLOCK"
    ; creator= `Pk "Alice"
    ; winner= `Pk "Babu"
    ; parent_block_identifier=
        Block_identifier.create (Int64.of_int_exn 3) "STATE_HASH_PARENT"
    ; timestamp= Int64.of_int_exn 1594937771
    ; internal_info= Internal_command_info.dummies
    ; user_commands= User_command_info.dummies }
end

module Sql = struct
  module Block = struct
    module Extras = struct
      let creator (creator, _) = `Pk creator

      let winner (_, winner) = `Pk winner

      let typ = Caqti_type.(tup2 string string)
    end

    let typ = Caqti_type.(tup3 int Archive_lib.Processor.Block.typ Extras.typ)

    let query_height =
      Caqti_request.find_opt Caqti_type.int64 typ
        (* According to the clarification of the Rosetta spec here
         * https://community.rosetta-api.org/t/querying-block-by-just-its-index/84/3 ,
         * it is important to select only the block on the canonical chain for a
         * given height, and not an arbitrary one.
         *
         * This query recursively traverses the blockchain from the longest tip
         * backwards until it reaches a block of the given height. *)
        {|
WITH RECURSIVE chain AS (
  (SELECT id, state_hash, parent_id, parent_hash, creator_id, block_winner_id, snarked_ledger_hash_id, staking_epoch_data_id, next_epoch_data_id, ledger_hash, height, global_slot, global_slot_since_genesis, timestamp FROM blocks b WHERE height = (select MAX(height) from blocks)
  ORDER BY timestamp ASC
  LIMIT 1)

  UNION ALL

  SELECT b.id, b.state_hash, b.parent_id, b.parent_hash, b.creator_id, b.block_winner_id, b.snarked_ledger_hash_id, b.staking_epoch_data_id, b.next_epoch_data_id, b.ledger_hash, b.height, b.global_slot, b.global_slot_since_genesis, b.timestamp FROM blocks b
  INNER JOIN chain
  ON b.id = chain.parent_id AND chain.id <> chain.parent_id
) SELECT c.id, c.state_hash, c.parent_id, c.parent_hash, c.creator_id, c.block_winner_id, c.snarked_ledger_hash_id, c.staking_epoch_data_id, c.next_epoch_data_id, c.ledger_hash, c.height, c.global_slot, c.global_slot_since_genesis, c.timestamp, pk.value as creator, bw.value as winner FROM chain c
  INNER JOIN public_keys pk
  ON pk.id = c.creator_id
  INNER JOIN public_keys bw
  ON bw.id = c.block_winner_id
  WHERE c.height = ?
      |}

    let query_hash =
      Caqti_request.find_opt Caqti_type.string typ
        {| SELECT b.id, b.state_hash, b.parent_id, b.parent_hash, b.creator_id, b.block_winner_id, b.snarked_ledger_hash_id, b.staking_epoch_data_id, b.next_epoch_data_id, b.ledger_hash, b.height, b.global_slot, b.global_slot_since_genesis, b.timestamp, pk.value as creator, bw.value as winner FROM blocks b
        INNER JOIN public_keys pk
        ON pk.id = b.creator_id
        INNER JOIN public_keys bw
        ON bw.id = b.block_winner_id
        WHERE b.state_hash = ? |}

    let query_both =
      Caqti_request.find_opt
        Caqti_type.(tup2 string int64)
        typ
        {| SELECT b.id, b.state_hash, b.parent_id, b.parent_hash, b.creator_id, b.block_winner_id, b.snarked_ledger_hash_id, b.staking_epoch_data_id, b.next_epoch_data_id, b.ledger_hash, b.height, b.global_slot, b.global_slot_since_genesis, b.timestamp, pk.value as creator, bw.value as winner FROM blocks b
        INNER JOIN public_keys pk
        ON pk.id = b.creator_id
        INNER JOIN public_keys bw
        ON bw.id = b.block_winner_id
        WHERE b.state_hash = ? AND b.height = ? |}

    let query_by_id =
      Caqti_request.find_opt Caqti_type.int typ
        {| SELECT b.id, b.state_hash, b.parent_id, b.parent_hash, b.creator_id, b.block_winner_id, b.snarked_ledger_hash_id, b.staking_epoch_data_id, b.next_epoch_data_id, b.ledger_hash, b.height, b.global_slot, b.global_slot_since_genesis, b.timestamp, pk.value as creator, bw.value as winner FROM blocks b
        INNER JOIN public_keys pk
        ON pk.id = b.creator_id
        INNER JOIN public_keys bw
        ON bw.id = b.block_winner_id
        WHERE b.id = ? |}

    let query_best =
      Caqti_request.find_opt Caqti_type.unit typ
        {| SELECT b.id, b.state_hash, b.parent_id, b.parent_hash, b.creator_id, b.block_winner_id, b.snarked_ledger_hash_id, b.staking_epoch_data_id, b.next_epoch_data_id, b.ledger_hash, b.height, b.global_slot, b.global_slot_since_genesis, b.timestamp, pk.value as creator, bw.value as winner FROM blocks b
           INNER JOIN public_keys pk
           ON pk.id = b.creator_id
           INNER JOIN public_keys bw
           ON bw.id = b.block_winner_id
           WHERE b.height = (select MAX(b.height) from blocks b)
           ORDER BY timestamp ASC
           LIMIT 1 |}

    let run_by_id (module Conn : Caqti_async.CONNECTION) id =
      Conn.find_opt query_by_id id

    let run (module Conn : Caqti_async.CONNECTION) = function
      | Some (`This (`Height h)) ->
          Conn.find_opt query_height h
      | Some (`That (`Hash h)) ->
          Conn.find_opt query_hash h
      | Some (`Those (`Height height, `Hash hash)) ->
          Conn.find_opt query_both (hash, height)
      | None ->
          Conn.find_opt query_best ()
  end

  module User_commands = struct
    module Extras = struct
      (* TODO: A few of these actually aren't used; should we leave in for future or remove? *)
      type t =
        { fee_payer: string
        ; source: string
        ; receiver: string
        ; status: string option
        ; failure_reason: string option
        ; fee_payer_account_creation_fee_paid: int64 option
        ; receiver_account_creation_fee_paid: int64 option
        ; created_token: int64 option }
      [@@deriving hlist]

      let fee_payer t = `Pk t.fee_payer

      let source t = `Pk t.source

      let receiver t = `Pk t.receiver

      let status t = t.status

      let failure_reason t = t.failure_reason

      let fee_payer_account_creation_fee_paid t =
        t.fee_payer_account_creation_fee_paid

      let receiver_account_creation_fee_paid t =
        t.receiver_account_creation_fee_paid

      let created_token t = t.created_token

      let typ =
        let open Archive_lib.Processor.Caqti_type_spec in
        let spec =
          Caqti_type.
            [ string
            ; string
            ; string
            ; option string
            ; option string
            ; option int64
            ; option int64
            ; option int64 ]
        in
        let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
        let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
        Caqti_type.custom ~encode ~decode (to_rep spec)
    end

    let typ =
      Caqti_type.(
        tup3 int Archive_lib.Processor.User_command.Signed_command.typ
          Extras.typ)

    let query =
      Caqti_request.collect Caqti_type.int typ
        {| SELECT DISTINCT ON (u.hash) u.id, u.type, u.fee_payer_id, u.source_id, u.receiver_id, u.fee_token, u.token, u.nonce, u.amount, u.fee,
        u.valid_until, u.memo, u.hash,
        pk1.value as fee_payer, pk2.value as source, pk3.value as receiver,
        blocks_user_commands.status,
        blocks_user_commands.failure_reason,
        blocks_user_commands.fee_payer_account_creation_fee_paid,
        blocks_user_commands.receiver_account_creation_fee_paid,
        blocks_user_commands.created_token
        FROM user_commands u
        INNER JOIN blocks_user_commands ON blocks_user_commands.user_command_id = u.id
        INNER JOIN public_keys pk1 ON pk1.id = u.fee_payer_id
        INNER JOIN public_keys pk2 ON pk2.id = u.source_id
        INNER JOIN public_keys pk3 ON pk3.id = u.receiver_id
        WHERE blocks_user_commands.block_id = ?
      |}

    let run (module Conn : Caqti_async.CONNECTION) id =
      Conn.collect_list query id
  end

  module Internal_commands = struct
    module Extras = struct
      let receiver x = `Pk x

      let typ = Caqti_type.string
    end

    let typ =
      Caqti_type.(
        tup3 int Archive_lib.Processor.Internal_command.typ Extras.typ)

    let query =
      Caqti_request.collect Caqti_type.int typ
        {| SELECT DISTINCT ON (i.hash,i.type) i.id, i.type, i.receiver_id, i.fee, i.token, i.hash, pk.value as receiver FROM internal_commands i
        INNER JOIN blocks_internal_commands ON blocks_internal_commands.internal_command_id = i.id
        INNER JOIN public_keys pk ON pk.id = i.receiver_id
        WHERE blocks_internal_commands.block_id = ?
      |}

    let run (module Conn : Caqti_async.CONNECTION) id =
      Conn.collect_list query id
  end

  let run (module Conn : Caqti_async.CONNECTION) input =
    let module M = struct
      include Deferred.Result

      module List = struct
        let map ~f =
          List.fold ~init:(return []) ~f:(fun acc x ->
              let open Let_syntax in
              let%bind xs = acc in
              let%map y = f x in
              y :: xs )
      end
    end in
    let open M.Let_syntax in
    let%bind block_id, raw_block, block_extras =
      match%bind
        Block.run (module Conn) input
        |> Errors.Lift.sql ~context:"Finding block"
      with
      | None ->
          M.fail (Errors.create `Block_missing)
      | Some (block_id, raw_block, block_extras) ->
          M.return (block_id, raw_block, block_extras)
    in
    let%bind parent_id =
      Option.value_map raw_block.parent_id
        ~default:(M.fail (Errors.create `Block_missing))
        ~f:M.return
    in
    let%bind raw_parent_block, _parent_block_extras =
      match%bind
        Block.run_by_id (module Conn) parent_id
        |> Errors.Lift.sql ~context:"Finding parent block"
      with
      | None ->
          M.fail (Errors.create ~context:"Parent block" `Block_missing)
      | Some (_, raw_parent_block, parent_block_extras) ->
          M.return (raw_parent_block, parent_block_extras)
    in
    let%bind raw_user_commands =
      User_commands.run (module Conn) block_id
      |> Errors.Lift.sql ~context:"Finding user commands within block"
    in
    let%bind raw_internal_commands =
      Internal_commands.run (module Conn) block_id
      |> Errors.Lift.sql ~context:"Finding internal commands within block"
    in
    let%bind internal_commands =
      M.List.map raw_internal_commands ~f:(fun (_, ic, extras) ->
          let%map kind =
            match ic.Archive_lib.Processor.Internal_command.typ with
            | "fee_transfer" ->
                M.return `Fee_transfer
            | "coinbase" ->
                M.return `Coinbase
            | "fee_transfer_via_coinbase" ->
                M.return `Fee_transfer_via_coinbase
            | other ->
                M.fail
                  (Errors.create
                     ~context:
                       (sprintf
                          "The archive database is storing internal commands \
                           with %s; this is neither fee_transfer nor coinbase \
                           not fee_transfer_via_coinbase. Please report a bug!"
                          other)
                     `Invariant_violation)
          in
          { Internal_command_info.kind
          ; receiver= Internal_commands.Extras.receiver extras
          ; fee= Unsigned.UInt64.of_int64 ic.fee
          ; token= Unsigned.UInt64.of_int64 ic.token
          ; hash= ic.hash } )
    in
    let%map user_commands =
      M.List.map raw_user_commands ~f:(fun (_, uc, extras) ->
          let open M.Let_syntax in
          let%bind kind =
            match uc.Archive_lib.Processor.User_command.Signed_command.typ with
            | "payment" ->
                M.return `Payment
            | "delegation" ->
                M.return `Delegation
            | "create_token" ->
                M.return `Create_token
            | "create_account" ->
                (* N.B.: not create_token_account *)
                M.return `Create_token_account
            | "mint_tokens" ->
                M.return `Mint_tokens
            | other ->
                M.fail
                  (Errors.create
                     ~context:
                       (sprintf
                          "The archive database is storing user commands with \
                           %s; this is not a known type. Please report a bug!"
                          other)
                     `Invariant_violation)
          in
          let%map failure_status =
            match User_commands.Extras.failure_reason extras with
            | None -> (
              match
                ( User_commands.Extras.fee_payer_account_creation_fee_paid
                    extras
                , User_commands.Extras.receiver_account_creation_fee_paid
                    extras )
              with
              | None, None ->
                  M.return
                  @@ `Applied
                       User_command_info.Account_creation_fees_paid.By_no_one
              | Some fee_payer, None ->
                  M.return
                  @@ `Applied
                       (User_command_info.Account_creation_fees_paid
                        .By_fee_payer
                          (Unsigned.UInt64.of_int64 fee_payer))
              | None, Some receiver ->
                  M.return
                  @@ `Applied
                       (User_command_info.Account_creation_fees_paid
                        .By_receiver
                          (Unsigned.UInt64.of_int64 receiver))
              | Some _, Some _ ->
                  M.fail
                    (Errors.create
                       ~context:
                         "The archive database is storing creation fees paid \
                          by two different pks. This is impossible."
                       `Invariant_violation) )
            | Some status ->
                M.return @@ `Failed status
          in
          { User_command_info.kind
          ; fee_payer= User_commands.Extras.fee_payer extras
          ; source= User_commands.Extras.source extras
          ; receiver= User_commands.Extras.receiver extras
          ; fee_token= Unsigned.UInt64.of_int64 uc.fee_token
          ; token= Unsigned.UInt64.of_int64 uc.token
          ; nonce= Unsigned.UInt32.of_int uc.nonce
          ; amount= Option.map ~f:Unsigned.UInt64.of_int64 uc.amount
          ; fee= Unsigned.UInt64.of_int64 uc.fee
          ; hash= uc.hash
          ; failure_status= Some failure_status } )
    in
    { Block_info.block_identifier=
        {Block_identifier.index= raw_block.height; hash= raw_block.state_hash}
    ; creator= Block.Extras.creator block_extras
    ; winner= Block.Extras.winner block_extras
    ; parent_block_identifier=
        { Block_identifier.index= raw_parent_block.height
        ; hash= raw_parent_block.state_hash }
    ; timestamp= raw_block.timestamp
    ; internal_info= internal_commands
    ; user_commands }
end

module Specific = struct
  module Env = struct
    (* All side-effects go in the env so we can mock them out later *)
    module T (M : Monad_fail.S) = struct
      type 'gql t =
        { gql: unit -> ('gql, Errors.t) M.t
        ; logger: Logger.t
        ; db_block: Block_query.t -> (Block_info.t, Errors.t) M.t
        ; validate_network_choice: 'gql Network.Validate_choice.Impl(M).t }
    end

    (* The real environment does things asynchronously *)
    module Real = T (Deferred.Result)

    (* But for tests, we want things to go fast *)
    module Mock = T (Result)

    let real :
           logger:Logger.t
        -> db:(module Caqti_async.CONNECTION)
        -> graphql_uri:Uri.t
        -> 'gql Real.t =
     fun ~logger ~db ~graphql_uri ->
      { gql=
          (fun () ->
            Graphql.query (Get_coinbase_and_genesis.make ()) graphql_uri )
      ; logger
      ; db_block=
          (fun query ->
            let (module Conn : Caqti_async.CONNECTION) = db in
            Sql.run (module Conn) query )
      ; validate_network_choice= Network.Validate_choice.Real.validate }

    let mock : logger:Logger.t -> 'gql Mock.t =
     fun ~logger ->
      { gql=
          (fun () ->
            Result.return
            @@ object
                 method genesisBlock =
                   object
                     method stateHash = "STATE_HASH_GENESIS"
                   end
               end )
          (* TODO: Add variants to cover every branch *)
      ; logger
      ; db_block= (fun _query -> Result.return @@ Block_info.dummy)
      ; validate_network_choice= Network.Validate_choice.Mock.succeed }
  end

  module Impl (M : Monad_fail.S) = struct
    module Query = Block_query.T (M)
    module Internal_command_info_ops = Internal_command_info.T (M)

    let handle :
           env:'gql Env.T(M).t
        -> Block_request.t
        -> (Block_response.t, Errors.t) M.t =
     fun ~env req ->
      let open M.Let_syntax in
      let logger = env.logger in
      let%bind query = Query.of_partial_identifier req.block_identifier in
      let%bind res = env.gql () in
      let%bind () =
        env.validate_network_choice ~network_identifier:req.network_identifier
          ~gql_response:res
      in
      let genesisBlock = res#genesisBlock in
      let%bind block_info =
        if Query.is_genesis ~hash:genesisBlock#stateHash query then
          let genesis_block_identifier =
            { Block_identifier.index= Network.genesis_block_height
            ; hash= genesisBlock#stateHash }
          in
          M.return
            { Block_info.block_identifier=
                genesis_block_identifier
                (* parent_block_identifier for genesis block should be the same as block identifier as described https://www.rosetta-api.org/docs/common_mistakes.html#correct-example *)
            ; parent_block_identifier= genesis_block_identifier
            ; creator= `Pk (genesisBlock#creatorAccount)#publicKey
            ; winner= `Pk (genesisBlock#winnerAccount)#publicKey
            ; timestamp=
                Int64.of_string
                  ((genesisBlock#protocolState)#blockchainState)#date
            ; internal_info= []
            ; user_commands= [] }
        else env.db_block query
      in
      let coinbase_receiver =
        List.find block_info.internal_info ~f:(fun info ->
            info.Internal_command_info.kind = `Coinbase )
        |> Option.map ~f:(fun cmd -> cmd.Internal_command_info.receiver)
      in
      let%map internal_transactions =
        List.fold block_info.internal_info ~init:(M.return [])
          ~f:(fun macc info ->
            let%bind acc = macc in
            let%map operations =
              Internal_command_info_ops.to_operations ~coinbase_receiver info
            in
            [%log debug]
              ~metadata:[("info", Internal_command_info.to_yojson info)]
              "Block internal received $info" ;
            { Transaction.transaction_identifier=
                {Transaction_identifier.hash= info.hash}
            ; operations
            ; metadata= None }
            :: acc )
        |> M.map ~f:List.rev
      in
      { Block_response.block=
          Some
            { Block.block_identifier= block_info.block_identifier
            ; parent_block_identifier= block_info.parent_block_identifier
            ; timestamp= block_info.timestamp
            ; transactions=
                internal_transactions
                @ List.map block_info.user_commands ~f:(fun info ->
                      [%log debug]
                        ~metadata:[("info", User_command_info.to_yojson info)]
                        "Block user received $info" ;
                      { Transaction.transaction_identifier=
                          {Transaction_identifier.hash= info.hash}
                      ; operations= User_command_info.to_operations' info
                      ; metadata= None } )
            ; metadata= Some (Block_info.creator_metadata block_info) }
      ; other_transactions= [] }
  end

  module Real = Impl (Deferred.Result)

  let%test_module "blocks" =
    ( module struct
      module Mock = Impl (Result)

      (* This test intentionally fails as there has not been time to implement
       * it properly yet *)
      (*
      let%test_unit "all dummies" =
        Test.assert_ ~f:Block_response.to_yojson
          ~expected:
            (Mock.handle ~env:Env.mock
               (Block_request.create
                  (Network_identifier.create "x" "y")
                  (Partial_block_identifier.create ())))
          ~actual:(Result.fail (Errors.create (`Json_parse None)))
    *)
    end )
end

let router ~graphql_uri ~logger ~with_db (route : string list) body =
  let open Async.Deferred.Result.Let_syntax in
  [%log debug] "Handling /block/ $route"
    ~metadata:[("route", `List (List.map route ~f:(fun s -> `String s)))] ;
  match route with
  | [] | [""] ->
      with_db (fun ~db ->
          let%bind req =
            Errors.Lift.parse ~context:"Request"
            @@ Block_request.of_yojson body
            |> Errors.Lift.wrap
          in
          let%map res =
            Specific.Real.handle
              ~env:(Specific.Env.real ~logger ~db ~graphql_uri)
              req
            |> Errors.Lift.wrap
          in
          Block_response.to_yojson res )
  (* Note: We do not need to implement /block/transaction endpoint because we
   * don't return any "other_transactions" *)
  | _ ->
      Deferred.Result.fail `Page_not_found
