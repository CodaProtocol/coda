open Core
open Async
open Cli_lib

module Graphql_block = struct
  open Mina_base
  open Mina_state

  let validation_callback =
    Mina_net2.Validation_callback.create_without_expiration ()

  let delta_transition_chain_proof = (State_hash.zero, [])

  let protocol_state_proof = Proof.blockchain_dummy

  let dummy_aux_hash = Staged_ledger_hash.Aux_hash.dummy

  let genesis_state_hash =
    Precomputed_values.(genesis_state_hash (Lazy.force compiled))

  let genesis_ledger_hash =
    Precomputed_values.(genesis_state (Lazy.force compiled))
    |> Protocol_state.blockchain_state |> Blockchain_state.snarked_ledger_hash

  let snarked_next_available_token =
    Precomputed_values.(genesis_state (Lazy.force compiled))
    |> Protocol_state.blockchain_state
    |> Blockchain_state.snarked_next_available_token

  let protocol_constants : Protocol_constants_checked.Value.t =
    let t = Genesis_constants.compiled.protocol in
    let f = Unsigned.UInt32.of_int in
    { k= f t.k
    ; slots_per_epoch= f t.slots_per_epoch
    ; slots_per_sub_window= f t.slots_per_sub_window
    ; delta= f t.delta
    ; genesis_state_timestamp= Block_time.of_int64 t.genesis_state_timestamp }

  let dummy_pending_coinbase =
    Or_error.ok_exn (Pending_coinbase.create ~depth:1 ())

  let dummy_sub_window_densities = []

  open Mina_numbers

  let dummy_coinbase_receiver =
    Signature_lib.Public_key.Compressed.of_base58_check_exn
      "B62qrLj8KDbgbgKMnmMvXvhCwCEwZzUiH6T1MvjNvmkpKUCpybAH8LC"

  let () = Protocol_version.(set_current zero)

  let read_one states json =
    let open Yojson.Safe.Util in
    let ( -. ) o x =
      try List.Assoc.find_exn (to_assoc o) ~equal:String.equal x
      with _ ->
        failwithf "(id=%s) %s not found in %s"
          (to_string
             (List.Assoc.find_exn (to_assoc json) ~equal:String.equal "_id"))
          x (Yojson.Safe.to_string o) ()
    in
    let fee x =
      Currency.Fee.of_uint64 (Unsigned.UInt64.of_string (to_string x))
    in
    let amount x =
      Currency.Amount.of_uint64 (Unsigned.UInt64.of_string (to_string x))
    in
    let token_id x =
      Token_id.of_uint64 (Unsigned.UInt64.of_string (to_string x))
    in
    let pubkey x =
      Signature_lib.Public_key.Compressed.of_base58_check_exn (to_string x)
    in
    let state_hash x = State_hash.of_base58_check_exn (to_string x) in
    let ledger_hash x = Ledger_hash.of_base58_check_exn (to_string x) in
    let curr_state_hash = state_hash (json -. "stateHash") in
    let epoch_data (x : Yojson.Safe.t) =
      { Epoch_data.Poly.epoch_length=
          Length.of_string (to_string (x -. "epochLength"))
      ; seed= Epoch_seed.of_base58_check_exn (to_string (x -. "seed"))
      ; start_checkpoint= state_hash (x -. "startCheckpoint")
      ; lock_checkpoint= state_hash (x -. "lockCheckpoint")
      ; ledger=
          { Epoch_ledger.Poly.hash= ledger_hash (x -. "ledger" -. "hash")
          ; total_currency= amount (x -. "ledger" -. "totalCurrency") } }
    in
    let creator = pubkey (json -. "creator") in
    let ( (staged_ledger_diff : Staged_ledger_diff.t)
        , transactions
        , coinbase_receiver ) =
      let txns = json -. "transactions" in
      let coinbase_amount = amount (txns -. "coinbase") in
      let diff : Staged_ledger_diff.Pre_diff_with_at_most_two_coinbase.t =
        { Staged_ledger_diff.Pre_diff_two.completed_works= []
        ; commands= []
        ; coinbase= Staged_ledger_diff.At_most_two.Zero
        ; internal_command_balances= [] }
      in
      let has_coinbase =
        not (Currency.Amount.equal Currency.Amount.zero coinbase_amount)
      in
      let coinbase_receiver =
        if has_coinbase then
          pubkey (txns -. "coinbaseReceiverAccount" -. "publicKey")
        else dummy_coinbase_receiver
      in
      let zero_balance = Some Currency.Balance.zero in
      let zero_amount = Some Currency.Amount.zero in
      let balance_data =
        { Transaction_status.Balance_data.fee_payer_balance= zero_balance
        ; source_balance= zero_balance
        ; receiver_balance= zero_balance }
      in
      let aux_data =
        { Transaction_status.Auxiliary_data.fee_payer_account_creation_fee_paid=
            zero_amount
        ; receiver_account_creation_fee_paid= zero_amount
        ; created_token= None }
      in
      let transactions =
        let fts =
          List.map
            (to_list (txns -. "feeTransfer"))
            ~f:(fun ft ->
              Fee_transfer.Single.create ~fee_token:Token_id.default
                ~receiver_pk:(pubkey (ft -. "recipient"))
                ~fee:(fee (ft -. "fee")) )
          |> One_or_two.group_list
          |> List.map ~f:(fun x ->
                 { Mina_base.With_status.data=
                     Transaction.Fee_transfer
                       (Or_error.ok_exn (Fee_transfer.of_singles x))
                 ; status=
                     Applied
                       (aux_data, {balance_data with source_balance= None}) }
             )
        in
        let commands : Transaction.t With_status.t list =
          List.map
            (to_list (txns -. "userCommands"))
            ~f:(fun x ->
              ( let source = pubkey (x -. "source" -. "publicKey") in
                let receiver = pubkey (x -. "receiver" -. "publicKey") in
                let body =
                  match to_string (x -. "kind") with
                  | "PAYMENT" ->
                      Signed_command_payload.Body.Payment
                        { Payment_payload.Poly.source_pk= source
                        ; receiver_pk= receiver
                        ; amount= amount (x -. "amount")
                        ; token_id= token_id (x -. "token") }
                  | "STAKE_DELEGATION" ->
                      Signed_command_payload.Body.Stake_delegation
                        (Set_delegate
                           { delegator= pubkey (x -. "delegator" -. "publicKey")
                           ; new_delegate= receiver })
                  | s ->
                      failwithf "unknown kind: %s" s ()
                in
                { Mina_base.With_status.status= Applied (aux_data, balance_data)
                ; data=
                    Transaction.Command
                      (User_command.Signed_command
                         { Signed_command.Poly.payload=
                             Signed_command_payload.create
                               ~fee:(fee (x -. "fee"))
                               ~fee_token:(token_id (x -. "feeToken"))
                               ~fee_payer_pk:source
                               ~nonce:
                                 (Account.Nonce.of_int (to_int (x -. "nonce")))
                               ~valid_until:None
                               ~memo:
                                 (Signed_command_memo.of_string
                                    (to_string (x -. "memo")))
                               ~body
                         ; signer=
                             Signature_lib.Public_key.decompress_exn source
                         ; signature= Signature.dummy }) }
                : Transaction.t With_status.t ) )
        in
        commands
        @ ( if has_coinbase then
            [ { Mina_base.With_status.status=
                  Applied (aux_data, {balance_data with source_balance= None})
              ; data=
                  Transaction.Coinbase
                    ( Coinbase.create ~amount:coinbase_amount
                        ~receiver:coinbase_receiver ~fee_transfer:None
                    |> Or_error.ok_exn ) } ]
          else [] )
        @ fts
      in
      ({Staged_ledger_diff.diff= (diff, None)}, transactions, coinbase_receiver)
    in
    let protocol_state : Protocol_state.Value.t =
      let p = json -. "protocolState" in
      let cs = p -. "consensusState" in
      let next_epoch_data, staking_epoch_data =
        match Map.find states curr_state_hash with
        | Some (s : Protocol_state.Value.t) ->
            let cs = Protocol_state.consensus_state s in
            Consensus.Data.Consensus_state.
              (next_epoch_data cs, staking_epoch_data cs)
        | None ->
            ( epoch_data (cs -. "nextEpochData")
            , epoch_data (cs -. "stakingEpochData") )
      in
      Protocol_state.create_value
        ~previous_state_hash:(state_hash (p -. "previousStateHash"))
        ~constants:protocol_constants ~genesis_state_hash
        ~blockchain_state:
          (let bs = p -. "blockchainState" in
           Blockchain_state.create_value
             ~staged_ledger_hash:
               (Staged_ledger_hash.of_aux_ledger_and_coinbase_hash
                  dummy_aux_hash
                  (ledger_hash (bs -. "stagedLedgerHash"))
                  dummy_pending_coinbase)
             ~snarked_ledger_hash:(ledger_hash (bs -. "snarkedLedgerHash"))
             ~genesis_ledger_hash ~snarked_next_available_token
             ~timestamp:(Block_time.of_string_exn (to_string (bs -. "date"))))
        ~consensus_state:
          (let length x = Mina_numbers.Length.of_string (to_string x) in
           { Consensus.Proof_of_stake.Consensus_state.Poly.blockchain_length=
               length (cs -. "blockchainLength")
           ; epoch_count= length (cs -. "epochCount")
           ; min_window_density= length (cs -. "minWindowDensity")
           ; sub_window_densities= dummy_sub_window_densities
           ; last_vrf_output=
               Consensus.Proof_of_stake.Vrf_output.Truncated
               .of_base58_check_exn
                 (to_string (cs -. "lastVrfOutput"))
           ; total_currency= amount (cs -. "totalCurrency")
           ; curr_global_slot=
               Consensus.Global_slot.of_epoch_and_slot
                 ~constants:
                   (Consensus.Constants.create
                      ~protocol_constants:Genesis_constants.compiled.protocol
                      ~constraint_constants:
                        Genesis_constants.Constraint_constants.compiled)
                 ( Consensus.Epoch.of_string (to_string (cs -. "epoch"))
                 , Consensus.Slot.of_string (to_string (cs -. "slot")) )
           ; global_slot_since_genesis=
               Mina_numbers.Global_slot.of_string
                 (to_string (cs -. "slotSinceGenesis"))
           ; staking_epoch_data
           ; next_epoch_data
           ; has_ancestor_in_same_checkpoint_window= false
           ; block_stake_winner=
               coinbase_receiver (*not present in block data*)
           ; block_creator= creator
           ; coinbase_receiver
           ; supercharge_coinbase= false (*not in the json/schema*) })
    in
    ( Mina_transition.External_transition.create ~protocol_state
        ~staged_ledger_diff ~protocol_state_proof ~delta_transition_chain_proof
        ~validation_callback ()
      |> With_hash.of_data ~hash_data:(fun _ -> curr_state_hash)
    , transactions )
end

module Best_tip_diff_log = struct
  open Transition_frontier.Extensions.Best_tip_diff.Log_event

  type t =
    { added_transitions:
        Transition_frontier.Extensions.Best_tip_diff.Log_event.t list
    ; removed_transitions:
        Transition_frontier.Extensions.Best_tip_diff.Log_event.t list
    ; reorg_best_tip: bool }
  [@@deriving yojson]

  let read_one (json : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    let ( -. ) o x = List.Assoc.find_exn (to_assoc o) ~equal:String.equal x in
    let md = json -. "jsonPayload" -. "metadata" in
    let f s = (s, md -. s) in
    of_yojson
      (`Assoc
        [f "added_transitions"; f "removed_transitions"; f "reorg_best_tip"])
    |> function Ok x -> x | Error e -> failwith e

  let of_file x =
    Reader.file_contents x >>| Yojson.Safe.from_string
    >>| function
    | `List xs -> List.map ~f:read_one xs | _ -> failwith "Expected list"

  let extract_protocol_states
      {added_transitions; removed_transitions; reorg_best_tip= _} =
    List.concat_map
      [added_transitions; removed_transitions]
      ~f:
        (List.map
           ~f:(fun {protocol_state; state_hash; just_emitted_a_proof= _} ->
             (state_hash, protocol_state) ))
    |> Mina_base.State_hash.Map.of_alist_reduce ~f:Fn.const
end

let deferred_result_map xs ~f =
  let open Deferred.Result.Let_syntax in
  let rec go acc = function
    | [] ->
        return (List.rev acc)
    | x :: xs ->
        let%bind y = f x in
        go (y :: acc) xs
  in
  go [] xs

open Mina_base

let recover_main ~path () =
  (*let best_tip_logs =
    [ "frontend/archive-node-blocks/recover/best_tip_logs__2020-11-23T09-18.json"
    ; "frontend/archive-node-blocks/recover/missing-best-tip-logs-2020-11-19T02-16.json"
    ; "frontend/archive-node-blocks/recover/downloaded-logs-20201212-210358.json"
    ; "frontend/archive-node-blocks/recover/downloaded-logs-20201212-211054.json"
    ; "frontend/archive-node-blocks/recover/downloaded-logs-20201212-214452.json"
    ; "frontend/archive-node-blocks/recover/downloaded-logs-20201212-214913.json"
    ; "frontend/archive-node-blocks/recover/downloaded-logs-20201212-215811.json"
    ]
  in
  let%bind states =
    Deferred.List.concat_map ~f:Best_tip_diff_log.of_file best_tip_logs
    >>| List.map ~f:Best_tip_diff_log.extract_protocol_states
    >>| List.reduce_exn ~f:(fun x y ->
            Map.merge x y ~f:(fun ~key:_ t ->
                match t with `Both (x, _) | `Left x | `Right x -> Some x ) )
  in*)
  let states = State_hash.Map.empty in
  let%bind bs =
    let%map s = Reader.file_contents path in
    List.filter_map
      ~f:(fun x -> try Some (Graphql_block.read_one states x) with _ -> None)
      (Yojson.Safe.Util.to_list (Yojson.Safe.from_string s))
  in
  let bs =
    (* Topological sort *)
    let by_hash =
      State_hash.Table.of_alist_multi
        (List.map bs ~f:(fun (b, ts) -> (b.hash, (b, ts))))
      |> Hashtbl.map ~f:List.hd_exn
    in
    let children =
      State_hash.Table.of_alist_multi
        (List.map bs ~f:(fun (b, _ts) ->
             (Mina_transition.External_transition.parent_hash b.data, b.hash)
         ))
    in
    let roots =
      List.filter_map bs ~f:(fun (b, _) ->
          let parent_present =
            Hashtbl.mem by_hash
              (Mina_transition.External_transition.parent_hash b.data)
          in
          if parent_present then None else Some b.hash )
    in
    let rec go acc next =
      match next with
      | [] ->
          List.rev acc
      | _ ->
          let next' =
            List.concat_map next ~f:(fun h -> Hashtbl.find_multi children h)
          in
          go
            (List.rev_append (List.map next ~f:(Hashtbl.find_exn by_hash)) acc)
            next'
    in
    go [] roots
  in
  let%bind res =
    let open Deferred.Result.Let_syntax in
    let postgres =
      Uri.of_string "postgres://o1labs:o1labs@127.0.0.1:5432/encore_o1"
    in
    let%bind ((module Conn) as conn) = Caqti_async.connect postgres in
    Core.printf "yo: %d\n%!" (List.length bs) ;
    let%bind () = Conn.start () in
    match%bind.Async
      let%bind _xs =
        let i = ref 0 in
        deferred_result_map bs ~f:(fun (b, ts) ->
            Core.printf "yo: %d\n%!" !i ;
            incr i ;
            Archive_lib.Processor.Block.add_if_doesn't_exist conn
              ~transactions_override:ts
              ~constraint_constants:
                Genesis_constants.Constraint_constants.compiled b )
      in
      Conn.commit ()
    with
    | Ok () ->
        return ()
    | Error err ->
        let%bind.Async _ = Conn.rollback () in
        Deferred.Result.fail err
  in
  Result.map_error res ~f:(fun e -> Caqti_error.Exn e) |> Result.ok_exn ;
  Core.print_endline "cool" ;
  Deferred.return ()

let command_recover =
  let open Command.Let_syntax in
  Command.async ~summary:"Recover"
    (let%map_open path =
       flag "--path" (required string)
         ~doc:"path to the json file containing blocks"
     in
     fun () -> recover_main ~path ())

let command_run =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Run an archive process that can store all of the data of Mina"
    (let%map_open log_json = Flag.Log.json
     and log_level = Flag.Log.level
     and server_port = Flag.Port.Archive.server
     and metrics_server_port =
       flag "--metrics-port" ~aliases:["-metrics-port"]
         ~doc:
           "PORT metrics server for scraping via Prometheus (default no \
            metrics-server)"
         (optional Cli_lib.Arg_type.int16)
     and missing_blocks_width =
       flag "--missing-blocks-width" ~aliases:["-missing-blocks-width"]
         ~doc:
           (sprintf
              "int The width of block heights within which missing blocks are \
               reported in Prometheus metrics. If the maximum height in the \
               database is h and missing-blocks-width is n, then \
               Coda_Archive_missing_blocks will report missing blocks between \
               heights max(1, h-n) and h (default %d)"
              Archive_lib.Metrics.default_missing_blocks_width)
         (optional int)
     and postgres = Flag.Uri.Archive.postgres
     and runtime_config_file =
       flag "--config-file" ~aliases:["-config-file"] (optional string)
         ~doc:"PATH to the configuration file containing the genesis ledger"
     and delete_older_than =
       flag "--delete-older-than" ~aliases:["-delete-older-than"]
         (optional int)
         ~doc:
           "int Delete blocks that are more than n blocks lower than the \
            maximum seen block."
     in
     let runtime_config_opt =
       Option.map runtime_config_file ~f:(fun file ->
           Yojson.Safe.from_file file |> Runtime_config.of_yojson
           |> Result.ok_or_failwith )
     in
     fun () ->
       let logger = Logger.create () in
       Stdout_log.setup log_json log_level ;
       Archive_lib.Processor.setup_server ~metrics_server_port ~logger
         ~constraint_constants:Genesis_constants.Constraint_constants.compiled
         ~postgres_address:postgres.value
         ~server_port:
           (Option.value server_port.value ~default:server_port.default)
         ~delete_older_than ~runtime_config_opt ~missing_blocks_width)

let time_arg =
  (* Same timezone as Genesis_constants.genesis_state_timestamp. *)
  let default_timezone = Core.Time.Zone.of_utc_offset ~hours:(-8) in
  Command.Arg_type.create
    (Time.of_string_gen ~if_no_timezone:(`Use_this_one default_timezone))

let command_prune =
  let open Command.Let_syntax in
  Command.async ~summary:"Prune old blocks and their transactions"
    (let%map_open height =
       flag "--height" ~aliases:["-height"] (optional int)
         ~doc:"int Delete blocks with height lower than the given height"
     and num_blocks =
       flag "--num-blocks" ~aliases:["-num-blocks"] (optional int)
         ~doc:
           "int Delete blocks that are more than n blocks lower than the \
            maximum seen block. This argument is ignored if the --height \
            argument is also given"
     and timestamp =
       flag "--timestamp" ~aliases:["-timestamp"] (optional time_arg)
         ~doc:
           "timestamp Delete blocks that are older than the given timestamp. \
            Format: 2000-00-00 12:00:00+0100"
     and postgres = Flag.Uri.Archive.postgres in
     fun () ->
       let timestamp =
         timestamp
         |> Option.map ~f:Block_time.of_time
         |> Option.map ~f:Block_time.to_int64
       in
       let go () =
         let open Deferred.Result.Let_syntax in
         let%bind ((module Conn) as conn) =
           Caqti_async.connect postgres.value
         in
         let%bind () = Conn.start () in
         match%bind.Async
           let%bind () =
             Archive_lib.Processor.Block.delete_if_older_than ?height
               ?num_blocks ?timestamp conn
           in
           Conn.commit ()
         with
         | Ok () ->
             return ()
         | Error err ->
             let%bind.Async _ = Conn.rollback () in
             Deferred.Result.fail err
       in
       let logger = Logger.create () in
       let cmd_metadata =
         List.filter_opt
           [ Option.map height ~f:(fun v -> ("height", `Int v))
           ; Option.map num_blocks ~f:(fun v -> ("num_blocks", `Int v))
           ; Option.map timestamp ~f:(fun v ->
                 ("timestamp", `String (Int64.to_string v)) ) ]
       in
       match%map.Async go () with
       | Ok () ->
           [%log info] "Successfully purged blocks." ~metadata:cmd_metadata
       | Error err ->
           [%log error] "Failed to purge blocks"
             ~metadata:
               (("error", `String (Caqti_error.show err)) :: cmd_metadata))

let commands =
  [("run", command_run); ("prune", command_prune); ("recover", command_recover)]

let () = Command.run (Command.group ~summary:"Archive node commands" commands)
