(* delegation_compliance.ml *)

(* check whether a block producer delegated to from Mina Foundation or
   O(1) Labs follows requirements at
   https://docs.minaprotocol.com/en/advanced/foundation-delegation-program
*)

open Core_kernel
open Async
open Mina_base
open Signature_lib

type input = {epoch: int; staking_ledger: Runtime_config.Ledger.t}
[@@deriving yojson]

type delegation_source = O1 | Mina_foundation [@@deriving yojson]

type payout_information =
  { payout_pk: Public_key.Compressed.t
  ; payout_id: int
  ; delegation_source: delegation_source
  ; delegatee: Public_key.Compressed.t
  ; delegatee_id: int
  ; payments: Sql.User_command.t list
  ; payments_to_slot_3500: Sql.User_command.t list
  ; payments_past_slot_3500: Sql.User_command.t list }
[@@deriving yojson]

type csv_data =
  { payout_addr: Public_key.Compressed.t
  ; balance: Currency.Balance.t
  ; delegatee: Public_key.Compressed.t
  ; delegation: Currency.Amount.t
  ; blocks_won: int
  ; payout_obligation: Currency.Amount.t
  ; payout_received: Currency.Amount.t
  ; deficit: Currency.Amount.t
  ; check: bool }

module Delegatee_payout_address = struct
  type t =
    { delegatee: Public_key.Compressed.Stable.Latest.t
    ; payout_addr: Public_key.Compressed.Stable.Latest.t }
  [@@deriving hash, bin_io_unversioned, compare, sexp]
end

module Deficit = Hashable.Make_binable (Delegatee_payout_address)

type previous_epoch_status =
  {payout_received: Currency.Amount.t; deficit: Currency.Amount.t}

(* map from delegatee, payout address to payment_received, deficit from previous epoch *)
let deficit_tbl : previous_epoch_status Deficit.Table.t =
  Deficit.Table.create ()

let csv_data_of_strings ss =
  match ss with
  | [ payout_address
    ; balance
    ; delegatee
    ; total_delegation
    ; blocks_won
    ; payout_obligation
    ; payout_received
    ; deficit
    ; check ] ->
      let payout_addr =
        Public_key.Compressed.of_base58_check_exn payout_address
      in
      let balance = Currency.Balance.of_formatted_string balance in
      let delegatee = Public_key.Compressed.of_base58_check_exn delegatee in
      let delegation = Currency.Amount.of_formatted_string total_delegation in
      let blocks_won = Int.of_string blocks_won in
      let payout_obligation =
        Currency.Amount.of_formatted_string payout_obligation
      in
      let payout_received =
        Currency.Amount.of_formatted_string payout_received
      in
      let deficit = Currency.Amount.of_formatted_string deficit in
      let check = Bool.of_string check in
      { payout_addr
      ; balance
      ; delegatee
      ; delegation
      ; blocks_won
      ; payout_obligation
      ; payout_received
      ; deficit
      ; check }
  | _ ->
      failwith "Incorrect number of fields in CSV line"

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let proof_level = Genesis_constants.Proof_level.Full

let currency_string_of_int64 i64 =
  Currency.Amount.of_uint64 (Unsigned.UInt64.of_int64 i64)
  |> Currency.Amount.to_formatted_string

(* map from global slots to state hash, ledger hash pairs *)
let global_slot_hashes_tbl : (Int64.t, State_hash.t * Ledger_hash.t) Hashtbl.t
    =
  Int64.Table.create ()

(* cache of account keys *)
let pk_tbl : (int, Account.key) Hashtbl.t = Int.Table.create ()

let query_db pool ~f ~item =
  match%bind Caqti_async.Pool.use f pool with
  | Ok v ->
      return v
  | Error msg ->
      failwithf "Error getting %s from db, error: %s" item
        (Caqti_error.show msg) ()

let slots_per_epoch = Genesis_constants.slots_per_epoch

let slots_per_epoch_uint32 = slots_per_epoch |> Unsigned.UInt32.of_int

(* offset is slot within epoch, starting from 0 *)
let epoch_and_offset_of_global_slot global_slot =
  let open Unsigned.UInt32 in
  let global_slot_uint32 = global_slot |> Int64.to_string |> of_string in
  let epoch = div global_slot_uint32 slots_per_epoch_uint32 in
  let epoch_start_slot = mul epoch slots_per_epoch_uint32 in
  let offset = Unsigned.UInt32.sub global_slot_uint32 epoch_start_slot in
  (epoch, offset)

let pk_of_pk_id pool pk_id : Account.key Deferred.t =
  let open Deferred.Let_syntax in
  match Hashtbl.find pk_tbl pk_id with
  | Some pk ->
      return pk
  | None -> (
      (* not in cache, consult database *)
      match%map
        Caqti_async.Pool.use (fun db -> Sql.Public_key.run db pk_id) pool
      with
      | Ok (Some pk) -> (
        match Signature_lib.Public_key.Compressed.of_base58_check pk with
        | Ok pk ->
            Hashtbl.add_exn pk_tbl ~key:pk_id ~data:pk ;
            pk
        | Error err ->
            Error.tag_arg err "Error decoding public key"
              (("public_key", pk), ("id", pk_id))
              [%sexp_of: (string * string) * (string * int)]
            |> Error.raise )
      | Ok None ->
          failwithf "Could not find public key with id %d" pk_id ()
      | Error msg ->
          failwithf "Error retrieving public key with id %d, error: %s" pk_id
            (Caqti_error.show msg) () )

let pk_id_of_pk pool pk : int Deferred.t =
  let open Deferred.Let_syntax in
  match%map
    Caqti_async.Pool.use (fun db -> Sql.Public_key.run_for_id db pk) pool
  with
  | Ok (Some id) ->
      id
  | Ok None ->
      failwithf "Could not find id for public key %s" pk ()
  | Error msg ->
      failwithf "Error retrieving id for public key %s, error: %s" pk
        (Caqti_error.show msg) ()

let compute_delegated_stake staking_ledger delegatee =
  let open Currency in
  Ledger.foldi staking_ledger ~init:Amount.zero
    ~f:(fun _addr accum (account : Account.t) ->
      match account.delegate with
      | Some delegate ->
          if Public_key.Compressed.equal delegate delegatee then
            let balance_as_amount =
              Currency.Balance.to_amount account.balance
            in
            match Amount.add balance_as_amount accum with
            | Some sum ->
                sum
            | None ->
                failwith "Error summing delegated stake"
          else accum
      | None ->
          accum )

let account_balance ledger pk =
  let account_id = Account_id.create pk Token_id.default in
  match Ledger.location_of_account ledger account_id with
  | Some location -> (
    match Ledger.get ledger location with
    | Some account ->
        account.balance
    | None ->
        failwith "account_balance: Could not find account for public key" )
  | None ->
      failwith "account_balance: Could not find location for account"

let get_account_balance_as_amount ledger pk =
  let account_id = Account_id.create pk Token_id.default in
  match Ledger.location_of_account ledger account_id with
  | Some location -> (
    match Ledger.get ledger location with
    | Some account ->
        Currency.Balance.to_amount account.balance
    | None ->
        failwith
          "get_account_balance_as_amount: Could not find account for public key"
    )
  | None ->
      failwith
        "get_account_balance_as_amount: Could not find location for account"

let slot_bounds_for_epoch epoch =
  let open Unsigned.UInt32 in
  let low_slot = mul epoch slots_per_epoch_uint32 |> to_int64 in
  let high_slot = pred (mul (succ epoch) slots_per_epoch_uint32) |> to_int64 in
  (low_slot, high_slot)

let block_ids_in_epoch pool delegatee_id epoch =
  let low_slot, high_slot = slot_bounds_for_epoch epoch in
  query_db pool
    ~f:(fun db ->
      Sql.Block.get_block_ids_for_creator_in_slot_bounds db
        ~creator:delegatee_id ~low_slot ~high_slot )
    ~item:"block ids for delegatee in epoch"

let write_csv_header ~csv_out_channel =
  let line =
    String.concat ~sep:","
      [ "Payout address"
      ; "Balance"
      ; "Delegatee"
      ; "Total delegation"
      ; "Blocks won"
      ; "Payout obligation"
      ; "Payout received"
      ; "Deficit"
      ; "Check" ]
  in
  Out_channel.output_string csv_out_channel line ;
  Out_channel.newline csv_out_channel

let write_csv_line ~csv_out_channel ~payout_addr ~balance ~delegatee
    ~delegation ~blocks_won ~payout_obligation ~payout_received =
  let check = Currency.Amount.( >= ) payout_received payout_obligation in
  let deficit =
    match Currency.Amount.( - ) payout_obligation payout_received with
    | Some diff ->
        diff
    | None ->
        Currency.Amount.zero
  in
  let line =
    String.concat ~sep:","
      [ Public_key.Compressed.to_base58_check payout_addr
      ; Currency.Balance.to_formatted_string balance
      ; Public_key.Compressed.to_base58_check delegatee
      ; Currency.Amount.to_formatted_string delegation
      ; Int.to_string blocks_won
      ; Currency.Amount.to_formatted_string payout_obligation
      ; Currency.Amount.to_formatted_string payout_received
      ; Currency.Amount.to_formatted_string deficit
      ; Bool.to_string check ]
  in
  Out_channel.output_string csv_out_channel line ;
  Out_channel.newline csv_out_channel

let write_csv_line_of_csv_data ~csv_out_channel
    { payout_addr
    ; balance
    ; delegatee
    ; delegation
    ; blocks_won
    ; payout_obligation
    ; payout_received
    ; deficit= _
    ; check= _ } =
  write_csv_line ~csv_out_channel ~payout_addr ~balance ~delegatee ~delegation
    ~blocks_won ~payout_obligation ~payout_received

let main ~input_file ~csv_file ~preliminary_csv_file_opt ~archive_uri
    ~payout_addresses () =
  let logger = Logger.create () in
  if List.is_empty payout_addresses then (
    [%log error]
      "Please provide at least one payout address on the command line" ;
    Core.exit 1 ) ;
  let json = Yojson.Safe.from_file input_file in
  let input =
    match input_of_yojson json with
    | Ok inp ->
        inp
    | Error msg ->
        failwith
          (sprintf "Could not parse JSON in input file \"%s\": %s" input_file
             msg)
  in
  ( match preliminary_csv_file_opt with
  | None ->
      if input.epoch > 0 then
        failwith
          "Preliminary CSV file must be provided if epoch is greater than 0"
  | Some _ ->
      if input.epoch = 0 then
        failwith "Preliminary CSV file must not be provided if epoch is 0" ) ;
  let csv_datas =
    match preliminary_csv_file_opt with
    | None ->
        []
    | Some prelim_csv_file ->
        let prelim_csv_in_channel = In_channel.create prelim_csv_file in
        (* discard header line *)
        let lines =
          In_channel.input_lines prelim_csv_in_channel |> List.tl_exn
        in
        let split_lines =
          List.map lines ~f:(String.split_on_chars ~on:[','])
        in
        let csv_datas = List.map split_lines ~f:csv_data_of_strings in
        List.iter csv_datas
          ~f:(fun ({payout_addr; delegatee; payout_received; deficit; _} :
                    csv_data)
             ->
            let key : Delegatee_payout_address.t = {delegatee; payout_addr} in
            let data : previous_epoch_status = {payout_received; deficit} in
            match Deficit.Table.add deficit_tbl ~key ~data with
            | `Ok ->
                ()
            | `Duplicate ->
                failwith "Duplicate deficit table entry" ) ;
        csv_datas
  in
  let archive_uri = Uri.of_string archive_uri in
  match Caqti_async.connect_pool ~max_size:128 archive_uri with
  | Error e ->
      [%log fatal]
        ~metadata:[("error", `String (Caqti_error.show e))]
        "Failed to create a Caqti pool for Postgresql" ;
      exit 1
  | Ok pool ->
      [%log info] "Successfully created Caqti pool for Postgresql" ;
      (* load from runtime config in same way as daemon
         except that we don't consider loading from a tar file
      *)
      let%bind padded_accounts =
        match
          Genesis_ledger_helper.Ledger.padded_accounts_from_runtime_config_opt
            ~logger ~proof_level input.staking_ledger
            ~ledger_name_prefix:"genesis_ledger"
        with
        | None ->
            [%log fatal] "Could not load accounts from input staking ledger" ;
            exit 1
        | Some accounts ->
            return accounts
      in
      let packed_ledger =
        Genesis_ledger_helper.Ledger.packed_genesis_ledger_of_accounts
          ~depth:constraint_constants.ledger_depth padded_accounts
      in
      let ledger = Lazy.force @@ Genesis_ledger.Packed.t packed_ledger in
      let%bind max_slot =
        query_db pool
          ~f:(fun db -> Sql.Block.get_max_slot db ())
          ~item:"max slot"
      in
      [%log info] "Maximum global slot in blocks is %d" max_slot ;
      (* find longest canonical chain
         a slot may represent several blocks, only one of which can be on canonical chain
         starting with max slot, look for chain, decrementing slot until chain found
      *)
      let find_canonical_chain slot =
        let block_infos_from_state_hash state_hash =
          match%map
            query_db pool
              ~f:(fun db -> Sql.Block_info.run db state_hash)
              ~item:"block info"
          with
          | [] ->
              [%log info]
                "Block with state hash %s is not along canonical chain"
                state_hash ;
              None
          | block_infos ->
              Some (state_hash, block_infos)
        in
        let%bind state_hashes =
          query_db pool
            ~f:(fun db -> Sql.Block.get_state_hashes_by_slot db slot)
            ~item:"ids by slot"
        in
        Deferred.List.find_map state_hashes ~f:block_infos_from_state_hash
      in
      let num_tries = 5 in
      let%bind block_infos, usable_max_slot =
        let rec try_slot slot tries_left =
          if tries_left <= 0 then (
            [%log fatal] "Could not find canonical chain after trying %d slots"
              num_tries ;
            Core_kernel.exit 1 ) ;
          match%bind find_canonical_chain slot with
          | None ->
              try_slot (slot - 1) (tries_left - 1)
          | Some (state_hash, block_infos) ->
              [%log info]
                "Found possible canonical chain to target state hash %s at \
                 slot %d"
                state_hash slot ;
              return (block_infos, slot)
        in
        try_slot max_slot num_tries
      in
      let finalized_csv_only =
        usable_max_slot < ((input.epoch + 1) * slots_per_epoch) - 1
      in
      if finalized_csv_only then (
        if usable_max_slot < (input.epoch * slots_per_epoch) + 3500 then (
          [%log fatal]
            "Insufficient archive data for finalizing previous epoch CSV: \
             maximum usable global slot is less than slot 3500 slot in the \
             current epoch" ;
          Core_kernel.exit 1 ) )
      else if usable_max_slot < ((input.epoch + 1) * slots_per_epoch) - 1 then (
        [%log fatal]
          "Insufficient archive data for creating preliminary CSV for current \
           epoch: maximum usable global slot is less than last slot in the \
           current epoch" ;
        Core_kernel.exit 1 ) ;
      let csv_out_channel_opt =
        if not finalized_csv_only then Some (Out_channel.create csv_file)
        else None
      in
      ( match csv_out_channel_opt with
      | None ->
          ()
      | Some csv_out_channel ->
          write_csv_header ~csv_out_channel ) ;
      ( match (preliminary_csv_file_opt, finalized_csv_only) with
      | None, true ->
          [%log fatal]
            "Insufficient data for preliminary CSV for current epoch, and no \
             preliminary CSV from previous epoch provided" ;
          Core_kernel.exit 1
      | Some _, true ->
          [%log info]
            "Producing finalized CSV for previous epoch, no preliminary CSV \
             for current epoch"
      | None, false ->
          [%log info]
            "Producing only preliminary CSV for current epoch, no finalized \
             CSV for previous epoch"
      | Some _, false ->
          [%log info]
            "Producing preliminary CSV for current epoch and finalized CSV \
             for previous epoch" ) ;
      let block_ids =
        (* examine blocks in current epoch *)
        let min_slot = input.epoch * slots_per_epoch in
        let max_slot_int64 = min_slot + slots_per_epoch - 1 |> Int64.of_int in
        let min_slot_int64 = Int64.of_int min_slot in
        let relevant_block_infos =
          List.filter block_infos ~f:(fun {global_slot; _} ->
              Int64.( >= ) global_slot min_slot_int64
              && Int64.( <= ) global_slot max_slot_int64 )
        in
        let ids = List.map relevant_block_infos ~f:(fun {id; _} -> id) in
        (* build mapping from global slots to state and ledger hashes *)
        List.iter block_infos
          ~f:(fun {global_slot; state_hash; ledger_hash; _} ->
            Hashtbl.add_exn global_slot_hashes_tbl ~key:global_slot
              ~data:
                ( State_hash.of_string state_hash
                , Ledger_hash.of_string ledger_hash ) ) ;
        Int.Set.of_list ids
      in
      (* check that genesis block is in chain to target hash
         assumption: genesis block occupies global slot 0
      *)
      if Int64.Table.mem global_slot_hashes_tbl Int64.zero then
        [%log info]
          "Block chain leading to target state hash includes genesis block, \
           length = %d"
          (Int.Set.length block_ids)
      else (
        [%log fatal]
          "Block chain leading to target state hash does not include genesis \
           block; database contains unparented block" ;
        Core_kernel.exit 1 ) ;
      [%log info] "Building delegatee table " ;
      (* table of account public keys to delegatee public keys *)
      let delegatee_tbl = Public_key.Compressed.Table.create () in
      Ledger.iteri ledger ~f:(fun _ndx acct ->
          ignore
            (Option.map acct.delegate ~f:(fun delegate ->
                 match
                   Public_key.Compressed.Table.add delegatee_tbl
                     ~key:acct.public_key ~data:delegate
                 with
                 | `Ok ->
                     ()
                 | `Duplicate ->
                     failwith "Duplicate account in initial staking ledger" ))
      ) ;
      let slot_3500 = (input.epoch * slots_per_epoch) + 3500 |> Int64.of_int in
      [%log info] "Computing delegation information for payout addresses" ;
      let%bind payout_infos =
        (* sets for quick lookups *)
        let foundation_addresses =
          String.Set.of_list Payout_addresses.foundation_addresses
        in
        let o1_addresses = String.Set.of_list Payout_addresses.o1_addresses in
        Deferred.List.map payout_addresses ~f:(fun addr ->
            let%bind payout_id = pk_id_of_pk pool addr in
            let delegation_source =
              if String.Set.mem foundation_addresses addr then Mina_foundation
              else if String.Set.mem o1_addresses addr then O1
              else
                failwithf
                  "Payout address %s is neither a Foundation nor O1 delegator"
                  addr ()
            in
            let payout_pk = Public_key.Compressed.of_base58_check_exn addr in
            let delegatee =
              match
                Public_key.Compressed.Table.find delegatee_tbl payout_pk
              with
              | Some pk ->
                  pk
              | None ->
                  failwithf "No delegatee for payout address %s" addr ()
            in
            let delegatee_str =
              Public_key.Compressed.to_base58_check delegatee
            in
            let%bind delegatee_id = pk_id_of_pk pool delegatee_str in
            let%bind payments_from_delegatee_raw =
              query_db pool
                ~f:(fun db ->
                  Sql.User_command.run_payments_by_source_and_receiver db
                    ~source_id:delegatee_id ~receiver_id:payout_id )
                ~item:"payments from delegatee"
            in
            let compare_by_global_slot p1 p2 =
              let open Sql.User_command in
              Int64.compare p1.global_slot p2.global_slot
            in
            (* only payments in canonical chain *)
            let min_payment_slot =
              input.epoch * slots_per_epoch |> Int64.of_int
            in
            let payments_from_delegatee =
              List.filter payments_from_delegatee_raw ~f:(fun payment ->
                  Int.Set.mem block_ids payment.block_id
                  && payment.global_slot >= min_payment_slot )
              |> List.sort ~compare:compare_by_global_slot
            in
            let payment_amount_and_slot (user_cmd : Sql.User_command.t) =
              `Assoc
                [ ( "amount"
                  , Option.value_map user_cmd.amount ~default:`Null
                      ~f:(fun amt ->
                        `String
                          ( Int64.to_string amt |> Currency.Amount.of_string
                          |> Currency.Amount.to_formatted_string ) ) )
                ; ( "global_slot"
                  , `String (Int64.to_string user_cmd.global_slot) ) ]
            in
            let payment_sender_amount_and_slot sender_pk
                (user_cmd : Sql.User_command.t) =
              `Assoc
                [ ( "sender"
                  , `String (Public_key.Compressed.to_base58_check sender_pk)
                  )
                ; ( "amount"
                  , Option.value_map user_cmd.amount ~default:`Null
                      ~f:(fun amt ->
                        `String
                          ( Int64.to_string amt |> Currency.Amount.of_string
                          |> Currency.Amount.to_formatted_string ) ) )
                ; ( "global_slot"
                  , `String (Int64.to_string user_cmd.global_slot) ) ]
            in
            [%log info]
              "Direct payments from delegatee $delegatee to payout address \
               $payout_addr"
              ~metadata:
                [ ("delegatee", Public_key.Compressed.to_yojson delegatee)
                ; ("payout_addr", Public_key.Compressed.to_yojson payout_pk)
                ; ( "payments"
                  , `List
                      (List.map payments_from_delegatee
                         ~f:payment_amount_and_slot) ) ] ;
            let%bind coinbase_receiver_ids =
              match%map
                Caqti_async.Pool.use
                  (fun db ->
                    Sql.Coinbase_receivers_for_block_creator.run db
                      ~block_creator_id:delegatee_id )
                  pool
              with
              | Ok ids ->
                  ids
              | Error err ->
                  failwithf
                    "Error getting coinbase receiver ids from blocks where \
                     the delegatee %s is the block creator, %s"
                    delegatee_str (Caqti_error.show err) ()
            in
            let%bind payments_by_coinbase_receivers =
              match%map
                Archive_lib.Processor.deferred_result_list_fold
                  coinbase_receiver_ids ~init:[]
                  ~f:(fun accum coinbase_receiver_id ->
                    let%bind cb_receiver_pk =
                      pk_of_pk_id pool coinbase_receiver_id
                    in
                    let%map payments_raw =
                      query_db pool
                        ~f:(fun db ->
                          Sql.User_command.run_payments_by_source_and_receiver
                            db ~source_id:coinbase_receiver_id
                            ~receiver_id:payout_id )
                        ~item:
                          (sprintf
                             "Payments from coinbase receiver with id %d to \
                              payment address"
                             coinbase_receiver_id)
                    in
                    let payments =
                      (* only payments in canonical chain *)
                      List.filter payments_raw ~f:(fun payment ->
                          Int.Set.mem block_ids payment.block_id
                          && payment.global_slot >= min_payment_slot )
                      |> List.sort ~compare:compare_by_global_slot
                    in
                    Ok ((cb_receiver_pk, payments) :: accum) )
              with
              | Ok payments ->
                  payments
              | Error err ->
                  failwithf
                    "Error getting payments from coinbase receivers: %s"
                    (Caqti_error.show err) ()
            in
            if not (List.is_empty payments_by_coinbase_receivers) then
              [%log info]
                "Payments from delegatee $delegatee to payout address \
                 $payout_addr via a coinbase receiver"
                ~metadata:
                  [ ("delegatee", Public_key.Compressed.to_yojson delegatee)
                  ; ("payout_addr", Public_key.Compressed.to_yojson payout_pk)
                  ; ( "payments_via_coinbase_receivers"
                    , `List
                        (List.map payments_by_coinbase_receivers
                           ~f:(fun (cb_receiver, payments) ->
                             `Assoc
                               [ ( "coinbase_receiver"
                                 , Public_key.Compressed.to_yojson cb_receiver
                                 )
                               ; ( "payments"
                                 , `List
                                     (List.map payments
                                        ~f:payment_amount_and_slot) ) ] )) ) ] ;
            let payments_from_coinbase_receivers =
              (* to check compliance, don't need to know the payment source *)
              List.concat_map payments_by_coinbase_receivers
                ~f:(fun (_cb_receiver, payments) -> payments)
            in
            let payments_from_known_senders =
              payments_from_delegatee @ payments_from_coinbase_receivers
            in
            let%bind payments_from_anyone =
              let%map payments_raw =
                query_db pool
                  ~f:(fun db ->
                    Sql.User_command.run_payments_by_receiver db
                      ~receiver_id:payout_id )
                  ~item:"Payments to payment address"
              in
              (* only payments in canonical chain
                 don't include payments from delegatee or coinbase receivers
              *)
              List.filter payments_raw ~f:(fun payment ->
                  Int.Set.mem block_ids payment.block_id
                  && payment.global_slot >= min_payment_slot
                  && not
                       (List.mem payments_from_known_senders payment
                          ~equal:Sql.User_command.equal) )
              |> List.sort ~compare:compare_by_global_slot
            in
            let%map senders_and_payments_from_anyone =
              Deferred.List.map payments_from_anyone ~f:(fun payment ->
                  let%map sender_pk = pk_of_pk_id pool payment.source_id in
                  (sender_pk, payment) )
            in
            if not (List.is_empty senders_and_payments_from_anyone) then
              [%log info]
                "Payments from others, neither the delegatee $delegatee nor a \
                 coinbase receiver, to payout address $payout_addr"
                ~metadata:
                  [ ("delegatee", Public_key.Compressed.to_yojson delegatee)
                  ; ("payout_addr", Public_key.Compressed.to_yojson payout_pk)
                  ; ( "payments_from_others"
                    , `Assoc
                        (List.map senders_and_payments_from_anyone
                           ~f:(fun (sender_pk, payment) ->
                             ( "payment"
                             , payment_sender_amount_and_slot sender_pk payment
                             ) )) ) ] ;
            let payments =
              payments_from_known_senders @ payments_from_anyone
            in
            let payments_to_slot_3500, payments_past_slot_3500 =
              List.partition_tf payments ~f:(fun payment ->
                  Int64.( <= ) payment.global_slot slot_3500 )
            in
            { payout_pk
            ; payout_id
            ; delegation_source
            ; delegatee
            ; delegatee_id
            ; payments
            ; payments_to_slot_3500
            ; payments_past_slot_3500 } )
      in
      let epoch_uint32 = input.epoch |> Unsigned.UInt32.of_int in
      let%bind () =
        Deferred.List.iter payout_infos ~f:(fun payout_info ->
            [%log info]
              "Examining payments from delegatee %s to payout address %s"
              (Public_key.Compressed.to_base58_check payout_info.delegatee)
              (Public_key.Compressed.to_base58_check payout_info.payout_pk) ;
            let%bind num_blocks_produced =
              (* blocks produced in current epoch *)
              let%map creator_block_ids =
                block_ids_in_epoch pool payout_info.delegatee_id epoch_uint32
              in
              let filtered_block_ids =
                List.filter creator_block_ids ~f:(Int.Set.mem block_ids)
              in
              List.length filtered_block_ids
            in
            if num_blocks_produced > 0 && List.is_empty payout_info.payments
            then
              [%log error]
                "DELINQUENCY: In epoch %d, delegatee %s made no payments to \
                 payout address %s"
                input.epoch
                (Public_key.Compressed.to_base58_check payout_info.delegatee)
                (Public_key.Compressed.to_base58_check payout_info.payout_pk) ;
            let add_payment total (payment : Sql.User_command.t) =
              match payment.amount with
              | None ->
                  (* should be unreachable *)
                  failwith "Payment contains no total"
              | Some amount ->
                  Int64.( + ) amount total
            in
            let deficit_tbl_key : Delegatee_payout_address.t =
              { payout_addr= payout_info.payout_pk
              ; delegatee= payout_info.delegatee }
            in
            let { payout_received= prev_payout_received
                ; deficit= prev_epoch_deficit } =
              if input.epoch = 0 then
                { payout_received= Currency.Amount.zero
                ; deficit= Currency.Amount.zero }
              else Deficit.Table.find_exn deficit_tbl deficit_tbl_key
            in
            let total_to_slot_3500 =
              List.fold payout_info.payments_to_slot_3500 ~init:0L
                ~f:add_payment
            in
            let to_slot_3500_available_for_this_epoch =
              if Currency.Amount.( > ) prev_epoch_deficit Currency.Amount.zero
              then (
                [%log info]
                  "In epoch %d, delegatee %s had a deficit amount of %s to \
                   payout address %s; "
                  (input.epoch - 1)
                  (Currency.Amount.to_formatted_string prev_epoch_deficit)
                  (Public_key.Compressed.to_base58_check payout_info.delegatee)
                  (Public_key.Compressed.to_base58_check payout_info.payout_pk) ;
                let total_to_slot_3500_as_currency =
                  total_to_slot_3500 |> Unsigned.UInt64.of_int64
                  |> Currency.Amount.of_uint64
                in
                let remaining_deficit =
                  match
                    Currency.Amount.( - ) prev_epoch_deficit
                      total_to_slot_3500_as_currency
                  with
                  | None ->
                      Currency.Amount.zero
                  | Some diff ->
                      diff
                in
                if Currency.Amount.( > ) remaining_deficit Currency.Amount.zero
                then
                  [%log error]
                    "DELINQUENCY: Deficit in epoch %d from delegatee \
                     $delegatee to payout address $payout_addr is not \
                     satisified by payments through slot 3500 in epoch %d, \
                     remaining deficit is $remaining_deficit"
                    (input.epoch - 1) input.epoch
                    ~metadata:
                      [ ( "delegatee"
                        , Public_key.Compressed.to_yojson payout_info.delegatee
                        )
                      ; ( "payout_addr"
                        , Public_key.Compressed.to_yojson payout_info.payout_pk
                        )
                      ; ( "remaining_deficit"
                        , `String
                            (Currency.Amount.to_formatted_string
                               remaining_deficit) ) ]
                else
                  [%log info]
                    "Deficit in epoch %d from delegatee $delegatee to payout \
                     address $payout_addr is satisified by payments through \
                     slot 3500 in epoch %d"
                    (input.epoch - 1) input.epoch
                    ~metadata:
                      [ ( "delegatee"
                        , Public_key.Compressed.to_yojson payout_info.delegatee
                        )
                      ; ( "payout_addr"
                        , Public_key.Compressed.to_yojson payout_info.payout_pk
                        ) ] ;
                ( if input.epoch > 0 then
                  let deficit_reduction =
                    match
                      Currency.Amount.( - ) prev_epoch_deficit
                        remaining_deficit
                    with
                    | Some diff ->
                        diff
                    | None ->
                        failwith "Underflow calculating deficit reduction"
                  in
                  let updated_payout_received =
                    match
                      Currency.Amount.( + ) prev_payout_received
                        deficit_reduction
                    with
                    | Some sum ->
                        sum
                    | None ->
                        failwith "Overflow calculating updated payout received"
                  in
                  let data =
                    { payout_received= updated_payout_received
                    ; deficit= remaining_deficit }
                  in
                  Deficit.Table.set deficit_tbl ~key:deficit_tbl_key ~data ) ;
                let to_slot_3500_available =
                  match
                    Currency.Amount.( - ) total_to_slot_3500_as_currency
                      remaining_deficit
                  with
                  | None ->
                      Currency.Amount.zero
                  | Some diff ->
                      diff
                in
                to_slot_3500_available |> Currency.Amount.to_uint64
                |> Unsigned.UInt64.to_int64 )
              else total_to_slot_3500
            in
            if Int64.( > ) to_slot_3500_available_for_this_epoch Int64.zero
            then
              [%log info]
                "Total payments through slot 3500 in next epoch were %s, of \
                 which allocated %s to this epoch"
                (currency_string_of_int64 total_to_slot_3500)
                (currency_string_of_int64 to_slot_3500_available_for_this_epoch) ;
            let payment_total_in_epoch =
              Int64.( + ) to_slot_3500_available_for_this_epoch
                (List.fold payout_info.payments_past_slot_3500 ~init:0L
                   ~f:add_payment)
            in
            [%log info]
              "In epoch %d, delegatee %s made payments totaling %sto payout \
               address %s"
              input.epoch
              (Public_key.Compressed.to_base58_check payout_info.delegatee)
              (currency_string_of_int64 payment_total_in_epoch)
              (Public_key.Compressed.to_base58_check payout_info.payout_pk) ;
            let delegated_stake =
              compute_delegated_stake ledger payout_info.delegatee
            in
            let delegated_amount =
              get_account_balance_as_amount ledger payout_info.payout_pk
            in
            let fraction_of_stake =
              Float.round_decimal ~decimal_digits:5
                (Float.( / )
                   ( Currency.Amount.to_string delegated_amount
                   |> Float.of_string )
                   ( Currency.Amount.to_string delegated_stake
                   |> Float.of_string ))
            in
            let coinbase_amount = Float.( * ) 0.95 720.0 in
            [%log info]
              "Delegatee %s has a delegated stake of %s, of that amount, \
               payout address %s contributed %s, a fraction of %0.5f"
              (Public_key.Compressed.to_base58_check payout_info.delegatee)
              (Currency.Amount.to_formatted_string delegated_stake)
              (Public_key.Compressed.to_base58_check payout_info.payout_pk)
              (Currency.Amount.to_formatted_string delegated_amount)
              fraction_of_stake ;
            let payout_obligation_per_block =
              Float.( * ) fraction_of_stake coinbase_amount
            in
            let total_payout_obligation =
              Float.( * )
                (Float.of_int num_blocks_produced)
                payout_obligation_per_block
              |> Float.to_string |> Currency.Amount.of_formatted_string
            in
            [%log info]
              "In epoch %d, delegatee %s produced %d blocks; for payout \
               address %s, the payout obligation per-block is %0.9f, the \
               total obligation is %s"
              input.epoch
              (Public_key.Compressed.to_base58_check payout_info.delegatee)
              num_blocks_produced
              (Public_key.Compressed.to_base58_check payout_info.payout_pk)
              payout_obligation_per_block
              (Currency.Amount.to_formatted_string total_payout_obligation) ;
            let payment_total_as_amount =
              Int64.to_string payment_total_in_epoch
              |> Currency.Amount.of_string
            in
            if
              Currency.Amount.( < ) payment_total_as_amount
                total_payout_obligation
            then
              [%log error]
                "DELINQUENCY: In epoch %d, delegatee %s paid a total of %s to \
                 payout address %s, which is less than the payout obligation \
                 of %s"
                input.epoch
                (Public_key.Compressed.to_base58_check payout_info.delegatee)
                (Currency.Amount.to_formatted_string payment_total_as_amount)
                (Public_key.Compressed.to_base58_check payout_info.payout_pk)
                (Currency.Amount.to_formatted_string total_payout_obligation)
            else
              [%log info]
                "In epoch %d, delegatee %s paid a total of %s to payout \
                 address %s, satisfying the payout obligation of %s"
                input.epoch
                (Public_key.Compressed.to_base58_check payout_info.delegatee)
                (Currency.Amount.to_formatted_string payment_total_as_amount)
                (Public_key.Compressed.to_base58_check payout_info.payout_pk)
                (Currency.Amount.to_formatted_string total_payout_obligation) ;
            ( match csv_out_channel_opt with
            | None ->
                ()
            | Some csv_out_channel ->
                write_csv_line ~csv_out_channel
                  ~payout_addr:payout_info.payout_pk
                  ~balance:(account_balance ledger payout_info.payout_pk)
                  ~delegatee:payout_info.delegatee ~delegation:delegated_stake
                  ~blocks_won:num_blocks_produced
                  ~payout_obligation:total_payout_obligation
                  ~payout_received:payment_total_as_amount ) ;
            return () )
      in
      ( match preliminary_csv_file_opt with
      | None ->
          ()
      | Some prelim_csv_file ->
          (* write finalized CSV for previous epoch *)
          let finalized_csv_file = prelim_csv_file ^ ".finalized" in
          let csv_out_channel = Out_channel.create finalized_csv_file in
          let updated_csv_datas =
            List.map csv_datas
              ~f:(fun ({payout_addr; delegatee; _} as csv_data) ->
                let key : Delegatee_payout_address.t =
                  {payout_addr; delegatee}
                in
                let {payout_received; deficit} =
                  Deficit.Table.find_exn deficit_tbl key
                in
                let current_check =
                  Currency.Amount.equal deficit Currency.Amount.zero
                in
                {csv_data with payout_received; deficit; check= current_check}
            )
          in
          write_csv_header ~csv_out_channel ;
          List.iter updated_csv_datas
            ~f:(write_csv_line_of_csv_data ~csv_out_channel) ;
          Out_channel.close csv_out_channel ) ;
      Option.iter csv_out_channel_opt ~f:Out_channel.close ;
      Deferred.unit

let () =
  Command.(
    run
      (let open Let_syntax in
      Command.async
        ~summary:
          "Check compliance for Mina Foundation and O(1) Labs delegations"
        (let%map input_file =
           Param.flag "--input-file"
             ~doc:
               "file File containing the starting staking ledger and epoch \
                number"
             Param.(required string)
         and csv_file =
           Param.flag "--output-csv-file"
             ~doc:"file CSV file to write containing payment statuses"
             Param.(required string)
         and preliminary_csv_file_opt =
           Param.flag "--preliminary-csv-file"
             ~doc:"file Preliminary CSV file from previous epoch"
             Param.(optional string)
         and archive_uri =
           Param.flag "--archive-uri"
             ~doc:
               "URI URI for connecting to the archive database (e.g., \
                postgres://$USER@localhost:5432/archiver)"
             Param.(required string)
         and payout_addresses =
           Param.anon Anons.(sequence ("PAYOUT ADDRESSES" %: Param.string))
         in
         main ~input_file ~csv_file ~preliminary_csv_file_opt ~archive_uri
           ~payout_addresses)))
