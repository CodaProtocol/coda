(* replayer.ml -- replay transactions from archive node database *)

open Core
open Async
open Coda_base

(* given a target block B to replay to:

   target state hash  = protocol state hash in B
   target proof       = blockchain SNARK proof of the protocol state in B
*)
type input =
  { target_state_hash: State_hash.t
  ; target_proof: Proof.t
  ; genesis_ledger: Account.t list }
[@@deriving yojson]

type output =
  { target_state_hash: State_hash.t
  ; target_proof: Proof.t
  ; target_ledger: Account.t list }
[@@deriving yojson]

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let create_ledger accounts =
  let open Coda_base in
  let depth = constraint_constants.ledger_depth in
  let ledger = Ledger.create_ephemeral ~depth () in
  List.iter accounts ~f:(fun acct ->
      let pk = Account.public_key acct in
      let token_id = Account.token acct in
      let acct_id = Account_id.create pk token_id in
      Ledger.create_new_account_exn ledger acct_id acct ) ;
  ledger

let json_ledger_hash_of_ledger ledger =
  Ledger_hash.to_yojson @@ Ledger.merkle_root ledger

let create_output target_state_hash target_proof ledger =
  let target_ledger = Ledger.to_list ledger in
  {target_state_hash; target_proof; target_ledger}

(* map from global slots to expected ledger hashes *)

let global_slot_ledger_hash_tbl : (int64, Ledger_hash.t) Hashtbl.t =
  Int64.Table.create ()

(* cache of account keys *)
let pk_tbl : (int, Account.key) Hashtbl.t = Int.Table.create ()

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
            failwithf
              "Error decoding retrieved public key \"%s\" with id %d, error: %s"
              pk pk_id (Error.to_string_hum err) () )
      | Ok None ->
          failwithf "Could not find public key with id %d" pk_id ()
      | Error msg ->
          failwithf "Error retrieving public key with id %d, error: %s" pk_id
            (Caqti_error.show msg) () )

(* cache of fee transfers for coinbases *)
module Fee_transfer_key = struct
  module T = struct
    type t = int64 * int [@@deriving hash, sexp, compare]
  end

  type t = T.t

  include Hashable.Make (T)
end

let fee_transfer_tbl : (Fee_transfer_key.t, Coinbase_fee_transfer.t) Hashtbl.t
    =
  Fee_transfer_key.Table.create ()

let cache_fee_transfer_via_coinbase pool
    (internal_cmd : Sql.Internal_command.t) =
  match internal_cmd.type_ with
  | "fee_transfer_via_coinbase" ->
      let%map receiver_pk = pk_of_pk_id pool internal_cmd.receiver_id in
      let fee =
        Currency.Fee.of_uint64 (Unsigned.UInt64.of_int64 internal_cmd.fee)
      in
      let fee_transfer = Coinbase_fee_transfer.create ~receiver_pk ~fee in
      Hashtbl.add_exn fee_transfer_tbl
        ~key:(internal_cmd.global_slot, internal_cmd.sequence_no)
        ~data:fee_transfer
  | _ ->
      Deferred.unit

let run_internal_command ~logger ~pool ~ledger (cmd : Sql.Internal_command.t) =
  [%log info]
    "Applying internal command with global slot %Ld, sequence number %d, and \
     secondary sequence number %d"
    cmd.global_slot cmd.sequence_no cmd.secondary_sequence_no ;
  let%bind receiver_pk = pk_of_pk_id pool cmd.receiver_id in
  let fee = Currency.Fee.of_uint64 (Unsigned.UInt64.of_int64 cmd.fee) in
  let fee_token = Token_id.of_uint64 (Unsigned.UInt64.of_int64 cmd.token) in
  let txn_global_slot =
    cmd.global_slot |> Unsigned.UInt32.of_int64
    |> Coda_numbers.Global_slot.of_uint32
  in
  let fail_on_error err =
    failwithf
      "Could not apply internal command with global slot %Ld and sequence \
       number %d, error: %s"
      cmd.global_slot cmd.sequence_no (Error.to_string_hum err) ()
  in
  let open Coda_base.Ledger in
  match cmd.type_ with
  | "fee_transfer" -> (
      let fee_transfer =
        Fee_transfer.create_single ~receiver_pk ~fee ~fee_token
      in
      let undo_or_error =
        Ledger.apply_fee_transfer ~constraint_constants ~txn_global_slot ledger
          fee_transfer
      in
      match undo_or_error with
      | Ok _undo ->
          Deferred.unit
      | Error err ->
          fail_on_error err )
  | "coinbase" -> (
      let amount = Currency.Fee.to_uint64 fee |> Currency.Amount.of_uint64 in
      (* combining situation 1: add cached coinbase fee transfer, if it exists *)
      let fee_transfer =
        Hashtbl.find fee_transfer_tbl (cmd.global_slot, cmd.sequence_no)
      in
      let coinbase =
        match Coinbase.create ~amount ~receiver:receiver_pk ~fee_transfer with
        | Ok cb ->
            cb
        | Error err ->
            failwithf "Error creating coinbase for internal command, error: %s"
              (Error.to_string_hum err) ()
      in
      let undo_or_error =
        apply_coinbase ~constraint_constants ~txn_global_slot ledger coinbase
      in
      match undo_or_error with
      | Ok _undo ->
          Deferred.unit
      | Error err ->
          fail_on_error err )
  | "fee_transfer_via_coinbase" ->
      (* these are combined in the "coinbase" case *)
      Deferred.unit
  | _ ->
      failwithf "Unknown internal command \"%s\"" cmd.type_ ()

let apply_combined_fee_transfer ~logger ~pool ~ledger
    (cmd1 : Sql.Internal_command.t) (cmd2 : Sql.Internal_command.t) =
  [%log info] "Applying combined fee transfers with sequence number %d"
    cmd1.sequence_no ;
  let fee_transfer_of_cmd (cmd : Sql.Internal_command.t) =
    if not (String.equal cmd.type_ "fee_transfer") then
      failwithf "Expected fee transfer, got: %s" cmd.type_ () ;
    let%map receiver_pk = pk_of_pk_id pool cmd.receiver_id in
    let fee = Currency.Fee.of_uint64 (Unsigned.UInt64.of_int64 cmd.fee) in
    let fee_token = Token_id.of_uint64 (Unsigned.UInt64.of_int64 cmd.token) in
    Fee_transfer.Single.create ~receiver_pk ~fee ~fee_token
  in
  let%bind fee_transfer1 = fee_transfer_of_cmd cmd1 in
  let%bind fee_transfer2 = fee_transfer_of_cmd cmd2 in
  let fee_transfer =
    match Fee_transfer.create fee_transfer1 (Some fee_transfer2) with
    | Ok ft ->
        ft
    | Error err ->
        failwithf "Could not create combined fee transfer, error: %s"
          (Error.to_string_hum err) ()
  in
  let txn_global_slot =
    cmd2.global_slot |> Unsigned.UInt32.of_int64
    |> Coda_numbers.Global_slot.of_uint32
  in
  let undo_or_error =
    Ledger.apply_fee_transfer ~constraint_constants ~txn_global_slot ledger
      fee_transfer
  in
  match undo_or_error with
  | Ok _undo ->
      Deferred.unit
  | Error err ->
      failwithf
        "Error applying combined fee transfer with sequence number %d, error: \
         %s"
        cmd1.sequence_no (Error.to_string_hum err) ()

let body_of_sql_user_cmd pool
    ({type_; source_id; receiver_id; token= tok; amount; global_slot; _} :
      Sql.User_command.t) : Signed_command_payload.Body.t Deferred.t =
  let open Signed_command_payload.Body in
  let open Deferred.Let_syntax in
  let%bind source_pk = pk_of_pk_id pool source_id in
  let%map receiver_pk = pk_of_pk_id pool receiver_id in
  let token_id = Token_id.of_uint64 (Unsigned.UInt64.of_int64 tok) in
  let amount =
    Option.map amount
      ~f:(Fn.compose Currency.Amount.of_uint64 Unsigned.UInt64.of_int64)
  in
  (* possibilities from user_command_type enum in SQL schema *)
  (* TODO: handle "snapp" user commands *)
  match type_ with
  | "payment" ->
      if Option.is_none amount then
        failwithf "Payment at global slot %Ld has NULL amount" global_slot () ;
      let amount = Option.value_exn amount in
      Payment Payment_payload.Poly.{source_pk; receiver_pk; token_id; amount}
  | "delegation" ->
      Stake_delegation
        (Stake_delegation.Set_delegate
           {delegator= source_pk; new_delegate= receiver_pk})
  | "create_token" ->
      Create_new_token
        { New_token_payload.token_owner_pk= source_pk
        ; disable_new_accounts= false }
  | "create_account" ->
      Create_token_account
        { New_account_payload.token_id
        ; token_owner_pk= source_pk
        ; receiver_pk
        ; account_disabled= false }
  | "mint_tokens" ->
      if Option.is_none amount then
        failwithf "Mint token at global slot %Ld has NULL amount" global_slot
          () ;
      let amount = Option.value_exn amount in
      Mint_tokens
        { Minting_payload.token_id
        ; token_owner_pk= source_pk
        ; receiver_pk
        ; amount }
  | _ ->
      failwithf "Invalid user command type: %s" type_ ()

let run_user_command ~logger ~pool ~ledger (cmd : Sql.User_command.t) =
  [%log info]
    "Applying user command with nonce %Ld, global slot %Ld, and sequence \
     number %d"
    cmd.nonce cmd.global_slot cmd.sequence_no ;
  let%bind body = body_of_sql_user_cmd pool cmd in
  let%map fee_payer_pk = pk_of_pk_id pool cmd.fee_payer_id in
  let memo = Signed_command_memo.of_string cmd.memo in
  let payload =
    Signed_command_payload.create
      ~fee:(Currency.Fee.of_uint64 @@ Unsigned.UInt64.of_int64 cmd.fee)
      ~fee_token:(Token_id.of_uint64 @@ Unsigned.UInt64.of_int64 cmd.fee_token)
      ~fee_payer_pk
      ~nonce:(Unsigned.UInt32.of_int64 cmd.nonce)
      ~valid_until:None ~memo ~body
  in
  (* when applying the transaction, there's a check that the fee payer and
     signer keys are the same; since this transaction was accepted, we know
     those keys are the same
  *)
  let signer = Signature_lib.Public_key.decompress_exn fee_payer_pk in
  let signed_cmd =
    Signed_command.Poly.{payload; signer; signature= Signature.dummy}
  in
  (* the signature isn't checked when applying, the real signature was checked in the
     transaction SNARK, so deem the signature to be valid here
  *)
  let (`If_this_is_used_it_should_have_a_comment_justifying_it
        valid_signed_cmd) =
    Signed_command.to_valid_unsafe signed_cmd
  in
  let txn_global_slot = Unsigned.UInt32.of_int64 cmd.global_slot in
  match
    Ledger.apply_user_command ~constraint_constants ~txn_global_slot ledger
      valid_signed_cmd
  with
  | Ok _undo ->
      ()
  | Error err ->
      failwithf
        "User command with global slot %Ld and sequence number %d failed on \
         replay, error: %s"
        cmd.global_slot cmd.sequence_no (Error.to_string_hum err) ()

let unquoted_string_of_yojson json =
  (* Yojson.Safe.to_string produces double-quoted strings
     remove those quotes for SQL queries
  *)
  let s = Yojson.Safe.to_string json in
  String.sub s ~pos:1 ~len:(String.length s - 2)

let main ~input_file ~output_file ~archive_uri () =
  let logger = Logger.create () in
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
  let archive_uri = Uri.of_string archive_uri in
  match Caqti_async.connect_pool ~max_size:128 archive_uri with
  | Error e ->
      [%log error]
        ~metadata:[("error", `String (Caqti_error.show e))]
        "Failed to create a Caqti pool for Postgresql" ;
      exit 1
  | Ok pool ->
      [%log info] "Successfully created Caqti pool for Postgresql" ;
      let ledger = create_ledger input.genesis_ledger in
      let state_hash =
        State_hash.to_yojson input.target_state_hash
        |> unquoted_string_of_yojson
      in
      [%log info] "Loading global slots" ;
      let%bind global_slots =
        match%bind
          Caqti_async.Pool.use
            (fun db -> Sql.Global_slots_and_ledger_hashes.run db state_hash)
            pool
        with
        | Ok slots_and_hashes ->
            let slots =
              List.map slots_and_hashes ~f:(fun (slot, _hash) -> slot)
            in
            (* build mapping from global slots to ledger hashes *)
            List.iter slots_and_hashes ~f:(fun (slot, hash) ->
                Hashtbl.add_exn global_slot_ledger_hash_tbl ~key:slot
                  ~data:(Ledger_hash.of_string hash) ) ;
            return (Int64.Set.of_list slots)
        | Error msg ->
            [%log error] "Error getting global slots"
              ~metadata:[("error", `String (Caqti_error.show msg))] ;
            exit 1
      in
      [%log info] "Loading user command ids" ;
      let%bind user_cmd_ids =
        match%bind
          Caqti_async.Pool.use
            (fun db -> Sql.User_command_ids.run db state_hash)
            pool
        with
        | Ok ids ->
            return ids
        | Error msg ->
            [%log error] "Error getting user command ids"
              ~metadata:[("error", `String (Caqti_error.show msg))] ;
            exit 1
      in
      [%log info] "Loading internal command ids" ;
      let%bind internal_cmd_ids =
        match%bind
          Caqti_async.Pool.use
            (fun db -> Sql.Internal_command_ids.run db state_hash)
            pool
        with
        | Ok ids ->
            return ids
        | Error msg ->
            [%log error] "Error getting user command ids"
              ~metadata:[("error", `String (Caqti_error.show msg))] ;
            exit 1
      in
      [%log info] "Obtained %d user command ids and %d internal command ids"
        (List.length user_cmd_ids)
        (List.length internal_cmd_ids) ;
      let%bind unsorted_internal_cmds_list =
        Deferred.List.map internal_cmd_ids ~f:(fun id ->
            let open Deferred.Let_syntax in
            match%map
              Caqti_async.Pool.use
                (fun db -> Sql.Internal_command.run db id)
                pool
            with
            | Ok [] ->
                failwithf "Could not find any internal commands with id: %d" id
                  ()
            | Ok internal_cmds ->
                internal_cmds
            | Error msg ->
                failwithf
                  "Error querying for internal commands with id %d, error %s"
                  id (Caqti_error.show msg) () )
      in
      let unsorted_internal_cmds = List.concat unsorted_internal_cmds_list in
      (* filter out internal commands in blocks not along chain from target state hash *)
      let filtered_internal_cmds =
        List.filter unsorted_internal_cmds ~f:(fun cmd ->
            Int64.Set.mem global_slots cmd.global_slot )
      in
      let sorted_internal_cmds =
        List.sort filtered_internal_cmds ~compare:(fun ic1 ic2 ->
            let tuple (ic : Sql.Internal_command.t) =
              (ic.global_slot, ic.sequence_no, ic.secondary_sequence_no)
            in
            [%compare: int64 * int * int] (tuple ic1) (tuple ic2) )
      in
      (* populate cache of fee transfer via coinbase items *)
      let%bind () =
        Deferred.List.iter sorted_internal_cmds
          ~f:(cache_fee_transfer_via_coinbase pool)
      in
      let%bind unsorted_user_cmds_list =
        Deferred.List.map user_cmd_ids ~f:(fun id ->
            let open Deferred.Let_syntax in
            match%map
              Caqti_async.Pool.use (fun db -> Sql.User_command.run db id) pool
            with
            | Ok [] ->
                failwithf "Expected at least one user command with id %d" id ()
            | Ok user_cmds ->
                user_cmds
            | Error msg ->
                failwithf
                  "Error querying for user commands with id %d, error %s" id
                  (Caqti_error.show msg) () )
      in
      let unsorted_user_cmds = List.concat unsorted_user_cmds_list in
      (* filter out user commands in blocks not along chain from target state hash *)
      let filtered_user_cmds =
        List.filter unsorted_user_cmds ~f:(fun cmd ->
            Int64.Set.mem global_slots cmd.global_slot )
      in
      let sorted_user_cmds =
        List.sort filtered_user_cmds ~compare:(fun uc1 uc2 ->
            let tuple (uc : Sql.User_command.t) =
              (uc.global_slot, uc.sequence_no)
            in
            [%compare: int64 * int] (tuple uc1) (tuple uc2) )
      in
      (* apply commands in global slot, sequence order *)
      let rec apply_commands (internal_cmds : Sql.Internal_command.t list)
          (user_cmds : Sql.User_command.t list) ~last_global_slot =
        let log_on_slot_change curr_global_slot =
          if Int64.( > ) curr_global_slot last_global_slot then
            let expected_ledger_hash =
              Hashtbl.find_exn global_slot_ledger_hash_tbl last_global_slot
            in
            if
              Ledger_hash.equal
                (Ledger.merkle_root ledger)
                expected_ledger_hash
            then
              [%log info]
                "Applied all commands at global slot %Ld, got expected ledger \
                 hash"
                ~metadata:[("ledger_hash", json_ledger_hash_of_ledger ledger)]
                last_global_slot
            else (
              [%log error]
                "Applied all commands at global slot %Ld, ledger hash differs \
                 from expected ledger hash"
                ~metadata:
                  [ ("ledger_hash", json_ledger_hash_of_ledger ledger)
                  ; ( "expected_ledger_hash"
                    , Ledger_hash.to_yojson expected_ledger_hash ) ]
                last_global_slot ;
              Core_kernel.exit 1 )
        in
        let combine_or_run_internal_cmds (ic : Sql.Internal_command.t)
            (ics : Sql.Internal_command.t list) =
          match ics with
          | ic2 :: ics2
            when Int64.equal ic.global_slot ic2.global_slot
                 && Int.equal ic.sequence_no ic2.sequence_no
                 && String.equal ic.type_ "fee_transfer"
                 && String.equal ic.type_ ic2.type_ ->
              (* combining situation 2
                 two fee transfer commands with same global slot, sequence number
              *)
              log_on_slot_change ic.global_slot ;
              let%bind () =
                apply_combined_fee_transfer ~logger ~pool ~ledger ic ic2
              in
              apply_commands ics2 user_cmds ~last_global_slot:ic.global_slot
          | _ ->
              log_on_slot_change ic.global_slot ;
              let%bind () = run_internal_command ~logger ~pool ~ledger ic in
              apply_commands ics user_cmds ~last_global_slot:ic.global_slot
        in
        (* choose command with least global slot, sequence number
           TODO: check for gaps?
        *)
        let cmp_ic_uc (ic : Sql.Internal_command.t) (uc : Sql.User_command.t) =
          [%compare: int64 * int]
            (ic.global_slot, ic.sequence_no)
            (uc.global_slot, uc.sequence_no)
        in
        match (internal_cmds, user_cmds) with
        | [], [] ->
            Deferred.unit
        | [], uc :: ucs ->
            log_on_slot_change uc.global_slot ;
            let%bind () = run_user_command ~logger ~pool ~ledger uc in
            apply_commands [] ucs ~last_global_slot:uc.global_slot
        | ic :: _, uc :: ucs when cmp_ic_uc ic uc > 0 ->
            log_on_slot_change uc.global_slot ;
            let%bind () = run_user_command ~logger ~pool ~ledger uc in
            apply_commands internal_cmds ucs ~last_global_slot:uc.global_slot
        | ic :: ics, [] ->
            combine_or_run_internal_cmds ic ics
        | ic :: ics, uc :: _ when cmp_ic_uc ic uc < 0 ->
            combine_or_run_internal_cmds ic ics
        | ic :: _, _ :: __ ->
            failwithf
              "An internal command and a user command have the same global \
               slot %Ld and sequence number %d"
              ic.global_slot ic.sequence_no ()
      in
      [%log info] "At genesis, ledger hash"
        ~metadata:[("ledger_hash", json_ledger_hash_of_ledger ledger)] ;
      let%bind () =
        apply_commands sorted_internal_cmds sorted_user_cmds
          ~last_global_slot:0L
      in
      [%log info] "Writing output to $output_file"
        ~metadata:[("output_file", `String output_file)] ;
      let output =
        create_output input.target_state_hash input.target_proof ledger
        |> output_to_yojson |> Yojson.Safe.to_string
      in
      let%map writer = Async_unix.Writer.open_file output_file in
      Async.fprintf writer "%s\n" output ;
      ()

let () =
  Command.(
    run
      (let open Let_syntax in
      Command.async ~summary:"Replay transactions from Coda archive"
        (let%map input_file =
           Param.flag "--input-file"
             ~doc:"file File containing the genesis ledger"
             Param.(required string)
         and output_file =
           Param.flag "--output-file"
             ~doc:"file File containing the resulting ledger"
             Param.(required string)
         and archive_uri =
           Param.flag "--archive-uri"
             ~doc:
               "URI URI for connecting to the archive database (e.g., \
                postgres://$USER:$USER@localhost:5432/archiver)"
             Param.(required string)
         in
         main ~input_file ~output_file ~archive_uri)))
