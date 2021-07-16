[%%import "/src/config.mlh"]

(* Only show stdout for failed inline tests. *)
open Inline_test_quiet_logs
open Core_kernel
open Async_kernel
open Mina_base
open Currency
open O1trace
open Signature_lib

let option lab =
  Option.value_map ~default:(Or_error.error_string lab) ~f:(fun x -> Ok x)

module T = struct
  module Scan_state = Transaction_snark_scan_state
  module Pre_diff_info = Pre_diff_info

  module Staged_ledger_error = struct
    type t =
      | Non_zero_fee_excess of
          Scan_state.Space_partition.t * Transaction.t With_status.t list
      | Invalid_proofs of
          ( Ledger_proof.t
          * Transaction_snark.Statement.t
          * Mina_base.Sok_message.t )
          list
      | Couldn't_reach_verifier of Error.t
      | Pre_diff of Pre_diff_info.Error.t
      | Insufficient_work of string
      | Mismatched_statuses of
          Transaction.t With_status.t * Transaction_status.t
      | Invalid_public_key of Public_key.Compressed.t
      | Unexpected of Error.t
    [@@deriving sexp]

    let to_string = function
      | Couldn't_reach_verifier e ->
          Format.asprintf
            !"The verifier could not be reached: %{sexp:Error.t}"
            e
      | Non_zero_fee_excess (partition, txns) ->
          Format.asprintf
            !"Fee excess is non-zero for the transactions: %{sexp: \
              Transaction.t With_status.t list} and the current queue with \
              slots partitioned as %{sexp: Scan_state.Space_partition.t} \n"
            txns partition
      | Pre_diff pre_diff_error ->
          Format.asprintf
            !"Pre_diff_info.Error error: %{sexp:Pre_diff_info.Error.t}"
            pre_diff_error
      | Invalid_proofs ts ->
          Format.asprintf
            !"Verification failed for proofs with (statement, work_id, \
              prover): %{sexp: (Transaction_snark.Statement.t * int * string) \
              list}\n"
            (List.map ts ~f:(fun (_p, s, m) ->
                 ( s
                 , Transaction_snark.Statement.hash s
                 , Yojson.Safe.to_string
                   @@ Public_key.Compressed.to_yojson m.prover )))
      | Insufficient_work str ->
          str
      | Mismatched_statuses (transaction, status) ->
          Format.asprintf
            !"Got a different status %{sexp: Transaction_status.t} when \
              applying the transaction %{sexp: Transaction.t With_status.t}"
            status transaction
      | Invalid_public_key pk ->
          Format.asprintf
            !"A transaction contained an invalid public key %{sexp: \
              Public_key.Compressed.t}"
            pk
      | Unexpected e ->
          Error.to_string_hum e

    let to_error = Fn.compose Error.of_string to_string
  end

  let to_staged_ledger_or_error = function
    | Ok a ->
        Ok a
    | Error e ->
        Error (Staged_ledger_error.Unexpected e)

  type job = Scan_state.Available_job.t [@@deriving sexp]

  let verify_proofs ~logger ~verifier proofs =
    let statements () =
      `List
        (List.map proofs ~f:(fun (_, s, _) ->
             Transaction_snark.Statement.to_yojson s))
    in
    let log_error err_str ~metadata =
      [%log warn]
        ~metadata:
          ( [ ("statements", statements ())
            ; ("error", `String err_str)
            ; ( "sok_messages"
              , `List
                  (List.map proofs ~f:(fun (_, _, m) -> Sok_message.to_yojson m))
              )
            ]
          @ metadata )
        "Invalid transaction snark for statement $statement: $error" ;
      Deferred.return (Or_error.error_string err_str)
    in
    if
      List.exists proofs ~f:(fun (proof, statement, _msg) ->
          not
            (Transaction_snark.Statement.equal
               (Ledger_proof.statement proof)
               statement))
    then
      log_error "Statement and proof do not match"
        ~metadata:
          [ ( "statements_from_proof"
            , `List
                (List.map proofs ~f:(fun (p, _, _) ->
                     Transaction_snark.Statement.to_yojson
                       (Ledger_proof.statement p))) )
          ]
    else
      let start = Time.now () in
      match%map
        Verifier.verify_transaction_snarks verifier
          (List.map proofs ~f:(fun (proof, _, msg) -> (proof, msg)))
      with
      | Ok b ->
          let time_ms = Time.abs_diff (Time.now ()) start |> Time.Span.to_ms in
          [%log trace]
            ~metadata:
              [ ( "work_id"
                , `List
                    (List.map proofs ~f:(fun (_, s, _) ->
                         `Int (Transaction_snark.Statement.hash s))) )
              ; ("time", `Float time_ms)
              ]
            "Verification in apply_diff for work $work_id took $time ms" ;
          Ok b
      | Error e ->
          [%log fatal]
            ~metadata:
              [ ( "statement"
                , `List
                    (List.map proofs ~f:(fun (_, s, _) ->
                         Transaction_snark.Statement.to_yojson s)) )
              ; ("error", Error_json.error_to_yojson e)
              ]
            "Verifier error when checking transaction snark for statement \
             $statement: $error" ;
          Error e

  let map_opt xs ~f =
    with_return (fun { return } ->
        Some
          (List.map xs ~f:(fun x ->
               match f x with Some y -> y | None -> return None)))

  let verify ~logger ~verifier job_msg_proofs =
    let open Deferred.Let_syntax in
    match
      map_opt job_msg_proofs ~f:(fun (job, msg, proof) ->
          Option.map (Scan_state.statement_of_job job) ~f:(fun s ->
              (proof, s, msg)))
    with
    | None ->
        Deferred.return
          ( Or_error.errorf
              !"Error creating statement from job %{sexp:job list}"
              (List.map job_msg_proofs ~f:(fun (j, _, _) -> j))
          |> to_staged_ledger_or_error )
    | Some proof_statement_msgs -> (
        match%map verify_proofs ~logger ~verifier proof_statement_msgs with
        | Ok true ->
            Ok ()
        | Ok false ->
            Error (Staged_ledger_error.Invalid_proofs proof_statement_msgs)
        | Error e ->
            Error (Couldn't_reach_verifier e) )

  module Statement_scanner = struct
    include Scan_state.Make_statement_scanner
              (Deferred)
              (struct
                type t = unit

                let verify ~verifier:() _proofs = Deferred.return (Ok true)
              end)
  end

  module Statement_scanner_proof_verifier = struct
    type t = { logger : Logger.t; verifier : Verifier.t }

    let verify ~verifier:{ logger; verifier } ts =
      verify_proofs ~logger ~verifier
        (List.map ts ~f:(fun (p, m) -> (p, Ledger_proof.statement p, m)))
  end

  module Statement_scanner_with_proofs =
    Scan_state.Make_statement_scanner
      (Deferred)
      (Statement_scanner_proof_verifier)

  type t =
    { scan_state : Scan_state.t
    ; ledger :
        ((* Invariant: this is the ledger after having applied all the
            transactions in the above state. *)
         Ledger.attached_mask
        [@sexp.opaque])
    ; constraint_constants : Genesis_constants.Constraint_constants.t
    ; pending_coinbase_collection : Pending_coinbase.t
    }
  [@@deriving sexp]

  let proof_txns_with_state_hashes t =
    Scan_state.latest_ledger_proof t.scan_state
    |> Option.bind ~f:(Fn.compose Non_empty_list.of_list_opt snd)

  let scan_state { scan_state; _ } = scan_state

  let all_work_pairs t
      ~(get_state : State_hash.t -> Mina_state.Protocol_state.value Or_error.t)
      =
    Scan_state.all_work_pairs t.scan_state ~get_state

  let all_work_statements_exn t =
    Scan_state.all_work_statements_exn t.scan_state

  let pending_coinbase_collection { pending_coinbase_collection; _ } =
    pending_coinbase_collection

  let get_target ((proof, _), _) =
    let { Transaction_snark.Statement.target; _ } =
      Ledger_proof.statement proof
    in
    target

  let verify_scan_state_after_apply ~constraint_constants ~next_available_token
      ~pending_coinbase_collection ledger (scan_state : Scan_state.t) =
    let error_prefix =
      "Error verifying the parallel scan state after applying the diff."
    in
    let registers_end : _ Mina_state.Registers.t =
      { ledger
      ; next_available_token
      ; local_state = Mina_state.Local_state.empty
      ; pending_coinbase_stack =
          (let `Needs_some_work_for_snapps_on_mainnet =
             Mina_base.Util.todo_snapps
           in
           Pending_coinbase.latest_stack
             ~is_new_stack:
               false (* TODO: Not sure what is_new_stack should be. *)
             pending_coinbase_collection
           |> Or_error.ok_exn)
      }
    in
    let pending_coinbase_stack_equal = Pending_coinbase.Stack_versioned.equal in
    match Scan_state.latest_ledger_proof scan_state with
    | None ->
        Statement_scanner.check_invariants ~pending_coinbase_stack_equal
          ~pending_coinbase_stack_equal':pending_coinbase_stack_equal
          ~constraint_constants scan_state ~verifier:() ~error_prefix
          ~registers_end ~registers_begin:None
    | Some proof ->
        Statement_scanner.check_invariants ~pending_coinbase_stack_equal
          ~pending_coinbase_stack_equal':pending_coinbase_stack_equal
          ~constraint_constants scan_state ~verifier:() ~error_prefix
          ~registers_end
          ~registers_begin:(Some (get_target proof))

  let statement_exn ~constraint_constants t =
    let open Deferred.Let_syntax in
    match%map
      Statement_scanner.scan_statement ~constraint_constants t.scan_state
        ~verifier:()
    with
    | Ok s ->
        `Non_empty s
    | Error `Empty ->
        `Empty
    | Error (`Error e) ->
        failwithf !"statement_exn: %{sexp:Error.t}" e ()

  let of_scan_state_and_ledger_unchecked ~ledger ~scan_state
      ~constraint_constants ~pending_coinbase_collection =
    { ledger; scan_state; constraint_constants; pending_coinbase_collection }

  let of_scan_state_and_ledger ~logger
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ~verifier ~snarked_registers ~ledger ~scan_state
      ~pending_coinbase_collection =
    let open Deferred.Or_error.Let_syntax in
    let t =
      of_scan_state_and_ledger_unchecked ~ledger ~scan_state
        ~constraint_constants ~pending_coinbase_collection
    in
    let%bind () =
      Statement_scanner_with_proofs.check_invariants
        ~pending_coinbase_stack_equal:Unit.equal
        ~pending_coinbase_stack_equal':(fun () _ -> true)
        ~constraint_constants scan_state
        ~verifier:{ Statement_scanner_proof_verifier.logger; verifier }
        ~error_prefix:"Staged_ledger.of_scan_state_and_ledger"
        ~registers_begin:(Some snarked_registers)
        ~registers_end:
          { local_state = Mina_state.Local_state.empty
          ; ledger =
              Frozen_ledger_hash.of_ledger_hash (Ledger.merkle_root ledger)
          ; next_available_token = Ledger.next_available_token ledger
          ; pending_coinbase_stack = ()
          }
    in
    return t

  let of_scan_state_and_ledger_unchecked
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ~snarked_registers ~ledger ~scan_state ~pending_coinbase_collection =
    let open Deferred.Or_error.Let_syntax in
    let t =
      { ledger; scan_state; constraint_constants; pending_coinbase_collection }
    in
    let%bind () =
      Statement_scanner.check_invariants
        ~pending_coinbase_stack_equal:Unit.equal
        ~pending_coinbase_stack_equal':(fun () _ -> true)
        ~constraint_constants scan_state ~verifier:()
        ~error_prefix:"Staged_ledger.of_scan_state_and_ledger"
        ~registers_begin:(Some snarked_registers)
        ~registers_end:
          { local_state = Mina_state.Local_state.empty
          ; ledger =
              Frozen_ledger_hash.of_ledger_hash (Ledger.merkle_root ledger)
          ; next_available_token = Ledger.next_available_token ledger
          ; pending_coinbase_stack = ()
          }
    in
    return t

  let of_scan_state_pending_coinbases_and_snarked_ledger ~logger
      ~constraint_constants ~verifier ~scan_state ~snarked_ledger
      ~snarked_local_state ~expected_merkle_root ~pending_coinbases ~get_state =
    let open Deferred.Or_error.Let_syntax in
    let snarked_ledger_hash = Ledger.merkle_root snarked_ledger in
    let snarked_frozen_ledger_hash =
      Frozen_ledger_hash.of_ledger_hash snarked_ledger_hash
    in
    let snarked_next_available_token =
      Ledger.next_available_token snarked_ledger
    in
    let%bind txs_with_protocol_state =
      Scan_state.staged_transactions_with_protocol_states scan_state ~get_state
      |> Deferred.return
    in
    let%bind _ =
      Deferred.Or_error.List.iter txs_with_protocol_state
        ~f:(fun (tx, protocol_state) ->
          let%map.Async.Deferred () = Async.Scheduler.yield () in
          let%bind.Or_error txn_with_info =
            Ledger.apply_transaction ~constraint_constants
              ~txn_state_view:
                (Mina_state.Protocol_state.Body.view protocol_state.body)
              snarked_ledger tx.data
          in
          let computed_status =
            Ledger.Transaction_applied.user_command_status txn_with_info
          in
          if Transaction_status.equal tx.status computed_status then Ok ()
          else
            Or_error.errorf
              !"Mismatched user command status. Expected: %{sexp: \
                Transaction_status.t} Got: %{sexp: Transaction_status.t}"
              tx.status computed_status)
    in
    let%bind () =
      let staged_ledger_hash = Ledger.merkle_root snarked_ledger in
      Deferred.return
      @@ Result.ok_if_true
           (Ledger_hash.equal expected_merkle_root staged_ledger_hash)
           ~error:
             (Error.createf
                !"Mismatching merkle root Expected:%{sexp:Ledger_hash.t} \
                  Got:%{sexp:Ledger_hash.t}"
                expected_merkle_root staged_ledger_hash)
    in
    of_scan_state_and_ledger ~logger ~constraint_constants ~verifier
      ~snarked_registers:
        { ledger = snarked_frozen_ledger_hash
        ; next_available_token = snarked_next_available_token
        ; local_state = snarked_local_state
        ; pending_coinbase_stack = ()
        }
      ~ledger:snarked_ledger ~scan_state
      ~pending_coinbase_collection:pending_coinbases

  let copy
      { scan_state; ledger; constraint_constants; pending_coinbase_collection }
      =
    let new_mask = Ledger.Mask.create ~depth:(Ledger.depth ledger) () in
    { scan_state
    ; ledger = Ledger.register_mask ledger new_mask
    ; constraint_constants
    ; pending_coinbase_collection
    }

  let hash
      { scan_state
      ; ledger
      ; constraint_constants = _
      ; pending_coinbase_collection
      } : Staged_ledger_hash.t =
    Staged_ledger_hash.of_aux_ledger_and_coinbase_hash
      (Scan_state.hash scan_state)
      (Ledger.merkle_root ledger)
      pending_coinbase_collection

  [%%if call_logger]

  let hash t =
    Mina_debug.Call_logger.record_call "Staged_ledger.hash" ;
    hash t

  [%%endif]

  let ledger { ledger; _ } = ledger

  let create_exn ~constraint_constants ~ledger : t =
    { scan_state = Scan_state.empty ~constraint_constants ()
    ; ledger
    ; constraint_constants
    ; pending_coinbase_collection =
        Pending_coinbase.create
          ~depth:constraint_constants.pending_coinbase_depth ()
        |> Or_error.ok_exn
    }

  let current_ledger_proof t =
    Option.map
      (Scan_state.latest_ledger_proof t.scan_state)
      ~f:(Fn.compose fst fst)

  let replace_ledger_exn t ledger =
    [%test_result: Ledger_hash.t]
      ~message:"Cannot replace ledger since merkle_root differs"
      ~expect:(Ledger.merkle_root t.ledger)
      (Ledger.merkle_root ledger) ;
    { t with ledger }

  let sum_fees xs ~f =
    with_return (fun { return } ->
        Ok
          (List.fold ~init:Fee.zero xs ~f:(fun acc x ->
               match Fee.add acc (f x) with
               | None ->
                   return (Or_error.error_string "Fee overflow")
               | Some res ->
                   res)))

  let working_stack pending_coinbase_collection ~is_new_stack =
    to_staged_ledger_or_error
      (Pending_coinbase.latest_stack pending_coinbase_collection ~is_new_stack)

  let push_coinbase current_stack (t : Transaction.t) =
    match t with
    | Coinbase c ->
        Pending_coinbase.Stack.push_coinbase c current_stack
    | _ ->
        current_stack

  let push_state current_stack state_body_hash =
    Pending_coinbase.Stack.push_state state_body_hash current_stack

  module Stack_state_with_init_stack = struct
    type t =
      { pc : Transaction_snark.Pending_coinbase_stack_state.t
      ; init_stack : Pending_coinbase.Stack.t
      }
  end

  let coinbase_amount ~supercharge_coinbase
      ~(constraint_constants : Genesis_constants.Constraint_constants.t) =
    if supercharge_coinbase then
      Currency.Amount.scale constraint_constants.coinbase_amount
        constraint_constants.supercharged_coinbase_factor
    else Some constraint_constants.coinbase_amount

  let _coinbase_amount_or_error ~supercharge_coinbase
      ~(constraint_constants : Genesis_constants.Constraint_constants.t) =
    if supercharge_coinbase then
      Option.value_map
        ~default:
          (Error
             (Pre_diff_info.Error.Coinbase_error
                (sprintf
                   !"Overflow when calculating coinbase amount: Supercharged \
                     coinbase factor (%d) x coinbase amount (%{sexp: \
                     Currency.Amount.t})"
                   constraint_constants.supercharged_coinbase_factor
                   constraint_constants.coinbase_amount)))
        (coinbase_amount ~supercharge_coinbase ~constraint_constants)
        ~f:(fun x -> Ok x)
    else Ok constraint_constants.coinbase_amount

  let apply_transaction_and_get_statement ~constraint_constants ledger
      (pending_coinbase_stack_state : Stack_state_with_init_stack.t) s
      txn_state_view =
    let open Result.Let_syntax in
    let%bind fee_excess = Transaction.fee_excess s |> to_staged_ledger_or_error
    and supply_increase =
      Transaction.supply_increase s |> to_staged_ledger_or_error
    in
    let source =
      Ledger.merkle_root ledger |> Frozen_ledger_hash.of_ledger_hash
    in
    let next_available_token_before = Ledger.next_available_token ledger in
    let pending_coinbase_target =
      push_coinbase pending_coinbase_stack_state.pc.target s
    in
    let new_init_stack =
      push_coinbase pending_coinbase_stack_state.init_stack s
    in
    let%map applied_txn =
      Ledger.apply_transaction ~constraint_constants ~txn_state_view ledger s
      |> to_staged_ledger_or_error
    in
    let next_available_token_after = Ledger.next_available_token ledger in
    ( applied_txn
    , { Transaction_snark.Statement.source =
          { ledger = source
          ; next_available_token = next_available_token_before
          ; pending_coinbase_stack = pending_coinbase_stack_state.pc.source
          ; local_state = Mina_state.Local_state.empty
          }
      ; target =
          { ledger =
              Ledger.merkle_root ledger |> Frozen_ledger_hash.of_ledger_hash
          ; next_available_token = next_available_token_after
          ; pending_coinbase_stack = pending_coinbase_target
          ; local_state = Mina_state.Local_state.empty
          }
      ; fee_excess
      ; supply_increase
      ; sok_digest = ()
      }
    , { Stack_state_with_init_stack.pc =
          { source = pending_coinbase_target; target = pending_coinbase_target }
      ; init_stack = new_init_stack
      } )

  let apply_transaction_and_get_witness ~constraint_constants ledger
      pending_coinbase_stack_state s status txn_state_view state_and_body_hash =
    let account_ids : Transaction.t -> _ = function
      | Fee_transfer t ->
          Fee_transfer.receivers t
      | Command t ->
          let t = (t :> User_command.t) in
          let next_available_token = Ledger.next_available_token ledger in
          User_command.accounts_accessed ~next_available_token t
      | Coinbase c ->
          let ft_receivers =
            Option.map ~f:Coinbase.Fee_transfer.receiver c.fee_transfer
            |> Option.to_list
          in
          Account_id.create c.receiver Token_id.default :: ft_receivers
    in
    let ledger_witness =
      measure "sparse ledger" (fun () ->
          Sparse_ledger.of_ledger_subset_exn ledger (account_ids s))
    in
    let r =
      measure "apply+stmt" (fun () ->
          apply_transaction_and_get_statement ~constraint_constants ledger
            pending_coinbase_stack_state s txn_state_view)
    in
    let open Result.Let_syntax in
    let%bind applied_txn, statement, updated_pending_coinbase_stack_state = r in
    let%map () =
      match status with
      | None ->
          return ()
      | Some status ->
          (* Validate that command status matches. *)
          let got_status =
            Ledger.Transaction_applied.user_command_status applied_txn
          in
          if Transaction_status.equal status got_status then return ()
          else
            Result.fail
              (Staged_ledger_error.Mismatched_statuses
                 ({ With_status.data = s; status }, got_status))
    in
    ( { Scan_state.Transaction_with_witness.transaction_with_info = applied_txn
      ; state_hash = state_and_body_hash
      ; state_view = txn_state_view
      ; ledger_witness
      ; init_stack = Base pending_coinbase_stack_state.init_stack
      ; statement
      }
    , updated_pending_coinbase_stack_state )

  let rec partition size = function
    | [] ->
        []
    | ls ->
        let sub, rest = List.split_n ls size in
        sub :: partition size rest

  let update_ledger_and_get_statements ~constraint_constants ledger
      current_stack ts current_state_view state_and_body_hash =
    let open Deferred.Result.Let_syntax in
    let current_stack_with_state =
      push_state current_stack (snd state_and_body_hash)
    in
    let%map res_rev, pending_coinbase_stack_state =
      let pending_coinbase_stack_state : Stack_state_with_init_stack.t =
        { pc = { source = current_stack; target = current_stack_with_state }
        ; init_stack = current_stack
        }
      in
      let exception Exit of Staged_ledger_error.t in
      Async.try_with ~extract_exn:true (fun () ->
          let tss = partition 10 ts in
          Deferred.List.fold tss ~init:([], pending_coinbase_stack_state)
            ~f:(fun (acc, pending_coinbase_stack_state) ts ->
              let open Deferred.Let_syntax in
              let%map () = Async.Scheduler.yield () in
              List.fold ts ~init:(acc, pending_coinbase_stack_state)
                ~f:(fun (acc, pending_coinbase_stack_state) t ->
                  ( match
                      List.find (Transaction.public_keys t.With_status.data)
                        ~f:(fun pk ->
                          Option.is_none
                            (Signature_lib.Public_key.decompress pk))
                    with
                  | None ->
                      ()
                  | Some pk ->
                      raise (Exit (Invalid_public_key pk)) ) ;
                  match
                    apply_transaction_and_get_witness ~constraint_constants
                      ledger pending_coinbase_stack_state t.With_status.data
                      (Some t.status) current_state_view state_and_body_hash
                  with
                  | Ok (res, updated_pending_coinbase_stack_state) ->
                      (res :: acc, updated_pending_coinbase_stack_state)
                  | Error err ->
                      raise (Exit err))))
      |> Deferred.Result.map_error ~f:(function
           | Exit err ->
               err
           | exn ->
               raise exn)
    in
    (List.rev res_rev, pending_coinbase_stack_state.pc.target)

  let check_completed_works ~logger ~verifier scan_state
      (completed_works : Transaction_snark_work.t list) =
    let work_count = List.length completed_works in
    let job_pairs =
      Scan_state.k_work_pairs_for_new_diff scan_state ~k:work_count
    in
    let jmps =
      List.concat_map (List.zip_exn job_pairs completed_works)
        ~f:(fun (jobs, work) ->
          let message = Sok_message.create ~fee:work.fee ~prover:work.prover in
          One_or_two.(
            to_list
              (map (zip_exn jobs work.proofs) ~f:(fun (job, proof) ->
                   (job, message, proof)))))
    in
    verify jmps ~logger ~verifier

  (**The total fee excess caused by any diff should be zero. In the case where
     the slots are split into two partitions, total fee excess of the transactions
     to be enqueued on each of the partitions should be zero respectively *)
  let check_zero_fee_excess scan_state data =
    let partitions = Scan_state.partition_if_overflowing scan_state in
    let txns_from_data data =
      List.fold_right ~init:(Ok []) data
        ~f:(fun (d : Scan_state.Transaction_with_witness.t) acc ->
          let open Or_error.Let_syntax in
          let%map acc = acc in
          let t =
            d.transaction_with_info |> Ledger.Transaction_applied.transaction
          in
          t :: acc)
    in
    let total_fee_excess txns =
      List.fold_until txns ~init:Fee_excess.empty ~finish:Or_error.return
        ~f:(fun acc (txn : Transaction.t With_status.t) ->
          match
            let open Or_error.Let_syntax in
            let%bind fee_excess = Transaction.fee_excess txn.data in
            Fee_excess.combine acc fee_excess
          with
          | Ok fee_excess ->
              Continue fee_excess
          | Error _ as err ->
              Stop err)
      |> to_staged_ledger_or_error
    in
    let open Result.Let_syntax in
    let check data slots =
      let%bind txns = txns_from_data data |> to_staged_ledger_or_error in
      let%bind fee_excess = total_fee_excess txns in
      if Fee_excess.is_zero fee_excess then Ok ()
      else Error (Non_zero_fee_excess (slots, txns))
    in
    let%bind () = check (List.take data (fst partitions.first)) partitions in
    Option.value_map ~default:(Result.return ())
      ~f:(fun _ -> check (List.drop data (fst partitions.first)) partitions)
      partitions.second

  let time ~logger label f =
    let start = Core.Time.now () in
    let%map x = f () in
    [%log debug]
      ~metadata:
        [ ("time_elapsed", `Float Core.Time.(Span.to_ms @@ diff (now ()) start))
        ]
      "%s took $time_elapsed" label ;
    x

  let update_coinbase_stack_and_get_data ~constraint_constants scan_state ledger
      pending_coinbase_collection transactions current_state_view
      state_and_body_hash =
    let open Deferred.Result.Let_syntax in
    let coinbase_exists txns =
      List.fold_until ~init:false txns
        ~f:(fun acc t ->
          match t.With_status.data with
          | Transaction.Coinbase _ ->
              Stop true
          | _ ->
              Continue acc)
        ~finish:Fn.id
    in
    let { Scan_state.Space_partition.first = slots, _; second } =
      Scan_state.partition_if_overflowing scan_state
    in
    if List.length transactions > 0 then
      match second with
      | None ->
          (*Single partition:
            1.Check if a new stack is required and get a working stack [working_stack]
            2.create data for enqueuing onto the scan state *)
          let is_new_stack = Scan_state.next_on_new_tree scan_state in
          let%bind working_stack =
            working_stack pending_coinbase_collection ~is_new_stack
            |> Deferred.return
          in
          let%map data, updated_stack =
            update_ledger_and_get_statements ~constraint_constants ledger
              working_stack transactions current_state_view state_and_body_hash
          in
          ( is_new_stack
          , data
          , Pending_coinbase.Update.Action.Update_one
          , `Update_one updated_stack )
      | Some _ ->
          (*Two partition:
            Assumption: Only one of the partition will have coinbase transaction(s)in it.
            1. Get the latest stack for coinbase in the first set of transactions
            2. get the first set of scan_state data[data1]
            3. get a new stack for the second partion because the second set of transactions would start from the begining of the next tree in the scan_state
            4. Initialize the new stack with the state from the first stack
            5. get the second set of scan_state data[data2]*)
          let txns_for_partition1 = List.take transactions slots in
          let coinbase_in_first_partition =
            coinbase_exists txns_for_partition1
          in
          let%bind working_stack1 =
            working_stack pending_coinbase_collection ~is_new_stack:false
            |> Deferred.return
          in
          let%bind data1, updated_stack1 =
            update_ledger_and_get_statements ~constraint_constants ledger
              working_stack1 txns_for_partition1 current_state_view
              state_and_body_hash
          in
          let txns_for_partition2 = List.drop transactions slots in
          (*Push the new state to the state_stack from the previous block even in the second stack*)
          let working_stack2 =
            Pending_coinbase.Stack.create_with working_stack1
          in
          let%map data2, updated_stack2 =
            update_ledger_and_get_statements ~constraint_constants ledger
              working_stack2 txns_for_partition2 current_state_view
              state_and_body_hash
          in
          let second_has_data = List.length txns_for_partition2 > 0 in
          let pending_coinbase_action, stack_update =
            match (coinbase_in_first_partition, second_has_data) with
            | true, true ->
                ( Pending_coinbase.Update.Action.Update_two_coinbase_in_first
                , `Update_two (updated_stack1, updated_stack2) )
            (*updated_stack2 does not have coinbase and but has the state from the previous stack*)
            | true, false ->
                (*updated_stack1 has some new coinbase but parition 2 has no
                  data and so we have only one stack to update*)
                (Update_one, `Update_one updated_stack1)
            | false, true ->
                (*updated_stack1 just has the new state. [updated stack2] might have coinbase, definitely has some
                  data and therefore will have a non-dummy state.*)
                ( Update_two_coinbase_in_second
                , `Update_two (updated_stack1, updated_stack2) )
            | false, false ->
                (* a diff consists of only non-coinbase transactions. This is currently not possible because a diff will have a coinbase at the very least, so don't update anything?*)
                (Update_none, `Update_none)
          in
          (false, data1 @ data2, pending_coinbase_action, stack_update)
    else
      Deferred.return
        (Ok (false, [], Pending_coinbase.Update.Action.Update_none, `Update_none))

  (*update the pending_coinbase tree with the updated/new stack and delete the oldest stack if a proof was emitted*)
  let update_pending_coinbase_collection ~depth pending_coinbase_collection
      stack_update ~is_new_stack ~ledger_proof =
    let open Result.Let_syntax in
    (*Deleting oldest stack if proof emitted*)
    let%bind pending_coinbase_collection_updated1 =
      match ledger_proof with
      | Some (proof, _) ->
          let%bind oldest_stack, pending_coinbase_collection_updated1 =
            Pending_coinbase.remove_coinbase_stack ~depth
              pending_coinbase_collection
            |> to_staged_ledger_or_error
          in
          let ledger_proof_stack =
            (Ledger_proof.statement proof).target.pending_coinbase_stack
          in
          let%map () =
            if Pending_coinbase.Stack.equal oldest_stack ledger_proof_stack then
              Ok ()
            else
              Error
                (Staged_ledger_error.Unexpected
                   (Error.of_string
                      "Pending coinbase stack of the ledger proof did not \
                       match the oldest stack in the pending coinbase tree."))
          in
          pending_coinbase_collection_updated1
      | None ->
          Ok pending_coinbase_collection
    in
    (*updating the latest stack and/or adding a new one*)
    match stack_update with
    | `Update_none ->
        Ok pending_coinbase_collection_updated1
    | `Update_one stack1 ->
        Pending_coinbase.update_coinbase_stack ~depth
          pending_coinbase_collection_updated1 stack1 ~is_new_stack
        |> to_staged_ledger_or_error
    | `Update_two (stack1, stack2) ->
        (*The case when some of the transactions go into the old tree and remaining on to the new tree*)
        let%bind update1 =
          Pending_coinbase.update_coinbase_stack ~depth
            pending_coinbase_collection_updated1 stack1 ~is_new_stack:false
          |> to_staged_ledger_or_error
        in
        Pending_coinbase.update_coinbase_stack ~depth update1 stack2
          ~is_new_stack:true
        |> to_staged_ledger_or_error

  let coinbase_for_blockchain_snark = function
    | [] ->
        Ok Currency.Amount.zero
    | [ amount ] ->
        Ok amount
    | [ amount1; _ ] ->
        Ok amount1
    | _ ->
        Error
          (Staged_ledger_error.Pre_diff
             (Pre_diff_info.Error.Coinbase_error "More than two coinbase parts"))

  let apply_diff ?(skip_verification = false) ~logger ~constraint_constants t
      pre_diff_info ~current_state_view ~state_and_body_hash ~log_prefix =
    let open Deferred.Result.Let_syntax in
    let max_throughput =
      Int.pow 2 t.constraint_constants.transaction_capacity_log_2
    in
    let spots_available, proofs_waiting =
      let jobs = Scan_state.all_work_statements_exn t.scan_state in
      ( Int.min (Scan_state.free_space t.scan_state) max_throughput
      , List.length jobs )
    in
    let new_mask = Ledger.Mask.create ~depth:(Ledger.depth t.ledger) () in
    let new_ledger = Ledger.register_mask t.ledger new_mask in
    let transactions, works, commands_count, coinbases = pre_diff_info in
    let%bind is_new_stack, data, stack_update_in_snark, stack_update =
      time ~logger "update_coinbase_stack_start_time" (fun () ->
          update_coinbase_stack_and_get_data ~constraint_constants t.scan_state
            new_ledger t.pending_coinbase_collection transactions
            current_state_view state_and_body_hash)
    in
    let slots = List.length data in
    let work_count = List.length works in
    let required_pairs = Scan_state.work_statements_for_new_diff t.scan_state in
    let%bind () =
      time ~logger "sufficient work check" (fun () ->
          let required = List.length required_pairs in
          if
            work_count < required
            && List.length data
               > Scan_state.free_space t.scan_state - required + work_count
          then
            Deferred.return
              (Error
                 (Staged_ledger_error.Insufficient_work
                    (sprintf
                       !"Insufficient number of transaction snark work (slots \
                         occupying: %d)  required %d, got %d"
                       slots required work_count)))
          else Deferred.return (Ok ()))
    in
    let%bind () = Deferred.return (check_zero_fee_excess t.scan_state data) in
    let%bind res_opt, scan_state' =
      time ~logger "fill_work_and_enqueue_transactions" (fun () ->
          let r =
            Scan_state.fill_work_and_enqueue_transactions t.scan_state data
              works
          in
          Or_error.iter_error r ~f:(fun e ->
              let data_json =
                `List
                  (List.map data
                     ~f:(fun
                          { Scan_state.Transaction_with_witness.statement; _ }
                        -> Transaction_snark.Statement.to_yojson statement))
              in
              [%log error]
                ~metadata:
                  [ ( "scan_state"
                    , `String (Scan_state.snark_job_list_json t.scan_state) )
                  ; ("data", data_json)
                  ; ("error", Error_json.error_to_yojson e)
                  ; ("prefix", `String log_prefix)
                  ]
                !"$prefix: Unexpected error when applying diff data $data to \
                  the scan_state $scan_state: $error") ;
          Deferred.return (to_staged_ledger_or_error r))
    in
    let%bind updated_pending_coinbase_collection' =
      time ~logger "update_pending_coinbase_collection" (fun () ->
          update_pending_coinbase_collection
            ~depth:t.constraint_constants.pending_coinbase_depth
            t.pending_coinbase_collection stack_update ~is_new_stack
            ~ledger_proof:res_opt
          |> Deferred.return)
    in
    let%bind coinbase_amount =
      coinbase_for_blockchain_snark coinbases |> Deferred.return
    in
    let%map () =
      time ~logger
        (sprintf "verify_scan_state_after_apply (skip=%b)" skip_verification)
        (fun () ->
          if skip_verification then Deferred.return (Ok ())
          else
            Deferred.(
              verify_scan_state_after_apply ~constraint_constants
                ~next_available_token:(Ledger.next_available_token new_ledger)
                (Frozen_ledger_hash.of_ledger_hash
                   (Ledger.merkle_root new_ledger))
                ~pending_coinbase_collection:
                  updated_pending_coinbase_collection' scan_state'
              >>| to_staged_ledger_or_error))
    in
    [%log debug]
      ~metadata:
        [ ("user_command_count", `Int commands_count)
        ; ("coinbase_count", `Int (List.length coinbases))
        ; ("spots_available", `Int spots_available)
        ; ("proof_bundles_waiting", `Int proofs_waiting)
        ; ("work_count", `Int (List.length works))
        ; ("prefix", `String log_prefix)
        ]
      "$prefix: apply_diff block info: No of transactions \
       included:$user_command_count\n\
      \      Coinbase parts:$coinbase_count Spots\n\
      \      available:$spots_available Pending work in the \
       scan-state:$proof_bundles_waiting Work included:$work_count" ;
    let new_staged_ledger =
      { scan_state = scan_state'
      ; ledger = new_ledger
      ; constraint_constants = t.constraint_constants
      ; pending_coinbase_collection = updated_pending_coinbase_collection'
      }
    in
    ( `Hash_after_applying (hash new_staged_ledger)
    , `Ledger_proof res_opt
    , `Staged_ledger new_staged_ledger
    , `Pending_coinbase_update
        ( is_new_stack
        , { Pending_coinbase.Update.Poly.action = stack_update_in_snark
          ; coinbase_amount
          } ) )

  let update_metrics (t : t) (witness : Staged_ledger_diff.t) =
    let open Or_error.Let_syntax in
    let commands = Staged_ledger_diff.commands witness in
    let work = Staged_ledger_diff.completed_works witness in
    let%bind total_txn_fee =
      sum_fees commands ~f:(fun { data = cmd; _ } -> User_command.fee_exn cmd)
    in
    let%bind total_snark_fee = sum_fees work ~f:Transaction_snark_work.fee in
    let%bind () = Scan_state.update_metrics t.scan_state in
    Or_error.try_with (fun () ->
        let open Mina_metrics in
        Gauge.set Scan_state_metrics.snark_fee_per_block
          (Int.to_float @@ Fee.to_int total_snark_fee) ;
        Gauge.set Scan_state_metrics.transaction_fees_per_block
          (Int.to_float @@ Fee.to_int total_txn_fee) ;
        Gauge.set Scan_state_metrics.purchased_snark_work_per_block
          (Float.of_int @@ List.length work) ;
        Gauge.set Scan_state_metrics.snark_work_required
          (Float.of_int
             (List.length (Scan_state.all_work_statements_exn t.scan_state))))

  let forget_prediff_info ((a : Transaction.Valid.t With_status.t list), b, c, d)
      =
    ((a :> Transaction.t With_status.t list), b, c, d)

  [%%if feature_snapps]

  let check_commands ledger ~verifier (cs : User_command.t list) =
    let cs =
      List.map cs
        ~f:
          (let open Ledger in
          User_command.to_verifiable ~ledger ~get ~location_of_account)
    in
    let open Deferred.Or_error.Let_syntax in
    let%map xs = Verifier.verify_commands verifier cs in
    Result.all
      (List.map xs ~f:(function
        | `Valid x ->
            Ok x
        | `Invalid ->
            Error
              (Verifier.Failure.Verification_failed
                 (Error.of_string "verification failed on command"))
        | `Valid_assuming _ ->
            Error
              (Verifier.Failure.Verification_failed
                 (Error.of_string "batch verification failed"))))

  [%%else]

  (* imeckler: added this version because the call to the verifier was
     causing super catchup to proceed more slowly than it could have otherwise.

     The reason is as follows: catchup would have, say 100 blocks in the "to verify"
     queue and 20 in the "already verified, to apply" queue. Those 20 would be
     processed very slowly because each one would have to call the verifier, which
     the other queue was trying to call as well. *)
  let check_commands _ledger ~verifier:_ (cs : User_command.t list) :
      (User_command.Valid.t list, _) result Deferred.Or_error.t =
    Result.all
      (List.map cs ~f:(function
        | Snapp_command _ ->
            Error
              (Verifier.Failure.Verification_failed
                 (Error.of_string "check_commands: snapp commands disabled"))
        | Signed_command c -> (
            match Signed_command.check c with
            | Some c ->
                Ok (User_command.Signed_command c)
            | None ->
                Error
                  (Verifier.Failure.Verification_failed
                     (Error.of_string "signature failed to verify")) )))
    |> Deferred.Or_error.return

  [%%endif]

  let apply ?skip_verification ~constraint_constants t
      (witness : Staged_ledger_diff.t) ~logger ~verifier ~current_state_view
      ~state_and_body_hash ~coinbase_receiver ~supercharge_coinbase =
    let open Deferred.Result.Let_syntax in
    let work = Staged_ledger_diff.completed_works witness in
    let%bind () =
      time ~logger "check_completed_works" (fun () ->
          match skip_verification with
          | Some `All | Some `Proofs ->
              return ()
          | None ->
              check_completed_works ~logger ~verifier t.scan_state work)
    in
    let%bind prediff =
      Pre_diff_info.get witness ~constraint_constants ~coinbase_receiver
        ~supercharge_coinbase
        ~check:(check_commands t.ledger ~verifier)
      |> Deferred.map
           ~f:
             (Result.map_error ~f:(fun error ->
                  Staged_ledger_error.Pre_diff error))
    in
    let apply_diff_start_time = Core.Time.now () in
    let%map ((_, _, `Staged_ledger new_staged_ledger, _) as res) =
      apply_diff
        ~skip_verification:
          ([%equal: [ `All | `Proofs ] option] skip_verification (Some `All))
        ~constraint_constants t
        (forget_prediff_info prediff)
        ~logger ~current_state_view ~state_and_body_hash
        ~log_prefix:"apply_diff"
    in
    [%log debug]
      ~metadata:
        [ ( "time_elapsed"
          , `Float Core.Time.(Span.to_ms @@ diff (now ()) apply_diff_start_time)
          )
        ]
      "Staged_ledger.apply_diff take $time_elapsed" ;
    let () =
      Or_error.iter_error (update_metrics new_staged_ledger witness)
        ~f:(fun e ->
          [%log error]
            ~metadata:[ ("error", Error_json.error_to_yojson e) ]
            !"Error updating metrics after applying staged_ledger diff: $error")
    in
    res

  let apply_diff_unchecked ~constraint_constants t
      (sl_diff : Staged_ledger_diff.With_valid_signatures_and_proofs.t) ~logger
      ~current_state_view ~state_and_body_hash ~coinbase_receiver
      ~supercharge_coinbase =
    let open Deferred.Result.Let_syntax in
    let%bind prediff =
      Result.map_error ~f:(fun error -> Staged_ledger_error.Pre_diff error)
      @@ Pre_diff_info.get_unchecked ~constraint_constants ~coinbase_receiver
           ~supercharge_coinbase sl_diff
      |> Deferred.return
    in
    apply_diff t
      (forget_prediff_info prediff)
      ~constraint_constants ~logger ~current_state_view ~state_and_body_hash
      ~log_prefix:"apply_diff_unchecked"

  module Resources = struct
    module Discarded = struct
      type t =
        { commands_rev : User_command.Valid.t With_status.t Sequence.t
        ; completed_work : Transaction_snark_work.Checked.t Sequence.t
        }
      [@@deriving sexp_of]

      let add_user_command t uc =
        { t with
          commands_rev = Sequence.append t.commands_rev (Sequence.singleton uc)
        }

      let add_completed_work t cw =
        { t with
          completed_work =
            Sequence.append (Sequence.singleton cw) t.completed_work
        }
    end

    type t =
      { max_space : int (*max space available currently*)
      ; max_jobs : int
            (*Required amount of work for max_space that can be purchased*)
      ; commands_rev : User_command.Valid.t With_status.t Sequence.t
      ; completed_work_rev : Transaction_snark_work.Checked.t Sequence.t
      ; fee_transfers : Fee.t Public_key.Compressed.Map.t
      ; add_coinbase : bool
      ; coinbase : Coinbase.Fee_transfer.t Staged_ledger_diff.At_most_two.t
      ; supercharge_coinbase : bool
      ; receiver_pk : Public_key.Compressed.t
      ; budget : Fee.t Or_error.t
      ; discarded : Discarded.t
      ; is_coinbase_receiver_new : bool
      ; logger : (Logger.t[@sexp.opaque])
      }
    [@@deriving sexp_of]

    let coinbase_ft (cw : Transaction_snark_work.t) =
      (* Here we could not add the fee transfer if the prover=receiver_pk but
         retaining it to preserve that information in the
         staged_ledger_diff. It will be checked in apply_diff before adding*)
      Option.some_if
        Fee.(cw.fee > Fee.zero)
        (Coinbase.Fee_transfer.create ~receiver_pk:cw.prover ~fee:cw.fee)

    let cheapest_two_work (works : Transaction_snark_work.Checked.t Sequence.t)
        =
      Sequence.fold works ~init:(None, None) ~f:(fun (w1, w2) w ->
          match (w1, w2) with
          | None, _ ->
              (Some w, None)
          | Some x, None ->
              if Currency.Fee.compare w.fee x.fee < 0 then (Some w, w1)
              else (w1, Some w)
          | Some x, Some y ->
              if Currency.Fee.compare w.fee x.fee < 0 then (Some w, w1)
              else if Currency.Fee.compare w.fee y.fee < 0 then (w1, Some w)
              else (w1, w2))

    let coinbase_work
        ~(constraint_constants : Genesis_constants.Constraint_constants.t)
        ?(is_two = false) (works : Transaction_snark_work.Checked.t Sequence.t)
        ~is_coinbase_receiver_new ~supercharge_coinbase =
      let open Option.Let_syntax in
      let min1, min2 = cheapest_two_work works in
      let diff ws ws' =
        Sequence.filter ws ~f:(fun w ->
            Sequence.mem ws'
              (Transaction_snark_work.statement w)
              ~equal:Transaction_snark_work.Statement.equal
            |> not)
      in
      let%bind coinbase_amount =
        coinbase_amount ~supercharge_coinbase ~constraint_constants
      in
      let%bind budget =
        (*if the coinbase receiver is new then the account creation fee will be deducted from the reward*)
        if is_coinbase_receiver_new then
          Currency.Amount.(
            sub coinbase_amount
              (of_fee constraint_constants.account_creation_fee))
        else Some coinbase_amount
      in
      let stmt = Transaction_snark_work.statement in
      if is_two then
        match (min1, min2) with
        | None, _ ->
            None
        | Some w, None ->
            if Amount.(of_fee w.fee <= budget) then
              let cb =
                Staged_ledger_diff.At_most_two.Two
                  (Option.map (coinbase_ft w) ~f:(fun ft -> (ft, None)))
              in
              Some (cb, diff works (Sequence.of_list [ stmt w ]))
            else
              let cb = Staged_ledger_diff.At_most_two.Two None in
              Some (cb, works)
        | Some w1, Some w2 ->
            let%map sum = Fee.add w1.fee w2.fee in
            if Amount.(of_fee sum <= budget) then
              let cb =
                Staged_ledger_diff.At_most_two.Two
                  (Option.map (coinbase_ft w1) ~f:(fun ft ->
                       (ft, coinbase_ft w2)))
                (*Why add work without checking if work constraints are
                  satisfied? If we reach here then it means that we are trying to
                  fill the last two slots of the tree with coinbase trnasactions
                  and if there's any work in [works] then that has to be included,
                  either in the coinbase or as fee transfers that gets paid by
                  the transaction fees. So having it as coinbase ft will at least
                  reduce the slots occupied by fee transfers*)
              in
              (cb, diff works (Sequence.of_list [ stmt w1; stmt w2 ]))
            else if Amount.(of_fee w1.fee <= coinbase_amount) then
              let cb =
                Staged_ledger_diff.At_most_two.Two
                  (Option.map (coinbase_ft w1) ~f:(fun ft -> (ft, None)))
              in
              (cb, diff works (Sequence.of_list [ stmt w1 ]))
            else
              let cb = Staged_ledger_diff.At_most_two.Two None in
              (cb, works)
      else
        Option.map min1 ~f:(fun w ->
            if Amount.(of_fee w.fee <= budget) then
              let cb = Staged_ledger_diff.At_most_two.One (coinbase_ft w) in
              (cb, diff works (Sequence.of_list [ stmt w ]))
            else
              let cb = Staged_ledger_diff.At_most_two.One None in
              (cb, works))

    let init_coinbase_and_fee_transfers ~constraint_constants cw_seq
        ~add_coinbase ~job_count ~slots ~is_coinbase_receiver_new
        ~supercharge_coinbase =
      let cw_unchecked work =
        Sequence.map work ~f:Transaction_snark_work.forget
      in
      let coinbase, rem_cw =
        match
          ( add_coinbase
          , coinbase_work ~constraint_constants cw_seq ~is_coinbase_receiver_new
              ~supercharge_coinbase )
        with
        | true, Some (ft, rem_cw) ->
            (ft, rem_cw)
        | true, None ->
            (*Coinbase could not be added because work-fees > coinbase-amount*)
            if job_count = 0 || slots - job_count >= 1 then
              (*Either no jobs are required or there is a free slot that can be filled without having to include any work*)
              (One None, cw_seq)
            else (Zero, cw_seq)
        | _ ->
            (Zero, cw_seq)
      in
      let rem_cw = cw_unchecked rem_cw in
      let singles =
        Sequence.filter_map rem_cw
          ~f:(fun { Transaction_snark_work.fee; prover; _ } ->
            if Fee.equal fee Fee.zero then None else Some (prover, fee))
        |> Sequence.to_list_rev
      in
      (coinbase, singles)

    let init ~constraint_constants
        (uc_seq : User_command.Valid.t With_status.t Sequence.t)
        (cw_seq : Transaction_snark_work.Checked.t Sequence.t)
        (slots, job_count) ~receiver_pk ~add_coinbase ~supercharge_coinbase
        logger ~is_coinbase_receiver_new =
      let seq_rev seq =
        let rec go seq rev_seq =
          match Sequence.next seq with
          | Some (w, rem_seq) ->
              go rem_seq (Sequence.append (Sequence.singleton w) rev_seq)
          | None ->
              rev_seq
        in
        go seq Sequence.empty
      in
      let coinbase, singles =
        init_coinbase_and_fee_transfers ~constraint_constants cw_seq
          ~add_coinbase ~job_count ~slots ~is_coinbase_receiver_new
          ~supercharge_coinbase
      in
      let fee_transfers =
        Public_key.Compressed.Map.of_alist_reduce singles ~f:(fun f1 f2 ->
            Option.value_exn (Fee.add f1 f2))
      in
      let budget =
        Or_error.map2
          (sum_fees (Sequence.to_list uc_seq) ~f:(fun t ->
               User_command.fee_exn (t.data :> User_command.t)))
          (sum_fees
             (List.filter
                ~f:(fun (k, _) ->
                  not (Public_key.Compressed.equal k receiver_pk))
                singles)
             ~f:snd)
          ~f:(fun r c -> option "budget did not suffice" (Fee.sub r c))
        |> Or_error.join
      in
      let discarded =
        { Discarded.completed_work = Sequence.empty
        ; commands_rev = Sequence.empty
        }
      in
      { max_space = slots
      ; max_jobs = job_count
      ; commands_rev =
          uc_seq
          (*Completed work in reverse order for faster removal of proofs if budget doesn't suffice*)
      ; completed_work_rev = seq_rev cw_seq
      ; fee_transfers
      ; add_coinbase
      ; supercharge_coinbase
      ; receiver_pk
      ; coinbase
      ; budget
      ; discarded
      ; is_coinbase_receiver_new
      ; logger
      }

    let reselect_coinbase_work ~constraint_constants t =
      let cw_unchecked work =
        Sequence.map work ~f:Transaction_snark_work.forget
      in
      let coinbase, rem_cw =
        match t.coinbase with
        | Staged_ledger_diff.At_most_two.Zero ->
            (t.coinbase, t.completed_work_rev)
        | One _ -> (
            match
              coinbase_work ~constraint_constants t.completed_work_rev
                ~is_coinbase_receiver_new:t.is_coinbase_receiver_new
                ~supercharge_coinbase:t.supercharge_coinbase
            with
            | None ->
                (One None, t.completed_work_rev)
            | Some (ft, rem_cw) ->
                (ft, rem_cw) )
        | Two _ -> (
            match
              coinbase_work ~constraint_constants t.completed_work_rev
                ~is_two:true
                ~is_coinbase_receiver_new:t.is_coinbase_receiver_new
                ~supercharge_coinbase:t.supercharge_coinbase
            with
            | None ->
                (Two None, t.completed_work_rev)
                (*Check for work constraint will be done in [check_constraints_and_update]*)
            | Some (fts', rem_cw) ->
                (fts', rem_cw) )
      in
      let rem_cw = cw_unchecked rem_cw in
      let singles =
        Sequence.filter_map rem_cw
          ~f:(fun { Transaction_snark_work.fee; prover; _ } ->
            if Fee.equal fee Fee.zero then None else Some (prover, fee))
        |> Sequence.to_list_rev
      in
      let fee_transfers =
        Public_key.Compressed.Map.of_alist_reduce singles ~f:(fun f1 f2 ->
            Option.value_exn (Fee.add f1 f2))
      in
      { t with coinbase; fee_transfers }

    let rebudget t =
      (*get the correct coinbase and calculate the fee transfers*)
      let open Or_error.Let_syntax in
      let payment_fees =
        sum_fees (Sequence.to_list t.commands_rev) ~f:(fun t ->
            User_command.fee_exn (t.data :> User_command.t))
      in
      let prover_fee_others =
        Public_key.Compressed.Map.fold t.fee_transfers ~init:(Ok Fee.zero)
          ~f:(fun ~key ~data fees ->
            let%bind others = fees in
            if Public_key.Compressed.equal t.receiver_pk key then Ok others
            else option "Fee overflow" (Fee.add others data))
      in
      let revenue = payment_fees in
      let cost = prover_fee_others in
      Or_error.map2 revenue cost ~f:(fun r c ->
          option "budget did not suffice" (Fee.sub r c))
      |> Or_error.join

    let budget_sufficient t =
      match t.budget with Ok _ -> true | Error _ -> false

    let coinbase_added t =
      match t.coinbase with
      | Staged_ledger_diff.At_most_two.Zero ->
          0
      | One _ ->
          1
      | Two _ ->
          2

    let slots_occupied t =
      let fee_for_self =
        match t.budget with
        | Error _ ->
            0
        | Ok b ->
            if Fee.(b > Fee.zero) then 1 else 0
      in
      let other_provers =
        Public_key.Compressed.Map.filter_keys t.fee_transfers
          ~f:(Fn.compose not (Public_key.Compressed.equal t.receiver_pk))
      in
      let total_fee_transfer_pks =
        Public_key.Compressed.Map.length other_provers + fee_for_self
      in
      Sequence.length t.commands_rev
      + ((total_fee_transfer_pks + 1) / 2)
      + coinbase_added t

    let space_available res =
      let slots = slots_occupied res in
      res.max_space > slots

    let work_done t =
      let no_of_proof_bundles = Sequence.length t.completed_work_rev in
      let slots = slots_occupied t in
      (*If more jobs were added in the previous diff then ( t.max_space-t.max_jobs) slots can go for free in this diff*)
      no_of_proof_bundles = t.max_jobs || slots <= t.max_space - t.max_jobs

    let space_constraint_satisfied t =
      let occupied = slots_occupied t in
      occupied <= t.max_space

    let work_constraint_satisfied (t : t) =
      (*Are we doing all the work available? *)
      let all_proofs = work_done t in
      (*enough work*)
      let slots = slots_occupied t in
      let cw_count = Sequence.length t.completed_work_rev in
      let enough_work = cw_count >= slots in
      (*if there are no transactions then don't need any proofs*)
      all_proofs || slots = 0 || enough_work

    let available_space t = t.max_space - slots_occupied t

    let discard_last_work ~constraint_constants t =
      match Sequence.next t.completed_work_rev with
      | None ->
          (t, None)
      | Some (w, rem_seq) ->
          let to_be_discarded = Transaction_snark_work.forget w in
          let discarded = Discarded.add_completed_work t.discarded w in
          let new_t =
            reselect_coinbase_work ~constraint_constants
              { t with completed_work_rev = rem_seq; discarded }
          in
          let budget =
            match t.budget with
            | Ok b ->
                option "Currency overflow" (Fee.add b to_be_discarded.fee)
            | _ ->
                rebudget new_t
          in
          ({ new_t with budget }, Some w)

    let discard_user_command t =
      let decr_coinbase t =
        (*When discarding coinbase's fee transfer, add the fee transfer to the fee_transfers map so that budget checks can be done *)
        let update_fee_transfers t (ft : Coinbase.Fee_transfer.t) coinbase =
          let updated_fee_transfers =
            Public_key.Compressed.Map.update t.fee_transfers ft.receiver_pk
              ~f:(fun _ -> ft.fee)
          in
          let new_t =
            { t with coinbase; fee_transfers = updated_fee_transfers }
          in
          let updated_budget = rebudget new_t in
          { new_t with budget = updated_budget }
        in
        match t.coinbase with
        | Staged_ledger_diff.At_most_two.Zero ->
            t
        | One None ->
            { t with coinbase = Staged_ledger_diff.At_most_two.Zero }
        | Two None ->
            { t with coinbase = One None }
        | Two (Some (ft, None)) ->
            { t with coinbase = One (Some ft) }
        | One (Some ft) ->
            update_fee_transfers t ft Zero
        | Two (Some (ft1, Some ft2)) ->
            update_fee_transfers t ft2 (One (Some ft1))
      in
      match Sequence.next t.commands_rev with
      | None ->
          (* If we have reached here then it means we couldn't afford a slot for coinbase as well *)
          (decr_coinbase t, None)
      | Some (uc, rem_seq) ->
          let discarded = Discarded.add_user_command t.discarded uc in
          let new_t = { t with commands_rev = rem_seq; discarded } in
          let budget =
            match t.budget with
            | Ok b ->
                option "Fee insufficient"
                  (Fee.sub b (User_command.fee_exn (uc.data :> User_command.t)))
            | _ ->
                rebudget new_t
          in
          ({ new_t with budget }, Some uc)

    let worked_more ~constraint_constants resources =
      (*Is the work constraint satisfied even after discarding a work bundle?
         We reach here after having more than enough work*)
      let more_work t =
        let slots = slots_occupied t in
        let cw_count = Sequence.length t.completed_work_rev in
        cw_count > 0 && cw_count >= slots
      in
      let r, _ = discard_last_work ~constraint_constants resources in
      more_work r

    let incr_coinbase_part_by ~constraint_constants t count =
      let open Or_error.Let_syntax in
      let incr = function
        | Staged_ledger_diff.At_most_two.Zero ->
            Ok (Staged_ledger_diff.At_most_two.One None)
        | One None ->
            Ok (Two None)
        | One (Some ft) ->
            Ok (Two (Some (ft, None)))
        | _ ->
            Or_error.error_string "Coinbase count cannot be more than two"
      in
      let by_one res =
        let res' =
          match Sequence.next res.discarded.completed_work with
          (*add one from the discarded list to [completed_work_rev] and then select a work from [completed_work_rev] except the one already used*)
          | Some (w, rem_work) ->
              let%map coinbase = incr res.coinbase in
              let res' =
                { res with
                  completed_work_rev =
                    Sequence.append (Sequence.singleton w)
                      res.completed_work_rev
                ; discarded = { res.discarded with completed_work = rem_work }
                ; coinbase
                }
              in
              reselect_coinbase_work ~constraint_constants res'
          | None ->
              let%bind coinbase = incr res.coinbase in
              let res = { res with coinbase } in
              if work_done res then Ok res
              else
                Or_error.error_string
                  "Could not increment coinbase transaction count because of \
                   insufficient work"
        in
        match res' with
        | Ok res'' ->
            res''
        | Error e ->
            [%log' error t.logger] "Error when increasing coinbase: $error"
              ~metadata:[ ("error", Error_json.error_to_yojson e) ] ;
            res
      in
      match count with `One -> by_one t | `Two -> by_one (by_one t)
  end

  let rec check_constraints_and_update ~constraint_constants
      (resources : Resources.t) log =
    if Resources.slots_occupied resources = 0 then (resources, log)
    else if Resources.work_constraint_satisfied resources then
      if
        (*There's enough work. Check if they satisfy other constraints*)
        Resources.budget_sufficient resources
      then
        if Resources.worked_more ~constraint_constants resources then
          (*There are too many fee_transfers(from the proofs) occupying the slots. discard one and check*)
          let resources', work_opt =
            Resources.discard_last_work ~constraint_constants resources
          in
          check_constraints_and_update ~constraint_constants resources'
            (Option.value_map work_opt ~default:log ~f:(fun work ->
                 Diff_creation_log.discard_completed_work `Extra_work work log))
        else if Resources.space_constraint_satisfied resources then
          (resources, log)
        else
          (*Well, there's no space; discard a user command *)
          let resources', uc_opt = Resources.discard_user_command resources in
          check_constraints_and_update ~constraint_constants resources'
            (Option.value_map uc_opt ~default:log ~f:(fun uc ->
                 Diff_creation_log.discard_command `No_space
                   (uc.data :> User_command.t)
                   log))
      else
        (* insufficient budget; reduce the cost*)
        let resources', work_opt =
          Resources.discard_last_work ~constraint_constants resources
        in
        check_constraints_and_update ~constraint_constants resources'
          (Option.value_map work_opt ~default:log ~f:(fun work ->
               Diff_creation_log.discard_completed_work `Insufficient_fees work
                 log))
    else
      (* There isn't enough work for the transactions. Discard a transaction and check again *)
      let resources', uc_opt = Resources.discard_user_command resources in
      check_constraints_and_update ~constraint_constants resources'
        (Option.value_map uc_opt ~default:log ~f:(fun uc ->
             Diff_creation_log.discard_command `No_work
               (uc.data :> User_command.t)
               log))

  let one_prediff ~constraint_constants cw_seq ts_seq ~receiver ~add_coinbase
      slot_job_count logger ~is_coinbase_receiver_new partition
      ~supercharge_coinbase =
    O1trace.measure "one_prediff" (fun () ->
        let init_resources =
          Resources.init ~constraint_constants ts_seq cw_seq slot_job_count
            ~receiver_pk:receiver ~add_coinbase logger ~is_coinbase_receiver_new
            ~supercharge_coinbase
        in
        let log =
          Diff_creation_log.init
            ~completed_work:init_resources.completed_work_rev
            ~commands:init_resources.commands_rev
            ~coinbase:init_resources.coinbase ~partition
            ~available_slots:(fst slot_job_count)
            ~required_work_count:(snd slot_job_count)
        in
        check_constraints_and_update ~constraint_constants init_resources log)

  let generate ~constraint_constants logger cw_seq ts_seq ~receiver
      ~is_coinbase_receiver_new ~supercharge_coinbase
      (partitions : Scan_state.Space_partition.t) =
    let pre_diff_with_one (res : Resources.t) :
        Staged_ledger_diff.With_valid_signatures_and_proofs
        .pre_diff_with_at_most_one_coinbase =
      O1trace.measure "pre_diff_with_one" (fun () ->
          let to_at_most_one = function
            | Staged_ledger_diff.At_most_two.Zero ->
                Staged_ledger_diff.At_most_one.Zero
            | One x ->
                One x
            | _ ->
                [%log error]
                  "Error creating staged ledger diff: Should have at most one \
                   coinbase in the second pre_diff" ;
                Zero
          in
          (* We have to reverse here because we only know they work in THIS order *)
          { Staged_ledger_diff.Pre_diff_one.commands =
              Sequence.to_list_rev res.commands_rev
          ; completed_works = Sequence.to_list_rev res.completed_work_rev
          ; coinbase = to_at_most_one res.coinbase
          ; internal_command_balances =
              (* These will get filled in by the caller. *) []
          })
    in
    let pre_diff_with_two (res : Resources.t) :
        Staged_ledger_diff.With_valid_signatures_and_proofs
        .pre_diff_with_at_most_two_coinbase =
      (* We have to reverse here because we only know they work in THIS order *)
      { commands = Sequence.to_list_rev res.commands_rev
      ; completed_works = Sequence.to_list_rev res.completed_work_rev
      ; coinbase = res.coinbase
      ; internal_command_balances =
          (* These will get filled in by the caller. *) []
      }
    in
    let end_log ((res : Resources.t), (log : Diff_creation_log.t)) =
      Diff_creation_log.end_log log ~completed_work:res.completed_work_rev
        ~commands:res.commands_rev ~coinbase:res.coinbase
    in
    let make_diff res1 = function
      | Some res2 ->
          ( (pre_diff_with_two (fst res1), Some (pre_diff_with_one (fst res2)))
          , List.map ~f:end_log [ res1; res2 ] )
      | None ->
          ((pre_diff_with_two (fst res1), None), [ end_log res1 ])
    in
    let has_no_commands (res : Resources.t) =
      Sequence.length res.commands_rev = 0
    in
    let second_pre_diff (res : Resources.t) partition ~add_coinbase work =
      one_prediff ~constraint_constants work res.discarded.commands_rev
        ~receiver partition ~add_coinbase logger ~is_coinbase_receiver_new
        ~supercharge_coinbase `Second
    in
    let isEmpty (res : Resources.t) =
      has_no_commands res && Resources.coinbase_added res = 0
    in
    (*Partitioning explained in PR #687 *)
    match partitions.second with
    | None ->
        let res, log =
          one_prediff ~constraint_constants cw_seq ts_seq ~receiver
            partitions.first ~add_coinbase:true logger ~is_coinbase_receiver_new
            ~supercharge_coinbase `First
        in
        make_diff (res, log) None
    | Some y ->
        assert (Sequence.length cw_seq <= snd partitions.first + snd y) ;
        let cw_seq_1 = Sequence.take cw_seq (snd partitions.first) in
        let cw_seq_2 = Sequence.drop cw_seq (snd partitions.first) in
        let res, log1 =
          one_prediff ~constraint_constants cw_seq_1 ts_seq ~receiver
            partitions.first ~add_coinbase:false logger
            ~is_coinbase_receiver_new ~supercharge_coinbase `First
        in
        let incr_coinbase_and_compute res count =
          let new_res =
            Resources.incr_coinbase_part_by ~constraint_constants res count
          in
          if Resources.space_available new_res then
            (*All slots could not be filled either because of budget constraints or not enough work done. Don't create the second prediff instead recompute first diff with just once coinbase*)
            ( one_prediff ~constraint_constants cw_seq_1 ts_seq ~receiver
                partitions.first ~add_coinbase:true logger
                ~is_coinbase_receiver_new ~supercharge_coinbase `First
            , None )
          else
            let res2, log2 =
              second_pre_diff new_res y ~add_coinbase:false cw_seq_2
            in
            if isEmpty res2 then
              (*Don't create the second prediff instead recompute first diff with just once coinbase*)
              ( one_prediff ~constraint_constants cw_seq_1 ts_seq ~receiver
                  partitions.first ~add_coinbase:true logger
                  ~is_coinbase_receiver_new ~supercharge_coinbase `First
              , None )
            else ((new_res, log1), Some (res2, log2))
        in
        let try_with_coinbase () =
          one_prediff ~constraint_constants cw_seq_1 ts_seq ~receiver
            partitions.first ~add_coinbase:true logger ~is_coinbase_receiver_new
            ~supercharge_coinbase `First
        in
        let res1, res2 =
          if Sequence.is_empty res.commands_rev then
            let res = try_with_coinbase () in
            (res, None)
          else
            match Resources.available_space res with
            | 0 ->
                (*generate the next prediff with a coinbase at least*)
                let res2 = second_pre_diff res y ~add_coinbase:true cw_seq_2 in
                ((res, log1), Some res2)
            | 1 ->
                (*There's a slot available in the first partition, fill it with coinbase and create another pre_diff for the slots in the second partiton with the remaining user commands and work *)
                incr_coinbase_and_compute res `One
            | 2 ->
                (*There are two slots which cannot be filled using user commands, so we split the coinbase into two parts and fill those two spots*)
                incr_coinbase_and_compute res `Two
            | _ ->
                (* Too many slots left in the first partition. Either there wasn't enough work to add transactions or there weren't enough transactions. Create a new pre_diff for just the first partition*)
                let res = try_with_coinbase () in
                (res, None)
        in
        let coinbase_added =
          Resources.coinbase_added (fst res1)
          + Option.value_map
              ~f:(Fn.compose Resources.coinbase_added fst)
              res2 ~default:0
        in
        if coinbase_added > 0 then make_diff res1 res2
        else
          (*Coinbase takes priority over user-commands. Create a diff in partitions.first with coinbase first and user commands if possible*)
          let res = try_with_coinbase () in
          make_diff res None

  let can_apply_supercharged_coinbase_exn ~winner ~epoch_ledger ~global_slot =
    Sparse_ledger.has_locked_tokens_exn ~global_slot
      ~account_id:(Account_id.create winner Token_id.default)
      epoch_ledger
    |> not

  let create_diff
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ?(log_block_creation = false) t ~coinbase_receiver ~logger
      ~current_state_view
      ~(transactions_by_fee : User_command.Valid.t Sequence.t)
      ~(get_completed_work :
            Transaction_snark_work.Statement.t
         -> Transaction_snark_work.Checked.t option) ~supercharge_coinbase =
    let open Result.Let_syntax in
    O1trace.trace_event "curr_hash" ;
    let validating_ledger = Transaction_validator.create t.ledger in
    let is_new_account pk =
      Transaction_validator.Hashless_ledger.location_of_account
        validating_ledger
        (Account_id.create pk Token_id.default)
      |> Option.is_none
    in
    let is_coinbase_receiver_new = is_new_account coinbase_receiver in
    if supercharge_coinbase then
      [%log info]
        "No locked tokens in the delegator/delegatee account, applying \
         supercharged coinbase" ;
    O1trace.trace_event "done mask" ;
    let partitions = Scan_state.partition_if_overflowing t.scan_state in
    O1trace.trace_event "partitioned" ;
    let work_to_do = Scan_state.work_statements_for_new_diff t.scan_state in
    O1trace.trace_event "computed_work" ;
    let completed_works_seq, proof_count =
      List.fold_until work_to_do ~init:(Sequence.empty, 0)
        ~f:(fun (seq, count) w ->
          match get_completed_work w with
          | Some cw_checked ->
              (*If new provers can't pay the account-creation-fee then discard
                their work unless their fee is zero in which case their account
                won't be created. This is to encourage using an existing accounts
                for snarking.
                This also imposes new snarkers to have a min fee until one of
                their snarks are purchased and their accounts get created*)
              if
                Currency.Fee.(cw_checked.fee = zero)
                || Currency.Fee.(
                     cw_checked.fee >= constraint_constants.account_creation_fee)
                || not (is_new_account cw_checked.prover)
              then
                Continue
                  ( Sequence.append seq (Sequence.singleton cw_checked)
                  , One_or_two.length cw_checked.proofs + count )
              else (
                [%log debug]
                  ~metadata:
                    [ ( "work"
                      , Transaction_snark_work.Checked.to_yojson cw_checked )
                    ; ( "work_ids"
                      , Transaction_snark_work.Statement.compact_json w )
                    ; ("snark_fee", Currency.Fee.to_yojson cw_checked.fee)
                    ; ( "account_creation_fee"
                      , Currency.Fee.to_yojson
                          constraint_constants.account_creation_fee )
                    ]
                  !"Staged_ledger_diff creation: Snark fee $snark_fee \
                    insufficient to create the snark worker account" ;
                Stop (seq, count) )
          | None ->
              [%log debug]
                ~metadata:
                  [ ("statement", Transaction_snark_work.Statement.to_yojson w)
                  ; ("work_ids", Transaction_snark_work.Statement.compact_json w)
                  ]
                !"Staged_ledger_diff creation: No snark work found for \
                  $statement" ;
              Stop (seq, count))
        ~finish:Fn.id
    in
    O1trace.trace_event "found completed work" ;
    (*Transactions in reverse order for faster removal if there is no space when creating the diff*)
    let valid_on_this_ledger =
      Sequence.fold_until transactions_by_fee ~init:(Sequence.empty, 0)
        ~f:(fun (seq, count) txn ->
          match
            O1trace.measure "validate txn" (fun () ->
                Transaction_validator.apply_transaction ~constraint_constants
                  validating_ledger ~txn_state_view:current_state_view
                  (Command (txn :> User_command.t)))
          with
          | Error e ->
              [%log error]
                ~metadata:
                  [ ("user_command", User_command.Valid.to_yojson txn)
                  ; ("error", Error_json.error_to_yojson e)
                  ]
                "Staged_ledger_diff creation: Skipping user command: \
                 $user_command due to error: $error" ;
              Continue (seq, count)
          | Ok status ->
              let txn_with_status = { With_status.data = txn; status } in
              let seq' =
                Sequence.append (Sequence.singleton txn_with_status) seq
              in
              let count' = count + 1 in
              if count' >= Scan_state.free_space t.scan_state then Stop seq'
              else Continue (seq', count'))
        ~finish:fst
    in
    let diff, log =
      O1trace.measure "generate diff" (fun () ->
          generate ~constraint_constants logger completed_works_seq
            valid_on_this_ledger ~receiver:coinbase_receiver
            ~is_coinbase_receiver_new ~supercharge_coinbase partitions)
    in
    let%map diff =
      (* Fill in the statuses for commands. *)
      let generate_status =
        let status_ledger = Transaction_validator.create t.ledger in
        fun txn ->
          O1trace.measure "get txn status" (fun () ->
              Transaction_validator.apply_transaction ~constraint_constants
                status_ledger ~txn_state_view:current_state_view txn)
      in
      Pre_diff_info.compute_statuses ~constraint_constants ~diff
        ~coinbase_amount:
          (Option.value_exn
             (coinbase_amount ~constraint_constants ~supercharge_coinbase))
        ~coinbase_receiver ~generate_status ~forget:User_command.forget_check
    in
    let summaries, detailed = List.unzip log in
    [%log debug]
      "Number of proofs ready for purchase: $proof_count Number of user \
       commands ready to be included: $txn_count Diff creation log: $diff_log"
      ~metadata:
        [ ("proof_count", `Int proof_count)
        ; ("txn_count", `Int (Sequence.length valid_on_this_ledger))
        ; ("diff_log", Diff_creation_log.summary_list_to_yojson summaries)
        ] ;
    if log_block_creation then
      [%log debug] "Detailed diff creation log: $diff_log"
        ~metadata:
          [ ( "diff_log"
            , Diff_creation_log.detail_list_to_yojson
                (List.map ~f:List.rev detailed) )
          ] ;
    trace_event "prediffs done" ;
    { Staged_ledger_diff.With_valid_signatures_and_proofs.diff }
end

include T

let%test_module "test" =
  ( module struct
    module Sl = T

    let self_pk =
      Quickcheck.random_value ~seed:(`Deterministic "self_pk")
        Public_key.Compressed.gen

    let coinbase_receiver =
      Quickcheck.random_value ~seed:(`Deterministic "receiver_pk")
        Public_key.Compressed.gen

    let proof_level = Genesis_constants.Proof_level.for_unit_tests

    let constraint_constants =
      Genesis_constants.Constraint_constants.for_unit_tests

    let logger = Logger.null ()

    let verifier =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Verifier.create ~logger ~proof_level ~constraint_constants
            ~conf_dir:None
            ~pids:(Child_processes.Termination.create_pid_table ()))

    let supercharge_coinbase ~ledger ~winner ~global_slot =
      (*using staged ledger to confirm coinbase amount is correctly generated*)
      let epoch_ledger =
        Sparse_ledger.of_ledger_subset_exn ledger
          (List.map [ winner ] ~f:(fun k ->
               Account_id.create k Token_id.default))
      in
      Sl.can_apply_supercharged_coinbase_exn ~winner ~global_slot ~epoch_ledger

    (* Functor for testing with different instantiated staged ledger modules. *)
    let create_and_apply_with_state_body_hash
        ?(coinbase_receiver = coinbase_receiver) ?(winner = self_pk)
        ~(current_state_view : Snapp_predicate.Protocol_state.View.t)
        ~state_and_body_hash sl txns stmt_to_work =
      let open Deferred.Let_syntax in
      let supercharge_coinbase =
        supercharge_coinbase ~ledger:(Sl.ledger !sl) ~winner
          ~global_slot:current_state_view.global_slot_since_genesis
      in
      let diff =
        Sl.create_diff ~constraint_constants !sl ~logger ~current_state_view
          ~transactions_by_fee:txns ~get_completed_work:stmt_to_work
          ~supercharge_coinbase ~coinbase_receiver
      in
      let diff =
        match diff with
        | Ok x ->
            x
        | Error e ->
            Error.raise (Pre_diff_info.Error.to_error e)
      in
      let diff' = Staged_ledger_diff.forget diff in
      let%map ( `Hash_after_applying hash
              , `Ledger_proof ledger_proof
              , `Staged_ledger sl'
              , `Pending_coinbase_update (is_new_stack, pc_update) ) =
        match%map
          Sl.apply ~constraint_constants !sl diff' ~logger ~verifier
            ~current_state_view ~state_and_body_hash ~coinbase_receiver
            ~supercharge_coinbase
        with
        | Ok x ->
            x
        | Error e ->
            Error.raise (Sl.Staged_ledger_error.to_error e)
      in
      assert (Staged_ledger_hash.equal hash (Sl.hash sl')) ;
      sl := sl' ;
      (ledger_proof, diff', is_new_stack, pc_update, supercharge_coinbase)

    let dummy_state_view
        ?(global_slot_since_genesis = Mina_numbers.Global_slot.zero) () =
      let state_body =
        let consensus_constants =
          let genesis_constants = Genesis_constants.for_unit_tests in
          Consensus.Constants.create ~constraint_constants
            ~protocol_constants:genesis_constants.protocol
        in
        let compile_time_genesis =
          (*not using Precomputed_values.for_unit_test because of dependency cycle*)
          Mina_state.Genesis_protocol_state.t
            ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
            ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
            ~constraint_constants ~consensus_constants
        in
        compile_time_genesis.data |> Mina_state.Protocol_state.body
      in
      { (Mina_state.Protocol_state.Body.view state_body) with
        global_slot_since_genesis
      }

    let create_and_apply ?(coinbase_receiver = coinbase_receiver)
        ?(winner = self_pk) sl txns stmt_to_work =
      let open Deferred.Let_syntax in
      let%map ledger_proof, diff, _, _, _ =
        create_and_apply_with_state_body_hash ~coinbase_receiver ~winner
          ~current_state_view:(dummy_state_view ())
          ~state_and_body_hash:(State_hash.dummy, State_body_hash.dummy)
          sl txns stmt_to_work
      in
      (ledger_proof, diff)

    (* Run the given function inside of the Deferred monad, with a staged
         ledger and a separate test ledger, after applying the given
         init_state to both. In the below tests we apply the same commands to
         the staged and test ledgers, and verify they are in the same state.
    *)
    let async_with_ledgers ledger_init_state
        (f : Sl.t ref -> Ledger.Mask.Attached.t -> unit Deferred.t) =
      Ledger.with_ephemeral_ledger ~depth:constraint_constants.ledger_depth
        ~f:(fun ledger ->
          Ledger.apply_initial_ledger_state ledger ledger_init_state ;
          let casted = Ledger.Any_ledger.cast (module Ledger) ledger in
          let test_mask =
            Ledger.Maskable.register_mask casted
              (Ledger.Mask.create ~depth:(Ledger.depth ledger) ())
          in
          let sl = ref @@ Sl.create_exn ~constraint_constants ~ledger in
          Async.Thread_safe.block_on_async_exn (fun () -> f sl test_mask) ;
          ignore @@ Ledger.Maskable.unregister_mask_exn ~loc:__LOC__ test_mask)

    (* Assert the given staged ledger is in the correct state after applying
         the first n user commands passed to the given base ledger. Checks the
         states of the block producer account and user accounts but ignores
         snark workers for simplicity. *)
    let assert_ledger :
           Ledger.t
        -> coinbase_cost:Currency.Fee.t
        -> Sl.t
        -> User_command.Valid.t list
        -> int
        -> Account_id.t list
        -> unit =
     fun test_ledger ~coinbase_cost staged_ledger cmds_all cmds_used
         pks_to_check ->
      let producer_account_id =
        Account_id.create coinbase_receiver Token_id.default
      in
      let producer_account =
        Option.bind
          (Ledger.location_of_account test_ledger producer_account_id)
          ~f:(Ledger.get test_ledger)
      in
      let is_producer_acc_new = Option.is_none producer_account in
      let old_producer_balance =
        Option.value_map producer_account ~default:Currency.Balance.zero
          ~f:(fun a -> a.balance)
      in
      let rec apply_cmds =
        let open Or_error.Let_syntax in
        function
        | [] ->
            return ()
        | (cmd : User_command.Valid.t) :: cmds ->
            let%bind _ =
              Ledger.apply_transaction ~constraint_constants test_ledger
                ~txn_state_view:(dummy_state_view ())
                (Command (cmd :> User_command.t))
            in
            apply_cmds cmds
      in
      Or_error.ok_exn @@ apply_cmds @@ List.take cmds_all cmds_used ;
      let get_account_exn ledger pk =
        Option.value_exn
          (Option.bind
             (Ledger.location_of_account ledger pk)
             ~f:(Ledger.get ledger))
      in
      (* Check the user accounts in the updated staged ledger are as
           expected. *)
      List.iter pks_to_check ~f:(fun pk ->
          let expect = get_account_exn test_ledger pk in
          let actual = get_account_exn (Sl.ledger staged_ledger) pk in
          [%test_result: Account.t] ~expect actual) ;
      (* We only test that the block producer got the coinbase reward here, since calculating the exact correct amount depends on the snark fees and tx fees. *)
      let producer_balance_with_coinbase =
        (let open Option.Let_syntax in
        let%bind total_cost =
          if is_producer_acc_new then
            Currency.Fee.add coinbase_cost
              constraint_constants.account_creation_fee
          else Some coinbase_cost
        in
        let%bind reward =
          Currency.Amount.(
            sub constraint_constants.coinbase_amount (of_fee total_cost))
        in
        Currency.Balance.add_amount old_producer_balance reward)
        |> Option.value_exn
      in
      let new_producer_balance =
        (get_account_exn (Sl.ledger staged_ledger) producer_account_id).balance
      in
      assert (
        Currency.Balance.(
          new_producer_balance >= producer_balance_with_coinbase) )

    let work_fee = constraint_constants.account_creation_fee

    (* Deterministically compute a prover public key from a snark work statement. *)
    let stmt_to_prover :
        Transaction_snark_work.Statement.t -> Public_key.Compressed.t =
     fun stmts ->
      let prover_seed =
        One_or_two.fold stmts ~init:"P" ~f:(fun p stmt ->
            p ^ Frozen_ledger_hash.to_bytes stmt.target.ledger)
      in
      Quickcheck.random_value ~seed:(`Deterministic prover_seed)
        Public_key.Compressed.gen

    let proofs stmts : Ledger_proof.t One_or_two.t =
      let sok_digest = Sok_message.Digest.default in
      One_or_two.map stmts ~f:(fun statement ->
          Ledger_proof.create ~statement ~sok_digest
            ~proof:Proof.transaction_dummy)

    let stmt_to_work_random_prover (stmts : Transaction_snark_work.Statement.t)
        : Transaction_snark_work.Checked.t option =
      let prover = stmt_to_prover stmts in
      Some
        { Transaction_snark_work.Checked.fee = work_fee
        ; proofs = proofs stmts
        ; prover
        }

    let stmt_to_work_zero_fee ~prover
        (stmts : Transaction_snark_work.Statement.t) :
        Transaction_snark_work.Checked.t option =
      Some
        { Transaction_snark_work.Checked.fee = Currency.Fee.zero
        ; proofs = proofs stmts
        ; prover
        }

    (* Fixed public key for when there is only one snark worker. *)
    let snark_worker_pk =
      Quickcheck.random_value ~seed:(`Deterministic "snark worker")
        Public_key.Compressed.gen

    let stmt_to_work_one_prover (stmts : Transaction_snark_work.Statement.t) :
        Transaction_snark_work.Checked.t option =
      Some { fee = work_fee; proofs = proofs stmts; prover = snark_worker_pk }

    let coinbase_first_prediff = function
      | Staged_ledger_diff.At_most_two.Zero ->
          (0, [])
      | One None ->
          (1, [])
      | One (Some ft) ->
          (1, [ ft ])
      | Two None ->
          (2, [])
      | Two (Some (ft, None)) ->
          (2, [ ft ])
      | Two (Some (ft1, Some ft2)) ->
          (2, [ ft1; ft2 ])

    let coinbase_second_prediff = function
      | Staged_ledger_diff.At_most_one.Zero ->
          (0, [])
      | One None ->
          (1, [])
      | One (Some ft) ->
          (1, [ ft ])

    let coinbase_count (sl_diff : Staged_ledger_diff.t) =
      (coinbase_first_prediff (fst sl_diff.diff).coinbase |> fst)
      + Option.value_map ~default:0 (snd sl_diff.diff) ~f:(fun d ->
            coinbase_second_prediff d.coinbase |> fst)

    let coinbase_cost (sl_diff : Staged_ledger_diff.t) =
      let coinbase_fts =
        (coinbase_first_prediff (fst sl_diff.diff).coinbase |> snd)
        @ Option.value_map ~default:[] (snd sl_diff.diff) ~f:(fun d ->
              coinbase_second_prediff d.coinbase |> snd)
      in
      List.fold coinbase_fts ~init:Currency.Fee.zero ~f:(fun total ft ->
          Currency.Fee.add total ft.fee |> Option.value_exn)

    let () =
      Async.Scheduler.set_record_backtraces true ;
      Backtrace.elide := false

    (* The tests are still very slow, so we set ~trials very low for all the
       QuickCheck tests. We may be able to turn them up after #2759 and/or #2760
       happen.
    *)

    (* Get the public keys from a ledger init state. *)
    let init_pks (init : Ledger.init_state) =
      Array.to_sequence init
      |> Sequence.map ~f:(fun (kp, _, _, _) ->
             Account_id.create
               (Public_key.compress kp.public_key)
               Token_id.default)
      |> Sequence.to_list

    (* Fee excess at top level ledger proofs should always be zero *)
    let assert_fee_excess :
        (Ledger_proof.t * (Transaction.t With_status.t * _) list) option -> unit
        =
     fun proof_opt ->
      let fee_excess =
        Option.value_map ~default:Fee_excess.zero proof_opt
          ~f:(fun (proof, _txns) -> (Ledger_proof.statement proof).fee_excess)
      in
      assert (Fee_excess.is_zero fee_excess)

    let transaction_capacity =
      Int.pow 2 constraint_constants.transaction_capacity_log_2

    (* Abstraction for the pattern of taking a list of commands and applying it
       in chunks up to a given max size. *)
    let rec iter_cmds_acc :
           User_command.Valid.t list (** All the commands to apply. *)
        -> int option list
           (** A list of chunk sizes. If a chunk's size is None, apply as many
            commands as possible. *)
        -> 'acc
        -> (   User_command.Valid.t list (** All commands remaining. *)
            -> int option (* Current chunk size. *)
            -> User_command.Valid.t Sequence.t
               (* Sequence of commands to apply. *)
            -> 'acc
            -> (Staged_ledger_diff.t * 'acc) Deferred.t)
        -> 'acc Deferred.t =
     fun cmds cmd_iters acc f ->
      match cmd_iters with
      | [] ->
          Deferred.return acc
      | count_opt :: counts_rest ->
          let cmds_this_iter_max =
            match count_opt with
            | None ->
                cmds
            | Some count ->
                assert (count <= List.length cmds) ;
                List.take cmds count
          in
          let%bind diff, acc' =
            f cmds count_opt (Sequence.of_list cmds_this_iter_max) acc
          in
          let cmds_applied_count =
            List.length @@ Staged_ledger_diff.commands diff
          in
          iter_cmds_acc (List.drop cmds cmds_applied_count) counts_rest acc' f

    (** Generic test framework. *)

    let test_simple :
           Ledger.init_state
        -> User_command.Valid.t list
        -> int option list
        -> Sl.t ref
        -> ?expected_proof_count:int option (*Number of ledger proofs expected*)
        -> Ledger.Mask.Attached.t
        -> [ `One_prover | `Many_provers ]
        -> (   Transaction_snark_work.Statement.t
            -> Transaction_snark_work.Checked.t option)
        -> unit Deferred.t =
     fun init_state cmds cmd_iters sl ?(expected_proof_count = None) test_mask
         provers stmt_to_work ->
      let%map total_ledger_proofs =
        iter_cmds_acc cmds cmd_iters 0
          (fun cmds_left count_opt cmds_this_iter proof_count ->
            let%bind ledger_proof, diff =
              create_and_apply sl cmds_this_iter stmt_to_work
            in
            let proof_count' =
              proof_count + if Option.is_some ledger_proof then 1 else 0
            in
            assert_fee_excess ledger_proof ;
            let cmds_applied_this_iter =
              List.length @@ Staged_ledger_diff.commands diff
            in
            let cb = coinbase_count diff in
            ( match provers with
            | `One_prover ->
                assert (cb = 1)
            | `Many_provers ->
                assert (cb > 0 && cb < 3) ) ;
            ( match count_opt with
            | Some _ ->
                (* There is an edge case where cmds_applied_this_iter = 0, when
                   there is only enough space for coinbase transactions. *)
                assert (cmds_applied_this_iter <= Sequence.length cmds_this_iter) ;
                [%test_eq: User_command.t list]
                  (List.map (Staged_ledger_diff.commands diff)
                     ~f:(fun { With_status.data; _ } -> data))
                  ( Sequence.take cmds_this_iter cmds_applied_this_iter
                    |> Sequence.to_list
                    :> User_command.t list )
            | None ->
                () ) ;
            let coinbase_cost = coinbase_cost diff in
            assert_ledger test_mask ~coinbase_cost !sl cmds_left
              cmds_applied_this_iter (init_pks init_state) ;
            return (diff, proof_count'))
      in
      (*Should have enough blocks to generate at least expected_proof_count
        proofs*)
      if Option.is_some expected_proof_count then
        assert (total_ledger_proofs = Option.value_exn expected_proof_count)

    (* How many blocks do we need to fully exercise the ledger
       behavior and produce one ledger proof *)
    let min_blocks_for_first_snarked_ledger_generic =
      (constraint_constants.transaction_capacity_log_2 + 1)
      * (constraint_constants.work_delay + 1)
      + 1

    (* n-1 extra blocks for n ledger proofs since we are already producing one
       proof *)
    let max_blocks_for_coverage n =
      min_blocks_for_first_snarked_ledger_generic + n - 1

    (** Generator for when we always have enough commands to fill all slots. *)

    let gen_at_capacity :
        (Ledger.init_state * User_command.Valid.t list * int option list)
        Quickcheck.Generator.t =
      let open Quickcheck.Generator.Let_syntax in
      let%bind ledger_init_state = Ledger.gen_initial_ledger_state in
      let%bind iters = Int.gen_incl 1 (max_blocks_for_coverage 0) in
      let total_cmds = transaction_capacity * iters in
      let%bind cmds =
        User_command.Valid.Gen.sequence ~length:total_cmds ~sign_type:`Real
          ledger_init_state
      in
      assert (List.length cmds = total_cmds) ;
      return (ledger_init_state, cmds, List.init iters ~f:(Fn.const None))

    (*Same as gen_at_capacity except that the number of iterations[iters] is
      the function of [extra_block_count] and is same for all generated values*)
    let gen_at_capacity_fixed_blocks extra_block_count :
        (Ledger.init_state * User_command.Valid.t list * int option list)
        Quickcheck.Generator.t =
      let open Quickcheck.Generator.Let_syntax in
      let%bind ledger_init_state = Ledger.gen_initial_ledger_state in
      let iters = max_blocks_for_coverage extra_block_count in
      let total_cmds = transaction_capacity * iters in
      let%bind cmds =
        User_command.Valid.Gen.sequence ~length:total_cmds ~sign_type:`Real
          ledger_init_state
      in
      assert (List.length cmds = total_cmds) ;
      return (ledger_init_state, cmds, List.init iters ~f:(Fn.const None))

    (* Generator for when we have less commands than needed to fill all slots. *)
    let gen_below_capacity ?(extra_blocks = false) () =
      let open Quickcheck.Generator.Let_syntax in
      let%bind ledger_init_state = Ledger.gen_initial_ledger_state in
      let iters_max =
        max_blocks_for_coverage 0 * if extra_blocks then 4 else 2
      in
      let%bind iters = Int.gen_incl 1 iters_max in
      (* N.B. user commands per block is much less than transactions per block
         due to fee transfers and coinbases, especially with worse case number
         of provers, so in order to exercise not filling the scan state
         completely we always apply <= 1/2 transaction_capacity commands.
      *)
      let%bind cmds_per_iter =
        Quickcheck.Generator.list_with_length iters
          (Int.gen_incl 1 ((transaction_capacity / 2) - 1))
      in
      let total_cmds = List.sum (module Int) ~f:Fn.id cmds_per_iter in
      let%bind cmds =
        User_command.Valid.Gen.sequence ~length:total_cmds ~sign_type:`Real
          ledger_init_state
      in
      assert (List.length cmds = total_cmds) ;
      return (ledger_init_state, cmds, List.map ~f:Option.some cmds_per_iter)

    let%test_unit "Max throughput-ledger proof count-fixed blocks" =
      let expected_proof_count = 3 in
      Quickcheck.test (gen_at_capacity_fixed_blocks expected_proof_count)
        ~sexp_of:
          [%sexp_of:
            Ledger.init_state
            * Mina_base.User_command.Valid.t list
            * int option list] ~trials:1
        ~f:(fun (ledger_init_state, cmds, iters) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_simple ledger_init_state cmds iters sl
                ~expected_proof_count:(Some expected_proof_count) test_mask
                `Many_provers stmt_to_work_random_prover))

    let%test_unit "Max throughput" =
      Quickcheck.test gen_at_capacity
        ~sexp_of:
          [%sexp_of:
            Ledger.init_state
            * Mina_base.User_command.Valid.t list
            * int option list] ~trials:15
        ~f:(fun (ledger_init_state, cmds, iters) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_simple ledger_init_state cmds iters sl test_mask
                `Many_provers stmt_to_work_random_prover))

    let%test_unit "Be able to include random number of commands" =
      Quickcheck.test (gen_below_capacity ()) ~trials:20
        ~f:(fun (ledger_init_state, cmds, iters) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_simple ledger_init_state cmds iters sl test_mask
                `Many_provers stmt_to_work_random_prover))

    let%test_unit "Be able to include random number of commands (One prover)" =
      Quickcheck.test (gen_below_capacity ()) ~trials:20
        ~f:(fun (ledger_init_state, cmds, iters) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_simple ledger_init_state cmds iters sl test_mask `One_prover
                stmt_to_work_one_prover))

    let%test_unit "Zero proof-fee should not create a fee transfer" =
      let stmt_to_work_zero_fee stmts =
        Some
          { Transaction_snark_work.Checked.fee = Currency.Fee.zero
          ; proofs = proofs stmts
          ; prover = snark_worker_pk
          }
      in
      let expected_proof_count = 3 in
      Quickcheck.test (gen_at_capacity_fixed_blocks expected_proof_count)
        ~trials:20 ~f:(fun (ledger_init_state, cmds, iters) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              let%map () =
                test_simple ~expected_proof_count:(Some expected_proof_count)
                  ledger_init_state cmds iters sl test_mask `One_prover
                  stmt_to_work_zero_fee
              in
              assert (
                Option.is_none
                  (Mina_base.Ledger.location_of_account test_mask
                     (Account_id.create snark_worker_pk Token_id.default)) )))

    let compute_statuses ~ledger ~coinbase_amount diff =
      let generate_status =
        let status_ledger = Transaction_validator.create ledger in
        fun txn ->
          O1trace.measure "get txn status" (fun () ->
              Transaction_validator.apply_transaction ~constraint_constants
                status_ledger ~txn_state_view:(dummy_state_view ()) txn)
      in
      Pre_diff_info.compute_statuses ~constraint_constants ~diff
        ~coinbase_amount ~coinbase_receiver ~generate_status ~forget:Fn.id
      |> Result.map_error ~f:Pre_diff_info.Error.to_error
      |> Or_error.ok_exn

    let%test_unit "Invalid diff test: check zero fee excess for partitions" =
      let create_diff_with_non_zero_fee_excess ~ledger ~coinbase_amount txns
          completed_works (partition : Sl.Scan_state.Space_partition.t) :
          Staged_ledger_diff.t =
        (*With exact number of user commands in partition.first, the fee transfers that settle the fee_excess would be added to the next tree causing a non-zero fee excess*)
        let slots, job_count1 = partition.first in
        match partition.second with
        | None ->
            { diff =
                compute_statuses ~ledger ~coinbase_amount
                @@ ( { completed_works = List.take completed_works job_count1
                     ; commands = List.take txns slots
                     ; coinbase = Zero
                     ; internal_command_balances = []
                     }
                   , None )
            }
        | Some (_, _) ->
            let txns_in_second_diff = List.drop txns slots in
            let diff : Staged_ledger_diff.Diff.t =
              ( { completed_works = List.take completed_works job_count1
                ; commands = List.take txns slots
                ; coinbase = Zero
                ; internal_command_balances = []
                }
              , Some
                  { completed_works =
                      ( if List.is_empty txns_in_second_diff then []
                      else List.drop completed_works job_count1 )
                  ; commands = txns_in_second_diff
                  ; coinbase = Zero
                  ; internal_command_balances = []
                  } )
            in
            { diff = compute_statuses ~ledger ~coinbase_amount diff }
      in
      let empty_diff : Staged_ledger_diff.t =
        { diff =
            ( { completed_works = []
              ; commands = []
              ; coinbase = Staged_ledger_diff.At_most_two.Zero
              ; internal_command_balances = []
              }
            , None )
        }
      in
      Quickcheck.test (gen_below_capacity ())
        ~sexp_of:
          [%sexp_of:
            Ledger.init_state * User_command.Valid.t list * int option list]
        ~trials:10 ~f:(fun (ledger_init_state, cmds, iters) ->
          async_with_ledgers ledger_init_state (fun sl _test_mask ->
              let%map checked =
                iter_cmds_acc cmds iters true
                  (fun _cmds_left _count_opt cmds_this_iter checked ->
                    let scan_state = Sl.scan_state !sl in
                    let work =
                      Sl.Scan_state.work_statements_for_new_diff scan_state
                    in
                    let partitions =
                      Sl.Scan_state.partition_if_overflowing scan_state
                    in
                    let work_done =
                      List.map
                        ~f:(fun stmts ->
                          { Transaction_snark_work.Checked.fee = Fee.zero
                          ; proofs = proofs stmts
                          ; prover = snark_worker_pk
                          })
                        work
                    in
                    let cmds_this_iter =
                      cmds_this_iter |> Sequence.to_list
                      |> List.map ~f:(fun cmd ->
                             { With_status.data = (cmd :> User_command.t)
                             ; status =
                                 Applied
                                   ( Transaction_status.Auxiliary_data.empty
                                   , Transaction_status.Balance_data.empty )
                             })
                    in
                    let diff =
                      create_diff_with_non_zero_fee_excess
                        ~ledger:(Sl.ledger !sl)
                        ~coinbase_amount:constraint_constants.coinbase_amount
                        cmds_this_iter work_done partitions
                    in
                    let%bind apply_res =
                      Sl.apply ~constraint_constants !sl diff ~logger ~verifier
                        ~current_state_view:(dummy_state_view ())
                        ~state_and_body_hash:
                          (State_hash.dummy, State_body_hash.dummy)
                        ~coinbase_receiver ~supercharge_coinbase:true
                    in
                    let checked', diff' =
                      match apply_res with
                      | Error (Sl.Staged_ledger_error.Non_zero_fee_excess _) ->
                          (true, empty_diff)
                      | Error err ->
                          failwith
                          @@ sprintf
                               !"Expecting Non-zero-fee-excess error, got \
                                 %{sexp: Sl.Staged_ledger_error.t}"
                               err
                      | Ok
                          ( `Hash_after_applying _hash
                          , `Ledger_proof _ledger_proof
                          , `Staged_ledger sl'
                          , `Pending_coinbase_update _ ) ->
                          sl := sl' ;
                          (false, diff)
                    in
                    return (diff', checked || checked'))
              in
              (*Note: if this fails, try increasing the number of trials*)
              assert checked))

    let%test_unit "Provers can't pay the account creation fee" =
      let no_work_included (diff : Staged_ledger_diff.t) =
        List.is_empty (Staged_ledger_diff.completed_works diff)
      in
      let stmt_to_work stmts =
        let prover = stmt_to_prover stmts in
        Some
          { Transaction_snark_work.Checked.fee =
              Currency.Fee.(sub work_fee (of_int 1)) |> Option.value_exn
          ; proofs = proofs stmts
          ; prover
          }
      in
      Quickcheck.test (gen_below_capacity ())
        ~sexp_of:
          [%sexp_of:
            Ledger.init_state * User_command.Valid.t list * int option list]
        ~shrinker:
          (Quickcheck.Shrinker.create (fun (init_state, cmds, iters) ->
               if List.length iters > 1 then
                 Sequence.singleton
                   ( init_state
                   , List.take cmds (List.length cmds - transaction_capacity)
                   , [ None ] )
               else Sequence.empty))
        ~trials:1
        ~f:(fun (ledger_init_state, cmds, iters) ->
          async_with_ledgers ledger_init_state (fun sl _test_mask ->
              iter_cmds_acc cmds iters ()
                (fun _cmds_left _count_opt cmds_this_iter () ->
                  let diff =
                    let diff =
                      Sl.create_diff ~constraint_constants !sl ~logger
                        ~current_state_view:(dummy_state_view ())
                        ~transactions_by_fee:cmds_this_iter
                        ~get_completed_work:stmt_to_work ~coinbase_receiver
                        ~supercharge_coinbase:true
                    in
                    match diff with
                    | Ok x ->
                        Staged_ledger_diff.forget x
                    | Error e ->
                        Error.raise (Pre_diff_info.Error.to_error e)
                  in
                  (*No proofs were purchased since the fee for the proofs are not sufficient to pay for account creation*)
                  assert (no_work_included diff) ;
                  Deferred.return (diff, ()))))

    let stmt_to_work_restricted work_list provers
        (stmts : Transaction_snark_work.Statement.t) :
        Transaction_snark_work.Checked.t option =
      let prover =
        match provers with
        | `Many_provers ->
            stmt_to_prover stmts
        | `One_prover ->
            snark_worker_pk
      in
      if
        Option.is_some
          (List.find work_list ~f:(fun s ->
               Transaction_snark_work.Statement.compare s stmts = 0))
      then
        Some
          { Transaction_snark_work.Checked.fee = work_fee
          ; proofs = proofs stmts
          ; prover
          }
      else None

    (** Like test_simple but with a random number of completed jobs available.
    *)

    let test_random_number_of_proofs :
           Ledger.init_state
        -> User_command.Valid.t list
        -> int option list
        -> int list
        -> Sl.t ref
        -> Ledger.Mask.Attached.t
        -> [ `One_prover | `Many_provers ]
        -> unit Deferred.t =
     fun init_state cmds cmd_iters proofs_available sl test_mask provers ->
      let%map proofs_available_left =
        iter_cmds_acc cmds cmd_iters proofs_available
          (fun cmds_left _count_opt cmds_this_iter proofs_available_left ->
            let work_list : Transaction_snark_work.Statement.t list =
              Transaction_snark_scan_state.all_work_statements_exn
                !sl.scan_state
            in
            let proofs_available_this_iter =
              List.hd_exn proofs_available_left
            in
            let%map proof, diff =
              create_and_apply sl cmds_this_iter
                (stmt_to_work_restricted
                   (List.take work_list proofs_available_this_iter)
                   provers)
            in
            assert_fee_excess proof ;
            let cmds_applied_this_iter =
              List.length @@ Staged_ledger_diff.commands diff
            in
            let cb = coinbase_count diff in
            assert (proofs_available_this_iter = 0 || cb > 0) ;
            ( match provers with
            | `One_prover ->
                assert (cb <= 1)
            | `Many_provers ->
                assert (cb <= 2) ) ;
            let coinbase_cost = coinbase_cost diff in
            assert_ledger test_mask ~coinbase_cost !sl cmds_left
              cmds_applied_this_iter (init_pks init_state) ;
            (diff, List.tl_exn proofs_available_left))
      in
      assert (List.is_empty proofs_available_left)

    let%test_unit "max throughput-random number of proofs-worst case provers" =
      (* Always at worst case number of provers *)
      let g =
        let open Quickcheck.Generator.Let_syntax in
        let%bind ledger_init_state, cmds, iters = gen_at_capacity in
        (* How many proofs will be available at each iteration. *)
        let%bind proofs_available =
          (* I think in the worst case every user command begets 1.5
             transactions - one for the command and half of one for a fee
             transfer - and the merge overhead means you need (amortized) twice
             as many SNARKs as transactions, but since a SNARK work usually
             covers two SNARKS it cancels. So we need to admit up to (1.5 * the
             number of commands) works. I make it twice as many for simplicity
             and to cover coinbases. *)
          Quickcheck_lib.map_gens iters ~f:(fun _ ->
              Int.gen_incl 0 (transaction_capacity * 2))
        in
        return (ledger_init_state, cmds, iters, proofs_available)
      in
      Quickcheck.test g ~trials:10
        ~f:(fun (ledger_init_state, cmds, iters, proofs_available) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_random_number_of_proofs ledger_init_state cmds iters
                proofs_available sl test_mask `Many_provers))

    let%test_unit "random no of transactions-random number of proofs-worst \
                   case provers" =
      let g =
        let open Quickcheck.Generator.Let_syntax in
        let%bind ledger_init_state, cmds, iters =
          gen_below_capacity ~extra_blocks:true ()
        in
        let%bind proofs_available =
          Quickcheck_lib.map_gens iters ~f:(fun cmds_opt ->
              Int.gen_incl 0 (3 * Option.value_exn cmds_opt))
        in
        return (ledger_init_state, cmds, iters, proofs_available)
      in
      let shrinker =
        Quickcheck.Shrinker.create
          (fun (ledger_init_state, cmds, iters, proofs_available) ->
            let all_but_last xs = List.take xs (List.length xs - 1) in
            let iter_count = List.length iters in
            let mod_iters iters' =
              ( ledger_init_state
              , List.take cmds
                @@ List.sum (module Int) iters' ~f:(Option.value ~default:0)
              , iters'
              , List.take proofs_available (List.length iters') )
            in
            let half_iters =
              if iter_count > 1 then
                Some (mod_iters (List.take iters (iter_count / 2)))
              else None
            in
            let one_less_iters =
              if iter_count > 2 then Some (mod_iters (all_but_last iters))
              else None
            in
            List.filter_map [ half_iters; one_less_iters ] ~f:Fn.id
            |> Sequence.of_list)
      in
      Quickcheck.test g ~shrinker ~shrink_attempts:`Exhaustive
        ~sexp_of:
          [%sexp_of:
            Ledger.init_state
            * User_command.Valid.t list
            * int option list
            * int list] ~trials:50
        ~f:(fun (ledger_init_state, cmds, iters, proofs_available) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_random_number_of_proofs ledger_init_state cmds iters
                proofs_available sl test_mask `Many_provers))

    let%test_unit "Random number of commands-random number of proofs-one \
                   prover)" =
      let g =
        let open Quickcheck.Generator.Let_syntax in
        let%bind ledger_init_state, cmds, iters =
          gen_below_capacity ~extra_blocks:true ()
        in
        let%bind proofs_available =
          Quickcheck_lib.map_gens iters ~f:(fun cmds_opt ->
              Int.gen_incl 0 (3 * Option.value_exn cmds_opt))
        in
        return (ledger_init_state, cmds, iters, proofs_available)
      in
      Quickcheck.test g ~trials:10
        ~f:(fun (ledger_init_state, cmds, iters, proofs_available) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_random_number_of_proofs ledger_init_state cmds iters
                proofs_available sl test_mask `One_prover))

    let stmt_to_work_random_fee work_list provers
        (stmts : Transaction_snark_work.Statement.t) :
        Transaction_snark_work.Checked.t option =
      let prover =
        match provers with
        | `Many_provers ->
            stmt_to_prover stmts
        | `One_prover ->
            snark_worker_pk
      in
      Option.map
        (List.find work_list ~f:(fun (s, _) ->
             Transaction_snark_work.Statement.compare s stmts = 0))
        ~f:(fun (_, fee) ->
          { Transaction_snark_work.Checked.fee; proofs = proofs stmts; prover })

    (** Like test_random_number_of_proofs but with random proof fees.
    *)
    let test_random_proof_fee :
           Ledger.init_state
        -> User_command.Valid.t list
        -> int option list
        -> (int * Fee.t list) list
        -> Sl.t ref
        -> Ledger.Mask.Attached.t
        -> [ `One_prover | `Many_provers ]
        -> unit Deferred.t =
     fun _init_state cmds cmd_iters proofs_available sl _test_mask provers ->
      let%map proofs_available_left =
        iter_cmds_acc cmds cmd_iters proofs_available
          (fun _cmds_left _count_opt cmds_this_iter proofs_available_left ->
            let work_list : Transaction_snark_work.Statement.t list =
              Sl.Scan_state.work_statements_for_new_diff (Sl.scan_state !sl)
            in
            let proofs_available_this_iter, fees_for_each =
              List.hd_exn proofs_available_left
            in
            let work_to_be_done =
              let work_list = List.take work_list proofs_available_this_iter in
              List.(zip_exn work_list (take fees_for_each (length work_list)))
            in
            let%map _proof, diff =
              create_and_apply sl cmds_this_iter
                (stmt_to_work_random_fee work_to_be_done provers)
            in
            let sorted_work_from_diff1
                (pre_diff :
                  Staged_ledger_diff.Pre_diff_with_at_most_two_coinbase.t) =
              List.sort pre_diff.completed_works ~compare:(fun w w' ->
                  Fee.compare w.fee w'.fee)
            in
            let sorted_work_from_diff2
                (pre_diff :
                  Staged_ledger_diff.Pre_diff_with_at_most_one_coinbase.t option)
                =
              Option.value_map pre_diff ~default:[] ~f:(fun p ->
                  List.sort p.completed_works ~compare:(fun w w' ->
                      Fee.compare w.fee w'.fee))
            in
            let () =
              let assert_same_fee { Coinbase.Fee_transfer.fee; _ } fee' =
                assert (Fee.equal fee fee')
              in
              let first_pre_diff, second_pre_diff_opt = diff.diff in
              match
                ( first_pre_diff.coinbase
                , Option.value_map second_pre_diff_opt
                    ~default:Staged_ledger_diff.At_most_one.Zero ~f:(fun d ->
                      d.coinbase) )
              with
              | ( Staged_ledger_diff.At_most_two.Zero
                , Staged_ledger_diff.At_most_one.Zero )
              | Two None, Zero ->
                  ()
              | One ft_opt, Zero ->
                  Option.value_map ft_opt ~default:() ~f:(fun single ->
                      let work =
                        List.hd_exn (sorted_work_from_diff1 first_pre_diff)
                        |> Transaction_snark_work.forget
                      in
                      assert_same_fee single work.fee)
              | Zero, One ft_opt ->
                  Option.value_map ft_opt ~default:() ~f:(fun single ->
                      let work =
                        List.hd_exn (sorted_work_from_diff2 second_pre_diff_opt)
                        |> Transaction_snark_work.forget
                      in
                      assert_same_fee single work.fee)
              | Two (Some (ft, ft_opt)), Zero ->
                  let work_done = sorted_work_from_diff1 first_pre_diff in
                  let work =
                    List.hd_exn work_done |> Transaction_snark_work.forget
                  in
                  assert_same_fee ft work.fee ;
                  Option.value_map ft_opt ~default:() ~f:(fun single ->
                      let work =
                        List.hd_exn (List.drop work_done 1)
                        |> Transaction_snark_work.forget
                      in
                      assert_same_fee single work.fee)
              | _ ->
                  failwith
                    (sprintf
                       !"Incorrect coinbase in the diff %{sexp: \
                         Staged_ledger_diff.t}"
                       diff)
            in
            (diff, List.tl_exn proofs_available_left))
      in
      assert (List.is_empty proofs_available_left)

    let%test_unit "max throughput-random-random fee-number of proofs-worst \
                   case provers" =
      (* Always at worst case number of provers *)
      let g =
        let open Quickcheck.Generator.Let_syntax in
        let%bind ledger_init_state, cmds, iters = gen_at_capacity in
        (* How many proofs will be available at each iteration. *)
        let%bind proofs_available =
          Quickcheck_lib.map_gens iters ~f:(fun _ ->
              let%bind number_of_proofs =
                Int.gen_incl 0 (transaction_capacity * 2)
              in
              let%map fees =
                Quickcheck.Generator.list_with_length number_of_proofs
                  Fee.(gen_incl (of_int 1) (of_int 20))
              in
              (number_of_proofs, fees))
        in
        return (ledger_init_state, cmds, iters, proofs_available)
      in
      Quickcheck.test g ~trials:10
        ~f:(fun (ledger_init_state, cmds, iters, proofs_available) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_random_proof_fee ledger_init_state cmds iters
                proofs_available sl test_mask `Many_provers))

    let%test_unit "Max throughput-random fee" =
      let g =
        let open Quickcheck.Generator.Let_syntax in
        let%bind ledger_init_state, cmds, iters = gen_at_capacity in
        let%bind proofs_available =
          Quickcheck_lib.map_gens iters ~f:(fun _ ->
              let number_of_proofs =
                transaction_capacity
                (*All proofs are available*)
              in
              let%map fees =
                Quickcheck.Generator.list_with_length number_of_proofs
                  Fee.(gen_incl (of_int 1) (of_int 20))
              in
              (number_of_proofs, fees))
        in
        return (ledger_init_state, cmds, iters, proofs_available)
      in
      Quickcheck.test g
        ~sexp_of:
          [%sexp_of:
            Ledger.init_state
            * Mina_base.User_command.Valid.t list
            * int option list
            * (int * Fee.t list) list] ~trials:10
        ~f:(fun (ledger_init_state, cmds, iters, proofs_available) ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_random_proof_fee ledger_init_state cmds iters
                proofs_available sl test_mask `Many_provers))

    let check_pending_coinbase ~supercharge_coinbase proof ~sl_before ~sl_after
        (_state_hash, state_body_hash) pc_update ~is_new_stack =
      let pending_coinbase_before = Sl.pending_coinbase_collection sl_before in
      let root_before = Pending_coinbase.merkle_root pending_coinbase_before in
      let unchecked_root_after =
        Pending_coinbase.merkle_root (Sl.pending_coinbase_collection sl_after)
      in
      let f_pop_and_add =
        let open Snark_params.Tick in
        let open Pending_coinbase in
        let proof_emitted =
          if Option.is_some proof then Boolean.true_ else Boolean.false_
        in
        let%bind root_after_popping, _deleted_stack =
          Pending_coinbase.Checked.pop_coinbases ~constraint_constants
            ~proof_emitted
            (Hash.var_of_t root_before)
        in
        let pc_update_var = Update.var_of_t pc_update in
        let coinbase_receiver =
          Public_key.Compressed.(var_of_t coinbase_receiver)
        in
        let supercharge_coinbase = Boolean.var_of_value supercharge_coinbase in
        let state_body_hash_var = State_body_hash.var_of_t state_body_hash in
        Pending_coinbase.Checked.add_coinbase ~constraint_constants
          root_after_popping pc_update_var ~coinbase_receiver
          ~supercharge_coinbase state_body_hash_var
      in
      let checked_root_after_update =
        let open Snark_params.Tick in
        let open Pending_coinbase in
        let comp =
          let%map result =
            handle f_pop_and_add
              (unstage
                 (handler ~depth:constraint_constants.pending_coinbase_depth
                    pending_coinbase_before ~is_new_stack))
          in
          As_prover.read Hash.typ result
        in
        let (), x = Or_error.ok_exn (run_and_check comp ()) in
        x
      in
      [%test_eq: Pending_coinbase.Hash.t] unchecked_root_after
        checked_root_after_update

    let test_pending_coinbase :
           Ledger.init_state
        -> User_command.Valid.t list
        -> int option list
        -> int list
        -> (State_hash.t * State_body_hash.t) list
        -> Mina_base.Snapp_predicate.Protocol_state.View.t
        -> Sl.t ref
        -> Ledger.Mask.Attached.t
        -> [ `One_prover | `Many_provers ]
        -> unit Deferred.t =
     fun init_state cmds cmd_iters proofs_available state_body_hashes
         current_state_view sl test_mask provers ->
      let%map proofs_available_left, _state_body_hashes_left =
        iter_cmds_acc cmds cmd_iters (proofs_available, state_body_hashes)
          (fun
            cmds_left
            _count_opt
            cmds_this_iter
            (proofs_available_left, state_body_hashes)
          ->
            let work_list : Transaction_snark_work.Statement.t list =
              Sl.Scan_state.all_work_statements_exn !sl.scan_state
            in
            let proofs_available_this_iter =
              List.hd_exn proofs_available_left
            in
            let sl_before = !sl in
            let state_body_hash = List.hd_exn state_body_hashes in
            let%map proof, diff, is_new_stack, pc_update, supercharge_coinbase =
              create_and_apply_with_state_body_hash ~current_state_view
                ~state_and_body_hash:state_body_hash sl cmds_this_iter
                (stmt_to_work_restricted
                   (List.take work_list proofs_available_this_iter)
                   provers)
            in
            check_pending_coinbase proof ~supercharge_coinbase ~sl_before
              ~sl_after:!sl state_body_hash pc_update ~is_new_stack ;
            assert_fee_excess proof ;
            let cmds_applied_this_iter =
              List.length @@ Staged_ledger_diff.commands diff
            in
            let cb = coinbase_count diff in
            assert (proofs_available_this_iter = 0 || cb > 0) ;
            ( match provers with
            | `One_prover ->
                assert (cb <= 1)
            | `Many_provers ->
                assert (cb <= 2) ) ;
            let coinbase_cost = coinbase_cost diff in
            assert_ledger test_mask ~coinbase_cost !sl cmds_left
              cmds_applied_this_iter (init_pks init_state) ;
            ( diff
            , (List.tl_exn proofs_available_left, List.tl_exn state_body_hashes)
            ))
      in
      assert (List.is_empty proofs_available_left)

    let pending_coinbase_test prover =
      let g =
        let open Quickcheck.Generator.Let_syntax in
        let%bind ledger_init_state, cmds, iters =
          gen_below_capacity ~extra_blocks:true ()
        in
        let%bind state_body_hashes =
          Quickcheck_lib.map_gens iters ~f:(fun _ ->
              Quickcheck.Generator.tuple2 State_hash.gen State_body_hash.gen)
        in
        let%bind proofs_available =
          Quickcheck_lib.map_gens iters ~f:(fun cmds_opt ->
              Int.gen_incl 0 (3 * Option.value_exn cmds_opt))
        in
        return
          (ledger_init_state, cmds, iters, proofs_available, state_body_hashes)
      in
      let current_state_view = dummy_state_view () in
      Quickcheck.test g ~trials:5
        ~f:(fun
             ( ledger_init_state
             , cmds
             , iters
             , proofs_available
             , state_body_hashes )
           ->
          async_with_ledgers ledger_init_state (fun sl test_mask ->
              test_pending_coinbase ledger_init_state cmds iters
                proofs_available state_body_hashes current_state_view sl
                test_mask prover))

    let%test_unit "Validate pending coinbase for random number of \
                   commands-random number of proofs-one prover)" =
      pending_coinbase_test `One_prover

    let%test_unit "Validate pending coinbase for random number of \
                   commands-random number of proofs-many provers)" =
      pending_coinbase_test `Many_provers

    let timed_account n =
      let keypair =
        Quickcheck.random_value
          ~seed:(`Deterministic (sprintf "timed_account_%d" n))
          Keypair.gen
      in
      let account_id =
        Account_id.create
          (Public_key.compress keypair.public_key)
          Token_id.default
      in
      let balance = Balance.of_int 100_000_000_000 in
      (*Should fully vest by slot = 7*)
      let acc =
        Account.create_timed account_id balance ~initial_minimum_balance:balance
          ~cliff_time:(Mina_numbers.Global_slot.of_int 4)
          ~cliff_amount:Amount.zero
          ~vesting_period:(Mina_numbers.Global_slot.of_int 2)
          ~vesting_increment:(Amount.of_int 50_000_000_000)
        |> Or_error.ok_exn
      in
      (keypair, acc)

    let untimed_account n =
      let keypair =
        Quickcheck.random_value
          ~seed:(`Deterministic (sprintf "untimed_account_%d" n))
          Keypair.gen
      in
      let account_id =
        Account_id.create
          (Public_key.compress keypair.public_key)
          Token_id.default
      in
      let balance = Balance.of_int 100_000_000_000 in
      let acc = Account.create account_id balance in
      (keypair, acc)

    let supercharge_coinbase_test ~(self : Account.t) ~(delegator : Account.t)
        ~block_count f_expected_balance sl =
      let coinbase_receiver = self in
      let init_balance = coinbase_receiver.balance in
      let check_receiver_account sl count =
        let location =
          Ledger.location_of_account (Sl.ledger sl)
            (Account.identifier coinbase_receiver)
          |> Option.value_exn
        in
        let account = Ledger.get (Sl.ledger sl) location |> Option.value_exn in
        [%test_eq: Balance.t]
          (f_expected_balance count init_balance)
          account.balance
      in
      Deferred.List.iter
        (List.init block_count ~f:(( + ) 1))
        ~f:(fun block_count ->
          let%bind _ =
            create_and_apply_with_state_body_hash ~winner:delegator.public_key
              ~coinbase_receiver:coinbase_receiver.public_key sl
              ~current_state_view:
                (dummy_state_view
                   ~global_slot_since_genesis:
                     (Mina_numbers.Global_slot.of_int block_count)
                   ())
              ~state_and_body_hash:(State_hash.dummy, State_body_hash.dummy)
              Sequence.empty
              (stmt_to_work_zero_fee ~prover:self.public_key)
          in
          check_receiver_account !sl block_count ;
          return ())

    let normal_coinbase = constraint_constants.coinbase_amount

    let scale_exn amt i = Amount.scale amt i |> Option.value_exn

    let supercharged_coinbase =
      scale_exn constraint_constants.coinbase_amount
        constraint_constants.supercharged_coinbase_factor

    let g = Ledger.gen_initial_ledger_state

    let%test_unit "Supercharged coinbase - staking" =
      let keypair_self, self = timed_account 1 in
      let slots_with_locked_tokens =
        7
        (*calculated from the timing values for timed_accounts*)
      in
      let block_count = slots_with_locked_tokens + 5 in
      let f_expected_balance block_no init_balance =
        if block_no <= slots_with_locked_tokens then
          Balance.add_amount init_balance (scale_exn normal_coinbase block_no)
          |> Option.value_exn
        else
          (* init balance +
                (normal_coinbase * slots_with_locked_tokens) +
                (supercharged_coinbase * remaining slots))*)
          Balance.add_amount
            ( Balance.add_amount init_balance
                (scale_exn normal_coinbase slots_with_locked_tokens)
            |> Option.value_exn )
            (scale_exn supercharged_coinbase
               (block_no - slots_with_locked_tokens))
          |> Option.value_exn
      in
      Quickcheck.test g ~trials:1 ~f:(fun ledger_init_state ->
          let ledger_init_state =
            Array.append
              [| ( keypair_self
                 , Balance.to_amount self.balance
                 , self.nonce
                 , self.timing )
              |]
              ledger_init_state
          in
          async_with_ledgers ledger_init_state (fun sl _test_mask ->
              supercharge_coinbase_test ~self ~delegator:self ~block_count
                f_expected_balance sl))

    let%test_unit "Supercharged coinbase - unlocked account delegating to \
                   locked account" =
      let keypair_self, locked_self = timed_account 1 in
      let keypair_delegator, unlocked_delegator = untimed_account 1 in
      let slots_with_locked_tokens =
        7
        (*calculated from the timing values for timed_accounts*)
      in
      let block_count = slots_with_locked_tokens + 2 in
      let f_expected_balance block_no init_balance =
        Balance.add_amount init_balance
          (scale_exn supercharged_coinbase block_no)
        |> Option.value_exn
      in
      Quickcheck.test g ~trials:1 ~f:(fun ledger_init_state ->
          let ledger_init_state =
            Array.append
              [| ( keypair_self
                 , Balance.to_amount locked_self.balance
                 , locked_self.nonce
                 , locked_self.timing )
               ; ( keypair_delegator
                 , Balance.to_amount unlocked_delegator.balance
                 , unlocked_delegator.nonce
                 , unlocked_delegator.timing )
              |]
              ledger_init_state
          in
          async_with_ledgers ledger_init_state (fun sl _test_mask ->
              supercharge_coinbase_test ~self:locked_self
                ~delegator:unlocked_delegator ~block_count f_expected_balance sl))

    let%test_unit "Supercharged coinbase - locked account delegating to \
                   unlocked account" =
      let keypair_self, unlocked_self = untimed_account 1 in
      let keypair_delegator, locked_delegator = timed_account 1 in
      let slots_with_locked_tokens =
        7
        (*calculated from the timing values for the timed_account*)
      in
      let block_count = slots_with_locked_tokens + 2 in
      let f_expected_balance block_no init_balance =
        if block_no <= slots_with_locked_tokens then
          Balance.add_amount init_balance (scale_exn normal_coinbase block_no)
          |> Option.value_exn
        else
          (* init balance +
                (normal_coinbase * slots_with_locked_tokens) +
                (supercharged_coinbase * remaining slots))*)
          Balance.add_amount
            ( Balance.add_amount init_balance
                (scale_exn normal_coinbase slots_with_locked_tokens)
            |> Option.value_exn )
            (scale_exn supercharged_coinbase
               (block_no - slots_with_locked_tokens))
          |> Option.value_exn
      in
      Quickcheck.test g ~trials:1 ~f:(fun ledger_init_state ->
          let ledger_init_state =
            Array.append
              [| ( keypair_self
                 , Balance.to_amount unlocked_self.balance
                 , unlocked_self.nonce
                 , unlocked_self.timing )
               ; ( keypair_delegator
                 , Balance.to_amount locked_delegator.balance
                 , locked_delegator.nonce
                 , locked_delegator.timing )
              |]
              ledger_init_state
          in
          async_with_ledgers ledger_init_state (fun sl _test_mask ->
              supercharge_coinbase_test ~self:unlocked_self
                ~delegator:locked_delegator ~block_count f_expected_balance sl))

    let%test_unit "Supercharged coinbase - locked account delegating to locked \
                   account" =
      let keypair_self, locked_self = timed_account 1 in
      let keypair_delegator, locked_delegator = timed_account 2 in
      let slots_with_locked_tokens =
        7
        (*calculated from the timing values for timed_accounts*)
      in
      let block_count = slots_with_locked_tokens in
      let f_expected_balance block_no init_balance =
        (*running the test as long as both the accounts remain locked and hence normal coinbase in all the blocks*)
        Balance.add_amount init_balance (scale_exn normal_coinbase block_no)
        |> Option.value_exn
      in
      Quickcheck.test g ~trials:1 ~f:(fun ledger_init_state ->
          let ledger_init_state =
            Array.append
              [| ( keypair_self
                 , Balance.to_amount locked_self.balance
                 , locked_self.nonce
                 , locked_self.timing )
               ; ( keypair_delegator
                 , Balance.to_amount locked_delegator.balance
                 , locked_delegator.nonce
                 , locked_delegator.timing )
              |]
              ledger_init_state
          in
          async_with_ledgers ledger_init_state (fun sl _test_mask ->
              supercharge_coinbase_test ~self:locked_self
                ~delegator:locked_delegator ~block_count f_expected_balance sl))
  end )
