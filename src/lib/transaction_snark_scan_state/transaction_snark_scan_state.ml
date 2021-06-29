open Core_kernel
open Mina_base
open Currency

let option lab =
  Option.value_map ~default:(Or_error.error_string lab) ~f:(fun x -> Ok x)

let map2_or_error xs ys ~f =
  let rec go xs ys acc =
    match (xs, ys) with
    | [], [] ->
        Ok (List.rev acc)
    | x :: xs, y :: ys -> (
      match f x y with Error e -> Error e | Ok z -> go xs ys (z :: acc) )
    | _, _ ->
        Or_error.error_string "Length mismatch"
  in
  go xs ys []

module type Monad_with_Or_error_intf = sig
  type 'a t

  include Monad.S with type 'a t := 'a t

  module Or_error : sig
    type nonrec 'a t = 'a Or_error.t t

    include Monad.S with type 'a t := 'a t
  end
end

module Transaction_with_witness = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      (* TODO: The statement is redundant here - it can be computed from the
         witness and the transaction
      *)
      type t =
        { transaction_with_info:
            Transaction_logic.Transaction_applied.Stable.V1.t
        ; state_hash: State_hash.Stable.V1.t * State_body_hash.Stable.V1.t
              (* TODO: It's inefficient to store this here. Optimize it someday. *)
        ; state_view: Mina_base.Snapp_predicate.Protocol_state.View.Stable.V1.t
        ; statement: Transaction_snark.Statement.Stable.V1.t
        ; init_stack:
            Transaction_snark.Pending_coinbase_stack_state.Init_stack.Stable.V1
            .t
        ; ledger_witness: Mina_base.Sparse_ledger.Stable.V1.t sexp_opaque }
      [@@deriving sexp]

      let to_latest = Fn.id
    end
  end]
end

module Ledger_proof_with_sok_message = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Ledger_proof.Stable.V1.t * Sok_message.Stable.V1.t
      [@@deriving sexp]

      let to_latest = Fn.id
    end
  end]
end

module Available_job = struct
  type t =
    ( Ledger_proof_with_sok_message.t
    , Transaction_with_witness.t )
    Parallel_scan.Available_job.t
  [@@deriving sexp]
end

module Space_partition = Parallel_scan.Space_partition

module Job_view = struct
  type t = Transaction_snark.Statement.t Parallel_scan.Job_view.t
  [@@deriving sexp]

  let to_yojson ({value; position} : t) : Yojson.Safe.t =
    let hash_yojson h = Frozen_ledger_hash.to_yojson h in
    let statement_to_yojson (s : Transaction_snark.Statement.t) =
      `Assoc
        [ ("Work_id", `Int (Transaction_snark.Statement.hash s))
        ; ("Source", hash_yojson s.source)
        ; ("Target", hash_yojson s.target)
        ; ( "Fee Excess"
          , `List
              [ `Assoc
                  [ ("token", Token_id.to_yojson s.fee_excess.fee_token_l)
                  ; ("amount", Fee.Signed.to_yojson s.fee_excess.fee_excess_l)
                  ]
              ; `Assoc
                  [ ("token", Token_id.to_yojson s.fee_excess.fee_token_r)
                  ; ("amount", Fee.Signed.to_yojson s.fee_excess.fee_excess_r)
                  ] ] )
        ; ("Supply Increase", Currency.Amount.to_yojson s.supply_increase)
        ; ( "Pending coinbase stack"
          , Transaction_snark.Pending_coinbase_stack_state.to_yojson
              s.pending_coinbase_stack_state ) ]
    in
    let job_to_yojson =
      match value with
      | BEmpty ->
          `Assoc [("B", `List [])]
      | MEmpty ->
          `Assoc [("M", `List [])]
      | MPart x ->
          `Assoc [("M", `List [statement_to_yojson x])]
      | MFull (x, y, {seq_no; status}) ->
          `Assoc
            [ ( "M"
              , `List
                  [ statement_to_yojson x
                  ; statement_to_yojson y
                  ; `Int seq_no
                  ; `Assoc
                      [ ( "Status"
                        , `String (Parallel_scan.Job_status.to_string status)
                        ) ] ] ) ]
      | BFull (x, {seq_no; status}) ->
          `Assoc
            [ ( "B"
              , `List
                  [ statement_to_yojson x
                  ; `Int seq_no
                  ; `Assoc
                      [ ( "Status"
                        , `String (Parallel_scan.Job_status.to_string status)
                        ) ] ] ) ]
    in
    `List [`Int position; job_to_yojson]
end

type job = Available_job.t [@@deriving sexp]

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Ledger_proof_with_sok_message.Stable.V1.t
      , Transaction_with_witness.Stable.V1.t )
      Parallel_scan.State.Stable.V1.t
    [@@deriving sexp]

    let to_latest = Fn.id

    (* TODO: Review this. The version bytes for the underlying types are
       included in the hash, so it can never be stable between versions.
    *)

    let hash t =
      let state_hash =
        Parallel_scan.State.hash t
          (Binable.to_string (module Ledger_proof_with_sok_message.Stable.V1))
          (Binable.to_string (module Transaction_with_witness.Stable.V1))
      in
      Staged_ledger_hash.Aux_hash.of_bytes
        (state_hash |> Digestif.SHA256.to_raw_string)
  end
end]

[%%define_locally
Stable.Latest.(hash)]

(**********Helpers*************)

let create_expected_statement ~constraint_constants
    { Transaction_with_witness.transaction_with_info
    ; state_hash
    ; state_view
    ; ledger_witness
    ; init_stack
    ; statement } =
  let open Or_error.Let_syntax in
  let source =
    Frozen_ledger_hash.of_ledger_hash
    @@ Sparse_ledger.merkle_root ledger_witness
  in
  let next_available_token_before =
    Sparse_ledger.next_available_token ledger_witness
  in
  let {With_status.data= transaction; status= _} =
    Ledger.Transaction_applied.transaction transaction_with_info
  in
  let%bind after =
    Or_error.try_with (fun () ->
        Sparse_ledger.apply_transaction_exn ~constraint_constants
          ~txn_state_view:state_view ledger_witness transaction )
  in
  let target =
    Frozen_ledger_hash.of_ledger_hash @@ Sparse_ledger.merkle_root after
  in
  let next_available_token_after = Sparse_ledger.next_available_token after in
  let%bind pending_coinbase_before =
    match init_stack with
    | Base source ->
        Ok source
    | Merge ->
        Or_error.errorf
          !"Invalid init stack in Pending coinbase stack state . Expected \
            Base found Merge"
  in
  let pending_coinbase_after =
    let state_body_hash = snd state_hash in
    let pending_coinbase_with_state =
      Pending_coinbase.Stack.push_state state_body_hash pending_coinbase_before
    in
    match transaction with
    | Coinbase c ->
        Pending_coinbase.Stack.push_coinbase c pending_coinbase_with_state
    | _ ->
        pending_coinbase_with_state
  in
  let%bind fee_excess = Transaction.fee_excess transaction in
  let%map supply_increase = Transaction.supply_increase transaction in
  { Transaction_snark.Statement.source
  ; target
  ; fee_excess
  ; next_available_token_before
  ; next_available_token_after
  ; supply_increase
  ; pending_coinbase_stack_state=
      { statement.pending_coinbase_stack_state with
        target= pending_coinbase_after }
  ; sok_digest= () }

let completed_work_to_scanable_work (job : job) (fee, current_proof, prover) :
    'a Or_error.t =
  let sok_digest = Ledger_proof.sok_digest current_proof
  and proof = Ledger_proof.underlying_proof current_proof in
  match job with
  | Base {statement; _} ->
      let ledger_proof = Ledger_proof.create ~statement ~sok_digest ~proof in
      Ok (ledger_proof, Sok_message.create ~fee ~prover)
  | Merge ((p, _), (p', _)) ->
      let s = Ledger_proof.statement p and s' = Ledger_proof.statement p' in
      let open Or_error.Let_syntax in
      let%map fee_excess = Fee_excess.combine s.fee_excess s'.fee_excess
      and supply_increase =
        Amount.add s.supply_increase s'.supply_increase
        |> option "Error adding supply_increases"
      and _valid_pending_coinbase_stack =
        if
          Pending_coinbase.Stack.equal s.pending_coinbase_stack_state.target
            s'.pending_coinbase_stack_state.source
        then Ok ()
        else Or_error.error_string "Invalid pending coinbase stack state"
      and () =
        if
          Token_id.equal s.next_available_token_after
            s'.next_available_token_before
        then Ok ()
        else
          Or_error.error_string
            "Statements have incompatible next_available_token state"
      in
      let statement : Transaction_snark.Statement.t =
        { source= s.source
        ; target= s'.target
        ; supply_increase
        ; pending_coinbase_stack_state=
            { source= s.pending_coinbase_stack_state.source
            ; target= s'.pending_coinbase_stack_state.target }
        ; fee_excess
        ; next_available_token_before= s.next_available_token_before
        ; next_available_token_after= s'.next_available_token_after
        ; sok_digest= () }
      in
      ( Ledger_proof.create ~statement ~sok_digest ~proof
      , Sok_message.create ~fee ~prover )

let total_proofs (works : Transaction_snark_work.t list) =
  List.sum (module Int) works ~f:(fun w -> One_or_two.length w.proofs)

(*************exposed functions*****************)

module P = struct
  type t = Ledger_proof_with_sok_message.t
end

module Make_statement_scanner
    (M : Monad_with_Or_error_intf) (Verifier : sig
        type t

        val verify : verifier:t -> P.t list -> bool M.Or_error.t
    end) =
struct
  module Fold = Parallel_scan.State.Make_foldable (Monad.Ident)

  let logger = lazy (Logger.create ())

  let time label f =
    let logger = Lazy.force logger in
    let open M.Let_syntax in
    let start = Core.Time.now () in
    let%map x = f () in
    [%log debug]
      ~metadata:
        [("time_elapsed", `Float Core.Time.(Span.to_ms @@ diff (now ()) start))]
      "%s took $time_elapsed" label ;
    x

  module Timer = struct
    module Info = struct
      module Time_span = struct
        type t = Time.Span.t

        let to_yojson t = `Float (Time.Span.to_ms t)
      end

      type t =
        {total: Time_span.t; count: int; min: Time_span.t; max: Time_span.t}
      [@@deriving to_yojson]

      let singleton time = {total= time; count= 1; max= time; min= time}

      let update (t : t) time =
        { total= Time.Span.( + ) t.total time
        ; count= t.count + 1
        ; min= Time.Span.min t.min time
        ; max= Time.Span.max t.max time }
    end

    type t = Info.t String.Table.t

    let create () : t = String.Table.create ()

    let time (t : t) label f =
      let start = Time.now () in
      let x = f () in
      let elapsed = Time.(diff (now ()) start) in
      Hashtbl.update t label ~f:(function
        | None ->
            Info.singleton elapsed
        | Some acc ->
            Info.update acc elapsed ) ;
      x

    let log label (t : t) =
      let logger = Lazy.force logger in
      [%log debug]
        ~metadata:
          (List.map (Hashtbl.to_alist t) ~f:(fun (k, info) ->
               (k, Info.to_yojson info) ))
        "%s timing" label
  end

  (*TODO: fold over the pending_coinbase tree and validate the statements?*)
  let scan_statement ~constraint_constants tree ~verifier :
      (Transaction_snark.Statement.t, [`Error of Error.t | `Empty]) Result.t
      M.t =
    let timer = Timer.create () in
    let module Acc = struct
      type t = (Transaction_snark.Statement.t * P.t list) option
    end in
    let write_error description =
      sprintf !"Staged_ledger.scan_statement: %s\n" description
    in
    let with_error ~f message =
      let result = f () in
      Result.map_error result ~f:(fun e ->
          Error.createf !"%s: %{sexp:Error.t}" (write_error message) e )
    in
    let merge_acc ~proofs (acc : Acc.t) s2 : Acc.t Or_error.t =
      let open Or_error.Let_syntax in
      Timer.time timer (sprintf "merge_acc:%s" __LOC__) (fun () ->
          with_error "Bad merge proof" ~f:(fun () ->
              match acc with
              | None ->
                  Ok (Some (s2, proofs))
              | Some (s1, ps) ->
                  let%map merged_statement =
                    Transaction_snark.Statement.merge s1 s2
                  in
                  Some (merged_statement, proofs @ ps) ) )
    in
    let fold_step_a acc_statement job =
      match job with
      | Parallel_scan.Merge.Job.Part (proof, message) ->
          let statement = Ledger_proof.statement proof in
          merge_acc ~proofs:[(proof, message)] acc_statement statement
      | Empty | Full {status= Parallel_scan.Job_status.Done; _} ->
          Or_error.return acc_statement
      | Full {left= proof_1, message_1; right= proof_2, message_2; _} ->
          let open Or_error.Let_syntax in
          let%bind merged_statement =
            Timer.time timer (sprintf "merge:%s" __LOC__) (fun () ->
                Transaction_snark.Statement.merge
                  (Ledger_proof.statement proof_1)
                  (Ledger_proof.statement proof_2) )
          in
          merge_acc acc_statement merged_statement
            ~proofs:[(proof_1, message_1); (proof_2, message_2)]
    in
    let fold_step_d acc_statement job =
      match job with
      | Parallel_scan.Base.Job.Empty
      | Full {status= Parallel_scan.Job_status.Done; _} ->
          Or_error.return acc_statement
      | Full {job= transaction; _} ->
          with_error "Bad base statement" ~f:(fun () ->
              let open Or_error.Let_syntax in
              let%bind expected_statement =
                Timer.time timer
                  (sprintf "create_expected_statement:%s" __LOC__) (fun () ->
                    create_expected_statement ~constraint_constants transaction
                )
              in
              if
                Transaction_snark.Statement.equal transaction.statement
                  expected_statement
              then merge_acc ~proofs:[] acc_statement transaction.statement
              else
                Or_error.error_string
                  (sprintf
                     !"Bad base statement expected: \
                       %{sexp:Transaction_snark.Statement.t} got: \
                       %{sexp:Transaction_snark.Statement.t}"
                     transaction.statement expected_statement) )
    in
    let res =
      Fold.fold_chronological_until tree ~init:None
        ~f_merge:(fun acc (_weight, job) ->
          let open Container.Continue_or_stop in
          match fold_step_a acc job with
          | Ok next ->
              Continue next
          | e ->
              Stop e )
        ~f_base:(fun acc (_weight, job) ->
          let open Container.Continue_or_stop in
          match fold_step_d acc job with
          | Ok next ->
              Continue next
          | e ->
              Stop e )
        ~finish:Result.return
    in
    Timer.log "scan_statement" timer ;
    match res with
    | Ok None ->
        M.return (Error `Empty)
    | Ok (Some (res, proofs)) -> (
        let open M.Let_syntax in
        match%map
          ksprintf time "verify:%s" __LOC__ (fun () ->
              Verifier.verify ~verifier proofs )
        with
        | Ok true ->
            Ok res
        | Ok false ->
            Error (`Error (Error.of_string "Bad proofs"))
        | Error e ->
            Error (`Error e) )
    | Error e ->
        M.return (Error (`Error e))

  let check_invariants t ~constraint_constants ~verifier ~error_prefix
      ~ledger_hash_end:current_ledger_hash
      ~ledger_hash_begin:snarked_ledger_hash
      ~next_available_token_begin:snarked_ledger_next_available_token
      ~next_available_token_end:current_ledger_next_available_token =
    let clarify_error cond err =
      if not cond then Or_error.errorf "%s : %s" error_prefix err else Ok ()
    in
    let open M.Let_syntax in
    match%map
      time "scan_statement" (fun () ->
          scan_statement ~constraint_constants ~verifier t )
    with
    | Error (`Error e) ->
        Error e
    | Error `Empty ->
        let current_ledger_hash = current_ledger_hash in
        Option.value_map ~default:(Ok ()) snarked_ledger_hash ~f:(fun hash ->
            clarify_error
              (Frozen_ledger_hash.equal hash current_ledger_hash)
              "did not connect with snarked ledger hash" )
    | Ok
        { fee_excess= {fee_token_l; fee_excess_l; fee_token_r; fee_excess_r}
        ; source
        ; target
        ; next_available_token_before
        ; next_available_token_after
        ; supply_increase= _
        ; pending_coinbase_stack_state= _ (*TODO: check pending coinbases?*)
        ; sok_digest= () } ->
        let open Or_error.Let_syntax in
        let%map () =
          Option.value_map ~default:(Ok ()) snarked_ledger_hash ~f:(fun hash ->
              clarify_error
                (Frozen_ledger_hash.equal hash source)
                "did not connect with snarked ledger hash" )
        and () =
          clarify_error
            (Frozen_ledger_hash.equal current_ledger_hash target)
            "incorrect statement target hash"
        and () =
          clarify_error
            (Fee.Signed.equal Fee.Signed.zero fee_excess_l)
            "nonzero fee excess"
        and () =
          clarify_error
            (Fee.Signed.equal Fee.Signed.zero fee_excess_r)
            "nonzero fee excess"
        and () =
          clarify_error
            (Token_id.equal Token_id.default fee_token_l)
            "nondefault fee token"
        and () =
          clarify_error
            (Token_id.equal Token_id.default fee_token_r)
            "nondefault fee token"
        and () =
          Option.value_map ~default:(Ok ()) snarked_ledger_next_available_token
            ~f:(fun next_tkn ->
              clarify_error
                Token_id.(next_available_token_before = next_tkn)
                "next available token from snarked ledger does not match" )
        and () =
          clarify_error
            Token_id.(
              next_available_token_after = current_ledger_next_available_token)
            "next available token from staged ledger does not match"
        in
        ()
end

module Staged_undos = struct
  type applied_txn = Ledger.Transaction_applied.t

  type t = applied_txn list

  let apply ~constraint_constants t ledger =
    List.fold_left t ~init:(Ok ()) ~f:(fun acc t ->
        Or_error.bind
          (Or_error.map acc ~f:(fun _ -> t))
          ~f:(fun u -> Ledger.undo ~constraint_constants ledger u) )
end

let statement_of_job : job -> Transaction_snark.Statement.t option = function
  | Base {statement; _} ->
      Some statement
  | Merge ((p1, _), (p2, _)) ->
      let stmt1 = Ledger_proof.statement p1
      and stmt2 = Ledger_proof.statement p2 in
      let open Option.Let_syntax in
      let%bind () =
        Option.some_if (Frozen_ledger_hash.equal stmt1.target stmt2.source) ()
      in
      let%map fee_excess =
        match Fee_excess.combine stmt1.fee_excess stmt2.fee_excess with
        | Ok ret ->
            Some ret
        | Error _ ->
            None
      and supply_increase =
        Currency.Amount.add stmt1.supply_increase stmt2.supply_increase
      and () =
        Option.some_if
          (Token_id.equal stmt1.next_available_token_after
             stmt2.next_available_token_before)
          ()
      in
      ( { source= stmt1.source
        ; target= stmt2.target
        ; supply_increase
        ; pending_coinbase_stack_state=
            { source= stmt1.pending_coinbase_stack_state.source
            ; target= stmt2.pending_coinbase_stack_state.target }
        ; fee_excess
        ; next_available_token_before= stmt1.next_available_token_before
        ; next_available_token_after= stmt2.next_available_token_after
        ; sok_digest= () }
        : Transaction_snark.Statement.t )

let create ~work_delay ~transaction_capacity_log_2 =
  let k = Int.pow 2 transaction_capacity_log_2 in
  Parallel_scan.empty ~delay:work_delay ~max_base_jobs:k

let empty ~(constraint_constants : Genesis_constants.Constraint_constants.t) ()
    =
  create ~work_delay:constraint_constants.work_delay
    ~transaction_capacity_log_2:constraint_constants.transaction_capacity_log_2

let extract_txns txns_with_witnesses =
  (* TODO: This type checks, but are we actually pulling the inverse txn here? *)
  List.map txns_with_witnesses
    ~f:(fun (txn_with_witness : Transaction_with_witness.t) ->
      let txn =
        Ledger.Transaction_applied.transaction
          txn_with_witness.transaction_with_info
      in
      let state_hash = fst txn_with_witness.state_hash in
      (txn, state_hash) )

let latest_ledger_proof t =
  let open Option.Let_syntax in
  let%map proof, txns_with_witnesses = Parallel_scan.last_emitted_value t in
  (proof, extract_txns txns_with_witnesses)

let free_space = Parallel_scan.free_space

(*This needs to be grouped like in work_to_do function. Group of two jobs per list and not group of two jobs after concatenating the lists*)
let all_jobs = Parallel_scan.all_jobs

let next_on_new_tree = Parallel_scan.next_on_new_tree

let base_jobs_on_latest_tree = Parallel_scan.base_jobs_on_latest_tree

(* TODO: make this operation O(1) -- gets used in Sync_handler RPC, which is performance sensitive (#3733) *)
let target_merkle_root t =
  let open Transaction_with_witness in
  let open Option.Let_syntax in
  let%map last_item = List.last (Parallel_scan.pending_data t) in
  last_item.statement.target

(*All the transactions in the order in which they were applied*)
let staged_transactions t =
  List.map ~f:(fun (t : Transaction_with_witness.t) ->
      t.transaction_with_info |> Ledger.Transaction_applied.transaction )
  @@ Parallel_scan.pending_data t

let staged_transactions_with_protocol_states t
    ~(get_state : State_hash.t -> Mina_state.Protocol_state.value Or_error.t) =
  let open Or_error.Let_syntax in
  List.map ~f:(fun (t : Transaction_with_witness.t) ->
      let txn =
        t.transaction_with_info |> Ledger.Transaction_applied.transaction
      in
      let%map protocol_state = get_state (fst t.state_hash) in
      (txn, protocol_state) )
  @@ Parallel_scan.pending_data t
  |> Or_error.all

(*All the staged transactions in the reverse order of their application (Latest first)*)
let staged_undos t : Staged_undos.t =
  List.map
    (Parallel_scan.pending_data t |> List.rev)
    ~f:(fun (t : Transaction_with_witness.t) -> t.transaction_with_info)

let partition_if_overflowing t =
  let bundle_count work_count = (work_count + 1) / 2 in
  let {Space_partition.first= slots, job_count; second} =
    Parallel_scan.partition_if_overflowing t
  in
  { Space_partition.first= (slots, bundle_count job_count)
  ; second=
      Option.map second ~f:(fun (slots, job_count) ->
          (slots, bundle_count job_count) ) }

let extract_from_job (job : job) =
  match job with
  | Parallel_scan.Available_job.Base d ->
      First
        ( d.transaction_with_info
        , d.statement
        , d.state_hash
        , d.ledger_witness
        , d.init_stack )
  | Merge ((p1, _), (p2, _)) ->
      Second (p1, p2)

let snark_job_list_json t =
  let all_jobs : Job_view.t list list =
    let fa (a : Ledger_proof_with_sok_message.t) =
      Ledger_proof.statement (fst a)
    in
    let fd (d : Transaction_with_witness.t) = d.statement in
    Parallel_scan.view_jobs_with_position t fa fd
  in
  Yojson.Safe.to_string
    (`List
      (List.map all_jobs ~f:(fun tree ->
           `List (List.map tree ~f:Job_view.to_yojson) )))

(*Always the same pairing of jobs*)
let all_work_statements_exn t : Transaction_snark_work.Statement.t list =
  let work_seqs = all_jobs t in
  List.concat_map work_seqs ~f:(fun work_seq ->
      One_or_two.group_list
        (List.map work_seq ~f:(fun job ->
             match statement_of_job job with
             | None ->
                 assert false
             | Some stmt ->
                 stmt )) )

let required_work_pairs t ~slots =
  let work_list = Parallel_scan.jobs_for_slots t ~slots in
  List.concat_map work_list ~f:(fun works -> One_or_two.group_list works)

let k_work_pairs_for_new_diff t ~k =
  let work_list = Parallel_scan.jobs_for_next_update t in
  List.(
    take (concat_map work_list ~f:(fun works -> One_or_two.group_list works)) k)

(*Always the same pairing of jobs*)
let work_statements_for_new_diff t : Transaction_snark_work.Statement.t list =
  let work_list = Parallel_scan.jobs_for_next_update t in
  List.concat_map work_list ~f:(fun work_seq ->
      One_or_two.group_list
        (List.map work_seq ~f:(fun job ->
             match statement_of_job job with
             | None ->
                 assert false
             | Some stmt ->
                 stmt )) )

let all_work_pairs t
    ~(get_state : State_hash.t -> Mina_state.Protocol_state.value Or_error.t) :
    ( Transaction.t
    , Transaction_witness.t
    , Ledger_proof.t )
    Snark_work_lib.Work.Single.Spec.t
    One_or_two.t
    list
    Or_error.t =
  let all_jobs = all_jobs t in
  let module A = Available_job in
  let open Or_error.Let_syntax in
  let single_spec (job : job) =
    match extract_from_job job with
    | First
        ( transaction_with_info
        , statement
        , state_hash
        , ledger_witness
        , init_stack ) ->
        let {With_status.data= transaction; status} =
          Ledger.Transaction_applied.transaction transaction_with_info
        in
        let%bind protocol_state_body =
          let%map state = get_state (fst state_hash) in
          Mina_state.Protocol_state.body state
        in
        let%map init_stack =
          match init_stack with
          | Base x ->
              Ok x
          | Merge ->
              Or_error.error_string "init_stack was Merge"
        in
        Snark_work_lib.Work.Single.Spec.Transition
          ( statement
          , transaction
          , { Transaction_witness.ledger= ledger_witness
            ; protocol_state_body
            ; init_stack
            ; status } )
    | Second (p1, p2) ->
        let%map merged =
          Transaction_snark.Statement.merge
            (Ledger_proof.statement p1)
            (Ledger_proof.statement p2)
        in
        Snark_work_lib.Work.Single.Spec.Merge (merged, p1, p2)
  in
  List.fold_until all_jobs ~init:[]
    ~finish:(fun lst -> Ok lst)
    ~f:(fun acc jobs ->
      let specs_list : 'a One_or_two.t list Or_error.t =
        List.fold ~init:(Ok []) (One_or_two.group_list jobs)
          ~f:(fun acc' pair ->
            let%bind acc' = acc' in
            let%map spec = One_or_two.Or_error.map ~f:single_spec pair in
            spec :: acc' )
      in
      match specs_list with
      | Ok list ->
          Continue (acc @ List.rev list)
      | Error e ->
          Stop (Error e) )

let update_metrics = Parallel_scan.update_metrics

let fill_work_and_enqueue_transactions t transactions work =
  let open Or_error.Let_syntax in
  let fill_in_transaction_snark_work t (works : Transaction_snark_work.t list)
      : (Ledger_proof.t * Sok_message.t) list Or_error.t =
    let next_jobs =
      List.(
        take
          (concat @@ Parallel_scan.jobs_for_next_update t)
          (total_proofs works))
    in
    map2_or_error next_jobs
      (List.concat_map works
         ~f:(fun {Transaction_snark_work.fee; proofs; prover} ->
           One_or_two.map proofs ~f:(fun proof -> (fee, proof, prover))
           |> One_or_two.to_list ))
      ~f:completed_work_to_scanable_work
  in
  let old_proof = Parallel_scan.last_emitted_value t in
  let%bind work_list = fill_in_transaction_snark_work t work in
  let%bind proof_opt, updated_scan_state =
    Parallel_scan.update t ~completed_jobs:work_list ~data:transactions
  in
  let%map result_opt =
    Option.value_map ~default:(Ok None) proof_opt
      ~f:(fun ((proof, _), txns_with_witnesses) ->
        let curr_source = (Ledger_proof.statement proof).source in
        (*TODO: get genesis ledger hash if the old_proof is none*)
        let prev_target =
          Option.value_map ~default:curr_source old_proof
            ~f:(fun ((p', _), _) -> (Ledger_proof.statement p').target)
        in
        if Frozen_ledger_hash.equal curr_source prev_target then
          Ok (Some (proof, extract_txns txns_with_witnesses))
        else Or_error.error_string "Unexpected ledger proof emitted" )
  in
  (result_opt, updated_scan_state)

let required_state_hashes t =
  List.fold ~init:State_hash.Set.empty
    ~f:(fun acc (t : Transaction_with_witness.t) ->
      Set.add acc (fst t.state_hash) )
    (Parallel_scan.pending_data t)

let check_required_protocol_states t ~protocol_states =
  let open Or_error.Let_syntax in
  let required_state_hashes = required_state_hashes t in
  let check_length states =
    let required = State_hash.Set.length required_state_hashes in
    let received = List.length states in
    if required = received then Or_error.return ()
    else
      Or_error.errorf
        !"Required %d protocol states but received %d"
        required received
  in
  (*Don't check further if the lengths dont match*)
  let%bind () = check_length protocol_states in
  let received_state_map =
    List.fold protocol_states ~init:Mina_base.State_hash.Map.empty
      ~f:(fun m ps ->
        State_hash.Map.set m ~key:(Mina_state.Protocol_state.hash ps) ~data:ps
    )
  in
  let protocol_states_assoc =
    List.filter_map (State_hash.Set.to_list required_state_hashes)
      ~f:(fun hash ->
        let open Option.Let_syntax in
        let%map state = State_hash.Map.find received_state_map hash in
        (hash, state) )
  in
  let%map () = check_length protocol_states_assoc in
  protocol_states_assoc
