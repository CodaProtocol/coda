(** This module glues together the various components that compose
*  the transition frontier, wrapping high-level initialization
*  logic as well as gluing together the logic for adding items
*  to the frontier *)
open Core

open Async_kernel
open Mina_base
open Mina_transition
include Frontier_base
module Full_frontier = Full_frontier
module Extensions = Extensions
module Persistent_root = Persistent_root
module Persistent_frontier = Persistent_frontier
module Catchup_tree = Catchup_tree
module Full_catchup_tree = Full_catchup_tree
module Catchup_hash_tree = Catchup_hash_tree

let max_catchup_chunk_length = 20

let global_max_length (genesis_constants : Genesis_constants.t) =
  genesis_constants.protocol.k

type t =
  { logger: Logger.t
  ; verifier: Verifier.t
  ; consensus_local_state: Consensus.Data.Local_state.t
  ; catchup_tree: Catchup_tree.t
  ; full_frontier: Full_frontier.t
  ; persistent_root: Persistent_root.t
  ; persistent_root_instance: Persistent_root.Instance.t
  ; persistent_frontier: Persistent_frontier.t
  ; persistent_frontier_instance: Persistent_frontier.Instance.t
  ; extensions: Extensions.t
  ; genesis_state_hash: State_hash.t
  ; closed: unit Ivar.t }

let catchup_tree t = t.catchup_tree

type Structured_log_events.t += Added_breadcrumb_user_commands
  [@@deriving register_event]

(* There is no Diff.Full.E.of_yojson, so we store raw Yojson.Safe.t here so that
 * we can still deserialize something to inspect *)
type Structured_log_events.t += Applying_diffs of {diffs: Yojson.Safe.t list}
  [@@deriving register_event {msg= "Applying diffs: $diffs"}]

let genesis_root_data ~precomputed_values =
  let open Root_data.Limited in
  let transition = External_transition.genesis ~precomputed_values in
  let constraint_constants = precomputed_values.constraint_constants in
  let scan_state = Staged_ledger.Scan_state.empty ~constraint_constants () in
  (*if scan state is empty the protocol states required is also empty*)
  let protocol_states = [] in
  let pending_coinbase =
    Or_error.ok_exn
      (Pending_coinbase.create
         ~depth:constraint_constants.pending_coinbase_depth ())
  in
  create ~transition ~scan_state ~pending_coinbase ~protocol_states

let load_from_persistence_and_start ~logger ~verifier ~consensus_local_state
    ~max_length ~persistent_root ~persistent_root_instance ~persistent_frontier
    ~persistent_frontier_instance ~precomputed_values ~catchup_mode
    ignore_consensus_local_state =
  let open Deferred.Result.Let_syntax in
  let root_identifier =
    match
      Persistent_root.Instance.load_root_identifier persistent_root_instance
    with
    | Some root_identifier ->
        root_identifier
    | None ->
        failwith
          "no persistent root identifier found (should have been written \
           already)"
  in
  let%bind () =
    Deferred.return
      ( match
          Persistent_frontier.Instance.fast_forward
            persistent_frontier_instance root_identifier
        with
      | Ok () ->
          [%log info] "Fast forward successful" ;
          Ok ()
      | Error `Sync_cannot_be_running ->
          Error (`Failure "sync job is already running on persistent frontier")
      | Error `Bootstrap_required ->
          Error `Bootstrap_required
      | Error (`Failure msg) ->
          [%log fatal]
            ~metadata:
              [("target_root", Root_identifier.to_yojson root_identifier)]
            "Unable to fast forward persistent frontier: %s" msg ;
          Error (`Failure msg) )
  in
  let%bind full_frontier, extensions =
    Deferred.map
      (Persistent_frontier.Instance.load_full_frontier
         persistent_frontier_instance ~max_length
         ~root_ledger:
           (Persistent_root.Instance.snarked_ledger persistent_root_instance)
         ~consensus_local_state ~ignore_consensus_local_state
         ~precomputed_values)
      ~f:
        (Result.map_error ~f:(function
          | `Sync_cannot_be_running ->
              `Failure "sync job is already running on persistent frontier"
          | `Failure _ as err ->
              err ))
  in
  [%log info] "Loaded full frontier and extensions" ;
  let%map () =
    Deferred.return
      ( Persistent_frontier.Instance.start_sync
          ~constraint_constants:precomputed_values.constraint_constants
          persistent_frontier_instance
      |> Result.map_error ~f:(function
           | `Sync_cannot_be_running ->
               `Failure "sync job is already running on persistent frontier"
           | `Not_found _ as err ->
               `Failure
                 (Persistent_frontier.Database.Error.not_found_message err) )
      )
  in
  { logger
  ; catchup_tree=
      Catchup_tree.create catchup_mode ~root:(Full_frontier.root full_frontier)
  ; verifier
  ; consensus_local_state
  ; full_frontier
  ; persistent_root
  ; persistent_root_instance
  ; persistent_frontier
  ; persistent_frontier_instance
  ; extensions
  ; closed= Ivar.create ()
  ; genesis_state_hash=
      Precomputed_values.genesis_state_hash precomputed_values }

let rec load_with_max_length :
       max_length:int
    -> ?retry_with_fresh_db:bool
    -> logger:Logger.t
    -> verifier:Verifier.t
    -> consensus_local_state:Consensus.Data.Local_state.t
    -> persistent_root:Persistent_root.t
    -> persistent_frontier:Persistent_frontier.t
    -> precomputed_values:Precomputed_values.t
    -> catchup_mode:[`Normal | `Super]
    -> unit
    -> ( t
       , [> `Bootstrap_required
         | `Persistent_frontier_malformed
         | `Failure of string ] )
       Deferred.Result.t =
 fun ~max_length ?(retry_with_fresh_db = true) ~logger ~verifier
     ~consensus_local_state ~persistent_root ~persistent_frontier
     ~precomputed_values ~catchup_mode () ->
  let open Deferred.Let_syntax in
  (* TODO: #3053 *)
  let continue persistent_frontier_instance ~ignore_consensus_local_state =
    let persistent_root_instance =
      Persistent_root.create_instance_exn persistent_root
    in
    match%bind
      load_from_persistence_and_start ~logger ~verifier ~consensus_local_state
        ~max_length ~persistent_root ~persistent_root_instance ~catchup_mode
        ~persistent_frontier ~persistent_frontier_instance ~precomputed_values
        ignore_consensus_local_state
    with
    | Ok _ as result ->
        return result
    | Error _ as err ->
        let%map () =
          Persistent_frontier.Instance.destroy persistent_frontier_instance
        in
        Persistent_root.Instance.destroy persistent_root_instance ;
        err
  in
  let persistent_frontier_instance =
    Persistent_frontier.create_instance_exn persistent_frontier
  in
  let reset_and_continue ?(destroy_frontier_instance = true) () =
    let%bind () =
      if destroy_frontier_instance then
        Persistent_frontier.Instance.destroy persistent_frontier_instance
      else return ()
    in
    let%bind () =
      Persistent_frontier.reset_database_exn persistent_frontier
        ~root_data:(genesis_root_data ~precomputed_values)
        ~genesis_state_hash:precomputed_values.protocol_state_with_hash.hash
    in
    let%bind () =
      Persistent_root.reset_to_genesis_exn persistent_root ~precomputed_values
    in
    continue
      (Persistent_frontier.create_instance_exn persistent_frontier)
      ~ignore_consensus_local_state:false
  in
  match
    Persistent_frontier.Instance.check_database
      ~genesis_state_hash:precomputed_values.protocol_state_with_hash.hash
      persistent_frontier_instance
  with
  | Error `Not_initialized ->
      (* TODO: this case can be optimized to not create the
         * database twice through rocks -- currently on clean bootup,
         * this code path will reinitialize the rocksdb twice *)
      [%log info] "persistent frontier database does not exist" ;
      reset_and_continue ()
  | Error `Invalid_version ->
      [%log info] "persistent frontier database out of date" ;
      reset_and_continue ()
  | Error (`Genesis_state_mismatch persisted_genesis_state_hash) ->
      [%log info]
        "Genesis state in persisted frontier $persisted_state_hash differs \
         from the current genesis state $precomputed_state_hash"
        ~metadata:
          [ ( "persisted_state_hash"
            , State_hash.to_yojson persisted_genesis_state_hash )
          ; ( "precomputed_state_hash"
            , State_hash.to_yojson
                precomputed_values.protocol_state_with_hash.hash ) ] ;
      reset_and_continue ()
  | Error (`Corrupt err) ->
      [%log error] "Persistent frontier database is corrupt: %s"
        (Persistent_frontier.Database.Error.message err) ;
      if retry_with_fresh_db then (
        (* should retry be on by default? this could be unnecessarily destructive *)
        [%log info] "destroying old persistent frontier database " ;
        let%bind () =
          Persistent_frontier.Instance.destroy persistent_frontier_instance
        in
        let%bind () =
          Persistent_frontier.destroy_database_exn persistent_frontier
        in
        load_with_max_length ~max_length ~logger ~verifier
          ~consensus_local_state ~persistent_root ~persistent_frontier
          ~retry_with_fresh_db:false ~precomputed_values ~catchup_mode ()
        >>| Result.map_error ~f:(function
              | `Persistent_frontier_malformed ->
                  `Failure
                    "failed to destroy and create new persistent frontier \
                     database"
              | err ->
                  err ) )
      else return (Error `Persistent_frontier_malformed)
  | Ok () -> (
      match%bind
        continue persistent_frontier_instance
          ~ignore_consensus_local_state:true
      with
      | Error (`Failure err) when retry_with_fresh_db ->
          [%log error]
            "Failed to initialize transition frontier: $err. Destroying old \
             persistent frontier database and retrying."
            ~metadata:[("err", `String err)] ;
          (* The frontier instance is already destroyed by [continue] before it
             returns an [Error], don't attempt to do it again.
          *)
          reset_and_continue ~destroy_frontier_instance:false ()
      | res ->
          return res )

let load ?(retry_with_fresh_db = true) ~logger ~verifier ~consensus_local_state
    ~persistent_root ~persistent_frontier ~precomputed_values ~catchup_mode ()
    =
  let max_length =
    global_max_length (Precomputed_values.genesis_constants precomputed_values)
  in
  load_with_max_length ~max_length ~retry_with_fresh_db ~logger ~verifier
    ~consensus_local_state ~persistent_root ~persistent_frontier
    ~precomputed_values ~catchup_mode ()

(* The persistent root and persistent frontier as safe to ignore here
 * because their lifecycle is longer than the transition frontier's *)
let close ~loc
    { logger
    ; verifier= _
    ; consensus_local_state= _
    ; catchup_tree= _
    ; full_frontier
    ; persistent_root= _safe_to_ignore_1
    ; persistent_root_instance
    ; persistent_frontier= _safe_to_ignore_2
    ; persistent_frontier_instance
    ; extensions
    ; closed
    ; genesis_state_hash= _ } =
  [%log debug] "Closing transition frontier" ;
  Full_frontier.close ~loc full_frontier ;
  Extensions.close extensions ;
  let%map () =
    Persistent_frontier.Instance.destroy persistent_frontier_instance
  in
  Persistent_root.Instance.destroy persistent_root_instance ;
  Ivar.fill_if_empty closed ()

let closed t = Ivar.read t.closed

let persistent_root {persistent_root; _} = persistent_root

let persistent_frontier {persistent_frontier; _} = persistent_frontier

let extensions {extensions; _} = extensions

let genesis_state_hash {genesis_state_hash; _} = genesis_state_hash

let root_snarked_ledger {persistent_root_instance; _} =
  Persistent_root.Instance.snarked_ledger persistent_root_instance

let add_breadcrumb_exn t breadcrumb =
  let open Deferred.Let_syntax in
  let diffs = Full_frontier.calculate_diffs t.full_frontier breadcrumb in
  [%log' trace t.logger]
    ~metadata:
      [ ( "state_hash"
        , State_hash.to_yojson
            (Breadcrumb.state_hash (Full_frontier.best_tip t.full_frontier)) )
      ; ( "n"
        , `Int (List.length @@ Full_frontier.all_breadcrumbs t.full_frontier)
        ) ]
    "PRE: ($state_hash, $n)" ;
  [%str_log' trace t.logger]
    (Applying_diffs {diffs= List.map ~f:Diff.Full.E.to_yojson diffs}) ;
  Catchup_tree.apply_diffs t.catchup_tree diffs ;
  let (`New_root_and_diffs_with_mutants
        (new_root_identifier, diffs_with_mutants)) =
    (* Root DB moves here *)
    Full_frontier.apply_diffs t.full_frontier diffs
      ~has_long_catchup_job:
        (Catchup_tree.max_catchup_chain_length t.catchup_tree > 5)
      ~enable_epoch_ledger_sync:(`Enabled (root_snarked_ledger t))
  in
  Option.iter new_root_identifier
    ~f:
      (Persistent_root.Instance.set_root_identifier t.persistent_root_instance) ;
  [%log' trace t.logger]
    ~metadata:
      [ ( "state_hash"
        , State_hash.to_yojson
            (Breadcrumb.state_hash @@ Full_frontier.best_tip t.full_frontier)
        )
      ; ( "n"
        , `Int (List.length @@ Full_frontier.all_breadcrumbs t.full_frontier)
        ) ]
    "POST: ($state_hash, $n)" ;
  let user_cmds = Breadcrumb.commands breadcrumb in
  if not (List.is_empty user_cmds) then
    (* N.B.: surprisingly, the JSON does not contain a tag indicating whether we have a signed
       command or snapp command
    *)
    [%str_log' trace t.logger] Added_breadcrumb_user_commands
      ~metadata:
        [ ( "user_commands"
          , `List
              (List.map user_cmds
                 ~f:(With_status.to_yojson User_command.Valid.to_yojson)) ) ] ;
  let lite_diffs =
    List.map diffs ~f:Diff.(fun (Full.E.E diff) -> Lite.E.E (to_lite diff))
  in
  let%bind sync_result =
    (* Diffs get put into a buffer here. They're processed asynchronously, except for root transitions *)
    Persistent_frontier.Instance.notify_sync t.persistent_frontier_instance
      ~diffs:lite_diffs
  in
  sync_result
  |> Result.map_error ~f:(fun `Sync_must_be_running ->
         Failure
           "Cannot add breadcrumb because persistent frontier sync job is not \
            running, which indicates that transition frontier initialization \
            has not been performed correctly" )
  |> Result.ok_exn ;
  Extensions.notify t.extensions ~frontier:t.full_frontier ~diffs_with_mutants

(* proxy full frontier functions *)
include struct
  open Full_frontier

  let proxy1 f {full_frontier; _} = f full_frontier

  let max_length = proxy1 max_length

  let consensus_local_state = proxy1 consensus_local_state

  let all_breadcrumbs = proxy1 all_breadcrumbs

  let visualize ~filename = proxy1 (visualize ~filename)

  let visualize_to_string = proxy1 visualize_to_string

  let iter = proxy1 iter

  let common_ancestor = proxy1 common_ancestor

  (* reduce sucessors functions (probably remove hashes special case *)
  let successors = proxy1 successors

  let successors_rec = proxy1 successors_rec

  let successor_hashes = proxy1 successor_hashes

  let successor_hashes_rec = proxy1 successor_hashes_rec

  let hash_path = proxy1 hash_path

  let best_tip = proxy1 best_tip

  let root = proxy1 root

  let find = proxy1 find

  let precomputed_values = proxy1 precomputed_values

  let genesis_constants = proxy1 genesis_constants

  (* TODO: find -> option externally, find_exn internally *)
  let find_exn = proxy1 find_exn

  (* TODO: is this an abstraction leak? *)
  let root_length = proxy1 root_length

  (* TODO: probably shouldn't be an `_exn` function *)
  let best_tip_path ?max_length = proxy1 (best_tip_path ?max_length)

  let best_tip_path_length_exn = proxy1 best_tip_path_length_exn

  let find_protocol_state = proxy1 find_protocol_state

  (* why can't this one be proxied? *)
  let path_map ?max_length {full_frontier; _} breadcrumb ~f =
    path_map ?max_length full_frontier breadcrumb ~f
end

module For_tests = struct
  open Signature_lib
  module Ledger_transfer = Ledger_transfer.Make (Ledger) (Ledger.Db)
  open Full_frontier.For_tests

  let proxy2 f {full_frontier= x; _} {full_frontier= y; _} = f x y

  let equal = proxy2 equal

  let load_with_max_length = load_with_max_length

  let rec deferred_rose_tree_iter (Rose_tree.T (root, trees)) ~f =
    let%bind () = f root in
    Deferred.List.iter trees ~f:(deferred_rose_tree_iter ~f)

  (*
  let with_frontier_from_rose_tree (Rose_tree.T (root, trees)) ~logger ~verifier ~consensus_local_state ~max_length ~root_snarked_ledger ~f =
    with_temp_persistence ~f:(fun ~persistent_root ~persistent_frontier ->
      Persistent_root.with_instance_exn persistent_root ~f:(fun instance ->
        Persistent_root.Instance.set_root_state_hash instance (Breadcrumb.state_hash @@ root);
        ignore @@ Ledger_transfer.transfer_accounts
          ~src:root_snarked_ledger
          ~dest:(Persistent_root.snarked_ledger instance));
      let frontier =
        let fail msg = failwith ("failed to load transition frontier: "^msg) in
        load_with_max_length
          {logger; verifier; consensus_local_state}
          ~persistent_root ~persistent_frontier
          ~max_length
        >>| Result.map_error ~f:(Fn.compose fail (function
          | `Bootstrap_required -> "bootstrap required"
          | `Persistent_frontier_malformed -> "persistent frontier malformed"
          | `Faliure msg -> msg))
        >>| Result.ok_or_failwith
      in
      let%bind () = Deferred.List.iter trees ~f:(deferred_rose_tree_iter ~f:(add_breadcrumb_exn frontier)) in
      f frontier)
  *)

  (* a helper quickcheck generator which always returns the genesis breadcrumb *)
  let gen_genesis_breadcrumb ?(logger = Logger.null ()) ~verifier
      ~(precomputed_values : Precomputed_values.t) () =
    let constraint_constants = precomputed_values.constraint_constants in
    Quickcheck.Generator.create (fun ~size:_ ~random:_ ->
        let transition_receipt_time = Some (Time.now ()) in
        let genesis_transition =
          External_transition.For_tests.genesis ~precomputed_values
        in
        let genesis_ledger =
          Lazy.force (Precomputed_values.genesis_ledger precomputed_values)
        in
        (*scan state is empty so no protocol state should be required*)
        let get_state hash =
          Or_error.errorf
            !"Protocol state (for scan state transactions) for \
              %{sexp:State_hash.t} not found"
            hash
        in
        let genesis_staged_ledger =
          Or_error.ok_exn
            (Async.Thread_safe.block_on_async_exn (fun () ->
                 Staged_ledger
                 .of_scan_state_pending_coinbases_and_snarked_ledger ~logger
                   ~verifier ~constraint_constants
                   ~scan_state:
                     (Staged_ledger.Scan_state.empty ~constraint_constants ())
                   ~get_state
                   ~pending_coinbases:
                     ( Or_error.ok_exn
                     @@ Pending_coinbase.create
                          ~depth:constraint_constants.pending_coinbase_depth ()
                     )
                   ~snarked_ledger:genesis_ledger
                   ~snarked_local_state:Mina_state.Local_state.empty
                   ~expected_merkle_root:(Ledger.merkle_root genesis_ledger) ))
        in
        Breadcrumb.create ~validated_transition:genesis_transition
          ~staged_ledger:genesis_staged_ledger ~just_emitted_a_proof:false
          ~transition_receipt_time )

  let gen_persistence ?(logger = Logger.null ()) ~verifier
      ~(precomputed_values : Precomputed_values.t) () =
    let open Core in
    let root_dir = "/tmp/coda_unit_test" in
    Quickcheck.Generator.create (fun ~size:_ ~random:_ ->
        let uuid = Uuid_unix.create () in
        let temp_dir = root_dir ^/ Uuid.to_string uuid in
        let root_dir = temp_dir ^/ "root" in
        let frontier_dir = temp_dir ^/ "frontier" in
        let cleaned = ref false in
        let clean_temp_dirs _ =
          if not !cleaned then (
            let process_info =
              Unix.create_process ~prog:"rm" ~args:["-rf"; temp_dir]
            in
            Unix.waitpid process_info.pid
            |> Result.map_error ~f:(function
                 | `Exit_non_zero n ->
                     Printf.sprintf "error (exit code %d)" n
                 | `Signal _ ->
                     "error (received unexpected signal)" )
            |> Result.ok_or_failwith ;
            cleaned := true )
        in
        Unix.mkdir_p temp_dir ;
        Unix.mkdir root_dir ;
        Unix.mkdir frontier_dir ;
        let persistent_root =
          Persistent_root.create ~logger ~directory:root_dir
            ~ledger_depth:precomputed_values.constraint_constants.ledger_depth
        in
        let persistent_frontier =
          Persistent_frontier.create ~logger ~verifier
            ~time_controller:(Block_time.Controller.basic ~logger)
            ~directory:frontier_dir
        in
        Gc.Expert.add_finalizer_exn persistent_root clean_temp_dirs ;
        Gc.Expert.add_finalizer_exn persistent_frontier (fun x ->
            Option.iter
              persistent_frontier.Persistent_frontier.Factory_type.instance
              ~f:(fun instance ->
                Persistent_frontier.Database.close instance.db ) ;
            Option.iter persistent_root.Persistent_root.Factory_type.instance
              ~f:(fun instance -> Ledger.Db.close instance.snarked_ledger) ;
            clean_temp_dirs x ) ;
        (persistent_root, persistent_frontier) )

  let gen_genesis_breadcrumb_with_protocol_states ~logger ~verifier
      ~precomputed_values () =
    let open Quickcheck.Generator.Let_syntax in
    let%map root =
      gen_genesis_breadcrumb ~logger ~verifier ~precomputed_values ()
    in
    (* List of protocol states required to prove transactions in the scan state; empty scan state at genesis*)
    let protocol_states = [] in
    (root, protocol_states)

  let gen ?(logger = Logger.null ()) ~verifier ?trust_system
      ?consensus_local_state ~precomputed_values
      ?(root_ledger_and_accounts =
        ( Lazy.force (Precomputed_values.genesis_ledger precomputed_values)
        , Lazy.force (Precomputed_values.accounts precomputed_values) ))
      ?(gen_root_breadcrumb =
        gen_genesis_breadcrumb_with_protocol_states ~logger ~verifier
          ~precomputed_values ()) ~max_length ~size () =
    let open Quickcheck.Generator.Let_syntax in
    let trust_system =
      Option.value trust_system ~default:(Trust_system.null ())
    in
    let epoch_ledger_location =
      Filename.temp_dir_name ^/ "epoch_ledger"
      ^ (Uuid_unix.create () |> Uuid.to_string)
    in
    let consensus_local_state =
      Option.value consensus_local_state
        ~default:
          (Consensus.Data.Local_state.create
             ~genesis_ledger:
               (Precomputed_values.genesis_ledger precomputed_values)
             ~genesis_epoch_data:precomputed_values.genesis_epoch_data
             ~epoch_ledger_location Public_key.Compressed.Set.empty
             ~ledger_depth:precomputed_values.constraint_constants.ledger_depth
             ~genesis_state_hash:
               (With_hash.hash precomputed_values.protocol_state_with_hash))
    in
    let root_snarked_ledger, root_ledger_accounts = root_ledger_and_accounts in
    (* TODO: ensure that rose_tree cannot be longer than k *)
    let%bind root, branches, protocol_states =
      let%bind root, protocol_states = gen_root_breadcrumb in
      let%map (Rose_tree.T (root, branches)) =
        Quickcheck.Generator.with_size ~size
          (Quickcheck_lib.gen_imperative_rose_tree
             (Quickcheck.Generator.return root)
             (Breadcrumb.For_tests.gen_non_deferred ~logger ~precomputed_values
                ~verifier ~trust_system
                ~accounts_with_secret_keys:root_ledger_accounts))
      in
      (root, branches, protocol_states)
    in
    let root_data =
      Root_data.Limited.create
        ~transition:(Breadcrumb.validated_transition root)
        ~scan_state:(Breadcrumb.staged_ledger root |> Staged_ledger.scan_state)
        ~pending_coinbase:
          ( Breadcrumb.staged_ledger root
          |> Staged_ledger.pending_coinbase_collection )
        ~protocol_states
    in
    let%map persistent_root, persistent_frontier =
      gen_persistence ~logger ~precomputed_values ~verifier ()
    in
    Async.Thread_safe.block_on_async_exn (fun () ->
        Persistent_frontier.reset_database_exn persistent_frontier ~root_data
          ~genesis_state_hash:precomputed_values.protocol_state_with_hash.hash
    ) ;
    Persistent_root.with_instance_exn persistent_root ~f:(fun instance ->
        Persistent_root.Instance.set_root_state_hash instance
          (External_transition.Validated.state_hash
             (Root_data.Limited.transition root_data)) ;
        ignore
        @@ Ledger_transfer.transfer_accounts ~src:root_snarked_ledger
             ~dest:(Persistent_root.Instance.snarked_ledger instance) ) ;
    let frontier_result =
      Async.Thread_safe.block_on_async_exn (fun () ->
          load_with_max_length ~max_length ~retry_with_fresh_db:false ~logger
            ~verifier ~consensus_local_state ~persistent_root
            ~catchup_mode:`Normal ~persistent_frontier ~precomputed_values ()
      )
    in
    let frontier =
      let fail msg = failwith ("failed to load transition frontier: " ^ msg) in
      match frontier_result with
      | Error `Bootstrap_required ->
          fail "bootstrap required"
      | Error `Persistent_frontier_malformed ->
          fail "persistent frontier malformed"
      | Error (`Failure msg) ->
          fail msg
      | Ok frontier ->
          frontier
    in
    Async.Thread_safe.block_on_async_exn (fun () ->
        Deferred.List.iter ~how:`Sequential branches
          ~f:(deferred_rose_tree_iter ~f:(add_breadcrumb_exn frontier)) ) ;
    Core.Gc.Expert.add_finalizer_exn consensus_local_state
      (fun consensus_local_state ->
        Consensus.Data.Local_state.(
          Snapshot.Ledger_snapshot.close
          @@ staking_epoch_ledger consensus_local_state) ;
        Consensus.Data.Local_state.(
          Snapshot.Ledger_snapshot.close
          @@ next_epoch_ledger consensus_local_state) ) ;
    frontier

  let gen_with_branch ?logger ~verifier ?trust_system ?consensus_local_state
      ~precomputed_values
      ?(root_ledger_and_accounts =
        ( Lazy.force (Precomputed_values.genesis_ledger precomputed_values)
        , Lazy.force (Precomputed_values.accounts precomputed_values) ))
      ?gen_root_breadcrumb ?(get_branch_root = root) ~max_length ~frontier_size
      ~branch_size () =
    let open Quickcheck.Generator.Let_syntax in
    let%bind frontier =
      gen ?logger ~verifier ?trust_system ?consensus_local_state
        ~precomputed_values ?gen_root_breadcrumb ~root_ledger_and_accounts
        ~max_length ~size:frontier_size ()
    in
    let%map make_branch =
      Breadcrumb.For_tests.gen_seq ?logger ~precomputed_values ~verifier
        ?trust_system
        ~accounts_with_secret_keys:(snd root_ledger_and_accounts)
        branch_size
    in
    let branch =
      Async.Thread_safe.block_on_async_exn (fun () ->
          make_branch (get_branch_root frontier) )
    in
    (frontier, branch)
end
