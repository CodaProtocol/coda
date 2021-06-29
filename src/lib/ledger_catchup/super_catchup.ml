open Core
open Async
open Cache_lib
open Pipe_lib
open Mina_numbers
open Mina_base
open Mina_transition
open Network_peer

(** [Ledger_catchup] is a procedure that connects a foreign external transition
    into a transition frontier by requesting a path of external_transitions
    from its peer. It receives the state_hash to catchup from
    [Catchup_scheduler]. With that state_hash, it will ask its peers for
    a merkle path/list from their oldest transition to the state_hash it is
    asking for. Upon receiving the merkle path/list, it will do the following:

    1. verify the merkle path/list is correct by calling
    [Transition_chain_verifier.verify]. This function would returns a list
    of state hashes if the verification is successful.

    2. using the list of state hashes to poke a transition frontier
    in order to find the hashes of missing transitions. If none of the hashes
    are found, then it means some more transitions are missing.

    Once the list of missing hashes are computed, it would do another request to
    download the corresponding transitions in a batch fashion. Next it will perform the
    following validations on each external_transition:

    1. Check the list of transitions corresponds to the list of hashes that we
    requested;

    2. Each transition is checked through [Transition_processor.Validator] and
    [Protocol_state_validator]

    If any of the external_transitions is invalid,
    1) the sender is punished;
    2) those external_transitions that already passed validation would be
       invalidated.
    Otherwise, [Ledger_catchup] will build a corresponding breadcrumb path from
    the path of external_transitions. A breadcrumb from the path is built using
    its corresponding external_transition staged_ledger_diff and applying it to
    its preceding breadcrumb staged_ledger to obtain its corresponding
    staged_ledger. If there was an error in building the breadcrumbs, then
    catchup would invalidate the cached transitions.
    After building the breadcrumb path, [Ledger_catchup] will then send it to
    the [Processor] via writing them to catchup_breadcrumbs_writer. *)

open Transition_frontier.Full_catchup_tree

module G = Graph.Graphviz.Dot (struct
  type nonrec t = t

  module V = struct
    type t = Node.t
  end

  module E = struct
    type t = {parent: Node.t; child: Node.t}

    let src t = t.parent

    let dst t = t.child
  end

  let iter_vertex (f : V.t -> unit) (t : t) = Hashtbl.iter t.nodes ~f

  let iter_edges_e (f : E.t -> unit) (t : t) =
    Hashtbl.iter t.nodes ~f:(fun child ->
        match Hashtbl.find t.nodes child.parent with
        | None ->
            ()
        | Some parent ->
            f {child; parent} )

  let graph_attributes (_ : t) = [`Rankdir `LeftToRight]

  let get_subgraph _ = None

  let default_vertex_attributes _ = [`Shape `Circle]

  let vertex_attributes (v : Node.t) =
    let color =
      match v.state with
      | Failed ->
          (* red *)
          0xFF3333
      | Root _ | Finished ->
          (* green *)
          0x00CC00
      | To_download _ ->
          (* gray *)
          0xA0A0A0
      | Wait_for_parent _ ->
          (* black *)
          0x000000
      | To_build_breadcrumb _ ->
          (* dark purple *)
          0x330033
      | To_initial_validate _ ->
          (* yellow *)
          0xFFFF33
      | To_verify _ ->
          (* orange *)
          0xFF9933
    in
    [`Shape `Circle; `Style `Filled; `Fillcolor color]

  let vertex_name (node : V.t) =
    sprintf "\"%s\"" (State_hash.to_base58_check node.state_hash)

  let default_edge_attributes _ = []

  let edge_attributes _ = []
end)

let write_graph (_ : t) =
  let _ = G.output_graph in
  ()

let verify_transition ~logger ~consensus_constants ~trust_system ~frontier
    ~unprocessed_transition_cache enveloped_transition =
  let sender = Envelope.Incoming.sender enveloped_transition in
  let genesis_state_hash = Transition_frontier.genesis_state_hash frontier in
  let transition_with_hash = Envelope.Incoming.data enveloped_transition in
  let cached_initially_validated_transition_result =
    let open Result.Let_syntax in
    let%bind initially_validated_transition =
      transition_with_hash
      |> External_transition.skip_time_received_validation
           `This_transition_was_not_received_via_gossip
      |> External_transition.validate_genesis_protocol_state
           ~genesis_state_hash
      >>= External_transition.validate_protocol_versions
      >>= External_transition.validate_delta_transition_chain
    in
    let enveloped_initially_validated_transition =
      Envelope.Incoming.map enveloped_transition
        ~f:(Fn.const initially_validated_transition)
    in
    Transition_handler.Validator.validate_transition ~logger ~frontier
      ~consensus_constants ~unprocessed_transition_cache
      enveloped_initially_validated_transition
  in
  let state_hash =
    External_transition.Validation.forget_validation_with_hash
      transition_with_hash
    |> With_hash.hash |> State_hash.to_yojson
  in
  let open Deferred.Let_syntax in
  match cached_initially_validated_transition_result with
  | Ok x ->
      [%log trace]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: validation is successful" ;
      Deferred.return @@ Ok (`Building_path x)
  | Error (`In_frontier hash) ->
      [%log trace]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: transition queried during ledger catchup has \
         already been seen" ;
      Deferred.return @@ Ok (`In_frontier hash)
  | Error (`In_process consumed_state) -> (
      [%log trace]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: transition queried during ledger catchup is still \
         in process in one of the components in transition_frontier" ;
      match%map Ivar.read consumed_state with
      | `Failed ->
          [%log trace]
            ~metadata:[("state_hash", state_hash)]
            "initial_validate: transition queried during ledger catchup failed" ;
          Error (Error.of_string "Previous transition failed")
      | `Success hash ->
          [%log trace]
            ~metadata:[("state_hash", state_hash)]
            "initial_validate: transition queried during ledger catchup is \
             added to frontier" ;
          Ok (`In_frontier hash) )
  | Error (`Verifier_error error) ->
      [%log warn]
        ~metadata:
          [ ("error", Error_json.error_to_yojson error)
          ; ("state_hash", state_hash) ]
        "initial_validate: verifier threw an error while verifying transiton \
         queried during ledger catchup: $error" ;
      Deferred.Or_error.fail (Error.tag ~tag:"verifier threw an error" error)
  | Error `Invalid_proof ->
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Gossiped_invalid_transition
          , Some ("invalid proof", []) )
      in
      [%log warn]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: invalid proof" ;
      Error (Error.of_string "invalid proof")
  | Error `Invalid_genesis_protocol_state ->
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Gossiped_invalid_transition
          , Some ("invalid genesis protocol state", []) )
      in
      [%log warn]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: invalid genesis protocol state" ;
      Error (Error.of_string "invalid genesis protocol state")
  | Error `Invalid_delta_transition_chain_proof ->
      [%log warn]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: invalid delta transition chain proof" ;
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Gossiped_invalid_transition
          , Some ("invalid delta transition chain witness", []) )
      in
      Error (Error.of_string "invalid delta transition chain witness")
  | Error `Invalid_protocol_version ->
      [%log warn]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: invalid protocol version" ;
      let transition =
        External_transition.Validation.forget_validation transition_with_hash
      in
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Sent_invalid_protocol_version
          , Some
              ( "Invalid current or proposed protocol version in catchup block"
              , [ ( "current_protocol_version"
                  , `String
                      ( External_transition.current_protocol_version transition
                      |> Protocol_version.to_string ) )
                ; ( "proposed_protocol_version"
                  , `String
                      ( External_transition.proposed_protocol_version_opt
                          transition
                      |> Option.value_map ~default:"<None>"
                           ~f:Protocol_version.to_string ) ) ] ) )
      in
      Error (Error.of_string "invalid protocol version")
  | Error `Mismatched_protocol_version ->
      [%log warn]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: mismatch protocol version" ;
      let transition =
        External_transition.Validation.forget_validation transition_with_hash
      in
      let%map () =
        Trust_system.record_envelope_sender trust_system logger sender
          ( Trust_system.Actions.Sent_mismatched_protocol_version
          , Some
              ( "Current protocol version in catchup block does not match \
                 daemon protocol version"
              , [ ( "block_current_protocol_version"
                  , `String
                      ( External_transition.current_protocol_version transition
                      |> Protocol_version.to_string ) )
                ; ( "daemon_current_protocol_version"
                  , `String Protocol_version.(get_current () |> to_string) ) ]
              ) )
      in
      Error (Error.of_string "mismatched protocol version")
  | Error `Disconnected ->
      [%log warn]
        ~metadata:[("state_hash", state_hash)]
        "initial_validate: disconnected chain" ;
      Deferred.Or_error.fail @@ Error.of_string "disconnected chain"

let find_map_ok ?how xs ~f =
  let res = Ivar.create () in
  let errs = ref [] in
  don't_wait_for
    (let%map () =
       Deferred.List.iter xs ?how ~f:(fun x ->
           if Ivar.is_full res then Deferred.unit
           else
             match%map
               choose
                 [ choice (Ivar.read res) (fun _ -> `Finished)
                 ; choice (f x) (fun x -> `Ok x) ]
             with
             | `Finished ->
                 ()
             | `Ok (Ok x) ->
                 Ivar.fill_if_empty res (Ok x)
             | `Ok (Error e) ->
                 errs := e :: !errs )
     in
     Ivar.fill_if_empty res (Error !errs)) ;
  Ivar.read res

type download_state_hashes_error =
  [ `Peer_moves_too_fast
  | `No_common_ancestor
  | `Failed_to_download_transition_chain_proof
  | `Invalid_transition_chain_proof ]

let rec contains_no_common_ancestor = function
  | [] ->
      false
  | `No_common_ancestor :: _ ->
      true
  | _ :: errors ->
      contains_no_common_ancestor errors

let try_to_connect_hash_chain t hashes ~frontier
    ~blockchain_length_of_target_hash =
  let logger = t.logger in
  let blockchain_length_of_root =
    Transition_frontier.root frontier
    |> Transition_frontier.Breadcrumb.blockchain_length
  in
  List.fold_until
    (Non_empty_list.to_list hashes)
    ~init:(blockchain_length_of_target_hash, [])
    ~f:(fun (blockchain_length, acc) hash ->
      let f x = Continue_or_stop.Stop (Ok (x, acc)) in
      match
        (Hashtbl.find t.nodes hash, Transition_frontier.find frontier hash)
      with
      | Some node, None ->
          f (`Node node)
      | Some node, Some b ->
          finish t node (Ok b) ;
          f (`Node node)
      | None, Some b ->
          f (`Breadcrumb b)
      | None, None ->
          Continue (Unsigned.UInt32.pred blockchain_length, hash :: acc) )
    ~finish:(fun (blockchain_length, acc) ->
      let module T = struct
        type t = State_hash.t list [@@deriving to_yojson]
      end in
      let all_hashes =
        List.map (Transition_frontier.all_breadcrumbs frontier) ~f:(fun b ->
            Frontier_base.Breadcrumb.state_hash b )
      in
      [%log debug]
        ~metadata:
          [ ("n", `Int (List.length acc))
          ; ("hashes", T.to_yojson acc)
          ; ("all_hashes", T.to_yojson all_hashes) ]
        "Finishing download_state_hashes with $n $hashes. with $all_hashes" ;
      if
        Unsigned.UInt32.compare blockchain_length blockchain_length_of_root
        <= 0
      then Result.fail `No_common_ancestor
      else Result.fail `Peer_moves_too_fast )

module Downloader = struct
  module Key = struct
    module T = struct
      type t = State_hash.t * Length.t [@@deriving to_yojson, hash, sexp]

      let compare (h1, n1) (h2, n2) =
        match Length.compare n1 n2 with
        | 0 ->
            State_hash.compare h1 h2
        | c ->
            c
    end

    include T
    include Hashable.Make (T)
    include Comparable.Make (T)
  end

  include Downloader.Make
            (Key)
            (struct
              include Attempt_history.Attempt

              let download : t = {failure_reason= `Download}

              let worth_retrying (t : t) =
                match t.failure_reason with `Download -> true | _ -> false
            end)
            (struct
              type t = External_transition.t

              let key (t : t) =
                External_transition.(state_hash t, blockchain_length t)
            end)
            (struct
              type t = (State_hash.t * Length.t) option
            end)
end

let with_lengths hs ~target_length =
  List.filter_mapi (Non_empty_list.to_list hs) ~f:(fun i x ->
      let open Option.Let_syntax in
      let%map x_len = Length.sub target_length (Length.of_int i) in
      (x, x_len) )

(* returns a list of state-hashes with the older ones at the front *)
let download_state_hashes t ~logger ~trust_system ~network ~frontier
    ~target_hash ~target_length ~downloader ~blockchain_length_of_target_hash =
  [%log debug]
    ~metadata:[("target_hash", State_hash.to_yojson target_hash)]
    "Doing a catchup job with target $target_hash" ;
  let%bind peers =
    (* TODO: Find some preferred peers, e.g., whoever told us about this target_hash *)
    Mina_networking.peers network >>| List.permute
  in
  let open Deferred.Result.Let_syntax in
  find_map_ok ~how:(`Max_concurrent_jobs 12) peers ~f:(fun peer ->
      let%bind transition_chain_proof =
        let open Deferred.Let_syntax in
        match%map
          Mina_networking.get_transition_chain_proof
            ~timeout:(Time.Span.of_sec 10.) network peer target_hash
        with
        | Error _ ->
            Result.fail `Failed_to_download_transition_chain_proof
        | Ok transition_chain_proof ->
            Result.return transition_chain_proof
      in
      let now = Time.now () in
      (* a list of state_hashes from new to old *)
      let%bind hashes =
        match
          Transition_chain_verifier.verify ~target_hash ~transition_chain_proof
        with
        | Some hs ->
            let ks = with_lengths hs ~target_length in
            Downloader.update_knowledge downloader peer (`Some ks) ;
            Downloader.mark_preferred downloader peer ~now ;
            Deferred.Result.return hs
        | None ->
            let error_msg =
              sprintf
                !"Peer %{sexp:Network_peer.Peer.t} sent us bad proof"
                peer
            in
            ignore
              Trust_system.(
                record trust_system logger peer
                  Actions.
                    ( Sent_invalid_transition_chain_merkle_proof
                    , Some (error_msg, []) )) ;
            Deferred.Result.fail `Invalid_transition_chain_proof
      in
      Deferred.return
        ( match
            try_to_connect_hash_chain t hashes ~frontier
              ~blockchain_length_of_target_hash
          with
        | Ok x ->
            Downloader.mark_preferred downloader peer ~now ;
            Ok x
        | Error e ->
            Error e ) )

let get_state_hashes = ()

module Initial_validate_batcher = struct
  open Network_pool.Batcher

  type input =
    (External_transition.t, State_hash.t) With_hash.t Envelope.Incoming.t

  type nonrec 'a t = (input, input, 'a) t

  let create ~verifier : _ t =
    create
      ~logger:
        (Logger.create
           ~metadata:[("name", `String "initial_validate_batcher")]
           ())
      ~how_to_add:`Insert ~max_weight_per_call:1000
      ~weight:(fun _ -> 1)
      ~compare_init:(fun e1 e2 ->
        let len (x : input) =
          External_transition.blockchain_length x.data.data
        in
        match Length.compare (len e1) (len e2) with
        | 0 ->
            compare_envelope e1 e2
        | c ->
            c )
      (fun xs ->
        let input = function `Partially_validated x | `Init x -> x in
        List.map xs ~f:(fun x ->
            External_transition.Validation.wrap
              (Envelope.Incoming.data (input x)) )
        |> External_transition.validate_proofs ~verifier
        >>| function
        | Ok tvs ->
            Ok (List.map tvs ~f:(fun x -> `Valid x))
        | Error `Invalid_proof ->
            Ok (List.map xs ~f:(fun x -> `Potentially_invalid (input x)))
        | Error (`Verifier_error e) ->
            Error e )

  let verify (t : _ t) = verify t
end

module Verify_work_batcher = struct
  open Network_pool.Batcher

  type input = External_transition.Initial_validated.t Envelope.Incoming.t

  type nonrec 'a t = (input, input, 'a) t

  let create ~verifier : _ t =
    let works (x : input) =
      let wh, _ = x.data in
      External_transition.staged_ledger_diff wh.data
      |> Staged_ledger_diff.completed_works
    in
    create
      ~logger:
        (Logger.create ~metadata:[("name", `String "verify_work_batcher")] ())
      ~weight:(fun (x : input) ->
        List.fold ~init:0 (works x) ~f:(fun acc {proofs; _} ->
            acc + One_or_two.length proofs ) )
      ~max_weight_per_call:1000 ~how_to_add:`Insert
      ~compare_init:(fun e1 e2 ->
        let len (x : input) =
          External_transition.Initial_validated.blockchain_length x.data
        in
        match Length.compare (len e1) (len e2) with
        | 0 ->
            compare_envelope e1 e2
        | c ->
            c )
      (fun xs ->
        let input : _ -> input = function
          | `Partially_validated x | `Init x ->
              x
        in
        List.concat_map xs ~f:(fun x ->
            works (input x)
            |> List.concat_map ~f:(fun {fee; prover; proofs} ->
                   let msg = Sok_message.create ~fee ~prover in
                   One_or_two.to_list
                     (One_or_two.map proofs ~f:(fun p -> (p, msg))) ) )
        |> Verifier.verify_transaction_snarks verifier
        >>| function
        | Ok true ->
            Ok (List.map xs ~f:(fun x -> `Valid (input x)))
        | Ok false ->
            Ok (List.map xs ~f:(fun x -> `Potentially_invalid (input x)))
        | Error e ->
            Error e )

  let verify (t : _ t) = verify t
end

let initial_validate ~(precomputed_values : Precomputed_values.t) ~logger
    ~trust_system ~(batcher : _ Initial_validate_batcher.t) ~frontier
    ~unprocessed_transition_cache transition =
  let verification_start_time = Core.Time.now () in
  let open Deferred.Result.Let_syntax in
  let state_hash =
    Envelope.Incoming.data transition |> With_hash.hash |> State_hash.to_yojson
  in
  [%log debug]
    ~metadata:[("state_hash", state_hash)]
    "initial_validate: start processing $state_hash" ;
  let%bind tv =
    let open Deferred.Let_syntax in
    match%bind Initial_validate_batcher.verify batcher transition with
    | Ok (Ok tv) ->
        return (Ok {transition with data= tv})
    | Ok (Error ()) ->
        let s = "initial_validate: proof failed to verify" in
        [%log warn] ~metadata:[("state_hash", state_hash)] "%s" s ;
        let%map () =
          match transition.sender with
          | Local ->
              Deferred.unit
          | Remote peer ->
              Trust_system.(
                record trust_system logger peer
                  Actions.(Sent_invalid_proof, None))
        in
        Error (`Error (Error.of_string s))
    | Error e ->
        [%log warn]
          ~metadata:
            [ ("error", Error_json.error_to_yojson e)
            ; ("state_hash", state_hash) ]
          "initial_validate: verification of blockchain snark failed but it \
           was our fault" ;
        return (Error `Couldn't_reach_verifier)
  in
  let verification_end_time = Core.Time.now () in
  [%log debug]
    ~metadata:
      [ ( "time_elapsed"
        , `Float
            Core.Time.(
              Span.to_sec @@ diff verification_end_time verification_start_time)
        )
      ; ("state_hash", state_hash) ]
    "initial_validate: verification of proofs complete" ;
  verify_transition ~logger
    ~consensus_constants:precomputed_values.consensus_constants ~trust_system
    ~frontier ~unprocessed_transition_cache tv
  |> Deferred.map ~f:(Result.map_error ~f:(fun e -> `Error e))

open Frontier_base

let check_invariant ~downloader t =
  Downloader.check_invariant downloader ;
  [%test_eq: int]
    (Downloader.total_jobs downloader)
    (Hashtbl.count t.nodes ~f:(fun node ->
         Node.State.enum node.state = To_download ))

let download s d ~key ~attempts =
  let logger = Logger.create () in
  [%log debug]
    ~metadata:[("key", Downloader.Key.to_yojson key); ("caller", `String s)]
    "Download download $key" ;
  Downloader.download d ~key ~attempts

let create_node ~downloader t x =
  let attempts = Attempt_history.empty in
  let state, h, blockchain_length, parent, result =
    match x with
    | `Root root ->
        ( Node.State.Finished
        , Breadcrumb.state_hash root
        , Breadcrumb.blockchain_length root
        , Breadcrumb.parent_hash root
        , Ivar.create_full (Ok `Added_to_frontier) )
    | `Hash (h, l, parent) ->
        ( Node.State.To_download
            (download "create_node" downloader ~key:(h, l) ~attempts)
        , h
        , l
        , parent
        , Ivar.create () )
    | `Initial_validated b ->
        let t = (Cached.peek b).Envelope.Incoming.data in
        let open External_transition.Initial_validated in
        ( Node.State.To_verify b
        , state_hash t
        , blockchain_length t
        , parent_hash t
        , Ivar.create () )
  in
  let node =
    {Node.state; state_hash= h; blockchain_length; attempts; parent; result}
  in
  upon (Ivar.read node.result) (fun _ ->
      Downloader.cancel downloader (h, blockchain_length) ) ;
  Transition_frontier.Full_catchup_tree.add_state t.states node ;
  Hashtbl.set t.nodes ~key:h ~data:node ;
  ( try check_invariant ~downloader t
    with e ->
      [%log' debug t.logger]
        ~metadata:[("exn", `String (Exn.to_string e))]
        "create_node $exn" ) ;
  write_graph t ; node

let set_state t node s = set_state t node s ; write_graph t

let pick ~constants
    (x : (Mina_state.Protocol_state.Value.t, State_hash.t) With_hash.t)
    (y : _ With_hash.t) =
  let f = With_hash.map ~f:Mina_state.Protocol_state.consensus_state in
  match
    Consensus.Hooks.select ~constants ~existing:(f x) ~candidate:(f y)
      ~logger:(Logger.null ())
  with
  | `Keep ->
      x
  | `Take ->
      y

let forest_pick forest =
  with_return (fun {return} ->
      List.iter forest ~f:(Rose_tree.iter ~f:return) ;
      assert false )

(* TODO: In the future, this could take over scheduling bootstraps too. *)
let run ~logger ~trust_system ~verifier ~network ~frontier
    ~(catchup_job_reader :
       ( State_hash.t
       * ( External_transition.Initial_validated.t Envelope.Incoming.t
         , State_hash.t )
         Cached.t
         Rose_tree.t
         list )
       Strict_pipe.Reader.t) ~precomputed_values ~unprocessed_transition_cache
    ~(catchup_breadcrumbs_writer :
       ( (Transition_frontier.Breadcrumb.t, State_hash.t) Cached.t Rose_tree.t
         list
         * [`Ledger_catchup of unit Ivar.t | `Catchup_scheduler]
       , Strict_pipe.crash Strict_pipe.buffered
       , unit )
       Strict_pipe.Writer.t) =
  let t =
    match Transition_frontier.catchup_tree frontier with
    | Full t ->
        t
    | Hash _ ->
        failwith
          "If super catchup is running, the frontier should have a full \
           catchup tree"
  in
  let stop = Transition_frontier.closed frontier in
  upon stop (fun () -> tear_down t) ;
  let combine =
    Option.merge
      ~f:
        (pick
           ~constants:precomputed_values.Precomputed_values.consensus_constants)
  in
  let pre_context
      (trees :
        ( External_transition.Initial_validated.t Envelope.Incoming.t
        , _ )
        Cached.t
        Rose_tree.t
        list) =
    let f tree =
      let best = ref None in
      Rose_tree.iter tree
        ~f:(fun (x :
                  ( External_transition.Initial_validated.t Envelope.Incoming.t
                  , _ )
                  Cached.t)
           ->
          let x, _ = (Cached.peek x).data in
          best :=
            combine !best
              (Some (With_hash.map ~f:External_transition.protocol_state x)) ) ;
      !best
    in
    List.map trees ~f |> List.reduce ~f:combine |> Option.join
  in
  let best_tip_r, best_tip_w = Broadcast_pipe.create None in
  let%bind downloader =
    let knowledge h peer =
      let heartbeat_timeout = Time_ns.Span.of_sec 30. in
      match h with
      | None ->
          return `All
      | Some (h, len) -> (
          match%map
            Mina_networking.get_transition_chain_proof
              ~timeout:(Time.Span.of_sec 30.) ~heartbeat_timeout network peer h
          with
          | Error _ ->
              `Some []
          | Ok p -> (
            match
              Transition_chain_verifier.verify ~target_hash:h
                ~transition_chain_proof:p
            with
            | Some hs ->
                let ks = with_lengths hs ~target_length:len in
                `Some ks
            | None ->
                `Some [] ) )
    in
    Downloader.create ~stop ~trust_system ~preferred:[] ~max_batch_size:5
      ~get:(fun peer hs ->
        let sec =
          let sec_per_block =
            Option.value_map
              (Sys.getenv "MINA_EXPECTED_PER_BLOCK_DOWNLOAD_TIME")
              ~default:15. ~f:Float.of_string
          in
          Float.of_int (List.length hs) *. sec_per_block
        in
        Mina_networking.get_transition_chain
          ~heartbeat_timeout:(Time_ns.Span.of_sec sec)
          ~timeout:(Time.Span.of_sec sec) network peer (List.map hs ~f:fst) )
      ~peers:(fun () -> Mina_networking.peers network)
      ~knowledge_context:
        (Broadcast_pipe.map best_tip_r
           ~f:
             (Option.map ~f:(fun (x : _ With_hash.t) ->
                  ( x.hash
                  , Mina_state.Protocol_state.consensus_state x.data
                    |> Consensus.Data.Consensus_state.blockchain_length ) )))
      ~knowledge
  in
  check_invariant ~downloader t ;
  let () =
    Downloader.set_check_invariant (fun downloader ->
        check_invariant ~downloader t )
  in
  every ~stop (Time.Span.of_sec 10.) (fun () ->
      [%log debug] ~metadata:[("states", to_yojson t)] "Catchup states" ) ;
  let initial_validation_batcher = Initial_validate_batcher.create ~verifier in
  let verify_work_batcher = Verify_work_batcher.create ~verifier in
  let set_state t node s =
    set_state t node s ;
    try check_invariant ~downloader t
    with e ->
      [%log' debug t.logger]
        ~metadata:[("exn", `String (Exn.to_string e))]
        "set_state $exn"
  in
  let rec run_node (node : Node.t) =
    let state_hash = node.state_hash in
    let failed ?error ~sender failure_reason =
      [%log' debug t.logger] "failed with $error"
        ~metadata:
          [ ( "error"
            , Option.value_map ~default:`Null error ~f:(fun e ->
                  `String (Error.to_string_hum e) ) )
          ; ("reason", Attempt_history.Attempt.reason_to_yojson failure_reason)
          ] ;
      node.attempts
      <- ( match sender with
         | Envelope.Sender.Local ->
             node.attempts
         | Remote peer ->
             Map.set node.attempts ~key:peer ~data:{failure_reason} ) ;
      set_state t node
        (To_download
           (download "failed" downloader
              ~key:(state_hash, node.blockchain_length)
              ~attempts:node.attempts)) ;
      run_node node
    in
    let step d : (_, [`Finished]) Deferred.Result.t =
      (* TODO: See if the bail out is happening. *)
      Deferred.any [(Ivar.read node.result >>| fun _ -> Error `Finished); d]
    in
    let open Deferred.Result.Let_syntax in
    let retry () =
      let%bind () =
        step (after (Time.Span.of_sec 15.) |> Deferred.map ~f:Result.return)
      in
      run_node node
    in
    match node.state with
    | Failed | Finished | Root _ ->
        return ()
    | To_download download_job ->
        let%bind b, attempts = step (Downloader.Job.result download_job) in
        [%log' debug t.logger]
          ~metadata:
            [ ("state_hash", State_hash.to_yojson state_hash)
            ; ( "donwload_number"
              , `Int
                  (Hashtbl.count t.nodes ~f:(fun node ->
                       Node.State.enum node.state = To_download )) )
            ; ("total_nodes", `Int (Hashtbl.length t.nodes))
            ; ( "node_states"
              , let s = Node.State.Enum.Table.create () in
                Hashtbl.iter t.nodes ~f:(fun node ->
                    Hashtbl.incr s (Node.State.enum node.state) ) ;
                `List
                  (List.map (Hashtbl.to_alist s) ~f:(fun (k, v) ->
                       `List [Node.State.Enum.to_yojson k; `Int v] )) )
            ; ("total_jobs", `Int (Downloader.total_jobs downloader))
            ; ("downloader", Downloader.to_yojson downloader) ]
          "download finished $state_hash" ;
        node.attempts <- attempts ;
        set_state t node (To_initial_validate b) ;
        run_node node
    | To_initial_validate b -> (
        match%bind
          step
            ( initial_validate ~precomputed_values ~logger ~trust_system
                ~batcher:initial_validation_batcher ~frontier
                ~unprocessed_transition_cache
                {b with data= {With_hash.data= b.data; hash= state_hash}}
            |> Deferred.map ~f:(fun x -> Ok x) )
        with
        | Error (`Error e) ->
            (* TODO: Log *)
            (* Validation failed. Record the failure and go back to download. *)
            failed ~error:e ~sender:b.sender `Initial_validate
        | Error `Couldn't_reach_verifier ->
            retry ()
        | Ok (`In_frontier hash) ->
            finish t node (Ok (Transition_frontier.find_exn frontier hash)) ;
            Deferred.return (Ok ())
        | Ok (`Building_path tv) ->
            set_state t node (To_verify tv) ;
            run_node node )
    | To_verify tv -> (
        let iv = Cached.peek tv in
        (* TODO: Set up job to invalidate tv on catchup_breadcrumbs_writer closing *)
        match%bind
          step
            (* TODO: give the batch verifier a way to somehow throw away stuff if
    this node gets removed from the tree. *)
            ( Verify_work_batcher.verify verify_work_batcher iv
            |> Deferred.map ~f:Result.return )
        with
        | Error _e ->
            [%log' debug t.logger] "Couldn't reach verifier. Retrying"
              ~metadata:[("state_hash", State_hash.to_yojson node.state_hash)] ;
            (* No need to redownload in this case. We just wait a little and try again. *)
            retry ()
        | Ok (Error ()) ->
            [%log' warn t.logger] "verification failed! redownloading"
              ~metadata:[("state_hash", State_hash.to_yojson node.state_hash)] ;
            ( match iv.sender with
            | Local ->
                ()
            | Remote peer ->
                Trust_system.(
                  record trust_system logger peer
                    Actions.(Sent_invalid_proof, None))
                |> don't_wait_for ) ;
            let _ = Cached.invalidate_with_failure tv in
            failed ~sender:iv.sender `Verify
        | Ok (Ok av) ->
            let av =
              { av with
                data=
                  External_transition.skip_frontier_dependencies_validation
                    `This_transition_belongs_to_a_detached_subtree av.data }
            in
            let av = Cached.transform tv ~f:(fun _ -> av) in
            set_state t node (Wait_for_parent av) ;
            run_node node )
    | Wait_for_parent av ->
        let%bind parent =
          step
            (let parent = Hashtbl.find_exn t.nodes node.parent in
             match%map.Async Ivar.read parent.result with
             | Ok `Added_to_frontier ->
                 Ok parent.state_hash
             | Error _ ->
                 let _ = Cached.invalidate_with_failure av in
                 finish t node (Error ()) ;
                 Error `Finished)
        in
        set_state t node (To_build_breadcrumb (`Parent parent, av)) ;
        run_node node
    | To_build_breadcrumb (`Parent parent_hash, c) -> (
        let transition_receipt_time = Some (Time.now ()) in
        let av = Cached.peek c in
        match%bind
          let s =
            let open Deferred.Result.Let_syntax in
            let%bind parent =
              Deferred.return
                ( match Transition_frontier.find frontier parent_hash with
                | None ->
                    Error `Parent_breadcrumb_not_found
                | Some breadcrumb ->
                    Ok breadcrumb )
            in
            Transition_frontier.Breadcrumb.build ~logger
              ~skip_staged_ledger_verification:`Proofs ~precomputed_values
              ~verifier ~trust_system ~parent ~transition:av.data
              ~sender:(Some av.sender) ~transition_receipt_time ()
          in
          step (Deferred.map ~f:Result.return s)
        with
        | Error e ->
            let _ = Cached.invalidate_with_failure c in
            let e =
              match e with
              | `Exn e ->
                  Error.tag (Error.of_exn e) ~tag:"exn"
              | `Fatal_error e ->
                  Error.tag (Error.of_exn e) ~tag:"fatal"
              | `Invalid_staged_ledger_diff e ->
                  Error.tag e ~tag:"invalid staged ledger diff"
              | `Invalid_staged_ledger_hash e ->
                  Error.tag e ~tag:"invalid staged ledger hash"
              | `Parent_breadcrumb_not_found ->
                  Error.tag
                    (Error.of_string
                       (sprintf
                          "Parent breadcrumb with state_hash %s not found"
                          (State_hash.to_string parent_hash)))
                    ~tag:"parent breadcrumb not found"
            in
            failed ~error:e ~sender:av.sender `Build_breadcrumb
        | Ok breadcrumb ->
            let%bind () =
              Scheduler.yield () |> Deferred.map ~f:Result.return
            in
            let finished = Ivar.create () in
            let c = Cached.transform c ~f:(fun _ -> breadcrumb) in
            Strict_pipe.Writer.write catchup_breadcrumbs_writer
              ( [Rose_tree.of_non_empty_list (Non_empty_list.singleton c)]
              , `Ledger_catchup finished ) ;
            let%bind () =
              (* The cached value is "freed" by the transition processor in [add_and_finalize]. *)
              step (Deferred.map (Ivar.read finished) ~f:Result.return)
            in
            Ivar.fill_if_empty node.result (Ok `Added_to_frontier) ;
            set_state t node Finished ;
            return () )
  in
  (* TODO: Maybe add everything from transition frontier at the beginning? *)
  (* TODO: Print out the hashes you're adding *)
  Strict_pipe.Reader.iter_without_pushback catchup_job_reader
    ~f:(fun (target_parent_hash, forest) ->
      don't_wait_for
        (let prev_ctx = Broadcast_pipe.Reader.peek best_tip_r in
         let ctx = combine prev_ctx (pre_context forest) in
         let eq x y =
           let f = Option.map ~f:With_hash.hash in
           Option.equal State_hash.equal (f x) (f y)
         in
         if eq prev_ctx ctx then Deferred.unit
         else Broadcast_pipe.Writer.write best_tip_w ctx) ;
      don't_wait_for
        (let state_hashes =
           let target_length =
             let len =
               forest_pick forest |> Cached.peek |> Envelope.Incoming.data
               |> External_transition.Initial_validated.blockchain_length
             in
             Option.value_exn (Length.sub len (Length.of_int 1))
           in
           let blockchain_length_of_target_hash =
             let blockchain_length_of_dangling_block =
               List.hd_exn forest |> Rose_tree.root |> Cached.peek
               |> Envelope.Incoming.data
               |> External_transition.Initial_validated.blockchain_length
             in
             Unsigned.UInt32.pred blockchain_length_of_dangling_block
           in
           match
             List.find_map (List.concat_map ~f:Rose_tree.flatten forest)
               ~f:(fun c ->
                 let h =
                   External_transition.Initial_validated.state_hash
                     (Cached.peek c).data
                 in
                 ( match (Cached.peek c).sender with
                 | Local ->
                     ()
                 | Remote peer ->
                     Downloader.add_knowledge downloader peer
                       [(target_parent_hash, target_length)] ) ;
                 let open Option.Let_syntax in
                 let%bind {proof= path, root; data} = Best_tip_lru.get h in
                 let%bind p =
                   Transition_chain_verifier.verify
                     ~target_hash:
                       (External_transition.Initial_validated.state_hash data)
                     ~transition_chain_proof:
                       (External_transition.state_hash root, path)
                 in
                 Result.ok
                   (try_to_connect_hash_chain t p ~frontier
                      ~blockchain_length_of_target_hash) )
           with
           | None ->
               download_state_hashes t ~logger ~trust_system ~network ~frontier
                 ~downloader ~target_length ~target_hash:target_parent_hash
                 ~blockchain_length_of_target_hash
           | Some res ->
               [%log debug] "Succeeded in using cache." ;
               Deferred.Result.return res
         in
         match%map state_hashes with
         | Error errors ->
             [%log debug]
               ~metadata:
                 [("target_hash", State_hash.to_yojson target_parent_hash)]
               "Failed to download state hashes for $target_hash" ;
             if contains_no_common_ancestor errors then
               List.iter forest ~f:(fun subtree ->
                   let transition =
                     Rose_tree.root subtree |> Cached.peek
                     |> Envelope.Incoming.data
                   in
                   let children_transitions =
                     List.concat_map
                       (Rose_tree.children subtree)
                       ~f:Rose_tree.flatten
                   in
                   let children_state_hashes =
                     List.map children_transitions ~f:(fun cached_transition ->
                         Cached.peek cached_transition
                         |> Envelope.Incoming.data
                         |> External_transition.Initial_validated.state_hash )
                   in
                   [%log error]
                     ~metadata:
                       [ ( "state_hashes_of_children"
                         , `List
                             (List.map children_state_hashes
                                ~f:State_hash.to_yojson) )
                       ; ( "state_hash"
                         , State_hash.to_yojson
                           @@ External_transition.Initial_validated.state_hash
                                transition )
                       ; ( "reason"
                         , `String
                             "no common ancestor with our transition frontier"
                         )
                       ; ( "protocol_state"
                         , External_transition.Initial_validated.protocol_state
                             transition
                           |> Mina_state.Protocol_state.value_to_yojson ) ]
                     "Validation error: external transition with state hash \
                      $state_hash and its children were rejected for reason \
                      $reason" ;
                   Mina_metrics.(
                     Counter.inc Rejected_blocks.no_common_ancestor
                       (Float.of_int @@ (1 + List.length children_transitions)))
               ) ;
             List.iter forest ~f:(fun subtree ->
                 Rose_tree.iter subtree ~f:(fun cached ->
                     Cached.invalidate_with_failure cached |> ignore ) )
         | Ok (root, state_hashes) ->
             [%log' debug t.logger]
               ~metadata:
                 [ ("downloader", Downloader.to_yojson downloader)
                 ; ( "node_states"
                   , let s = Node.State.Enum.Table.create () in
                     Hashtbl.iter t.nodes ~f:(fun node ->
                         Hashtbl.incr s (Node.State.enum node.state) ) ;
                     `List
                       (List.map (Hashtbl.to_alist s) ~f:(fun (k, v) ->
                            `List [Node.State.Enum.to_yojson k; `Int v] )) ) ]
               "before everything" ;
             let root =
               match root with
               | `Breadcrumb root ->
                   (* If we hit this case we should probably remove the parent from the
                  table and prune, although in theory that should be handled by
                 the frontier calling [Full_catchup_tree.apply_diffs]. *)
                   create_node ~downloader t (`Root root)
               | `Node node ->
                   (* TODO: Log what is going on with transition frontier. *)
                   node
             in
             [%log debug]
               ~metadata:[("n", `Int (List.length state_hashes))]
               "Adding $n nodes" ;
             List.iter forest
               ~f:
                 (Rose_tree.iter ~f:(fun c ->
                      let node =
                        create_node ~downloader t (`Initial_validated c)
                      in
                      run_node node |> ignore )) ;
             List.fold state_hashes
               ~init:(root.state_hash, root.blockchain_length)
               ~f:(fun (parent, l) h ->
                 let l = Length.succ l in
                 ( if not (Hashtbl.mem t.nodes h) then
                   let node =
                     create_node t ~downloader (`Hash (h, l, parent))
                   in
                   don't_wait_for (run_node node >>| ignore) ) ;
                 (h, l) )
             |> ignore) )

let run ~logger ~precomputed_values ~trust_system ~verifier ~network ~frontier
    ~catchup_job_reader ~catchup_breadcrumbs_writer
    ~unprocessed_transition_cache : unit =
  run ~logger ~trust_system ~verifier ~network ~frontier ~catchup_job_reader
    ~precomputed_values ~unprocessed_transition_cache
    ~catchup_breadcrumbs_writer
  |> don't_wait_for
