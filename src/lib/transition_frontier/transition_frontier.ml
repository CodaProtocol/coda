open Core_kernel
open Async_kernel
open Protocols.Coda_transition_frontier
open Coda_base
open Coda_state
open Coda_transition
open Pipe_lib
open Coda_incremental

module type Inputs_intf = Inputs.Inputs_intf

module Make (Inputs : Inputs_intf) :
  Transition_frontier_intf
  with type state_hash := State_hash.t
   and type mostly_validated_external_transition :=
              ( [`Time_received] * Truth.true_t
              , [`Proof] * Truth.true_t
              , [`Frontier_dependencies] * Truth.true_t
              , [`Staged_ledger_diff] * Truth.false_t )
              Inputs.External_transition.Validation.with_transition
   and type external_transition_validated :=
              Inputs.External_transition.Validated.t
   and type ledger_database := Ledger.Db.t
   and type staged_ledger_diff := Inputs.Staged_ledger_diff.t
   and type staged_ledger := Inputs.Staged_ledger.t
   and type masked_ledger := Ledger.Mask.Attached.t
   and type transaction_snark_scan_state := Inputs.Staged_ledger.Scan_state.t
   and type consensus_state := Consensus.Data.Consensus_state.Value.t
   and type consensus_local_state := Consensus.Data.Local_state.t
   and type user_command := User_command.t
   and type pending_coinbase := Pending_coinbase.t
   and type verifier := Inputs.Verifier.t
   and module Extensions.Work = Inputs.Transaction_snark_work.Statement =
struct
  open Inputs

  (* NOTE: is Consensus_mechanism.select preferable over distance? *)
  exception
    Parent_not_found of ([`Parent of State_hash.t] * [`Target of State_hash.t])

  exception Already_exists of State_hash.t

  module Breadcrumb = struct
    (* TODO: external_transition should be type : External_transition.With_valid_protocol_state.t #1344 *)
    type t =
      { transition_with_hash:
          (External_transition.Validated.t, State_hash.t) With_hash.t
      ; mutable staged_ledger: Staged_ledger.t sexp_opaque
      ; just_emitted_a_proof: bool }
    [@@deriving sexp, fields]

    let to_yojson {transition_with_hash; staged_ledger= _; just_emitted_a_proof}
        =
      `Assoc
        [ ( "transition_with_hash"
          , With_hash.to_yojson External_transition.Validated.to_yojson
              State_hash.to_yojson transition_with_hash )
        ; ("staged_ledger", `String "<opaque>")
        ; ("just_emitted_a_proof", `Bool just_emitted_a_proof) ]

    let create transition_with_hash staged_ledger =
      {transition_with_hash; staged_ledger; just_emitted_a_proof= false}

    let copy t = {t with staged_ledger= Staged_ledger.copy t.staged_ledger}

    module Staged_ledger_validation =
      External_transition.Staged_ledger_validation (Staged_ledger)

    let build ~logger ~verifier ~trust_system ~parent
        ~transition:transition_with_validation ~sender =
      O1trace.measure "Breadcrumb.build" (fun () ->
          let open Deferred.Let_syntax in
          match%bind
            Staged_ledger_validation.validate_staged_ledger_diff ~logger
              ~verifier ~parent_staged_ledger:parent.staged_ledger
              transition_with_validation
          with
          | Ok
              ( `Just_emitted_a_proof just_emitted_a_proof
              , `External_transition_with_validation
                  fully_valid_external_transition
              , `Staged_ledger transitioned_staged_ledger ) ->
              return
                (Ok
                   { transition_with_hash=
                       External_transition.Validation.lift
                         fully_valid_external_transition
                   ; staged_ledger= transitioned_staged_ledger
                   ; just_emitted_a_proof })
          | Error `Invalid_ledger_hash_after_staged_ledger_application ->
              let%map () =
                match sender with
                | None | Some Envelope.Sender.Local ->
                    return ()
                | Some (Envelope.Sender.Remote inet_addr) ->
                    Trust_system.(
                      record trust_system logger inet_addr
                        Actions.
                          ( Gossiped_invalid_transition
                          , Some ("Invalid staged ledger hash", []) ))
              in
              Error
                (`Invalid_staged_ledger_hash
                  (Error.of_string
                     "Snarked ledger hash and Staged ledger hash after \
                      applying the diff does not match blockchain state's \
                      ledger hash and staged ledger hash resp."))
          | Error
              (`Staged_ledger_application_failed
                (Staged_ledger.Staged_ledger_error.Unexpected e)) ->
              return (Error (`Fatal_error (Error.to_exn e)))
          | Error (`Staged_ledger_application_failed staged_ledger_error) ->
              let%map () =
                match sender with
                | None | Some Envelope.Sender.Local ->
                    return ()
                | Some (Envelope.Sender.Remote inet_addr) ->
                    let error_string =
                      Staged_ledger.Staged_ledger_error.to_string
                        staged_ledger_error
                    in
                    let make_actions action =
                      ( action
                      , Some
                          ( "Staged_ledger error: $error"
                          , [("error", `String error_string)] ) )
                    in
                    let open Trust_system.Actions in
                    (* TODO : refine these actions, issue 2375 *)
                    let action =
                      match staged_ledger_error with
                      | Invalid_proof _ ->
                          make_actions Sent_invalid_proof
                      | Bad_signature _ ->
                          make_actions Sent_invalid_signature
                      | Coinbase_error _
                      | Bad_prev_hash _
                      | Insufficient_fee _
                      | Non_zero_fee_excess _ ->
                          make_actions Gossiped_invalid_transition
                      | Unexpected _ ->
                          failwith
                            "build: Unexpected staged ledger error should \
                             have been caught in another pattern"
                    in
                    Trust_system.record trust_system logger inet_addr action
              in
              Error
                (`Invalid_staged_ledger_diff
                  (Staged_ledger.Staged_ledger_error.to_error
                     staged_ledger_error)) )

    let external_transition {transition_with_hash; _} =
      With_hash.data transition_with_hash

    let state_hash {transition_with_hash; _} =
      With_hash.hash transition_with_hash

    let parent_hash {transition_with_hash; _} =
      With_hash.data transition_with_hash
      |> External_transition.Validated.protocol_state
      |> Protocol_state.previous_state_hash

    let equal breadcrumb1 breadcrumb2 =
      State_hash.equal (state_hash breadcrumb1) (state_hash breadcrumb2)

    let compare breadcrumb1 breadcrumb2 =
      State_hash.compare (state_hash breadcrumb1) (state_hash breadcrumb2)

    let hash = Fn.compose State_hash.hash state_hash

    let consensus_state {transition_with_hash; _} =
      With_hash.data transition_with_hash
      |> External_transition.Validated.protocol_state
      |> Protocol_state.consensus_state

    let blockchain_state {transition_with_hash; _} =
      With_hash.data transition_with_hash
      |> External_transition.Validated.protocol_state
      |> Protocol_state.blockchain_state

    let name t =
      Visualization.display_short_sexp (module State_hash) @@ state_hash t

    type display =
      { state_hash: string
      ; blockchain_state: Blockchain_state.display
      ; consensus_state: Consensus.Data.Consensus_state.display
      ; parent: string }
    [@@deriving yojson]

    let display t =
      let blockchain_state = Blockchain_state.display (blockchain_state t) in
      let consensus_state = consensus_state t in
      let parent =
        Visualization.display_short_sexp (module State_hash) @@ parent_hash t
      in
      { state_hash= name t
      ; blockchain_state
      ; consensus_state= Consensus.Data.Consensus_state.display consensus_state
      ; parent }

    let to_user_commands
        {transition_with_hash= {data= external_transition; _}; _} =
      let open External_transition.Validated in
      let open Staged_ledger_diff in
      user_commands @@ staged_ledger_diff external_transition
  end

  module Diff_hash = struct
    open Digestif.SHA256

    type nonrec t = t

    include Binable.Of_stringable (struct
      type nonrec t = t

      let of_string = of_hex

      let to_string = to_hex
    end)

    let equal t1 t2 = equal t1 t2

    let empty = digest_string ""

    let merge t1 string = digestv_string [to_hex t1; string]

    let to_string = to_raw_string
  end

  module Diff_mutant = struct
    module Key = struct
      module New_frontier = struct
        (* TODO: version *)
        type t =
          ( External_transition.Validated.Stable.V1.t
          , State_hash.Stable.V1.t )
          With_hash.Stable.V1.t
          * Staged_ledger.Scan_state.Stable.V1.t
          * Pending_coinbase.Stable.V1.t
        [@@deriving bin_io]
      end

      module Add_transition = struct
        (* TODO: version *)
        type t =
          ( External_transition.Validated.Stable.V1.t
          , State_hash.Stable.V1.t )
          With_hash.Stable.V1.t
        [@@deriving bin_io]
      end

      module Update_root = struct
        (* TODO: version *)
        type t =
          State_hash.Stable.V1.t
          * Staged_ledger.Scan_state.Stable.V1.t
          * Pending_coinbase.Stable.V1.t
        [@@deriving bin_io]
      end
    end

    type _ t =
      | New_frontier : Key.New_frontier.t -> unit t
      | Add_transition :
          Key.Add_transition.t
          -> Consensus.Data.Consensus_state.Value.Stable.V1.t t
      | Remove_transitions :
          ( External_transition.Validated.Stable.V1.t
          , State_hash.Stable.V1.t )
          With_hash.Stable.V1.t
          list
          -> Consensus.Data.Consensus_state.Value.Stable.V1.t list t
      | Update_root :
          Key.Update_root.t
          -> ( State_hash.Stable.V1.t
             * Staged_ledger.Scan_state.Stable.V1.t
             * Pending_coinbase.t )
             t

    type 'a diff_mutant = 'a t

    let serialize_consensus_state =
      Binable.to_string (module Consensus.Data.Consensus_state.Value.Stable.V1)

    let json_consensus_state consensus_state =
      Consensus.Data.Consensus_state.(
        display_to_yojson @@ display consensus_state)

    let name : type a. a t -> string = function
      | New_frontier _ ->
          "New_frontier"
      | Add_transition _ ->
          "Add_transition"
      | Remove_transitions _ ->
          "Remove_transitions"
      | Update_root _ ->
          "Update_root"

    let update_root_to_yojson (state_hash, scan_state, pending_coinbase) =
      (* We need some representation of scan_state and pending_coinbase,
        so the serialized version of these states would be fine *)
      `Assoc
        [ ("state_hash", State_hash.to_yojson state_hash)
        ; ( "scan_state"
          , `Int
              ( String.hash
              @@ Binable.to_string
                   (module Staged_ledger.Scan_state.Stable.V1)
                   scan_state ) )
        ; ( "pending_coinbase"
          , `Int
              ( String.hash
              @@ Binable.to_string
                   (module Pending_coinbase.Stable.V1)
                   pending_coinbase ) ) ]

    (* Yojson is not performant and should be turned off *)
    let value_to_yojson (type a) (key : a t) (value : a) =
      let json_value =
        match (key, value) with
        | New_frontier _, () ->
            `Null
        | Add_transition _, parent_consensus_state ->
            json_consensus_state parent_consensus_state
        | Remove_transitions _, removed_consensus_state ->
            `List (List.map removed_consensus_state ~f:json_consensus_state)
        | Update_root _, (old_state_hash, old_scan_state, old_pending_coinbase)
          ->
            update_root_to_yojson
              (old_state_hash, old_scan_state, old_pending_coinbase)
      in
      `List [`String (name key); json_value]

    let key_to_yojson (type a) (key : a t) =
      let json_key =
        match key with
        | New_frontier (With_hash.{hash; _}, _, _) ->
            State_hash.to_yojson hash
        | Add_transition With_hash.{hash; _} ->
            State_hash.to_yojson hash
        | Remove_transitions removed_transitions ->
            `List
              (List.map removed_transitions ~f:(fun With_hash.{hash; _} ->
                   State_hash.to_yojson hash ))
        | Update_root (state_hash, scan_state, pending_coinbase) ->
            update_root_to_yojson (state_hash, scan_state, pending_coinbase)
      in
      `List [`String (name key); json_key]

    let merge = Fn.flip Diff_hash.merge

    let hash_root_data (hash, scan_state, pending_coinbase) acc =
      merge
        ( Bin_prot.Utils.bin_dump
            [%bin_type_class:
              State_hash.Stable.V1.t
              * Staged_ledger.Scan_state.Stable.V1.t
              * Pending_coinbase.Stable.V1.t]
              .writer
            (hash, scan_state, pending_coinbase)
        |> Bigstring.to_string )
        acc

    let hash_diff_contents (type mutant) (t : mutant t) acc =
      match t with
      | New_frontier ({With_hash.hash; _}, scan_state, pending_coinbase) ->
          hash_root_data (hash, scan_state, pending_coinbase) acc
      | Add_transition {With_hash.hash; _} ->
          Diff_hash.merge acc (State_hash.to_bytes hash)
      | Remove_transitions removed_transitions ->
          List.fold removed_transitions ~init:acc
            ~f:(fun acc_hash With_hash.{hash= state_hash; _} ->
              Diff_hash.merge acc_hash (State_hash.to_bytes state_hash) )
      | Update_root (new_hash, new_scan_state, pending_coinbase) ->
          hash_root_data (new_hash, new_scan_state, pending_coinbase) acc

    let hash_mutant (type mutant) (t : mutant t) (mutant : mutant) acc =
      match (t, mutant) with
      | New_frontier _, () ->
          acc
      | Add_transition _, parent_external_transition ->
          merge (serialize_consensus_state parent_external_transition) acc
      | Remove_transitions _, removed_transitions ->
          List.fold removed_transitions ~init:acc
            ~f:(fun acc_hash removed_transition ->
              merge (serialize_consensus_state removed_transition) acc_hash )
      | Update_root _, (old_root, old_scan_state, old_pending_coinbase) ->
          hash_root_data (old_root, old_scan_state, old_pending_coinbase) acc

    let hash (type mutant) acc_hash (t : mutant t) (mutant : mutant) =
      let diff_contents_hash = hash_diff_contents t acc_hash in
      hash_mutant t mutant diff_contents_hash

    module E = struct
      type t = E : 'output diff_mutant -> t

      (* HACK:  This makes the existential type easily binable *)
      include Binable.Of_binable (struct
                  type t =
                    [ `New_frontier of Key.New_frontier.t
                    | `Add_transition of Key.Add_transition.t
                    | `Remove_transitions of
                      ( External_transition.Validated.Stable.V1.t
                      , State_hash.Stable.V1.t )
                      With_hash.Stable.V1.t
                      list
                    | `Update_root of Key.Update_root.t ]
                  [@@deriving bin_io]
                end)
                (struct
                  type nonrec t = t

                  let of_binable = function
                    | `New_frontier data ->
                        E (New_frontier data)
                    | `Add_transition data ->
                        E (Add_transition data)
                    | `Remove_transitions transitions ->
                        E (Remove_transitions transitions)
                    | `Update_root data ->
                        E (Update_root data)

                  let to_binable = function
                    | E (New_frontier data) ->
                        `New_frontier data
                    | E (Add_transition data) ->
                        `Add_transition data
                    | E (Remove_transitions transitions) ->
                        `Remove_transitions transitions
                    | E (Update_root data) ->
                        `Update_root data
                end)
    end
  end

  module Fake_db = struct
    include Coda_base.Ledger.Db

    type location = Location.t

    let get_or_create ledger key =
      let key, loc =
        match
          get_or_create_account_exn ledger key (Account.initialize key)
        with
        | `Existed, loc ->
            ([], loc)
        | `Added, loc ->
            ([key], loc)
      in
      (key, get ledger loc |> Option.value_exn, loc)
  end

  module TL = Coda_base.Transaction_logic.Make (Fake_db)

  module type Transition_frontier_extension_intf =
    Transition_frontier_extension_intf0
    with type transition_frontier_breadcrumb := Breadcrumb.t

  let max_length = max_length

  module Extensions = struct
    module Work = Transaction_snark_work.Statement

    module Snark_pool_refcount = Snark_pool_refcount.Make (struct
      include Inputs
      module Breadcrumb = Breadcrumb
    end)

    module Root_history = struct
      module Queue = Hash_queue.Make (State_hash)

      type t =
        { history: Breadcrumb.t Queue.t
        ; capacity: int
        ; mutable most_recent: Breadcrumb.t option }

      let create capacity =
        let history = Queue.create () in
        {history; capacity; most_recent= None}

      let lookup {history; _} = Queue.lookup history

      let most_recent {most_recent; _} = most_recent

      let mem {history; _} = Queue.mem history

      let enqueue ({history; capacity; _} as t) state_hash breadcrumb =
        if Queue.length history >= capacity then
          Queue.dequeue_front_exn history |> ignore ;
        Queue.enqueue_back history state_hash breadcrumb |> ignore ;
        t.most_recent <- Some breadcrumb

      let is_empty {history; _} = Queue.is_empty history
    end

    (* TODO: guard against waiting for transitions that already exist in the frontier *)
    module Transition_registry = struct
      type t = unit Ivar.t list State_hash.Table.t

      let create () = State_hash.Table.create ()

      let notify t state_hash =
        State_hash.Table.change t state_hash ~f:(function
          | Some ls ->
              List.iter ls ~f:(Fn.flip Ivar.fill ()) ;
              None
          | None ->
              None )

      let register t state_hash =
        Deferred.create (fun ivar ->
            State_hash.Table.update t state_hash ~f:(function
              | Some ls ->
                  ivar :: ls
              | None ->
                  [ivar] ) )
    end

    (** A transition frontier extension that exposes the changes in the transactions
        in the best tip. *)
    module Persistence_diff = struct
      type t = unit

      type input = unit

      type view = Diff_mutant.E.t list

      let create () = ()

      let initial_view () = []

      let scan_state breadcrumb =
        breadcrumb |> Breadcrumb.staged_ledger |> Staged_ledger.scan_state

      let pending_coinbase breadcrumb =
        breadcrumb |> Breadcrumb.staged_ledger
        |> Staged_ledger.pending_coinbase_collection

      let handle_diff () (diff : Breadcrumb.t Transition_frontier_diff.t) :
          view option =
        let open Transition_frontier_diff in
        let open Diff_mutant.E in
        Option.return
        @@
        match diff with
        | New_frontier breadcrumb ->
            [ E
                (New_frontier
                   ( Breadcrumb.transition_with_hash breadcrumb
                   , scan_state breadcrumb
                   , pending_coinbase breadcrumb )) ]
        | New_breadcrumb breadcrumb ->
            [E (Add_transition (Breadcrumb.transition_with_hash breadcrumb))]
        | New_best_tip {garbage; added_to_best_tip_path; new_root; old_root; _}
          ->
            let added_transition =
              E
                (Add_transition
                   ( Non_empty_list.last added_to_best_tip_path
                   |> Breadcrumb.transition_with_hash ))
            in
            let remove_transition =
              E
                (Remove_transitions
                   (List.map garbage ~f:Breadcrumb.transition_with_hash))
            in
            if
              State_hash.equal
                (Breadcrumb.state_hash old_root)
                (Breadcrumb.state_hash new_root)
            then [added_transition; remove_transition]
            else
              [ added_transition
              ; E
                  (Update_root
                     ( Breadcrumb.state_hash new_root
                     , scan_state new_root
                     , pending_coinbase new_root ))
              ; remove_transition ]
    end

    module Best_tip_diff = Best_tip_diff.Make (Breadcrumb)
    module Root_diff = Root_diff.Make (Breadcrumb)

    type t =
      { root_history: Root_history.t
      ; snark_pool_refcount: Snark_pool_refcount.t
      ; transition_registry: Transition_registry.t
      ; best_tip_diff: Best_tip_diff.t
      ; root_diff: Root_diff.t
      ; persistence_diff: Persistence_diff.t
      ; new_transition: External_transition.Validated.t New_transition.Var.t }
    [@@deriving fields]

    (* TODO: Each of these extensions should be created with the input of the breadcrumb *)
    let create root_breadcrumb =
      let new_transition =
        New_transition.Var.create
          (Breadcrumb.external_transition root_breadcrumb)
      in
      { root_history= Root_history.create (2 * max_length)
      ; snark_pool_refcount= Snark_pool_refcount.create ()
      ; transition_registry= Transition_registry.create ()
      ; best_tip_diff= Best_tip_diff.create ()
      ; root_diff= Root_diff.create ()
      ; persistence_diff= Persistence_diff.create ()
      ; new_transition }

    type writers =
      { snark_pool: Snark_pool_refcount.view Broadcast_pipe.Writer.t
      ; best_tip_diff: Best_tip_diff.view Broadcast_pipe.Writer.t
      ; root_diff: Root_diff.view Broadcast_pipe.Writer.t
      ; persistence_diff: Persistence_diff.view Broadcast_pipe.Writer.t }

    type readers =
      { snark_pool: Snark_pool_refcount.view Broadcast_pipe.Reader.t
      ; best_tip_diff: Best_tip_diff.view Broadcast_pipe.Reader.t
      ; root_diff: Root_diff.view Broadcast_pipe.Reader.t
      ; persistence_diff: Persistence_diff.view Broadcast_pipe.Reader.t }
    [@@deriving fields]

    let make_pipes () : readers * writers =
      let snark_reader, snark_writer =
        Broadcast_pipe.create (Snark_pool_refcount.initial_view ())
      and best_tip_reader, best_tip_writer =
        Broadcast_pipe.create (Best_tip_diff.initial_view ())
      and root_diff_reader, root_diff_writer =
        Broadcast_pipe.create (Root_diff.initial_view ())
      and persistence_diff_reader, persistence_diff_writer =
        Broadcast_pipe.create (Persistence_diff.initial_view ())
      in
      ( { snark_pool= snark_reader
        ; best_tip_diff= best_tip_reader
        ; root_diff= root_diff_reader
        ; persistence_diff= persistence_diff_reader }
      , { snark_pool= snark_writer
        ; best_tip_diff= best_tip_writer
        ; root_diff= root_diff_writer
        ; persistence_diff= persistence_diff_writer } )

    let close_pipes
        ({snark_pool; best_tip_diff; root_diff; persistence_diff} : writers) =
      Broadcast_pipe.Writer.close snark_pool ;
      Broadcast_pipe.Writer.close best_tip_diff ;
      Broadcast_pipe.Writer.close root_diff ;
      Broadcast_pipe.Writer.close persistence_diff

    let mb_write_to_pipe diff ext_t handle pipe =
      Option.value ~default:Deferred.unit
      @@ Option.map ~f:(Broadcast_pipe.Writer.write pipe) (handle ext_t diff)

    let handle_diff t (pipes : writers)
        (diff : Breadcrumb.t Transition_frontier_diff.t) : unit Deferred.t =
      let use handler pipe acc field =
        let%bind () = acc in
        mb_write_to_pipe diff (Field.get field t) handler pipe
      in
      ( match diff with
      | Transition_frontier_diff.New_best_tip {old_root; new_root; _} ->
          if not (Breadcrumb.equal old_root new_root) then
            Root_history.enqueue t.root_history
              (Breadcrumb.state_hash old_root)
              old_root
      | _ ->
          () ) ;
      let%map () =
        Fields.fold ~init:diff
          ~root_history:(fun _ _ -> Deferred.unit)
          ~snark_pool_refcount:
            (use Snark_pool_refcount.handle_diff pipes.snark_pool)
          ~transition_registry:(fun acc _ -> acc)
          ~best_tip_diff:(use Best_tip_diff.handle_diff pipes.best_tip_diff)
          ~root_diff:(use Root_diff.handle_diff pipes.root_diff)
          ~persistence_diff:
            (use Persistence_diff.handle_diff pipes.persistence_diff)
          ~new_transition:(fun acc _ -> acc)
      in
      let bc_opt =
        match diff with
        | New_breadcrumb bc ->
            Some bc
        | New_best_tip {added_to_best_tip_path; _} ->
            Some (Non_empty_list.last added_to_best_tip_path)
        | _ ->
            None
      in
      Option.iter bc_opt ~f:(fun bc ->
          (* Other components may be waiting on these, so it's important they're
             updated after the views above so that those other components see
             the views updated with the new breadcrumb. *)
          Transition_registry.notify t.transition_registry
            (Breadcrumb.state_hash bc) ;
          New_transition.Var.set t.new_transition
          @@ Breadcrumb.external_transition bc ;
          New_transition.stabilize () )
  end

  module Node = struct
    type t =
      { breadcrumb: Breadcrumb.t
      ; successor_hashes: State_hash.t list
      ; length: int }
    [@@deriving sexp, fields]

    type display =
      { length: int
      ; state_hash: string
      ; blockchain_state: Blockchain_state.display
      ; consensus_state: Consensus.Data.Consensus_state.display }
    [@@deriving yojson]

    let equal node1 node2 = Breadcrumb.equal node1.breadcrumb node2.breadcrumb

    let hash node = Breadcrumb.hash node.breadcrumb

    let compare node1 node2 =
      Breadcrumb.compare node1.breadcrumb node2.breadcrumb

    let name t = Breadcrumb.name t.breadcrumb

    let display t =
      let {Breadcrumb.state_hash; consensus_state; blockchain_state; _} =
        Breadcrumb.display t.breadcrumb
      in
      {state_hash; blockchain_state; length= t.length; consensus_state}
  end

  let breadcrumb_of_node {Node.breadcrumb; _} = breadcrumb

  (* Invariant: The path from the root to the tip inclusively, will be max_length + 1 *)
  (* TODO: Make a test of this invariant *)
  type t =
    { root_snarked_ledger: Ledger.Db.t
    ; mutable root: State_hash.t
    ; mutable best_tip: State_hash.t
    ; logger: Logger.t
    ; table: Node.t State_hash.Table.t
    ; consensus_local_state: Consensus.Data.Local_state.t
    ; extensions: Extensions.t
    ; extension_readers: Extensions.readers
    ; extension_writers: Extensions.writers }

  let logger t = t.logger

  let snark_pool_refcount_pipe {extension_readers; _} =
    extension_readers.snark_pool

  let best_tip_diff_pipe {extension_readers; _} =
    extension_readers.best_tip_diff

  let root_diff_pipe {extension_readers; _} = extension_readers.root_diff

  let persistence_diff_pipe {extension_readers; _} =
    extension_readers.persistence_diff

  let new_transition {extensions; _} =
    let new_transition_incr =
      New_transition.Var.watch extensions.new_transition
    in
    New_transition.stabilize () ;
    new_transition_incr

  (* TODO: load from and write to disk *)
  let create ~logger
      ~(root_transition :
         (External_transition.Validated.t, State_hash.t) With_hash.t)
      ~root_snarked_ledger ~root_staged_ledger ~consensus_local_state =
    let root_hash = With_hash.hash root_transition in
    let root_protocol_state =
      External_transition.Validated.protocol_state
        (With_hash.data root_transition)
    in
    let root_blockchain_state =
      Protocol_state.blockchain_state root_protocol_state
    in
    let root_blockchain_state_ledger_hash =
      Blockchain_state.snarked_ledger_hash root_blockchain_state
    in
    assert (
      Ledger_hash.equal
        (Ledger.Db.merkle_root root_snarked_ledger)
        (Frozen_ledger_hash.to_ledger_hash root_blockchain_state_ledger_hash)
    ) ;
    let root_breadcrumb =
      { Breadcrumb.transition_with_hash= root_transition
      ; staged_ledger= root_staged_ledger
      ; just_emitted_a_proof= false }
    in
    let root_node =
      {Node.breadcrumb= root_breadcrumb; successor_hashes= []; length= 0}
    in
    let table = State_hash.Table.of_alist_exn [(root_hash, root_node)] in
    let extension_readers, extension_writers = Extensions.make_pipes () in
    let t =
      { logger
      ; root_snarked_ledger
      ; root= root_hash
      ; best_tip= root_hash
      ; table
      ; consensus_local_state
      ; extensions= Extensions.create root_breadcrumb
      ; extension_readers
      ; extension_writers }
    in
    let%map () =
      Extensions.handle_diff t.extensions t.extension_writers
        (Transition_frontier_diff.New_frontier root_breadcrumb)
    in
    t

  let close {extension_writers; _} = Extensions.close_pipes extension_writers

  let consensus_local_state {consensus_local_state; _} = consensus_local_state

  let all_breadcrumbs t =
    List.map (Hashtbl.data t.table) ~f:(fun {breadcrumb; _} -> breadcrumb)

  let find t hash =
    let open Option.Let_syntax in
    let%map node = Hashtbl.find t.table hash in
    node.breadcrumb

  let find_exn t hash =
    let node = Hashtbl.find_exn t.table hash in
    node.breadcrumb

  let find_in_root_history t hash =
    Extensions.Root_history.lookup t.extensions.root_history hash

  let path_search t state_hash ~find ~f =
    let open Option.Let_syntax in
    let rec go state_hash =
      let%map breadcrumb = find t state_hash in
      let elem = f breadcrumb in
      match go (Breadcrumb.parent_hash breadcrumb) with
      | Some subresult ->
          Non_empty_list.cons elem subresult
      | None ->
          Non_empty_list.singleton elem
    in
    Option.map ~f:Non_empty_list.rev (go state_hash)

  let previous_root t =
    Extensions.Root_history.most_recent t.extensions.root_history

  let get_path_inclusively_in_root_history t state_hash ~f =
    path_search t state_hash
      ~find:(fun t -> Extensions.Root_history.lookup t.extensions.root_history)
      ~f

  let root_history_path_map t state_hash ~f =
    let open Option.Let_syntax in
    match path_search t ~find ~f state_hash with
    | None ->
        get_path_inclusively_in_root_history t state_hash ~f
    | Some frontier_path ->
        let root_history_path =
          let%bind root_breadcrumb = find t t.root in
          get_path_inclusively_in_root_history t
            (Breadcrumb.parent_hash root_breadcrumb)
            ~f
        in
        Some
          (Option.value_map root_history_path ~default:frontier_path
             ~f:(fun root_history ->
               Non_empty_list.append root_history frontier_path ))

  let path_map t breadcrumb ~f =
    let rec find_path b =
      let elem = f b in
      let parent_hash = Breadcrumb.parent_hash b in
      if State_hash.equal (Breadcrumb.state_hash b) t.root then []
      else if State_hash.equal parent_hash t.root then [elem]
      else elem :: find_path (find_exn t parent_hash)
    in
    List.rev (find_path breadcrumb)

  let hash_path t breadcrumb = path_map t breadcrumb ~f:Breadcrumb.state_hash

  let iter t ~f = Hashtbl.iter t.table ~f:(fun n -> f n.breadcrumb)

  let root t = find_exn t t.root

  let root_length t = (Hashtbl.find_exn t.table t.root).length

  let best_tip t = find_exn t t.best_tip

  let successor_hashes t hash =
    let node = Hashtbl.find_exn t.table hash in
    node.successor_hashes

  let rec successor_hashes_rec t hash =
    List.bind (successor_hashes t hash) ~f:(fun succ_hash ->
        succ_hash :: successor_hashes_rec t succ_hash )

  let successors t breadcrumb =
    List.map
      (successor_hashes t (Breadcrumb.state_hash breadcrumb))
      ~f:(find_exn t)

  let rec successors_rec t breadcrumb =
    List.bind (successors t breadcrumb) ~f:(fun succ ->
        succ :: successors_rec t succ )

  (* Visualize the structure of the transition frontier or a particular node
   * within the frontier (for debugging purposes). *)
  module Visualizor = struct
    let fold t ~f = Hashtbl.fold t.table ~f:(fun ~key:_ ~data -> f data)

    include Visualization.Make_ocamlgraph (Node)

    let to_graph t =
      fold t ~init:empty ~f:(fun (node : Node.t) graph ->
          let graph_with_node = add_vertex graph node in
          List.fold node.successor_hashes ~init:graph_with_node
            ~f:(fun acc_graph successor_state_hash ->
              match State_hash.Table.find t.table successor_state_hash with
              | Some child_node ->
                  add_edge acc_graph node child_node
              | None ->
                  Logger.info t.logger ~module_:__MODULE__ ~location:__LOC__
                    ~metadata:
                      [ ( "state_hash"
                        , State_hash.to_yojson successor_state_hash ) ]
                    "Could not visualize node $state_hash. Looks like the \
                     node did not get garbage collected properly" ;
                  acc_graph ) )
  end

  let visualize ~filename (t : t) =
    Out_channel.with_file filename ~f:(fun output_channel ->
        let graph = Visualizor.to_graph t in
        Visualizor.output_graph output_channel graph )

  let visualize_to_string t =
    let graph = Visualizor.to_graph t in
    let buf = Buffer.create 0 in
    let formatter = Format.formatter_of_buffer buf in
    Visualizor.fprint_graph formatter graph ;
    Format.pp_print_flush formatter () ;
    Buffer.contents buf

  let attach_node_to t ~(parent_node : Node.t) ~(node : Node.t) =
    let hash = Breadcrumb.state_hash (Node.breadcrumb node) in
    let parent_hash = Breadcrumb.state_hash parent_node.breadcrumb in
    if
      not
        (State_hash.equal parent_hash (Breadcrumb.parent_hash node.breadcrumb))
    then
      failwith
        "invalid call to attach_to: hash parent_node <> parent_hash node" ;
    (* We only want to update the parent node if we don't have a dupe *)
    Hashtbl.change t.table hash ~f:(function
      | Some x ->
          Logger.warn t.logger ~module_:__MODULE__ ~location:__LOC__
            ~metadata:[("state_hash", State_hash.to_yojson hash)]
            "attach_node_to with breadcrumb for state $state_hash already \
             present; catchup scheduler bug?" ;
          Some x
      | None ->
          Hashtbl.set t.table ~key:parent_hash
            ~data:
              { parent_node with
                successor_hashes= hash :: parent_node.successor_hashes } ;
          Some node )

  let attach_breadcrumb_exn t breadcrumb =
    let hash = Breadcrumb.state_hash breadcrumb in
    let parent_hash = Breadcrumb.parent_hash breadcrumb in
    let parent_node =
      Option.value_exn
        (Hashtbl.find t.table parent_hash)
        ~error:
          (Error.of_exn (Parent_not_found (`Parent parent_hash, `Target hash)))
    in
    let node =
      {Node.breadcrumb; successor_hashes= []; length= parent_node.length + 1}
    in
    attach_node_to t ~parent_node ~node

  (** Given:
   *
   *        o                   o
   *       /                   /
   *    o ---- o --------------
   *    t  \ soon_to_be_root   \
   *        o                   o
   *                        children
   *
   *  Delegates up to Staged_ledger reparent and makes the
   *  modifies the heir's staged-ledger and sets the heir as the new root.
   *  Modifications are in-place
  *)
  let move_root t (soon_to_be_root_node : Node.t) : Node.t =
    let root_node = Hashtbl.find_exn t.table t.root in
    let root_breadcrumb = root_node.breadcrumb in
    let root = root_breadcrumb |> Breadcrumb.staged_ledger in
    let soon_to_be_root =
      soon_to_be_root_node.breadcrumb |> Breadcrumb.staged_ledger
    in
    let children =
      List.map soon_to_be_root_node.successor_hashes ~f:(fun h ->
          (Hashtbl.find_exn t.table h).breadcrumb |> Breadcrumb.staged_ledger
          |> Staged_ledger.ledger )
    in
    let root_ledger = Staged_ledger.ledger root in
    let soon_to_be_root_ledger = Staged_ledger.ledger soon_to_be_root in
    let soon_to_be_root_merkle_root =
      Ledger.merkle_root soon_to_be_root_ledger
    in
    Ledger.commit soon_to_be_root_ledger ;
    let root_ledger_merkle_root_after_commit =
      Ledger.merkle_root root_ledger
    in
    [%test_result: Ledger_hash.t]
      ~message:
        "Merkle root of soon-to-be-root before commit, is same as root \
         ledger's merkle root afterwards"
      ~expect:soon_to_be_root_merkle_root root_ledger_merkle_root_after_commit ;
    let new_root =
      Breadcrumb.create soon_to_be_root_node.breadcrumb.transition_with_hash
        (Staged_ledger.replace_ledger_exn soon_to_be_root root_ledger)
    in
    let new_root_node = {soon_to_be_root_node with breadcrumb= new_root} in
    let new_root_hash =
      soon_to_be_root_node.breadcrumb.transition_with_hash.hash
    in
    Ledger.remove_and_reparent_exn soon_to_be_root_ledger
      soon_to_be_root_ledger ~children ;
    Hashtbl.remove t.table t.root ;
    Hashtbl.set t.table ~key:new_root_hash ~data:new_root_node ;
    t.root <- new_root_hash ;
    new_root_node

  let common_ancestor t (bc1 : Breadcrumb.t) (bc2 : Breadcrumb.t) :
      State_hash.t =
    let rec go ancestors1 ancestors2 sh1 sh2 =
      Hash_set.add ancestors1 sh1 ;
      Hash_set.add ancestors2 sh2 ;
      if Hash_set.mem ancestors1 sh2 then sh2
      else if Hash_set.mem ancestors2 sh1 then sh1
      else
        let parent_unless_root sh =
          if State_hash.equal sh t.root then sh
          else find_exn t sh |> Breadcrumb.parent_hash
        in
        go ancestors1 ancestors2 (parent_unless_root sh1)
          (parent_unless_root sh2)
    in
    go
      (Hash_set.create (module State_hash) ())
      (Hash_set.create (module State_hash) ())
      (Breadcrumb.state_hash bc1)
      (Breadcrumb.state_hash bc2)

  (* Get the breadcrumbs that are on bc1's path but not bc2's, and vice versa.
     Ordered oldest to newest.
  *)
  let get_path_diff t (bc1 : Breadcrumb.t) (bc2 : Breadcrumb.t) :
      Breadcrumb.t list * Breadcrumb.t list =
    let ancestor = common_ancestor t bc1 bc2 in
    (* Find the breadcrumbs connecting bc1 and bc2, excluding bc1. Precondition:
       bc1 is an ancestor of bc2. *)
    let path_from_to bc1 bc2 =
      let rec go cursor acc =
        if Breadcrumb.equal cursor bc1 then acc
        else go (find_exn t @@ Breadcrumb.parent_hash cursor) (cursor :: acc)
      in
      go bc2 []
    in
    Logger.debug t.logger ~module_:__MODULE__ ~location:__LOC__
      !"Common ancestor: %{sexp: State_hash.t}"
      ancestor ;
    ( path_from_to (find_exn t ancestor) bc1
    , path_from_to (find_exn t ancestor) bc2 )

  (* Adding a breadcrumb to the transition frontier is broken into the following steps:
   *   1) attach the breadcrumb to the transition frontier
   *   2) calculate the distance from the new node to the parent and the
   *      best tip node
   *   3) set the new node as the best tip if the new node has a greater length than
   *      the current best tip
   *   4) move the root if the path to the new node is longer than the max length
   *       I   ) find the immediate successor of the old root in the path to the
   *             longest node (the heir)
   *       II  ) find all successors of the other immediate successors of the
   *             old root (bads)
   *       III ) cleanup bad node masks, but don't garbage collect yet
   *       IV  ) move_root the breadcrumbs (rewires staged ledgers, cleans up heir)
   *       V   ) garbage collect the bads
   *       VI  ) grab the new root staged ledger
   *       VII ) notify the consensus mechanism of the new root
   *       VIII) if commit on an heir node that just emitted proof txns then
   *             write them to snarked ledger
   *       XI  ) add old root to root_history
   *   5) return a diff object describing what changed (for use in updating extensions)
  *)
  let add_breadcrumb_exn t breadcrumb =
    O1trace.measure "add_breadcrumb" (fun () ->
        let consensus_state_of_breadcrumb b =
          Breadcrumb.transition_with_hash b
          |> With_hash.data |> External_transition.Validated.protocol_state
          |> Protocol_state.consensus_state
        in
        let hash =
          With_hash.hash (Breadcrumb.transition_with_hash breadcrumb)
        in
        let root_node = Hashtbl.find_exn t.table t.root in
        let old_best_tip = best_tip t in
        let local_state_was_synced_at_start =
          Consensus.Hooks.required_local_state_sync
            ~consensus_state:(consensus_state_of_breadcrumb old_best_tip)
            ~local_state:t.consensus_local_state
          |> Option.is_none
        in
        (* 1 *)
        attach_breadcrumb_exn t breadcrumb ;
        let parent_hash = Breadcrumb.parent_hash breadcrumb in
        let parent_node =
          Option.value_exn
            (Hashtbl.find t.table parent_hash)
            ~error:
              (Error.of_exn
                 (Parent_not_found (`Parent parent_hash, `Target hash)))
        in
        Debug_assert.debug_assert (fun () ->
            (* if the proof verified, then this should always hold*)
            assert (
              Consensus.Hooks.select
                ~existing:
                  (consensus_state_of_breadcrumb parent_node.breadcrumb)
                ~candidate:(consensus_state_of_breadcrumb breadcrumb)
                ~logger:
                  (Logger.extend t.logger
                     [ ( "selection_context"
                       , `String
                           "debug_assert that child is preferred over parent"
                       ) ])
              = `Take ) ) ;
        let node = Hashtbl.find_exn t.table hash in
        (* 2 *)
        let distance_to_parent = node.length - root_node.length in
        let best_tip_node = Hashtbl.find_exn t.table t.best_tip in
        (* 3 *)
        let best_tip_change =
          Consensus.Hooks.select
            ~existing:(consensus_state_of_breadcrumb best_tip_node.breadcrumb)
            ~candidate:(consensus_state_of_breadcrumb node.breadcrumb)
            ~logger:
              (Logger.extend t.logger
                 [ ( "selection_context"
                   , `String "comparing new breadcrumb to best tip" ) ])
        in
        let added_to_best_tip_path, removed_from_best_tip_path =
          match best_tip_change with
          | `Keep ->
              ([], [])
          | `Take ->
              t.best_tip <- hash ;
              get_path_diff t breadcrumb best_tip_node.breadcrumb
        in
        Logger.debug t.logger ~module_:__MODULE__ ~location:__LOC__
          "added %d breadcrumbs and removed %d making path to new best tip"
          (List.length added_to_best_tip_path)
          (List.length removed_from_best_tip_path)
          ~metadata:
            [ ( "new_breadcrumbs"
              , `List (List.map ~f:Breadcrumb.to_yojson added_to_best_tip_path)
              )
            ; ( "old_breadcrumbs"
              , `List
                  (List.map ~f:Breadcrumb.to_yojson removed_from_best_tip_path)
              ) ] ;
        (* 4 *)
        (* note: new_root_node is the same as root_node if the root didn't change *)
        let garbage_breadcrumbs, new_root_node =
          if distance_to_parent > max_length then (
            Logger.info t.logger ~module_:__MODULE__ ~location:__LOC__
              !"Distance to parent: %d exceeded max_lenth %d"
              distance_to_parent max_length ;
            (* 4.I *)
            let heir_hash = List.hd_exn (hash_path t node.breadcrumb) in
            let heir_node = Hashtbl.find_exn t.table heir_hash in
            (* 4.II *)
            let bad_hashes =
              List.filter root_node.successor_hashes
                ~f:(Fn.compose not (State_hash.equal heir_hash))
            in
            let bad_nodes =
              List.map bad_hashes ~f:(Hashtbl.find_exn t.table)
            in
            (* 4.III *)
            let root_staged_ledger =
              Breadcrumb.staged_ledger root_node.breadcrumb
            in
            let root_ledger = Staged_ledger.ledger root_staged_ledger in
            List.map bad_nodes ~f:breadcrumb_of_node
            |> List.iter ~f:(fun bad ->
                   ignore
                     (Ledger.unregister_mask_exn root_ledger
                        (Breadcrumb.staged_ledger bad |> Staged_ledger.ledger))
               ) ;
            (* 4.IV *)
            let new_root_node = move_root t heir_node in
            (* 4.V *)
            let garbage =
              bad_hashes @ List.bind bad_hashes ~f:(successor_hashes_rec t)
            in
            Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
              ~metadata:
                [ ("garbage", `List (List.map garbage ~f:State_hash.to_yojson))
                ; ("length_of_garbage", `Int (List.length garbage))
                ; ( "bad_hashes"
                  , `List (List.map bad_hashes ~f:State_hash.to_yojson) ) ]
              "collecting $length_of_garbage nodes rooted from $bad_hashes" ;
            let garbage_breadcrumbs =
              List.map garbage ~f:(fun g ->
                  (Hashtbl.find_exn t.table g).breadcrumb )
            in
            List.iter garbage ~f:(Hashtbl.remove t.table) ;
            (* 4.VI *)
            let new_root_staged_ledger =
              Breadcrumb.staged_ledger new_root_node.breadcrumb
            in
            (* 4.VII *)
            Consensus.Hooks.lock_transition
              (Breadcrumb.consensus_state root_node.breadcrumb)
              (Breadcrumb.consensus_state new_root_node.breadcrumb)
              ~local_state:t.consensus_local_state
              ~snarked_ledger:
                (Coda_base.Ledger.Any_ledger.cast
                   (module Coda_base.Ledger.Db)
                   t.root_snarked_ledger) ;
            Debug_assert.debug_assert (fun () ->
                (* After the lock transition, if the local_state was previously synced, it should continue to be synced *)
                match
                  Consensus.Hooks.required_local_state_sync
                    ~consensus_state:
                      (consensus_state_of_breadcrumb
                         (Hashtbl.find_exn t.table t.best_tip).breadcrumb)
                    ~local_state:t.consensus_local_state
                with
                | Some jobs ->
                    (* But if there wasn't sync work to do when we started, then there shouldn't be now. *)
                    if local_state_was_synced_at_start then (
                      Logger.fatal t.logger
                        "after lock transition, the best tip consensus state \
                         is out of sync with the local state -- bug in either \
                         required_local_state_sync or lock_transition."
                        ~module_:__MODULE__ ~location:__LOC__
                        ~metadata:
                          [ ( "sync_jobs"
                            , `List
                                ( Non_empty_list.to_list jobs
                                |> List.map
                                     ~f:
                                       Consensus.Hooks
                                       .local_state_sync_to_yojson ) )
                          ; ( "local_state"
                            , Consensus.Data.Local_state.to_yojson
                                t.consensus_local_state )
                          ; ("tf_viz", `String (visualize_to_string t)) ] ;
                      assert false )
                | None ->
                    () ) ;
            (* 4.VIII *)
            ( match
                ( Staged_ledger.proof_txns new_root_staged_ledger
                , heir_node.breadcrumb.just_emitted_a_proof )
              with
            | Some txns, true ->
                let proof_data =
                  Staged_ledger.current_ledger_proof new_root_staged_ledger
                  |> Option.value_exn
                in
                [%test_result: Frozen_ledger_hash.t]
                  ~message:
                    "Root snarked ledger hash should be the same as the \
                     source hash in the proof that was just emitted"
                  ~expect:(Ledger_proof.statement proof_data).source
                  ( Ledger.Db.merkle_root t.root_snarked_ledger
                  |> Frozen_ledger_hash.of_ledger_hash ) ;
                let db_mask = Ledger.of_database t.root_snarked_ledger in
                Non_empty_list.iter txns ~f:(fun txn ->
                    (* TODO: @cmr use the ignore-hash ledger here as well *)
                    TL.apply_transaction t.root_snarked_ledger txn
                    |> Or_error.ok_exn |> ignore ) ;
                (* TODO: See issue #1606 to make this faster *)

                (*Ledger.commit db_mask ;*)
                ignore
                  (Ledger.Maskable.unregister_mask_exn
                     (Ledger.Any_ledger.cast
                        (module Ledger.Db)
                        t.root_snarked_ledger)
                     db_mask)
            | _, false | None, _ ->
                () ) ;
            [%test_result: Frozen_ledger_hash.t]
              ~message:
                "Root snarked ledger hash diverged from blockchain state \
                 after root transition"
              ~expect:
                (Blockchain_state.snarked_ledger_hash
                   (Breadcrumb.blockchain_state new_root_node.breadcrumb))
              ( Ledger.Db.merkle_root t.root_snarked_ledger
              |> Frozen_ledger_hash.of_ledger_hash ) ;
            (* 4.IX *)
            let root_breadcrumb = Node.breadcrumb root_node in
            let root_state_hash = Breadcrumb.state_hash root_breadcrumb in
            Extensions.Root_history.enqueue t.extensions.root_history
              root_state_hash root_breadcrumb ;
            (garbage_breadcrumbs, new_root_node) )
          else ([], root_node)
        in
        (* 5 *)
        Extensions.handle_diff t.extensions t.extension_writers
          ( match best_tip_change with
          | `Keep ->
              Transition_frontier_diff.New_breadcrumb node.breadcrumb
          | `Take ->
              Transition_frontier_diff.New_best_tip
                { old_root= root_node.breadcrumb
                ; old_root_length= root_node.length
                ; new_root= new_root_node.breadcrumb
                ; added_to_best_tip_path=
                    Non_empty_list.of_list_opt added_to_best_tip_path
                    |> Option.value_exn
                ; new_best_tip_length= node.length
                ; removed_from_best_tip_path
                ; garbage= garbage_breadcrumbs } ) )

  let add_breadcrumb_if_present_exn t breadcrumb =
    let parent_hash = Breadcrumb.parent_hash breadcrumb in
    match Hashtbl.find t.table parent_hash with
    | Some _ ->
        add_breadcrumb_exn t breadcrumb
    | None ->
        Logger.warn t.logger ~module_:__MODULE__ ~location:__LOC__
          !"When trying to add breadcrumb, its parent had been removed from \
            transition frontier: %{sexp: State_hash.t}"
          parent_hash ;
        Deferred.unit

  let best_tip_path_length_exn {table; root; best_tip; _} =
    let open Option.Let_syntax in
    let result =
      let%bind best_tip_node = Hashtbl.find table best_tip in
      let%map root_node = Hashtbl.find table root in
      best_tip_node.length - root_node.length
    in
    result |> Option.value_exn

  let shallow_copy_root_snarked_ledger {root_snarked_ledger; _} =
    Ledger.of_database root_snarked_ledger

  let wait_for_transition t target_hash =
    if Hashtbl.mem t.table target_hash then Deferred.unit
    else
      let transition_registry = Extensions.transition_registry t.extensions in
      Extensions.Transition_registry.register transition_registry target_hash

  let equal t1 t2 =
    let sort_breadcrumbs = List.sort ~compare:Breadcrumb.compare in
    let equal_breadcrumb breadcrumb1 breadcrumb2 =
      let open Breadcrumb in
      let open Option.Let_syntax in
      let get_successor_nodes frontier breadcrumb =
        let%map node = Hashtbl.find frontier.table @@ state_hash breadcrumb in
        Node.successor_hashes node
      in
      equal breadcrumb1 breadcrumb2
      && State_hash.equal (parent_hash breadcrumb1) (parent_hash breadcrumb2)
      && (let%bind successors1 = get_successor_nodes t1 breadcrumb1 in
          let%map successors2 = get_successor_nodes t2 breadcrumb2 in
          List.equal State_hash.equal
            (successors1 |> List.sort ~compare:State_hash.compare)
            (successors2 |> List.sort ~compare:State_hash.compare))
         |> Option.value_map ~default:false ~f:Fn.id
    in
    List.equal equal_breadcrumb
      (all_breadcrumbs t1 |> sort_breadcrumbs)
      (all_breadcrumbs t2 |> sort_breadcrumbs)

  module For_tests = struct
    let root_snarked_ledger {root_snarked_ledger; _} = root_snarked_ledger

    let root_history_mem {extensions; _} hash =
      Extensions.Root_history.mem extensions.root_history hash

    let root_history_is_empty {extensions; _} =
      Extensions.Root_history.is_empty extensions.root_history
  end
end

include Make (struct
  module Staged_ledger_aux_hash = struct
    include Staged_ledger_hash.Aux_hash.Stable.V1

    [%%define_locally
    Staged_ledger_hash.Aux_hash.(of_bytes, to_bytes)]
  end

  module Verifier = Verifier
  module Pending_coinbase_stack_state =
    Transaction_snark.Pending_coinbase_stack_state
  module Ledger_proof_statement = Transaction_snark.Statement
  module Ledger_proof = Ledger_proof
  module Transaction_snark_work = Transaction_snark_work
  module Staged_ledger_diff = Staged_ledger_diff
  module External_transition = External_transition
  module Transaction_witness = Transaction_witness
  module Staged_ledger = Staged_ledger

  let max_length = Consensus.Constants.k
end)
