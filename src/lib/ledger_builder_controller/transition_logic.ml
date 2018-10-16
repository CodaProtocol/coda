open Core_kernel
open Async_kernel

module type Inputs_intf = sig
  module State_hash : sig
    type t [@@deriving sexp, eq, compare]
  end

  module Ledger_hash : sig
    type t [@@deriving sexp, eq]
  end

  module Ledger_builder_hash : sig
    type t [@@deriving eq, sexp, compare]
  end

  module Frozen_ledger_hash : sig
    type t [@@deriving eq, bin_io, sexp, eq]
  end

  module Blockchain_state : sig
    type value [@@deriving eq]

    val ledger_hash : value -> Frozen_ledger_hash.t

    val ledger_builder_hash : value -> Ledger_builder_hash.t
  end

  module Consensus_mechanism : sig
    module Local_state : sig
      type t
    end

    module Consensus_state : sig
      type value
    end

    module Protocol_state : sig
      type value [@@deriving sexp]

      val previous_state_hash : value -> State_hash.t

      val blockchain_state : value -> Blockchain_state.value

      val consensus_state : value -> Consensus_state.value

      val equal_value : value -> value -> bool

      val hash : value -> State_hash.t
    end

    module External_transition : sig
      type t [@@deriving eq, sexp, compare, bin_io]

      val protocol_state : t -> Protocol_state.value
    end

    val select :
         Consensus_state.value
      -> Consensus_state.value
      -> logger:Logger.t
      -> time_received:Unix_timestamp.t
      -> [`Keep | `Take]
  end

  module Tip : sig
    type t [@@deriving sexp]

    type state_hash = State_hash.t

    val state : t -> Consensus_mechanism.Protocol_state.value

    val copy : t -> t

    val transition_unchecked :
         t
      -> (Consensus_mechanism.External_transition.t, state_hash) With_hash.t
      -> (t, state_hash) With_hash.t Deferred.t

    val is_parent_of :
         child:( Consensus_mechanism.External_transition.t
               , state_hash )
               With_hash.t
      -> parent:(t, state_hash) With_hash.t
      -> bool

    val is_materialization_of :
         (t, state_hash) With_hash.t
      -> (Consensus_mechanism.External_transition.t, state_hash) With_hash.t
      -> bool

    val assert_materialization_of :
         (t, state_hash) With_hash.t
      -> (Consensus_mechanism.External_transition.t, state_hash) With_hash.t
      -> unit
  end

  module Transition_logic_state :
    Transition_logic_state.S
    with type tip := Tip.t
     and type consensus_local_state := Consensus_mechanism.Local_state.t
     and type external_transition := Consensus_mechanism.External_transition.t
     and type state_hash := State_hash.t

  module Step : sig
    (* This checks the SNARKs in State/LB and does the transition *)

    val step :
         (Tip.t, State_hash.t) With_hash.t
      -> (Consensus_mechanism.External_transition.t, State_hash.t) With_hash.t
      -> (Tip.t, State_hash.t) With_hash.t Deferred.Or_error.t
  end

  module Catchup : sig
    type t

    val sync :
         t
      -> Transition_logic_state.t
      -> state_mutator:(   Transition_logic_state.t
                        -> Transition_logic_state.Change.t list
                        -> Consensus_mechanism.External_transition.t
                        -> unit)
      -> (Consensus_mechanism.External_transition.t, State_hash.t) With_hash.t
      -> ( (Consensus_mechanism.External_transition.t, State_hash.t) With_hash.t
         , unit )
         Job.t
  end
end

module type S = sig
  type t

  type catchup

  type transition

  type tip

  type transition_logic_state

  type handler_state_change

  type state

  type state_hash

  val create : transition_logic_state -> Logger.t -> t

  val state : t -> transition_logic_state

  val strongest_tip : t -> (tip * transition) Linear_pipe.Reader.t

  val on_new_transition :
       catchup
    -> t
    -> (transition, state_hash) With_hash.t
    -> time_received:Unix_timestamp.t
    -> ((transition, state_hash) With_hash.t, unit) Job.t option Deferred.t

  val local_get_tip :
       t
    -> p_tip:((tip, state_hash) With_hash.t -> bool)
    -> p_trans:((transition, state_hash) With_hash.t -> bool)
    -> ((tip, state_hash) With_hash.t * state) Deferred.Or_error.t
end

module Make (Inputs : Inputs_intf) :
  S
  with type catchup := Inputs.Catchup.t
   and type transition := Inputs.Consensus_mechanism.External_transition.t
   and type transition_logic_state := Inputs.Transition_logic_state.t
   and type handler_state_change := Inputs.Transition_logic_state.Change.t
   and type tip := Inputs.Tip.t
   and type state := Inputs.Consensus_mechanism.Protocol_state.value
   and type state_hash := Inputs.State_hash.t =
struct
  open Inputs
  open Consensus_mechanism
  open Transition_logic_state

  let transition_is_parent_of ~child:{With_hash.data= child; hash= _}
      ~parent:{With_hash.hash= parent_state_hash; data= _} =
    State_hash.equal parent_state_hash
      ( External_transition.protocol_state child
      |> Protocol_state.previous_state_hash )

  (* HACK: To prevent a DoS from healthy nodes trying to gossip the same
   * transition, there's an extra piece of mutable state here that we adjust
   * whenever starting an async job and use to drop duplicate incoming
   * transitions *)
  module Pending_target : sig
    type t [@@deriving sexp]

    val create : parent_log:Logger.t -> t

    val attempt_replace :
         t
      -> (External_transition.t, State_hash.t) With_hash.t
      -> [`Continue | `Stop]

    val finish_target :
      t -> (External_transition.t, State_hash.t) With_hash.t -> unit
  end = struct
    type t = {mutable data: State_hash.t option; log: Logger.t sexp_opaque}
    [@@deriving sexp]

    let create ~parent_log =
      {data= None; log= Logger.child parent_log __MODULE__}

    let attempt_replace t {With_hash.data= _; hash= h'} =
      match t.data with
      | None ->
          Logger.trace t.log
            !"No existing pending target, now replaced with %{sexp: \
              State_hash.t}"
            h' ;
          t.data <- Some h' ;
          `Continue
      | Some h ->
          if State_hash.equal h h' then (
            Logger.trace t.log
              !"Same existing pending target, so dropping %{sexp: State_hash.t}"
              h' ;
            `Stop )
          else (
            Logger.trace t.log
              !"Pending target was on %{sexp: State_hash.t} and now is \
                switching to %{sexp: State_hash.t}"
              h h' ;
            t.data <- Some h' ;
            `Continue )

    let finish_target t {With_hash.data= _; hash= h'} =
      Option.iter t.data ~f:(fun h ->
          if State_hash.equal h h' then
            Logger.trace t.log
              !"Finishing pending target %{sexp: State_hash.t} cleanly"
              h
          else (
            Logger.warn t.log
              !"Attempted to finishing pending target %{sexp: State_hash.t}, \
                but we are pending on %{sexp: State_hash.t}"
              h h' ;
            t.data <- None ) )
  end

  (* TODO: propogate mutate_function into everything that is getting mutated *)
  type t =
    { mutable state: Transition_logic_state.t
    ; log: Logger.t
    ; pending_target: Pending_target.t
    ; strongest_tip_writer:
        (Tip.t * External_transition.t) Linear_pipe.Writer.t
    ; strongest_tip_reader:
        (Tip.t * External_transition.t) Linear_pipe.Reader.t }

  let state {state; _} = state

  let strongest_tip {strongest_tip_reader; _} = strongest_tip_reader

  let mutate_state t old_state changes transition =
    (* TODO: We can make change-resolving more intelligent if different
    * concurrent processes took different times to finish. Since we
    * serialize to one job at a time this shouldn't happen anyway though *)
    let old_longest_branch_tip =
      old_state |> Transition_logic_state.longest_branch_tip
    in
    let new_longest_branch_tip =
      List.find_map changes ~f:(function
        | Transition_logic_state.Change.Longest_branch_tip tip -> Some tip
        | _ -> None )
    in
    let new_state = Transition_logic_state.apply_all old_state changes in
    t.state <- new_state ;
    match new_longest_branch_tip with
    | None -> ()
    | Some new_longest_branch_tip ->
        if
          not
            (Protocol_state.equal_value
               (old_longest_branch_tip |> With_hash.data |> Tip.state)
               (new_longest_branch_tip |> With_hash.data |> Tip.state))
        then
          don't_wait_for
            (Linear_pipe.write t.strongest_tip_writer
               (With_hash.data new_longest_branch_tip, transition))

  type t0 = t

  module Path =
    Path.Make (struct
        type t = (Protocol_state.value, State_hash.t) With_hash.t
        [@@deriving sexp]
      end)
      (struct
        type t = (External_transition.t, State_hash.t) With_hash.t
        [@@deriving sexp]

        let target = With_hash.map ~f:External_transition.protocol_state
      end)

  let create state parent_log : t =
    let log = Logger.child parent_log __MODULE__ in
    let strongest_tip_reader, strongest_tip_writer = Linear_pipe.create () in
    { state
    ; log
    ; pending_target= Pending_target.create ~parent_log:log
    ; strongest_tip_reader
    ; strongest_tip_writer }

  let locked_and_best tree =
    let path = Transition_tree.longest_path tree in
    (List.hd_exn path, List.last_exn path)

  module Path_traversal = struct
    type t = ((External_transition.t, State_hash.t) With_hash.t, unit) Job.t

    let transition_unchecked h t =
      Interruptible.uninterruptible (Tip.transition_unchecked h t)

    let run (t: t0) new_tree old_tree new_best_path _logger _transition =
      let old_state = t.state in
      let locked_tip = Transition_logic_state.locked_tip old_state
      and longest_branch_tip =
        Transition_logic_state.longest_branch_tip old_state
      in
      let new_head, _new_tip = locked_and_best new_tree in
      let old_head, _old_tip = locked_and_best old_tree in
      let open Interruptible.Let_syntax in
      let ivar : (External_transition.t, State_hash.t) With_hash.t Ivar.t =
        Ivar.create ()
      in
      let step tip_with_hash transition_with_hash =
        Interruptible.lift
          (Step.step tip_with_hash transition_with_hash)
          (Deferred.map (Ivar.read ivar) ~f:ignore)
      in
      let work =
        (* Adjust the locked_ledger if necessary *)
        let%bind locked_tip =
          if transition_is_parent_of ~child:new_head ~parent:old_head then
            let locked_tip = With_hash.map locked_tip ~f:Tip.copy in
            transition_unchecked locked_tip.data new_head
          else return locked_tip
        in
        (* Now adjust the longest_branch_tip *)
        let tip, path =
          match
            Path.findi new_best_path ~f:(fun _ x ->
                Tip.is_materialization_of longest_branch_tip x )
          with
          | None -> (With_hash.map locked_tip ~f:Tip.copy, new_best_path)
          | Some (i, _) ->
              ( With_hash.map longest_branch_tip ~f:Tip.copy
              , Path.drop new_best_path (i + 1) )
        in
        let last_transition = List.last_exn path.Path.path in
        (* Now step over the path *)
        assert (
          State_hash.equal (With_hash.hash tip)
            (With_hash.hash path.Path.source) ) ;
        let%map result =
          List.fold path.Path.path ~init:(Interruptible.return (Some tip)) ~f:
            (fun work curr ->
              match%bind work with
              | None -> return None
              | Some tip ->
                  match%bind step tip curr with
                  | Ok tip -> return (Some tip)
                  | Error e ->
                      (* TODO: Punish sender *)
                      Logger.warn t.log "Recieved malicious transition %s"
                        (Error.to_string_hum e) ;
                      return None )
        in
        match result with
        | Some tip ->
            Tip.assert_materialization_of tip last_transition ;
            let changes =
              [ Transition_logic_state.Change.Longest_branch_tip tip
              ; Transition_logic_state.Change.Ktree new_tree ]
            in
            mutate_state t old_state changes (With_hash.data last_transition)
        | None -> ()
      in
      (work, ivar)

    let create (t: t0) new_tree old_tree new_best_path (logger: Logger.t)
        (transition_with_hash:
          (External_transition.t, State_hash.t) With_hash.t) : t =
      Job.create transition_with_hash
        ~f:(run t new_tree old_tree new_best_path logger)
  end

  let local_get_tip t ~p_tip ~p_trans =
    let old_state = t.state in
    let locked_tip = Transition_logic_state.locked_tip old_state
    and ktree = Transition_logic_state.ktree old_state
    and longest_branch_tip =
      Transition_logic_state.longest_branch_tip old_state
    in
    match ktree with
    | None ->
        Logger.trace t.log !"Local-get-tip unsuccessful because no ktree" ;
        return
          (Or_error.error_string "Not found locally, because I have no ktree")
    | Some ktree ->
        let attempt_easy tip err_msg_name =
          let maybe_state =
            Transition_tree.find_map ktree ~f:
              (fun ({With_hash.data= trans; hash= _} as trans_with_hash) ->
                if p_trans trans_with_hash then
                  Some (External_transition.protocol_state trans)
                else None )
          in
          match maybe_state with
          | None ->
              return
              @@ Or_error.errorf
                   "This was our %s, but we didn't witness the state"
                   err_msg_name
          | Some state -> return @@ Ok (tip, state)
        in
        if p_tip locked_tip then attempt_easy locked_tip "locked"
        else if p_tip longest_branch_tip then
          attempt_easy longest_branch_tip "longest_branch"
        else
          match
            Option.map
              (Transition_tree.path ktree ~f:p_trans)
              ~f:Path.of_tree_path
          with
          | Some path -> (
              (* Note: We can't have zero transitions because then we would have
             * matched the locked_tip *)
              let last_transition = List.last_exn path.Path.path in
              Logger.trace t.log
                !"Attempting a local path traversal to last_transition \
                  %{sexp: (External_transition.t, State_hash.t) With_hash.t}"
                last_transition ;
              assert (p_tip (Transition_logic_state.longest_branch_tip old_state)) ;
              let job =
                Path_traversal.create t ktree ktree path t.log last_transition
              in
              let w, _ = Job.run job in
              match%map w.d with
              | Error _ ->
                  failwith "We are never cancelling, so it can't be cancelled"
              | Ok () ->
                  Logger.trace t.log
                    !"Successfully path traversed to last_transition %{sexp: \
                      (External_transition.t, State_hash.t) With_hash.t}"
                    last_transition ;
                  Ok
                    ( longest_branch_tip
                    , External_transition.protocol_state
                        (With_hash.data last_transition) ) )
          | None ->
              return
                (Or_error.error_string "Not found locally within our ktree")

  let unguarded_on_new_transition catchup t transition_with_hash ~time_received
      :
      ((External_transition.t, State_hash.t) With_hash.t, unit) Job.t option
      Deferred.t =
    let old_state = t.state in
    let longest_branch_tip = Transition_logic_state.longest_branch_tip old_state
    and ktree = Transition_logic_state.ktree old_state in
    match ktree with
    | None -> (
        let source_state = Tip.state (With_hash.data longest_branch_tip) in
        let target_state =
          External_transition.protocol_state
            (With_hash.data transition_with_hash)
        in
        if
          Tip.is_parent_of ~child:transition_with_hash
            ~parent:longest_branch_tip
        then (
          (* Bootstrap from genesis *)
          let tree = Transition_tree.singleton transition_with_hash in
          match%map Step.step longest_branch_tip transition_with_hash with
          | Ok tip ->
              let changes =
                [ Transition_logic_state.Change.Ktree tree
                ; Transition_logic_state.Change.Longest_branch_tip tip
                ; Transition_logic_state.Change.Locked_tip tip ]
              in
              mutate_state t old_state changes
                (With_hash.data transition_with_hash) ;
              None
          | Error e ->
              (* TODO: Punish sender *)
              Logger.info t.log "Recieved malicious transition %s"
                (Error.to_string_hum e) ;
              None )
        else
          match
            Consensus_mechanism.select
              (Protocol_state.consensus_state source_state)
              (Protocol_state.consensus_state target_state)
              ~logger:t.log ~time_received
          with
          | `Keep -> return None
          | `Take ->
              let lh =
                With_hash.data transition_with_hash
                |> External_transition.protocol_state
                |> Protocol_state.blockchain_state
                |> Blockchain_state.ledger_hash
              in
              Logger.debug t.log
                !"Branch catchup for transition: lh:%{sexp: \
                  Frozen_ledger_hash.t} state:%{sexp:Protocol_state.value}"
                lh target_state ;
              return
                (Some
                   (Catchup.sync ~state_mutator:(mutate_state t) catchup
                      old_state transition_with_hash)) )
    | Some old_tree ->
      match
        Transition_tree.add old_tree transition_with_hash ~parent:(fun x ->
            transition_is_parent_of ~child:transition_with_hash ~parent:x )
      with
      | `No_parent -> (
          let best_tip = locked_and_best old_tree |> snd in
          match
            Consensus_mechanism.select
              ( transition_with_hash |> With_hash.data
              |> External_transition.protocol_state
              |> Protocol_state.consensus_state )
              ( best_tip |> With_hash.data |> External_transition.protocol_state
              |> Protocol_state.consensus_state )
              ~logger:t.log ~time_received
          with
          | `Keep ->
              Logger.debug t.log "Branch noparent" ;
              return
                (Some
                   (Catchup.sync ~state_mutator:(mutate_state t) catchup
                      old_state transition_with_hash))
          | `Take -> return None )
      | `Repeat -> return None
      | `Added new_tree ->
          let old_locked_head, old_best_tip = locked_and_best old_tree in
          let new_head, new_tip = locked_and_best new_tree in
          if
            With_hash.compare External_transition.compare State_hash.compare
              old_locked_head new_head
            = 0
            && With_hash.compare External_transition.compare State_hash.compare
                 old_best_tip new_tip
               = 0
          then (
            mutate_state t old_state
              [Transition_logic_state.Change.Ktree new_tree]
              (With_hash.data new_tip) ;
            return None )
          else
            let new_best_path =
              Transition_tree.longest_path new_tree |> Path.of_tree_path
            in
            return
              (Some
                 (Path_traversal.create t new_tree old_tree new_best_path t.log
                    transition_with_hash))

  let on_new_transition catchup ({pending_target; _} as t) transition_with_hash
      ~(time_received: Unix_timestamp.t) :
      ((External_transition.t, State_hash.t) With_hash.t, unit) Job.t option
      Deferred.t =
    match
      Pending_target.attempt_replace pending_target transition_with_hash
    with
    | `Stop -> return None
    | `Continue ->
        let%map job =
          unguarded_on_new_transition catchup t transition_with_hash
            ~time_received
        in
        match job with
        | None ->
            Pending_target.finish_target pending_target transition_with_hash ;
            None
        | Some job ->
            Some
              (Job.after job ~f:(fun () ->
                   Pending_target.finish_target pending_target
                     transition_with_hash ))
end
