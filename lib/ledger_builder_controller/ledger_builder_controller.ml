open Core_kernel
open Async_kernel
open Core_kernel
open Async_kernel

module type S = sig
  include Coda.Ledger_builder_controller_intf
end

module Make (Ledger_builder_hash : sig
  type t [@@deriving eq, bin_io]
end) (Ledger_hash : sig
  type t [@@deriving bin_io]
end) (Ledger_builder_transition : sig
  type t [@@deriving eq, sexp, compare, bin_io]
end) (Ledger : sig
  type t

  val merkle_root : t -> Ledger_hash.t
end) (Ledger_builder : sig
  type t [@@deriving bin_io]

  type proof

  val ledger : t -> Ledger.t

  val create : Ledger.t -> t

  val copy : t -> t

  val hash : t -> Ledger_builder_hash.t

  val apply :
       t
    -> Ledger_builder_transition.t
    -> (Ledger_hash.t * proof) option Deferred.Or_error.t
end) (State_hash : sig
  type t [@@deriving eq]
end) (State : sig
  type t [@@deriving eq, sexp, compare, bin_io]

  val ledger_builder_hash : t -> Ledger_builder_hash.t

  val hash : t -> State_hash.t

  val previous_state_hash : t -> State_hash.t
end) (Valid_transaction : sig
  type t [@@deriving eq, sexp, compare, bin_io]
end) (Net : sig
  include Coda.Ledger_builder_io_intf
          with type ledger_builder := Ledger_builder.t
           and type ledger_builder_hash := Ledger_builder_hash.t
           and type state := State.t
end) (Snark_pool : sig
  type t
end)
(Store : Storage.With_checksum_intf) =
struct
  module Config = struct
    type t =
      { keep_count: int [@default 50]
      ; parent_log: Logger.t
      ; net_deferred: Net.net Deferred.t
      ; ledger_builder_transitions:
          (Valid_transaction.t list * State.t * Ledger_builder_transition.t)
          Linear_pipe.Reader.t
      ; genesis_ledger: Ledger.t
      ; disk_location: string
      ; snark_pool: Snark_pool.t }
    [@@deriving make]
  end

  module Witness = struct
    type t =
      { transactions: Valid_transaction.t list
      ; transition: Ledger_builder_transition.t
      ; state: State.t }
    [@@deriving eq, compare, bin_io, sexp, fields]

    let ledger_builder_hash {state} = State.ledger_builder_hash state

    let state_hash {state} = State.hash state

    let previous_state_hash {state} = State.previous_state_hash state

    let gen txn_gen transition_gen state_gen =
      let open Quickcheck.Generator.Let_syntax in
      let%map transactions = Quickcheck.Generator.list txn_gen
      and transition = transition_gen
      and state = state_gen in
      {transactions; transition; state}
  end

  module Witness_tree =
    Ktree.Make (Witness)
      (struct
        let k = 50
      end)

  module Tagged_lb = struct
    type t = Ledger_hash.t ref * Ledger_builder.t [@@deriving bin_io]

    let create ~hash ~builder = (ref hash, builder)

    let ledger_builder = snd

    let hash t = !(fst t)

    let set_hash t h = fst t := h
  end

  module State = struct
    type t =
      { locked_ledger_builder: Tagged_lb.t
      ; longest_branch_tip: Tagged_lb.t
      ; mutable ktree: Witness_tree.t option
      (* TODO: This impl assumes we have the original Ouroboros assumption. In
         order to work with the Praos assumption we'll need to keep a linked
         list as well at the prefix of size (#blocks possible out of order)
       *)
      }
    [@@deriving bin_io]

    let create genesis_ledger : t =
      let root = Ledger.merkle_root genesis_ledger in
      { locked_ledger_builder=
          Tagged_lb.create ~hash:root
            ~builder:(Ledger_builder.create genesis_ledger)
      ; longest_branch_tip=
          Tagged_lb.create ~hash:root
            ~builder:(Ledger_builder.create genesis_ledger)
      ; ktree= None }
  end

  type t =
    { ledger_builder_io: Net.t
    ; log: Logger.t
    ; state: State.t
    ; strongest_ledgers: Ledger_builder.t Linear_pipe.Reader.t }

  let best_tip tree = Witness_tree.longest_path tree |> List.last_exn

  let locked_head tree = Witness_tree.longest_path tree |> List.hd_exn

  (* The following assertion will always pass without extra checks because
     we're supposed to have validated the witness upstream this pipe (see
     coda.ml) *)
  let assert_valid_state (witness: Witness.t) builder =
    assert (
      Ledger_builder_hash.equal
        (Witness.ledger_builder_hash witness)
        (Ledger_builder.hash builder) ) ;
    ()

  let create (config: Config.t) =
    let log = Logger.child config.parent_log "ledger_builder_controller" in
    let storage_controller =
      Store.Controller.create ~parent_log:log [%bin_type_class : State.t]
    in
    let%bind state =
      match%map Store.load storage_controller config.disk_location with
      | Ok state -> state
      | Error (`IO_error e) ->
          Logger.info log "Ledger failed to load from storage %s; recreating"
            (Error.to_string_hum e) ;
          State.create config.genesis_ledger
      | Error `No_exist ->
          Logger.info log "Ledger doesn't exist in storage; recreating" ;
          State.create config.genesis_ledger
      | Error `Checksum_no_match ->
          Logger.warn log "Checksum failed when loading ledger, recreating" ;
          State.create config.genesis_ledger
    in
    let%map net = config.net_deferred in
    (* Here we effectfully listen to transitions and emit what we belive are
       the strongest ledger_builders *)
    let strongest_ledgers =
      Linear_pipe.filter_map_unordered ~max_concurrency:1
        config.ledger_builder_transitions ~f:
        (fun (transactions, s, transition) ->
          match state.ktree with
          (* If we've seen no data from the network, we can only do nothing here *)
          | None ->
              return None
          | Some old_tree ->
              let witness_to_add : Witness.t =
                {transactions; transition; state= s}
              in
              let p_eq_previous_state_hash (w: Witness.t) =
                State_hash.equal (Witness.state_hash w)
                  (Witness.previous_state_hash witness_to_add)
              in
              (* When we get a new transition adjust our ktree *)
              let new_tree =
                Witness_tree.add old_tree witness_to_add
                  ~parent:p_eq_previous_state_hash
              in
              state.ktree <- Some new_tree ;
              let force_apply_transition lb tip =
                match%map Ledger_builder.apply lb tip with
                | Ok _ -> ()
                | Error e ->
                    failwithf
                      "Invariant failed, we should have validated the data \
                       before here %s"
                      (Error.to_string_hum e) ()
              in
              (* Adjust the locked_ledger if necessary *)
              let%bind () =
                let new_head = locked_head new_tree in
                if Witness.equal (locked_head old_tree) new_head then return ()
                else
                  let lb =
                    Tagged_lb.ledger_builder state.locked_ledger_builder
                  in
                  let%map () =
                    force_apply_transition lb (Witness.transition new_head)
                  in
                  assert_valid_state new_head lb ;
                  ()
              in
              (* Adjust the longest_branch_tip if necessary *)
              let new_tip =
                Witness_tree.longest_path new_tree |> List.last_exn
              in
              if Witness.equal (best_tip old_tree) new_tip then return None
              else
                let lb = Tagged_lb.ledger_builder state.longest_branch_tip in
                let%map () =
                  force_apply_transition lb (Witness.transition new_tip)
                in
                Some lb )
    in
    { ledger_builder_io= Net.create net
    ; log= Logger.child config.parent_log "ledger_builder_controller"
    ; strongest_ledgers
    ; state }

  let strongest_ledgers {strongest_ledgers} = strongest_ledgers

  (** Returns a reference to a ledger_builder denoted by [hash], materialize a
   fresh ledger at a specific hash if necessary *)
  let local_get_ledger t hash =
    let lb_hash tagged_lb =
      Tagged_lb.ledger_builder tagged_lb |> Ledger_builder.hash
    in
    let find_state tree lb_hash =
      Witness_tree.find_map tree ~f:(fun w ->
          if Ledger_builder_hash.equal (Witness.ledger_builder_hash w) lb_hash
          then Some (Witness.state w)
          else None )
    in
    Option.map t.state.ktree ~f:(fun tree ->
        (* First let's see if we have an easy case *)
        let locked = t.state.locked_ledger_builder in
        let tip = t.state.longest_branch_tip in
        let attempt_easy w err_msg_name =
          match find_state tree (lb_hash w) with
          | None ->
              return
              @@ Or_error.errorf
                   "This was our %s, but we didn't witness the state"
                   err_msg_name
          | Some state -> return @@ Ok (Tagged_lb.ledger_builder w, state)
        in
        if Ledger_builder_hash.equal hash (lb_hash locked) then
          attempt_easy locked "locked_head"
        else if Ledger_builder_hash.equal hash (lb_hash tip) then
          attempt_easy tip "tip"
        else
          (* Now we need to materialize it *)
          match
            Witness_tree.path tree ~f:(fun w ->
                Ledger_builder_hash.equal hash (Witness.ledger_builder_hash w)
            )
          with
          | Some path ->
              let lb_start =
                Tagged_lb.ledger_builder t.state.locked_ledger_builder
              in
              assert_valid_state (List.hd_exn path) lb_start ;
              let lb = Ledger_builder.copy lb_start in
              (* Fast-forward the lb *)
              let%map () =
                Deferred.List.fold ~init:() (List.tl_exn path) ~f:(fun () w ->
                    let open Deferred.Let_syntax in
                    match%map Ledger_builder.apply lb w.Witness.transition with
                    | Ok None -> ()
                    | Ok (Some _) -> ()
                    (* We've already verified that all the patches can be
                       applied successfully before we added to the ktree, so we
                       can force-unwrap here *)
                    | Error e ->
                        failwithf
                          "We should have already verified patches can be \
                           applied: %s"
                          (Error.to_string_hum e) () )
              in
              assert (Ledger_builder_hash.equal (Ledger_builder.hash lb) hash) ;
              Ok (lb, List.last_exn path |> Witness.state)
          | None -> return (Or_error.error_string "Hash not found locally") )
    |> Option.value
         ~default:(return @@ Or_error.error_string "Haven't seen any nodes yet")
end
