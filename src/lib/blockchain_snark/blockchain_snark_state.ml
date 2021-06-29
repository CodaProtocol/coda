open Core_kernel
open Snark_params
open Tick
open Mina_base
open Mina_state
open Pickles_types

include struct
  open Snarky_backendless.Request

  type _ t +=
    | Prev_state : Protocol_state.Value.t t
    | Transition : Snark_transition.Value.t t
end

module Witness = struct
  type t =
    {prev_state: Protocol_state.Value.t; transition: Snark_transition.Value.t}
end

let blockchain_handler on_unhandled {Witness.prev_state; transition} =
  let open Snarky_backendless.Request in
  fun (With {request; respond} as r) ->
    let k x = respond (Provide x) in
    match request with
    | Prev_state ->
        k prev_state
    | Transition ->
        k transition
    | _ ->
        on_unhandled r

let wrap_handler h w =
  match h with
  | None ->
      blockchain_handler
        (fun (Snarky_backendless.Request.With {respond; _}) ->
          respond Unhandled )
        w
  | Some h ->
      (* TODO: Clean up the handler composition interface. *)
      fun r -> blockchain_handler h w r

let with_handler k w ?handler =
  let h = wrap_handler handler w in
  k ?handler:(Some h)

(* Blockchain_snark ~old ~nonce ~ledger_snark ~ledger_hash ~timestamp ~new_hash
      Input:
        old : Blockchain.t
        old_snark : proof
        nonce : int
        work_snark : proof
        ledger_hash : Ledger_hash.t
        timestamp : Time.t
        new_hash : State_hash.t
      Witness:
        transition : Transition.t
      such that
        the old_snark verifies against old
        new = update_with_asserts(old, nonce, timestamp, ledger_hash)
        hash(new) = new_hash
        the work_snark verifies against the old.ledger_hash and new_ledger_hash
        new.timestamp > old.timestamp
        transition consensus data is valid
        new consensus state is a function of the old consensus state
*)
let%snarkydef step ~(logger : Logger.t)
    ~(proof_level : Genesis_constants.Proof_level.t)
    ~(constraint_constants : Genesis_constants.Constraint_constants.t)
    Hlist.HlistId.
      [ previous_state_hash
      ; (txn_snark : Transaction_snark.Statement.With_sok.Checked.t) ]
    new_state_hash : (_, _) Tick.Checked.t =
  let%bind transition =
    with_label __LOC__
      (exists Snark_transition.typ ~request:(As_prover.return Transition))
  in
  let%bind previous_state, previous_state_body_hash =
    let%bind t =
      with_label __LOC__
        (exists
           (Protocol_state.typ ~constraint_constants)
           ~request:(As_prover.return Prev_state))
    in
    let%bind h, body = Protocol_state.hash_checked t in
    let%map () =
      with_label __LOC__ (State_hash.assert_equal h previous_state_hash)
    in
    (t, body)
  in
  let%bind `Success updated_consensus_state, consensus_state =
    with_label __LOC__
      (Consensus_state_hooks.next_state_checked ~constraint_constants
         ~prev_state:previous_state ~prev_state_hash:previous_state_hash
         transition txn_snark.supply_increase)
  in
  let supercharge_coinbase =
    Consensus.Data.Consensus_state.supercharge_coinbase_var consensus_state
  in
  let prev_pending_coinbase_root =
    previous_state |> Protocol_state.blockchain_state
    |> Blockchain_state.staged_ledger_hash
    |> Staged_ledger_hash.pending_coinbase_hash_var
  in
  let%bind genesis_state_hash =
    (*get the genesis state hash from previous state unless previous state is the genesis state itslef*)
    Protocol_state.genesis_state_hash_checked ~state_hash:previous_state_hash
      previous_state
  in
  let%bind new_state, is_base_case =
    let t =
      Protocol_state.create_var ~previous_state_hash ~genesis_state_hash
        ~blockchain_state:(Snark_transition.blockchain_state transition)
        ~consensus_state
        ~constants:(Protocol_state.constants previous_state)
    in
    let%bind is_base_case =
      Protocol_state.consensus_state t
      |> Consensus.Data.Consensus_state.is_genesis_state_var
    in
    let%bind previous_state_hash =
      match constraint_constants.fork with
      | Some {previous_state_hash= fork_prev; _} ->
          State_hash.if_ is_base_case
            ~then_:(State_hash.var_of_t fork_prev)
            ~else_:t.previous_state_hash
      | None ->
          Checked.return t.previous_state_hash
    in
    let t = {t with previous_state_hash} in
    let%map () =
      let%bind h, _ = Protocol_state.hash_checked t in
      with_label __LOC__ (State_hash.assert_equal h new_state_hash)
    in
    (t, is_base_case)
  in
  let%bind txn_snark_should_verify, success =
    let%bind ledger_hash_didn't_change =
      Frozen_ledger_hash.equal_var
        ( previous_state |> Protocol_state.blockchain_state
        |> Blockchain_state.snarked_ledger_hash )
        txn_snark.target
    and supply_increase_is_zero =
      Currency.Amount.(equal_var txn_snark.supply_increase (var_of_t zero))
    in
    let%bind new_pending_coinbase_hash, deleted_stack, no_coinbases_popped =
      let coinbase_receiver =
        Consensus.Data.Consensus_state.coinbase_receiver_var consensus_state
      in
      let%bind root_after_delete, deleted_stack =
        Pending_coinbase.Checked.pop_coinbases ~constraint_constants
          prev_pending_coinbase_root
          ~proof_emitted:(Boolean.not ledger_hash_didn't_change)
      in
      (*If snarked ledger hash did not change (no new ledger proof) then pop_coinbases should be a no-op*)
      let%bind no_coinbases_popped =
        Pending_coinbase.Hash.equal_var root_after_delete
          prev_pending_coinbase_root
      in
      (*new stack or update one*)
      let%map new_root =
        with_label __LOC__
          (Pending_coinbase.Checked.add_coinbase ~constraint_constants
             root_after_delete
             (Snark_transition.pending_coinbase_update transition)
             ~coinbase_receiver ~supercharge_coinbase previous_state_body_hash)
      in
      (new_root, deleted_stack, no_coinbases_popped)
    in
    let pending_coinbase_source_stack =
      Pending_coinbase.Stack.Checked.create_with deleted_stack
    in
    let%bind txn_snark_input_correct =
      let lh t =
        Protocol_state.blockchain_state t
        |> Blockchain_state.snarked_ledger_hash
      in
      let open Checked in
      let%bind () =
        Fee_excess.(assert_equal_checked (var_of_t zero) txn_snark.fee_excess)
      in
      all
        [ Frozen_ledger_hash.equal_var txn_snark.source (lh previous_state)
        ; Frozen_ledger_hash.equal_var txn_snark.target (lh new_state)
        ; Pending_coinbase.Stack.equal_var
            txn_snark.pending_coinbase_stack_state.source
            pending_coinbase_source_stack
        ; Pending_coinbase.Stack.equal_var
            txn_snark.pending_coinbase_stack_state.target deleted_stack
        ; Token_id.Checked.equal txn_snark.next_available_token_before
            ( previous_state |> Protocol_state.blockchain_state
            |> Blockchain_state.snarked_next_available_token )
        ; Token_id.Checked.equal txn_snark.next_available_token_after
            ( transition |> Snark_transition.blockchain_state
            |> Blockchain_state.snarked_next_available_token ) ]
      >>= Boolean.all
    in
    let%bind nothing_changed =
      let%bind next_available_token_didn't_change =
        Token_id.Checked.equal txn_snark.next_available_token_after
          txn_snark.next_available_token_before
      in
      Boolean.all
        [ ledger_hash_didn't_change
        ; supply_increase_is_zero
        ; no_coinbases_popped
        ; next_available_token_didn't_change ]
    in
    let%bind correct_coinbase_status =
      let new_root =
        transition |> Snark_transition.blockchain_state
        |> Blockchain_state.staged_ledger_hash
        |> Staged_ledger_hash.pending_coinbase_hash_var
      in
      Pending_coinbase.Hash.equal_var new_pending_coinbase_hash new_root
    in
    let%bind () =
      Boolean.Assert.any [txn_snark_input_correct; nothing_changed]
    in
    let transaction_snark_should_verifiy = Boolean.not nothing_changed in
    let%bind result =
      Boolean.all [updated_consensus_state; correct_coinbase_status]
    in
    let%map () =
      as_prover
        As_prover.(
          Let_syntax.(
            let%map txn_snark_input_correct =
              read Boolean.typ txn_snark_input_correct
            and nothing_changed = read Boolean.typ nothing_changed
            and no_coinbases_popped = read Boolean.typ no_coinbases_popped
            and updated_consensus_state =
              read Boolean.typ updated_consensus_state
            and correct_coinbase_status =
              read Boolean.typ correct_coinbase_status
            and result = read Boolean.typ result in
            [%log trace]
              "blockchain snark update success: $result = \
               (transaction_snark_input_correct=$transaction_snark_input_correct \
               ∨ nothing_changed \
               (no_coinbases_popped=$no_coinbases_popped)=$nothing_changed) \
               ∧ updated_consensus_state=$updated_consensus_state ∧ \
               correct_coinbase_status=$correct_coinbase_status"
              ~metadata:
                [ ( "transaction_snark_input_correct"
                  , `Bool txn_snark_input_correct )
                ; ("nothing_changed", `Bool nothing_changed)
                ; ("updated_consensus_state", `Bool updated_consensus_state)
                ; ("correct_coinbase_status", `Bool correct_coinbase_status)
                ; ("result", `Bool result)
                ; ("no_coinbases_popped", `Bool no_coinbases_popped) ]))
    in
    (transaction_snark_should_verifiy, result)
  in
  let txn_snark_should_verify =
    match proof_level with
    | Check | None ->
        Boolean.false_
    | Full ->
        txn_snark_should_verify
  in
  let prev_should_verify =
    match proof_level with
    | Check | None ->
        Boolean.false_
    | Full ->
        Boolean.not is_base_case
  in
  let%map () = Boolean.Assert.any [is_base_case; success] in
  (prev_should_verify, txn_snark_should_verify)

let check w ?handler ~proof_level ~constraint_constants txn_snark
    new_state_hash : unit Or_error.t =
  let open Tick in
  check
    (Fn.flip handle (wrap_handler handler w)
       (let%bind prev =
          exists State_hash.typ
            ~compute:(As_prover.return (Protocol_state.hash w.prev_state))
        and curr =
          exists State_hash.typ ~compute:(As_prover.return new_state_hash)
        and txn_snark =
          exists Transaction_snark.Statement.With_sok.typ
            ~compute:(As_prover.return txn_snark)
        in
        step ~proof_level ~constraint_constants ~logger:(Logger.create ())
          [prev; txn_snark] curr))
    ()

let rule ~proof_level ~constraint_constants transaction_snark self :
    _ Pickles.Inductive_rule.t =
  { identifier= "step"
  ; prevs= [self; transaction_snark]
  ; main=
      (fun [x1; x2] x ->
        let b1, b2 =
          Run.run_checked
            (step ~proof_level ~constraint_constants ~logger:(Logger.create ())
               [x1; x2] x)
        in
        [b1; b2] )
  ; main_value=
      (fun [prev; (txn : Transaction_snark.Statement.With_sok.t)] curr ->
        let lh t =
          Protocol_state.blockchain_state t
          |> Blockchain_state.snarked_ledger_hash
        in
        [ not
            (Consensus.Data.Consensus_state.is_genesis_state
               (Protocol_state.consensus_state curr))
        ; List.for_all ~f:Fn.id
            [ Frozen_ledger_hash.equal (lh prev) (lh curr)
            ; Currency.Amount.(equal zero)
                txn.Transaction_snark.Statement.supply_increase
            ; Pending_coinbase.Stack.equal
                txn.pending_coinbase_stack_state.source
                txn.pending_coinbase_stack_state.target ]
          |> not ] ) }

module Statement = struct
  type t = Protocol_state.Value.t

  let to_field_elements (t : t) : Tick.Field.t array = [|Protocol_state.hash t|]
end

module Statement_var = struct
  type t = State_hash.var

  let to_field_elements (t : t) = [|State_hash.var_to_hash_packed t|]
end

let typ =
  Typ.transport State_hash.typ ~there:Protocol_state.hash ~back:(fun _ ->
      failwith "cannot unhash" )

type tag =
  (State_hash.var, Protocol_state.value, Nat.N2.n, Nat.N1.n) Pickles.Tag.t

module type S = sig
  module Proof :
    Pickles.Proof_intf
    with type t = (Nat.N2.n, Nat.N2.n) Pickles.Proof.t
     and type statement = Protocol_state.Value.t

  val tag : tag

  val cache_handle : Pickles.Cache_handle.t

  open Nat

  val step :
       Witness.t
    -> ( Protocol_state.Value.t
         * (Transaction_snark.Statement.With_sok.t * unit)
       , N2.n * (N2.n * unit)
       , N1.n * (N2.n * unit)
       , Protocol_state.Value.t
       , Proof.t Async.Deferred.t )
       Pickles.Prover.t

  val constraint_system_digests : (string * Md5_lib.t) list Lazy.t
end

let verify ts ~key = Pickles.verify (module Nat.N2) (module Statement) key ts

let constraint_system_digests ~proof_level ~constraint_constants () =
  let digest = Tick.R1CS_constraint_system.digest in
  [ ( "blockchain-step"
    , digest
        (let main x =
           let open Tick in
           let%bind x1 = exists Mina_base.State_hash.typ in
           let%bind x2 = exists Transaction_snark.Statement.With_sok.typ in
           let%map _ =
             step ~proof_level ~constraint_constants ~logger:(Logger.create ())
               [x1; x2] x
           in
           ()
         in
         Tick.constraint_system ~exposing:[Mina_base.State_hash.typ] main) ) ]

module Make (T : sig
  val tag : Transaction_snark.tag

  val constraint_constants : Genesis_constants.Constraint_constants.t

  val proof_level : Genesis_constants.Proof_level.t
end) : S = struct
  open T

  let tag, cache_handle, p, Pickles.Provers.[step] =
    Pickles.compile ~cache:Cache_dir.cache
      (module Statement_var)
      (module Statement)
      ~typ
      ~branches:(module Nat.N1)
      ~max_branching:(module Nat.N2)
      ~name:"blockchain-snark"
      ~constraint_constants:
        (Genesis_constants.Constraint_constants.to_snark_keys_header
           constraint_constants)
      ~choices:(fun ~self ->
        [rule ~proof_level ~constraint_constants T.tag self] )

  let step = with_handler step

  let constraint_system_digests =
    lazy (constraint_system_digests ~proof_level ~constraint_constants ())

  module Proof = (val p)
end
