open Async_kernel
open Core_kernel
open Coda_base
open Coda_state
open Signature_lib
open Module_version

module type Staged_ledger_diff_intf = sig
  type t [@@deriving bin_io, sexp, version]

  val creator : t -> Public_key.Compressed.t

  val user_commands : t -> User_command.t list
end

module Make
    (Ledger_proof : Coda_intf.Ledger_proof_intf)
    (Verifier : Coda_intf.Verifier_intf
                with type ledger_proof := Ledger_proof.t)
    (Staged_ledger_diff : Staged_ledger_diff_intf) :
  Coda_intf.External_transition_intf
  with type ledger_proof := Ledger_proof.t
   and type verifier := Verifier.t
   and type staged_ledger_diff := Staged_ledger_diff.t = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type t =
          { protocol_state: Protocol_state.Value.Stable.V1.t
          ; protocol_state_proof: Proof.Stable.V1.t sexp_opaque
          ; staged_ledger_diff: Staged_ledger_diff.t
          ; delta_transition_chain_witness:
              State_hash.Stable.V1.t * State_body_hash.Stable.V1.t list }
        [@@deriving sexp, fields, bin_io, version]

        type external_transition = t

        let to_yojson
            { protocol_state
            ; protocol_state_proof= _
            ; staged_ledger_diff= _
            ; delta_transition_chain_witness= _ } =
          `Assoc
            [ ("protocol_state", Protocol_state.value_to_yojson protocol_state)
            ; ("protocol_state_proof", `String "<opaque>")
            ; ("staged_ledger_diff", `String "<opaque>")
            ; ("delta_transition_chain_witness", `String "<opaque>") ]

        (* TODO: Important for bkase to review *)
        let compare t1 t2 =
          Protocol_state.Value.Stable.V1.compare t1.protocol_state
            t2.protocol_state

        let consensus_state {protocol_state; _} =
          Protocol_state.consensus_state protocol_state

        let state_hash {protocol_state; _} = Protocol_state.hash protocol_state

        let parent_hash {protocol_state; _} =
          Protocol_state.previous_state_hash protocol_state

        let proposer {staged_ledger_diff; _} =
          Staged_ledger_diff.creator staged_ledger_diff

        let user_commands {staged_ledger_diff; _} =
          Staged_ledger_diff.user_commands staged_ledger_diff

        let payments external_transition =
          List.filter
            (user_commands external_transition)
            ~f:
              (Fn.compose User_command_payload.is_payment User_command.payload)
      end

      include T
      include Comparable.Make (T)
      include Registration.Make_latest_version (T)
    end

    module Latest = V1

    module Module_decl = struct
      let name = "external_transition"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  (* bin_io omitted *)
  type t = Stable.Latest.t =
    { protocol_state: Protocol_state.Value.Stable.V1.t
    ; protocol_state_proof: Proof.Stable.V1.t sexp_opaque
    ; staged_ledger_diff: Staged_ledger_diff.t
    ; delta_transition_chain_witness: State_hash.t * State_body_hash.t list }
  [@@deriving sexp]

  type external_transition = t

  [%%define_locally
  Stable.Latest.
    ( protocol_state
    , protocol_state_proof
    , staged_ledger_diff
    , consensus_state
    , state_hash
    , parent_hash
    , proposer
    , user_commands
    , payments
    , to_yojson )]

  include Comparable.Make (Stable.Latest)

  let create ~protocol_state ~protocol_state_proof ~staged_ledger_diff
      ~delta_transition_chain_witness =
    { protocol_state
    ; protocol_state_proof
    ; staged_ledger_diff
    ; delta_transition_chain_witness }

  let timestamp {protocol_state; _} =
    Protocol_state.blockchain_state protocol_state
    |> Blockchain_state.timestamp

  module Validated = struct
    include Stable.Latest
    module Stable = Stable

    let create_unsafe t = `I_swear_this_is_safe_see_my_comment t

    let forget_validation = Fn.id
  end

  module Validation = struct
    type ( 'time_received
         , 'proof
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'delta_transition_chain_witness )
         t =
      'time_received
      * 'proof
      * 'frontier_dependencies
      * 'staged_ledger_diff
      * 'delta_transition_chain_witness
      constraint 'time_received = [`Time_received] * (unit, _) Truth.t
      constraint 'proof = [`Proof] * (unit, _) Truth.t
      constraint
        'frontier_dependencies =
        [`Frontier_dependencies] * (unit, _) Truth.t
      constraint
        'staged_ledger_diff =
        [`Staged_ledger_diff] * (unit, _) Truth.t
      constraint
        'delta_transition_chain_witness =
        [`Delta_transition_chain_witness]
        * (State_hash.t Non_empty_list.t, _) Truth.t

    type fully_invalid =
      ( [`Time_received] * unit Truth.false_t
      , [`Proof] * unit Truth.false_t
      , [`Frontier_dependencies] * unit Truth.false_t
      , [`Staged_ledger_diff] * unit Truth.false_t
      , [`Delta_transition_chain_witness]
        * State_hash.t Non_empty_list.t Truth.false_t )
      t

    type fully_valid =
      ( [`Time_received] * unit Truth.true_t
      , [`Proof] * unit Truth.true_t
      , [`Frontier_dependencies] * unit Truth.true_t
      , [`Staged_ledger_diff] * unit Truth.true_t
      , [`Delta_transition_chain_witness]
        * State_hash.t Non_empty_list.t Truth.true_t )
      t

    type ( 'time_received
         , 'proof
         , 'frontier_dependencies
         , 'staged_ledger_diff
         , 'delta_transition_chain_witness )
         with_transition =
      (external_transition, State_hash.t) With_hash.t
      * ( 'time_received
        , 'proof
        , 'frontier_dependencies
        , 'staged_ledger_diff
        , 'delta_transition_chain_witness )
        t

    let fully_invalid =
      ( (`Time_received, Truth.False)
      , (`Proof, Truth.False)
      , (`Frontier_dependencies, Truth.False)
      , (`Staged_ledger_diff, Truth.False)
      , (`Delta_transition_chain_witness, Truth.False) )

    let wrap t = (t, fully_invalid)

    let lift (t, _) = t

    let lower t v = (t, v)

    module Unsafe = struct
      let set_valid_time_received :
             ( [`Time_received] * unit Truth.false_t
             , 'proof
             , 'frontier_dependencies
             , 'staged_ledger_diff
             , 'delta_transition_chain_witness )
             t
          -> ( [`Time_received] * unit Truth.true_t
             , 'proof
             , 'frontier_dependencies
             , 'staged_ledger_diff
             , 'delta_transition_chain_witness )
             t = function
        | ( (`Time_received, Truth.False)
          , proof
          , frontier_dependencies
          , staged_ledger_diff
          , delta_transition_chain_witness ) ->
            ( (`Time_received, Truth.True ())
            , proof
            , frontier_dependencies
            , staged_ledger_diff
            , delta_transition_chain_witness )
        | _ ->
            failwith "why can't this be refuted?"

      let set_valid_proof :
             ( 'time_received
             , [`Proof] * unit Truth.false_t
             , 'frontier_dependencies
             , 'staged_ledger_diff
             , 'delta_transition_chain_witness )
             t
          -> ( 'time_received
             , [`Proof] * unit Truth.true_t
             , 'frontier_dependencies
             , 'staged_ledger_diff
             , 'delta_transition_chain_witness )
             t = function
        | ( time_received
          , (`Proof, Truth.False)
          , frontier_dependencies
          , staged_ledger_diff
          , delta_transition_chain_witness ) ->
            ( time_received
            , (`Proof, Truth.True ())
            , frontier_dependencies
            , staged_ledger_diff
            , delta_transition_chain_witness )
        | _ ->
            failwith "why can't this be refuted?"

      let set_valid_delta_transition_chain_witness :
             ( 'time_received
             , 'proof
             , 'frontier_dependencies
             , 'staged_ledger_diff
             , [`Delta_transition_chain_witness]
               * State_hash.t Non_empty_list.t Truth.false_t )
             t
          -> State_hash.t Non_empty_list.t
          -> ( 'time_received
             , 'proof
             , 'frontier_dependencies
             , 'staged_ledger_diff
             , [`Delta_transition_chain_witness]
               * State_hash.t Non_empty_list.t Truth.true_t )
             t =
       fun validation hashes ->
        match validation with
        | ( time_received
          , proof
          , frontier_dependencies
          , staged_ledger_diff
          , (`Delta_transition_chain_witness, Truth.False) ) ->
            ( time_received
            , proof
            , frontier_dependencies
            , staged_ledger_diff
            , (`Delta_transition_chain_witness, Truth.True hashes) )
        | _ ->
            failwith "why can't this be refuted?"

      let set_valid_frontier_dependencies :
             ( 'time_received
             , 'proof
             , [`Frontier_dependencies] * unit Truth.false_t
             , 'staged_ledger_diff
             , 'delta_transition_chain_witness )
             t
          -> ( 'time_received
             , 'proof
             , [`Frontier_dependencies] * unit Truth.true_t
             , 'staged_ledger_diff
             , 'delta_transition_chain_witness )
             t = function
        | ( time_received
          , proof
          , (`Frontier_dependencies, Truth.False)
          , staged_ledger_diff
          , delta_transition_chain_witness ) ->
            ( time_received
            , proof
            , (`Frontier_dependencies, Truth.True ())
            , staged_ledger_diff
            , delta_transition_chain_witness )
        | _ ->
            failwith "why can't this be refuted?"

      let set_valid_staged_ledger_diff :
             ( 'time_received
             , 'proof
             , 'frontier_dependencies
             , [`Staged_ledger_diff] * unit Truth.false_t
             , 'delta_transition_chain_witness )
             t
          -> ( 'time_received
             , 'proof
             , 'frontier_dependencies
             , [`Staged_ledger_diff] * unit Truth.true_t
             , 'delta_transition_chain_witness )
             t = function
        | ( time_received
          , proof
          , frontier_dependencies
          , (`Staged_ledger_diff, Truth.False)
          , delta_transition_chain_witness ) ->
            ( time_received
            , proof
            , frontier_dependencies
            , (`Staged_ledger_diff, Truth.True ())
            , delta_transition_chain_witness )
        | _ ->
            failwith "why can't this be refuted?"
    end
  end

  type with_initial_validation =
    ( [`Time_received] * unit Truth.true_t
    , [`Proof] * unit Truth.true_t
    , [`Frontier_dependencies] * unit Truth.false_t
    , [`Staged_ledger_diff] * unit Truth.false_t
    , [`Delta_transition_chain_witness]
      * State_hash.t Non_empty_list.t Truth.true_t )
    Validation.with_transition

  let skip_time_received_validation
      `This_transition_was_not_received_via_gossip (t, validation) =
    (t, Validation.Unsafe.set_valid_time_received validation)

  let validate_time_received (t, validation) ~time_received =
    let consensus_state =
      With_hash.data t |> protocol_state |> Protocol_state.consensus_state
    in
    let received_unix_timestamp =
      Block_time.to_span_since_epoch time_received |> Block_time.Span.to_ms
    in
    match
      Consensus.Hooks.received_at_valid_time consensus_state
        ~time_received:received_unix_timestamp
    with
    | Ok () ->
        Ok (t, Validation.Unsafe.set_valid_time_received validation)
    | Error err ->
        Error (`Invalid_time_received err)

  let skip_proof_validation `This_transition_was_generated_internally
      (t, validation) =
    (t, Validation.Unsafe.set_valid_proof validation)

  let skip_delta_transition_chain_witness_validation
      `This_transition_was_not_received_via_gossip (t, validation) =
    let previous_protocol_state_hash = With_hash.data t |> parent_hash in
    ( t
    , Validation.Unsafe.set_valid_delta_transition_chain_witness validation
        (Non_empty_list.singleton previous_protocol_state_hash) )

  let validate_proof (t, validation) ~verifier =
    let open Blockchain_snark.Blockchain in
    let open Deferred.Let_syntax in
    let {protocol_state= state; protocol_state_proof= proof; _} =
      With_hash.data t
    in
    match%map Verifier.verify_blockchain_snark verifier {state; proof} with
    | Ok verified ->
        if verified then Ok (t, Validation.Unsafe.set_valid_proof validation)
        else Error `Invalid_proof
    | Error e ->
        Error (`Verifier_error e)

  let validate_delta_transition_chain_witness (t, validation) =
    (* I didn't use the Transition_chain_witness.verify function because otherwise it
       would include a cyclic dependencies *)
    let transition = With_hash.data t in
    let init, merkle_list = transition.delta_transition_chain_witness in
    let hashes =
      List.fold merkle_list ~init:(Non_empty_list.singleton init)
        ~f:(fun acc proof_elem ->
          Non_empty_list.cons
            (Protocol_state.hash_abstract ~hash_body:Fn.id
               {previous_state_hash= Non_empty_list.head acc; body= proof_elem})
            acc )
    in
    if
      State_hash.equal
        (Protocol_state.previous_state_hash transition.protocol_state)
        (Non_empty_list.head hashes)
    then
      Ok
        ( t
        , Validation.Unsafe.set_valid_delta_transition_chain_witness validation
            hashes )
    else Error `Invalid_delta_transition_chain_witness

  let skip_frontier_dependencies_validation
      `This_transition_belongs_to_a_detached_subtree (t, validation) =
    (t, Validation.Unsafe.set_valid_frontier_dependencies validation)

  module Transition_frontier_validation (Transition_frontier : sig
    type t

    module Breadcrumb : sig
      type t

      val transition_with_hash : t -> (Validated.t, State_hash.t) With_hash.t
    end

    val root : t -> Breadcrumb.t

    val find : t -> State_hash.t -> Breadcrumb.t option
  end) =
  struct
    let validate_frontier_dependencies (t, validation) ~logger ~frontier =
      let open Result.Let_syntax in
      let hash = With_hash.hash t in
      let protocol_state = protocol_state (With_hash.data t) in
      let parent_hash = Protocol_state.previous_state_hash protocol_state in
      let root_protocol_state =
        Transition_frontier.root frontier
        |> Transition_frontier.Breadcrumb.transition_with_hash
        |> With_hash.data |> Validated.protocol_state
      in
      let%bind () =
        Result.ok_if_true
          (Transition_frontier.find frontier hash |> Option.is_none)
          ~error:`Already_in_frontier
      in
      let%bind () =
        Result.ok_if_true
          (Transition_frontier.find frontier parent_hash |> Option.is_some)
          ~error:`Parent_missing_from_frontier
      in
      let%map () =
        (* need pervasive (=) in scope for comparing polymorphic variant *)
        let ( = ) = Pervasives.( = ) in
        Result.ok_if_true
          ( `Take
          = Consensus.Hooks.select
              ~logger:
                (Logger.extend logger
                   [ ( "selection_context"
                     , `String
                         "External_transition.Transition_frontier_validation.validate_frontier_dependencies"
                     ) ])
              ~existing:(Protocol_state.consensus_state root_protocol_state)
              ~candidate:(Protocol_state.consensus_state protocol_state) )
          ~error:`Not_selected_over_frontier_root
      in
      (t, Validation.Unsafe.set_valid_frontier_dependencies validation)
  end

  module Staged_ledger_validation (Staged_ledger : sig
    type t

    module Staged_ledger_error : sig
      type t
    end

    val apply :
         t
      -> Staged_ledger_diff.t
      -> logger:Logger.t
      -> verifier:Verifier.t
      -> ( [`Hash_after_applying of Staged_ledger_hash.t]
           * [`Ledger_proof of (Ledger_proof.t * Transaction.t list) option]
           * [`Staged_ledger of t]
           * [`Pending_coinbase_data of bool * Currency.Amount.t]
         , Staged_ledger_error.t )
         Deferred.Result.t

    val current_ledger_proof : t -> Ledger_proof.t option
  end) =
  struct
    let target_hash_of_ledger_proof =
      let open Ledger_proof in
      Fn.compose statement_target statement

    let validate_staged_ledger_diff :
           ( 'time_received
           , 'proof
           , 'frontier_dependencies
           , [`Staged_ledger_diff] * unit Truth.false_t
           , 'delta_transition_chain_witness )
           Validation.with_transition
        -> logger:Logger.t
        -> verifier:Verifier.t
        -> parent_staged_ledger:Staged_ledger.t
        -> ( [`Just_emitted_a_proof of bool]
             * [ `External_transition_with_validation of
                 ( 'time_received
                 , 'proof
                 , 'frontier_dependencies
                 , [`Staged_ledger_diff] * unit Truth.true_t
                 , 'delta_transition_chain_witness )
                 Validation.with_transition ]
             * [`Staged_ledger of Staged_ledger.t]
           , [ `Invalid_staged_ledger_diff of
               [ `Incorrect_target_staged_ledger_hash
               | `Incorrect_target_snarked_ledger_hash ]
               list
             | `Staged_ledger_application_failed of
               Staged_ledger.Staged_ledger_error.t ] )
           Deferred.Result.t =
     fun (t, validation) ~logger ~verifier ~parent_staged_ledger ->
      let open Deferred.Result.Let_syntax in
      let transition = With_hash.data t in
      let blockchain_state =
        Protocol_state.blockchain_state (protocol_state transition)
      in
      let staged_ledger_diff = staged_ledger_diff transition in
      let%bind ( `Hash_after_applying staged_ledger_hash
               , `Ledger_proof proof_opt
               , `Staged_ledger transitioned_staged_ledger
               , `Pending_coinbase_data _ ) =
        Staged_ledger.apply ~logger ~verifier parent_staged_ledger
          staged_ledger_diff
        |> Deferred.Result.map_error ~f:(fun e ->
               `Staged_ledger_application_failed e )
      in
      let target_ledger_hash =
        match proof_opt with
        | None ->
            Option.value_map
              (Staged_ledger.current_ledger_proof transitioned_staged_ledger)
              ~f:target_hash_of_ledger_proof
              ~default:
                (Frozen_ledger_hash.of_ledger_hash
                   (Ledger.merkle_root (Lazy.force Genesis_ledger.t)))
        | Some (proof, _) ->
            target_hash_of_ledger_proof proof
      in
      let maybe_errors =
        Option.all
          [ Option.some_if
              (not
                 (Staged_ledger_hash.equal staged_ledger_hash
                    (Blockchain_state.staged_ledger_hash blockchain_state)))
              `Incorrect_target_staged_ledger_hash
          ; Option.some_if
              (not
                 (Frozen_ledger_hash.equal target_ledger_hash
                    (Blockchain_state.snarked_ledger_hash blockchain_state)))
              `Incorrect_target_snarked_ledger_hash ]
      in
      Deferred.return
        ( match maybe_errors with
        | Some errors ->
            Error (`Invalid_staged_ledger_diff errors)
        | None ->
            Ok
              ( `Just_emitted_a_proof (Option.is_some proof_opt)
              , `External_transition_with_validation
                  (t, Validation.Unsafe.set_valid_staged_ledger_diff validation)
              , `Staged_ledger transitioned_staged_ledger ) )
  end
end

include Make (Ledger_proof) (Verifier)
          (struct
            include Staged_ledger_diff.Stable.V1

            [%%define_locally
            Staged_ledger_diff.(creator, user_commands)]
          end)
