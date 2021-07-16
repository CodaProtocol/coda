open Async_kernel
open Core_kernel
open Mina_base
open Mina_state

module Validate_content = struct
  type t = Mina_net2.Validation_callback.t

  let bin_read_t buf ~pos_ref =
    bin_read_unit buf ~pos_ref ;
    Mina_net2.Validation_callback.create_without_expiration ()

  let bin_write_t buf ~pos _ = bin_write_unit buf ~pos ()

  let bin_shape_t = bin_shape_unit

  let bin_size_t _ = bin_size_unit ()

  let t_of_sexp _ = Mina_net2.Validation_callback.create_without_expiration ()

  let sexp_of_t _ = sexp_of_unit ()

  let compare _ _ = 0

  let __versioned__ = ()
end

(* do not expose refer to types in here directly; use allocation functor version instead *)
module Raw_versioned__ = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t =
        { protocol_state : Protocol_state.Value.Stable.V2.t
        ; protocol_state_proof : Proof.Stable.V1.t [@sexp.opaque]
        ; staged_ledger_diff : Staged_ledger_diff.Stable.V2.t
        ; delta_transition_chain_proof :
            State_hash.Stable.V1.t * State_body_hash.Stable.V1.t list
        ; current_protocol_version : Protocol_version.Stable.V1.t
        ; proposed_protocol_version_opt : Protocol_version.Stable.V1.t option
        ; mutable validation_callback : Validate_content.t
        }
      [@@deriving compare, sexp, fields]

      let to_latest = Fn.id

      type 'a creator =
           protocol_state:Protocol_state.Value.t
        -> protocol_state_proof:Proof.t
        -> staged_ledger_diff:Staged_ledger_diff.t
        -> delta_transition_chain_proof:State_hash.t * State_body_hash.t list
        -> validation_callback:Validate_content.t
        -> ?proposed_protocol_version_opt:Protocol_version.t
        -> unit
        -> 'a

      let map_creator c ~f ~protocol_state ~protocol_state_proof
          ~staged_ledger_diff ~delta_transition_chain_proof ~validation_callback
          ?proposed_protocol_version_opt () =
        f
          (c ~protocol_state ~protocol_state_proof ~staged_ledger_diff
             ~delta_transition_chain_proof ~validation_callback
             ?proposed_protocol_version_opt ())

      let create ~protocol_state ~protocol_state_proof ~staged_ledger_diff
          ~delta_transition_chain_proof ~validation_callback
          ?proposed_protocol_version_opt () =
        let current_protocol_version =
          try Protocol_version.get_current ()
          with _ ->
            failwith
              "Cannot create external transition before setting current \
               protocol version"
        in
        { protocol_state
        ; protocol_state_proof
        ; staged_ledger_diff
        ; delta_transition_chain_proof
        ; current_protocol_version
        ; proposed_protocol_version_opt
        ; validation_callback
        }
    end

    module V1 = struct
      type t =
        { protocol_state : Protocol_state.Value.Stable.V1.t
        ; protocol_state_proof : Proof.Stable.V1.t [@sexp.opaque]
        ; staged_ledger_diff : Staged_ledger_diff.Stable.V1.t
        ; delta_transition_chain_proof :
            State_hash.Stable.V1.t * State_body_hash.Stable.V1.t list
        ; current_protocol_version : Protocol_version.Stable.V1.t
        ; proposed_protocol_version_opt : Protocol_version.Stable.V1.t option
        ; mutable validation_callback : Validate_content.t
        }
      [@@deriving compare, sexp, fields]

      let to_latest (t : t) : V2.t =
        { protocol_state =
            Protocol_state.Value.Stable.V1.to_latest t.protocol_state
        ; protocol_state_proof = t.protocol_state_proof
        ; staged_ledger_diff =
            Staged_ledger_diff.Stable.V1.to_latest t.staged_ledger_diff
        ; delta_transition_chain_proof = t.delta_transition_chain_proof
        ; current_protocol_version = t.current_protocol_version
        ; proposed_protocol_version_opt = t.proposed_protocol_version_opt
        ; validation_callback = t.validation_callback
        }

      type 'a creator =
           protocol_state:Protocol_state.Value.Stable.V1.t
        -> protocol_state_proof:Proof.t
        -> staged_ledger_diff:Staged_ledger_diff.Stable.V1.t
        -> delta_transition_chain_proof:State_hash.t * State_body_hash.t list
        -> validation_callback:Validate_content.t
        -> ?proposed_protocol_version_opt:Protocol_version.t
        -> unit
        -> 'a

      let map_creator c ~f ~protocol_state ~protocol_state_proof
          ~staged_ledger_diff ~delta_transition_chain_proof ~validation_callback
          ?proposed_protocol_version_opt () =
        f
          (c ~protocol_state ~protocol_state_proof ~staged_ledger_diff
             ~delta_transition_chain_proof ~validation_callback
             ?proposed_protocol_version_opt ())

      let create ~protocol_state ~protocol_state_proof ~staged_ledger_diff
          ~delta_transition_chain_proof ~validation_callback
          ?proposed_protocol_version_opt () =
        let current_protocol_version =
          try Protocol_version.get_current ()
          with _ ->
            failwith
              "Cannot create external transition before setting current \
               protocol version"
        in
        { protocol_state
        ; protocol_state_proof
        ; staged_ledger_diff
        ; delta_transition_chain_proof
        ; current_protocol_version
        ; proposed_protocol_version_opt
        ; validation_callback
        }
    end
  end]
end

include Allocation_functor.Make.Versioned_v2.Sexp (struct
  let id = "external_transition"

  include Raw_versioned__
end)

[%%define_locally
Raw_versioned__.
  ( protocol_state
  , protocol_state_proof
  , staged_ledger_diff
  , delta_transition_chain_proof
  , current_protocol_version
  , proposed_protocol_version_opt
  , validation_callback
  , set_validation_callback
  , compare )]

[%%define_locally Stable.Latest.(create, sexp_of_t, t_of_sexp)]

type external_transition = t

(*
type t_ = Raw_versioned__.t =
  { protocol_state: Protocol_state.Value.t
  ; protocol_state_proof: Proof.t [@sexp.opaque]
  ; staged_ledger_diff: Staged_ledger_diff.t
  ; delta_transition_chain_proof: State_hash.t * State_body_hash.t list
  ; current_protocol_version: Protocol_version.t
  ; proposed_protocol_version_opt: Protocol_version.t option
  ; mutable validation_callback: Validate_content.t }
*)

module Precomputed_block = struct
  module Proof = struct
    type t = Proof.t

    let to_bin_string proof =
      let proof_string = Binable.to_string (module Proof.Stable.Latest) proof in
      (* We use base64 with the uri-safe alphabet to ensure that encoding and
         decoding is cheap, and that the proof can be easily sent over http
         etc. without escaping or re-encoding.
      *)
      Base64.encode_string ~alphabet:Base64.uri_safe_alphabet proof_string

    let of_bin_string str =
      let str = Base64.decode_exn ~alphabet:Base64.uri_safe_alphabet str in
      Binable.of_string (module Proof.Stable.Latest) str

    let sexp_of_t proof = Sexp.Atom (to_bin_string proof)

    let _sexp_of_t_structured = Proof.sexp_of_t

    (* Supports decoding base64-encoded and structure encoded proofs. *)
    let t_of_sexp = function
      | Sexp.Atom str ->
          of_bin_string str
      | sexp ->
          Proof.t_of_sexp sexp

    let to_yojson proof = `String (to_bin_string proof)

    let _to_yojson_structured = Proof.to_yojson

    let of_yojson = function
      | `String str ->
          Or_error.try_with (fun () -> of_bin_string str)
          |> Result.map_error ~f:(fun err ->
                 sprintf
                   "External_transition.Precomputed_block.Proof.of_yojson: %s"
                   (Error.to_string_hum err))
      | json ->
          Proof.of_yojson json
  end

  module T = struct
    type t =
      { scheduled_time : Block_time.t
      ; protocol_state : Protocol_state.value
      ; protocol_state_proof : Proof.t
      ; staged_ledger_diff : Staged_ledger_diff.t
      ; delta_transition_chain_proof :
          Frozen_ledger_hash.t * Frozen_ledger_hash.t list
      }
    [@@deriving sexp, yojson]
  end

  include T

  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V2 = struct
      type t = T.t =
        { scheduled_time : Block_time.Stable.V1.t
        ; protocol_state : Protocol_state.Value.Stable.V2.t
        ; protocol_state_proof : Mina_base.Proof.Stable.V1.t
        ; staged_ledger_diff : Staged_ledger_diff.Stable.V2.t
              (* TODO: Delete this or find out why it is here. *)
        ; delta_transition_chain_proof :
            Frozen_ledger_hash.Stable.V1.t * Frozen_ledger_hash.Stable.V1.t list
        }

      let to_latest = Fn.id
    end

    module V1 = struct
      type t =
        { scheduled_time : Block_time.Stable.V1.t
        ; protocol_state : Protocol_state.Value.Stable.V1.t
        ; protocol_state_proof : Mina_base.Proof.Stable.V1.t
        ; staged_ledger_diff : Staged_ledger_diff.Stable.V1.t
        ; delta_transition_chain_proof :
            Frozen_ledger_hash.Stable.V1.t * Frozen_ledger_hash.Stable.V1.t list
        }

      let to_latest (t : t) : V2.t =
        { scheduled_time = t.scheduled_time
        ; protocol_state =
            Protocol_state.Value.Stable.V1.to_latest t.protocol_state
        ; protocol_state_proof = t.protocol_state_proof
        ; staged_ledger_diff =
            Staged_ledger_diff.Stable.V1.to_latest t.staged_ledger_diff
        ; delta_transition_chain_proof = t.delta_transition_chain_proof
        }
    end
  end]

  let of_external_transition ~scheduled_time (t : external_transition) =
    { scheduled_time
    ; protocol_state = t.protocol_state
    ; protocol_state_proof = t.protocol_state_proof
    ; staged_ledger_diff = t.staged_ledger_diff
    ; delta_transition_chain_proof = t.delta_transition_chain_proof
    }

  (* NOTE: This serialization is used externally and MUST NOT change.
     If the underlying types change, you should write a conversion, or add
     optional fields and handle them appropriately.
  *)
  let%test_unit "Sexp serialization is stable" =
    let serialized_block =
      External_transition_sample_precomputed_block.sample_block_sexp
    in
    ignore @@ t_of_sexp @@ Sexp.of_string serialized_block

  let%test_unit "Sexp serialization roundtrips" =
    let serialized_block =
      External_transition_sample_precomputed_block.sample_block_sexp
    in
    let sexp = Sexp.of_string serialized_block in
    let sexp_roundtrip = sexp_of_t @@ t_of_sexp sexp in
    [%test_eq: Sexp.t] sexp sexp_roundtrip

  (* NOTE: This serialization is used externally and MUST NOT change.
     If the underlying types change, you should write a conversion, or add
     optional fields and handle them appropriately.
  *)
  let%test_unit "JSON serialization is stable" =
    let serialized_block =
      External_transition_sample_precomputed_block.sample_block_json
    in
    match of_yojson @@ Yojson.Safe.from_string serialized_block with
    | Ok _ ->
        ()
    | Error err ->
        failwith err

  let%test_unit "JSON serialization roundtrips" =
    let serialized_block =
      External_transition_sample_precomputed_block.sample_block_json
    in
    let json = Yojson.Safe.from_string serialized_block in
    let json_roundtrip =
      match Result.map ~f:to_yojson @@ of_yojson json with
      | Ok json ->
          json
      | Error err ->
          failwith err
    in
    assert (Yojson.Safe.equal json json_roundtrip)
end

let consensus_state = Fn.compose Protocol_state.consensus_state protocol_state

let blockchain_state = Fn.compose Protocol_state.blockchain_state protocol_state

let state_hash = Fn.compose Protocol_state.hash protocol_state

let parent_hash = Fn.compose Protocol_state.previous_state_hash protocol_state

let blockchain_length =
  Fn.compose Consensus.Data.Consensus_state.blockchain_length consensus_state

let consensus_time_produced_at =
  Fn.compose Consensus.Data.Consensus_state.consensus_time consensus_state

let global_slot =
  Fn.compose Consensus.Data.Consensus_state.curr_global_slot consensus_state

let block_producer =
  Fn.compose Consensus.Data.Consensus_state.block_creator consensus_state

let coinbase_receiver =
  Fn.compose Consensus.Data.Consensus_state.coinbase_receiver consensus_state

let supercharge_coinbase =
  Fn.compose Consensus.Data.Consensus_state.supercharge_coinbase consensus_state

let block_winner =
  Fn.compose Consensus.Data.Consensus_state.block_stake_winner consensus_state

let commands = Fn.compose Staged_ledger_diff.commands staged_ledger_diff

let completed_works =
  Fn.compose Staged_ledger_diff.completed_works staged_ledger_diff

let to_yojson t =
  `Assoc
    [ ("protocol_state", Protocol_state.value_to_yojson (protocol_state t))
    ; ("protocol_state_proof", `String "<opaque>")
    ; ("staged_ledger_diff", `String "<opaque>")
    ; ("delta_transition_chain_proof", `String "<opaque>")
    ; ( "current_protocol_version"
      , `String (Protocol_version.to_string (current_protocol_version t)) )
    ; ( "proposed_protocol_version"
      , `String
          (Option.value_map
             (proposed_protocol_version_opt t)
             ~default:"<None>" ~f:Protocol_version.to_string) )
    ]

let equal =
  Comparable.lift Consensus.Data.Consensus_state.Value.equal ~f:consensus_state

let transactions ~constraint_constants t =
  let open Staged_ledger.Pre_diff_info in
  let coinbase_receiver =
    Consensus.Data.Consensus_state.coinbase_receiver (consensus_state t)
  in
  let supercharge_coinbase =
    Consensus.Data.Consensus_state.supercharge_coinbase (consensus_state t)
  in
  match
    get_transactions ~constraint_constants ~coinbase_receiver
      ~supercharge_coinbase (staged_ledger_diff t)
  with
  | Ok transactions ->
      transactions
  | Error e ->
      Core.Error.raise (Error.to_error e)

let payments t =
  List.filter_map (commands t) ~f:(function
    | { data = Signed_command ({ payload = { body = Payment _; _ }; _ } as c)
      ; status
      } ->
        Some { With_status.data = c; status }
    | _ ->
        None)

let accept t =
  Mina_net2.Validation_callback.fire_exn (validation_callback t) `Accept

let reject t =
  Mina_net2.Validation_callback.fire_exn (validation_callback t) `Reject

let poke_validation_callback t cb = set_validation_callback t cb

let timestamp =
  Fn.compose Blockchain_state.timestamp
    (Fn.compose Protocol_state.blockchain_state protocol_state)

type protocol_version_status =
  { valid_current : bool; valid_next : bool; matches_daemon : bool }

let protocol_version_status t =
  let valid_current = Protocol_version.is_valid (current_protocol_version t) in
  let valid_next =
    Option.for_all
      (proposed_protocol_version_opt t)
      ~f:Protocol_version.is_valid
  in
  let matches_daemon =
    Protocol_version.compatible_with_daemon (current_protocol_version t)
  in
  { valid_current; valid_next; matches_daemon }

module Validation = struct
  type ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       t =
    'time_received
    * 'genesis_state
    * 'proof
    * 'delta_transition_chain
    * 'frontier_dependencies
    * 'staged_ledger_diff
    * 'protocol_versions
    constraint 'time_received = [ `Time_received ] * (unit, _) Truth.t
    constraint 'genesis_state = [ `Genesis_state ] * (unit, _) Truth.t
    constraint 'proof = [ `Proof ] * (unit, _) Truth.t
    constraint
      'delta_transition_chain =
      [ `Delta_transition_chain ] * (State_hash.t Non_empty_list.t, _) Truth.t
    constraint
      'frontier_dependencies =
      [ `Frontier_dependencies ] * (unit, _) Truth.t
    constraint 'staged_ledger_diff = [ `Staged_ledger_diff ] * (unit, _) Truth.t
    constraint 'protocol_versions = [ `Protocol_versions ] * (unit, _) Truth.t

  type fully_invalid =
    ( [ `Time_received ] * unit Truth.false_t
    , [ `Genesis_state ] * unit Truth.false_t
    , [ `Proof ] * unit Truth.false_t
    , [ `Delta_transition_chain ] * State_hash.t Non_empty_list.t Truth.false_t
    , [ `Frontier_dependencies ] * unit Truth.false_t
    , [ `Staged_ledger_diff ] * unit Truth.false_t
    , [ `Protocol_versions ] * unit Truth.false_t )
    t

  type fully_valid =
    ( [ `Time_received ] * unit Truth.true_t
    , [ `Genesis_state ] * unit Truth.true_t
    , [ `Proof ] * unit Truth.true_t
    , [ `Delta_transition_chain ] * State_hash.t Non_empty_list.t Truth.true_t
    , [ `Frontier_dependencies ] * unit Truth.true_t
    , [ `Staged_ledger_diff ] * unit Truth.true_t
    , [ `Protocol_versions ] * unit Truth.true_t )
    t

  type ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'protocol_versions )
       with_transition =
    (external_transition, State_hash.t) With_hash.t
    * ( 'time_received
      , 'genesis_state
      , 'proof
      , 'delta_transition_chain
      , 'frontier_dependencies
      , 'staged_ledger_diff
      , 'protocol_versions )
      t

  let fully_invalid =
    ( (`Time_received, Truth.False)
    , (`Genesis_state, Truth.False)
    , (`Proof, Truth.False)
    , (`Delta_transition_chain, Truth.False)
    , (`Frontier_dependencies, Truth.False)
    , (`Staged_ledger_diff, Truth.False)
    , (`Protocol_versions, Truth.False) )

  type initial_valid =
    ( [ `Time_received ] * unit Truth.true_t
    , [ `Genesis_state ] * unit Truth.true_t
    , [ `Proof ] * unit Truth.true_t
    , [ `Delta_transition_chain ] * State_hash.t Non_empty_list.t Truth.true_t
    , [ `Frontier_dependencies ] * unit Truth.false_t
    , [ `Staged_ledger_diff ] * unit Truth.false_t
    , [ `Protocol_versions ] * unit Truth.true_t )
    t

  type almost_valid =
    ( [ `Time_received ] * unit Truth.true_t
    , [ `Genesis_state ] * unit Truth.true_t
    , [ `Proof ] * unit Truth.true_t
    , [ `Delta_transition_chain ] * State_hash.t Non_empty_list.t Truth.true_t
    , [ `Frontier_dependencies ] * unit Truth.true_t
    , [ `Staged_ledger_diff ] * unit Truth.false_t
    , [ `Protocol_versions ] * unit Truth.true_t )
    t

  let wrap t = (t, fully_invalid)

  let extract_delta_transition_chain_witness = function
    | ( _
      , _
      , _
      , (`Delta_transition_chain, Truth.True delta_transition_chain_witness)
      , _
      , _
      , _ ) ->
        delta_transition_chain_witness
    | _ ->
        failwith "why can't this be refuted?"

  let reset_frontier_dependencies_validation (transition_with_hash, validation)
      =
    match validation with
    | ( time_received
      , genesis_state
      , proof
      , delta_transition_chain
      , (`Frontier_dependencies, Truth.True ())
      , staged_ledger_diff
      , protocol_versions ) ->
        ( transition_with_hash
        , ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , (`Frontier_dependencies, Truth.False)
          , staged_ledger_diff
          , protocol_versions ) )
    | _ ->
        failwith "why can't this be refuted?"

  let reset_staged_ledger_diff_validation (transition_with_hash, validation) =
    match validation with
    | ( time_received
      , genesis_state
      , proof
      , delta_transition_chain
      , frontier_dependencies
      , (`Staged_ledger_diff, Truth.True ())
      , protocol_versions ) ->
        ( transition_with_hash
        , ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , (`Staged_ledger_diff, Truth.False)
          , protocol_versions ) )
    | _ ->
        failwith "why can't this be refuted?"

  let forget_validation (t, _) = With_hash.data t

  let forget_validation_with_hash (t, _) = t

  module Unsafe = struct
    let set_valid_time_received :
           ( [ `Time_received ] * unit Truth.false_t
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'protocol_versions )
           t
        -> ( [ `Time_received ] * unit Truth.true_t
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'protocol_versions )
           t = function
      | ( (`Time_received, Truth.False)
        , genesis_state
        , proof
        , delta_transition_chain
        , frontier_dependencies
        , staged_ledger_diff
        , protocol_versions ) ->
          ( (`Time_received, Truth.True ())
          , genesis_state
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , staged_ledger_diff
          , protocol_versions )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_proof :
           ( 'time_received
           , 'genesis_state
           , [ `Proof ] * unit Truth.false_t
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'protocol_versions )
           t
        -> ( 'time_received
           , 'genesis_state
           , [ `Proof ] * unit Truth.true_t
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'protocol_versions )
           t = function
      | ( time_received
        , genesis_state
        , (`Proof, Truth.False)
        , delta_transition_chain
        , frontier_dependencies
        , staged_ledger_diff
        , protocol_versions ) ->
          ( time_received
          , genesis_state
          , (`Proof, Truth.True ())
          , delta_transition_chain
          , frontier_dependencies
          , staged_ledger_diff
          , protocol_versions )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_genesis_state :
           ( 'time_received
           , [ `Genesis_state ] * unit Truth.false_t
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'protocol_versions )
           t
        -> ( 'time_received
           , [ `Genesis_state ] * unit Truth.true_t
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'protocol_versions )
           t = function
      | ( time_received
        , (`Genesis_state, Truth.False)
        , proof
        , delta_transition_chain
        , frontier_dependencies
        , staged_ledger_diff
        , protocol_versions ) ->
          ( time_received
          , (`Genesis_state, Truth.True ())
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , staged_ledger_diff
          , protocol_versions )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_delta_transition_chain :
           ( 'time_received
           , 'genesis_state
           , 'proof
           , [ `Delta_transition_chain ]
             * State_hash.t Non_empty_list.t Truth.false_t
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'protocol_versions )
           t
        -> State_hash.t Non_empty_list.t
        -> ( 'time_received
           , 'genesis_state
           , 'proof
           , [ `Delta_transition_chain ]
             * State_hash.t Non_empty_list.t Truth.true_t
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'protocol_versions )
           t =
     fun validation hashes ->
      match validation with
      | ( time_received
        , genesis_state
        , proof
        , (`Delta_transition_chain, Truth.False)
        , frontier_dependencies
        , staged_ledger_diff
        , protocol_versions ) ->
          ( time_received
          , genesis_state
          , proof
          , (`Delta_transition_chain, Truth.True hashes)
          , frontier_dependencies
          , staged_ledger_diff
          , protocol_versions )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_frontier_dependencies :
           ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , [ `Frontier_dependencies ] * unit Truth.false_t
           , 'staged_ledger_diff
           , 'protocol_versions )
           t
        -> ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , [ `Frontier_dependencies ] * unit Truth.true_t
           , 'staged_ledger_diff
           , 'protocol_versions )
           t = function
      | ( time_received
        , genesis_state
        , proof
        , delta_transition_chain
        , (`Frontier_dependencies, Truth.False)
        , staged_ledger_diff
        , protocol_versions ) ->
          ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , (`Frontier_dependencies, Truth.True ())
          , staged_ledger_diff
          , protocol_versions )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_staged_ledger_diff :
           ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , [ `Staged_ledger_diff ] * unit Truth.false_t
           , 'protocol_versions )
           t
        -> ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , [ `Staged_ledger_diff ] * unit Truth.true_t
           , 'protocol_versions )
           t = function
      | ( time_received
        , genesis_state
        , proof
        , delta_transition_chain
        , frontier_dependencies
        , (`Staged_ledger_diff, Truth.False)
        , protocol_versions ) ->
          ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , (`Staged_ledger_diff, Truth.True ())
          , protocol_versions )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_protocol_versions :
           ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , [ `Protocol_versions ] * unit Truth.false_t )
           t
        -> ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , [ `Protocol_versions ] * unit Truth.true_t )
           t = function
      | ( time_received
        , genesis_state
        , proof
        , delta_transition_chain
        , frontier_dependencies
        , staged_ledger_diff
        , (`Protocol_versions, Truth.False) ) ->
          ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , staged_ledger_diff
          , (`Protocol_versions, Truth.True ()) )
      | _ ->
          failwith "why can't this be refuted?"
  end
end

let skip_time_received_validation `This_transition_was_not_received_via_gossip
    (t, validation) =
  (t, Validation.Unsafe.set_valid_time_received validation)

let skip_genesis_protocol_state_validation
    `This_transition_was_generated_internally (t, validation) =
  (t, Validation.Unsafe.set_valid_genesis_state validation)

let validate_time_received ~(precomputed_values : Precomputed_values.t)
    (t, validation) ~time_received =
  let consensus_state =
    With_hash.data t |> protocol_state |> Protocol_state.consensus_state
  in
  let constants = precomputed_values.consensus_constants in
  let received_unix_timestamp =
    Block_time.to_span_since_epoch time_received |> Block_time.Span.to_ms
  in
  match
    Consensus.Hooks.received_at_valid_time ~constants consensus_state
      ~time_received:received_unix_timestamp
  with
  | Ok () ->
      Ok (t, Validation.Unsafe.set_valid_time_received validation)
  | Error err ->
      Error (`Invalid_time_received err)

let skip_proof_validation `This_transition_was_generated_internally
    (t, validation) =
  (t, Validation.Unsafe.set_valid_proof validation)

let skip_delta_transition_chain_validation
    `This_transition_was_not_received_via_gossip (t, validation) =
  let previous_protocol_state_hash = With_hash.data t |> parent_hash in
  ( t
  , Validation.Unsafe.set_valid_delta_transition_chain validation
      (Non_empty_list.singleton previous_protocol_state_hash) )

let validate_genesis_protocol_state ~genesis_state_hash (t, validation) =
  let state = protocol_state (With_hash.data t) in
  if
    State_hash.equal
      (Protocol_state.genesis_state_hash state)
      genesis_state_hash
  then Ok (t, Validation.Unsafe.set_valid_genesis_state validation)
  else Error `Invalid_genesis_protocol_state

let validate_proofs tvs ~verifier =
  let open Deferred.Let_syntax in
  match%map
    Verifier.verify_blockchain_snarks verifier
      (List.map tvs ~f:(fun (t, _validation) ->
           let transition = With_hash.data t in
           Blockchain_snark.Blockchain.create
             ~state:(protocol_state transition)
             ~proof:(protocol_state_proof transition)))
  with
  | Ok verified ->
      if verified then
        Ok
          (List.map tvs ~f:(fun (t, validation) ->
               (t, Validation.Unsafe.set_valid_proof validation)))
      else Error `Invalid_proof
  | Error e ->
      Error (`Verifier_error e)

let validate_delta_transition_chain (t, validation) =
  let transition = With_hash.data t in
  match
    Transition_chain_verifier.verify ~target_hash:(parent_hash transition)
      ~transition_chain_proof:transition.delta_transition_chain_proof
  with
  | Some hashes ->
      Ok
        (t, Validation.Unsafe.set_valid_delta_transition_chain validation hashes)
  | None ->
      Error `Invalid_delta_transition_chain_proof

let validate_protocol_versions (t, validation) =
  let { valid_current; valid_next; matches_daemon } =
    protocol_version_status (With_hash.data t)
  in
  if not (valid_current && valid_next) then Error `Invalid_protocol_version
  else if not matches_daemon then Error `Mismatched_protocol_version
  else Ok (t, Validation.Unsafe.set_valid_protocol_versions validation)

let skip_frontier_dependencies_validation
    (_ :
      [ `This_transition_belongs_to_a_detached_subtree
      | `This_transition_was_loaded_from_persistence ]) (t, validation) =
  (t, Validation.Unsafe.set_valid_frontier_dependencies validation)

let validate_staged_ledger_hash
    (`Staged_ledger_already_materialized staged_ledger_hash) (t, validation) =
  if
    Staged_ledger_hash.equal staged_ledger_hash
      (Blockchain_state.staged_ledger_hash
         (blockchain_state (With_hash.data t)))
  then Ok (t, Validation.Unsafe.set_valid_staged_ledger_diff validation)
  else Error `Staged_ledger_hash_mismatch

let skip_staged_ledger_diff_validation
    `This_transition_has_a_trusted_staged_ledger (t, validation) =
  (t, Validation.Unsafe.set_valid_staged_ledger_diff validation)

let skip_protocol_versions_validation
    `This_transition_has_valid_protocol_versions (t, validation) =
  (t, Validation.Unsafe.set_valid_protocol_versions validation)

module With_validation = struct
  let compare (t1, _) (t2, _) = compare (With_hash.data t1) (With_hash.data t2)

  let state_hash (t, _) = With_hash.hash t

  let lift f (t, _) = With_hash.data t |> f

  let protocol_state t = lift protocol_state t

  let protocol_state_proof t = lift protocol_state_proof t

  let blockchain_state t = lift blockchain_state t

  let blockchain_length t = lift blockchain_length t

  let staged_ledger_diff t = lift staged_ledger_diff t

  let consensus_state t = lift consensus_state t

  let parent_hash t = lift parent_hash t

  let consensus_time_produced_at t = lift consensus_time_produced_at t

  let block_producer t = lift block_producer t

  let block_winner t = lift block_winner t

  let coinbase_receiver t = lift coinbase_receiver t

  let supercharge_coinbase t = lift supercharge_coinbase t

  let commands t = lift commands t

  let completed_works t = lift completed_works t

  let transactions ~constraint_constants t =
    lift (transactions ~constraint_constants) t

  let payments t = lift payments t

  let global_slot t = lift global_slot t

  let delta_transition_chain_proof t = lift delta_transition_chain_proof t

  let current_protocol_version t = lift current_protocol_version t

  let proposed_protocol_version_opt t = lift proposed_protocol_version_opt t

  let protocol_version_status t = lift protocol_version_status t

  let accept t = lift accept t

  let reject t = lift reject t

  let poke_validation_callback t = lift poke_validation_callback t
end

module Initial_validated = struct
  type t =
    (external_transition, State_hash.t) With_hash.t * Validation.initial_valid

  type nonrec protocol_version_status = protocol_version_status =
    { valid_current : bool; valid_next : bool; matches_daemon : bool }

  include With_validation
end

module Almost_validated = struct
  type t =
    (external_transition, State_hash.t) With_hash.t * Validation.almost_valid

  type nonrec protocol_version_status = protocol_version_status =
    { valid_current : bool; valid_next : bool; matches_daemon : bool }

  include With_validation
end

module Validated = struct
  [%%versioned_binable
  module Stable = struct
    module V2 = struct
      type t =
        (Stable.V2.t, State_hash.t) With_hash.t
        * ( [ `Time_received ] * (unit, Truth.True.t) Truth.t
          , [ `Genesis_state ] * (unit, Truth.True.t) Truth.t
          , [ `Proof ] * (unit, Truth.True.t) Truth.t
          , [ `Delta_transition_chain ]
            * (State_hash.t Non_empty_list.t, Truth.True.t) Truth.t
          , [ `Frontier_dependencies ] * (unit, Truth.True.t) Truth.t
          , [ `Staged_ledger_diff ] * (unit, Truth.True.t) Truth.t
          , [ `Protocol_versions ] * (unit, Truth.True.t) Truth.t )
          Validation.t

      let to_latest = Fn.id

      module Erased = struct
        (* if this type receives a new version, that changes the serialization of
                 the type `t', so that type must also get a new version
        *)
        [%%versioned
        module Stable = struct
          module V1 = struct
            type t =
              (Stable.V2.t, State_hash.Stable.V1.t) With_hash.Stable.V1.t
              * State_hash.Stable.V1.t Non_empty_list.Stable.V1.t
            [@@deriving sexp]

            let to_latest = Fn.id
          end
        end]
      end

      let erase (transition_with_hash, validation) =
        ( transition_with_hash
        , Validation.extract_delta_transition_chain_witness validation )

      let elaborate (transition_with_hash, delta_transition_chain_witness) =
        ( transition_with_hash
        , ( (`Time_received, Truth.True ())
          , (`Genesis_state, Truth.True ())
          , (`Proof, Truth.True ())
          , (`Delta_transition_chain, Truth.True delta_transition_chain_witness)
          , (`Frontier_dependencies, Truth.True ())
          , (`Staged_ledger_diff, Truth.True ())
          , (`Protocol_versions, Truth.True ()) ) )

      include Sexpable.Of_sexpable
                (Erased.Stable.V1)
                (struct
                  type nonrec t = t

                  let of_sexpable = elaborate

                  let to_sexpable = erase
                end)

      include Binable.Of_binable
                (Erased.Stable.V1)
                (struct
                  type nonrec t = t

                  let of_binable = elaborate

                  let to_binable = erase
                end)

      let to_yojson (transition_with_hash, _) =
        With_hash.to_yojson to_yojson State_hash.to_yojson transition_with_hash

      let create_unsafe_pre_hashed t =
        `I_swear_this_is_safe_see_my_comment
          ( Validation.wrap t
          |> skip_time_received_validation
               `This_transition_was_not_received_via_gossip
          |> skip_genesis_protocol_state_validation
               `This_transition_was_generated_internally
          |> skip_proof_validation `This_transition_was_generated_internally
          |> skip_delta_transition_chain_validation
               `This_transition_was_not_received_via_gossip
          |> skip_frontier_dependencies_validation
               `This_transition_belongs_to_a_detached_subtree
          |> skip_staged_ledger_diff_validation
               `This_transition_has_a_trusted_staged_ledger
          |> skip_protocol_versions_validation
               `This_transition_has_valid_protocol_versions )

      let create_unsafe t =
        create_unsafe_pre_hashed (With_hash.of_data t ~hash_data:state_hash)

      include With_validation
    end

    module V1 = struct
      type t =
        (Stable.V1.t, State_hash.t) With_hash.t
        * ( [ `Time_received ] * (unit, Truth.True.t) Truth.t
          , [ `Genesis_state ] * (unit, Truth.True.t) Truth.t
          , [ `Proof ] * (unit, Truth.True.t) Truth.t
          , [ `Delta_transition_chain ]
            * (State_hash.t Non_empty_list.t, Truth.True.t) Truth.t
          , [ `Frontier_dependencies ] * (unit, Truth.True.t) Truth.t
          , [ `Staged_ledger_diff ] * (unit, Truth.True.t) Truth.t
          , [ `Protocol_versions ] * (unit, Truth.True.t) Truth.t )
          Validation.t

      let to_latest ((t, v) : t) : V2.t =
        ({ t with data = Raw_versioned__.Stable.V1.to_latest t.data }, v)

      module Erased = struct
        (* if this type receives a new version, that changes the serialization of
                 the type `t', so that type must also get a new version
        *)
        [%%versioned
        module Stable = struct
          module V1 = struct
            type t =
              (Stable.V1.t, State_hash.Stable.V1.t) With_hash.Stable.V1.t
              * State_hash.Stable.V1.t Non_empty_list.Stable.V1.t
            [@@deriving sexp]

            let to_latest = Fn.id
          end
        end]
      end

      let erase (transition_with_hash, validation) =
        ( transition_with_hash
        , Validation.extract_delta_transition_chain_witness validation )

      let elaborate (transition_with_hash, delta_transition_chain_witness) =
        ( transition_with_hash
        , ( (`Time_received, Truth.True ())
          , (`Genesis_state, Truth.True ())
          , (`Proof, Truth.True ())
          , (`Delta_transition_chain, Truth.True delta_transition_chain_witness)
          , (`Frontier_dependencies, Truth.True ())
          , (`Staged_ledger_diff, Truth.True ())
          , (`Protocol_versions, Truth.True ()) ) )

      include Sexpable.Of_sexpable
                (Erased.Stable.V1)
                (struct
                  type nonrec t = t

                  let of_sexpable = elaborate

                  let to_sexpable = erase
                end)

      include Binable.Of_binable
                (Erased.Stable.V1)
                (struct
                  type nonrec t = t

                  let of_binable = elaborate

                  let to_binable = erase
                end)
    end
  end]

  type nonrec protocol_version_status = protocol_version_status =
    { valid_current : bool; valid_next : bool; matches_daemon : bool }

  [%%define_locally
  Stable.Latest.
    ( sexp_of_t
    , t_of_sexp
    , create_unsafe_pre_hashed
    , create_unsafe
    , protocol_state
    , delta_transition_chain_proof
    , current_protocol_version
    , proposed_protocol_version_opt
    , protocol_version_status
    , accept
    , reject
    , poke_validation_callback
    , protocol_state_proof
    , blockchain_state
    , blockchain_length
    , staged_ledger_diff
    , consensus_state
    , state_hash
    , parent_hash
    , consensus_time_produced_at
    , block_producer
    , block_winner
    , coinbase_receiver
    , supercharge_coinbase
    , transactions
    , commands
    , completed_works
    , payments
    , global_slot
    , erase
    , to_yojson )]

  include Comparable.Make (Stable.Latest)

  let to_initial_validated t =
    t |> Validation.reset_frontier_dependencies_validation
    |> Validation.reset_staged_ledger_diff_validation

  let commands (t : t) =
    List.map (commands t) ~f:(fun x ->
        (* This is safe because at this point the stage ledger diff has been
             applied successfully. *)
        let (`If_this_is_used_it_should_have_a_comment_justifying_it c) =
          User_command.to_valid_unsafe x.data
        in
        { x with data = c })
end

let genesis ~precomputed_values =
  let genesis_protocol_state =
    Precomputed_values.genesis_state_with_hash precomputed_values
  in
  let empty_diff = Staged_ledger_diff.empty_diff in
  (* the genesis transition is assumed to be valid *)
  let (`I_swear_this_is_safe_see_my_comment transition) =
    Validated.create_unsafe_pre_hashed
      (With_hash.map genesis_protocol_state ~f:(fun protocol_state ->
           create
             ~protocol_state
               (* We pass a dummy proof here, with the understanding that it will
                  never be validated except as part of the snark for the first
                  block produced (where we will explicitly generate the genesis
                  proof).
               *)
             ~protocol_state_proof:Proof.blockchain_dummy
             ~staged_ledger_diff:empty_diff
             ~validation_callback:
               (Mina_net2.Validation_callback.create_without_expiration ())
             ~delta_transition_chain_proof:
               (Protocol_state.previous_state_hash protocol_state, [])
             ()))
  in
  transition

module For_tests = struct
  let create ~protocol_state ~protocol_state_proof ~staged_ledger_diff
      ~delta_transition_chain_proof ~validation_callback
      ?proposed_protocol_version_opt () =
    Protocol_version.(set_current zero) ;
    create ~protocol_state ~protocol_state_proof ~staged_ledger_diff
      ~delta_transition_chain_proof ~validation_callback
      ?proposed_protocol_version_opt ()

  let genesis ~precomputed_values =
    Protocol_version.(set_current zero) ;
    genesis ~precomputed_values
end

module Transition_frontier_validation (Transition_frontier : sig
  type t

  module Breadcrumb : sig
    type t

    val validated_transition : t -> Validated.t
  end

  val root : t -> Breadcrumb.t

  val find : t -> State_hash.t -> Breadcrumb.t option
end) =
struct
  let validate_frontier_dependencies (t, validation) ~consensus_constants
      ~logger ~frontier =
    let open Result.Let_syntax in
    let hash = With_hash.hash t in
    let root_transition =
      Transition_frontier.root frontier
      |> Transition_frontier.Breadcrumb.validated_transition
      |> Validation.forget_validation_with_hash
    in
    let protocol_state = protocol_state (With_hash.data t) in
    let parent_hash = Protocol_state.previous_state_hash protocol_state in
    let%bind () =
      Result.ok_if_true
        (Transition_frontier.find frontier hash |> Option.is_none)
        ~error:`Already_in_frontier
    in
    let%bind () =
      (* need pervasive (=) in scope for comparing polymorphic variant *)
      let ( = ) = Stdlib.( = ) in
      Result.ok_if_true
        ( `Take
        = Consensus.Hooks.select ~constants:consensus_constants
            ~logger:
              (Logger.extend logger
                 [ ( "selection_context"
                   , `String
                       "External_transition.Transition_frontier_validation.validate_frontier_dependencies"
                   )
                 ])
            ~existing:(With_hash.map ~f:consensus_state root_transition)
            ~candidate:(With_hash.map ~f:consensus_state t) )
        ~error:`Not_selected_over_frontier_root
    in
    let%map () =
      Result.ok_if_true
        (Transition_frontier.find frontier parent_hash |> Option.is_some)
        ~error:`Parent_missing_from_frontier
    in
    (t, Validation.Unsafe.set_valid_frontier_dependencies validation)
end

module Staged_ledger_validation = struct
  let target_hash_of_ledger_proof =
    let open Ledger_proof in
    Fn.compose Registers.ledger (Fn.compose statement_target statement)

  let validate_staged_ledger_diff :
         ?skip_staged_ledger_verification:[ `All | `Proofs ]
      -> ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , [ `Staged_ledger_diff ] * unit Truth.false_t
         , 'protocol_versions )
         Validation.with_transition
      -> logger:Logger.t
      -> precomputed_values:Precomputed_values.t
      -> verifier:Verifier.t
      -> parent_staged_ledger:Staged_ledger.t
      -> parent_protocol_state:Protocol_state.value
      -> ( [ `Just_emitted_a_proof of bool ]
           * [ `External_transition_with_validation of
               ( 'time_received
               , 'genesis_state
               , 'proof
               , 'delta_transition_chain
               , 'frontier_dependencies
               , [ `Staged_ledger_diff ] * unit Truth.true_t
               , 'protocol_versions )
               Validation.with_transition ]
           * [ `Staged_ledger of Staged_ledger.t ]
         , [ `Invalid_staged_ledger_diff of
             [ `Incorrect_target_staged_ledger_hash
             | `Incorrect_target_snarked_ledger_hash ]
             list
           | `Staged_ledger_application_failed of
             Staged_ledger.Staged_ledger_error.t ] )
         Deferred.Result.t =
   fun ?skip_staged_ledger_verification (t, validation) ~logger
       ~precomputed_values ~verifier ~parent_staged_ledger
       ~parent_protocol_state ->
    let open Deferred.Result.Let_syntax in
    let transition = With_hash.data t in
    let blockchain_state =
      Protocol_state.blockchain_state (protocol_state transition)
    in
    let staged_ledger_diff = staged_ledger_diff transition in
    let coinbase_receiver = coinbase_receiver transition in
    let supercharge_coinbase =
      consensus_state transition
      |> Consensus.Data.Consensus_state.supercharge_coinbase
    in
    let apply_start_time = Core.Time.now () in
    let%bind ( `Hash_after_applying staged_ledger_hash
             , `Ledger_proof proof_opt
             , `Staged_ledger transitioned_staged_ledger
             , `Pending_coinbase_update _ ) =
      Staged_ledger.apply ?skip_verification:skip_staged_ledger_verification
        ~constraint_constants:precomputed_values.constraint_constants ~logger
        ~verifier parent_staged_ledger staged_ledger_diff
        ~current_state_view:
          Mina_state.Protocol_state.(Body.view @@ body parent_protocol_state)
        ~state_and_body_hash:
          (let body_hash =
             Protocol_state.(Body.hash @@ body parent_protocol_state)
           in
           ( Protocol_state.hash_with_body parent_protocol_state ~body_hash
           , body_hash ))
        ~coinbase_receiver ~supercharge_coinbase
      |> Deferred.Result.map_error ~f:(fun e ->
             `Staged_ledger_application_failed e)
    in
    [%log debug]
      ~metadata:
        [ ( "time_elapsed"
          , `Float Core.Time.(Span.to_ms @@ diff (now ()) apply_start_time) )
        ]
      "Staged_ledger.apply takes $time_elapsed" ;
    let target_ledger_hash =
      match proof_opt with
      | None ->
          Option.value_map
            (Staged_ledger.current_ledger_proof transitioned_staged_ledger)
            ~f:target_hash_of_ledger_proof
            ~default:
              ( Precomputed_values.genesis_ledger precomputed_values
              |> Lazy.force |> Ledger.merkle_root
              |> Frozen_ledger_hash.of_ledger_hash )
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
            `Incorrect_target_snarked_ledger_hash
        ]
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
