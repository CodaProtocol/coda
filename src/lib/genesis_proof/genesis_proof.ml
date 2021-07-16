open Core_kernel
open Mina_base
open Mina_state

module Inputs = struct
  type t =
    { runtime_config : Runtime_config.t
    ; constraint_constants : Genesis_constants.Constraint_constants.t
    ; proof_level : Genesis_constants.Proof_level.t
    ; genesis_constants : Genesis_constants.t
    ; genesis_ledger : Genesis_ledger.Packed.t
    ; genesis_epoch_data : Consensus.Genesis_epoch_data.t
    ; consensus_constants : Consensus.Constants.t
    ; protocol_state_with_hash :
        (Protocol_state.value, State_hash.t) With_hash.t
    ; constraint_system_digests : (string * Md5_lib.t) list option
    ; blockchain_proof_system_id :
        (* This is only used for calculating the hash to lookup the genesis
           proof with. It is re-calculated when building the blockchain prover,
           so it is always okay -- if less efficient at startup -- to pass
           [None] here.
        *)
        Pickles.Verification_key.Id.t option
    }

  let runtime_config { runtime_config; _ } = runtime_config

  let constraint_constants { constraint_constants; _ } = constraint_constants

  let genesis_constants { genesis_constants; _ } = genesis_constants

  let proof_level { proof_level; _ } = proof_level

  let protocol_constants t = (genesis_constants t).protocol

  let ledger_depth { genesis_ledger; _ } =
    Genesis_ledger.Packed.depth genesis_ledger

  include Genesis_ledger.Utils

  let genesis_ledger { genesis_ledger; _ } =
    Genesis_ledger.Packed.t genesis_ledger

  let genesis_epoch_data { genesis_epoch_data; _ } = genesis_epoch_data

  let accounts { genesis_ledger; _ } =
    Genesis_ledger.Packed.accounts genesis_ledger

  let find_new_account_record_exn { genesis_ledger; _ } =
    Genesis_ledger.Packed.find_new_account_record_exn genesis_ledger

  let find_new_account_record_exn_ { genesis_ledger; _ } =
    Genesis_ledger.Packed.find_new_account_record_exn_ genesis_ledger

  let largest_account_exn { genesis_ledger; _ } =
    Genesis_ledger.Packed.largest_account_exn genesis_ledger

  let largest_account_keypair_exn { genesis_ledger; _ } =
    Genesis_ledger.Packed.largest_account_keypair_exn genesis_ledger

  let largest_account_pk_exn { genesis_ledger; _ } =
    Genesis_ledger.Packed.largest_account_pk_exn genesis_ledger

  let consensus_constants { consensus_constants; _ } = consensus_constants

  let genesis_state_with_hash { protocol_state_with_hash; _ } =
    protocol_state_with_hash

  let genesis_state t = (genesis_state_with_hash t).data

  let genesis_state_hash t = (genesis_state_with_hash t).hash
end

module Proof_data = struct
  type t =
    { blockchain_proof_system_id : Pickles.Verification_key.Id.t
    ; genesis_proof : Proof.t
    }
end

module T = struct
  type t =
    { runtime_config : Runtime_config.t
    ; constraint_constants : Genesis_constants.Constraint_constants.t
    ; genesis_constants : Genesis_constants.t
    ; proof_level : Genesis_constants.Proof_level.t
    ; genesis_ledger : Genesis_ledger.Packed.t
    ; genesis_epoch_data : Consensus.Genesis_epoch_data.t
    ; consensus_constants : Consensus.Constants.t
    ; protocol_state_with_hash :
        (Protocol_state.value, State_hash.t) With_hash.t
    ; constraint_system_digests : (string * Md5_lib.t) list Lazy.t
    ; proof_data : Proof_data.t option
    }

  let runtime_config { runtime_config; _ } = runtime_config

  let constraint_constants { constraint_constants; _ } = constraint_constants

  let genesis_constants { genesis_constants; _ } = genesis_constants

  let proof_level { proof_level; _ } = proof_level

  let protocol_constants t = (genesis_constants t).protocol

  let ledger_depth { genesis_ledger; _ } =
    Genesis_ledger.Packed.depth genesis_ledger

  include Genesis_ledger.Utils

  let genesis_ledger { genesis_ledger; _ } =
    Genesis_ledger.Packed.t genesis_ledger

  let genesis_epoch_data { genesis_epoch_data; _ } = genesis_epoch_data

  let accounts { genesis_ledger; _ } =
    Genesis_ledger.Packed.accounts genesis_ledger

  let find_new_account_record_exn { genesis_ledger; _ } =
    Genesis_ledger.Packed.find_new_account_record_exn genesis_ledger

  let find_new_account_record_exn_ { genesis_ledger; _ } =
    Genesis_ledger.Packed.find_new_account_record_exn_ genesis_ledger

  let largest_account_exn { genesis_ledger; _ } =
    Genesis_ledger.Packed.largest_account_exn genesis_ledger

  let largest_account_keypair_exn { genesis_ledger; _ } =
    Genesis_ledger.Packed.largest_account_keypair_exn genesis_ledger

  let largest_account_pk_exn { genesis_ledger; _ } =
    Genesis_ledger.Packed.largest_account_pk_exn genesis_ledger

  let consensus_constants { consensus_constants; _ } = consensus_constants

  let genesis_state_with_hash { protocol_state_with_hash; _ } =
    protocol_state_with_hash

  let genesis_state t = (genesis_state_with_hash t).data

  let genesis_state_hash t = (genesis_state_with_hash t).hash

  let genesis_proof { proof_data; _ } =
    Option.map proof_data ~f:(fun { Proof_data.genesis_proof = p; _ } -> p)
end

include T

let base_proof (module B : Blockchain_snark.Blockchain_snark_state.S)
    (t : Inputs.t) =
  let genesis_ledger = Genesis_ledger.Packed.t t.genesis_ledger in
  let constraint_constants = t.constraint_constants in
  let consensus_constants = t.consensus_constants in
  let prev_state =
    Protocol_state.negative_one ~genesis_ledger
      ~genesis_epoch_data:t.genesis_epoch_data ~constraint_constants
      ~consensus_constants
  in
  let curr = t.protocol_state_with_hash.data in
  let dummy_txn_stmt : Transaction_snark.Statement.With_sok.t =
    let reg (t : Blockchain_state.Value.t) =
      { t.registers with
        next_available_token = Token_id.(next default)
      ; pending_coinbase_stack = Mina_base.Pending_coinbase.Stack.empty
      }
    in
    { sok_digest = Mina_base.Sok_message.Digest.default
    ; source = reg (Protocol_state.blockchain_state prev_state)
    ; target = reg (Protocol_state.blockchain_state curr)
    ; supply_increase = Currency.Amount.zero
    ; fee_excess = Fee_excess.zero
    }
  in
  let genesis_epoch_ledger =
    match t.genesis_epoch_data with
    | None ->
        genesis_ledger
    | Some data ->
        data.staking.ledger
  in
  let open Pickles_types in
  let blockchain_dummy = Pickles.Proof.dummy Nat.N2.n Nat.N2.n Nat.N2.n in
  let txn_dummy = Pickles.Proof.dummy Nat.N2.n Nat.N2.n Nat.N0.n in
  B.step
    ~handler:
      (Consensus.Data.Prover_state.precomputed_handler ~constraint_constants
         ~genesis_epoch_ledger)
    { transition =
        Snark_transition.genesis ~constraint_constants ~consensus_constants
          ~genesis_ledger
    ; prev_state
    }
    [ (prev_state, blockchain_dummy); (dummy_txn_stmt, txn_dummy) ]
    t.protocol_state_with_hash.data

let digests (module T : Transaction_snark.S)
    (module B : Blockchain_snark.Blockchain_snark_state.S) =
  let open Lazy.Let_syntax in
  let%bind txn_digests = T.constraint_system_digests in
  let%map blockchain_digests = B.constraint_system_digests in
  txn_digests @ blockchain_digests

let blockchain_snark_state (inputs : Inputs.t) :
    (module Transaction_snark.S)
    * (module Blockchain_snark.Blockchain_snark_state.S) =
  let module T = Transaction_snark.Make (struct
    let constraint_constants = inputs.constraint_constants

    let proof_level = inputs.proof_level
  end) in
  let module B = Blockchain_snark.Blockchain_snark_state.Make (struct
    let tag = T.tag

    let constraint_constants = inputs.constraint_constants

    let proof_level = inputs.proof_level
  end) in
  ((module T), (module B))

let create_values txn b (t : Inputs.t) =
  let%map.Async.Deferred genesis_proof = base_proof b t in
  { runtime_config = t.runtime_config
  ; constraint_constants = t.constraint_constants
  ; proof_level = t.proof_level
  ; genesis_constants = t.genesis_constants
  ; genesis_ledger = t.genesis_ledger
  ; genesis_epoch_data = t.genesis_epoch_data
  ; consensus_constants = t.consensus_constants
  ; protocol_state_with_hash = t.protocol_state_with_hash
  ; constraint_system_digests = digests txn b
  ; proof_data =
      Some
        { blockchain_proof_system_id =
            (let (module B) = b in
             Lazy.force B.Proof.id)
        ; genesis_proof
        }
  }

let create_values_no_proof (t : Inputs.t) =
  { runtime_config = t.runtime_config
  ; constraint_constants = t.constraint_constants
  ; proof_level = t.proof_level
  ; genesis_constants = t.genesis_constants
  ; genesis_ledger = t.genesis_ledger
  ; genesis_epoch_data = t.genesis_epoch_data
  ; consensus_constants = t.consensus_constants
  ; protocol_state_with_hash = t.protocol_state_with_hash
  ; constraint_system_digests =
      lazy
        (let txn, b = blockchain_snark_state t in
         Lazy.force (digests txn b))
  ; proof_data = None
  }

let to_inputs (t : t) : Inputs.t =
  { runtime_config = t.runtime_config
  ; constraint_constants = t.constraint_constants
  ; proof_level = t.proof_level
  ; genesis_constants = t.genesis_constants
  ; genesis_ledger = t.genesis_ledger
  ; genesis_epoch_data = t.genesis_epoch_data
  ; consensus_constants = t.consensus_constants
  ; protocol_state_with_hash = t.protocol_state_with_hash
  ; constraint_system_digests =
      ( if Lazy.is_val t.constraint_system_digests then
        Some (Lazy.force t.constraint_system_digests)
      else None )
  ; blockchain_proof_system_id =
      ( match t.proof_data with
      | Some { blockchain_proof_system_id; _ } ->
          Some blockchain_proof_system_id
      | None ->
          None )
  }
