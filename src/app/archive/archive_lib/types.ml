open Core
open Coda_base
open Coda_state
open Coda_transition
open Graphql_query.Base_types

(** Library used to encode and decode types to a graphql format.

    Unfortunately, the graphql_ppx does not have an "encode" attribute that
    allows us to map an OCaml type into some graphql schema type in a clean
    way. Therefore, we have to make our own encode and decode functions. On top
    of this, the generated GraphQL schema constructed by Hasura creates insert
    methods where the fields for each argument are optional, even though the
    inputs are explicitly labeled as NOT NULL in Postgres.Therefore, we are
    forced to lift the option types to these nested types. Furthermore, some
    types that are in Postgres but are not GraphQL primitive types are treated
    as custom scalars. These types include `bigint`, `bit(n)` and enums.
    graphql_ppx treats custom scalars as Yojson.Basic.t types (usually they are
    encoded as Json string types).

    **)

let encode_as_obj_rel_insert_input data
    (on_conflict : ('constraint_, 'update_columns) Ast.On_conflict.t) =
  object
    method data = data

    method on_conflict = Some on_conflict
  end

let encode_as_arr_rel_insert_input data
    (on_conflict : ('constraint_, 'update_columns) Ast.On_conflict.t) =
  object
    method data = Array.of_list data

    method on_conflict = Some on_conflict
  end

module Public_key = struct
  let encode public_key =
    object
      method blocks = None

      method fee_transfers = None

      method snark_jobs = None

      method userCommandsByReceiver = None

      method user_commands = None

      method value =
        Option.some
        @@ Signature_lib.Public_key.Compressed.to_base58_check public_key
    end

  let encode_as_obj_rel_insert_input public_key =
    encode_as_obj_rel_insert_input (encode public_key)
      Ast.On_conflict.public_keys
end

module User_command = struct
  let receiver user_command =
    match (User_command.payload user_command).body with
    | Payment payment ->
        payment.receiver
    | Stake_delegation (Set_delegate delegation) ->
        delegation.new_delegate

  let encode {With_hash.data= user_command; hash} first_seen =
    let payload = User_command.payload user_command in
    let body = payload.body in
    let open Option in
    object
      method hash = some @@ Transaction_hash.to_base58_check hash

      method blocks_user_commands = None

      method amount =
        some
        @@ Amount.serialize
             ( match body with
             | Payment payment ->
                 payment.amount
             | Stake_delegation _ ->
                 Currency.Amount.zero )

      method fee = some @@ Fee.serialize (User_command.fee user_command)

      method first_seen = Option.map first_seen ~f:Block_time.serialize

      method memo =
        some @@ User_command_memo.to_string
        @@ User_command_payload.memo payload

      method nonce =
        some @@ Nonce.serialize @@ User_command_payload.nonce payload

      method public_key =
        some @@ Public_key.encode_as_obj_rel_insert_input
        @@ User_command.sender user_command

      method publicKeyByReceiver =
        some @@ Public_key.encode_as_obj_rel_insert_input
        @@ receiver user_command

      method typ =
        some
        @@ User_command_type.encode
             ( match body with
             | Payment _ ->
                 `Payment
             | Stake_delegation _ ->
                 `Delegation )
    end

  let encode_as_obj_rel_insert_input user_command_with_hash first_seen =
    encode_as_obj_rel_insert_input
      (encode user_command_with_hash first_seen)
      Ast.On_conflict.user_commands

  let decode obj =
    let receiver = (obj#publicKeyByReceiver)#value in
    let sender = (obj#public_key)#value in
    let body =
      let open User_command_payload.Body in
      match obj#typ with
      | `Delegation ->
          Stake_delegation (Set_delegate {new_delegate= receiver})
      | `Payment ->
          Payment {receiver; amount= obj#amount}
    in
    let payload =
      User_command_payload.create ~fee:obj#fee ~nonce:obj#nonce ~memo:obj#memo
        ~body (* TODO: We should actually be passing obj#valid_until *)
        ~valid_until:Coda_numbers.Global_slot.max_value
    in
    ( Coda_base.{User_command.Poly.Stable.V1.payload; sender; signature= ()}
    , obj#first_seen )
end

module Fee_transfer = struct
  let encode {With_hash.data: Fee_transfer.Single.t = (receiver, fee); hash}
      first_seen =
    let open Option in
    object
      method hash = some @@ Transaction_hash.to_base58_check hash

      method fee = some @@ Fee.serialize fee

      method first_seen = Option.map first_seen ~f:Block_time.serialize

      method public_key =
        some @@ Public_key.encode_as_obj_rel_insert_input receiver

      method receiver = None

      method blocks_fee_transfers = None
    end

  let encode_as_obj_rel_insert_input fee_transfer_with_hash first_seen =
    encode_as_obj_rel_insert_input
      (encode fee_transfer_with_hash first_seen)
      Ast.On_conflict.fee_transfers
end

module Snark_job = struct
  let encode ({fee; prover; work_ids; _} : Transaction_snark_work.Info.t) =
    let open Option in
    let job1, job2 =
      match work_ids with
      | `One job1 ->
          (Some job1, None)
      | `Two (job1, job2) ->
          (Some job1, Some job2)
    in
    object
      method blocks_snark_jobs = None

      method fee = some @@ Fee.serialize fee

      method job1 = job1

      method job2 = job2

      method prover = None

      method public_key =
        some @@ Public_key.encode_as_obj_rel_insert_input prover
    end

  let encode_as_obj_rel_insert_input transaction_snark_work =
    encode_as_obj_rel_insert_input
      (encode transaction_snark_work)
      Ast.On_conflict.snark_jobs
end

module Receipt_chain_hash = struct
  type t = {value: Receipt.Chain_hash.t; parent: Receipt.Chain_hash.t}

  let to_obj value parent =
    object
      method blocks_user_commands = None

      method hash = value

      method receipt_chain_hash = None

      method receipt_chain_hashes = parent
    end

  let encode t =
    let open Option in
    let parent =
      to_obj (some @@ Receipt.Chain_hash.to_string @@ t.parent) None
    in
    let value = some @@ Receipt.Chain_hash.to_string @@ t.value in
    let encoded_receipt_chain =
      to_obj value
        ( some
        @@ encode_as_arr_rel_insert_input [parent]
             Ast.On_conflict.receipt_chain_hash )
    in
    encode_as_obj_rel_insert_input encoded_receipt_chain
      Ast.On_conflict.receipt_chain_hash
end

module Blocks_user_commands = struct
  let encode user_command_with_hash first_seen receipt_chain_opt =
    object
      method block = None

      method receipt_chain_hash =
        Option.map receipt_chain_opt ~f:Receipt_chain_hash.encode

      method user_command =
        Some
          (User_command.encode_as_obj_rel_insert_input user_command_with_hash
             first_seen)
    end

  let encode_as_arr_rel_insert_input user_commands =
    encode_as_arr_rel_insert_input
      (List.map user_commands
         ~f:(fun (user_command_with_hash, first_seen, receipt_chain) ->
           encode user_command_with_hash first_seen receipt_chain ))
      Ast.On_conflict.blocks_user_commands
end

module Blocks_fee_transfers = struct
  let encode fee_transfer first_seen =
    object
      method block = None

      method block_id = None

      method fee_transfer =
        Some
          (Fee_transfer.encode_as_obj_rel_insert_input fee_transfer first_seen)

      method fee_transfer_id = None
    end

  let encode_as_arr_rel_insert_input fee_transfers =
    encode_as_arr_rel_insert_input
      (List.map fee_transfers ~f:(fun (fee_transfers_with_hash, first_seen) ->
           encode fee_transfers_with_hash first_seen ))
      Ast.On_conflict.blocks_fee_transfers
end

module Blocks_snark_job = struct
  let encode snark_job =
    let obj =
      object
        method block = None

        method block_id = None

        method snark_job =
          Option.some @@ Snark_job.encode_as_obj_rel_insert_input snark_job

        method snark_job_id = None
      end
    in
    obj

  let encode_as_arr_rel_insert_input snark_jobs =
    encode_as_arr_rel_insert_input
      (List.map snark_jobs ~f:encode)
      Ast.On_conflict.blocks_snark_jobs
end

module State_hashes = struct
  let encode state_hash =
    object
      method block = None

      method blocks = None

      method value = Some (State_hash.to_base58_check state_hash)
    end

  let encode_as_obj_rel_insert_input state_hash =
    encode_as_obj_rel_insert_input (encode state_hash)
      Ast.On_conflict.state_hashes
end

module Blocks = struct
  let serialize
      (With_hash.{hash; data= external_transition} :
        (External_transition.t, State_hash.t) With_hash.t)
      (user_commands :
        ( (Coda_base.User_command.t, Transaction_hash.t) With_hash.t
        * Coda_base.Block_time.t option
        * Receipt_chain_hash.t option )
        list)
      (fee_transfers :
        ( (Coda_base.Fee_transfer.Single.t, Transaction_hash.t) With_hash.t
        * Coda_base.Block_time.t option )
        list) =
    let blockchain_state =
      External_transition.blockchain_state external_transition
    in
    let consensus_state =
      External_transition.consensus_state external_transition
    in
    let global_slot =
      Consensus.Data.Consensus_state.global_slot consensus_state
    in
    let staged_ledger_diff =
      External_transition.staged_ledger_diff external_transition
    in
    let snark_jobs =
      List.map
        (Staged_ledger_diff.completed_works staged_ledger_diff)
        ~f:Transaction_snark_work.info
    in
    let open Option in
    object
      method stateHashByStateHash =
        some @@ State_hashes.encode_as_obj_rel_insert_input hash

      method public_key =
        some @@ Public_key.encode_as_obj_rel_insert_input
        @@ External_transition.proposer external_transition

      method stateHashByParentHash =
        some @@ State_hashes.encode_as_obj_rel_insert_input
        @@ External_transition.parent_hash external_transition

      method snarked_ledger_hash =
        some @@ Ledger_hash.to_string @@ Frozen_ledger_hash.to_ledger_hash
        @@ Blockchain_state.snarked_ledger_hash blockchain_state

      method ledger_hash =
        some @@ Ledger_hash.to_string @@ Staged_ledger_hash.ledger_hash
        @@ Blockchain_state.staged_ledger_hash blockchain_state

      method global_slot = some @@ Unsigned.UInt32.to_int global_slot

      (* TODO: Need to implement *)
      method ledger_proof_nonce = some 0

      (* When a new block is added, their status would be pending and its block
         confirmation number is 0 *)
      method status = some 0

      method block_length =
        some @@ Length.serialize
        @@ Consensus.Data.Consensus_state.blockchain_length consensus_state

      method block_time =
        some @@ Block_time.serialize
        @@ External_transition.timestamp external_transition

      method blocks_fee_transfers =
        some
        @@ Blocks_fee_transfers.encode_as_arr_rel_insert_input fee_transfers

      method blocks_snark_jobs =
        some @@ Blocks_snark_job.encode_as_arr_rel_insert_input snark_jobs

      method blocks_user_commands =
        some
        @@ Blocks_user_commands.encode_as_arr_rel_insert_input user_commands
    end
end
