open Core_kernel
open Nanobit_base
open Util
open Snark_params
open Tick
open Let_syntax

include Blockchain_state

let bound_divisor = `Two_to_the 11
let delta_minus_one_max_bits = 7
let target_time_ms = `Two_to_the 13 (* 8.192 seconds *)

let compute_target timestamp (previous_target : Target.t) time =
  let target_time_ms =
    let `Two_to_the k = target_time_ms in
    Bignum.Bigint.(pow (of_int 2) (of_int k))
  in
  let target_max = Target.(to_bigint max) in
  let delta_minus_one_max =
    Bignum.Bigint.(pow (of_int 2) (of_int delta_minus_one_max_bits) - one)
  in
  let div_pow_2 x (`Two_to_the k) = Bignum.Bigint.shift_right x k in
  let previous_target = Target.to_bigint previous_target in
  assert Block_time.(time > timestamp);
  let rate_multiplier = div_pow_2 Bignum.Bigint.(target_max - previous_target) bound_divisor in
  let delta =
    Bignum.Bigint.(
      of_int64 Block_time.(Span.to_ms (diff time timestamp)) / target_time_ms)
  in
  let open Bignum.Bigint in
  Target.of_bigint begin
    if delta = zero
    then begin
      if previous_target < rate_multiplier
      then one
      else previous_target - rate_multiplier
    end else begin
      let gamma =
        min (delta - one) delta_minus_one_max
      in
      previous_target + rate_multiplier * gamma
    end
  end
;;

let negative_one =
  let next_difficulty : Target.Unpacked.value =
    if Insecure.initial_difficulty
    then Target.max
    else
      Target.of_bigint
        Bignum.Bigint.(
          Target.(to_bigint max) / pow (of_int 2) (of_int 4))
  in
  let timestamp =
    Block_time.of_time
      (Time.sub (Block_time.to_time Block.genesis.header.time) (Time.Span.second))
  in
  { next_difficulty
  ; previous_state_hash = State_hash.of_hash Pedersen.zero_hash
  ; ledger_hash = Ledger.merkle_root Genesis_ledger.ledger
  ; strength = Strength.zero
  ; timestamp
  }

let update_unchecked : value -> Block.t -> value =
  fun state block ->
    let next_difficulty =
      compute_target state.timestamp state.next_difficulty
        block.header.time
    in
    { next_difficulty
    ; previous_state_hash = hash state
    ; ledger_hash = block.body.target_hash
    ; strength = Strength.increase state.strength ~by:state.next_difficulty
    ; timestamp = block.header.time
    }

let zero = update_unchecked negative_one Block.genesis
let zero_hash = hash zero

let bit_length =
  let add bit_length acc _field = acc + bit_length in
  Fields_of_t_.fold zero ~init:0
    ~next_difficulty:(add Target.bit_length)
    ~previous_state_hash:(add State_hash.bit_length)
    ~ledger_hash:(add Ledger_hash.bit_length)
    ~strength:(add Strength.bit_length)
    ~timestamp:(fun acc _ _ -> acc + Block_time.bit_length)

module Make_update (T : Transaction_snark.S) = struct
  let update_exn state (block : Block.t) =
    assert (
      Ledger_hash.equal state.ledger_hash block.body.target_hash
      ||
      T.verify
        (Transaction_snark.create
          ~source:state.ledger_hash
          ~target:block.body.target_hash
          ~proof_type:Merge
          ~fee_excess:Currency.Amount.Signed.zero
          ~proof:block.body.proof));
    let next_state = update_unchecked state block in
    let proof_of_work =
      Proof_of_work.create next_state block.header.nonce
    in
    assert (Proof_of_work.meets_target_unchecked proof_of_work state.next_difficulty);
    next_state

  module Checked = struct
    let compute_target prev_time prev_target time =
      let div_pow_2 bits (`Two_to_the k) = List.drop bits k in
      let delta_minus_one_max_bits = 7 in
      with_label __LOC__ begin
        let prev_target_n = Number.of_bits (Target.Unpacked.var_to_bits prev_target) in
        let%bind rate_multiplier =
          with_label __LOC__ begin
            let%map distance_to_max_target =
              let open Number in
              to_bits (constant (Target.max :> Field.t) - prev_target_n)
            in
            Number.of_bits (div_pow_2 distance_to_max_target bound_divisor)
          end
        in
        let%bind delta =
          with_label __LOC__ begin
            (* This also checks that time >= prev_time *)
            let%map d = Block_time.diff_checked time prev_time in
            div_pow_2 (Block_time.Span.Unpacked.var_to_bits d) target_time_ms
          end
        in
        let%bind delta_is_zero, delta_minus_one =
          with_label __LOC__ begin
            (* There used to be a trickier version of this code that did this in 2 fewer constraints,
              might be worth going back to if there is ever a reason. *)
            let n = List.length delta in
            let d = Checked.pack delta in
            let%bind delta_is_zero = Checked.equal d (Cvar.constant Field.zero) in
            let%map delta_minus_one =
              Checked.if_ delta_is_zero
                ~then_:(Cvar.constant Field.zero)
                ~else_:Cvar.(Infix.(d - constant Field.one))
              (* We convert to bits here because we will in [clamp_to_n_bits] anyway, and
                this gives us the upper bound we need for multiplying with [rate_multiplier]. *)
              >>= Checked.unpack ~length:n
              >>| Number.of_bits
            in
            delta_is_zero, delta_minus_one
          end
        in
        let%bind nonzero_case =
          with_label __LOC__ begin
            let open Number in
            let%bind gamma = clamp_to_n_bits delta_minus_one delta_minus_one_max_bits in
            let%map rg = rate_multiplier * gamma in
            prev_target_n + rg
          end
        in
        let%bind zero_case =
          (* This could be more efficient *)
          with_label __LOC__ begin
            let%bind less = Number.(prev_target_n < rate_multiplier) in
            Checked.if_ less
              ~then_:(Cvar.constant Field.one)
              ~else_:(Cvar.sub (Number.to_var prev_target_n) (Number.to_var rate_multiplier))
          end
        in
        let%bind res =
          with_label __LOC__ begin
            Checked.if_ delta_is_zero
              ~then_:zero_case
              ~else_:(Number.to_var nonzero_case)
          end
        in
        Target.var_to_unpacked res
      end
    ;;

    let meets_target (target : Target.Packed.var) (hash : Pedersen.Digest.Packed.var) =
      if Insecure.check_target
      then return Boolean.true_
      else Target.passes target hash

    module Prover_state = struct
      type t =
        { transaction_snark : Tock.Proof.t
        }
      [@@deriving fields]
    end

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
    hash(new_hash||nonce) < target(old.next_difficulty)
    *)
    let update ((previous_state_hash, previous_state) : State_hash.var * var) (block : Block.var)
      : (State_hash.var * var * [ `Success of Boolean.var ], _) Tick.Checked.t
      =
      with_label __LOC__ begin
        let%bind good_body =
          let%bind correct_transaction_snark =
            T.verify_complete_merge
              previous_state.ledger_hash block.body.target_hash
              (As_prover.return block.body.proof)
          and ledger_hash_didn't_change =
            Ledger_hash.equal_var previous_state.ledger_hash block.body.target_hash
          in
          Boolean.(correct_transaction_snark || ledger_hash_didn't_change)
        in
        let difficulty = previous_state.next_difficulty in
        let difficulty_packed = Target.pack_var difficulty in
        let time = block.header.time in
        let%bind new_difficulty = compute_target previous_state.timestamp difficulty time in
        let%bind new_strength =
          Strength.increase_checked (Strength.pack_var previous_state.strength)
            ~by:(difficulty_packed, difficulty)
          >>= Strength.unpack_var
        in
        let new_state =
          { next_difficulty = new_difficulty
          ; previous_state_hash
          ; ledger_hash = block.body.target_hash
          ; strength = new_strength
          ; timestamp = time
          }
        in
        let%bind state_bits = to_bits new_state in
        let%bind state_hash =
          Pedersen_hash.hash state_bits
            ~params:Pedersen.params
            ~init:(0, Tick.Hash_curve.Checked.identity)
        in
        let%bind pow_hash =
          Pedersen_hash.hash (Block.Nonce.Unpacked.var_to_bits block.header.nonce)
            ~params:Pedersen.params
            ~init:(List.length state_bits, state_hash)
          >>| Pedersen_hash.digest
        in
        let%bind meets_target = meets_target difficulty_packed pow_hash in
        let%map success = Boolean.(good_body && meets_target) in
        (State_hash.var_of_hash_packed (Pedersen_hash.digest state_hash), new_state, `Success success)
      end
    ;;
  end
end

module Checked = struct
  let is_base_hash h =
    with_label __LOC__
      (Checked.equal (Cvar.constant (zero_hash :> Field.t))
         (State_hash.var_to_hash_packed h))

  let hash (t : var) =
    with_label __LOC__
      (to_bits t
       >>= digest_bits ~init:Hash_prefix.blockchain_state
       >>| State_hash.var_of_hash_packed)
end
