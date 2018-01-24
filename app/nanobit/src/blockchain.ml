open Core_kernel
open Async_kernel
open Util
open Snark_params

module State = struct
  open Tick
  open Let_syntax

  module Digest = Pedersen.Digest

  let difficulty_window = 17

  let all_but_last_exn xs = fst (split_last_exn xs)

  (* Someday: It may well be worth using bitcoin's compact nbits for target values since
    targets are quite chunky *)
  type ('time, 'target, 'digest, 'number, 'strength) t_ =
    { difficulty_info : ('time * 'target) list
    ; block_hash      : 'digest
    ; number          : 'number
    ; strength        : 'strength
    }
  [@@deriving bin_io]

  type t = (Block_time.t, Target.t, Digest.t, Block.Body.t, Strength.t) t_
  [@@deriving bin_io]

  type var =
    ( Block_time.Unpacked.var
    , Target.Unpacked.var
    , Digest.Packed.var
    , Block.Body.Packed.var
    , Strength.Packed.var
    ) t_

  type value =
    ( Block_time.Unpacked.value
    , Target.Unpacked.value
    , Digest.Packed.value
    , Block.Body.Packed.value
    , Strength.Packed.value
    ) t_

  let to_hlist { difficulty_info; block_hash; number; strength } = H_list.([ difficulty_info; block_hash; number; strength ])
  let of_hlist = H_list.(fun [ difficulty_info; block_hash; number; strength ] -> { difficulty_info; block_hash; number; strength })

  let data_spec =
    let open Data_spec in
    [ Var_spec.(
        list ~length:difficulty_window
          (tuple2 Block_time.Unpacked.spec Target.Unpacked.spec))
    ; Digest.Packed.spec
    ; Block.Body.Packed.spec
    ; Strength.Packed.spec
    ]

  let spec : (var, value) Var_spec.t =
    Var_spec.of_hlistable data_spec
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
      ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

  let compute_target_unchecked _ : Target.t = 
    Target.of_field Field.(negate one)

  let compute_target = compute_target_unchecked

  let update_exn (state : value) (block : Block.t) =
    let target = compute_target_unchecked state.difficulty_info in
    let block_hash = Block.hash block in
    assert (Target.meets_target target ~hash:block_hash);
    let strength = Target.strength_unchecked target in
    assert Int64.(block.body > state.number);
    { difficulty_info =
        (block.header.time, target)
        :: all_but_last_exn state.difficulty_info
    ; block_hash
    ; number = block.body
    ; strength = Field.add strength state.strength
    }

  let negative_one : value =
    let time = Block_time.of_time Core.Time.epoch in
    let target : Target.Unpacked.value =
      Target.(unpack (of_field (Field.of_int (-1))))
    in
    { difficulty_info =
        List.init difficulty_window ~f:(fun _ -> (time, target))
    ; block_hash = Block.(hash genesis)
    ; number = Int64.of_int (-1)
    ; strength = Strength.zero
    }

  let zero = update_exn negative_one Block.genesis 

  let to_bits ({ difficulty_info; block_hash; number; strength } : var) =
    let%map h = Digest.Checked.(unpack block_hash >>| to_bits)
    and n = Block.Body.Checked.(unpack number >>| to_bits)
    and s = Strength.Checked.(unpack strength >>| to_bits)
    in
    List.concat_map difficulty_info ~f:(fun (x, y) ->
      Block_time.Checked.to_bits x @ Target.Checked.to_bits y)
    @ h
    @ n
    @ s

  let to_bits_unchecked ({ difficulty_info; block_hash; number; strength } : value) =
    let h = Digest.(Unpacked.to_bits (unpack block_hash)) in
    let n = Block.Body.(Unpacked.to_bits (unpack number)) in
    let s = Strength.(Unpacked.to_bits (unpack strength)) in
    List.concat_map difficulty_info ~f:(fun (x, y) ->
      Block_time.Bits.to_bits x @ Target.Unpacked.to_bits y)
    @ h
    @ n
    @ s

  let hash t =
    let s = Pedersen.State.create Pedersen.params in
    Pedersen.State.update_fold s
      (List.fold_left (to_bits_unchecked t))
    |> Pedersen.State.digest

  let zero_hash = hash zero

  module Checked = struct
    let is_base_hash h = Checked.equal (Cvar.constant zero_hash) h

    let hash (t : var) = to_bits t >>= hash_digest

    (* TODO: A subsequent PR will replace this with the actual difficulty calculation *)
    let compute_target _ = return (Cvar.constant Field.(negate one))

    let meets_target (target : Target.Packed.var) (hash : Digest.Packed.var) =
      with_label "meets_target" begin
        let%map { less } =
          Util.compare ~bit_length:Field.size_in_bits hash (target :> Cvar.t)
        in
        less
      end

    let valid_body ~prev body =
      let%bind { less } = Util.compare ~bit_length:Block.Body.bit_length prev body in
      Boolean.Assert.is_true less
    ;;

    let update (state : var) (block : Block.Packed.var) =
      with_label "Blockchain.State.update" begin
        let%bind () =
          assert_equal ~label:"previous_block_hash"
            block.header.previous_block_hash state.block_hash
        in
        let%bind () = valid_body ~prev:state.number block.body in
        let%bind target = compute_target state.difficulty_info in
        let%bind strength = Target.strength target in
        let%bind block_unpacked = Block.Checked.unpack block in
        let%bind block_hash =
          let bits = Block.Checked.to_bits block_unpacked in
          hash_digest bits
        in
        let%bind meets_target = meets_target target block_hash in
        let%map target_unpacked = Target.Checked.unpack target in
        ( { difficulty_info =
              (block_unpacked.header.time, target_unpacked)
              :: all_but_last_exn state.difficulty_info
          ; block_hash
          ; number = block.body
          ; strength = Cvar.Infix.(strength + state.strength)
          }
        , `Success meets_target
        )
      end
  end
end

type t =
  { state : State.t
  ; proof : Proof.t
  }
[@@deriving bin_io]

module Update = struct
  type nonrec t =
    | New_chain of t
end

let valid t = failwith "TODO"

let accumulate ~init ~updates ~strongest_chain =
  don't_wait_for begin
    let%map _last_block =
      Linear_pipe.fold updates ~init ~f:(fun chain (Update.New_chain new_chain) ->
        if not (valid new_chain)
        then return chain
        else if Strength.(new_chain.state.strength > chain.state.strength)
        then 
          let%map () = Pipe.write strongest_chain new_chain in
          new_chain
        else
          return chain)
    in
    ()
  end

module Digest = Tick.Pedersen.Digest

module System = struct
  module State = State
  module Update = Block.Packed
end

module Transition =
  Transition_system.Make
    (struct
      module Tick = Digest
      module Tock = Bits.Snarkable.Field(Tock)
    end)
    (struct let hash = Tick.hash_digest end)
    (System)

let base_hash =
  Transition.instance_hash System.State.zero

module Step = Transition.Step
module Wrap = Transition.Wrap

let base_proof =
  let dummy_proof =
    let open Tock in
    let input = Data_spec.[] in
    let main =
      let one = Cvar.constant Field.one in
      assert_equal one one
    in
    let keypair = generate_keypair input main in
    prove (Keypair.pk keypair) input () main
  in
  Tick.prove Step.proving_key (Step.input ())
    { Step.Prover_state.prev_proof = dummy_proof
    ; wrap_vk  = Wrap.verification_key
    ; prev_state = System.State.negative_one
    ; update = Block.genesis
    }
    Step.main
    base_hash

let genesis = { state = State.zero; proof = base_proof }

let () =
  assert
    (Tick.verify base_proof Step.verification_key
       (Step.input ()) base_hash)
;;

let extend_exn { state=prev_state; proof=prev_proof } block =
  let proof = Transition.step ~prev_proof ~prev_state block in
  { proof; state = State.update_exn prev_state block }
;;

