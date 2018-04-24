open Core
open Nanobit_base
open Snark_params
open Snarky
open Currency

let bundle_length = 1

let tick_input () = Tick.(Data_spec.([ Field.typ ]))
let tick_input_size = Tick.Data_spec.size (tick_input ())
let wrap_input () = Tock.(Data_spec.([ Field.typ ]))

let provide_witness' typ ~f =
  Tick.(provide_witness typ As_prover.(map get_state ~f))

module Proof_type = struct
  type t = Base | Merge
  [@@deriving bin_io]

  let is_base = function
    | Base -> true
    | Merge -> false
end

type t =
  { source     : Ledger_hash.Stable.V1.t
  ; target     : Ledger_hash.Stable.V1.t
  ; proof_type : Proof_type.t
  ; proof      : Proof.Stable.V1.t
  }
[@@deriving fields, bin_io]

let create = Fields.create

module Keys0 = struct
  module Binable_of_bigstringable
      (M : sig
         type t
         val to_bigstring : t -> Bigstring.t
         val of_bigstring : Bigstring.t -> t
       end)
    = struct
      type t = M.t
      include Binable.Of_binable(Bigstring)(struct
        type t = M.t
        let to_binable = M.to_bigstring
        let of_binable = M.of_bigstring
      end)
    end

  module Tick_vk = Binable_of_bigstringable(Tick_curve.Verification_key)
  module Tick_pk = Binable_of_bigstringable(Tick_curve.Proving_key)
  module Tock_vk = Binable_of_bigstringable(Tock_curve.Verification_key)
  module Tock_pk = Binable_of_bigstringable(Tock_curve.Proving_key)

  type t =
    { base_vk  : Tick_vk.t
    ; base_pk  : Tick_pk.t
    ; wrap_vk  : Tock_vk.t
    ; wrap_pk  : Tock_pk.t
    ; merge_vk : Tick_vk.t
    ; merge_pk : Tick_pk.t
    }
  [@@deriving bin_io]

  let dummy () =
    let tick_keypair =
      let open Tick in
      generate_keypair ~exposing:(tick_input ()) (fun x -> assert_equal x x)
    in
    let tock_keypair =
      let open Tock in
      generate_keypair ~exposing:(wrap_input ()) (fun x -> assert_equal x x)
    in
    { base_vk  = Tick.Keypair.vk tick_keypair
    ; base_pk  = Tick.Keypair.pk tick_keypair
    ; wrap_vk  = Tock.Keypair.vk tock_keypair
    ; wrap_pk  = Tock.Keypair.pk tock_keypair
    ; merge_vk = Tick.Keypair.vk tick_keypair
    ; merge_pk = Tick.Keypair.pk tick_keypair
    }
end

let handle_with_ledger (ledger : Ledger.t) =
  let open Tick in
  let path_at_index idx =
    List.map ~f:Ledger.Path.elem_hash (Ledger.merkle_path_at_index_exn ledger idx)
  in
  fun (With { request; respond }) ->
    let open Ledger_hash in
    match request with
    | Get_element idx ->
      let elt = Ledger.get_at_index_exn ledger idx in
      let path = path_at_index idx in
      respond (Provide (elt, path))
    | Get_path idx ->
      let path = path_at_index idx in
      respond (Provide path)
    | Set (idx, account) ->
      Ledger.update_at_index_exn ledger idx account;
      respond (Provide ())
    | Find_index pk ->
      let index = Ledger.index_of_key_exn ledger pk in
      respond (Provide index)
    | _ -> unhandled

(* Staging:
   first make tick base.
   then make tick merge (which top_hashes in the tock wrap vk)
   then make tock wrap (which branches on the tick vk) *)

module Base = struct
  open Tick
  open Let_syntax

  (* spec for [apply_transaction root { sender; signature; payload }]:
     - check that [signature] is a signature by [sender] of payload
     - return the merkle tree [root'] where the sender balance is decremented by
     [payload.amount] and the receiver balance is incremented by [payload.amount].
  *)
  let apply_transaction root ({ sender; signature; payload } : Transaction.var) =
    (if not Insecure.transaction_replay
     then failwith "Insecure.transaction_replay false");
    let { Transaction.Payload.receiver; amount; fee } = payload in
    let%bind () =
      let%bind bs = Transaction.Payload.var_to_bits payload in
      Schnorr.Checked.assert_verifies signature sender bs
    in
    let%bind root =
      let%bind sender_compressed = Public_key.compress_var sender in
      Ledger_hash.modify_account root sender_compressed ~f:(fun account ->
        let%map balance = Balance.Checked.(account.balance - amount) in (* TODO: Fee *)
        { account with balance })
    in
    Ledger_hash.modify_account root receiver ~f:(fun account ->
      let%map balance = Balance.Checked.(account.balance + amount) in
      { account with balance })

(* Someday:
   write the following soundness tests:
   - apply a transaction where the signature is incorrect
   - apply a transaction where the sender does not have enough money in their account
   - apply a transaction and stuff in the wrong target hash
*)

  let apply_transactions root ts =
    Checked.List.fold ~init:root ~f:apply_transaction ts

  module Prover_state = struct
    type t =
      { transactions : Transaction.t list
      ; state1 : Ledger_hash.t
      ; state2 : Ledger_hash.t
      }
    [@@deriving fields]
  end

(* spec for [main top_hash]:
   constraints pass iff
   there exist l1 : Ledger_hash.t, l2 : Ledger_hash.t, ts : Transaction.t list
   such that
   H(l1, l2) = top_hash,
   applying ts to ledger with merkle hash l1 results in ledger with merkle hash l2. *)
  let main top_hash =
    let%bind l1 = provide_witness' Ledger_hash.typ ~f:Prover_state.state1 in
    let%bind l2 = provide_witness' Ledger_hash.typ ~f:Prover_state.state2 in
    let%bind () =
      let%bind b1 = Ledger_hash.var_to_bits l1
      and b2 = Ledger_hash.var_to_bits l2
      in
      hash_digest (b1 @ b2) >>= assert_equal top_hash
    in
    let%bind ts =
      provide_witness' (Typ.list ~length:bundle_length Transaction.typ)
        ~f:Prover_state.transactions
    in
    apply_transactions l1 ts >>= Ledger_hash.assert_equal l2

  let create_keys () =
    generate_keypair main ~exposing:(tick_input ())

  let top_hash s1 s2 =
    Pedersen.hash_fold Pedersen.params
      (fun ~init ~f ->
         let init = Ledger_hash.fold s1 ~init ~f in
         Ledger_hash.fold s2 ~init ~f)

  let bundle ~proving_key
        state1
        state2
        transaction
        handler
    =
    let prover_state : Prover_state.t =
      { state1; state2; transactions = [ transaction ] }
    in
    let main top_hash = handle (main top_hash) handler in
    let top_hash = top_hash state1 state2 in
    top_hash,
    prove proving_key (tick_input ()) prover_state main top_hash
end

module Merge = struct
  open Tick
  open Let_syntax

  module Prover_state = struct
    type t =
      { tock_vk : Tock_curve.Verification_key.t
      ; input1  : bool list
      ; proof12 : Proof_type.t * Tock_curve.Proof.t
      ; input2  : bool list
      ; proof23 : Proof_type.t * Tock_curve.Proof.t
      ; input3  : bool list
      }
    [@@deriving fields]
  end

  let input = tick_input

  let wrap_input_size = Tock.Data_spec.size (wrap_input ())

  let tock_vk_length = 11324
  let tock_vk_typ = Typ.list ~length:tock_vk_length Boolean.typ

  let wrap_input_typ = Typ.list ~length:Tock.Field.size_in_bits Boolean.typ

  module Verifier =
    Snarky.Verifier_gadget.Make(Tick)(Tick_curve)(Tock_curve)
      (struct let input_size = wrap_input_size end)

  (* spec for [verify_transition tock_vk proof_field s1 s2]:
     returns a bool which is true iff
     there is a snark proving making tock_vk
     accept on one of [ H(s1, s2); H(s1, s2, tock_vk) ] *)
  let verify_transition tock_vk proof_field s1 s2 =
    let open Let_syntax in
    let get_proof s = let (_t, proof) = proof_field s in proof in
    let get_type s = let (t, _proof) = proof_field s in t in
    let%bind states_hash = 
      with_label __LOC__ begin
        Pedersen_hash.hash (s1 @ s2)
          ~params:Pedersen.params
          ~init:(0, Hash_curve.Checked.identity)
      end
    in
    let%bind states_and_vk_hash =
      with_label __LOC__ begin
        Pedersen_hash.hash tock_vk
          ~params:Pedersen.params
          ~init:(2 * Tock.Field.size_in_bits, states_hash)
      end
    in
    let%bind is_base =
      with_label __LOC__ begin
        provide_witness' Boolean.typ ~f:(fun s ->
          Proof_type.is_base (get_type s))
      end
    in
    let%bind input =
      with_label __LOC__ begin
        Checked.if_ is_base
          ~then_:(Pedersen_hash.digest states_hash)
          ~else_:(Pedersen_hash.digest states_and_vk_hash)
        >>= Pedersen.Digest.choose_preimage_var
        >>| Pedersen.Digest.Unpacked.var_to_bits
      end
    in
    with_label __LOC__ begin
      Verifier.All_in_one.create ~verification_key:tock_vk ~input
        As_prover.(map get_state ~f:(fun s ->
          { Verifier.All_in_one.
            verification_key = s.Prover_state.tock_vk
          ; proof            = get_proof s
          }))
      >>| Verifier.All_in_one.result
    end
  ;;

  (* spec for [main top_hash]:
     constraints pass iff
     there exist s1, s3, tock_vk such that
     H(s1, s3, tock_vk) = top_hash,
     verify_transition tock_vk _ s1 s2 is true
     verify_transition tock_vk _ s2 s3 is true
  *)
  let main (top_hash : Pedersen.Digest.Packed.var) =
    let%bind tock_vk =
      provide_witness' tock_vk_typ ~f:(fun { Prover_state.tock_vk } ->
        Verifier.Verification_key.to_bool_list tock_vk)
    and s1 = provide_witness' wrap_input_typ ~f:Prover_state.input1
    and s2 = provide_witness' wrap_input_typ ~f:Prover_state.input2
    and s3 = provide_witness' wrap_input_typ ~f:Prover_state.input3
    in
    let%bind () = hash_digest (s1 @ s3 @ tock_vk) >>= assert_equal top_hash
    and verify_12 = verify_transition tock_vk Prover_state.proof12 s1 s2
    and verify_23 = verify_transition tock_vk Prover_state.proof23 s2 s3
    in
    Boolean.Assert.all [ verify_12; verify_23 ]


  let create_keys () =
    generate_keypair ~exposing:(input ()) main
end

module Wrap (Vk : sig
    val merge : Tick.Verification_key.t
    val base : Tick.Verification_key.t
  end)
= struct
  open Tock

  module Verifier =
    Snarky.Verifier_gadget.Make(Tock)(Tock_curve)(Tick_curve)
      (struct let input_size = tick_input_size end)

  let merge_vk_bits : bool list =
    Verifier.Verification_key.to_bool_list Vk.merge

  let base_vk_bits : bool list =
    Verifier.Verification_key.to_bool_list Vk.base

  let if_ (choice : Boolean.var) ~then_ ~else_ =
    List.map2_exn then_ else_ ~f:(fun t e ->
      match t, e with
      | true, true -> Boolean.true_
      | false, false -> Boolean.false_
      | true, false -> choice
      | false, true -> Boolean.not choice)

  module Prover_state = struct
    type t =
      { proof_type : Proof_type.t
      ; proof      : Tick_curve.Proof.t
      }
    [@@deriving fields]
  end

  let provide_witness' typ ~f = provide_witness typ As_prover.(map get_state ~f)

(* spec for [main input]:
   constraints pass iff
   (b1, b2, .., bn) = unpack input,
   there is a proof making one of [ base_vk; merge_vk ] accept (b1, b2, .., bn) *)
  let main input =
    let open Let_syntax in
    let%bind input =
      Checked.choose_preimage input
        ~length:Tick_curve.Field.size_in_bits
    in
    let%bind is_base =
      provide_witness' Boolean.typ ~f:(fun {Prover_state.proof_type} ->
        Proof_type.is_base proof_type)
    in
    let verification_key = if_ is_base ~then_:base_vk_bits ~else_:merge_vk_bits in
    let%bind v =
      (* someday: Probably an opportunity for optimization here since
          we are passing in one of two known verification keys. *)
      Verifier.All_in_one.create ~verification_key ~input
        As_prover.(map get_state ~f:(fun { Prover_state.proof_type; proof } ->
          let verification_key =
            match proof_type with
            | Base -> Vk.base
            | Merge -> Vk.merge
          in
          { Verifier.All_in_one.verification_key; proof }))
    in
    Boolean.Assert.is_true (Verifier.All_in_one.result v)

  let create_keys () =
    generate_keypair ~exposing:(wrap_input ()) main
end

let embed (x : Tick.Field.t) : Tock.Field.t =
  Tock.Field.project (Tick.Field.unpack x)

module type S = sig
  val verify : t -> bool

  val of_transaction
    : Ledger_hash.t
    -> Ledger_hash.t
    -> Transaction.t
    -> Tick.Handler.t
    -> t

  val merge : t -> t -> t

  val verify_merge
    : Ledger_hash.var
    -> Ledger_hash.var
    -> (Tock.Proof.t, 's) Tick.As_prover.t
    -> (Tick.Boolean.var, 's) Tick.Checked.t
end

let check_transaction source target transaction handler =
  let prover_state : Base.Prover_state.t =
    { state1=source; state2=target; transactions = [ transaction ] }
  in
  let open Tick in
  let top_hash = Base.top_hash source target in
  let main = handle (Base.main (Cvar.constant top_hash)) handler in
  let (_s, (), passed) =
    run_and_check (Checked.map main ~f:As_prover.return) prover_state
  in
  assert passed

module Make (K : sig val keys : Keys0.t end) = struct
  open K

  module Wrap = Wrap(struct
      let merge = keys.merge_vk
      let base = keys.base_vk
    end)

  let wrap proof_type proof input =
    Tock.prove keys.wrap_pk (wrap_input ())
      { Wrap.Prover_state.proof; proof_type }
      Wrap.main
      (embed input)

  let wrap_vk_bits = Merge.Verifier.Verification_key.to_bool_list keys.wrap_vk

  let merge_top_hash s1 s2 =
    Tick.Pedersen.hash_fold Tick.Pedersen.params
      (fun ~init ~f ->
        let init = Ledger_hash.fold ~init ~f s1 in
        let init = Ledger_hash.fold ~init ~f s2 in
        List.fold ~init ~f wrap_vk_bits)

  let merge_proof input1 input2 input3 proof12 proof23 =
    let top_hash = merge_top_hash input1 input3 in
    let to_bits = Ledger_hash.to_bits in
    top_hash,
    Tick.prove keys.merge_pk (tick_input ())
      { Merge.Prover_state.input1 = to_bits input1
      ; input2 = to_bits input2
      ; input3 = to_bits input3
      ; proof12
      ; proof23
      ; tock_vk = keys.wrap_vk
      }
      Merge.main
      top_hash

  let vk_curve_pt =
    let open Tick in
    let s =
      Pedersen.State.create
        ~bits_consumed:(Pedersen.Digest.size_in_bits * 2)
        Pedersen.params
    in
    (Pedersen.State.update_fold s (List.fold wrap_vk_bits)).acc

(* spec for [verify_merge s1 s2 _]:
   Returns a boolean which is true if there exists a tock proof proving
   (against the wrap verification key) H(s1, s2, wrap_vk).
   This in turn should only happen if there exists a tick proof proving
   (against the merge verification key) H(s1, s2, wrap_vk).

   We precompute the part of the pedersen involving wrap_vk outside the SNARK
   since this saves us many constraints.
*)
  let verify_merge s1 s2 get_proof =
    let open Tick in
    let open Let_syntax in
    let%bind s1 = Ledger_hash.var_to_bits s1
    and s2 = Ledger_hash.var_to_bits s2
    in
    let%bind top_hash =
      let (vx, vy) = vk_curve_pt in
      Pedersen_hash.hash ~params:Pedersen.params
        ~init:(0, (Cvar.constant vx, Cvar.constant vy))
        (s1 @ s2)
      >>| Pedersen_hash.digest
      >>= Pedersen.Digest.choose_preimage_var
      >>| Pedersen.Digest.Unpacked.var_to_bits
    in
    Merge.Verifier.All_in_one.create ~input:top_hash
      ~verification_key:(List.map ~f:Boolean.var_of_value wrap_vk_bits)
      (As_prover.map get_proof ~f:(fun proof ->
        { Merge.Verifier.All_in_one.proof; verification_key = keys.wrap_vk }))
    >>| Merge.Verifier.All_in_one.result
  ;;

  let verify { source; target; proof; proof_type } =
    let input =
      match proof_type with
      | Base -> Base.top_hash source target
      | Merge -> merge_top_hash source target
    in
    Tock.verify proof keys.wrap_vk (wrap_input ()) (embed input)

  let of_transaction source target transaction handler =
    let top_hash, proof = Base.bundle ~proving_key:keys.base_pk source target transaction handler in
    let proof_type = Proof_type.Base in
    { source
    ; target
    ; proof_type
    ; proof = wrap proof_type proof top_hash
    }

  let merge t1 t2 =
    (if not (Ledger_hash.(=) t1.target t2.source)
    then
      failwithf
        !"Transaction_snark.merge: t1.target <> t2.source (%{sexp:Ledger_hash.t} vs %{sexp:Ledger_hash.t})"
        t1.target
        t2.source ());
    let input, proof =
      merge_proof t1.source t1.target t2.target
        (t1.proof_type, t1.proof)
        (t2.proof_type, t2.proof)
    in
    let proof_type = Proof_type.Merge in
    { source = t1.source
    ; target = t2.target
    ; proof_type
    ; proof = wrap proof_type proof input
    }
end

module Keys = struct
  include Keys0

  let create () =
    let base = Base.create_keys () in
    let merge = Merge.create_keys () in
    let wrap =
      let module Wrap =
        Wrap(struct
          let base = Tick.Keypair.vk base
          let merge = Tick.Keypair.vk merge
        end)
      in
      Wrap.create_keys ()
    in
    { base_vk = Tick.Keypair.vk base
    ; base_pk = Tick.Keypair.pk base
    ; merge_vk = Tick.Keypair.vk merge
    ; merge_pk = Tick.Keypair.pk merge
    ; wrap_vk = Tock.Keypair.vk wrap
    ; wrap_pk = Tock.Keypair.pk wrap
    }
end

let%test_module "transaction_snark" =
  (module struct
    type wallet = { private_key : Private_key.t ; account : Account.t }

    let random_wallets () =
      let random_wallet () : wallet =
        let private_key = Private_key.create () in
        { private_key
        ; account =
            { public_key = Public_key.compress (Public_key.of_private_key private_key)
            ; balance = Balance.of_int (10 + Random.int 100)
            }
        }
      in
      let n = Int.pow 2 ledger_depth in
      Array.init n ~f:(fun _ -> random_wallet ())
    ;;

    let transaction wallets i j amt =
      let sender = wallets.(i) in
      let receiver = wallets.(j) in
      let payload : Transaction.Payload.t =
        { receiver = receiver.account.public_key
        ; fee = Fee.zero
        ; amount = Amount.of_int amt
        }
      in
      let signature =
        Tick.Schnorr.sign sender.private_key
          (Transaction.Payload.to_bits payload)
      in
      assert (Tick.Schnorr.verify signature (Public_key.of_private_key sender.private_key) (Transaction.Payload.to_bits payload));
      { Transaction.payload
      ; sender = Public_key.of_private_key sender.private_key
      ; signature
      }

    let keys = Keys.create ()

    include Make(struct let keys = keys end)

    let of_transaction' ledger transaction =
      let source = Ledger.merkle_root ledger in
      let target = Ledger.merkle_root_after_transaction_exn ledger transaction in
      of_transaction source target transaction (handle_with_ledger ledger)

    let%test "base_and_merge" =
      Test_util.with_randomness 123456789 (fun () ->
        let wallets = random_wallets () in
        let ledger =
          Ledger.create ()
        in
        Array.iter wallets ~f:(fun { account } ->
          Ledger.update ledger account.public_key account);
        let t1 = transaction wallets 0 1 8 in
        let t2 = transaction wallets 1 2 3 in
        let state1 = Ledger.merkle_root ledger in
        let proof12 = of_transaction' ledger t1 in
        let proof23 = of_transaction' ledger t2 in
        let state3 = Ledger.merkle_root ledger in
        let proof13 = merge proof12 proof23 in
        Tock.verify proof13.proof keys.wrap_vk (wrap_input ())
          (embed (merge_top_hash state1 state3)))
  end)
