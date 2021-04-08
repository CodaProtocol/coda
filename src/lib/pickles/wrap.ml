module SC = Scalar_challenge
module P = Proof
open Pickles_types
open Hlist
open Tuple_lib
open Common
open Core
open Import
open Types
open Backend

(* This contains the "wrap" prover *)

let vector_of_list (type a t)
    (module V : Snarky_intf.Vector.S with type elt = a and type t = t)
    (xs : a list) : t =
  let r = V.create () in
  List.iter xs ~f:(V.emplace_back r) ;
  r

let b_poly = Tick.Field.(Dlog_main.b_poly ~add ~mul ~one)

let combined_inner_product (type actual_num_input_proofs)
    ~actual_num_input_proofs:(module AB : Nat.Add.Intf
      with type n = actual_num_input_proofs) (e1, e2)
    ~(old_bulletproof_challenges : (_, actual_num_input_proofs) Vector.t) ~r
    ~xi ~zeta ~zetaw ~x_hat:(x_hat_1, x_hat_2)
    ~(step_branch_domains : Domains.t) =
  let T = AB.eq in
  let b_polys =
    Vector.map
      ~f:(fun chals -> unstage (b_poly (Vector.to_array chals)))
      old_bulletproof_challenges
  in
  let pi = AB.add Nat.N8.n in
  let combine (x_hat : Tick.Field.t) pt e =
    let a, b = Dlog_plonk_types.Evals.(to_vectors (e : _ array t)) in
    let v : (Tick.Field.t array, _) Vector.t =
      Vector.append
        (Vector.map b_polys ~f:(fun f -> [|f pt|]))
        ([|x_hat|] :: a) (snd pi)
    in
    let open Tick.Field in
    Pcs_batch.combine_split_evaluations
      (Common.dlog_pcs_batch (AB.add Nat.N8.n)
         ~max_quot_size:
           (Common.max_quot_size_int (Domain.size step_branch_domains.h)))
      ~xi ~init:Fn.id ~mul
      ~mul_and_add:(fun ~acc ~xi fx -> fx + (xi * acc))
      ~last:Array.last ~evaluation_point:pt
      ~shifted_pow:(fun deg x ->
        Pcs_batch.pow ~one ~mul x
          Int.(Max_degree.step - (deg mod Max_degree.step)) )
      v b
  in
  let open Tick.Field in
  combine x_hat_1 zeta e1 + (r * combine x_hat_2 zetaw e2)

module Pairing_acc = Tock.Inner_curve.Affine

(* The prover for wrapping a proof *)
let wrap
    (type actual_num_input_proofs max_num_input_proofs
    prev_max_num_input_proofss)
    ~(max_num_input_proofs : max_num_input_proofs Nat.t)
    (module Prev_max_num_input_proofs : Hlist.Maxes.S
      with type ns = prev_max_num_input_proofss
       and type length = max_num_input_proofs)
    (( module
      Req ) :
      (max_num_input_proofs, prev_max_num_input_proofss) Requests.Wrap.t)
    ~dlog_plonk_index wrap_main to_field_elements ~pairing_vk ~step_domains
    ~wrap_domains ~pairing_plonk_indices pk
    ({statement= prev_statement; prev_evals; proof; index= which_index} :
      ( _
      , _
      , (_, actual_num_input_proofs) Vector.t
      , (_, actual_num_input_proofs) Vector.t
      , prev_max_num_input_proofss H1.T(P.Base.Me_only.Dlog_based).t
      , ( (Tock.Field.t array Dlog_plonk_types.Evals.t * Tock.Field.t) Double.t
        , max_num_input_proofs )
        Vector.t )
      P.Base.Pairing_based.t) =
  (*
  let pairing_marlin_index =
    (Vector.to_array pairing_marlin_indices).(Index.to_int which_index)
  in
*)
  let prev_me_only =
    let module M =
      H1.Map (P.Base.Me_only.Dlog_based) (P.Base.Me_only.Dlog_based.Prepared)
        (struct
          let f = P.Base.Me_only.Dlog_based.prepare
        end)
    in
    M.f prev_statement.pass_through
  in
  let prev_statement_with_hashes : _ Types.Pairing_based.Statement.t =
    { proof_state=
        { prev_statement.proof_state with
          me_only=
            (* TODO: Careful here... the length of
               old_buletproof_challenges inside the me_only
               might not be correct *)
            Common.hash_pairing_me_only ~app_state:to_field_elements
              (P.Base.Me_only.Pairing_based.prepare ~dlog_plonk_index
                 prev_statement.proof_state.me_only) }
    ; pass_through=
        (let module M =
           H1.Map
             (P.Base.Me_only.Dlog_based.Prepared)
             (E01 (Digest.Constant))
             (struct
               let f (type n) (m : n P.Base.Me_only.Dlog_based.Prepared.t) =
                 let T =
                   Nat.eq_exn max_num_input_proofs
                     (Vector.length m.old_bulletproof_challenges)
                 in
                 Common.hash_dlog_me_only max_num_input_proofs m
             end)
         in
        let module V = H1.To_vector (Digest.Constant) in
        V.f Prev_max_num_input_proofs.length (M.f prev_me_only)) }
  in
  let handler (Snarky_backendless.Request.With {request; respond}) =
    let open Req in
    let k x = respond (Provide x) in
    match request with
    | Evals ->
        k prev_evals
    | Step_accs ->
        let module M =
          H1.Map
            (P.Base.Me_only.Dlog_based.Prepared)
            (E01 (Pairing_acc))
            (struct
              let f : type a.
                  a P.Base.Me_only.Dlog_based.Prepared.t -> Pairing_acc.t =
               fun t -> t.sg
            end)
        in
        let module V = H1.To_vector (Pairing_acc) in
        k (V.f Prev_max_num_input_proofs.length (M.f prev_me_only))
    | Old_bulletproof_challenges ->
        let module M =
          H1.Map
            (P.Base.Me_only.Dlog_based.Prepared)
            (Challenges_vector.Constant)
            (struct
              let f (t : _ P.Base.Me_only.Dlog_based.Prepared.t) =
                t.old_bulletproof_challenges
            end)
        in
        k (M.f prev_me_only)
    | Messages ->
        k proof.messages
    | Openings_proof ->
        k proof.openings.proof
    | Proof_state ->
        k prev_statement_with_hashes.proof_state
    | _ ->
        Snarky_backendless.Request.unhandled
  in
  let module O = Tick.Oracles in
  let public_input =
    tick_public_input_of_statement ~max_num_input_proofs
      prev_statement_with_hashes
  in
  let prev_challenges =
    Vector.map ~f:Ipa.Step.compute_challenges
      prev_statement.proof_state.me_only.old_bulletproof_challenges
  in
  let actual_num_input_proofs = Vector.length prev_challenges in
  let lte =
    Nat.lte_exn actual_num_input_proofs
      (Length.to_nat Prev_max_num_input_proofs.length)
  in
  let o =
    let sgs =
      let module M =
        H1.Map
          (P.Base.Me_only.Dlog_based.Prepared)
          (E01 (Tick.Curve.Affine))
          (struct
            let f : type n. n P.Base.Me_only.Dlog_based.Prepared.t -> _ =
             fun t -> t.sg
          end)
      in
      let module V = H1.To_vector (Tick.Curve.Affine) in
      V.f Prev_max_num_input_proofs.length (M.f prev_me_only)
    in
    O.create pairing_vk
      Vector.(
        map2 (Vector.trim sgs lte) prev_challenges ~f:(fun commitment cs ->
            { Tick.Proof.Challenge_polynomial.commitment
            ; challenges= Vector.to_array cs } )
        |> to_list)
      public_input proof
  in
  let x_hat = O.(p_eval_1 o, p_eval_2 o) in
  let next_statement : _ Types.Dlog_based.Statement.In_circuit.t =
    let scalar_chal f =
      Scalar_challenge.map ~f:Challenge.Constant.of_tick_field (f o)
    in
    let sponge_digest_before_evaluations = O.digest_before_evaluations o in
    let plonk0 =
      { Types.Dlog_based.Proof_state.Deferred_values.Plonk.Minimal.alpha=
          scalar_chal O.alpha
      ; beta= O.beta o
      ; gamma= O.gamma o
      ; zeta= scalar_chal O.zeta }
    in
    let r = scalar_chal O.u in
    let xi = scalar_chal O.v in
    let to_field =
      SC.to_field_constant
        (module Tick.Field)
        ~endo:Endo.Wrap_inner_curve.scalar
    in
    let module As_field = struct
      let r = to_field r

      let xi = to_field xi

      let zeta = to_field plonk0.zeta

      let alpha = to_field plonk0.alpha
    end in
    let domain =
      Domain.Pow_2_roots_of_unity pairing_vk.domain.log_size_of_group
    in
    let w = pairing_vk.domain.group_gen in
    (* Debug *)
    [%test_eq: Tick.Field.t] w
      (Tick.Field.domain_generator ~log2_size:(Domain.log2_size domain)) ;
    let zetaw = Tick.Field.mul As_field.zeta w in
    let combined_inner_product =
      let open As_field in
      combined_inner_product (* Note: We do not pad here. *)
        ~actual_num_input_proofs:(Nat.Add.create actual_num_input_proofs)
        proof.openings.evals ~x_hat ~r ~xi ~zeta ~zetaw
        ~step_branch_domains:step_domains
        ~old_bulletproof_challenges:prev_challenges
    in
    let me_only : _ P.Base.Me_only.Dlog_based.t =
      { sg= proof.openings.proof.sg
      ; old_bulletproof_challenges=
          Vector.map prev_statement.proof_state.unfinalized_proofs ~f:(fun t ->
              t.deferred_values.bulletproof_challenges ) }
    in
    let chal = Challenge.Constant.of_tick_field in
    let new_bulletproof_challenges, b =
      let prechals =
        Array.map (O.opening_prechallenges o) ~f:(fun x ->
            let x =
              Scalar_challenge.map ~f:Challenge.Constant.of_tick_field x
            in
            x )
      in
      let chals =
        Array.map prechals ~f:(fun x -> Ipa.Step.compute_challenge x)
      in
      let b_poly = unstage (b_poly chals) in
      let open As_field in
      let b =
        let open Tick.Field in
        b_poly zeta + (r * b_poly zetaw)
      in
      let prechals =
        Array.map prechals ~f:(fun x -> {Bulletproof_challenge.prechallenge= x})
      in
      (prechals, b)
    in
    let plonk, _ =
      Plonk_checks.derive_plonk
        (module Tick.Field)
        ~shift:Shifts.tick ~endo:Endo.Step_inner_curve.base
        ~mds:Tick_field_sponge.params.mds
        ~domain:
          (Plonk_checks.domain
             (module Tick.Field)
             domain ~shifts:Common.tick_shifts
             ~domain_generator:Backend.Tick.Field.domain_generator)
        {plonk0 with zeta= As_field.zeta; alpha= As_field.alpha}
        (Plonk_checks.evals_of_split_evals
           (module Tick.Field)
           proof.openings.evals ~rounds:(Nat.to_int Tick.Rounds.n)
           ~zeta:As_field.zeta ~zetaw)
        (fst x_hat)
    in
    let shift_value =
      Shifted_value.of_field (module Tick.Field) ~shift:Shifts.tick
    in
    { proof_state=
        { deferred_values=
            { xi
            ; b= shift_value b
            ; bulletproof_challenges=
                Vector.of_array_and_length_exn new_bulletproof_challenges
                  Tick.Rounds.n
            ; combined_inner_product= shift_value combined_inner_product
            ; which_rule= which_index
            ; plonk=
                { plonk with
                  zeta= plonk0.zeta
                ; alpha= plonk0.alpha
                ; beta= chal plonk0.beta
                ; gamma= chal plonk0.gamma } }
        ; sponge_digest_before_evaluations=
            Digest.Constant.of_tick_field sponge_digest_before_evaluations
        ; me_only }
    ; pass_through= prev_statement.proof_state.me_only }
  in
  let me_only_prepared =
    P.Base.Me_only.Dlog_based.prepare next_statement.proof_state.me_only
  in
  let%map.Async next_proof =
    let (T (input, conv)) = Impls.Wrap.input () in
    Common.time "wrap proof" (fun () ->
        Impls.Wrap.generate_witness_conv
          ~f:(fun {Impls.Wrap.Proof_inputs.auxiliary_inputs; public_inputs} ->
            Backend.Tock.Proof.create_async ~primary:public_inputs
              ~auxiliary:auxiliary_inputs pk
              ~message:
                ( Vector.map2
                    (Vector.extend_exn prev_statement.proof_state.me_only.sg
                       max_num_input_proofs
                       (Lazy.force Dummy.Ipa.Wrap.sg))
                    me_only_prepared.old_bulletproof_challenges
                    ~f:(fun sg chals ->
                      { Tock.Proof.Challenge_polynomial.commitment= sg
                      ; challenges= Vector.to_array chals } )
                |> Vector.to_list ) )
          [input]
          (fun x () ->
            ( Impls.Wrap.handle (fun () -> (wrap_main (conv x) : unit)) handler
              : unit ) )
          ()
          { pass_through= prev_statement_with_hashes.proof_state.me_only
          ; proof_state=
              { next_statement.proof_state with
                me_only=
                  Common.hash_dlog_me_only max_num_input_proofs
                    me_only_prepared } } )
  in
  ( { proof= next_proof
    ; statement= Types.Dlog_based.Statement.to_minimal next_statement
    ; prev_evals= proof.openings.evals
    ; prev_x_hat= x_hat }
    : _ P.Base.Dlog_based.t )
