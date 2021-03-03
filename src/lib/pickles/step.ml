module SC = Scalar_challenge
open Core
module P = Proof
open Pickles_types
open Poly_types
open Higher_kinded_poly
open Hlist
open Backend
open Tuple_lib
open Import
open Types
open Common

module Mk_double (M : T0) = struct
  type t = M.t Double.t
end

let b_poly = Tock.Field.(Dlog_main.b_poly ~add ~mul ~one)

let step_one : type var value max max_num_parents m.
       max Nat.t
    -> Impls.Wrap.Verification_key.t
    -> 'a
    -> (value, max_num_parents, m, P3.W(P.With_data).t) P3.t
    -> (var, value, max_num_parents, m) Tag.t
    -> must_verify:bool
    -> [`Sg of Tock.Curve.Affine.t]
       * Unfinalized.Constant.t
       * [`Me_only of Digest.Constant.t]
       * [`X_hat of Tock.Field.t Double.t]
       * (value, max_num_parents, m, P3.W(Per_proof_witness.Constant).t) P3.t =
 fun max dlog_vk dlog_index t tag ~must_verify ->
  let (T t) =
    let module M = P3.T (P.With_data) in
    M.of_poly t
  in
  let plonk0 = t.statement.proof_state.deferred_values.plonk in
  let plonk =
    let domain =
      (Vector.to_array (Types_map.lookup_step_domains tag)).(Index.to_int
                                                               t.statement
                                                                 .proof_state
                                                                 .deferred_values
                                                                 .which_rule)
    in
    let to_field =
      SC.to_field_constant
        (module Tick.Field)
        ~endo:Endo.Wrap_inner_curve.scalar
    in
    let alpha = to_field plonk0.alpha in
    let zeta = to_field plonk0.zeta in
    let zetaw =
      Tick.Field.(zeta * domain_generator ~log2_size:(Domain.log2_size domain))
    in
    time "plonk_checks" (fun () ->
        Plonk_checks.derive_plonk
          (module Tick.Field)
          ~endo:Endo.Step_inner_curve.base ~shift:Shifts.tick
          ~mds:Tick_field_sponge.params.mds
          ~domain:
            (Plonk_checks.domain
               (module Tick.Field)
               domain ~shifts:Common.tick_shifts
               ~domain_generator:Backend.Tick.Field.domain_generator)
          { zeta
          ; alpha
          ; beta= Challenge.Constant.to_tick_field plonk0.beta
          ; gamma= Challenge.Constant.to_tick_field plonk0.gamma }
          (Plonk_checks.evals_of_split_evals
             (module Tick.Field)
             t.prev_evals ~rounds:(Nat.to_int Tick.Rounds.n) ~zeta ~zetaw)
          (fst t.prev_x_hat)
        |> fst )
  in
  let data = Types_map.lookup_basic tag in
  let (module Max_num_parents) = data.max_num_parents in
  let T = Max_num_parents.eq in
  let statement = t.statement in
  let prev_challenges =
    (* TODO: This is redone in the call to Dlog_based_reduced_me_only.prepare *)
    Vector.map ~f:Ipa.Wrap.compute_challenges
      statement.proof_state.me_only.old_bulletproof_challenges
  in
  let prev_statement_with_hashes : _ Dlog_based.Statement.In_circuit.t =
    { pass_through=
        Common.hash_pairing_me_only
          (Reduced_me_only.Pairing_based.prepare ~dlog_plonk_index:dlog_index
             statement.pass_through)
          ~app_state:data.value_to_field_elements
    ; proof_state=
        { statement.proof_state with
          deferred_values=
            { statement.proof_state.deferred_values with
              plonk=
                { plonk with
                  zeta= plonk0.zeta
                ; alpha= plonk0.alpha
                ; beta= plonk0.beta
                ; gamma= plonk0.gamma } }
        ; me_only=
            Common.hash_dlog_me_only Max_num_parents.n
              { old_bulletproof_challenges=
                  (* TODO: Get rid of this padding *)
                  Vector.extend_exn prev_challenges Max_num_parents.n
                    Dummy.Ipa.Wrap.challenges_computed
              ; sg= statement.proof_state.me_only.sg } } }
  in
  let module O = Tock.Oracles in
  let o =
    let public_input =
      tock_public_input_of_statement prev_statement_with_hashes
    in
    O.create dlog_vk
      Vector.(
        map2
          (Vector.extend_exn statement.pass_through.sg Max_num_parents.n
             (Lazy.force Dummy.Ipa.Wrap.sg))
          (* This should indeed have length max_num_parents... No! It should have type max_num_parents_a. That is, the max_num_parents specific to a proof of this type...*)
          prev_challenges
          ~f:(fun commitment chals ->
            { Tock.Proof.Challenge_polynomial.commitment
            ; challenges= Vector.to_array chals } )
        |> to_list)
      public_input t.proof
  in
  let ((x_hat_1, x_hat_2) as x_hat) = O.(p_eval_1 o, p_eval_2 o) in
  let scalar_chal f =
    Scalar_challenge.map ~f:Challenge.Constant.of_tock_field (f o)
  in
  let plonk0 =
    { Types.Dlog_based.Proof_state.Deferred_values.Plonk.Minimal.alpha=
        scalar_chal O.alpha
    ; beta= O.beta o
    ; gamma= O.gamma o
    ; zeta= scalar_chal O.zeta }
  in
  let xi = scalar_chal O.v in
  let r = scalar_chal O.u in
  let sponge_digest_before_evaluations = O.digest_before_evaluations o in
  let to_field =
    SC.to_field_constant (module Tock.Field) ~endo:Endo.Step_inner_curve.scalar
  in
  let module As_field = struct
    let r = to_field r

    let xi = to_field xi

    let zeta = to_field plonk0.zeta

    let alpha = to_field plonk0.alpha
  end in
  let w =
    Tock.Field.domain_generator
      ~log2_size:(Domain.log2_size data.wrap_domains.h)
  in
  let zetaw = Tock.Field.mul As_field.zeta w in
  let new_bulletproof_challenges, b =
    let prechals =
      Array.map (O.opening_prechallenges o) ~f:(fun x ->
          Scalar_challenge.map ~f:Challenge.Constant.of_tock_field x )
    in
    let chals =
      Array.map prechals ~f:(fun x -> Ipa.Wrap.compute_challenge x)
    in
    let b_poly = unstage (b_poly chals) in
    let open As_field in
    let b =
      let open Tock.Field in
      b_poly zeta + (r * b_poly zetaw)
    in
    let prechals =
      Vector.of_list_and_length_exn
        ( Array.map prechals ~f:(fun x ->
              {Bulletproof_challenge.prechallenge= x} )
        |> Array.to_list )
        Tock.Rounds.n
    in
    (prechals, b)
  in
  let sg =
    if not must_verify then Ipa.Wrap.compute_sg new_bulletproof_challenges
    else t.proof.openings.proof.sg
  in
  let witness : _ Per_proof_witness.Constant.t =
    ( t.P.Base.Dlog_based.statement.pass_through.app_state
    , {prev_statement_with_hashes.proof_state with me_only= ()}
    , Double.map2 t.prev_evals t.prev_x_hat ~f:Tuple.T2.create
    , Vector.extend_exn t.statement.pass_through.sg Max_num_parents.n
        (Lazy.force Dummy.Ipa.Wrap.sg)
      (* TODO: This computation is also redone elsewhere. *)
    , Vector.extend_exn
        (Vector.map t.statement.pass_through.old_bulletproof_challenges
           ~f:Ipa.Step.compute_challenges)
        Max_num_parents.n Dummy.Ipa.Step.challenges_computed
    , ({t.proof.openings.proof with sg}, t.proof.messages) )
  in
  let combined_inner_product =
    let e1, e2 = t.proof.openings.evals in
    let b_polys =
      Vector.map
        ~f:(fun chals -> unstage (b_poly (Vector.to_array chals)))
        prev_challenges
    in
    let open As_field in
    let combine (x_hat : Tock.Field.t) pt e =
      let a, b = Dlog_plonk_types.Evals.(to_vectors (e : _ array t)) in
      let v : (Tock.Field.t array, _) Vector.t =
        Vector.append
          (Vector.map b_polys ~f:(fun f -> [|f pt|]))
          ([|x_hat|] :: a)
          (snd (Max_num_parents.add Nat.N8.n))
      in
      let open Tock.Field in
      let domains = data.wrap_domains in
      Pcs_batch.combine_split_evaluations
        (Common.dlog_pcs_batch
           (Max_num_parents.add Nat.N8.n)
           ~max_quot_size:(Common.max_quot_size_int (Domain.size domains.h)))
        ~xi ~init:Fn.id ~mul ~last:Array.last
        ~mul_and_add:(fun ~acc ~xi fx -> fx + (xi * acc))
        ~evaluation_point:pt
        ~shifted_pow:(fun deg x ->
          Pcs_batch.pow ~one ~mul x
            Int.(Max_degree.wrap - (deg mod Max_degree.wrap)) )
        v b
    in
    let open Tock.Field in
    combine x_hat_1 As_field.zeta e1 + (r * combine x_hat_2 zetaw e2)
  in
  let chal = Challenge.Constant.of_tock_field in
  let plonk =
    Plonk_checks.derive_plonk
      (module Tock.Field)
      ~shift:Shifts.tock ~endo:Endo.Wrap_inner_curve.base
      ~mds:Tock_field_sponge.params.mds
      ~domain:
        (Plonk_checks.domain
           (module Tock.Field)
           data.wrap_domains.h ~shifts:Common.tock_shifts
           ~domain_generator:Backend.Tock.Field.domain_generator)
      {plonk0 with zeta= As_field.zeta; alpha= As_field.alpha}
      (Plonk_checks.evals_of_split_evals
         (module Tock.Field)
         t.proof.openings.evals ~rounds:(Nat.to_int Tock.Rounds.n)
         ~zeta:As_field.zeta ~zetaw)
      x_hat_1
    |> fst
  in
  let shifted_value =
    Shifted_value.of_field (module Tock.Field) ~shift:Shifts.tock
  in
  let module M = P3.T (Per_proof_witness.Constant) in
  ( `Sg sg
  , { Types.Pairing_based.Proof_state.Per_proof.deferred_values=
        { plonk=
            { plonk with
              zeta= plonk0.zeta
            ; alpha= plonk0.alpha
            ; beta= chal plonk0.beta
            ; gamma= chal plonk0.gamma }
        ; combined_inner_product= shifted_value combined_inner_product
        ; xi
        ; bulletproof_challenges= new_bulletproof_challenges
        ; b= shifted_value b }
    ; should_finalize= must_verify
    ; sponge_digest_before_evaluations=
        Digest.Constant.of_tock_field sponge_digest_before_evaluations }
  , `Me_only prev_statement_with_hashes.proof_state.me_only
  , `X_hat x_hat
  , M.to_poly witness )

module Proof_system = struct
  module Step = struct
    include Step_main.Proof_system.Step

    let step_one :
           'max Pickles_types.Nat.t
        -> Pickles__.Impls.Wrap.Verification_key.t
        -> Backend.Tock.Curve.Affine.t
           Pickles_types.Dlog_plonk_types.Poly_comm.Without_degree_bound.t
           Pickles_types.Plonk_verification_key_evals.t
        -> ( 'value
           , 'max_num_parents
           , 'num_rules
           , Types.Proof_with_data.witness )
           Pickles_types.Higher_kinded_poly.P3.t
        -> ('var, 'value, 'max_num_parents, 'num_rules) Pickles__.Tag.t
        -> must_verify:bool
        -> [`Sg of Backend.Tock.Curve.Affine.t]
           * Types.Unfinalized_constant.t
           * [`Me_only of Pickles__.Impls.Step.Digest.Constant.t]
           * [`X_hat of Backend.Tock.Field.t Tuple_lib.Double.t]
           * ( 'value
             , 'max_num_parents
             , 'num_rules
             , Types.Per_proof_witness_constant.witness )
             Pickles_types.Higher_kinded_poly.P3.t =
      step_one
  end
end

module Evals_vector = struct
  type ('a, 'n) t = (('a * Backend.Tock.Field.t) Double.t, 'n) Vector.t
end

module Make
    (A : T0) (A_value : sig
        type t

        val to_field_elements : t -> Tick.Field.t array
    end)
    (Max_num_parents : Nat.Add.Intf_transparent) =
struct
  let double_zip = Double.map2 ~f:Core_kernel.Tuple2.create

  module E = struct
    type t = Tock.Field.t array Dlog_plonk_types.Evals.t Double.t
  end

  (* The prover corresponding to the given inductive rule. *)
  let f
      (type prev_max_num_parentss max_num_parentss self_num_rules prev_varss
      prev_valuess prev_num_parentsss prev_num_rulesss prevs_lengths
      prevs_length per_proof_witnesses per_proof_witness_constants unfinalizeds
      unfinalized_constants proof_with_datas evalss) ?handler
      (T branch_data :
        ( A.t
        , A_value.t
        , Max_num_parents.n
        , self_num_rules
        , prev_varss
        , prev_valuess
        , prev_num_parentsss
        , prev_num_rulesss
        , ( per_proof_witnesses
          , per_proof_witness_constants
          , unfinalizeds
          , unfinalized_constants
          , proof_with_datas
          , evalss )
          H6.T(Step_main.PS).t )
        Step_branch_data.t) (next_state : A_value.t)
      ~(proof_systems :
         ( per_proof_witnesses
         , per_proof_witness_constants
         , unfinalizeds
         , unfinalized_constants
         , proof_with_datas
         , evalss )
         H6.T(Step_main.PS).t)
      ~(maxes : (max_num_parentss, prev_max_num_parentss) H2.T(Maxes.Intf).t)
      ~(maxes_sum : (max_num_parentss, Max_num_parents.n) Nat.Sum.t)
      ~(prevs_lengths : (prev_varss, prevs_lengths) H2.T(Length).t)
      ~(prevs_length : (prevs_lengths, prevs_length) Nat.Sum.t) ~self
      ~step_domains ~self_dlog_plonk_index pk self_dlog_vk
      (prev_with_proofs :
        ( prev_valuess
        , prev_num_parentsss
        , prev_num_rulesss
        , proof_with_datas )
        H4.T(H3_1.T(P3)).t) :
      ( A_value.t
      , (_, max_num_parentss) H2.T(Vector).t
      , (_, prevs_length) Vector.t
      , (_, prevs_length) Vector.t
      , _
      , (_, max_num_parentss) H2.T(Evals_vector).t )
      P.Base.Pairing_based.t
      Async.Deferred.t =
    let _, prev_vars_lengths, prev_vars_length = branch_data.num_parents in
    let ltes = branch_data.ltes in
    let sum = branch_data.sum in
    (* The shape of maxes must be the one expected by this step branch. *)
    let T, T = Nat.Sum.eq_exn maxes_sum sum in
    let T, T = Nat.Sum.eq_exn prev_vars_length prevs_length in
    let (module Req) = branch_data.requests in
    let prev_values_length =
      let rec f : type a b c d e.
             (a, b, c, d) H4.T(H4.T(Tag)).t
          -> (a, e) H2.T(Length).t
          -> (b, e) H2.T(Length).t =
       fun tagss lengths ->
        match (tagss, lengths) with
        | [], [] ->
            []
        | tags :: tagss, length :: lengths ->
            let module L12 = H4.Length_1_to_2 (Tag) in
            L12.f tags length :: f tagss lengths
      in
      f branch_data.rule.prevs prev_vars_lengths
    in
    let inners_must_verifys =
      let prevs =
        let rec f
            : type prev_valuess prev_num_parentsss prev_num_rulesss per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants proof_with_datas evalss.
               ( prev_valuess
               , prev_num_parentsss
               , prev_num_rulesss
               , proof_with_datas )
               H4.T(H3_1.T(P3)).t
            -> ( per_proof_witnesses
               , per_proof_witness_constants
               , unfinalizeds
               , unfinalized_constants
               , proof_with_datas
               , evalss )
               H6.T(Step_main.PS).t
            -> prev_valuess H1.T(H1.T(Id)).t =
         fun proofss proof_systems ->
          match (proofss, proof_systems) with
          | [], [] ->
              []
          | [] :: proofss, _ :: proof_systems ->
              [] :: f proofss proof_systems
          | (proof :: proofs) :: proofss, ((module PS_) :: _ as proof_systems)
            ->
              let app_state = PS_.Step.proof_with_data_app_state proof in
              let (app_states :: app_statess) =
                f (proofs :: proofss) proof_systems
              in
              (app_state :: app_states) :: app_statess
        in
        f prev_with_proofs proof_systems
      in
      branch_data.rule.main_value prevs next_state
    in
    let sgs, unfinalized_proofs, me_onlys, x_hats, witnesses =
      let rec go
          : type vars values ns ms maxes prev_maxes k ks per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants proof_with_datas evalss.
             ( per_proof_witnesses
             , per_proof_witness_constants
             , unfinalizeds
             , unfinalized_constants
             , proof_with_datas
             , evalss )
             H6.T(Step_main.PS).t
          -> (values, ns, ms, proof_with_datas) H4.T(H3_1.T(P3)).t
          -> (maxes, prev_maxes) H2.T(Maxes.Intf).t
          -> (vars, values, ns, ms) H4.T(H4.T(Tag)).t
          -> vars H1.T(H1.T(E01(Bool))).t
          -> (vars, ks) H2.T(Length).t
          -> (ks, k) Nat.Sum.t
          -> k Vector.Carrying(Tock.Curve.Affine).t
             * (unfinalized_constants, ks) H2.T(Vector).t
             * ks H1.T(Vector.Carrying(Impls.Step.Digest.Constant)).t
             * ks H1.T(Vector.Carrying(Mk_double(Tock.Field))).t
             * (values, ns, ms, per_proof_witness_constants) H4.T(H3_1.T(P3)).t
          =
       fun proof_systems pss maxess tss must_verifyss ls sum ->
        match (proof_systems, pss, maxess, tss, must_verifyss, ls, sum) with
        | [], [], _, [], [], [], [] ->
            ([], [], [], [], [])
        | ( _ :: proof_systems
          , [] :: pss
          , _ :: maxess
          , [] :: tss
          , [] :: must_verifyss
          , Z :: ls
          , Z :: sum ) ->
            let sgss, uss, me_onlyss, xss, wss =
              go proof_systems pss maxess tss must_verifyss ls sum
            in
            (sgss, [] :: uss, [] :: me_onlyss, [] :: xss, [] :: wss)
        | ( ((module PS_) :: _ as proof_systems)
          , (p :: ps) :: pss
          , (module Maxes) :: maxess
          , (t :: ts) :: tss
          , (must_verify :: must_verifys) :: must_verifyss
          , S l :: ls
          , S add :: sum ) -> (
          match (Maxes.maxes, Maxes.length) with
          | max :: _, S _ ->
              let dlog_vk, dlog_index =
                if Type_equal.Id.same self.Tag.id t.id then
                  (self_dlog_vk, self_dlog_plonk_index)
                else
                  let d = Types_map.lookup_basic t in
                  (d.wrap_vk, d.wrap_key)
              in
              let `Sg sg, u, `Me_only me_only, `X_hat x, w =
                PS_.Step.step_one max dlog_vk dlog_index p t ~must_verify
              in
              let sgss, us :: uss, me_onlys :: me_onlyss, xs :: xss, ws :: wss
                  =
                go proof_systems (ps :: pss)
                  (Hlist.Maxes.Intf.pred (module Maxes) :: maxess)
                  (ts :: tss)
                  (must_verifys :: must_verifyss)
                  (l :: ls) (add :: sum)
              in
              ( sg :: sgss
              , (u :: us) :: uss
              , (me_only :: me_onlys) :: me_onlyss
              , (x :: xs) :: xss
              , (w :: ws) :: wss )
          | [], Z ->
              assert false )
        | _ :: _, _, [], _, _, _, _ ->
            assert false
      in
      go proof_systems prev_with_proofs maxes branch_data.rule.prevs
        inners_must_verifys prev_vars_lengths prev_vars_length
    in
    let next_statement : _ Types.Pairing_based.Statement.t =
      let unfinalized_proofs_extended =
        let rec f
            : type vars values ns ms maxes prev_maxes k per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants proof_with_datas evalss max_num_parentss max_num_parents ks.
               ( per_proof_witnesses
               , per_proof_witness_constants
               , unfinalizeds
               , unfinalized_constants
               , proof_with_datas
               , evalss )
               H6.T(Step_main.PS).t
            -> (max_num_parentss, max_num_parents) Nat.Sum.t
            -> (unfinalized_constants, ks) H2.T(Vector).t
            -> (ks, max_num_parentss) H2.T(Nat.Lte).t
            -> (unfinalized_constants, max_num_parentss) H2.T(Vector).t =
         fun proof_systems max_num_parentss unfinalized_proofss ltes ->
          match
            (proof_systems, max_num_parentss, unfinalized_proofss, ltes)
          with
          | [], [], [], [] ->
              []
          | ( (module PS_) :: proof_systems
            , max_num_parents :: max_num_parentss
            , unfinalized_proofs :: unfinalized_proofss
            , lte :: ltes ) ->
              let unfinalized_proofs_extended =
                Vector.extend unfinalized_proofs lte
                  (Nat.Adds.to_nat max_num_parents)
                  PS_.Step.dummy_unfinalized
              in
              let unfinalized_proofs_extendeds =
                f proof_systems max_num_parentss unfinalized_proofss ltes
              in
              unfinalized_proofs_extended :: unfinalized_proofs_extendeds
          | _ :: _, [], _, _ ->
              .
          | [], _ :: _, _, _ ->
              .
        in
        f proof_systems maxes_sum unfinalized_proofs ltes
      in
      let pass_through =
        let rec f
            : type per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants proof_with_datas evalss values ns ms ks k.
               ( per_proof_witnesses
               , per_proof_witness_constants
               , unfinalizeds
               , unfinalized_constants
               , proof_with_datas
               , evalss )
               H6.T(Step_main.PS).t
            -> (values, ns, ms, proof_with_datas) H4.T(H3_1.T(P3)).t
            -> ns H1.T(H1.T(P.Base.Me_only.Dlog_based)).t =
         fun proof_systems proofss ->
          match (proof_systems, proofss) with
          | [], [] ->
              []
          | _ :: proof_systems, [] :: proofss ->
              [] :: f proof_systems proofss
          | ((module PS_) :: _ as proof_systems), (proof :: proofs) :: proofss
            ->
              let pass_through =
                PS_.Step.proof_with_data_pass_throughs proof
              in
              let (pass_throughs :: pass_throughss) =
                f proof_systems (proofs :: proofss)
              in
              (pass_through :: pass_throughs) :: pass_throughss
        in
        f proof_systems prev_with_proofs
      in
      let old_bulletproof_challenges =
        let rec f
            : type per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants proof_with_datas evalss values ns ms ks k.
               ( per_proof_witnesses
               , per_proof_witness_constants
               , unfinalizeds
               , unfinalized_constants
               , proof_with_datas
               , evalss )
               H6.T(Step_main.PS).t
            -> (values, ns, ms, proof_with_datas) H4.T(H3_1.T(P3)).t
            -> (values, ks) H2.T(Length).t
            -> (ks, k) Nat.Sum.t
            -> ( Challenge.Constant.t Scalar_challenge.t Bulletproof_challenge.t
                 Step_bp_vec.t
               , k )
               Vector.t =
         fun proof_systems proofss ls sum ->
          match (proof_systems, proofss, ls, sum) with
          | [], [], [], [] ->
              []
          | _ :: proof_systems, [] :: proofss, Z :: ls, Z :: sum ->
              f proof_systems proofss ls sum
          | ( ((module PS_) :: _ as proof_systems)
            , (proof :: proofs) :: proofss
            , S len :: ls
            , S add :: sum ) ->
              let bulletproof_challenges =
                PS_.Step.proof_with_data_bulletproof_challenges proof
              in
              bulletproof_challenges
              :: f proof_systems (proofs :: proofss) (len :: ls) (add :: sum)
        in
        f proof_systems prev_with_proofs prev_values_length prev_vars_length
      in
      let me_only : _ Reduced_me_only.Pairing_based.t =
        (* Have the sg be available in the opening proof and verify it. *)
        {app_state= next_state; sg= sgs; old_bulletproof_challenges}
      in
      { proof_state= {unfinalized_proofs= unfinalized_proofs_extended; me_only}
      ; pass_through }
    in
    let next_me_only_prepared =
      Reduced_me_only.Pairing_based.prepare
        ~dlog_plonk_index:self_dlog_plonk_index
        next_statement.proof_state.me_only
    in
    let handler (Snarky_backendless.Request.With {request; respond} as r) =
      let k x = respond (Provide x) in
      match request with
      | Req.Proof_with_datas ->
          k witnesses
      | Req.Wrap_index ->
          k self_dlog_plonk_index
      | Req.App_state ->
          k next_me_only_prepared.app_state
      | _ -> (
        match handler with
        | Some f ->
            f r
        | None ->
            Snarky_backendless.Request.unhandled )
    in
    let%map.Async (next_proof : Tick.Proof.t) =
      let (T (input, conv)) =
        Step_branch_data.input_of_hlist ~max_num_parentss:maxes_sum
          ~proof_systems
      in
      let dummy_hash =
        lazy
          (let t : _ Types.Dlog_based.Proof_state.Me_only.t =
             { sg= Lazy.force Dummy.Ipa.Step.sg
             ; old_bulletproof_challenges=
                 Vector.init Max_num_parents.n ~f:(fun _ ->
                     Dummy.Ipa.Wrap.challenges_computed ) }
           in
           Common.hash_dlog_me_only Max_num_parents.n t)
      in
      let rec pad : type ns ks total.
             ks H1.T(Vector.Carrying(Impls.Step.Digest.Constant)).t
          -> (ks, ns) H2.T(Nat.Lte).t
          -> (ns, total) Nat.Sum.t
          -> ns H1.T(Vector.Carrying(Impls.Step.Digest.Constant)).t =
       fun xss ltes sum ->
        match (xss, ltes, sum) with
        | [], [], [] ->
            []
        | xs :: xss, lte :: ltes, add :: sum ->
            Vector.extend_add xs lte add (Lazy.force dummy_hash)
            :: pad xss ltes sum
      in
      let {Domains.h; x} =
        List.nth_exn
          (Vector.to_list step_domains)
          (Index.to_int branch_data.index)
      in
      let to_fold_in =
        let rec f
            : type per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants proof_with_datas evalss values ns ms ks k.
               ( per_proof_witnesses
               , per_proof_witness_constants
               , unfinalizeds
               , unfinalized_constants
               , proof_with_datas
               , evalss )
               H6.T(Step_main.PS).t
            -> (values, ns, ms, proof_with_datas) H4.T(H3_1.T(P3)).t
            -> (values, ks) H2.T(Length).t
            -> (ks, k) Nat.Sum.t
            -> (Tick.Curve.Affine.t, k) Vector.t =
         fun proof_systems proofss ls sum ->
          match (proof_systems, proofss, ls, sum) with
          | [], [], [], [] ->
              []
          | _ :: proof_systems, [] :: proofss, Z :: ls, Z :: sum ->
              f proof_systems proofss ls sum
          | ( ((module PS_) :: _ as proof_systems)
            , (proof :: proofs) :: proofss
            , S len :: ls
            , S add :: sum ) ->
              let sg = PS_.Step.proof_with_data_sg proof in
              sg :: f proof_systems (proofs :: proofss) (len :: ls) (add :: sum)
        in
        f proof_systems prev_with_proofs prev_values_length prev_vars_length
      in
      ksprintf Common.time "step-prover %d (%d, %d)"
        (Index.to_int branch_data.index) (Domain.size h) (Domain.size x)
        (fun () ->
          Impls.Step.generate_witness_conv
            ~f:
              (fun {Impls.Step.Proof_inputs.auxiliary_inputs; public_inputs} ->
              Backend.Tick.Proof.create_async ~primary:public_inputs
                ~auxiliary:auxiliary_inputs
                ~message:
                  (* emphatically NOT padded with dummies *)
                  Vector.(
                    map2 to_fold_in
                      next_me_only_prepared.old_bulletproof_challenges
                      ~f:(fun commitment chals ->
                        { Tick.Proof.Challenge_polynomial.commitment
                        ; challenges= Vector.to_array chals } )
                    |> to_list)
                pk )
            [input]
            (fun x () ->
              ( Impls.Step.handle
                  (fun () -> (branch_data.main ~step_domains (conv x) : unit))
                  handler
                : unit ) )
            ()
            { proof_state=
                { unfinalized_proofs=
                    next_statement.proof_state.unfinalized_proofs
                ; me_only=
                    Common.hash_pairing_me_only
                      ~app_state:A_value.to_field_elements
                      next_me_only_prepared }
            ; pass_through=
                (* TODO: Use the same pad_pass_through function as in wrap *)
                pad me_onlys ltes maxes_sum } )
    in
    let prev_evals =
      let rec f
          : type per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants proof_with_datas evalss values ns ms ks k max_num_parentss max_num_parents.
             ( per_proof_witnesses
             , per_proof_witness_constants
             , unfinalizeds
             , unfinalized_constants
             , proof_with_datas
             , evalss )
             H6.T(Step_main.PS).t
          -> (values, ns, ms, proof_with_datas) H4.T(H3_1.T(P3)).t
          -> ks H1.T(Vector.Carrying(Mk_double(Tock.Field))).t
          -> (values, ks) H2.T(Length).t
          -> (ks, max_num_parentss) H2.T(Nat.Lte).t
          -> (max_num_parentss, max_num_parents) Nat.Sum.t
          -> (evalss, max_num_parentss) H2.T(Evals_vector).t =
       fun proof_systems proofss x_hatss ls ltes sum ->
        match (proof_systems, proofss, x_hatss, ls, ltes, sum) with
        | [], [], [], [], [], [] ->
            []
        | ( _ :: proof_systems
          , [] :: proofss
          , [] :: x_hatss
          , Z :: ls
          , Z :: ltes
          , Z :: sum ) ->
            [] :: f proof_systems proofss x_hatss ls ltes sum
        | ( ((module PS_) :: _ as proof_systems)
          , ([] :: _ as proofss)
          , ([] :: _ as x_hatss)
          , (Z :: _ as ls)
          , Z :: ltes
          , S add :: sum ) ->
            let (evals :: evalss) =
              f proof_systems proofss x_hatss ls (Z :: ltes) (add :: sum)
            in
            (PS_.Step.dummy_evals :: evals) :: evalss
        | ( ((module PS_) :: _ as proof_systems)
          , (proof :: proofs) :: proofss
          , (x_hat :: x_hats) :: x_hatss
          , S len :: ls
          , S lte :: ltes
          , S add :: sum ) ->
            let eval =
              double_zip (PS_.Step.proof_with_data_evals proof) x_hat
            in
            let (evals :: evalss) =
              f proof_systems (proofs :: proofss) (x_hats :: x_hatss)
                (len :: ls) (lte :: ltes) (add :: sum)
            in
            (eval :: evals) :: evalss
        | _ :: _, (_ :: _) :: _, [] :: _, _ :: _, _, _ ->
            .
        | _ :: _, [] :: _, (_ :: _) :: _, _ :: _, _, _ ->
            .
        | _ :: _, (_ :: _) :: _, [], _, _, _ ->
            .
        | _ :: _, [] :: _, [], _, _, _ ->
            .
        | [], [], _ :: _, _, _, _ ->
            .
      in
      f proof_systems prev_with_proofs x_hats prev_values_length ltes maxes_sum
    in
    { P.Base.Pairing_based.proof= next_proof
    ; statement= next_statement
    ; index= branch_data.index
    ; prev_evals }
end
