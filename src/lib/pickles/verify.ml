module SC = Scalar_challenge
open Core
open Pickles_types
open Common
open Import
open Types
open Backend
open Tuple_lib

module Instance = struct
  type t =
    | T :
        (module Nat.Intf with type n = 'n)
        * (module Intf.Statement_value with type t = 'a)
        * Verification_key.t
        * 'a
        * ('n, 'n) Proof.t
        -> t
end

let verify_heterogenous (ts : Instance.t list) =
  let module Plonk = Types.Dlog_based.Proof_state.Deferred_values.Plonk in
  let module Tick_field = Backend.Tick.Field in
  let tick_field : _ Plonk_checks.field = (module Tick_field) in
  let check, result =
    let r = ref [] in
    let result () =
      match !r with
      | [] ->
          Ok ()
      | _ ->
          Error
            (String.concat ~sep:"\n"
               (List.map !r ~f:(fun lab -> Lazy.force lab)))
    in
    ((fun (lab, b) -> if not b then r := lab :: !r), result)
  in
  let in_circuit_plonks =
    List.map ts
      ~f:(fun (T
                ( _max_num_input_proofs
                , _statement
                , key
                , app_state
                , T
                    { statement
                    ; prev_x_hat= (x_hat1, _) as prev_x_hat
                    ; prev_evals= evals } ))
         ->
        Timer.start __LOC__ ;
        let statement =
          {statement with pass_through= {statement.pass_through with app_state}}
        in
        let open Pairing_marlin_types in
        let open Types.Dlog_based.Proof_state in
        let sc =
          SC.to_field_constant tick_field ~endo:Endo.Wrap_inner_curve.scalar
        in
        Timer.clock __LOC__ ;
        let { Deferred_values.xi
            ; plonk= plonk0
            ; combined_inner_product
            ; which_rule
            ; bulletproof_challenges } =
          Deferred_values.map_challenges ~f:Challenge.Constant.to_tick_field
            ~scalar:sc statement.proof_state.deferred_values
        in
        let zeta = sc plonk0.zeta in
        let alpha = sc plonk0.alpha in
        let step_domains = key.step_domains.(Index.to_int which_rule) in
        let w =
          Tick.Field.domain_generator
            ~log2_size:(Domain.log2_size step_domains.h)
        in
        let zetaw = Tick.Field.mul zeta w in
        let plonk =
          let chal = Challenge.Constant.to_tick_field in
          let p, `Check_equal (lin1, lin2) =
            Plonk_checks.derive_plonk
              (module Tick.Field)
              ~endo:Endo.Step_inner_curve.base ~shift:Shifts.tick
              ~mds:Tick_field_sponge.params.mds
              ~domain:
                (* TODO: Cache the shifts and domain_generator *)
                (Plonk_checks.domain
                   (module Tick.Field)
                   step_domains.h ~shifts:Common.tick_shifts
                   ~domain_generator:Backend.Tick.Field.domain_generator)
              {zeta; beta= chal plonk0.beta; gamma= chal plonk0.gamma; alpha}
              (Plonk_checks.evals_of_split_evals
                 (module Tick.Field)
                 evals ~rounds:(Nat.to_int Tick.Rounds.n) ~zeta ~zetaw)
              x_hat1
          in
          check (lazy "linearization_check", Tick.Field.equal lin1 lin2) ;
          { p with
            zeta= plonk0.zeta
          ; alpha= plonk0.alpha
          ; beta= plonk0.beta
          ; gamma= plonk0.gamma }
        in
        Timer.clock __LOC__ ;
        let absorb, squeeze =
          let open Tick_field_sponge.Bits in
          let sponge =
            let s = create Tick_field_sponge.params in
            absorb s
              (Digest.Constant.to_tick_field
                 statement.proof_state.sponge_digest_before_evaluations) ;
            s
          in
          let squeeze () =
            let underlying =
              Challenge.Constant.of_bits
                (squeeze sponge ~length:Challenge.Constant.length)
            in
            sc (Scalar_challenge underlying)
          in
          (absorb sponge, squeeze)
        in
        let absorb_evals (x_hat, e) =
          let xs, ys = Dlog_plonk_types.Evals.to_vectors e in
          List.iter
            Vector.([|x_hat|] :: (to_list xs @ to_list ys))
            ~f:(Array.iter ~f:absorb)
        in
        Double.(iter ~f:absorb_evals (map2 prev_x_hat evals ~f:Tuple2.create)) ;
        let xi_actual = squeeze () in
        let r_actual = squeeze () in
        Timer.clock __LOC__ ;
        (* TODO: The deferred values "bulletproof_challenges" should get routed
           into a "batch dlog Tick acc verifier" *)
        let actual_num_input_proofs =
          Vector.length statement.pass_through.old_bulletproof_challenges
        in
        Timer.clock __LOC__ ;
        let combined_inner_product_actual =
          Wrap.combined_inner_product
            ~actual_num_input_proofs:(Nat.Add.create actual_num_input_proofs)
            evals ~x_hat:prev_x_hat
            ~old_bulletproof_challenges:
              (Vector.map ~f:Ipa.Step.compute_challenges
                 statement.pass_through.old_bulletproof_challenges)
            ~r:r_actual ~xi ~zeta ~zetaw ~step_branch_domains:step_domains
        in
        let check_eq lab x y =
          check
            ( lazy
                (sprintf
                   !"%s: %{sexp:Tick_field.t} != %{sexp:Tick_field.t}"
                   lab x y)
            , Tick_field.equal x y )
        in
        Timer.clock __LOC__ ;
        Timer.clock __LOC__ ;
        List.iter
          ~f:(fun (s, x, y) -> check_eq s x y)
          (* Both these values can actually be omitted from the proof on the wire since we recompute them
   anyway. *)
          [ ("xi", xi, xi_actual)
          ; ( "combined_inner_product"
            , Shifted_value.to_field
                (module Tick.Field)
                combined_inner_product ~shift:Shifts.tick
            , combined_inner_product_actual ) ] ;
        plonk )
  in
  let open Backend.Tock.Proof in
  Common.time "batch_step_dlog_check" (fun () ->
      check
        ( lazy "batch_step_dlog_check"
        , Ipa.Step.accumulator_check
            (List.map ts ~f:(fun (T (_, _, _, _, T t)) ->
                 ( t.statement.proof_state.me_only.sg
                 , Ipa.Step.compute_challenges
                     t.statement.proof_state.deferred_values
                       .bulletproof_challenges ) )) ) ) ;
  Common.time "dlog_check" (fun () ->
      check
        ( lazy "dlog_check"
        , batch_verify
            (List.map2_exn ts in_circuit_plonks
               ~f:(fun (T
                         ( ( module
                         Max_num_input_proofs )
                         , ( module
                         A_value )
                         , key
                         , app_state
                         , T t ))
                  plonk
                  ->
                 let prepared_statement :
                     _ Types.Dlog_based.Statement.In_circuit.t =
                   { pass_through=
                       Common.hash_pairing_me_only
                         ~app_state:A_value.to_field_elements
                         (Reduced_me_only.Pairing_based.prepare
                            ~dlog_plonk_index:key.commitments
                            {t.statement.pass_through with app_state})
                   ; proof_state=
                       { t.statement.proof_state with
                         deferred_values=
                           {t.statement.proof_state.deferred_values with plonk}
                       ; me_only=
                           Common.hash_dlog_me_only Max_num_input_proofs.n
                             (Reduced_me_only.Dlog_based.prepare
                                t.statement.proof_state.me_only) } }
                 in
                 let input =
                   tock_unpadded_public_input_of_statement prepared_statement
                 in
                 ( key.index
                 , t.proof
                 , input
                 , Some
                     (Vector.to_list
                        (Vector.map2
                           ~f:(fun g cs ->
                             { Challenge_polynomial.challenges=
                                 Vector.to_array
                                   (Ipa.Wrap.compute_challenges cs)
                             ; commitment= g } )
                           (Vector.extend_exn t.statement.pass_through.sg
                              Max_num_input_proofs.n
                              (Lazy.force Dummy.Ipa.Wrap.sg))
                           t.statement.proof_state.me_only
                             .old_bulletproof_challenges)) ) )) ) ) ;
  match result () with
  | Ok () ->
      true
  | Error e ->
      eprintf !"bad verify: %s\n%!" e ;
      false

let verify (type a n)
    (max_num_input_proofs : (module Nat.Intf with type n = n))
    (a_value : (module Intf.Statement_value with type t = a))
    (key : Verification_key.t) (ts : (a * (n, n) Proof.t) list) =
  verify_heterogenous
    (List.map ts ~f:(fun (x, p) ->
         Instance.T (max_num_input_proofs, a_value, key, x, p) ))
