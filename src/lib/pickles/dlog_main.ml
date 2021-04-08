module type Inputs = Intf.Wrap_main_inputs.S

module S = Sponge
open Backend
open Core_kernel
open Util
module SC = Scalar_challenge
open Pickles_types
open Dlog_plonk_types
module Accumulator = Pairing_marlin_types.Accumulator
open Tuple_lib
open Import

(* given [chals], compute
   \prod_i (1 / chals.(i) + chals.(i) * x^{2^i}) *)
let b_poly ~one ~add ~mul chals =
  let ( + ) = add and ( * ) = mul in
  stage (fun pt ->
      let k = Array.length chals in
      let pow_two_pows =
        let res = Array.init k ~f:(fun _ -> pt) in
        for i = 1 to k - 1 do
          let y = res.(i - 1) in
          res.(i) <- y * y
        done ;
        res
      in
      let prod f =
        let r = ref (f 0) in
        for i = 1 to k - 1 do
          r := f i * !r
        done ;
        !r
      in
      prod (fun i -> one + (chals.(i) * pow_two_pows.(k - 1 - i))) )

module Make
    (Inputs : Inputs
              with type Impl.field = Tock.Field.t
               and type Inner_curve.Constant.Scalar.t = Tick.Field.t) =
struct
  open Inputs
  open Impl

  module Other_field = struct
    module Packed = struct
      module Constant = Other_field

      type t = Impls.Wrap.Other_field.t

      let typ = Impls.Wrap.Other_field.typ

      let to_bits_unsafe (x : t) = Wrap_main_inputs.Unsafe.unpack_unboolean x

      let absorb_shifted sponge (x : t Shifted_value.t) =
        match x with Shifted_value x -> Sponge.absorb sponge x
    end

    module Unpacked = struct
      type t = Boolean.var list

      type constant = bool list

      let typ : (t, constant) Typ.t =
        let typ = Typ.list ~length:Field.size_in_bits Boolean.typ in
        let p_msb =
          let test_bit x i = B.(shift_right x i land one = one) in
          List.init Other_field.size_in_bits ~f:(test_bit Other_field.size)
          |> List.rev
        in
        let check xs_lsb =
          let open Bitstring_lib.Bitstring in
          Snarky_backendless.Checked.all_unit
            [ typ.check xs_lsb
            ; make_checked (fun () ->
                  Bitstring_checked.lt_value
                    (Msb_first.of_list (List.rev xs_lsb))
                    (Msb_first.of_list p_msb)
                  |> Boolean.Assert.is_true ) ]
        in
        {typ with check}

      let assert_equal t1 t2 = Field.(Assert.equal (project t1) (project t2))
    end
  end

  let print_g lab (x, y) =
    if debug then
      as_prover
        As_prover.(
          fun () ->
            Core.printf "in-snark: %s %!" lab ;
            Field.Constant.print (read_var x) ;
            Core.printf ", %!" ;
            Field.Constant.print (read_var y) ;
            Core.printf "\n%!")

  let print_w lab gs =
    if debug then
      Array.iteri gs ~f:(fun i (fin, g) ->
          as_prover
            As_prover.(
              fun () -> Core.printf "fin=%b %!" (read Boolean.typ fin)) ;
          ksprintf print_g "%s[%d]" lab i g )

  let print_chal lab x =
    if debug then
      as_prover
        As_prover.(
          fun () ->
            Core.printf "in-snark %s:%!" lab ;
            Field.Constant.print
              (Field.Constant.project (List.map ~f:(read Boolean.typ) x)) ;
            Core.printf "\n%!")

  let print_bool lab x =
    if debug then
      as_prover (fun () ->
          printf "%s: %b\n%!" lab (As_prover.read Boolean.typ x) )

  module Challenge = Challenge.Make (Impl)
  module Digest = Digest.Make (Impl)
  module Scalar_challenge =
    SC.Make (Impl) (Inner_curve) (Challenge) (Endo.Wrap_inner_curve)
  module Ops = Plonk_curve_ops.Make (Impl) (Inner_curve)

  let product m f = List.reduce_exn (List.init m ~f) ~f:Field.( * )

  let absorb sponge ty t =
    absorb
      ~mask_g1_opt:(fun () -> assert false)
      ~absorb_field:(Sponge.absorb sponge)
      ~g1_to_field_elements:Inner_curve.to_field_elements
      ~absorb_scalar:(Sponge.absorb sponge) ty t

  let squeeze_scalar sponge : Scalar_challenge.t * Field.t SC.SC.t =
    (* No need to boolean constrain scalar challenges. *)
    let bits, packed = Sponge.squeeze sponge ~length:Challenge.length in
    (Scalar_challenge bits, Scalar_challenge packed)

  let bullet_reduce sponge gammas =
    let absorb t = absorb sponge t in
    let prechallenges =
      Array.map gammas ~f:(fun gammas_i ->
          absorb (PC :: PC) gammas_i ;
          squeeze_scalar sponge )
    in
    let term_and_challenge (l, r) (pre, pre_packed) =
      let left_term = Scalar_challenge.endo_inv l pre in
      let right_term = Scalar_challenge.endo r pre in
      ( Ops.add_fast left_term right_term
      , {Bulletproof_challenge.prechallenge= pre_packed} )
    in
    let terms, challenges =
      Array.map2_exn gammas prechallenges ~f:term_and_challenge |> Array.unzip
    in
    (Array.reduce_exn terms ~f:Ops.add_fast, challenges)

  let equal_g g1 g2 =
    List.map2_exn ~f:Field.equal
      (Inner_curve.to_field_elements g1)
      (Inner_curve.to_field_elements g2)
    |> Boolean.all

  module One_hot_vector = One_hot_vector.Make (Impl)

  type 'a index' = 'a Plonk_verification_key_evals.t

  type 'a index =
    'a Poly_comm.Without_degree_bound.t Plonk_verification_key_evals.t

  let positions () (type a) : (a index' -> a) index' =
    let open Plonk_verification_key_evals in
    { sigma_comm_0
    ; sigma_comm_1
    ; sigma_comm_2
    ; ql_comm
    ; qr_comm
    ; qo_comm
    ; qm_comm
    ; qc_comm
    ; rcm_comm_0
    ; rcm_comm_1
    ; rcm_comm_2
    ; psm_comm
    ; add_comm
    ; mul1_comm
    ; mul2_comm
    ; emul1_comm
    ; emul2_comm
    ; emul3_comm }

  (* Mask out the given vector of indices with the given one-hot vector *)
  let choose_key : type n.
         n One_hot_vector.t
      -> (Inner_curve.t index', n) Vector.t
      -> Inner_curve.t index' =
    let open Tuple_lib in
    let map = Plonk_verification_key_evals.map in
    let map2 = Plonk_verification_key_evals.map2 in
    fun bs keys ->
      let open Field in
      Vector.map2
        (bs :> (Boolean.var, n) Vector.t)
        keys
        ~f:(fun b key -> map key ~f:(fun g -> Double.map g ~f:(( * ) (b :> t))))
      |> Vector.reduce_exn ~f:(map2 ~f:(Double.map2 ~f:( + )))
      |> map ~f:(fun g -> Double.map ~f:(Util.seal (module Impl)) g)

  let lagrange (type n)
      ~domain:( (which_rule : n One_hot_vector.t)
              , (domains : (Domains.t, n) Vector.t) ) i =
    Vector.map domains ~f:(fun d ->
        let d =
          Precomputed.Lagrange_precomputations.index_of_domain_log2
            (Domain.log2_size d.h)
        in
        match Precomputed.Lagrange_precomputations.vesta.(d).(i) with
        | [|g|] ->
            let g = Inner_curve.Constant.of_affine g in
            Inner_curve.constant g
        | _ ->
            assert false )
    |> Vector.map2
         (which_rule :> (Boolean.var, n) Vector.t)
         ~f:(fun b (x, y) -> Field.((b :> t) * x, (b :> t) * y))
    |> Vector.reduce_exn ~f:(Double.map2 ~f:Field.( + ))

  let lagrange_with_correction (type n) ~input_length
      ~domain:( (which_rule : n One_hot_vector.t)
              , (domains : (Domains.t, n) Vector.t) ) i =
    let rec pow2pow x i =
      if i = 0 then x else pow2pow Inner_curve.Constant.(x + x) (i - 1)
    in
    Vector.map domains ~f:(fun d ->
        let d =
          Precomputed.Lagrange_precomputations.index_of_domain_log2
            (Domain.log2_size d.h)
        in
        match Precomputed.Lagrange_precomputations.vesta.(d).(i) with
        | [|g|] ->
            let g = Inner_curve.Constant.of_affine g in
            ( Inner_curve.constant g
            , Inner_curve.constant
                (Inner_curve.Constant.negate (pow2pow g input_length)) )
        | xs ->
            failwithf "expected commitment to have length 1. got %d"
              (Array.length xs) () )
    |> Vector.map2
         (which_rule :> (Boolean.var, n) Vector.t)
         ~f:(fun b pr ->
           Double.map pr ~f:(fun (x, y) -> Field.((b :> t) * x, (b :> t) * y))
           )
    |> Vector.reduce_exn ~f:(Double.map2 ~f:(Double.map2 ~f:Field.( + )))

  let h_precomp =
    Lazy.map ~f:Inner_curve.Scaling_precomputation.create Generators.h

  let group_map =
    let f =
      lazy
        (let module M =
           Group_map.Bw19.Make (Field.Constant) (Field)
             (struct
               let params =
                 Group_map.Bw19.Params.create
                   (module Field.Constant)
                   {b= Inner_curve.Params.b}
             end)
         in
        let open M in
        Snarky_group_map.Checked.wrap
          (module Impl)
          ~potential_xs
          ~y_squared:(fun ~x ->
            Field.(
              (x * x * x)
              + (constant Inner_curve.Params.a * x)
              + constant Inner_curve.Params.b) )
        |> unstage)
    in
    fun x -> Lazy.force f x

  module Split_commitments = struct
    module Point = struct
      type t =
        [ `Finite of Inner_curve.t
        | `Maybe_finite of Boolean.var * Inner_curve.t ]

      let finite : t -> Boolean.var = function
        | `Finite _ ->
            Boolean.true_
        | `Maybe_finite (b, _) ->
            b

      let add (p : t) (q : Inner_curve.t) =
        match p with
        | `Finite p ->
            Ops.add_fast p q
        | `Maybe_finite (finite, p) ->
            Inner_curve.if_ finite ~then_:(Ops.add_fast p q) ~else_:q

      let underlying = function `Finite p -> p | `Maybe_finite (_, p) -> p
    end

    module Curve_opt = struct
      type t = {point: Inner_curve.t; non_zero: Boolean.var}
    end

    let combine batch ~xi without_bound with_bound =
      let {Curve_opt.non_zero; point} =
        Pcs_batch.combine_split_commitments batch
          ~scale_and_add:(fun ~(acc : Curve_opt.t) ~xi (keep, (p : Point.t)) ->
            (* match acc.non_zero, keep with
              | false, false -> acc
              | true, false -> acc
              | false, true -> { point= p; non_zero= true }
              | true, true -> { point= p + xi * acc; non_zero= true }
            *)
            let point =
              Inner_curve.(
                if_ keep
                  ~then_:
                    (if_ acc.non_zero
                       ~then_:
                         (Point.add p (Scalar_challenge.endo acc.point xi))
                       ~else_:
                         ((* In this rule, the accumulator was zero, so there is no harm in
                            putting the potentially junk underlying point here. *)
                          Point.underlying p))
                  ~else_:acc.point)
            in
            let non_zero =
              Boolean.(keep &&& Point.finite p ||| acc.non_zero)
            in
            {Curve_opt.non_zero; point} )
          ~xi
          ~init:(fun (keep, p) ->
            { non_zero= Boolean.(keep &&& Point.finite p)
            ; point= Point.underlying p } )
          without_bound with_bound
      in
      Boolean.Assert.is_true non_zero ;
      point
  end

  let scale_fast p (Shifted_value.Shifted_value bits) =
    Ops.scale_fast p (`Plus_two_to_len (Array.of_list bits))

  let check_bulletproof ~pcs_batch ~sponge ~xi
      ~(combined_inner_product : Other_field.Packed.t Shifted_value.t)
      ~
      (* Corresponds to y in figure 7 of WTS *)
      (* sum_i r^i sum_j xi^j f_j(beta_i) *)
      (advice : _ Types.Pairing_based.Openings.Bulletproof.Advice.t)
      ~polynomials:(without_degree_bound, with_degree_bound)
      ~openings_proof:({lr; delta; z_1; z_2; sg} :
                        ( Inner_curve.t
                        , Other_field.Packed.t Shifted_value.t )
                        Openings.Bulletproof.t) =
    let scale_fast p s =
      scale_fast p (Shifted_value.map ~f:Other_field.Packed.to_bits_unsafe s)
    in
    with_label __LOC__ (fun () ->
        Other_field.Packed.absorb_shifted sponge combined_inner_product ;
        (* a_hat should be equal to
      sum_i < t, r^i pows(beta_i) >
      = sum_i r^i < t, pows(beta_i) > *)
        let u =
          let t = Sponge.squeeze_field sponge in
          group_map t
        in
        let open Inner_curve in
        let combined_polynomial (* Corresponds to xi in figure 7 of WTS *) =
          Split_commitments.combine pcs_batch ~xi without_degree_bound
            with_degree_bound
        in
        let lr_prod, challenges = bullet_reduce sponge lr in
        let p_prime =
          let uc = scale_fast u combined_inner_product in
          combined_polynomial + uc
        in
        let q = p_prime + lr_prod in
        absorb sponge PC delta ;
        let c, _c_packed = squeeze_scalar sponge in
        (* c Q + delta = z1 (G + b U) + z2 H *)
        let lhs =
          let cq = Scalar_challenge.endo q c in
          cq + delta
        in
        let rhs =
          let b_u = scale_fast u advice.b in
          let z_1_g_plus_b_u = scale_fast (sg + b_u) z_1 in
          let z2_h =
            scale_fast (Inner_curve.constant (Lazy.force Generators.h)) z_2
          in
          z_1_g_plus_b_u + z2_h
        in
        (`Success (equal_g lhs rhs), challenges) )

  module Opt =
    S.Bit_sponge.Make (struct
        type t = Boolean.var
      end)
      (struct
        type t = Field.t

        let to_bits t = Wrap_main_inputs.Unsafe.unpack_unboolean t

        let finalize_discarded = Util.boolean_constrain (module Impl)

        let high_entropy_bits = Wrap_main_inputs.high_entropy_bits
      end)
      (struct
        type t = Boolean.var * Field.t
      end)
      (Opt_sponge.Make (Impl) (Wrap_main_inputs.Sponge.Permutation))

  let absorb sponge ty t =
    Util.absorb ~absorb_field:(Opt.absorb sponge)
      ~g1_to_field_elements:(fun (b, (x, y)) -> [(b, x); (b, y)])
      ~absorb_scalar:(fun x -> Opt.absorb sponge (Boolean.true_, x))
      ~mask_g1_opt:(fun (keep, ((finite : Boolean.var), (x, y))) ->
        (keep, Field.((finite :> t) * x, (finite :> t) * y)) )
      ty t

  module Pseudo = Pseudo.Make (Impl)

  let mask (type n) (lengths : (int, n) Vector.t) (choice : n One_hot_vector.t)
      : Boolean.var array =
    let max =
      Option.value_exn
        (List.max_elt ~compare:Int.compare (Vector.to_list lengths))
    in
    let length = Pseudo.choose (choice, lengths) ~f:Field.of_int in
    let (T max) = Nat.of_int max in
    Vector.to_array (ones_vector (module Impl) ~first_zero:length max)

  let mask_messages (type n) ~(lengths : (int, n) Vector.t Evals.t)
      (choice : n One_hot_vector.t) (m : _ Messages.t) =
    let f lens = Array.zip_exn (mask lens choice) in
    let g lg {Poly_comm.With_degree_bound.shifted; unshifted} =
      { Poly_comm.With_degree_bound.shifted= (Boolean.true_, shifted)
      ; unshifted= f lg unshifted }
    in
    { Messages.l_comm= f lengths.l m.l_comm
    ; r_comm= f lengths.r m.r_comm
    ; o_comm= f lengths.o m.o_comm
    ; z_comm= f lengths.z m.z_comm
    ; t_comm= g lengths.t m.t_comm }

  module Plonk = Types.Dlog_based.Proof_state.Deferred_values.Plonk

  (* Just for exhaustiveness over fields *)
  let iter2 ~chal ~scalar_chal
      {Plonk.Minimal.alpha= alpha_0; beta= beta_0; gamma= gamma_0; zeta= zeta_0}
      {Plonk.Minimal.alpha= alpha_1; beta= beta_1; gamma= gamma_1; zeta= zeta_1}
      =
    scalar_chal alpha_0 alpha_1 ;
    chal beta_0 beta_1 ;
    chal gamma_0 gamma_1 ;
    scalar_chal zeta_0 zeta_1

  let assert_eq_marlin
      (m1 : (_, Field.t Pickles_types.Scalar_challenge.t) Plonk.Minimal.t)
      (m2 : (Boolean.var list, Scalar_challenge.t) Plonk.Minimal.t) =
    iter2 m1 m2
      ~chal:(fun c1 c2 -> Field.Assert.equal c1 (Field.project c2))
      ~scalar_chal:
        (fun (Scalar_challenge t1 : _ Pickles_types.Scalar_challenge.t)
             (Scalar_challenge t2 : Scalar_challenge.t) ->
        Field.Assert.equal t1 (Field.project t2) )

  let incrementally_verify_proof (type b)
      (module Max_num_input_proofs : Nat.Add.Intf with type n = b)
      ~rules_num_input_proofs ~step_domains
      ~verification_key:(m : _ Plonk_verification_key_evals.t) ~xi ~sponge
      ~public_input ~(sg_old : (_, Max_num_input_proofs.n) Vector.t)
      ~(combined_inner_product : _ Shifted_value.t) ~advice
      ~(messages : (_, Boolean.var * _) Messages.t) ~which_rule ~openings_proof
      ~(plonk :
         _ Types.Dlog_based.Proof_state.Deferred_values.Plonk.In_circuit.t) =
    let T = Max_num_input_proofs.eq in
    let messages =
      with_label __LOC__ (fun () ->
          let open Vector in
          let lengths =
            let f field = map step_domains ~f:(Fn.compose Domain.size field) in
            Commitment_lengths.generic map ~h:(f Domains.h)
              ~max_degree:Common.Max_degree.step
          in
          mask_messages ~lengths which_rule messages )
    in
    let sg_old =
      with_label __LOC__ (fun () ->
          let actual_num_input_proofs =
            Pseudo.choose (which_rule, rules_num_input_proofs) ~f:Field.of_int
          in
          Vector.map2
            (ones_vector
               (module Impl)
               ~first_zero:actual_num_input_proofs Max_num_input_proofs.n)
            sg_old
            ~f:(fun keep sg -> [|(keep, sg)|]) )
    in
    with_label __LOC__ (fun () ->
        let receive ty f =
          with_label __LOC__ (fun () ->
              let x = f messages in
              absorb sponge ty x ; x )
        in
        let sample () =
          let xs = Opt.squeeze sponge ~length:Challenge.length in
          Util.boolean_constrain (module Impl) xs ;
          xs
        in
        let sample_scalar () : Scalar_challenge.t =
          (* No need to boolean constrain scalar challenges. *)
          Scalar_challenge (Opt.squeeze sponge ~length:Challenge.length)
        in
        let open Dlog_plonk_types.Messages in
        let x_hat =
          with_label __LOC__ (fun () ->
              let domain = (which_rule, step_domains) in
              let terms =
                Array.mapi public_input ~f:(fun i x ->
                    match Array.of_list x with
                    | [|(b : Boolean.var)|] ->
                        assert_ (Constraint.boolean (b :> Field.t)) ;
                        `Cond_add (b, lagrange ~domain i)
                    | x ->
                        `Add_with_correction
                          ( x
                          , lagrange_with_correction
                              ~input_length:(Array.length x) ~domain i ) )
              in
              let correction =
                with_label __LOC__ (fun () ->
                    Array.reduce_exn
                      (Array.filter_map terms ~f:(function
                        | `Cond_add _ ->
                            None
                        | `Add_with_correction (_, (_, corr)) ->
                            Some corr ))
                      ~f:Ops.add_fast )
              in
              Array.fold terms ~init:correction ~f:(fun acc term ->
                  match term with
                  | `Cond_add (b, g) ->
                      Inner_curve.if_ b ~then_:(Ops.add_fast g acc) ~else_:acc
                  | `Add_with_correction (x, (g, _)) ->
                      Ops.add_fast acc (Ops.scale_fast g (`Plus_two_to_len x))
              ) )
          |> Inner_curve.negate
        in
        let without = Type.Without_degree_bound in
        let with_ = Type.With_degree_bound in
        absorb sponge PC (Boolean.true_, x_hat) ;
        let l_comm = receive without l_comm in
        let r_comm = receive without r_comm in
        let o_comm = receive without o_comm in
        let beta = sample () in
        let gamma = sample () in
        let z_comm = receive without z_comm in
        let alpha = sample_scalar () in
        let t_comm = receive with_ t_comm in
        let zeta = sample_scalar () in
        Util.boolean_constrain
          (module Impl)
          (match zeta with Scalar_challenge x -> x) ;
        (* At this point, we should use the previous "bulletproof_challenges" to
       compute to compute f(beta_1) outside the snark
       where f is the polynomial corresponding to sg_old
    *)
        let sponge =
          match S.Bit_sponge.underlying sponge with
          | {state; sponge_state; params} -> (
            match sponge_state with
            | Squeezed n ->
                S.make ~state ~sponge_state:(Squeezed n) ~params
            | _ ->
                assert false )
        in
        let sponge_before_evaluations = Sponge.copy sponge in
        let sponge_digest_before_evaluations = Sponge.squeeze_field sponge in
        (* xi, r are sampled here using the other sponge. *)
        (* No need to expose the polynomial evaluations as deferred values as they're
       not needed here for the incremental verification. All we need is a_hat and
       "combined_inner_product".

       Then, in the other proof, we can witness the evaluations and check their correctness
       against "combined_inner_product" *)
        let f_comm =
          let ( + ) = Ops.add_fast in
          let ( * ) = Fn.flip scale_fast in
          let generic =
            (plonk.gnrc_l * ((plonk.gnrc_r * m.qm_comm) + m.ql_comm))
            + (plonk.gnrc_r * m.qr_comm) + (plonk.gnrc_o * m.qo_comm)
            + m.qc_comm
          in
          let poseidon =
            let ( * ) = Fn.flip Scalar_challenge.endo in
            (* alpha^2 rcm_comm[0] + alpha^3 rcm_comm[1] + alpha^4 rcm_comm[2]
                =
                alpha^2 (rcm_comm[0] + alpha (rcm_comm[1] + alpha rcm_comm[2]))
            *)
            let a = alpha in
            m.rcm_comm_0 + (a * (m.rcm_comm_1 + (a * m.rcm_comm_2)))
            |> ( * ) a |> ( * ) a
          in
          let g =
            List.reduce_exn ~f:( + )
              [ plonk.perm1 * m.sigma_comm_2
              ; generic
              ; poseidon
              ; plonk.psdn0 * m.psm_comm
              ; plonk.ecad0 * m.add_comm
              ; plonk.vbmul0 * m.mul1_comm
              ; plonk.vbmul1 * m.mul2_comm
              ; plonk.endomul0 * m.emul1_comm
              ; plonk.endomul1 * m.emul2_comm
              ; plonk.endomul2 * m.emul3_comm ]
          in
          let res = Array.map z_comm ~f:(fun (b, x) -> (b, plonk.perm0 * x)) in
          res.(0)
          <- (let b, r = res.(0) in
              (Boolean.true_, Inner_curve.if_ b ~then_:(r + g) ~else_:g)) ;
          res
        in
        let bulletproof_challenges =
          (* This sponge needs to be initialized with (some derivative of)
         1. The polynomial commitments
         2. The combined inner product
         3. The challenge points.

         It should be sufficient to fork the sponge after squeezing beta_3 and then to absorb
         the combined inner product.
      *)
          let without_degree_bound =
            Vector.append sg_old
              [ [|(Boolean.true_, x_hat)|]
              ; l_comm
              ; r_comm
              ; o_comm
              ; z_comm
              ; f_comm
              ; [|(Boolean.true_, m.sigma_comm_0)|]
              ; [|(Boolean.true_, m.sigma_comm_1)|] ]
              (snd (Max_num_input_proofs.add Nat.N8.n))
          in
          check_bulletproof
            ~pcs_batch:
              (Common.dlog_pcs_batch
                 (Max_num_input_proofs.add Nat.N8.n)
                 ~max_quot_size:())
            ~sponge:sponge_before_evaluations ~xi ~combined_inner_product
            ~advice ~openings_proof
            ~polynomials:
              ( Vector.map without_degree_bound
                  ~f:(Array.map ~f:(fun (keep, x) -> (keep, `Finite x)))
              , [ t_comm
                  |> Dlog_plonk_types.Poly_comm.With_degree_bound.map
                       ~f:(fun (keep, x) -> (keep, `Maybe_finite x)) ] )
        in
        assert_eq_marlin
          { alpha= plonk.alpha
          ; beta= plonk.beta
          ; gamma= plonk.gamma
          ; zeta= plonk.zeta }
          {alpha; beta; gamma; zeta} ;
        (sponge_digest_before_evaluations, bulletproof_challenges) )

  module Split_evaluations = struct
    let combine_split_evaluations' s =
      Pcs_batch.combine_split_evaluations s
        ~mul:(fun (keep, x) (y : Field.t) -> (keep, Field.(y * x)))
        ~mul_and_add:(fun ~acc ~xi (keep, fx) ->
          Field.if_ keep ~then_:Field.(fx + (xi * acc)) ~else_:acc )
        ~init:(fun (_, fx) -> fx)
        ~shifted_pow:
          (Pseudo.Degree_bound.shifted_pow
             ~crs_max_degree:Common.Max_degree.wrap)
  end

  let mask_evals (type n) ~(lengths : (int, n) Vector.t Evals.t)
      (choice : n One_hot_vector.t) (e : Field.t array Evals.t) :
      (Boolean.var * Field.t) array Evals.t =
    Evals.map2 lengths e ~f:(fun lengths e ->
        Array.zip_exn (mask lengths choice) e )

  let combined_evaluation (type b b_plus_8) b_plus_8 ~xi ~evaluation_point
      ((without_degree_bound : (_, b_plus_8) Vector.t), with_degree_bound)
      ~max_quot_size =
    let open Field in
    Pcs_batch.combine_split_evaluations ~mul ~last:Array.last
      ~mul_and_add:(fun ~acc ~xi fx -> fx + (xi * acc))
      ~shifted_pow:
        (Pseudo.Degree_bound.shifted_pow ~crs_max_degree:Common.Max_degree.wrap)
      ~init:Fn.id ~evaluation_point ~xi
      (Common.dlog_pcs_batch b_plus_8 ~max_quot_size)
      without_degree_bound with_degree_bound

  let compute_challenges ~scalar chals =
    Vector.map chals ~f:(fun {Bulletproof_challenge.prechallenge} ->
        scalar prechallenge )

  let b_poly = Field.(b_poly ~add ~mul ~one)

  let pack_scalar_challenge (Pickles_types.Scalar_challenge.Scalar_challenge t)
      =
    Field.pack (Challenge.to_bits t)

  let actual_evaluation (e : Field.t array) (pt : Field.t) : Field.t =
    let pt_n =
      with_label __LOC__ (fun () ->
          let max_degree_log2 = Int.ceil_log2 Common.Max_degree.wrap in
          let rec go acc i =
            if i = 0 then acc else go (Field.square acc) (i - 1)
          in
          go pt max_degree_log2 )
    in
    with_label __LOC__ (fun () ->
        match List.rev (Array.to_list e) with
        | e :: es ->
            List.fold ~init:e es ~f:(fun acc y ->
                let acc' =
                  exists Field.typ ~compute:(fun () ->
                      As_prover.read_var Field.(y + (pt_n * acc)) )
                in
                (* acc' = y + pt_n * acc *)
                let pt_n_acc = Field.(pt_n * acc) in
                let open Zexe_backend_common.Plonk_constraint_system
                         .Plonk_constraint in
                (* 0 = - acc' + y + pt_n_acc *)
                let open Field.Constant in
                assert_
                  [ { annotation= None
                    ; basic=
                        T
                          (Basic
                             { l= (one, y)
                             ; r= (one, pt_n_acc)
                             ; o= (negate one, acc')
                             ; m= zero
                             ; c= zero }) } ] ;
                acc' )
        | [] ->
            failwith "empty list" )

  let shift =
    Shifted_value.Shift.(
      map ~f:Field.constant (create (module Field.Constant)))

  let%test_unit "endo scalar" =
    SC.test (module Impl) ~endo:Endo.Step_inner_curve.scalar

  (* This finalizes the "deferred values" coming from a previous proof over the same field.
   It
   1. Checks that [xi] and [r] where sampled correctly. I.e., by absorbing all the
   evaluation openings and then squeezing.
   2. Checks that the "combined inner product" value used in the elliptic curve part of
   the opening proof was computed correctly, in terms of the evaluation openings and the
   evaluation points.
   3. Check that the "b" value was computed correctly.
   4. Perform the arithmetic checks from marlin. *)
  let finalize_other_proof (type b)
      (module Num_input_proofs : Nat.Add.Intf with type n = b)
      ?actual_num_input_proofs ~domain ~max_quot_size ~sponge
      ~(old_bulletproof_challenges : (_, b) Vector.t)
      ({xi; combined_inner_product; bulletproof_challenges; b; plonk} :
        ( _
        , _
        , _ Shifted_value.t
        , _ )
        Types.Pairing_based.Proof_state.Deferred_values.In_circuit.t)
      ((evals1, x_hat1), (evals2, x_hat2)) =
    let T = Num_input_proofs.eq in
    let open Vector in
    (* You use the NEW bulletproof challenges to check b. Not the old ones. *)
    let open Field in
    let absorb_evals x_hat e =
      with_label __LOC__ (fun () ->
          let xs, ys = Evals.to_vectors e in
          List.iter
            Vector.([|x_hat|] :: (to_list xs @ to_list ys))
            ~f:(Array.iter ~f:(Sponge.absorb sponge)) )
    in
    (* A lot of hashing. *)
    absorb_evals x_hat1 evals1 ;
    absorb_evals x_hat2 evals2 ;
    let squeeze () =
      with_label __LOC__ (fun () ->
          let x, x_packed = Sponge.squeeze sponge ~length:Challenge.length in
          Util.boolean_constrain (module Impl) x ;
          (x, x_packed) )
    in
    let _, xi_actual = squeeze () in
    let r_actual, _ = squeeze () in
    let xi_correct =
      with_label __LOC__ (fun () ->
          (* Sample new sg challenge point here *)
          Field.equal xi_actual (pack_scalar_challenge xi) )
    in
    let scalar =
      SC.to_field_checked (module Impl) ~endo:Endo.Step_inner_curve.scalar
    in
    let plonk =
      with_label __LOC__ (fun () ->
          Types.Pairing_based.Proof_state.Deferred_values.Plonk.In_circuit
          .map_challenges
            ~f:(Fn.compose (Util.seal (module Impl)) Field.pack)
            ~scalar plonk
          |> Types.Pairing_based.Proof_state.Deferred_values.Plonk.In_circuit
             .map_fields
               ~f:(Shifted_value.map ~f:(Util.seal (module Impl))) )
    in
    let xi = scalar xi in
    (* TODO: r actually does not need to be a scalar challenge. *)
    let r = scalar (Scalar_challenge r_actual) in
    let zetaw = Field.mul domain#generator plonk.zeta in
    let combined_inner_product_correct =
      with_label __LOC__ (fun () ->
          (* sum_i r^i sum_j xi^j f_j(beta_i) *)
          let actual_combined_inner_product =
            let sg_olds =
              Vector.map old_bulletproof_challenges ~f:(fun chals ->
                  unstage (b_poly (Vector.to_array chals)) )
            in
            let combine pt x_hat e =
              let pi = Num_input_proofs.add Nat.N8.n in
              let a, b = Evals.to_vectors (e : Field.t array Evals.t) in
              let sg_evals =
                match actual_num_input_proofs with
                | None ->
                    Vector.map sg_olds ~f:(fun f -> [|f pt|])
                | Some num_input_proofs ->
                    let mask =
                      ones_vector
                        (module Impl)
                        ~first_zero:num_input_proofs (Vector.length sg_olds)
                    in
                    Vector.map2 mask sg_olds ~f:(fun b f ->
                        [|Field.((b :> t) * f pt)|] )
              in
              let v = Vector.append sg_evals ([|x_hat|] :: a) (snd pi) in
              combined_evaluation pi ~xi ~evaluation_point:pt (v, b)
                ~max_quot_size
            in
            combine plonk.zeta x_hat1 evals1 + (r * combine zetaw x_hat2 evals2)
          in
          equal
            (Shifted_value.to_field
               (module Field)
               ~shift combined_inner_product)
            actual_combined_inner_product )
    in
    let bulletproof_challenges =
      with_label __LOC__ (fun () ->
          compute_challenges ~scalar bulletproof_challenges )
    in
    let b_correct =
      with_label __LOC__ (fun () ->
          let b_poly =
            unstage (b_poly (Vector.to_array bulletproof_challenges))
          in
          let b_actual = b_poly plonk.zeta + (r * b_poly zetaw) in
          equal (Shifted_value.to_field (module Field) ~shift b) b_actual )
    in
    let plonk_checks_passed =
      with_label __LOC__ (fun () ->
          let e = Fn.flip actual_evaluation in
          Plonk_checks.checked
            (module Impl)
            ~endo:(Impl.Field.constant Endo.Wrap_inner_curve.base)
            ~domain ~shift plonk ~mds:sponge_params.mds
            ( Dlog_plonk_types.Evals.map ~f:(e plonk.zeta) evals1
            , Dlog_plonk_types.Evals.map ~f:(e zetaw) evals2 )
            x_hat1 )
    in
    print_bool "xi_correct" xi_correct ;
    print_bool "combined_inner_product_correct" combined_inner_product_correct ;
    print_bool "plonk_checks_passed" plonk_checks_passed ;
    print_bool "b_correct" b_correct ;
    ( Boolean.all
        [ xi_correct
        ; b_correct
        ; combined_inner_product_correct
        ; plonk_checks_passed ]
    , bulletproof_challenges )

  let map_challenges
      { Types.Pairing_based.Proof_state.Deferred_values.plonk
      ; combined_inner_product
      ; xi
      ; bulletproof_challenges
      ; b } ~f ~scalar =
    { Types.Pairing_based.Proof_state.Deferred_values.plonk=
        Types.Pairing_based.Proof_state.Deferred_values.Plonk.In_circuit
        .map_challenges plonk ~f ~scalar
    ; combined_inner_product
    ; bulletproof_challenges=
        Vector.map bulletproof_challenges
          ~f:(fun (r : _ Bulletproof_challenge.t) ->
            {Bulletproof_challenge.prechallenge= scalar r.prechallenge} )
    ; xi= scalar xi
    ; b }

  (* TODO: No need to hash the entire bulletproof challenges. Could
   just hash the segment of the public input LDE corresponding to them
   that we compute when verifying the previous proof. That is a commitment
   to them. *)

  let hash_me_only (type n) (_max_num_input_proofs : n Nat.t)
      (t : (_, (_, n) Vector.t) Types.Dlog_based.Proof_state.Me_only.t) =
    let sponge = Sponge.create sponge_params in
    Array.iter ~f:(Sponge.absorb sponge)
      (Types.Dlog_based.Proof_state.Me_only.to_field_elements
         ~g1:Inner_curve.to_field_elements t) ;
    Sponge.squeeze_field sponge
end
