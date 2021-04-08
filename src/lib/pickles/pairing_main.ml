(* q > p *)
open Core_kernel
open Import
open Util
open Types.Pairing_based
module SC = Scalar_challenge
open Pickles_types
open Common
open Import
module S = Sponge

module Make
    (Inputs : Intf.Pairing_main_inputs.S
              with type Impl.field = Backend.Tick.Field.t
               and type Impl.prover_state = unit
               and type Inner_curve.Constant.Scalar.t = Backend.Tock.Field.t) =
struct
  open Inputs
  open Impl
  module PC = Inner_curve
  module Challenge = Challenge.Make (Impl)
  module Digest = Digest.Make (Impl)
  module Number = Snarky_backendless.Number.Run.Make (Impl)

  (* Other_field.size > Field.size *)
  module Other_field = struct
    let size_in_bits = Field.size_in_bits

    module Constant = Other_field

    type t = Impls.Step.Other_field.t

    let typ = Impls.Step.Other_field.typ

    let to_bits_unsafe ((x, b) : t) =
      Step_main_inputs.Unsafe.unpack_unboolean x
        ~length:(Field.size_in_bits - 1)
      @ [b]
  end

  let print_g lab g =
    if debug then
      as_prover
        As_prover.(
          fun () ->
            match Inner_curve.to_field_elements g with
            | [x; y] ->
                printf !"%s: %!" lab ;
                Field.Constant.print (read_var x) ;
                printf ", %!" ;
                Field.Constant.print (read_var y) ;
                printf "\n%!"
            | _ ->
                assert false)

  let print_chal lab chal =
    if debug then
      as_prover
        As_prover.(
          fun () ->
            printf
              !"%s: %{sexp:Challenge.Constant.t}\n%!"
              lab
              (read Challenge.typ_unchecked chal))

  let print_fp lab x =
    if debug then
      as_prover (fun () ->
          printf "%s: %!" lab ;
          Field.Constant.print (As_prover.read Field.typ x) ;
          printf "\n%!" )

  let print_bool lab x =
    if debug then
      as_prover (fun () ->
          printf "%s: %b\n%!" lab (As_prover.read Boolean.typ x) )

  let equal_g g1 g2 =
    List.map2_exn ~f:Field.equal
      (Inner_curve.to_field_elements g1)
      (Inner_curve.to_field_elements g2)
    |> Boolean.all

  let absorb sponge ty t =
    absorb
      ~absorb_field:(fun x -> Sponge.absorb sponge (`Field x))
      ~g1_to_field_elements:Inner_curve.to_field_elements
      ~absorb_scalar:(fun (x, b) ->
        Sponge.absorb sponge (`Field x) ;
        Sponge.absorb sponge (`Bits [b]) )
      ~mask_g1_opt:(fun ((b : Boolean.var), (x, y)) ->
        Field.((b :> t) * x, (b :> t) * y) )
      ty t

  module Scalar_challenge =
    SC.Make (Impl) (Inner_curve) (Challenge) (Endo.Step_inner_curve)
  module Ops = Step_main_inputs.Ops

  module Inner_curve = struct
    include Inner_curve

    let ( + ) = Ops.add_fast
  end

  let multiscale_known ts =
    let rec pow2pow x i =
      if i = 0 then x else pow2pow Inner_curve.Constant.(x + x) (i - 1)
    in
    with_label __LOC__ (fun () ->
        let correction =
          Array.mapi ts ~f:(fun i (s, x) ->
              let n = Array.length s in
              pow2pow x n )
          |> Array.reduce_exn ~f:Inner_curve.Constant.( + )
        in
        let acc =
          Array.mapi ts ~f:(fun i (s, x) ->
              Ops.scale_fast (Inner_curve.constant x) (`Plus_two_to_len s) )
          |> Array.reduce_exn ~f:Inner_curve.( + )
        in
        Inner_curve.(acc + constant (Constant.negate correction)) )

  let squeeze_scalar sponge : Scalar_challenge.t * Field.t SC.SC.t =
    let bits, packed = Sponge.squeeze sponge ~length:Challenge.length in
    (* No need to boolean constrain scalar challenges. *)
    (Scalar_challenge bits, Scalar_challenge packed)

  let bullet_reduce sponge gammas =
    with_label __LOC__ (fun () ->
        let absorb t = absorb sponge t in
        let prechallenges =
          Array.mapi gammas ~f:(fun i gammas_i ->
              absorb (PC :: PC) gammas_i ;
              squeeze_scalar sponge )
        in
        let term_and_challenge (l, r) (pre, pre_packed) =
          let left_term = Scalar_challenge.endo_inv l pre in
          let right_term = Scalar_challenge.endo r pre in
          ( Inner_curve.(left_term + right_term)
          , {Bulletproof_challenge.prechallenge= pre_packed} )
        in
        let terms, challenges =
          Array.map2_exn gammas prechallenges ~f:term_and_challenge
          |> Array.unzip
        in
        (Array.reduce_exn terms ~f:Inner_curve.( + ), challenges) )

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

  let scale_fast p (Shifted_value.Shifted_value bits) =
    Ops.scale_fast p (`Plus_two_to_len (Array.of_list bits))

  let check_bulletproof ~pcs_batch ~sponge ~xi ~combined_inner_product
      ~
      (* Corresponds to y in figure 7 of WTS *)
      (* sum_i r^i sum_j xi^j f_j(beta_i) *)
      (advice : _ Openings.Bulletproof.Advice.t)
      ~polynomials:(without_degree_bound, with_degree_bound)
      ~openings_proof:({lr; delta; z_1; z_2; sg} :
                        ( Inner_curve.t
                        , Other_field.t Shifted_value.t )
                        Openings.Bulletproof.t) =
    let scale_fast p s =
      scale_fast p (Shifted_value.map ~f:Other_field.to_bits_unsafe s)
    in
    with_label __LOC__ (fun () ->
        absorb sponge Scalar
          ( match combined_inner_product with
          | Shifted_value.Shifted_value x ->
              x ) ;
        (* a_hat should be equal to
       sum_i < t, r^i pows(beta_i) >
       = sum_i r^i < t, pows(beta_i) > *)
        let u =
          let t = Sponge.squeeze_field sponge in
          group_map t
        in
        let open Inner_curve in
        let combined_polynomial (* Corresponds to xi in figure 7 of WTS *) =
          with_label __LOC__ (fun () ->
              Pcs_batch.combine_split_commitments pcs_batch
                ~scale_and_add:
                  (fun ~(acc :
                          [ `Maybe_finite of Boolean.var * Inner_curve.t
                          | `Finite of Inner_curve.t ]) ~xi p ->
                  match acc with
                  | `Maybe_finite (acc_is_finite, (acc : Inner_curve.t)) -> (
                    match p with
                    | `Maybe_finite (p_is_finite, p) ->
                        let is_finite =
                          Boolean.(p_is_finite ||| acc_is_finite)
                        in
                        let xi_acc = Scalar_challenge.endo acc xi in
                        `Maybe_finite
                          ( is_finite
                          , if_ acc_is_finite ~then_:(p + xi_acc) ~else_:p )
                    | `Finite p ->
                        let xi_acc = Scalar_challenge.endo acc xi in
                        `Finite
                          (if_ acc_is_finite ~then_:(p + xi_acc) ~else_:p) )
                  | `Finite acc ->
                      let xi_acc = Scalar_challenge.endo acc xi in
                      `Finite
                        ( match p with
                        | `Finite p ->
                            p + xi_acc
                        | `Maybe_finite (p_is_finite, p) ->
                            if_ p_is_finite ~then_:(p + xi_acc) ~else_:xi_acc
                        ) )
                ~xi
                ~init:(function
                  | `Finite x -> `Finite x | `Maybe_finite x -> `Maybe_finite x
                  )
                (Vector.map without_degree_bound
                   ~f:(Array.map ~f:(fun x -> `Finite x)))
                (Vector.map with_degree_bound
                   ~f:
                     (let open Dlog_plonk_types.Poly_comm.With_degree_bound in
                     fun {shifted; unshifted} ->
                       let f x = `Maybe_finite x in
                       {unshifted= Array.map ~f unshifted; shifted= f shifted}))
          )
          |> function `Finite x -> x | `Maybe_finite _ -> assert false
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
          with_label __LOC__ (fun () ->
              let b_u = scale_fast u advice.b in
              let z_1_g_plus_b_u = scale_fast (sg + b_u) z_1 in
              let z2_h =
                scale_fast (Inner_curve.constant (Lazy.force Generators.h)) z_2
              in
              z_1_g_plus_b_u + z2_h )
        in
        (`Success (equal_g lhs rhs), challenges) )

  let assert_eq_marlin
      (m1 :
        ( 'a
        , Inputs.Impl.Field.t Pickles_types.Scalar_challenge.t )
        Types.Pairing_based.Proof_state.Deferred_values.Plonk.Minimal.t)
      (m2 :
        ( Inputs.Impl.Field.t
        , Inputs.Impl.Field.t Pickles_types.Scalar_challenge.t )
        Types.Pairing_based.Proof_state.Deferred_values.Plonk.Minimal.t) =
    let open Types.Dlog_based.Proof_state.Deferred_values.Plonk.Minimal in
    let chal c1 c2 = Field.Assert.equal c1 c2 in
    let scalar_chal (Scalar_challenge t1 : _ Pickles_types.Scalar_challenge.t)
        (Scalar_challenge t2 : _ Pickles_types.Scalar_challenge.t) =
      Field.Assert.equal t1 t2
    in
    with_label __LOC__ (fun () -> chal m1.beta m2.beta) ;
    with_label __LOC__ (fun () -> chal m1.gamma m2.gamma) ;
    with_label __LOC__ (fun () -> scalar_chal m1.alpha m2.alpha) ;
    with_label __LOC__ (fun () -> scalar_chal m1.zeta m2.zeta)

  let lagrange_commitment ~domain i =
    let d =
      Zexe_backend.Pasta.Precomputed.Lagrange_precomputations
      .index_of_domain_log2 (Domain.log2_size domain)
    in
    match Precomputed.Lagrange_precomputations.pallas.(d).(i) with
    | [|g|] ->
        Inner_curve.Constant.of_affine g
    | _ ->
        assert false

  let incrementally_verify_proof (type b)
      (module Num_input_proofs : Nat.Add.Intf with type n = b) ~domain
      ~verification_key:(m : _ array Plonk_verification_key_evals.t) ~xi
      ~sponge ~public_input ~(sg_old : (_, Num_input_proofs.n) Vector.t)
      ~combined_inner_product ~advice
      ~(messages : (_, Boolean.var * _) Dlog_plonk_types.Messages.t)
      ~openings_proof
      ~(plonk :
         ( _
         , _
         , _ Shifted_value.t )
         Types.Dlog_based.Proof_state.Deferred_values.Plonk.In_circuit.t) =
    let m =
      Plonk_verification_key_evals.map m ~f:(function
        | [|g|] ->
            g
        | _ ->
            assert false )
    in
    with_label __LOC__ (fun () ->
        let receive ty f =
          with_label __LOC__ (fun () ->
              let x = f messages in
              absorb sponge ty x ; x )
        in
        let sample () =
          let bits, packed = Sponge.squeeze sponge ~length:Challenge.length in
          Util.boolean_constrain (module Impl) bits ;
          (bits, packed)
        in
        let sample_scalar () = squeeze_scalar sponge in
        let open Dlog_plonk_types.Messages in
        let x_hat =
          with_label __LOC__ (fun () ->
              multiscale_known
                (Array.mapi public_input ~f:(fun i x ->
                     (Array.of_list x, lagrange_commitment ~domain i) ))
              |> Inner_curve.negate )
        in
        let without = Type.Without_degree_bound in
        let with_ = Type.With_degree_bound in
        absorb sponge PC x_hat ;
        let l_comm = receive without l_comm in
        let r_comm = receive without r_comm in
        let o_comm = receive without o_comm in
        let beta, beta_packed = sample () in
        let gamma, gamma_packed = sample () in
        let z_comm = receive without z_comm in
        let alpha, alpha_packed = sample_scalar () in
        let t_comm = receive with_ t_comm in
        let zeta, zeta_packed = sample_scalar () in
        Util.boolean_constrain
          (module Impl)
          (match zeta with Scalar_challenge x -> x) ;
        (* At this point, we should use the previous "bulletproof_challenges" to
       compute to compute f(beta_1) outside the snark
       where f is the polynomial corresponding to sg_old
    *)
        let sponge_before_evaluations = Sponge.copy sponge in
        let sponge_digest_before_evaluations = Sponge.squeeze_field sponge in
        (* xi, r are sampled here using the other sponge. *)
        (* No need to expose the polynomial evaluations as deferred values as they're
       not needed here for the incremental verification. All we need is a_hat and
       "combined_inner_product".

       Then, in the other proof, we can witness the evaluations and check their correctness
       against "combined_inner_product" *)
        let f_comm =
          let ( + ) = Inner_curve.( + ) in
          let ( * ) = Fn.flip scale_fast in
          let generic =
            plonk.gnrc_l
            * ( with_label __LOC__ (fun () -> plonk.gnrc_r * m.qm_comm)
              + m.ql_comm )
            + with_label __LOC__ (fun () -> plonk.gnrc_r * m.qr_comm)
            + with_label __LOC__ (fun () -> plonk.gnrc_o * m.qo_comm)
            + m.qc_comm
          in
          let poseidon =
            (* alpha^2 rcm_comm[0] + alpha^3 rcm_comm[1] + alpha^4 rcm_comm[2]
                 =
                 alpha^2 (rcm_comm[0] + alpha (rcm_comm[1] + alpha rcm_comm[2]))
              *)
            let a = alpha in
            let ( * ) = Fn.flip Scalar_challenge.endo in
            m.rcm_comm_0 + (a * (m.rcm_comm_1 + (a * m.rcm_comm_2)))
            |> ( * ) a |> ( * ) a
          in
          let g =
            List.reduce_exn ~f:( + )
              [ with_label __LOC__ (fun () -> plonk.perm1 * m.sigma_comm_2)
              ; generic
              ; poseidon
              ; with_label __LOC__ (fun () -> plonk.psdn0 * m.psm_comm)
              ; with_label __LOC__ (fun () -> plonk.ecad0 * m.add_comm)
              ; with_label __LOC__ (fun () -> plonk.vbmul0 * m.mul1_comm)
              ; with_label __LOC__ (fun () -> plonk.vbmul1 * m.mul2_comm)
              ; with_label __LOC__ (fun () -> plonk.endomul0 * m.emul1_comm)
              ; with_label __LOC__ (fun () -> plonk.endomul1 * m.emul2_comm)
              ; with_label __LOC__ (fun () -> plonk.endomul2 * m.emul3_comm) ]
          in
          let res = Array.map z_comm ~f:(( * ) plonk.perm0) in
          res.(0) <- res.(0) + g ;
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
            let T = Num_input_proofs.eq in
            Vector.append
              (Vector.map sg_old ~f:(fun g -> [|g|]))
              [ [|x_hat|]
              ; l_comm
              ; r_comm
              ; o_comm
              ; z_comm
              ; f_comm
              ; [|m.sigma_comm_0|]
              ; [|m.sigma_comm_1|] ]
              (snd (Num_input_proofs.add Nat.N8.n))
          in
          with_label __LOC__ (fun () ->
              check_bulletproof
                ~pcs_batch:
                  (Common.dlog_pcs_batch
                     (Num_input_proofs.add Nat.N8.n)
                     ~max_quot_size:
                       (Common.max_quot_size_int (Domain.size domain)))
                ~sponge:sponge_before_evaluations ~xi ~combined_inner_product
                ~advice ~openings_proof
                ~polynomials:(without_degree_bound, [t_comm]) )
        in
        assert_eq_marlin
          { alpha= plonk.alpha
          ; beta= plonk.beta
          ; gamma= plonk.gamma
          ; zeta= plonk.zeta }
          { alpha= alpha_packed
          ; beta= beta_packed
          ; gamma= gamma_packed
          ; zeta= zeta_packed } ;
        (sponge_digest_before_evaluations, bulletproof_challenges) )

  let compute_challenges ~scalar chals =
    with_label __LOC__ (fun () ->
        Vector.map chals ~f:(fun {Bulletproof_challenge.prechallenge} ->
            scalar prechallenge ) )

  let b_poly = Field.(Dlog_main.b_poly ~add ~mul ~one)

  module Pseudo = Pseudo.Make (Impl)

  module Bounded = struct
    type t = {max: int; actual: Field.t}

    let of_pseudo ((_, ns) as p : _ Pseudo.t) =
      { max= Vector.reduce_exn ~f:Int.max ns
      ; actual= Pseudo.choose p ~f:Field.of_int }
  end

  let vanishing_polynomial mask =
    let mask = Vector.to_array mask in
    let max = Array.length mask in
    fun x ->
      let rec go acc i =
        if i >= max then acc
        else
          let should_square = mask.(i) in
          let acc =
            Field.if_ should_square ~then_:(Field.square acc) ~else_:acc
          in
          go acc (i + 1)
      in
      Field.sub (go x 0) Field.one

  let shifts ~log2_size =
    Common.tick_shifts ~log2_size
    |> Dlog_plonk_types.Shifts.map ~f:Impl.Field.constant

  let domain_generator ~log2_size =
    Backend.Tick.Field.domain_generator ~log2_size |> Impl.Field.constant

  module O = One_hot_vector.Make (Impl)

  let side_loaded_input_domain =
    let open Side_loaded_verification_key in
    let input_size = input_size ~of_int:Fn.id ~add:( + ) ~mul:( * ) in
    let max_num_input_proofs = Num_input_proofs.Max.n in
    let domain_log2s =
      Vector.init (S max_num_input_proofs) ~f:(fun w ->
          Int.ceil_log2 (input_size w) )
    in
    let (T max_log2_size) =
      let n = Int.ceil_log2 (input_size (Nat.to_int max_num_input_proofs)) in
      assert (List.last_exn (Vector.to_list domain_log2s) = n) ;
      Nat.of_int n
    in
    fun ~num_input_proofs ->
      let mask =
        O.of_index num_input_proofs ~length:(S max_num_input_proofs)
      in
      let shifts = lazy (Pseudo.Domain.shifts (mask, domain_log2s) ~shifts) in
      let generator =
        lazy (Pseudo.Domain.generator (mask, domain_log2s) ~domain_generator)
      in
      let vp =
        let log2_size = Pseudo.choose (mask, domain_log2s) ~f:Field.of_int in
        let mask =
          ones_vector (module Impl) max_log2_size ~first_zero:log2_size
        in
        vanishing_polynomial mask
      in
      object
        method shifts = Lazy.force shifts

        method generator = Lazy.force generator

        method size =
          Pseudo.choose (mask, domain_log2s) ~f:(fun x -> Field.of_int (1 lsl x))

        method vanishing_polynomial = vp
      end

  let side_loaded_domains (type num_rules) =
    let open Side_loaded_verification_key in
    fun (domains : (Field.t Domain.t Domains.t, num_rules) Vector.t)
        (which_rule : num_rules One_hot_vector.T(Impl).t) ->
      let domain v ~max =
        let (T max_n) = Nat.of_int max in
        let log2_size = Pseudo.choose ~f:Domain.log2_size (which_rule, v) in
        let mask = ones_vector (module Impl) max_n ~first_zero:log2_size in
        let log2_sizes =
          (O.of_index log2_size ~length:max_n, Vector.init max_n ~f:Fn.id)
        in
        let shifts = Pseudo.Domain.shifts log2_sizes ~shifts in
        let generator = Pseudo.Domain.generator log2_sizes ~domain_generator in
        let vanishing_polynomial = vanishing_polynomial mask in
        let size =
          Vector.map mask ~f:(fun b ->
              (* 0 -> 1
                  1 -> 2 *)
              Field.((b :> t) + one) )
          |> Vector.reduce_exn ~f:Field.( * )
        in
        object
          method size = size

          method log2_size = log2_size

          method vanishing_polynomial x = vanishing_polynomial x

          method shifts = shifts

          method generator = generator
        end
      in
      { Domains.h=
          domain
            (Vector.map domains ~f:(fun {h; _} -> h))
            ~max:(Domain.log2_size max_domains.h) }

  let%test_module "side loaded domains" =
    ( module struct
      let run k =
        let (), y =
          run_and_check
            (fun () ->
              let y = k () in
              fun () -> As_prover.read_var y )
            ()
          |> Or_error.ok_exn
        in
        y

      let%test_unit "side loaded input domain" =
        let open Side_loaded_verification_key in
        let input_size = input_size ~of_int:Fn.id ~add:( + ) ~mul:( * ) in
        let possibilities =
          Vector.init (S Num_input_proofs.Max.n) ~f:(fun w ->
              Int.ceil_log2 (input_size w) )
        in
        let pt = Field.Constant.random () in
        List.iteri (Vector.to_list possibilities) ~f:(fun i d ->
            let d_unchecked =
              Plonk_checks.domain
                (module Field.Constant)
                (Pow_2_roots_of_unity d) ~shifts:Common.tick_shifts
                ~domain_generator:Backend.Tick.Field.domain_generator
            in
            let checked_domain () =
              side_loaded_input_domain ~num_input_proofs:(Field.of_int i)
            in
            [%test_eq: Field.Constant.t]
              (d_unchecked#vanishing_polynomial pt)
              (run (fun () ->
                   (checked_domain ())#vanishing_polynomial (Field.constant pt)
               )) )

      let%test_unit "side loaded domains" =
        let module O = One_hot_vector.Make (Impl) in
        let open Side_loaded_verification_key in
        let num_rules = Nat.N2.n in
        let domains = Vector.[{Domains.h= 10}; {h= 15}] in
        let pt = Field.Constant.random () in
        List.iteri (Vector.to_list domains) ~f:(fun i ds ->
            let check field1 field2 =
              let d_unchecked =
                Plonk_checks.domain
                  (module Field.Constant)
                  (Pow_2_roots_of_unity (field1 ds))
                  ~shifts:Common.tick_shifts
                  ~domain_generator:Backend.Tick.Field.domain_generator
              in
              let checked_domain () =
                side_loaded_domains
                  (Vector.map domains
                     ~f:
                       (Domains.map ~f:(fun x ->
                            Domain.Pow_2_roots_of_unity (Field.of_int x) )))
                  (O.of_index (Field.of_int i) ~length:num_rules)
                |> field2
              in
              [%test_eq: Field.Constant.t] d_unchecked#size
                (run (fun () -> (checked_domain ())#size)) ;
              [%test_eq: Field.Constant.t]
                (d_unchecked#vanishing_polynomial pt)
                (run (fun () ->
                     (checked_domain ())#vanishing_polynomial
                       (Field.constant pt) ))
            in
            check Domains.h Domains.h )
    end )

  module Split_evaluations = struct
    open Dlog_plonk_types

    let mask' {Bounded.max; actual} : Boolean.var array =
      let (T max) = Nat.of_int max in
      Vector.to_array (ones_vector (module Impl) ~first_zero:actual max)

    let mask (type n) ~(lengths : (int, n) Vector.t)
        (choice : n One_hot_vector.T(Impl).t) : Boolean.var array =
      let max =
        Option.value_exn
          (List.max_elt ~compare:Int.compare (Vector.to_list lengths))
      in
      let actual = Pseudo.choose (choice, lengths) ~f:Field.of_int in
      mask' {max; actual}

    let last =
      Array.reduce_exn ~f:(fun (b_acc, x_acc) (b, x) ->
          (Boolean.(b_acc ||| b), Field.if_ b ~then_:x ~else_:x_acc) )

    let rec pow x bits_lsb =
      let rec go acc bs =
        match bs with
        | [] ->
            acc
        | b :: bs ->
            let acc = Field.square acc in
            let acc = Field.if_ b ~then_:Field.(x * acc) ~else_:acc in
            go acc bs
      in
      go Field.one (List.rev bits_lsb)

    let mod_max_degree =
      let k = Nat.to_int Backend.Tick.Rounds.n in
      fun d ->
        let d = Number.of_bits (Field.unpack ~length:max_log2_degree d) in
        Number.mod_pow_2 d (`Two_to_the k)

    let combine_split_evaluations' b_plus_8 ~max_quot_size =
      Pcs_batch.combine_split_evaluations ~last
        ~mul:(fun (keep, x) (y : Field.t) -> (keep, Field.(y * x)))
        ~mul_and_add:(fun ~acc ~xi (keep, fx) ->
          Field.if_ keep ~then_:Field.(fx + (xi * acc)) ~else_:acc )
        ~init:(fun (_, fx) -> fx)
        (Common.dlog_pcs_batch b_plus_8 ~max_quot_size)
        ~shifted_pow:
          (Pseudo.Degree_bound.shifted_pow ~crs_max_degree:Max_degree.step)

    let combine_split_evaluations_side_loaded b_plus_8 ~max_quot_size =
      Pcs_batch.combine_split_evaluations ~last
        ~mul:(fun (keep, x) (y : Field.t) -> (keep, Field.(y * x)))
        ~mul_and_add:(fun ~acc ~xi (keep, fx) ->
          Field.if_ keep ~then_:Field.(fx + (xi * acc)) ~else_:acc )
        ~init:(fun (_, fx) -> fx)
        (Common.dlog_pcs_batch b_plus_8 ~max_quot_size)
        ~shifted_pow:(fun deg x -> pow x deg)

    let mask_evals (type n) ~(lengths : (int, n) Vector.t Evals.t)
        (choice : n One_hot_vector.T(Impl).t) (e : Field.t array Evals.t) :
        (Boolean.var * Field.t) array Evals.t =
      Evals.map2 lengths e ~f:(fun lengths e ->
          Array.zip_exn (mask ~lengths choice) e )
  end

  let combined_evaluation (type b b_plus_8) b_plus_8 ~xi ~evaluation_point
      ((without_degree_bound : (_, b_plus_8) Vector.t), with_degree_bound)
      ~max_quot_size =
    let open Field in
    Pcs_batch.combine_split_evaluations ~mul
      ~mul_and_add:(fun ~acc ~xi fx -> fx + (xi * acc))
      ~shifted_pow:
        (Pseudo.Degree_bound.shifted_pow ~crs_max_degree:Max_degree.step)
      ~init:Fn.id ~evaluation_point ~xi
      (Common.dlog_pcs_batch b_plus_8 ~max_quot_size)
      without_degree_bound with_degree_bound

  let absorb_field sponge x = Sponge.absorb sponge (`Field x)

  let pack_scalar_challenge (Pickles_types.Scalar_challenge.Scalar_challenge t)
      =
    Field.pack (Challenge.to_bits t)

  let actual_evaluation (e : (Boolean.var * Field.t) array) (pt : Field.t) :
      Field.t =
    let pt_n =
      let max_degree_log2 = Int.ceil_log2 Max_degree.step in
      let rec go acc i =
        if i = 0 then acc else go (Field.square acc) (i - 1)
      in
      go pt max_degree_log2
    in
    match List.rev (Array.to_list e) with
    | (b, e) :: es ->
        List.fold
          ~init:Field.((b :> t) * e)
          es
          ~f:(fun acc (keep, fx) ->
            Field.if_ keep ~then_:Field.(fx + (pt_n * acc)) ~else_:acc )
    | [] ->
        failwith "empty list"

  open Dlog_plonk_types

  module Opt_sponge = struct
    module Underlying =
      Opt_sponge.Make (Impl) (Step_main_inputs.Sponge.Permutation)

    include S.Bit_sponge.Make (struct
                type t = Boolean.var
              end)
              (struct
                type t = Field.t

                let to_bits t = Step_main_inputs.Unsafe.unpack_unboolean t

                let finalize_discarded = Util.boolean_constrain (module Impl)

                let high_entropy_bits = Step_main_inputs.high_entropy_bits
              end)
              (struct
                type t = Boolean.var * Field.t
              end)
              (Underlying)
  end

  let side_loaded_commitment_lengths ~h =
    let max_lengths =
      Commitment_lengths.of_domains ~max_degree:Max_degree.step
        { h=
            Pow_2_roots_of_unity
              Side_loaded_verification_key.(Domain.log2_size max_domains.h)
        ; x= Pow_2_roots_of_unity 0 }
    in
    Commitment_lengths.generic' ~h ~add:Field.add ~mul:Field.mul ~sub:Field.sub
      ~of_int:Field.of_int
      ~ceil_div_max_degree:
        (let k = Nat.to_int Backend.Tick.Rounds.n in
         assert (Max_degree.step = 1 lsl k) ;
         fun d ->
           let open Number in
           let d = of_bits (Field.unpack ~length:max_log2_degree d) in
           to_var (Number.ceil_div_pow_2 d (`Two_to_the k)))
    |> Evals.map2 max_lengths ~f:(fun max actual ->
           Split_evaluations.mask' {actual; max} )

  let shift =
    Shifted_value.Shift.(
      map ~f:Field.constant (create (module Field.Constant)))

  let%test_unit "endo scalar" =
    SC.test (module Impl) ~endo:Endo.Wrap_inner_curve.scalar

  (* This finalizes the "deferred values" coming from a previous proof over the same field.
   It
   1. Checks that [xi] and [r] where sampled correctly. I.e., by absorbing all the
   evaluation openings and then squeezing.
   2. Checks that the "combined inner product" value used in the elliptic curve part of
   the opening proof was computed correctly, in terms of the evaluation openings and the
   evaluation points.
   3. Check that the "b" value was computed correctly.
   4. Perform the arithmetic checks from marlin. *)
  (* TODO: This needs to handle the fact of variable length evaluations.
   Meaning it needs opt sponge. *)
  let finalize_other_proof (type b num_rules)
      (module Num_input_proofs : Nat.Add.Intf with type n = b)
      ~num_input_proofs
      ~(step_domains :
         [ `Known of (Domains.t, num_rules) Vector.t
         | `Side_loaded of
           ( Field.t Side_loaded_verification_key.Domain.t
             Side_loaded_verification_key.Domains.t
           , num_rules )
           Vector.t ]) ~rules_num_input_proofs
      ~(* TODO: Add "actual num input_proofs" so that proofs don't
   carry around dummy "old bulletproof challenges" *)
      sponge ~(old_bulletproof_challenges : (_, b) Vector.t)
      ({ xi
       ; combined_inner_product
       ; bulletproof_challenges
       ; which_rule
       ; b
       ; plonk } :
        ( _
        , _
        , Field.t Shifted_value.t
        , _
        , _
        , _ )
        Types.Dlog_based.Proof_state.Deferred_values.In_circuit.t) es =
    let open Vector in
    let step_domains, input_domain =
      with_label __LOC__ (fun () ->
          match step_domains with
          | `Known domains ->
              ( `Known domains
              , Pseudo.Domain.to_domain ~shifts ~domain_generator
                  (which_rule, Vector.map domains ~f:Domains.x) )
          | `Side_loaded ds ->
              ( `Side_loaded (side_loaded_domains ds which_rule)
              , (* This has to be the num_input_proofs of this proof system rather than actual num_input_proofs *)
                side_loaded_input_domain
                  ~num_input_proofs:
                    (Side_loaded_verification_key.Num_input_proofs.Checked
                     .to_field
                       (Option.value_exn num_input_proofs)) ) )
    in
    let actual_num_input_proofs =
      Pseudo.choose (which_rule, rules_num_input_proofs) ~f:Fn.id
    in
    let (evals1, x_hat1), (evals2, x_hat2) =
      with_label __LOC__ (fun () ->
          let lengths =
            match step_domains with
            | `Known domains ->
                let hs = map domains ~f:(fun {Domains.h; _} -> h) in
                Commitment_lengths.generic map ~h:(map hs ~f:Domain.size)
                  ~max_degree:Max_degree.step
                |> Evals.map ~f:(fun lengths ->
                       Bounded.of_pseudo (which_rule, lengths) )
                |> Evals.map ~f:Split_evaluations.mask'
            | `Side_loaded {h} ->
                side_loaded_commitment_lengths ~h
          in
          Tuple_lib.Double.map es ~f:(fun (e, x) ->
              (Evals.map2 lengths e ~f:Array.zip_exn, x) ) )
    in
    let T = Num_input_proofs.eq in
    (* You use the NEW bulletproof challenges to check b. Not the old ones. *)
    let open Field in
    let absorb_evals x_hat e =
      with_label __LOC__ (fun () ->
          let xs, ys = Evals.to_vectors e in
          List.iter
            Vector.([|(Boolean.true_, x_hat)|] :: (to_list xs @ to_list ys))
            ~f:(Array.iter ~f:(fun (b, x) -> Opt_sponge.absorb sponge (b, x)))
      )
    in
    (* A lot of hashing. *)
    absorb_evals x_hat1 evals1 ;
    absorb_evals x_hat2 evals2 ;
    let squeeze () =
      let x = Opt_sponge.squeeze sponge ~length:Challenge.length in
      Util.boolean_constrain (module Impl) x ;
      x
    in
    let xi_actual = squeeze () in
    let r_actual = squeeze () in
    let xi_correct = equal (pack xi_actual) (pack_scalar_challenge xi) in
    let scalar =
      SC.to_field_checked (module Impl) ~endo:Endo.Wrap_inner_curve.scalar
    in
    let plonk =
      Types.Pairing_based.Proof_state.Deferred_values.Plonk.In_circuit
      .map_challenges ~f:Field.pack ~scalar plonk
    in
    let domain =
      match step_domains with
      | `Known ds ->
          let hs = map ds ~f:(fun {Domains.h; _} -> h) in
          Pseudo.Domain.to_domain (which_rule, hs) ~shifts ~domain_generator
      | `Side_loaded {h} ->
          (h :> _ Plonk_checks.plonk_domain)
    in
    let zetaw = Field.mul domain#generator plonk.zeta in
    let xi = scalar xi in
    let r = scalar (Scalar_challenge r_actual) in
    let combined_inner_product_correct =
      (* sum_i r^i sum_j xi^j f_j(beta_i) *)
      let actual_combined_inner_product =
        let sg_olds =
          Vector.map old_bulletproof_challenges ~f:(fun chals ->
              unstage (b_poly (Vector.to_array chals)) )
        in
        let max_quot_size =
          match step_domains with
          | `Known step_domains ->
              let open Int in
              `Known
                ( which_rule
                , Vector.map step_domains ~f:(fun x ->
                      Common.max_quot_size_int (Domain.size x.Domains.h) ) )
          | `Side_loaded domains ->
              let conv domain =
                let deg =
                  Common.max_quot_size ~of_int ~mul:( * ) ~sub:( - )
                    domain#size
                in
                let d = Split_evaluations.mod_max_degree deg in
                Number.(
                  to_bits (constant (Field.Constant.of_int Max_degree.step) - d))
              in
              `Side_loaded (conv domains.h)
        in
        let combine pt x_hat e =
          let pi = Num_input_proofs.add Nat.N8.n in
          let a, b =
            Evals.to_vectors (e : (Boolean.var * Field.t) array Evals.t)
          in
          let sg_evals =
            Vector.map2
              (ones_vector
                 (module Impl)
                 ~first_zero:actual_num_input_proofs Num_input_proofs.n)
              sg_olds
              ~f:(fun keep f -> [|(keep, f pt)|])
          in
          let v =
            Vector.append sg_evals ([|(Boolean.true_, x_hat)|] :: a) (snd pi)
          in
          match max_quot_size with
          | `Known max_quot_size ->
              Split_evaluations.combine_split_evaluations' pi ~xi
                ~evaluation_point:pt v b ~max_quot_size
          | `Side_loaded max_quot_size ->
              Split_evaluations.combine_split_evaluations_side_loaded pi ~xi
                ~evaluation_point:pt v b ~max_quot_size
        in
        combine plonk.zeta x_hat1 evals1 + (r * combine zetaw x_hat2 evals2)
      in
      equal
        (Shifted_value.to_field (module Field) ~shift combined_inner_product)
        actual_combined_inner_product
    in
    let bulletproof_challenges =
      compute_challenges ~scalar bulletproof_challenges
    in
    let b_correct =
      let b_poly = unstage (b_poly (Vector.to_array bulletproof_challenges)) in
      let b_actual = b_poly plonk.zeta + (r * b_poly zetaw) in
      let b_used = Shifted_value.to_field (module Field) ~shift b in
      equal b_used b_actual
    in
    let plonk_checks_passed =
      let e = Fn.flip actual_evaluation in
      Plonk_checks.checked
        (module Impl)
        ~endo:(Impl.Field.constant Endo.Step_inner_curve.base)
        ~domain ~shift plonk ~mds:sponge_params.mds
        ( Dlog_plonk_types.Evals.map ~f:(e plonk.zeta) evals1
        , Dlog_plonk_types.Evals.map ~f:(e zetaw) evals2 )
        x_hat1
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

  let hash_me_only (type s) ~index
      (state_to_field_elements : s -> Field.t array) =
    let open Types.Pairing_based.Proof_state.Me_only in
    let after_index =
      let sponge = Sponge.create sponge_params in
      Array.iter
        (Types.index_to_field_elements
           ~g:
             (fun (z :
                    Inputs.Inner_curve.t
                    Dlog_plonk_types.Poly_comm.Without_degree_bound.t) ->
             Array.concat_map z
               ~f:(Fn.compose List.to_array Inner_curve.to_field_elements) )
           index)
        ~f:(fun x -> Sponge.absorb sponge (`Field x)) ;
      sponge
    in
    stage (fun (t : _ Types.Pairing_based.Proof_state.Me_only.t) ->
        let sponge = Sponge.copy after_index in
        Array.iter
          ~f:(fun x -> Sponge.absorb sponge (`Field x))
          (to_field_elements_without_index t ~app_state:state_to_field_elements
             ~g:Inner_curve.to_field_elements) ;
        Sponge.squeeze_field sponge )

  let hash_me_only_opt (type s) ~index
      (state_to_field_elements : s -> Field.t array) =
    let open Types.Pairing_based.Proof_state.Me_only in
    let after_index =
      let sponge = Sponge.create sponge_params in
      Array.iter
        (Types.index_to_field_elements
           ~g:
             (fun (z :
                    Inputs.Inner_curve.t
                    Dlog_plonk_types.Poly_comm.Without_degree_bound.t) ->
             Array.concat_map z
               ~f:(Fn.compose List.to_array Inner_curve.to_field_elements) )
           index)
        ~f:(fun x -> Sponge.absorb sponge (`Field x)) ;
      sponge
    in
    stage (fun t ~num_input_proofs ~max_num_input_proofs ~which_rule ->
        let mask =
          ones_vector
            (module Impl)
            max_num_input_proofs
            ~first_zero:(Pseudo.choose ~f:Fn.id (which_rule, num_input_proofs))
        in
        let sponge = Sponge.copy after_index in
        let t =
          { t with
            old_bulletproof_challenges=
              Vector.map2 mask t.old_bulletproof_challenges ~f:(fun b v ->
                  Vector.map v ~f:(fun x -> `Opt (b, x)) )
          ; sg= Vector.map2 mask t.sg ~f:(fun b g -> (b, g)) }
        in
        let not_opt x = `Not_opt x in
        let hash_inputs =
          to_field_elements_without_index t
            ~app_state:
              (Fn.compose (Array.map ~f:not_opt) state_to_field_elements)
            ~g:(fun (b, g) ->
              List.map
                ~f:(fun x -> `Opt (b, x))
                (Inner_curve.to_field_elements g) )
        in
        match
          Array.fold hash_inputs ~init:(`Not_opt sponge) ~f:(fun acc t ->
              match (acc, t) with
              | `Not_opt sponge, `Not_opt t ->
                  Sponge.absorb sponge (`Field t) ;
                  acc
              | `Not_opt sponge, `Opt t ->
                  let sponge =
                    S.Bit_sponge.make (Opt_sponge.Underlying.of_sponge sponge)
                  in
                  Opt_sponge.absorb sponge t ; `Opt sponge
              | `Opt sponge, `Opt t ->
                  Opt_sponge.absorb sponge t ; acc
              | `Opt _, `Not_opt _ ->
                  assert false )
        with
        | `Not_opt sponge ->
            (* This means there were no optional inputs. *)
            Sponge.squeeze_field sponge
        | `Opt sponge ->
            Opt_sponge.squeeze_field sponge )

  let pack_scalar_challenge (Scalar_challenge c : Scalar_challenge.t) =
    Field.pack (Challenge.to_bits c)

  let verify ~num_input_proofs ~is_base_case ~sg_old
      ~(opening : _ Pickles_types.Dlog_plonk_types.Openings.Bulletproof.t)
      ~messages ~wrap_domain ~wrap_verification_key statement
      (unfinalized :
        ( _
        , _
        , _ Shifted_value.t
        , _
        , _
        , _ )
        Types.Pairing_based.Proof_state.Per_proof.In_circuit.t) =
    let public_input =
      let fp (Shifted_value.Shifted_value x) =
        [|Step_main_inputs.Unsafe.unpack_unboolean x|]
      in
      with_label __LOC__ (fun () ->
          Spec.pack
            (module Impl)
            fp Types.Dlog_based.Statement.In_circuit.spec
            (Types.Dlog_based.Statement.In_circuit.to_data statement) )
    in
    let sponge = Sponge.create sponge_params in
    let { Types.Pairing_based.Proof_state.Deferred_values.xi
        ; combined_inner_product
        ; b } =
      unfinalized.deferred_values
    in
    let ( sponge_digest_before_evaluations_actual
        , (`Success bulletproof_success, bulletproof_challenges_actual) ) =
      let xi =
        Pickles_types.Scalar_challenge.map xi
          ~f:(Field.unpack ~length:Challenge.length)
      in
      incrementally_verify_proof num_input_proofs ~domain:wrap_domain ~xi
        ~verification_key:wrap_verification_key ~sponge ~public_input ~sg_old
        ~combined_inner_product ~advice:{b} ~messages ~openings_proof:opening
        ~plonk:
          ((* Actually no need to boolean constrain here (i.e. in Other_field.to_bits_unsafe) when unpacking
              because the scaling functions boolean constrain the bits.
           *)
           Types.Dlog_based.Proof_state.Deferred_values.Plonk.In_circuit
           .map_fields unfinalized.deferred_values.plonk
             ~f:(Shifted_value.map ~f:Other_field.to_bits_unsafe))
    in
    with_label __LOC__ (fun () ->
        Field.Assert.equal unfinalized.sponge_digest_before_evaluations
          sponge_digest_before_evaluations_actual ;
        Array.iteri
          (Vector.to_array unfinalized.deferred_values.bulletproof_challenges)
          ~f:(fun i c1 ->
            let c2 = bulletproof_challenges_actual.(i) in
            let (Pickles_types.Scalar_challenge.Scalar_challenge c1) =
              c1.Bulletproof_challenge.prechallenge
            in
            let c2 =
              Field.if_ is_base_case ~then_:c1
                ~else_:(match c2.prechallenge with Scalar_challenge c2 -> c2)
            in
            Field.Assert.equal c1 c2 ) ) ;
    bulletproof_success
end

include Make (Step_main_inputs)
