open Core_kernel
open Pickles_types
open Pickles_base
open Tuple_lib
module Domain = Domain

type 'field vanishing_polynomial_domain =
  < vanishing_polynomial: 'field -> 'field >

type 'field plonk_domain =
  < vanishing_polynomial: 'field -> 'field
  ; shifts: 'field Marlin_plonk_bindings.Types.Plonk_verification_shifts.t
  ; generator: 'field
  ; size: 'field >

type 'field domain = < size: 'field ; vanishing_polynomial: 'field -> 'field >

let debug = false

module type Field_intf = sig
  type t

  val size_in_bits : int

  val one : t

  val of_int : int -> t

  val ( * ) : t -> t -> t

  val ( / ) : t -> t -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val negate : t -> t
end

type 'f field = (module Field_intf with type t = 'f)

let map_reduce reduce xs map = List.reduce_exn (List.map xs ~f:map) ~f:reduce

(* x^{2 ^ k} - 1 *)
let vanishing_polynomial (type t) ((module F) : t field) domain x =
  let k = Domain.log2_size domain in
  let rec pow2pow acc i =
    if i = 0 then acc else pow2pow F.(acc * acc) (i - 1)
  in
  F.(pow2pow x k - one)

let domain (type t) ((module F) : t field) ~shifts ~domain_generator
    (domain : Domain.t) : t plonk_domain =
  let size = F.of_int (Domain.size domain) in
  let log2_size = Domain.log2_size domain in
  let shifts = shifts ~log2_size in
  let generator = domain_generator ~log2_size in
  object
    method size = size

    method shifts = shifts

    method vanishing_polynomial x = vanishing_polynomial (module F) domain x

    method generator = generator
  end

let all_but m =
  List.filter Abc.Label.all ~f:(fun label -> not (Abc.Label.equal label m))

let actual_evaluation (type f) (module Field : Field_intf with type t = f)
    (e : Field.t array) (pt : Field.t) ~rounds : Field.t =
  let pt_n =
    let rec go acc i = if i = 0 then acc else go Field.(acc * acc) (i - 1) in
    go pt rounds
  in
  match List.rev (Array.to_list e) with
  | e :: es ->
      List.fold ~init:e es ~f:(fun acc fx -> Field.(fx + (pt_n * acc)))
  | [] ->
      failwith "empty list"

let evals_of_split_evals field ~zeta ~zetaw
    ((es1, es2) : _ Dlog_plonk_types.Evals.t Double.t) ~rounds =
  let e = Fn.flip (actual_evaluation field ~rounds) in
  Dlog_plonk_types.Evals.(map es1 ~f:(e zeta), map es2 ~f:(e zetaw))

open Composition_types.Dlog_based.Proof_state.Deferred_values.Plonk

let derive_plonk (type t) ?(with_label = fun _ (f : unit -> t) -> f ())
    (module F : Field_intf with type t = t) ~shift ~endo ~mds
    ~(domain : t plonk_domain) =
  let module Range = struct
    let psdn = (0, 3)

    let perm = (3, 5)

    let add = (5, 7)

    let endml = (7, 13)

    let mul = (13, 17)
  end in
  let open F in
  let square x = x * x in
  let double x = of_int 2 * x in
  let sbox x =
    (* x^5 *)
    square (square x) * x
  in
  let {Marlin_plonk_bindings.Types.Plonk_verification_shifts.r; o} =
    domain#shifts
  in
  fun ({alpha; beta; gamma; zeta} : _ Minimal.t)
      ((e0, e1) : _ Dlog_plonk_types.Evals.t Double.t) p_eval0 ->
    let alphas =
      let arr =
        let a2 = alpha * alpha in
        let a = Array.init 17 ~f:(fun _ -> a2) in
        for i = 1 to Int.(Array.length a - 1) do
          a.(i) <- with_label __LOC__ (fun () -> a.(Int.(i - 1)) * alpha)
        done ;
        a
      in
      fun (i, j) k ->
        assert (k < j) ;
        arr.(Int.(i + k))
    in
    let bz = beta * zeta in
    let w3, w2, w1 =
      (* generator^{n - 3} *)
      let gen = domain#generator in
      (* gen_inv = gen^{n - 1} = gen^{-1} *)
      let gen_inv = one / gen in
      let w3 = square gen_inv * gen_inv in
      let w2 = gen * w3 in
      (w3, w2, gen * w2)
    in
    let zkp =
      (* Vanishing polynomial of [w1, w2, w3]
          evaluated at x = zeta
      *)
      (zeta - w1) * (zeta - w2) * (zeta - w3)
    in
    let vp_zeta = domain#vanishing_polynomial zeta in
    let perm0, perm1 =
      let perm0 =
        with_label __LOC__ (fun () ->
            (e0.l + bz + gamma)
            * (e0.r + (bz * r) + gamma)
            * (e0.o + (bz * o) + gamma)
            * alpha * zkp
            + (alphas Range.perm 0 * vp_zeta / (zeta - one))
            + (alphas Range.perm 1 * vp_zeta / (zeta - w3)) )
      in
      let perm1 =
        let beta_sigma1 = with_label __LOC__ (fun () -> beta * e0.sigma1) in
        let beta_sigma2 = with_label __LOC__ (fun () -> beta * e0.sigma2) in
        let beta_alpha = with_label __LOC__ (fun () -> beta * alpha) in
        with_label __LOC__ (fun () ->
            negate (e0.l + beta_sigma1 + gamma)
            * (e0.r + beta_sigma2 + gamma)
            * (e1.z * beta_alpha * zkp) )
      in
      (perm0, perm1)
    in
    let gnrc_l = e0.l in
    let gnrc_r = e0.r in
    let gnrc_o = e0.o in
    let psdn0 =
      let lro =
        let s = [|sbox e0.l; sbox e0.r; sbox e0.o|] in
        Array.map mds ~f:(fun m ->
            Array.reduce_exn ~f:F.( + ) (Array.map2_exn s m ~f:F.( * )) )
      in
      with_label __LOC__ (fun () ->
          Array.mapi [|e1.l; e1.r; e1.o|] ~f:(fun i e ->
              (lro.(i) - e) * alphas Range.psdn i )
          |> Array.reduce_exn ~f:( + ) )
    in
    let ecad0 =
      with_label __LOC__ (fun () ->
          (((e1.r - e1.l) * (e0.o + e0.l)) - ((e1.l - e1.o) * (e0.r - e0.l)))
          * alphas Range.add 0
          + ( ((e1.l + e1.r + e1.o) * (e1.l - e1.o) * (e1.l - e1.o))
            - ((e0.o + e0.l) * (e0.o + e0.l)) )
            * alphas Range.add 1 )
    in
    let vbmul0, vbmul1 =
      let tmp = double e0.l - square e0.r + e1.r in
      ( with_label __LOC__ (fun () ->
            ((square e0.r - e0.r) * alphas Range.mul 0)
            + (((e1.l - e0.l) * e1.r) - e1.o + (e0.o * (double e0.r - one)))
              * alphas Range.mul 1 )
      , with_label __LOC__ (fun () ->
            ( square (double e0.o - (tmp * e0.r))
            - ((square e0.r - e1.r + e1.l) * square tmp) )
            * alphas Range.mul 2
            + ( ((e0.l - e1.l) * (double e0.o - (tmp * e0.r)))
              - ((e1.o + e0.o) * tmp) )
              * alphas Range.mul 3 ) )
    in
    let endomul0, endomul1, endomul2 =
      let xr = square e0.r - e0.l - e1.r in
      let t = e0.l - xr in
      let u = double e0.o - (t * e0.r) in
      ( with_label __LOC__ (fun () ->
            ((square e0.l - e0.l) * alphas Range.endml 0)
            + ((square e1.l - e1.l) * alphas Range.endml 1)
            + (e1.r - ((one + (e0.l * (endo - one))) * e0.r))
              * alphas Range.endml 2 )
      , with_label __LOC__ (fun () ->
            (((e1.l - e0.r) * e1.r) - e1.o + (e0.o * (double e0.l - one)))
            * alphas Range.endml 3 )
      , with_label __LOC__ (fun () ->
            (square u - (square t * (xr + e0.l + e1.l)))
            * alphas Range.endml 4
            + (((e0.l - e1.l) * u) - (t * (e0.o + e1.o)))
              * alphas Range.endml 5 ) )
    in
    let linearization_check =
      let w = w3 in
      `Check_equal
        ( ( e0.f + p_eval0
          - (e0.l + (beta * e0.sigma1) + gamma)
            * (e0.r + (beta * e0.sigma2) + gamma)
            * (e0.o + gamma) * e1.z * zkp * alpha
          - (e0.t * vp_zeta) )
          * (zeta - one) * (zeta - w)
        , (vp_zeta * alphas Range.perm 0 * (zeta - w))
          + (vp_zeta * alphas Range.perm 1 * (zeta - one)) )
    in
    ( In_circuit.map_fields
        ~f:(Shifted_value.of_field (module F) ~shift)
        { alpha
        ; beta
        ; gamma
        ; zeta
        ; perm0
        ; perm1
        ; gnrc_l
        ; gnrc_r
        ; gnrc_o
        ; psdn0
        ; ecad0
        ; vbmul0
        ; vbmul1
        ; endomul0
        ; endomul1
        ; endomul2 }
    , linearization_check )

let checked (type t)
    (module Impl : Snarky_backendless.Snark_intf.Run with type field = t)
    ~domain ~shift ~endo ~mds (plonk : _ In_circuit.t) evals p0 =
  let actual, `Check_equal (lin1, lin2) =
    derive_plonk ~with_label:Impl.with_label
      (module Impl.Field)
      ~endo ~mds ~domain ~shift
      { alpha= plonk.alpha
      ; beta= plonk.beta
      ; gamma= plonk.gamma
      ; zeta= plonk.zeta }
      evals p0
  in
  let open Impl in
  let open In_circuit in
  with_label __LOC__ (fun () ->
      Field.equal lin1 lin2
      :: List.map
           ~f:(fun f -> Shifted_value.equal Field.equal (f plonk) (f actual))
           [ perm0
           ; perm1
           ; gnrc_l
           ; gnrc_r
           ; gnrc_o
           ; psdn0
           ; ecad0
           ; vbmul0
           ; vbmul1
           ; endomul0
           ; endomul1
           ; endomul2 ]
      |> Boolean.all )
