open Pickles_types

type t =
  ( G1.Affine.t
  , Fp.t
  , (G1.Affine.t, Fp.t) Pairing_marlin_types.Openings.t )
  Pairing_marlin_types.Proof.t
[@@deriving bin_io]

let to_backend vk primary_input
    ({ messages=
         { w_hat= w_comm
         ; z_hat_a= za_comm
         ; z_hat_b= zb_comm
         ; gh_1= (g1_comm_0, g1_comm_1), h1_comm
         ; sigma_gh_2= sigma2, ((g2_comm_0, g2_comm_1), h2_comm)
         ; sigma_gh_3= sigma3, ((g3_comm_0, g3_comm_1), h3_comm) }
     ; openings=
         { proofs= proof1, proof2, proof3
         ; evals=
             { w_hat= w
             ; z_hat_a= za
             ; z_hat_b= zb
             ; h_1= h1
             ; h_2= h2
             ; h_3= h3
             ; g_1= g1
             ; g_2= g2
             ; g_3= g3
             ; row= {a= row_0; b= row_1; c= row_2}
             ; col= {a= col_0; b= col_1; c= col_2}
             ; value= {a= val_0; b= val_1; c= val_2}
             ; rc= {a= rc_0; b= rc_1; c= rc_2} } } } :
      t) =
  let g (a, b) =
    let open Snarky_bn382.G1.Affine in
    let t = create a b in
    Caml.Gc.finalise delete t ; t
  in
  let t =
    Snarky_bn382.Fp_proof.make vk primary_input (g w_comm) (g za_comm) (g zb_comm)
      (g h1_comm) (g g1_comm_0) (g g1_comm_1) (g h2_comm) (g g2_comm_0)
      (g g2_comm_1) (g h3_comm) (g g3_comm_0) (g g3_comm_1) (g proof1)
      (g proof2) (g proof3) sigma2 sigma3 w za zb h1 g1 h2 g2 h3 g3 row_0 row_1
      row_2 col_0 col_1 col_2 val_0 val_1 val_2 rc_0 rc_1 rc_2
  in
  Caml.Gc.finalise Snarky_bn382.Fp_proof.delete t ;
  t

let of_backend t : t =
  let g1 f =
    let aff = f t in
    let res = G1.Affine.of_backend aff in
    Snarky_bn382.G1.Affine.delete aff ;
    res
  in
  let fp' x =
    Caml.Gc.finalise Snarky_bn382.Fp.delete x ;
    x
  in
  let fp f =
    let res = f t in
    Caml.Gc.finalise Snarky_bn382.Fp.delete res ;
    res
  in
  let open Snarky_bn382.Fp_proof in
  let row_evals = row_evals_nocopy t in
  let col_evals = col_evals_nocopy t in
  let val_evals = val_evals_nocopy t in
  let rc_evals = rc_evals_nocopy t in
  let open Evals in
  let open Tuple_lib in
  let abc evals =
    {Abc.a= fp' (f0 evals); b= fp' (f1 evals); c= fp' (f2 evals)}
  in
  let g_i f =
    let g_i = f t in
    let t =
      Commitment_with_degree_bound.(f0 g_i, f1 g_i)
      |> Tuple_lib.Double.map ~f:G1.Affine.of_backend
    in
    t
  in
  { messages=
      { w_hat= g1 w_comm
      ; z_hat_a= g1 za_comm
      ; z_hat_b= g1 zb_comm
      ; gh_1= (g_i g1_comm_nocopy, g1 h1_comm)
      ; sigma_gh_2= (fp sigma2, (g_i g2_comm_nocopy, g1 h2_comm))
      ; sigma_gh_3= (fp sigma3, (g_i g3_comm_nocopy, g1 h3_comm)) }
  ; openings=
      { proofs= Triple.map ~f:g1 (proof1, proof2, proof3)
      ; evals=
          { w_hat= fp w_eval
          ; z_hat_a= fp za_eval
          ; z_hat_b= fp zb_eval
          ; h_1= fp h1_eval
          ; h_2= fp h2_eval
          ; h_3= fp h3_eval
          ; g_1= fp g1_eval
          ; g_2= fp g2_eval
          ; g_3= fp g3_eval
          ; row= abc row_evals
          ; col= abc col_evals
          ; value= abc val_evals
          ; rc= abc rc_evals } } }

type message = unit

let create ?message:_ pk ~primary ~auxiliary =
  let res = Snarky_bn382.Fp_proof.create pk primary auxiliary in
  let t = of_backend res in
  Snarky_bn382.Fp_proof.delete res ;
  t

let verify ?message:_ proof vk primary =
  let t = to_backend vk primary proof in
  Snarky_bn382.Fp_proof.verify vk t
