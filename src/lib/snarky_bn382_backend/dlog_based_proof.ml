open Core_kernel
open Pickles_types

type message = (Fq.t, G.Affine.t) Dlog_marlin_types.Challenge_polynomial.t list

type t = (G.Affine.t, Fq.t, Fq.t array) Dlog_marlin_types.Proof.t [@@deriving bin_io]

let g t f = G.Affine.of_backend (f t)

let fq t f =
  let t = f t in
  Caml.Gc.finalise Fq.delete t ;
  t

let fqv t f =
  let t = f t in
  Caml.Gc.finalise Fq.Vector.delete t ;
  Array.init (Fq.Vector.length t) (fun i -> Fq.Vector.get t i)

let pc t f =
  let t = f t in
  let open Snarky_bn382.Fq_poly_comm in
  let gvec (type a) (t : a) (f : a -> Snarky_bn382.G.Affine.t) : G.Affine.t =
    let t = f t in
    Snarky_bn382.G.Affine.(G.Affine.of_backend t)
  in
  let unshifted =
    let v = unshifted t in
    Array.init (Snarky_bn382.G.Affine.Vector.length v) (fun i ->
        gvec v (fun v -> Snarky_bn382.G.Affine.Vector.get v i) )
  in
  let shifted = shifted t in
  let open Dlog_marlin_types.Poly_comm in
  match shifted with
  | Some g ->
    `With_degree_bound
      { With_degree_bound.unshifted
      ; shifted= G.Affine.of_backend g
      }
  | None -> `Without_degree_bound unshifted

(* TODO: Lots of leakage here. *)
let of_backend (t : Snarky_bn382.Fq_proof.t) : t =
  let open Snarky_bn382.Fq_proof in
  let gpair (type a) (t : a) (f : a -> Snarky_bn382.G.Affine.Pair.t) :
      G.Affine.t * G.Affine.t =
    let t = f t in
    let g = G.Affine.of_backend in
    Snarky_bn382.G.Affine.Pair.(g (f0 t), g (f1 t))
  in
  let proof =
    let t = proof t in
    let fq = fq t in
    let g = g t in
    let open Snarky_bn382.Fq_opening_proof in
    let lr =
      let v = lr t in
      Array.init (Snarky_bn382.G.Affine.Pair.Vector.length v) (fun i ->
          gpair v (fun v -> Snarky_bn382.G.Affine.Pair.Vector.get v i) )
    in
    { Dlog_marlin_types.Openings.Bulletproof.lr
    ; z_1= fq z1
    ; z_2= fq z2
    ; delta= g delta
    ; sg= g sg }
  in
  let evals =
    let t = evals_nocopy t in
    Evaluations.Triple.(f0 t, f1 t, f2 t)
    |> Tuple_lib.Triple.map ~f:(fun e ->
           let open Evaluations in
           let abc trip =
             let t = trip e in
             let fqv = fqv t in
             let open Snarky_bn382.Fq_vector_triple in
             {Abc.a= fqv f0; b= fqv f1; c= fqv f2}
           in
           let fqv = fqv e in
           { Dlog_marlin_types.Evals.w_hat= fqv w
           ; z_hat_a= fqv za
           ; z_hat_b= fqv zb
           ; h_1= fqv h1
           ; h_2= fqv h2
           ; h_3= fqv h3
           ; row= abc row_nocopy
           ; col= abc col_nocopy
           ; value= abc val_nocopy
           ; rc= abc rc_nocopy
           ; g_1= fqv g1
           ; g_2= fqv g2
           ; g_3= fqv g3 } )
  in
  let fq = fq t in
  let pc = pc t in
  let wo x =
    match pc x with
    | `Without_degree_bound gs -> gs
    | _ -> assert false
  in
  let w x =
    match pc x with
    | `With_degree_bound t -> t
    | _ -> assert false
  in
  { messages=
      { w_hat= wo w_comm
      ; z_hat_a= wo za_comm
      ; z_hat_b= wo zb_comm
      ; gh_1= (w g1_comm_nocopy, wo h1_comm)
      ; sigma_gh_2= (fq sigma2, (w g2_comm_nocopy, wo h2_comm))
      ; sigma_gh_3= (fq sigma3, (w g3_comm_nocopy, wo h3_comm)) }
  ; openings= {proof; evals} }

let evalvec arr =
  let open Snarky_bn382.Fq in
  let vec = Snarky_bn382.Fq.Vector.create () in
  Array.iter arr ~f:(fun fe -> Snarky_bn382.Fq.Vector.emplace_back vec fe) ;
  vec

let eval_to_backend
    { Dlog_marlin_types.Evals.w_hat
    ; z_hat_a
    ; z_hat_b
    ; h_1
    ; h_2
    ; h_3
    ; row= {a= row_a; b= row_b; c= row_c}
    ; col= {a= col_a; b= col_b; c= col_c}
    ; value= {a= value_a; b= value_b; c= value_c}
    ; rc= {a= rc_a; b= rc_b; c= rc_c}
    ; g_1
    ; g_2
    ; g_3 } =
  Snarky_bn382.Fq_proof.Evaluations.make (evalvec w_hat) (evalvec z_hat_a)
    (evalvec z_hat_b) (evalvec h_1) (evalvec g_1) (evalvec h_2) (evalvec g_2)
    (evalvec h_3) (evalvec g_3) (evalvec row_a) (evalvec row_b) (evalvec row_c)
    (evalvec col_a) (evalvec col_b) (evalvec col_c) (evalvec value_a)
    (evalvec value_b) (evalvec value_c) (evalvec rc_a) (evalvec rc_b)
    (evalvec rc_c)

let field_vector_of_list xs =
  let v = Fq.Vector.create () in
  List.iter ~f:(Fq.Vector.emplace_back v) xs ;
  v

let to_backend' vk chal_polys primary_input
    ({ messages=
         { w_hat= w_comm
         ; z_hat_a= za_comm
         ; z_hat_b= zb_comm
         ; gh_1= g1_comm, h1_comm
         ; sigma_gh_2= sigma2, (g2_comm, h2_comm)
         ; sigma_gh_3= sigma3, (g3_comm, h3_comm) }
     ; openings=
         {proof= {lr; z_1; z_2; delta; sg}; evals= evals0, evals1, evals2} } :
      t) : Snarky_bn382.Fq_proof.t =
  let g (a, b) =
    let open Snarky_bn382.G.Affine in
    let t = create a b in
    Caml.Gc.finalise delete t ; t
  in
  let pcw (commitment : G.Affine.t Dlog_marlin_types.Poly_comm.With_degree_bound.t) =
    let unsh =
      let v = Snarky_bn382.G.Affine.Vector.create () in
      Array.iter commitment.unshifted ~f:(fun c ->
          (* Very leaky *)
          Snarky_bn382.G.Affine.Vector.emplace_back v (g c) ) ;
      v
    in
    let t = Snarky_bn382.Fq_poly_comm.make unsh (Some (g commitment.shifted)) in
    t
  in
  let pcwo (commitment : G.Affine.t Dlog_marlin_types.Poly_comm.Without_degree_bound.t) =
    let unsh =
      let v = Snarky_bn382.G.Affine.Vector.create () in
      Array.iter commitment ~f:(fun c ->
          (* Very leaky *)
          Snarky_bn382.G.Affine.Vector.emplace_back v (g c) ) ;
      v
    in
    let t = Snarky_bn382.Fq_poly_comm.make unsh None in
    t
  in
  let lr =
    let v = Snarky_bn382.G.Affine.Pair.Vector.create () in
    Array.iter lr ~f:(fun (l, r) ->
        (* Very leaky *)
        Snarky_bn382.G.Affine.Pair.Vector.emplace_back v
          (Snarky_bn382.G.Affine.Pair.make (g l) (g r)) ) ;
    v
  in
  let challenges =
    List.map chal_polys ~f:(fun {Dlog_marlin_types.Challenge_polynomial.challenges; commitment} ->
        challenges )
    |> Array.concat |> Fq.Vector.of_array
  in
  let commitments =
    Array.of_list_map chal_polys
      ~f:(fun {Dlog_marlin_types.Challenge_polynomial.commitment; _} ->
        G.Affine.to_backend commitment )
    |> G.Affine.Vector.of_array
  in
  Snarky_bn382.Fq_proof.make vk primary_input (pcwo w_comm) (pcwo za_comm)
    (pcwo zb_comm) (pcwo h1_comm) (pcw g1_comm) (pcwo h2_comm) (pcw g2_comm)
    (pcwo h3_comm) (pcw g3_comm) sigma2 sigma3 lr z_1 z_2 (g delta) (g sg)
    (* Leaky! *)
    (eval_to_backend evals0)
    (eval_to_backend evals1) (eval_to_backend evals2) challenges commitments

let to_backend vk chal_polys primary_input t =
  to_backend' vk chal_polys (field_vector_of_list primary_input) t

let create ?message pk ~primary ~auxiliary =
  let chal_polys = match message with
    | Some s -> s
    | None -> []
  in
  let challenges =
    List.map chal_polys ~f:(fun {Dlog_marlin_types.Challenge_polynomial.challenges; _} ->
        challenges )
    |> Array.concat |> Fq.Vector.of_array
  in
  let commitments =
    Array.of_list_map chal_polys
      ~f:(fun {Dlog_marlin_types.Challenge_polynomial.commitment; _} ->
        G.Affine.to_backend commitment )
    |> G.Affine.Vector.of_array
  in
  let res =
    Snarky_bn382.Fq_proof.create pk primary auxiliary challenges commitments
  in
  let t = of_backend res in
  Snarky_bn382.Fq_proof.delete res ;
  t

let batch_verify' vk (conv : 'a -> Fq.Vector.t)
    (ts : (t * 'a * message option) list)
    (vk : Snarky_bn382.Fq_verifier_index.t) =
  let open Snarky_bn382.Fq_proof in
  let v = Vector.create () in
  List.iter ts ~f:(fun (t, xs, m) ->
      let p = to_backend' vk (Option.value ~default:[] m) (conv xs) t in
      Vector.emplace_back v p ; delete p ) ;
  let res = batch_verify vk v in
  Vector.delete v ; res

let batch_verify vk = batch_verify' vk field_vector_of_list

let verify ?message t vk xs : bool = batch_verify' vk Fn.id [(t, xs, message)] vk