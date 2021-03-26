open Core_kernel
open Pickles_types

module type Stable_v1 = sig
  module Stable : sig
    module V1 : sig
      type t [@@deriving version, bin_io, sexp, compare, yojson, hash, eq]
    end

    module Latest = V1
  end

  type t = Stable.V1.t [@@deriving sexp, compare, yojson]
end

module type Inputs_intf = sig
  open Intf

  val id : string

  module Scalar_field : sig
    include Stable_v1

    val one : t

    module Vector : Snarky_intf.Vector.S with type elt = t
  end

  module Base_field : sig
    type t
  end

  module Curve : sig
    module Affine : sig
      include Stable_v1 with type Stable.V1.t = Base_field.t * Base_field.t

      module Backend : sig
        type t = (Base_field.t * Base_field.t) Or_infinity.t
      end

      val of_backend : Backend.t -> t Or_infinity.t

      val to_backend : t Or_infinity.t -> Backend.t
    end
  end

  module Poly_comm : sig
    type t = Curve.Affine.t Poly_comm.t

    module Backend : sig
      type t = Curve.Affine.Backend.t Marlin_plonk_bindings.Types.Poly_comm.t
    end

    val of_backend_with_degree_bound : Backend.t -> t

    val of_backend_without_degree_bound : Backend.t -> t

    val to_backend : t -> Backend.t
  end

  module Opening_proof_backend : sig
    type t =
      ( Scalar_field.t
      , Curve.Affine.Backend.t )
      Marlin_plonk_bindings.Types.Plonk_plookup_proof.Opening_proof.t
  end

  module Evaluations_backend : sig
    type t =
      Scalar_field.t
      Marlin_plonk_bindings.Types.Plonk_plookup_proof.Evaluations.t
  end

  module Index : sig
    type t
  end

  module Verifier_index : sig
    type t
  end

  module Backend : sig
    type t =
      ( Scalar_field.t
      , Curve.Affine.Backend.t
      , Poly_comm.Backend.t )
      Marlin_plonk_bindings_types.Plonk_plookup_proof.t

    val create :
         Index.t
      -> Scalar_field.Vector.t
      -> Scalar_field.Vector.t
      -> Scalar_field.t array
      -> Curve.Affine.Backend.t array
      -> t

    val create_async :
         Index.t
      -> Scalar_field.Vector.t
      -> Scalar_field.Vector.t
      -> Scalar_field.t array
      -> Curve.Affine.Backend.t array
      -> t Async.Deferred.t

    val batch_verify : Verifier_index.t array -> t array -> bool
  end
end

module Challenge_polynomial = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'fq) t = {challenges: 'fq array; commitment: 'g}
      [@@deriving version, bin_io, sexp, compare, yojson]

      let to_latest = Fn.id
    end
  end]
end

module Make (Inputs : Inputs_intf) = struct
  open Inputs
  module Backend = Backend
  module Fq = Scalar_field
  module G = Curve

  module Challenge_polynomial = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          ( G.Affine.Stable.V1.t
          , Fq.Stable.V1.t )
          Challenge_polynomial.Stable.V1.t
        [@@deriving sexp, compare, yojson]

        let to_latest = Fn.id
      end
    end]

    type ('g, 'fq) t_ = ('g, 'fq) Challenge_polynomial.t =
      {challenges: 'fq array; commitment: 'g}
  end

  type message = Challenge_polynomial.t list

  include Allocation_functor.Make.Versioned_v1.Full_compare_eq_hash (struct
    let id = "plong_dlog_proof_" ^ Inputs.id

    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          ( G.Affine.Stable.V1.t
          , G.Affine.Stable.V1.t Or_infinity.Stable.V1.t
          , Fq.Stable.V1.t
          , Fq.Stable.V1.t Dlog_plonk_types.Pc_array.Stable.V1.t )
          Dlog_plonk_plookup_types.Proof.Stable.V1.t
        [@@deriving compare, sexp, yojson, hash, eq]

        let to_latest = Fn.id

        type 'a creator =
             messages:( G.Affine.t
                      , G.Affine.t Or_infinity.t )
                      Dlog_plonk_plookup_types.Messages.Stable.V1.t
          -> openings:( G.Affine.t
                      , Fq.t
                      , Fq.t Dlog_plonk_types.Pc_array.t )
                      Dlog_plonk_plookup_types.Openings.Stable.V1.t
          -> 'a

        let map_creator c ~f ~messages ~openings = f (c ~messages ~openings)

        let create ~messages ~openings =
          let open Dlog_plonk_plookup_types.Proof in
          {messages; openings}
      end
    end]
  end)

  include (
    Stable.Latest :
      sig
        type t [@@deriving compare, sexp, yojson, hash, eq, bin_io]
      end
      with type t := t )

  [%%define_locally
  Stable.Latest.(create)]

  let g t f = G.Affine.of_backend (f t)

  let fq_array_to_vec arr =
    let vec = Fq.Vector.create () in
    Array.iter arr ~f:(fun fe -> Fq.Vector.emplace_back vec fe) ;
    vec

  let opening_proof_of_backend (t : Opening_proof_backend.t) =
    let g x = G.Affine.of_backend x |> Or_infinity.finite_exn in
    let gpair (type a) (t : G.Affine.Backend.t * G.Affine.Backend.t) :
        G.Affine.t * G.Affine.t =
      (g (fst t), g (snd t))
    in
    { Dlog_plonk_plookup_types.Openings.Bulletproof.lr= Array.map ~f:gpair t.lr
    ; z_1= t.z1
    ; z_2= t.z2
    ; delta= g t.delta
    ; sg= g t.sg }

  let of_backend (t : Backend.t) : t =
    let proof = opening_proof_of_backend t.proof in
    let evals =
      (fst t.evals, snd t.evals)
      |> Tuple_lib.Double.map ~f:(fun e ->
             let open Evaluations_backend in
             let l, r, o, q, p = e.w in
             let sigma1, sigma2, sigma3, sigma4 = e.s in
             { Dlog_plonk_plookup_types.Evals.l
             ; r
             ; o
             ; q
             ; p
             ; z= e.z
             ; t= e.t
             ; f= e.f
             ; sigma1
             ; sigma2
             ; sigma3
             ; sigma4
             ; lp= e.lp
             ; lw= e.lw
             ; h1= e.h1
             ; h2= e.h2
             ; tb= e.tb
             })
    in
    let wo x =
      match Poly_comm.of_backend_without_degree_bound x with
      | `Without_degree_bound gs ->
          gs
      | _ ->
          assert false
    in
    let w x =
      match Poly_comm.of_backend_with_degree_bound x with
      | `With_degree_bound t ->
          t
      | _ ->
          assert false
    in
    let l_comm, r_comm, o_comm, q_comm, p_comm = t.messages.w_comm in
    create
      ~messages:
        { l_comm= wo l_comm
        ; r_comm= wo r_comm
        ; o_comm= wo o_comm
        ; q_comm= wo q_comm
        ; p_comm= wo p_comm
        ; z_comm= wo t.messages.z_comm
        ; t_comm= w t.messages.t_comm
        ; lp_comm= wo t.messages.lp_comm
        ; lw_comm= wo t.messages.lw_comm
        ; h1_comm= wo t.messages.h1_comm
        ; h2_comm= wo t.messages.h2_comm
        }
      ~openings:{proof; evals}

  let eval_to_backend
      { Dlog_plonk_plookup_types.Evals.l
      ; r
      ; o
      ; q
      ; p
      ; z
      ; t
      ; f
      ; sigma1= s1
      ; sigma2= s2
      ; sigma3= s3
      ; sigma4= s4
      ; lp
      ; lw
      ; h1
      ; h2
      ; tb
      } : Evaluations_backend.t =
    {w= (l, r, o, q, p); z; t; f; s= (s1, s2, s3, s4); lp; lw; h1; h2; tb}

  let vec_to_array (type t elt)
      (module V : Snarky_intf.Vector.S with type t = t and type elt = elt)
      (v : t) =
    Array.init (V.length v) ~f:(V.get v)

  let to_backend' (chal_polys : Challenge_polynomial.t list) primary_input
      ({ messages= {l_comm; r_comm; o_comm; q_comm; p_comm; z_comm; t_comm; lp_comm; lw_comm; h1_comm; h2_comm}
       ; openings= {proof= {lr; z_1; z_2; delta; sg}; evals= evals0, evals1} } :
        t) : Backend.t =
    let g x = G.Affine.to_backend (Or_infinity.Finite x) in
    let pcw t = Poly_comm.to_backend (`With_degree_bound t) in
    let pcwo t = Poly_comm.to_backend (`Without_degree_bound t) in
    let lr = Array.map lr ~f:(fun (x, y) -> (g x, g y)) in
    { messages=
        { w_comm=
            (pcwo l_comm, pcwo r_comm, pcwo o_comm, pcwo q_comm, pcwo p_comm)
        ; z_comm= pcwo z_comm
        ; t_comm= pcw t_comm
        ; lp_comm= pcwo lp_comm
        ; lw_comm= pcwo lw_comm
        ; h1_comm= pcwo h1_comm
        ; h2_comm= pcwo h2_comm
        }
    ; proof= {lr; delta= g delta; z1= z_1; z2= z_2; sg= g sg}
    ; evals= (eval_to_backend evals0, eval_to_backend evals1)
    ; public= primary_input
    ; prev_challenges=
        Array.of_list_map chal_polys
          ~f:(fun {Challenge_polynomial.commitment; challenges} ->
            ( challenges
            , { Marlin_plonk_bindings.Types.Poly_comm.shifted= None
              ; unshifted= [|Or_infinity.Finite commitment|] } ) ) }

  let to_backend chal_polys primary_input t =
    to_backend' chal_polys (List.to_array primary_input) t

  let create ?message pk ~primary ~auxiliary =
    let chal_polys =
      match (message : message option) with Some s -> s | None -> []
    in
    let challenges =
      List.map chal_polys ~f:(fun {Challenge_polynomial.challenges; _} ->
          challenges )
      |> Array.concat
    in
    let commitments =
      Array.of_list_map chal_polys
        ~f:(fun {Challenge_polynomial.commitment; _} ->
          G.Affine.to_backend (Finite commitment) )
    in
    let res = Backend.create pk primary auxiliary challenges commitments in
    of_backend res

  let create_async ?message pk ~primary ~auxiliary =
    let chal_polys =
      match (message : message option) with Some s -> s | None -> []
    in
    let challenges =
      List.map chal_polys ~f:(fun {Challenge_polynomial.challenges; _} ->
          challenges )
      |> Array.concat
    in
    let commitments =
      Array.of_list_map chal_polys
        ~f:(fun {Challenge_polynomial.commitment; _} ->
          G.Affine.to_backend (Finite commitment) )
    in
    let%map.Async res =
      Backend.create_async pk primary auxiliary challenges commitments
    in
    of_backend res

  let batch_verify' (conv : 'a -> Fq.t array)
      (ts : (Verifier_index.t * t * 'a * message option) list) =
    let vks_and_v =
      Array.of_list_map ts ~f:(fun (vk, t, xs, m) ->
          let p = to_backend' (Option.value ~default:[] m) (conv xs) t in
          (vk, p) )
    in
    Backend.batch_verify
      (Array.map ~f:fst vks_and_v)
      (Array.map ~f:snd vks_and_v)

  let batch_verify = batch_verify' (fun xs -> List.to_array xs)

  let verify ?message t vk xs : bool =
    batch_verify'
      (vec_to_array (module Scalar_field.Vector))
      [(vk, t, xs, message)]
end
