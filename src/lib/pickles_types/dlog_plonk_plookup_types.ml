open Core_kernel

module Evals = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t =
        { l: 'a
        ; r: 'a
        ; o: 'a
        ; q: 'a
        ; p: 'a
        ; z: 'a
        ; t: 'a
        ; f: 'a
        ; sigma1: 'a
        ; sigma2: 'a
        ; sigma3: 'a
        ; sigma4: 'a
        ; lp: 'a
        ; lw: 'a
        ; h1: 'a
        ; h2: 'a
        ; tb: 'a
        }
      [@@deriving fields, sexp, compare, yojson, hash, eq]
    end
  end]

  let map (type a b)
      ({l; r; o; q; p; z; t; f= f'; sigma1; sigma2; sigma3; sigma4; lp; lw; h1; h2; tb;} : a t)
      ~(f : a -> b) : b t =
    { l= f l
    ; r= f r
    ; o= f o
    ; q= f q
    ; p= f p
    ; z= f z
    ; t= f t
    ; f= f f'
    ; sigma1= f sigma1
    ; sigma2= f sigma2
    ; sigma3= f sigma3
    ; sigma4= f sigma4
    ; lp= f lp
    ; lw= f lw
    ; h1= f h1
    ; h2= f h2
    ; tb= f tb
    }

  let map2 (type a b c) (t1 : a t) (t2 : b t) ~(f : a -> b -> c) : c t =
    { l= f t1.l t2.l
    ; r= f t1.r t2.r
    ; o= f t1.o t2.o
    ; q= f t1.q t2.q
    ; p= f t1.p t2.p
    ; z= f t1.z t2.z
    ; t= f t1.t t2.t
    ; f= f t1.f t2.f
    ; sigma1= f t1.sigma1 t2.sigma1
    ; sigma2= f t1.sigma2 t2.sigma2
    ; sigma3= f t1.sigma3 t2.sigma3
    ; sigma4= f t1.sigma4 t2.sigma4
    ; lp= f t1.lp t2.lp
    ; lw= f t1.lw t2.lw
    ; h1= f t1.h1 t2.h1
    ; h2= f t1.h2 t2.h2
    ; tb= f t1.tb t2.tb
    }

  let to_vectors {l; r; o; q; p; z; t; f; sigma1; sigma2; sigma3; sigma4; lp; lw; h1; h2; tb;} =
    (Vector.[l; r; o; q; p; z; f; sigma1; sigma2; sigma3; sigma4; lp; lw; h1; h2; tb;], Vector.[t])

  let of_vectors
      ( ([l; r; o; q; p; z; f; sigma1; sigma2; sigma3; sigma4; lp; lw; h1; h2; tb;] :
          ('a, _) Vector.t)
      , Vector.[t] ) : 'a t =
    {l; r; o; q; p; z; t; f; sigma1; sigma2; sigma3; sigma4; lp; lw; h1; h2; tb;}

  let typ (lengths : int t) (g : ('a, 'b, 'f) Snarky_backendless.Typ.t)
      ~default : ('a array t, 'b array t, 'f) Snarky_backendless.Typ.t =
    let v ls =
      Vector.map ls ~f:(fun length ->
          let t = Snarky_backendless.Typ.array ~length g in
          { t with
            store=
              (fun arr ->
                t.store
                  (Array.append arr
                     (Array.create ~len:(length - Array.length arr) default))
                ) } )
    in
    let t =
      let l1, l2 = to_vectors lengths in
      Snarky_backendless.Typ.tuple2 (Vector.typ' (v l1)) (Vector.typ' (v l2))
    in
    Snarky_backendless.Typ.transport t ~there:to_vectors ~back:of_vectors
    |> Snarky_backendless.Typ.transport_var ~there:to_vectors ~back:of_vectors
end

module Openings = struct
  module Bulletproof = Dlog_plonk_types.Openings.Bulletproof

  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'fq, 'fqv) t =
        { proof: ('g, 'fq) Bulletproof.Stable.V1.t
        ; evals: 'fqv Evals.Stable.V1.t * 'fqv Evals.Stable.V1.t }
      [@@deriving sexp, compare, yojson, hash, eq, hlist]
    end
  end]

  let typ (type g gv) (g : (gv, g, 'f) Snarky_backendless.Typ.t) fq
      ~bulletproof_rounds ~commitment_lengths ~dummy_group_element =
    let open Snarky_backendless.Typ in
    let double x = tuple2 x x in
    of_hlistable
      [ Bulletproof.typ fq g ~length:bulletproof_rounds
      ; double (Evals.typ ~default:dummy_group_element commitment_lengths g) ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist
end

module Poly_comm = Dlog_plonk_types.Poly_comm

module Messages = struct
  open Poly_comm

  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'g_opt) t =
        { l_comm: 'g Without_degree_bound.Stable.V1.t
        ; r_comm: 'g Without_degree_bound.Stable.V1.t
        ; o_comm: 'g Without_degree_bound.Stable.V1.t
        ; q_comm: 'g Without_degree_bound.Stable.V1.t
        ; p_comm: 'g Without_degree_bound.Stable.V1.t
        ; z_comm: 'g Without_degree_bound.Stable.V1.t
        ; t_comm: 'g_opt With_degree_bound.Stable.V1.t
        ; lp_comm: 'g Without_degree_bound.Stable.V1.t
        ; lw_comm: 'g Without_degree_bound.Stable.V1.t
        ; h1_comm: 'g Without_degree_bound.Stable.V1.t
        ; h2_comm: 'g Without_degree_bound.Stable.V1.t
        }
      [@@deriving sexp, compare, yojson, fields, hash, eq, hlist]
    end
  end]

  let typ (type n) g ~dummy ~(commitment_lengths : (int, n) Vector.t Evals.t)
      ~bool =
    let open Snarky_backendless.Typ in
    let {Evals.l; r; o; q; p; z; t; lp; lw; h1; h2; _} = commitment_lengths in
    let array ~length elt =
      Dlog_plonk_types.padded_array_typ ~dummy ~length elt
    in
    let wo n = array ~length:(Vector.reduce_exn n ~f:Int.max) g in
    let w n =
      With_degree_bound.typ g
        ~length:(Vector.reduce_exn n ~f:Int.max)
        ~dummy_group_element:dummy ~bool
    in
    of_hlistable
      [wo l; wo r; wo o; wo q; wo p; wo z; w t; wo lp; wo lw; wo h1; wo h2]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist
end

module Proof = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'g_opt, 'fq, 'fqv) t =
        { messages: ('g, 'g_opt) Messages.Stable.V1.t
        ; openings: ('g, 'fq, 'fqv) Openings.Stable.V1.t }
      [@@deriving sexp, compare, yojson, hash, eq]
    end
  end]
end

module Shifts = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'field t =
            'field
            Marlin_plonk_bindings_types.Plonk_5_wires_verification_shifts.t =
        {s0: 'field; s1: 'field; s2: 'field; s3: 'field; s4: 'field}
      [@@deriving sexp, compare, yojson, hash, eq]
    end
  end]

  let map ~f {s0; s1; s2; s3; s4} =
    {s0= f s0; s1= f s1; s2= f s2; s3= f s3; s4= f s4}
end
