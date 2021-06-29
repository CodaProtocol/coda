open Core_kernel

let padded_array_typ ~length ~dummy elt =
  let typ = Snarky_backendless.Typ.array ~length elt in
  { typ with
    store=
      (fun a ->
        let n = Array.length a in
        if n > length then failwithf "Expected %d <= %d" n length () ;
        typ.store (Array.append a (Array.create ~len:(length - n) dummy)) ) }

module Pc_array = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = 'a array [@@deriving compare, sexp, yojson, equal]

      let hash_fold_t f s a = List.hash_fold_t f s (Array.to_list a)
    end
  end]

  let hash_fold_t f s a = List.hash_fold_t f s (Array.to_list a)
end

module Evals = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t =
        {l: 'a; r: 'a; o: 'a; z: 'a; t: 'a; f: 'a; sigma1: 'a; sigma2: 'a}
      [@@deriving fields, sexp, compare, yojson, hash, equal]
    end
  end]

  let map (type a b) ({l; r; o; z; t; f= f'; sigma1; sigma2} : a t)
      ~(f : a -> b) : b t =
    { l= f l
    ; r= f r
    ; o= f o
    ; z= f z
    ; t= f t
    ; f= f f'
    ; sigma1= f sigma1
    ; sigma2= f sigma2 }

  let map2 (type a b c) (t1 : a t) (t2 : b t) ~(f : a -> b -> c) : c t =
    { l= f t1.l t2.l
    ; r= f t1.r t2.r
    ; o= f t1.o t2.o
    ; z= f t1.z t2.z
    ; t= f t1.t t2.t
    ; f= f t1.f t2.f
    ; sigma1= f t1.sigma1 t2.sigma1
    ; sigma2= f t1.sigma2 t2.sigma2 }

  let to_vectors {l; r; o; z; t; f; sigma1; sigma2} =
    (Vector.[l; r; o; z; f; sigma1; sigma2], Vector.[t])

  let of_vectors
      (([l; r; o; z; f; sigma1; sigma2] : ('a, _) Vector.t), Vector.[t]) : 'a t
      =
    {l; r; o; z; t; f; sigma1; sigma2}

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
  module Bulletproof = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('g, 'fq) t =
          { lr: ('g * 'g) Pc_array.Stable.V1.t
          ; z_1: 'fq
          ; z_2: 'fq
          ; delta: 'g
          ; sg: 'g }
        [@@deriving sexp, compare, yojson, hash, equal, hlist]
      end
    end]

    let typ fq g ~length =
      let open Snarky_backendless.Typ in
      of_hlistable
        [array ~length (g * g); fq; fq; g; g]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'fq, 'fqv) t =
        { proof: ('g, 'fq) Bulletproof.Stable.V1.t
        ; evals: 'fqv Evals.Stable.V1.t * 'fqv Evals.Stable.V1.t }
      [@@deriving sexp, compare, yojson, hash, equal, hlist]
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

module Poly_comm = struct
  module With_degree_bound = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type 'g_opt t =
          {unshifted: 'g_opt Pc_array.Stable.V1.t; shifted: 'g_opt}
        [@@deriving sexp, compare, yojson, hlist, hash, equal]
      end
    end]

    let map {unshifted; shifted} ~f =
      {unshifted= Array.map ~f unshifted; shifted= f shifted}

    let padded_array_typ0 = padded_array_typ

    let padded_array_typ elt ~length ~dummy ~bool =
      let open Snarky_backendless.Typ in
      let typ = array ~length (tuple2 bool elt) in
      { typ with
        store=
          (fun a ->
            let a = Array.map a ~f:(fun x -> (true, x)) in
            let n = Array.length a in
            if n > length then failwithf "Expected %d <= %d" n length () ;
            typ.store
              (Array.append a (Array.create ~len:(length - n) (false, dummy)))
            )
      ; read=
          (fun a ->
            let open Snarky_backendless.Typ_monads.Read.Let_syntax in
            let%map a = typ.read a in
            Array.filter_map a ~f:(fun (b, g) -> if b then Some g else None) )
      }

    let typ (type f g g_var bool_var)
        (g : (g_var, g, f) Snarky_backendless.Typ.t) ~length
        ~dummy_group_element
        ~(bool : (bool_var, bool, f) Snarky_backendless.Typ.t) :
        ((bool_var * g_var) t, g Or_infinity.t t, f) Snarky_backendless.Typ.t =
      let open Snarky_backendless.Typ in
      let g_inf =
        transport (tuple2 bool g)
          ~there:(function
            | Or_infinity.Infinity ->
                (false, dummy_group_element)
            | Finite x ->
                (true, x) )
          ~back:(fun (b, x) -> if b then Infinity else Finite x)
      in
      let arr = padded_array_typ0 ~length ~dummy:Or_infinity.Infinity g_inf in
      of_hlistable [arr; g_inf] ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
        ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
  end

  module Without_degree_bound = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type 'g t = 'g Pc_array.Stable.V1.t
        [@@deriving sexp, compare, yojson, hash, equal]
      end
    end]

    let typ g ~length = Snarky_backendless.Typ.array ~length g
  end
end

module Messages = struct
  open Poly_comm

  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'g_opt) t =
        { l_comm: 'g Without_degree_bound.Stable.V1.t
        ; r_comm: 'g Without_degree_bound.Stable.V1.t
        ; o_comm: 'g Without_degree_bound.Stable.V1.t
        ; z_comm: 'g Without_degree_bound.Stable.V1.t
        ; t_comm: 'g_opt With_degree_bound.Stable.V1.t }
      [@@deriving sexp, compare, yojson, fields, hash, equal, hlist]
    end
  end]

  let typ (type n) g ~dummy ~(commitment_lengths : (int, n) Vector.t Evals.t)
      ~bool =
    let open Snarky_backendless.Typ in
    let {Evals.l; r; o; z; t; _} = commitment_lengths in
    let array ~length elt = padded_array_typ ~dummy ~length elt in
    let wo n = array ~length:(Vector.reduce_exn n ~f:Int.max) g in
    let w n =
      With_degree_bound.typ g
        ~length:(Vector.reduce_exn n ~f:Int.max)
        ~dummy_group_element:dummy ~bool
    in
    of_hlistable
      [wo l; wo r; wo o; wo z; w t]
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
      [@@deriving sexp, compare, yojson, hash, equal]
    end
  end]
end

module Shifts = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'field t =
            'field Marlin_plonk_bindings_types.Plonk_verification_shifts.t =
        {r: 'field; o: 'field}
      [@@deriving sexp, compare, yojson, hash, equal]
    end
  end]

  let map ~f {r; o} = {r= f r; o= f o}
end
