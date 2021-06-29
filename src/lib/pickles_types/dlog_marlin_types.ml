open Core_kernel

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
        { w_hat: 'a
        ; z_hat_a: 'a
        ; z_hat_b: 'a
        ; h_1: 'a
        ; h_2: 'a
        ; h_3: 'a
        ; row: 'a Abc.Stable.V1.t
        ; col: 'a Abc.Stable.V1.t
        ; value: 'a Abc.Stable.V1.t
        ; rc: 'a Abc.Stable.V1.t
        ; g_1: 'a
        ; g_2: 'a
        ; g_3: 'a }
      [@@deriving fields, sexp, compare, yojson, hash, equal]
    end
  end]

  let map (type a b)
      ({ w_hat
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
       ; g_3 } :
        a t) ~(f : a -> b) : b t =
    { w_hat= f w_hat
    ; z_hat_a= f z_hat_a
    ; z_hat_b= f z_hat_b
    ; h_1= f h_1
    ; h_2= f h_2
    ; h_3= f h_3
    ; row= {a= f row_a; b= f row_b; c= f row_c}
    ; col= {a= f col_a; b= f col_b; c= f col_c}
    ; value= {a= f value_a; b= f value_b; c= f value_c}
    ; rc= {a= f rc_a; b= f rc_b; c= f rc_c}
    ; g_1= f g_1
    ; g_2= f g_2
    ; g_3= f g_3 }

  let map2 (type a b c) (t1 : a t) (t2 : b t) ~(f : a -> b -> c) : c t =
    { w_hat= f t1.w_hat t2.w_hat
    ; z_hat_a= f t1.z_hat_a t2.z_hat_a
    ; z_hat_b= f t1.z_hat_b t2.z_hat_b
    ; h_1= f t1.h_1 t2.h_1
    ; h_2= f t1.h_2 t2.h_2
    ; h_3= f t1.h_3 t2.h_3
    ; row= Abc.map2 t1.row t2.row ~f
    ; col= Abc.map2 t1.col t2.col ~f
    ; value= Abc.map2 t1.value t2.value ~f
    ; rc= Abc.map2 t1.rc t2.rc ~f
    ; g_1= f t1.g_1 t2.g_1
    ; g_2= f t1.g_2 t2.g_2
    ; g_3= f t1.g_3 t2.g_3 }

  let to_vectors
      { w_hat
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
    Vector.
      ( [ w_hat
        ; z_hat_a
        ; z_hat_b
        ; h_1
        ; h_2
        ; h_3
        ; row_a
        ; row_b
        ; row_c
        ; col_a
        ; col_b
        ; col_c
        ; value_a
        ; value_b
        ; value_c
        ; rc_a
        ; rc_b
        ; rc_c ]
      , [g_1; g_2; g_3] )

  let of_vectors
      (( [ w_hat
         ; z_hat_a
         ; z_hat_b
         ; h_1
         ; h_2
         ; h_3
         ; row_a
         ; row_b
         ; row_c
         ; col_a
         ; col_b
         ; col_c
         ; value_a
         ; value_b
         ; value_c
         ; rc_a
         ; rc_b
         ; rc_c ]
       , [g_1; g_2; g_3] ) :
        ('a, _) Vector.t * ('a, _) Vector.t) : 'a t =
    { w_hat
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
    ; g_3 }

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
        ; evals:
            'fqv Evals.Stable.V1.t
            * 'fqv Evals.Stable.V1.t
            * 'fqv Evals.Stable.V1.t }
      [@@deriving sexp, compare, yojson, hash, equal, hlist]
    end
  end]

  let typ (type g gv) (g : (gv, g, 'f) Snarky_backendless.Typ.t) fq
      ~bulletproof_rounds ~commitment_lengths ~dummy_group_element =
    let open Snarky_backendless.Typ in
    let triple x = tuple3 x x x in
    of_hlistable
      [ Bulletproof.typ fq g ~length:bulletproof_rounds
      ; triple (Evals.typ ~default:dummy_group_element commitment_lengths g) ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist
end

module Poly_comm = struct
  module With_degree_bound = struct
    include Dlog_plonk_types.Poly_comm.With_degree_bound

    let typ ?(array = Snarky_backendless.Typ.array) g ~length =
      Snarky_backendless.Typ.of_hlistable [array ~length g; g]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
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
      type ('g, 'fq) t =
        { w_hat: 'g Without_degree_bound.Stable.V1.t
        ; z_hat_a: 'g Without_degree_bound.Stable.V1.t
        ; z_hat_b: 'g Without_degree_bound.Stable.V1.t
        ; gh_1:
            'g With_degree_bound.Stable.V1.t
            * 'g Without_degree_bound.Stable.V1.t
        ; sigma_gh_2:
            'fq
            * ( 'g With_degree_bound.Stable.V1.t
              * 'g Without_degree_bound.Stable.V1.t )
        ; sigma_gh_3:
            'fq
            * ( 'g With_degree_bound.Stable.V1.t
              * 'g Without_degree_bound.Stable.V1.t ) }
      [@@deriving sexp, compare, yojson, fields, hash, equal, hlist]
    end
  end]

  let typ (type n) fq g ~dummy
      ~(commitment_lengths : (int, n) Vector.t Evals.t) =
    let open Snarky_backendless.Typ in
    let {Evals.w_hat; z_hat_a; z_hat_b; h_1; h_2; h_3; g_1; g_2; g_3; _} =
      commitment_lengths
    in
    let array ~length elt =
      let typ = Snarky_backendless.Typ.array ~length elt in
      { typ with
        store=
          (fun a ->
            let n = Array.length a in
            if n > length then failwithf "Expected %d <= %d" n length () ;
            typ.store (Array.append a (Array.create ~len:(length - n) dummy))
            ) }
    in
    let wo n = array ~length:(Vector.reduce_exn n ~f:Int.max) g in
    let w n =
      With_degree_bound.typ ~array g ~length:(Vector.reduce_exn n ~f:Int.max)
    in
    of_hlistable
      [ wo w_hat
      ; wo z_hat_a
      ; wo z_hat_b
      ; w g_1 * wo h_1
      ; fq * (w g_2 * wo h_2)
      ; fq * (w g_3 * wo h_3) ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist
end

module Proof = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('g, 'fq, 'fqv) t =
        { messages: ('g, 'fq) Messages.Stable.V1.t
        ; openings: ('g, 'fq, 'fqv) Openings.Stable.V1.t }
      [@@deriving sexp, compare, yojson, hash, equal]
    end
  end]
end
