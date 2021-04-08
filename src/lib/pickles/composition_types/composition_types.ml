open Pickles_types
module Scalar_challenge = Pickles_types.Scalar_challenge
module Bulletproof_challenge = Bulletproof_challenge
module Index = Index
module Digest = Digest
module Spec = Spec
open Core_kernel

let index_to_field_elements (k : 'a Plonk_verification_key_evals.t) ~g =
  let [ g1
      ; g2
      ; g3
      ; g4
      ; g5
      ; g6
      ; g7
      ; g8
      ; g9
      ; g10
      ; g11
      ; g12
      ; g13
      ; g14
      ; g15
      ; g16
      ; g17
      ; g18 ] =
    Plonk_verification_key_evals.to_hlist k
  in
  List.map
    [ g1
    ; g2
    ; g3
    ; g4
    ; g5
    ; g6
    ; g7
    ; g8
    ; g9
    ; g10
    ; g11
    ; g12
    ; g13
    ; g14
    ; g15
    ; g16
    ; g17
    ; g18 ]
    ~f:g
  |> Array.concat

module Dlog_based = struct
  module Proof_state = struct
    module Deferred_values = struct
      module Plonk = struct
        module Minimal = struct
          [%%versioned
          module Stable = struct
            module V1 = struct
              type ('challenge, 'scalar_challenge) t =
                { alpha: 'scalar_challenge
                ; beta: 'challenge
                ; gamma: 'challenge
                ; zeta: 'scalar_challenge }
              [@@deriving sexp, compare, yojson, hlist, hash, equal]

              let to_latest = Fn.id
            end
          end]
        end

        open Pickles_types

        module In_circuit = struct
          type ('challenge, 'scalar_challenge, 'fp) t =
            { alpha: 'scalar_challenge
            ; beta: 'challenge
            ; gamma: 'challenge
            ; zeta: 'scalar_challenge
            ; perm0: 'fp
            ; perm1: 'fp
            ; gnrc_l: 'fp
            ; gnrc_r: 'fp
            ; gnrc_o: 'fp
            ; psdn0: 'fp
            ; ecad0: 'fp
            ; vbmul0: 'fp
            ; vbmul1: 'fp
            ; endomul0: 'fp
            ; endomul1: 'fp
            ; endomul2: 'fp }
          [@@deriving sexp, compare, yojson, hlist, hash, equal, fields]

          let map_challenges t ~f ~scalar =
            { t with
              alpha= scalar t.alpha
            ; beta= f t.beta
            ; gamma= f t.gamma
            ; zeta= scalar t.zeta }

          let map_fields t ~f =
            { t with
              perm0= f t.perm0
            ; perm1= f t.perm1
            ; gnrc_l= f t.gnrc_l
            ; gnrc_r= f t.gnrc_r
            ; gnrc_o= f t.gnrc_o
            ; psdn0= f t.psdn0
            ; ecad0= f t.ecad0
            ; vbmul0= f t.vbmul0
            ; vbmul1= f t.vbmul1
            ; endomul0= f t.endomul0
            ; endomul1= f t.endomul1
            ; endomul2= f t.endomul2 }

          let typ (type f fp) ~challenge ~scalar_challenge
              (fp : (fp, _, f) Snarky_backendless.Typ.t) =
            Snarky_backendless.Typ.of_hlistable
              [ Scalar_challenge.typ scalar_challenge
              ; challenge
              ; challenge
              ; Scalar_challenge.typ scalar_challenge
              ; fp
              ; fp
              ; fp
              ; fp
              ; fp
              ; fp
              ; fp
              ; fp
              ; fp
              ; fp
              ; fp
              ; fp ]
              ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
              ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
        end

        let to_minimal (t : _ In_circuit.t) : _ Minimal.t =
          {alpha= t.alpha; beta= t.beta; zeta= t.zeta; gamma= t.gamma}
      end

      [%%versioned
      module Stable = struct
        [@@@no_toplevel_latest_type]

        module V1 = struct
          type ( 'plonk
               , 'scalar_challenge
               , 'fp
               , 'fq
               , 'bulletproof_challenges
               , 'index )
               t =
            { plonk: 'plonk
            ; combined_inner_product: 'fp
            ; b: 'fp
            ; xi: 'scalar_challenge
            ; bulletproof_challenges: 'bulletproof_challenges
            ; which_rule: 'index }
          [@@deriving sexp, compare, yojson, hlist, hash, equal]

          let to_latest = Fn.id
        end
      end]

      type ( 'plonk
           , 'scalar_challenge
           , 'fp
           , 'fq
           , 'bulletproof_challenges
           , 'index )
           t =
            ( 'plonk
            , 'scalar_challenge
            , 'fp
            , 'fq
            , 'bulletproof_challenges
            , 'index )
            Stable.Latest.t =
        { plonk: 'plonk
        ; combined_inner_product: 'fp
        ; b: 'fp
        ; xi: 'scalar_challenge
        ; bulletproof_challenges: 'bulletproof_challenges
        ; which_rule: 'index }
      [@@deriving sexp, compare, yojson, hlist, hash, equal]

      module Minimal = struct
        type ( 'challenge
             , 'scalar_challenge
             , 'fp
             , 'fq
             , 'bulletproof_challenges
             , 'index )
             t =
          ( ('challenge, 'scalar_challenge) Plonk.Minimal.t
          , 'scalar_challenge
          , 'fp
          , 'fq
          , 'bulletproof_challenges
          , 'index )
          Stable.Latest.t
        [@@deriving sexp, compare, yojson, hash, equal]
      end

      let map_challenges
          { plonk
          ; combined_inner_product
          ; b: 'fp
          ; xi
          ; bulletproof_challenges
          ; which_rule } ~f ~scalar =
        { xi= scalar xi
        ; combined_inner_product
        ; b
        ; plonk
        ; bulletproof_challenges
        ; which_rule }

      module In_circuit = struct
        type ( 'challenge
             , 'scalar_challenge
             , 'fp
             , 'fq
             , 'bulletproof_challenges
             , 'index )
             t =
          ( ('challenge, 'scalar_challenge, 'fp) Plonk.In_circuit.t
          , 'scalar_challenge
          , 'fp
          , 'fq
          , 'bulletproof_challenges
          , 'index )
          Stable.Latest.t
        [@@deriving sexp, compare, yojson, hash, equal]

        let to_hlist, of_hlist = (to_hlist, of_hlist)

        let typ (type f fp) ~challenge ~scalar_challenge
            (fp : (fp, _, f) Snarky_backendless.Typ.t) fq index =
          Snarky_backendless.Typ.of_hlistable
            [ Plonk.In_circuit.typ ~challenge ~scalar_challenge fp
            ; fp
            ; fp
            ; Scalar_challenge.typ scalar_challenge
            ; Vector.typ
                (Bulletproof_challenge.typ scalar_challenge)
                Backend.Tick.Rounds.n
            ; index ]
            ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
            ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
      end

      let to_minimal (t : _ In_circuit.t) : _ Minimal.t =
        {t with plonk= Plonk.to_minimal t.plonk}
    end

    module Me_only = struct
      [%%versioned
      module Stable = struct
        module V1 = struct
          type ('g1, 'bulletproof_challenges) t =
            {sg: 'g1; old_bulletproof_challenges: 'bulletproof_challenges}
          [@@deriving sexp, compare, yojson, hlist, hash, equal]
        end
      end]

      let to_field_elements {sg; old_bulletproof_challenges}
          ~g1:g1_to_field_elements =
        Array.concat
          [ Vector.to_array old_bulletproof_challenges
            |> Array.concat_map ~f:Vector.to_array
          ; Array.of_list (g1_to_field_elements sg) ]

      let typ g1 chal ~length =
        Snarky_backendless.Typ.of_hlistable
          [g1; Vector.typ chal length]
          ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
          ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
    end

    [%%versioned
    module Stable = struct
      module V1 = struct
        type ( 'plonk
             , 'scalar_challenge
             , 'fp
             , 'fq
             , 'me_only
             , 'digest
             , 'bp_chals
             , 'index )
             t =
          { deferred_values:
              ( 'plonk
              , 'scalar_challenge
              , 'fp
              , 'fq
              , 'bp_chals
              , 'index )
              Deferred_values.Stable.V1.t
          ; sponge_digest_before_evaluations: 'digest
                (* Not needed by other proof system *)
          ; me_only: 'me_only }
        [@@deriving sexp, compare, yojson, hlist, hash, equal]
      end
    end]

    module Minimal = struct
      type ( 'challenge
           , 'scalar_challenge
           , 'fp
           , 'fq
           , 'me_only
           , 'digest
           , 'bp_chals
           , 'index )
           t =
        ( ('challenge, 'scalar_challenge) Deferred_values.Plonk.Minimal.t
        , 'scalar_challenge
        , 'fp
        , 'fq
        , 'me_only
        , 'digest
        , 'bp_chals
        , 'index )
        Stable.Latest.t
      [@@deriving sexp, compare, yojson, hash, equal]
    end

    module In_circuit = struct
      type ( 'challenge
           , 'scalar_challenge
           , 'fp
           , 'fq
           , 'me_only
           , 'digest
           , 'bp_chals
           , 'index )
           t =
        ( ( 'challenge
          , 'scalar_challenge
          , 'fp )
          Deferred_values.Plonk.In_circuit.t
        , 'scalar_challenge
        , 'fp
        , 'fq
        , 'me_only
        , 'digest
        , 'bp_chals
        , 'index )
        Stable.Latest.t
      [@@deriving sexp, compare, yojson, hash, equal]

      let to_hlist, of_hlist = (to_hlist, of_hlist)

      let typ (type f fp) ~challenge ~scalar_challenge
          (fp : (fp, _, f) Snarky_backendless.Typ.t) fq me_only digest index =
        Snarky_backendless.Typ.of_hlistable
          [ Deferred_values.In_circuit.typ ~challenge ~scalar_challenge fp fq
              index
          ; digest
          ; me_only ]
          ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
          ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
    end

    let to_minimal (t : _ In_circuit.t) : _ Minimal.t =
      {t with deferred_values= Deferred_values.to_minimal t.deferred_values}
  end

  module Pass_through = struct
    type ('g, 's, 'sg, 'bulletproof_challenges) t =
      { app_state: 's
      ; dlog_plonk_index:
          'g Dlog_plonk_types.Poly_comm.Without_degree_bound.t
          Plonk_verification_key_evals.t
      ; sg: 'sg
      ; old_bulletproof_challenges: 'bulletproof_challenges }
    [@@deriving sexp]

    let to_field_elements
        {app_state; dlog_plonk_index; sg; old_bulletproof_challenges}
        ~app_state:app_state_to_field_elements ~comm ~g =
      Array.concat
        [ index_to_field_elements ~g:comm dlog_plonk_index
        ; app_state_to_field_elements app_state
        ; Array.of_list (List.concat_map ~f:g (Vector.to_list sg))
        ; Vector.to_array old_bulletproof_challenges
          |> Array.concat_map ~f:Vector.to_array ]

    let to_field_elements_without_index
        {app_state; dlog_plonk_index= _; sg; old_bulletproof_challenges}
        ~app_state:app_state_to_field_elements ~g =
      Array.concat
        [ app_state_to_field_elements app_state
        ; Array.of_list (List.concat_map ~f:g (Vector.to_list sg))
        ; Vector.to_array old_bulletproof_challenges
          |> Array.concat_map ~f:Vector.to_array ]

    open Snarky_backendless.H_list

    let to_hlist {app_state; dlog_plonk_index; sg; old_bulletproof_challenges}
        =
      [app_state; dlog_plonk_index; sg; old_bulletproof_challenges]

    let of_hlist
        ([app_state; dlog_plonk_index; sg; old_bulletproof_challenges] :
          (unit, _) t) =
      {app_state; dlog_plonk_index; sg; old_bulletproof_challenges}

    let typ comm g s chal num_input_proofs =
      Snarky_backendless.Typ.of_hlistable
        [ s
        ; Plonk_verification_key_evals.typ comm
        ; Vector.typ g num_input_proofs
        ; chal ]
        (* TODO: Should this really just be a vector typ of length Rounds.n ?*)
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
  end

  module Statement = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ( 'plonk
             , 'scalar_challenge
             , 'fp
             , 'fq
             , 'me_only
             , 'digest
             , 'pass_through
             , 'bp_chals
             , 'index )
             t =
          { proof_state:
              ( 'plonk
              , 'scalar_challenge
              , 'fp
              , 'fq
              , 'me_only
              , 'digest
              , 'bp_chals
              , 'index )
              Proof_state.Stable.V1.t
          ; pass_through: 'pass_through }
        [@@deriving compare, yojson, sexp, hash, equal]
      end
    end]

    module Minimal = struct
      [%%versioned
      module Stable = struct
        module V1 = struct
          type ( 'challenge
               , 'scalar_challenge
               , 'fp
               , 'fq
               , 'me_only
               , 'digest
               , 'pass_through
               , 'bp_chals
               , 'index )
               t =
            ( ( 'challenge
              , 'scalar_challenge )
              Proof_state.Deferred_values.Plonk.Minimal.Stable.V1.t
            , 'scalar_challenge
            , 'fp
            , 'fq
            , 'me_only
            , 'digest
            , 'pass_through
            , 'bp_chals
            , 'index )
            Stable.V1.t
          [@@deriving compare, yojson, sexp, hash, equal]
        end
      end]
    end

    module In_circuit = struct
      type ( 'challenge
           , 'scalar_challenge
           , 'fp
           , 'fq
           , 'me_only
           , 'digest
           , 'pass_through
           , 'bp_chals
           , 'index )
           t =
        ( ( 'challenge
          , 'scalar_challenge
          , 'fp )
          Proof_state.Deferred_values.Plonk.In_circuit.t
        , 'scalar_challenge
        , 'fp
        , 'fq
        , 'me_only
        , 'digest
        , 'pass_through
        , 'bp_chals
        , 'index )
        Stable.Latest.t
      [@@deriving compare, yojson, sexp, hash, equal]

      let spec =
        let open Spec in
        Struct
          [ Vector (B Field, Nat.N14.n)
          ; Vector (B Challenge, Nat.N2.n)
          ; Vector (Scalar Challenge, Nat.N3.n)
          ; Vector (B Digest, Nat.N3.n)
          ; Vector (B Bulletproof_challenge, Backend.Tick.Rounds.n)
          ; Vector (B Index, Nat.N1.n) ]

      let to_data
          ({ proof_state=
               { deferred_values=
                   { xi
                   ; combined_inner_product
                   ; b
                   ; which_rule
                   ; bulletproof_challenges
                   ; plonk=
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
                       ; endomul2 } }
               ; sponge_digest_before_evaluations
               ; me_only }
           ; pass_through } :
            _ t) =
        let open Vector in
        let fp =
          [ combined_inner_product
          ; b
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
          ; endomul2 ]
        in
        let challenge = [beta; gamma] in
        let scalar_challenge = [alpha; zeta; xi] in
        let digest =
          [sponge_digest_before_evaluations; me_only; pass_through]
        in
        let index = [which_rule] in
        Hlist.HlistId.
          [ fp
          ; challenge
          ; scalar_challenge
          ; digest
          ; bulletproof_challenges
          ; index ]

      let of_data
          Hlist.HlistId.
            [ fp
            ; challenge
            ; scalar_challenge
            ; digest
            ; bulletproof_challenges
            ; index ] : _ t =
        let open Vector in
        let [ combined_inner_product
            ; b
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
            ; endomul2 ] =
          fp
        in
        let [beta; gamma] = challenge in
        let [alpha; zeta; xi] = scalar_challenge in
        let [sponge_digest_before_evaluations; me_only; pass_through] =
          digest
        in
        let [which_rule] = index in
        { proof_state=
            { deferred_values=
                { xi
                ; combined_inner_product
                ; b
                ; which_rule
                ; bulletproof_challenges
                ; plonk=
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
                    ; endomul2 } }
            ; sponge_digest_before_evaluations
            ; me_only }
        ; pass_through }
    end

    let to_minimal (t : _ In_circuit.t) : _ Minimal.t =
      {t with proof_state= Proof_state.to_minimal t.proof_state}
  end
end

module Pairing_based = struct
  module Plonk_polys = Vector.Nat.N10

  module Openings = struct
    module Evaluations = struct
      module By_point = struct
        type 'fq t = {beta_1: 'fq; beta_2: 'fq; beta_3: 'fq; g_challenge: 'fq}
      end

      type 'fq t = ('fq By_point.t, Plonk_polys.n Vector.s) Vector.t
    end

    module Bulletproof = struct
      include Dlog_plonk_types.Openings.Bulletproof

      module Advice = struct
        (* This is data that can be computed in linear time from the above plus the statement.
        
          It doesn't need to be sent on the wire, but it does need to be provided to the verifier
        *)
        type ('fq, 'g) t = {b: 'fq} [@@deriving hlist]

        let typ fq g =
          let open Snarky_backendless.Typ in
          of_hlistable [fq] ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
            ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
      end
    end

    type ('fq, 'g) t =
      {evaluations: 'fq Evaluations.t; proof: ('fq, 'g) Bulletproof.t}
  end

  module Proof_state = struct
    module Deferred_values = struct
      module Plonk = Dlog_based.Proof_state.Deferred_values.Plonk

      type ('plonk, 'scalar_challenge, 'fq, 'bulletproof_challenges) t_ =
        { plonk: 'plonk
        ; combined_inner_product: 'fq
        ; xi: 'scalar_challenge (* 128 bits *)
        ; bulletproof_challenges: 'bulletproof_challenges
        ; b: 'fq }
      [@@deriving sexp, compare, yojson]

      module Minimal = struct
        type ('challenge, 'scalar_challenge, 'fq, 'bulletproof_challenges) t =
          ( ('challenge, 'scalar_challenge) Plonk.Minimal.t
          , 'scalar_challenge
          , 'fq
          , 'bulletproof_challenges )
          t_
        [@@deriving sexp, compare, yojson]
      end

      module In_circuit = struct
        type ('challenge, 'scalar_challenge, 'fq, 'bulletproof_challenges) t =
          ( ('challenge, 'scalar_challenge, 'fq) Plonk.In_circuit.t
          , 'scalar_challenge
          , 'fq
          , 'bulletproof_challenges )
          t_
        [@@deriving sexp, compare, yojson]
      end
    end

    module Pass_through = Dlog_based.Proof_state.Me_only
    module Me_only = Dlog_based.Pass_through

    module Per_proof = struct
      type ( 'plonk
           , 'scalar_challenge
           , 'fq
           , 'bulletproof_challenges
           , 'digest
           , 'bool )
           t_ =
        { deferred_values:
            ( 'plonk
            , 'scalar_challenge
            , 'fq
            , 'bulletproof_challenges )
            Deferred_values.t_
        ; should_finalize: 'bool
        ; sponge_digest_before_evaluations: 'digest }
      [@@deriving sexp, compare, yojson]

      module Minimal = struct
        type ( 'challenge
             , 'scalar_challenge
             , 'fq
             , 'bulletproof_challenges
             , 'digest
             , 'bool )
             t =
          ( ('challenge, 'scalar_challenge) Deferred_values.Plonk.Minimal.t
          , 'scalar_challenge
          , 'fq
          , 'bulletproof_challenges
          , 'digest
          , 'bool )
          t_
        [@@deriving sexp, compare, yojson]
      end

      module In_circuit = struct
        type ( 'challenge
             , 'scalar_challenge
             , 'fq
             , 'bulletproof_challenges
             , 'digest
             , 'bool )
             t =
          ( ( 'challenge
            , 'scalar_challenge
            , 'fq )
            Deferred_values.Plonk.In_circuit.t
          , 'scalar_challenge
          , 'fq
          , 'bulletproof_challenges
          , 'digest
          , 'bool )
          t_
        [@@deriving sexp, compare, yojson]

        let spec bp_log2 =
          let open Spec in
          Struct
            [ Vector (B Field, Nat.N14.n)
            ; Vector (B Digest, Nat.N1.n)
            ; Vector (B Challenge, Nat.N2.n)
            ; Vector (Scalar Challenge, Nat.N3.n)
            ; Vector (B Bulletproof_challenge, bp_log2)
            ; Vector (B Bool, Nat.N1.n) ]

        let to_data
            ({ deferred_values=
                 { xi
                 ; bulletproof_challenges
                 ; b
                 ; combined_inner_product
                 ; plonk=
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
                     ; endomul2 } }
             ; should_finalize
             ; sponge_digest_before_evaluations } :
              _ t) =
          let open Vector in
          let fq =
            [ combined_inner_product
            ; b
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
            ; endomul2 ]
          in
          let challenge = [beta; gamma] in
          let scalar_challenge = [alpha; zeta; xi] in
          let digest = [sponge_digest_before_evaluations] in
          let bool = [should_finalize] in
          let open Hlist.HlistId in
          [ fq
          ; digest
          ; challenge
          ; scalar_challenge
          ; bulletproof_challenges
          ; bool ]

        let of_data
            Hlist.HlistId.
              [ Vector.
                  [ combined_inner_product
                  ; b
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
                  ; endomul2 ]
              ; Vector.[sponge_digest_before_evaluations]
              ; Vector.[beta; gamma]
              ; Vector.[alpha; zeta; xi]
              ; bulletproof_challenges
              ; Vector.[should_finalize] ] : _ t =
          { deferred_values=
              { xi
              ; bulletproof_challenges
              ; b
              ; combined_inner_product
              ; plonk=
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
                  ; endomul2 } }
          ; should_finalize
          ; sponge_digest_before_evaluations }
      end
    end

    type ('unfinalized_proofs, 'me_only) t =
      {unfinalized_proofs: 'unfinalized_proofs; me_only: 'me_only}
    [@@deriving sexp, compare, yojson]

    let spec unfinalized_proofs me_only =
      let open Spec in
      Struct [unfinalized_proofs; me_only]

    include struct
      open Hlist.HlistId

      let to_data {unfinalized_proofs; me_only} =
        [Vector.map unfinalized_proofs ~f:Per_proof.In_circuit.to_data; me_only]

      let of_data [unfinalized_proofs; me_only] =
        { unfinalized_proofs=
            Vector.map unfinalized_proofs ~f:Per_proof.In_circuit.of_data
        ; me_only }
    end

    let typ impl num_input_proofs fq :
        ( ((_, _) Vector.t, _) t
        , ((_, _) Vector.t, _) t
        , _ )
        Snarky_backendless.Typ.t =
      let unfinalized_proofs =
        let open Spec in
        Vector
          (Per_proof.In_circuit.spec Backend.Tock.Rounds.n, num_input_proofs)
      in
      spec unfinalized_proofs (B Spec.Digest)
      |> Spec.typ impl fq ~challenge:`Constrained
           ~scalar_challenge:`Unconstrained
      |> Snarky_backendless.Typ.transport ~there:to_data ~back:of_data
      |> Snarky_backendless.Typ.transport_var ~there:to_data ~back:of_data
  end

  module Statement = struct
    type ('unfinalized_proofs, 'me_only, 'pass_through) t =
      { proof_state: ('unfinalized_proofs, 'me_only) Proof_state.t
      ; pass_through: 'pass_through }
    [@@deriving sexp, compare, yojson]

    let to_data {proof_state= {unfinalized_proofs; me_only}; pass_through} =
      let open Hlist.HlistId in
      [ Vector.map unfinalized_proofs
          ~f:Proof_state.Per_proof.In_circuit.to_data
      ; me_only
      ; pass_through ]

    let of_data Hlist.HlistId.[unfinalized_proofs; me_only; pass_through] =
      { proof_state=
          { unfinalized_proofs=
              Vector.map unfinalized_proofs
                ~f:Proof_state.Per_proof.In_circuit.of_data
          ; me_only }
      ; pass_through }

    let spec num_input_proofs bp_log2 =
      let open Spec in
      let per_proof = Proof_state.Per_proof.In_circuit.spec bp_log2 in
      Struct
        [ Vector (per_proof, num_input_proofs)
        ; B Digest
        ; Vector (B Digest, num_input_proofs) ]
  end
end

module Nvector = Vector.With_length
module Wrap_bp_vec = Backend.Tock.Rounds_vector
module Step_bp_vec = Backend.Tick.Rounds_vector

module Challenges_vector = struct
  type 'n t =
    (Backend.Tock.Field.t Snarky_backendless.Cvar.t Wrap_bp_vec.t, 'n) Vector.t

  module Constant = struct
    type 'n t = (Backend.Tock.Field.t Wrap_bp_vec.t, 'n) Vector.t
  end
end
