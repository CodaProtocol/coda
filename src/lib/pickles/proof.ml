open Core
open Pickles_types
open Import
open Types
open Common
open Backend

module Base = struct
  module Me_only = Reduced_me_only

  module Pairing_based = struct
    type ( 's
         , 'unfinalized_proofs
         , 'sgs
         , 'bp_chals
         , 'dlog_me_onlys
         , 'prev_evals )
         t =
      { statement:
          ( 'unfinalized_proofs
          , ('s, 'sgs, 'bp_chals) Me_only.Pairing_based.t
          , 'dlog_me_onlys )
          Types.Pairing_based.Statement.t
      ; index: Index.t
      ; prev_evals: 'prev_evals
      ; proof: Tick.Proof.t }
  end

  module Double = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type 'a t = 'a * 'a [@@deriving compare, sexp, yojson, hash, equal]
      end
    end]
  end

  module Dlog_based = struct
    [%%versioned
    module Stable = struct
      [@@@no_toplevel_latest_type]

      module V1 = struct
        type ('dlog_me_only, 'pairing_me_only) t =
          { statement:
              ( Limb_vector.Constant.Hex64.Stable.V1.t
                Vector.Vector_2.Stable.V1.t
              , Limb_vector.Constant.Hex64.Stable.V1.t
                Vector.Vector_2.Stable.V1.t
                Scalar_challenge.Stable.V1.t
              , Tick.Field.Stable.V1.t Shifted_value.Stable.V1.t
              , Tock.Field.Stable.V1.t
              , 'dlog_me_only
              , Digest.Constant.Stable.V1.t
              , 'pairing_me_only
              , Limb_vector.Constant.Hex64.Stable.V1.t
                Vector.Vector_2.Stable.V1.t
                Scalar_challenge.Stable.V1.t
                Bulletproof_challenge.Stable.V1.t
                Step_bp_vec.Stable.V1.t
              , Index.Stable.V1.t )
              Types.Dlog_based.Statement.Minimal.Stable.V1.t
          ; prev_evals:
              Tick.Field.Stable.V1.t Dlog_plonk_types.Pc_array.Stable.V1.t
              Dlog_plonk_types.Evals.Stable.V1.t
              Double.Stable.V1.t
          ; prev_x_hat: Tick.Field.Stable.V1.t Double.Stable.V1.t
          ; proof: Tock.Proof.Stable.V1.t }
        [@@deriving compare, sexp, yojson, hash, equal]
      end
    end]

    type ('dlog_me_only, 'pairing_me_only) t =
          ('dlog_me_only, 'pairing_me_only) Stable.Latest.t =
      { statement:
          ( Challenge.Constant.t
          , Challenge.Constant.t Scalar_challenge.t
          , Tick.Field.t Shifted_value.t
          , Tock.Field.t
          , 'dlog_me_only
          , Digest.Constant.t
          , 'pairing_me_only
          , Challenge.Constant.t Scalar_challenge.t Bulletproof_challenge.t
            Step_bp_vec.t
          , Index.t )
          Types.Dlog_based.Statement.Minimal.t
      ; prev_evals:
          Tick.Field.t Dlog_plonk_types.Pc_array.t Dlog_plonk_types.Evals.t
          Double.t
      ; prev_x_hat: Tick.Field.t Double.t
      ; proof: Tock.Proof.t }
    [@@deriving compare, sexp, yojson, hash, equal]
  end
end

type ('s, 'mlmb, _) with_data =
  | T :
      ( 'mlmb Base.Me_only.Dlog_based.t
      , ( 's
        , (Tock.Curve.Affine.t, 'most_recent_num_input_proofs) Vector.t
        , ( Challenge.Constant.t Scalar_challenge.Stable.Latest.t
            Bulletproof_challenge.t
            Step_bp_vec.t
          , 'most_recent_num_input_proofs )
          Vector.t )
        Base.Me_only.Pairing_based.t )
      Base.Dlog_based.t
      -> ('s, 'mlmb, _) with_data

module With_data = struct
  type ('s, 'mlmb, 'w) t = ('s, 'mlmb, 'w) with_data
end

type ('max_num_input_proofs, 'mlmb) t =
  (unit, 'mlmb, 'max_num_input_proofs) With_data.t

let dummy (type w h r) (_w : w Nat.t) (h : h Nat.t)
    (most_recent_num_input_proofs : r Nat.t) : (w, h) t =
  let open Ro in
  let g0 = Tock.Curve.(to_affine_exn one) in
  let g len = Array.create ~len g0 in
  let tock len = Array.init len ~f:(fun _ -> tock ()) in
  let tick_arr len = Array.init len ~f:(fun _ -> tick ()) in
  let lengths =
    Commitment_lengths.of_domains wrap_domains ~max_degree:Max_degree.wrap
  in
  T
    { statement=
        { proof_state=
            { deferred_values=
                { xi= scalar_chal ()
                ; combined_inner_product= Shifted_value (tick ())
                ; b= Shifted_value (tick ())
                ; which_rule= Option.value_exn (Index.of_int 0)
                ; bulletproof_challenges= Dummy.Ipa.Step.challenges
                ; plonk=
                    { alpha= scalar_chal ()
                    ; beta= chal ()
                    ; gamma= chal ()
                    ; zeta= scalar_chal () } }
            ; sponge_digest_before_evaluations=
                Digest.Constant.of_tock_field Tock.Field.zero
            ; me_only=
                { sg= Lazy.force Dummy.Ipa.Step.sg
                ; old_bulletproof_challenges=
                    Vector.init h ~f:(fun _ -> Dummy.Ipa.Wrap.challenges) } }
        ; pass_through=
            { app_state= ()
            ; old_bulletproof_challenges=
                (* Not sure if this should be w or h honestly ...*)
                Vector.init most_recent_num_input_proofs ~f:(fun _ ->
                    Dummy.Ipa.Step.challenges )
                (* TODO: Should this be wrap? *)
            ; sg=
                Vector.init most_recent_num_input_proofs ~f:(fun _ ->
                    Lazy.force Dummy.Ipa.Wrap.sg ) } }
    ; proof=
        { messages=
            { l_comm= g lengths.l
            ; r_comm= g lengths.r
            ; o_comm= g lengths.o
            ; z_comm= g lengths.z
            ; t_comm=
                { unshifted=
                    Array.map (g lengths.t) ~f:(fun x -> Or_infinity.Finite x)
                ; shifted= Finite g0 } }
        ; openings=
            { proof=
                { lr=
                    Array.init (Nat.to_int Tock.Rounds.n) ~f:(fun _ -> (g0, g0))
                ; z_1= Ro.tock ()
                ; z_2= Ro.tock ()
                ; delta= g0
                ; sg= g0 }
            ; evals=
                (let e () = Dlog_plonk_types.Evals.map lengths ~f:tock in
                 (e (), e ())) } }
    ; prev_evals=
        (let e () = Dlog_plonk_types.Evals.map lengths ~f:tick_arr in
         (e (), e ()))
    ; prev_x_hat= (tick (), tick ()) }

module Make (W : Nat.Intf) (Prev_max_num_input_proofs : Nat.Intf) = struct
  module Max_num_rules_at_most = At_most.With_length (W)
  module Prev_max_num_input_proofs_vec = Nvector (Prev_max_num_input_proofs)

  module Repr = struct
    type t =
      ( ( Tock.Inner_curve.Affine.t
        , Reduced_me_only.Dlog_based.Challenges_vector.t
          Prev_max_num_input_proofs_vec.t )
        Dlog_based.Proof_state.Me_only.t
      , ( unit
        , Tock.Curve.Affine.t Max_num_rules_at_most.t
        , Challenge.Constant.t Scalar_challenge.t Bulletproof_challenge.t
          Step_bp_vec.t
          Max_num_rules_at_most.t )
        Base.Me_only.Pairing_based.t )
      Base.Dlog_based.t
    [@@deriving compare, sexp, yojson, hash, equal]
  end

  type nonrec t = (W.n, Prev_max_num_input_proofs.n) t

  let to_repr (T t) : Repr.t =
    let lte = Nat.lte_exn (Vector.length t.statement.pass_through.sg) W.n in
    { t with
      statement=
        { t.statement with
          pass_through=
            { t.statement.pass_through with
              sg= At_most.of_vector t.statement.pass_through.sg lte
            ; old_bulletproof_challenges=
                At_most.of_vector
                  t.statement.pass_through.old_bulletproof_challenges lte } }
    }

  let of_repr (r : Repr.t) : t =
    let (Vector.T sg) = At_most.to_vector r.statement.pass_through.sg in
    let (Vector.T old_bulletproof_challenges) =
      At_most.to_vector r.statement.pass_through.old_bulletproof_challenges
    in
    let T =
      Nat.eq_exn (Vector.length sg) (Vector.length old_bulletproof_challenges)
    in
    T
      { r with
        statement=
          { r.statement with
            pass_through=
              {r.statement.pass_through with sg; old_bulletproof_challenges} }
      }

  let compare t1 t2 = Repr.compare (to_repr t1) (to_repr t2)

  let equal t1 t2 = Repr.equal (to_repr t1) (to_repr t2)

  let hash_fold_t s t = Repr.hash_fold_t s (to_repr t)

  let hash t = Repr.hash (to_repr t)

  include Sexpable.Of_sexpable
            (Repr)
            (struct
              type nonrec t = t

              let to_sexpable = to_repr

              let of_sexpable = of_repr
            end)

  let to_yojson x = Repr.to_yojson (to_repr x)

  let of_yojson x = Result.map ~f:of_repr (Repr.of_yojson x)
end

module Branching_2 = struct
  module T = Make (Nat.N2) (Nat.N2)

  module Repr = struct
    [%%versioned
    module Stable = struct
      [@@@no_toplevel_latest_type]

      module V1 = struct
        type t =
          ( ( Tock.Inner_curve.Affine.Stable.V1.t
            , Reduced_me_only.Dlog_based.Challenges_vector.Stable.V1.t
              Vector.Vector_2.Stable.V1.t )
            Dlog_based.Proof_state.Me_only.Stable.V1.t
          , ( unit
            , Tock.Curve.Affine.t At_most.At_most_2.Stable.V1.t
            , Limb_vector.Constant.Hex64.Stable.V1.t
              Vector.Vector_2.Stable.V1.t
              Scalar_challenge.Stable.V1.t
              Bulletproof_challenge.Stable.V1.t
              Step_bp_vec.Stable.V1.t
              At_most.At_most_2.Stable.V1.t )
            Base.Me_only.Pairing_based.Stable.V1.t )
          Base.Dlog_based.Stable.V1.t
        [@@deriving compare, sexp, yojson, hash, equal]

        let to_latest = Fn.id
      end
    end]

    include T.Repr

    (* Force the typechecker to verify that these types are equal. *)
    let _ =
      let _f : unit -> (t, Stable.Latest.t) Type_equal.t =
       fun () -> Type_equal.T
      in
      ()
  end

  [%%versioned_binable
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t = T.t

      let to_latest = Fn.id

      include (T : module type of T with type t := t with module Repr := T.Repr)

      include Binable.Of_binable
                (Repr.Stable.V1)
                (struct
                  type nonrec t = t

                  let to_binable = to_repr

                  let of_binable = of_repr
                end)
    end
  end]

  include (T : module type of T with module Repr := T.Repr)
end

module Branching_max = struct
  module T =
    Make
      (Side_loaded_verification_key.Num_input_proofs.Max)
      (Side_loaded_verification_key.Num_input_proofs.Max)

  module Repr = struct
    [%%versioned
    module Stable = struct
      [@@@no_toplevel_latest_type]

      module V1 = struct
        type t =
          ( ( Tock.Inner_curve.Affine.Stable.V1.t
            , Reduced_me_only.Dlog_based.Challenges_vector.Stable.V1.t
              Side_loaded_verification_key.Num_input_proofs.Max_vector.Stable
              .V1
              .t )
            Dlog_based.Proof_state.Me_only.Stable.V1.t
          , ( unit
            , Tock.Curve.Affine.t
              Side_loaded_verification_key.Num_input_proofs.Max_at_most.Stable
              .V1
              .t
            , Limb_vector.Constant.Hex64.Stable.V1.t
              Vector.Vector_2.Stable.V1.t
              Scalar_challenge.Stable.V1.t
              Bulletproof_challenge.Stable.V1.t
              Step_bp_vec.Stable.V1.t
              Side_loaded_verification_key.Num_input_proofs.Max_at_most.Stable
              .V1
              .t )
            Base.Me_only.Pairing_based.Stable.V1.t )
          Base.Dlog_based.Stable.V1.t
        [@@deriving compare, sexp, yojson, hash, equal]

        let to_latest = Fn.id
      end
    end]

    include T.Repr

    (* Force the typechecker to verify that these types are equal. *)
    let _ =
      let _f : unit -> (t, Stable.Latest.t) Type_equal.t =
       fun () -> Type_equal.T
      in
      ()
  end

  [%%versioned_binable
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t = T.t

      let to_latest = Fn.id

      include (T : module type of T with type t := t with module Repr := T.Repr)

      include Binable.Of_binable
                (Repr.Stable.V1)
                (struct
                  type nonrec t = t

                  let to_binable = to_repr

                  let of_binable = of_repr
                end)
    end
  end]

  include (T : module type of T with module Repr := T.Repr)
end
