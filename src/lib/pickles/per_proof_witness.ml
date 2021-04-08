open Backend
open Pickles_types
open Import
module Impl = Impls.Step
module One_hot_vector = One_hot_vector.Make (Impl)

type ('local_statement, 'local_max_num_input_proofs, 'local_num_rules) t =
  'local_statement
  * ( Challenge.Make(Impl).t
    , Challenge.Make(Impl).t Scalar_challenge.t
    , Impl.Field.t Shifted_value.t
    , Pairing_main.Make(Step_main_inputs).Other_field.t
    , unit
    , Digest.Make(Impl).t
    , Challenge.Make(Impl).t Scalar_challenge.t Types.Bulletproof_challenge.t
      Types.Step_bp_vec.t
    , 'local_num_rules One_hot_vector.t )
    Types.Dlog_based.Proof_state.In_circuit.t
  * (Impl.Field.t array Dlog_plonk_types.Evals.t * Impl.Field.t)
    Tuple_lib.Double.t
  * (Step_main_inputs.Inner_curve.t, 'local_max_num_input_proofs) Vector.t
  * ( (Impl.Field.t, Tick.Rounds.n) Vector.t
    , 'local_max_num_input_proofs )
    Vector.t
  * Wrap_proof.var

module Constant = struct
  open Zexe_backend

  type ('local_statement, 'local_max_num_input_proofs, _) t =
    'local_statement
    * ( Challenge.Constant.t
      , Challenge.Constant.t Scalar_challenge.t
      , Tick.Field.t Shifted_value.t
      , Tock.Field.t
      , unit
      , Digest.Constant.t
      , Challenge.Constant.t Scalar_challenge.t Types.Bulletproof_challenge.t
        Types.Step_bp_vec.t
      , Types.Index.t )
      Types.Dlog_based.Proof_state.In_circuit.t
    * (Tick.Field.t array Dlog_plonk_types.Evals.t * Tick.Field.t)
      Tuple_lib.Double.t
    * (Tick.Inner_curve.Affine.t, 'local_max_num_input_proofs) Vector.t
    * ( (Tick.Field.t, Tick.Rounds.n) Vector.t
      , 'local_max_num_input_proofs )
      Vector.t
    * Wrap_proof.t
end

open Core_kernel

let typ (type n avar aval m) (statement : (avar, aval) Impls.Step.Typ.t)
    (local_max_num_input_proofs : n Nat.t) (local_num_rules : m Nat.t)
    ~step_domains : ((avar, n, m) t, (aval, n, m) Constant.t) Impls.Step.Typ.t
    =
  let open Impls.Step in
  let open Step_main_inputs in
  let open Pairing_main in
  let index =
    Typ.transport (One_hot_vector.typ local_num_rules)
      ~there:Types.Index.to_int ~back:(fun x ->
        Option.value_exn (Types.Index.of_int x) )
  in
  Snarky_backendless.Typ.tuple6 statement
    (Types.Dlog_based.Proof_state.In_circuit.typ
       ~challenge:(Challenge.typ' `Constrained)
       ~scalar_challenge:(Challenge.typ' `Unconstrained)
       (Shifted_value.typ Field.typ)
       Other_field.typ
       (Snarky_backendless.Typ.unit ())
       Digest.typ index)
    (let lengths =
       Commitment_lengths.of_domains_vector step_domains
         ~max_degree:Common.Max_degree.step
       |> Dlog_plonk_types.Evals.map ~f:(Vector.reduce_exn ~f:Core.Int.max)
     in
     let t =
       Typ.tuple2
         (Dlog_plonk_types.Evals.typ ~default:Field.Constant.zero lengths
            Field.typ)
         Field.typ
     in
     Typ.tuple2 t t)
    (Vector.typ Inner_curve.typ local_max_num_input_proofs)
    (Vector.typ (Vector.typ Field.typ Tick.Rounds.n) local_max_num_input_proofs)
    Wrap_proof.typ
