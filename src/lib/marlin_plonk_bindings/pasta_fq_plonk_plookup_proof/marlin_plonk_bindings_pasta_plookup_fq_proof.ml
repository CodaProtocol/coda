open Marlin_plonk_bindings_types

type t =
  ( Marlin_plonk_bindings_pasta_fq.t
  , Marlin_plonk_bindings_pasta_pallas.Affine.t
  , Marlin_plonk_bindings_pasta_fq_urs.Poly_comm.t )
  Plonk_plookup_proof.t

(* TODO-someday: [prev_challenges] should be an array of arrays, not a flat array. *)
external create :
     Marlin_plonk_bindings_pasta_plookup_fq_index.t
  -> primary_input:Marlin_plonk_bindings_pasta_fq_vector.t
  -> auxiliary_input:Marlin_plonk_bindings_pasta_fq.t array
                     * Marlin_plonk_bindings_pasta_fq.t array
                     * Marlin_plonk_bindings_pasta_fq.t array
                     * Marlin_plonk_bindings_pasta_fq.t array
                     * Marlin_plonk_bindings_pasta_fq.t array
  -> prev_challenges:Marlin_plonk_bindings_pasta_fq.t array
  -> prev_sgs:Marlin_plonk_bindings_pasta_pallas.Affine.t array
  -> t
  = "caml_pasta_fq_plonk_plookup_proof_create"

external verify :
     Marlin_plonk_bindings_pasta_fq_urs.Poly_comm.t array
  -> Marlin_plonk_bindings_pasta_plookup_fq_verifier_index.t
  -> t
  -> bool
  = "caml_pasta_fq_plonk_plookup_proof_verify"

external batch_verify :
     Marlin_plonk_bindings_pasta_fq_urs.Poly_comm.t array array
  -> Marlin_plonk_bindings_pasta_plookup_fq_verifier_index.t array
  -> t array
  -> bool
  = "caml_pasta_fq_plonk_plookup_proof_batch_verify"

external dummy : unit -> t = "caml_pasta_fq_plonk_plookup_proof_dummy"

external deep_copy : t -> t = "caml_pasta_fq_plonk_plookup_proof_deep_copy"

let%test "deep_copy" =
  let x = dummy () in
  deep_copy x = x
