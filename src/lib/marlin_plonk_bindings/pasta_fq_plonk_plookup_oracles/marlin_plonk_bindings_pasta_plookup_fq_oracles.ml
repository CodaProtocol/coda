open Marlin_plonk_bindings_types

type t = Marlin_plonk_bindings_pasta_fq.t Oracles_plookup.t

external create :
     Marlin_plonk_bindings_pasta_fq_urs.Poly_comm.t array
  -> Marlin_plonk_bindings_pasta_plookup_fq_verifier_index.t
  -> Marlin_plonk_bindings_pasta_plookup_fq_proof.t
  -> t
  = "caml_pasta_fq_plonk_plookup_oracles_create"

external dummy : unit -> t = "caml_pasta_fq_plonk_plookup_oracles_dummy"

external deep_copy : t -> t = "caml_pasta_fq_plonk_plookup_oracles_deep_copy"

let%test "deep_copy" =
  let x = dummy () in
  deep_copy x = x
