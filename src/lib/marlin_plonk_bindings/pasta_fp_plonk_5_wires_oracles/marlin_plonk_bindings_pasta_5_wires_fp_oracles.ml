open Marlin_plonk_bindings_types

type t = Marlin_plonk_bindings_pasta_fp.t Oracles_plonk.t

external create :
     Marlin_plonk_bindings_pasta_fp_urs.Poly_comm.t array
  -> Marlin_plonk_bindings_pasta_5_wires_fp_verifier_index.t
  -> Marlin_plonk_bindings_pasta_5_wires_fp_proof.t
  -> t
  = "caml_pasta_fp_plonk_5_wires_oracles_create"

external dummy : unit -> t = "caml_pasta_fp_plonk_5_wires_oracles_dummy"

external deep_copy : t -> t = "caml_pasta_fp_plonk_5_wires_oracles_deep_copy"

let%test "deep_copy" =
  let x = dummy () in
  deep_copy x = x
