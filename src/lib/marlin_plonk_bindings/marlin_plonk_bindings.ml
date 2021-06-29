module Types = Marlin_plonk_bindings_types

(** Biginteger types *)

module Bigint_256 = Marlin_plonk_bindings_bigint_256

(** Finite fields *)

module Pasta_fp = Marlin_plonk_bindings_pasta_fp
module Pasta_fq = Marlin_plonk_bindings_pasta_fq

(* Finite field vectors *)

module Pasta_fp_vector = Marlin_plonk_bindings_pasta_fp_vector
module Pasta_fq_vector = Marlin_plonk_bindings_pasta_fq_vector

(* Groups *)

module Pasta_vesta = Marlin_plonk_bindings_pasta_vesta
module Pasta_pallas = Marlin_plonk_bindings_pasta_pallas

(* URSs *)

module Pasta_fp_urs = Marlin_plonk_bindings_pasta_fp_urs
module Pasta_fq_urs = Marlin_plonk_bindings_pasta_fq_urs

(* Indices *)

module Pasta_fp_index = Marlin_plonk_bindings_pasta_fp_index
module Pasta_fq_index = Marlin_plonk_bindings_pasta_fq_index

(* Verification indices *)

module Pasta_fp_verifier_index = Marlin_plonk_bindings_pasta_fp_verifier_index
module Pasta_fq_verifier_index = Marlin_plonk_bindings_pasta_fq_verifier_index

(* Proofs *)
module Pasta_fp_proof = Marlin_plonk_bindings_pasta_fp_proof
module Pasta_fq_proof = Marlin_plonk_bindings_pasta_fq_proof

(* Oracles *)
module Pasta_fp_oracles = Marlin_plonk_bindings_pasta_fp_oracles
module Pasta_fq_oracles = Marlin_plonk_bindings_pasta_fq_oracles
