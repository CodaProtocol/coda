//! The Marlin_plonk_stubs crate exports some functionalities
//! and structures from the following the Rust crates to OCaml:
//!
//! * [Marlin](https://github.com/o1-labs/marlin),
//!   a PLONK implementation.
//! * [Arkworks](http://arkworks.rs/),
//!   a math library that Marlin builds on top of.
//!

extern crate libc;

/* Arkworks types */
pub mod arkworks;
/* Caml pointers */
pub mod caml_pointer;
/* Field vectors */
pub mod pasta_fp_vector;
pub mod pasta_fq_vector;
/* Groups */
pub mod pasta_pallas;
pub mod pasta_vesta;
/* URS */
pub mod pasta_fp_urs;
pub mod pasta_fq_urs;
pub mod urs_utils;
/* Gates */
pub mod plonk_gate;
/* Indices */
pub mod index_serialization;
pub mod pasta_fp_plonk_index;
pub mod pasta_fp_plonk_verifier_index;
pub mod pasta_fq_plonk_index;
pub mod pasta_fq_plonk_verifier_index;
pub mod plonk_verifier_index;
/* Proofs */
pub mod pasta_fp_plonk_proof;
pub mod pasta_fq_plonk_proof;
/* Oracles */
pub mod pasta_fp_plonk_oracles;
pub mod pasta_fq_plonk_oracles;
