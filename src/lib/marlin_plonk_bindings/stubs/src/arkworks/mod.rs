//! This module contains wrapper types to Arkworks types.
//! To use Arkwork types in OCaml, you have to convert to these types,
//! and convert back from them to use them in Rust.
//!
//! For example:
//!
//! ```
//! use marlin_plonk_bindings::arkworks::CamlBiginteger256;
//! use ark_ff::BigInteger256;
//!
//! #[ocaml::func]
//! pub fn caml_add(x: CamlBigInteger256, y: CamlBigInteger256) -> CamlBigInteger256 {
//!    let x: BigInteger256 = x.into();
//!    let y: BigInteger256 = y.into();
//!    (x + y).into()
//! }
//! ```
//!

pub mod bigint_256;
pub mod group_affine;
pub mod group_projective;
pub mod pasta_fp;
pub mod pasta_fq;

// re-export what's important

pub use bigint_256::CamlBigInteger256;
pub use group_affine::{CamlGPallas, CamlGVesta, CamlGroupAffine};
pub use group_projective::{CamlGroupProjectivePallas, CamlGroupProjectiveVesta};
pub use pasta_fp::CamlFp;
pub use pasta_fq::CamlFq;
