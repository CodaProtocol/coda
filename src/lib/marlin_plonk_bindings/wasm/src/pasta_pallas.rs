use wasm_bindgen::prelude::*;
use mina_curves::pasta::{
    pallas::{Affine as GAffine, Projective as GProjective},
    fp::Fp,
};
use algebra::{
    curves::{AffineCurve, ProjectiveCurve},
    One, UniformRand,
};
use rand::rngs::StdRng;
use crate::pasta_fp::WasmPastaFp;
use crate::pasta_fq::WasmPastaFq;
use std::ops::Deref;
use std::convert::{Into, From};

#[wasm_bindgen]
pub struct WasmPallasGProjective(GProjective);

impl Deref for WasmPallasGProjective {
    type Target = GProjective;

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl From<GProjective> for WasmPallasGProjective {
    fn from(x: GProjective) -> Self {
        WasmPallasGProjective(x)
    }
}

impl From<&GProjective> for WasmPallasGProjective {
    fn from(x: &GProjective) -> Self {
        WasmPallasGProjective(*x)
    }
}

impl From<WasmPallasGProjective> for GProjective {
    fn from(x: WasmPallasGProjective) -> Self {
        x.0
    }
}

impl From<&WasmPallasGProjective> for GProjective {
    fn from(x: &WasmPallasGProjective) -> Self {
        x.0
    }
}

impl<'a> From<&'a WasmPallasGProjective> for &'a GProjective {
    fn from(x: &'a WasmPallasGProjective) -> Self {
        &x.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Copy)]
pub struct WasmPallasGAffine {
    pub x: WasmPastaFp,
    pub y: WasmPastaFp,
    pub infinity: bool,
}

impl From<&GAffine> for WasmPallasGAffine {
    fn from(pt: &GAffine) -> Self {
        WasmPallasGAffine {x: WasmPastaFp(pt.x), y: WasmPastaFp(pt.y), infinity: pt.infinity}
    }
}

impl From<GAffine> for WasmPallasGAffine {
    fn from(pt: GAffine) -> Self {
        WasmPallasGAffine {x: WasmPastaFp(pt.x), y: WasmPastaFp(pt.y), infinity: pt.infinity}
    }
}

impl From<WasmPallasGAffine> for GAffine {
    fn from(pt: WasmPallasGAffine) -> Self {
        GAffine::new (pt.x.0, pt.y.0, pt.infinity)
    }
}

impl From<&WasmPallasGAffine> for GAffine {
    fn from(pt: &WasmPallasGAffine) -> Self {
        GAffine::new (pt.x.0, pt.y.0, pt.infinity)
    }
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_one() -> WasmPallasGProjective {
    GProjective::prime_subgroup_generator().into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_add(
    x: &WasmPallasGProjective,
    y: &WasmPallasGProjective,
) -> WasmPallasGProjective {
    (**x + **y).into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_sub(
    x: &WasmPallasGProjective,
    y: &WasmPallasGProjective,
) -> WasmPallasGProjective {
    (**x - **y).into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_negate(x: &WasmPallasGProjective) -> WasmPallasGProjective {
    (-(**x)).into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_double(x: &WasmPallasGProjective) -> WasmPallasGProjective {
    (x.double()).into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_scale(x: &WasmPallasGProjective, y: WasmPastaFq) -> WasmPallasGProjective {
    (x.mul(y.0)).into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_random() -> WasmPallasGProjective {
    let rng = &mut rand_core::OsRng;
    WasmPallasGProjective(UniformRand::rand(rng))
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_rng(i: u32) -> WasmPallasGProjective {
    let i: u64 = i.into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    WasmPallasGProjective(UniformRand::rand(&mut rng))
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_endo_base() -> WasmPastaFp {
    let (endo_q, _endo_r) = commitment_dlog::srs::endos::<GAffine>();
    WasmPastaFp(endo_q)
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_endo_scalar() -> WasmPastaFq {
    let (_endo_q, endo_r) = commitment_dlog::srs::endos::<GAffine>();
    WasmPastaFq(endo_r)
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_to_affine(x: &WasmPallasGProjective) -> WasmPallasGAffine {
    Into::<&GProjective>::into(x).into_affine().into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_of_affine(x: &WasmPallasGAffine) -> WasmPallasGProjective {
    Into::<GAffine>::into(x).into_projective().into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_of_affine_coordinates(x: WasmPastaFp, y: WasmPastaFp) -> WasmPallasGProjective {
    GProjective::new(x.0, y.0, Fp::one()).into()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_affine_deep_copy(x: &WasmPallasGAffine) -> WasmPallasGAffine {
    x.clone()
}

#[wasm_bindgen]
pub fn caml_pasta_pallas_affine_one() -> WasmPallasGAffine {
    GAffine::prime_subgroup_generator().into()
}
