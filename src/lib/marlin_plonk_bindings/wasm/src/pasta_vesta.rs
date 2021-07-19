use wasm_bindgen::prelude::*;
use mina_curves::pasta::{
    vesta::{Affine as GAffine, Projective as GProjective},
    fq::Fq,
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
pub struct WasmVestaGProjective(GProjective);

impl Deref for WasmVestaGProjective {
    type Target = GProjective;

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl From<GProjective> for WasmVestaGProjective {
    fn from(x: GProjective) -> Self {
        WasmVestaGProjective(x)
    }
}

impl From<&GProjective> for WasmVestaGProjective {
    fn from(x: &GProjective) -> Self {
        WasmVestaGProjective(*x)
    }
}

impl From<WasmVestaGProjective> for GProjective {
    fn from(x: WasmVestaGProjective) -> Self {
        x.0
    }
}

impl From<&WasmVestaGProjective> for GProjective {
    fn from(x: &WasmVestaGProjective) -> Self {
        x.0
    }
}

impl<'a> From<&'a WasmVestaGProjective> for &'a GProjective {
    fn from(x: &'a WasmVestaGProjective) -> Self {
        &x.0
    }
}

#[wasm_bindgen]
#[derive(Clone, Copy)]
pub struct WasmVestaGAffine {
    pub x: WasmPastaFq,
    pub y: WasmPastaFq,
    pub infinity: bool,
}

impl From<&GAffine> for WasmVestaGAffine {
    fn from(pt: &GAffine) -> Self {
        WasmVestaGAffine {x: WasmPastaFq(pt.x), y: WasmPastaFq(pt.y), infinity: pt.infinity}
    }
}

impl From<GAffine> for WasmVestaGAffine {
    fn from(pt: GAffine) -> Self {
        WasmVestaGAffine {x: WasmPastaFq(pt.x), y: WasmPastaFq(pt.y), infinity: pt.infinity}
    }
}

impl From<WasmVestaGAffine> for GAffine {
    fn from(pt: WasmVestaGAffine) -> Self {
        GAffine::new (pt.x.0, pt.y.0, pt.infinity)
    }
}

impl From<&WasmVestaGAffine> for GAffine {
    fn from(pt: &WasmVestaGAffine) -> Self {
        GAffine::new (pt.x.0, pt.y.0, pt.infinity)
    }
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_one() -> WasmVestaGProjective {
    GProjective::prime_subgroup_generator().into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_add(
    x: &WasmVestaGProjective,
    y: &WasmVestaGProjective,
) -> WasmVestaGProjective {
    (**x + **y).into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_sub(
    x: &WasmVestaGProjective,
    y: &WasmVestaGProjective,
) -> WasmVestaGProjective {
    (**x - **y).into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_negate(x: &WasmVestaGProjective) -> WasmVestaGProjective {
    (-(**x)).into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_double(x: &WasmVestaGProjective) -> WasmVestaGProjective {
    (x.double()).into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_scale(x: &WasmVestaGProjective, y: WasmPastaFp) -> WasmVestaGProjective {
    (x.mul(y.0)).into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_random() -> WasmVestaGProjective {
    let rng = &mut rand_core::OsRng;
    WasmVestaGProjective(UniformRand::rand(rng))
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_rng(i: u32) -> WasmVestaGProjective {
    let i: u64 = i.into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    WasmVestaGProjective(UniformRand::rand(&mut rng))
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_endo_base() -> WasmPastaFq {
    let (endo_q, _endo_r) = commitment_dlog::srs::endos::<GAffine>();
    WasmPastaFq(endo_q)
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_endo_scalar() -> WasmPastaFp {
    let (_endo_q, endo_r) = commitment_dlog::srs::endos::<GAffine>();
    WasmPastaFp(endo_r)
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_to_affine(x: &WasmVestaGProjective) -> WasmVestaGAffine {
    Into::<&GProjective>::into(x).into_affine().into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_of_affine(x: &WasmVestaGAffine) -> WasmVestaGProjective {
    Into::<GAffine>::into(x).into_projective().into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_of_affine_coordinates(x: WasmPastaFq, y: WasmPastaFq) -> WasmVestaGProjective {
    GProjective::new(x.0, y.0, Fq::one()).into()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_affine_deep_copy(x: &WasmVestaGAffine) -> WasmVestaGAffine {
    x.clone()
}

#[wasm_bindgen]
pub fn caml_pasta_vesta_affine_one() -> WasmVestaGAffine {
    GAffine::prime_subgroup_generator().into()
}
