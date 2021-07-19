use ark_ec::{AffineCurve, ProjectiveCurve};
use ark_ff::{One, UniformRand};
use mina_curves::pasta::{
    fp::Fp,
    fq::Fq,
    pallas::{Affine as GAffine, Projective},
};
use rand::rngs::StdRng;

use crate::arkworks::{CamlFp, CamlFq, CamlGPallas, CamlGroupProjectivePallas};

#[ocaml::func]
pub fn caml_pasta_pallas_one() -> CamlGroupProjectivePallas {
    Projective::prime_subgroup_generator().into()
}

#[ocaml::func]
pub fn caml_pasta_pallas_add(
    x: ocaml::Pointer<CamlGroupProjectivePallas>,
    y: ocaml::Pointer<CamlGroupProjectivePallas>,
) -> CamlGroupProjectivePallas {
    x.as_ref() + y.as_ref()
}

#[ocaml::func]
pub fn caml_pasta_pallas_sub(
    x: ocaml::Pointer<CamlGroupProjectivePallas>,
    y: ocaml::Pointer<CamlGroupProjectivePallas>,
) -> CamlGroupProjectivePallas {
    x.as_ref() - y.as_ref()
}

#[ocaml::func]
pub fn caml_pasta_pallas_negate(
    x: ocaml::Pointer<CamlGroupProjectivePallas>,
) -> CamlGroupProjectivePallas {
    -(*x.as_ref())
}

#[ocaml::func]
pub fn caml_pasta_pallas_double(
    x: ocaml::Pointer<CamlGroupProjectivePallas>,
) -> CamlGroupProjectivePallas {
    x.as_ref().double().into()
}

#[ocaml::func]
pub fn caml_pasta_pallas_scale(
    x: ocaml::Pointer<CamlGroupProjectivePallas>,
    y: ocaml::Pointer<CamlFq>,
) -> CamlGroupProjectivePallas {
    let y: &Fq = &y.as_ref().0;
    let y: ark_ff::BigInteger256 = (*y).into();
    x.as_ref().mul(&y).into()
}

#[ocaml::func]
pub fn caml_pasta_pallas_random() -> CamlGroupProjectivePallas {
    let rng = &mut rand::rngs::OsRng;
    let proj: Projective = UniformRand::rand(rng);
    proj.into()
}

#[ocaml::func]
pub fn caml_pasta_pallas_rng(i: ocaml::Int) -> CamlGroupProjectivePallas {
    // We only care about entropy here, so we force a conversion i32 -> u32.
    let i: u64 = (i as u32).into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    let proj: Projective = UniformRand::rand(&mut rng);
    proj.into()
}

#[ocaml::func]
pub extern "C" fn caml_pasta_pallas_endo_base() -> CamlFp {
    let (endo_q, _endo_r) = commitment_dlog::srs::endos::<GAffine>();
    endo_q.into()
}

#[ocaml::func]
pub extern "C" fn caml_pasta_pallas_endo_scalar() -> CamlFq {
    let (_endo_q, endo_r) = commitment_dlog::srs::endos::<GAffine>();
    endo_r.into()
}

#[ocaml::func]
pub fn caml_pasta_pallas_to_affine(x: ocaml::Pointer<CamlGroupProjectivePallas>) -> CamlGPallas {
    x.as_ref().into_affine().into()
}

#[ocaml::func]
pub fn caml_pasta_pallas_of_affine(x: CamlGPallas) -> CamlGroupProjectivePallas {
    Into::<GAffine>::into(x).into_projective().into()
}

#[ocaml::func]
pub fn caml_pasta_pallas_of_affine_coordinates(x: CamlFp, y: CamlFp) -> CamlGroupProjectivePallas {
    let res = Projective::new(x.into(), y.into(), Fp::one());
    res.into()
}

#[ocaml::func]
pub fn caml_pasta_pallas_affine_deep_copy(x: CamlGPallas) -> CamlGPallas {
    x
}
