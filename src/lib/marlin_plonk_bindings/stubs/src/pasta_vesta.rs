use crate::arkworks::{CamlFp, CamlFq, CamlGVesta, CamlGroupProjectiveVesta};
use ark_ec::{AffineCurve, ProjectiveCurve};
use ark_ff::{One, UniformRand};
use mina_curves::pasta::{
    fp::Fp,
    fq::Fq,
    vesta::{Affine as GAffine, Projective},
};
use rand::rngs::StdRng;

#[ocaml::func]
pub fn caml_pasta_vesta_one() -> CamlGroupProjectiveVesta {
    Projective::prime_subgroup_generator().into()
}

#[ocaml::func]
pub fn caml_pasta_vesta_add(
    x: ocaml::Pointer<CamlGroupProjectiveVesta>,
    y: ocaml::Pointer<CamlGroupProjectiveVesta>,
) -> CamlGroupProjectiveVesta {
    x.as_ref() + y.as_ref()
}

#[ocaml::func]
pub fn caml_pasta_vesta_sub(
    x: ocaml::Pointer<CamlGroupProjectiveVesta>,
    y: ocaml::Pointer<CamlGroupProjectiveVesta>,
) -> CamlGroupProjectiveVesta {
    x.as_ref() - y.as_ref()
}

#[ocaml::func]
pub fn caml_pasta_vesta_negate(
    x: ocaml::Pointer<CamlGroupProjectiveVesta>,
) -> CamlGroupProjectiveVesta {
    -x.as_ref()
}

#[ocaml::func]
pub fn caml_pasta_vesta_double(
    x: ocaml::Pointer<CamlGroupProjectiveVesta>,
) -> CamlGroupProjectiveVesta {
    x.as_ref().double().into()
}

#[ocaml::func]
pub fn caml_pasta_vesta_scale(
    x: ocaml::Pointer<CamlGroupProjectiveVesta>,
    y: ocaml::Pointer<CamlFp>,
) -> CamlGroupProjectiveVesta {
    let y: &Fp = &y.as_ref().0;
    let y: ark_ff::BigInteger256 = (*y).into();
    x.as_ref().mul(&y).into()
}

#[ocaml::func]
pub fn caml_pasta_vesta_random() -> CamlGroupProjectiveVesta {
    let rng = &mut rand::rngs::OsRng;
    let proj: Projective = UniformRand::rand(rng);
    proj.into()
}

#[ocaml::func]
pub fn caml_pasta_vesta_rng(i: ocaml::Int) -> CamlGroupProjectiveVesta {
    // We only care about entropy here, so we force a conversion i32 -> u32.
    let i: u64 = (i as u32).into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    let proj: Projective = UniformRand::rand(&mut rng);
    proj.into()
}

#[ocaml::func]
pub extern "C" fn caml_pasta_vesta_endo_base() -> CamlFq {
    let (endo_q, _endo_r) = commitment_dlog::srs::endos::<GAffine>();
    endo_q.into()
}

#[ocaml::func]
pub extern "C" fn caml_pasta_vesta_endo_scalar() -> CamlFp {
    let (_endo_q, endo_r) = commitment_dlog::srs::endos::<GAffine>();
    endo_r.into()
}

#[ocaml::func]
pub fn caml_pasta_vesta_to_affine(x: ocaml::Pointer<CamlGroupProjectiveVesta>) -> CamlGVesta {
    x.as_ref().into_affine().into()
}

#[ocaml::func]
pub fn caml_pasta_vesta_of_affine(x: CamlGVesta) -> CamlGroupProjectiveVesta {
    Into::<GAffine>::into(x).into_projective().into()
}

#[ocaml::func]
pub fn caml_pasta_vesta_of_affine_coordinates(x: CamlFq, y: CamlFq) -> CamlGroupProjectiveVesta {
    let res = Projective::new(x.into(), y.into(), Fq::one());
    res.into()
}

#[ocaml::func]
pub fn caml_pasta_vesta_affine_deep_copy(x: CamlGVesta) -> CamlGVesta {
    x
}
