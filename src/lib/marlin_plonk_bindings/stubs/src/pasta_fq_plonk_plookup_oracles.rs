use algebra::pasta::{
    pallas::{Affine as GAffine, PallasParameters},
    fq::Fq,
};

use oracle::{
    self,
    poseidon_5_wires::PlonkSpongeConstants,
    sponge_5_wires::{DefaultFqSponge, DefaultFrSponge},
    FqSponge,
};

use commitment_dlog::commitment::{shift_scalar, PolyComm};
use plonk_plookup_circuits::scalars::RandomOracles;
use plonk_plookup_protocol_dlog::{
    index::VerifierIndex as DlogVerifierIndex, prover::ProverProof as DlogProof,
};

use crate::pasta_fq_plonk_plookup_verifier_index::CamlPastaFqPlonkVerifierIndex;

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlPastaFqPlonkOracles {
    pub o: RandomOracles<Fq>,
    pub p_eval: (Fq, Fq),
    pub opening_prechallenges: Vec<Fq>,
    pub digest_before_evaluations: Fq,
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_oracles_create(
    lgr_comm: Vec<PolyComm<GAffine>>,
    index: CamlPastaFqPlonkVerifierIndex,
    proof: DlogProof<GAffine>,
) -> CamlPastaFqPlonkOracles {
    let index: DlogVerifierIndex<'_, GAffine> = index.into();
    let proof: DlogProof<GAffine> = proof.into();
    let lgr_comm: Vec<PolyComm<GAffine>> = lgr_comm.into_iter().map(From::from).collect();

    let p_comm = PolyComm::<GAffine>::multi_scalar_mul(
        &lgr_comm
            .iter()
            .take(proof.public.len())
            .map(|x| x)
            .collect(),
        &proof.public.iter().map(|s| -*s).collect(),
    );
    let (mut sponge, digest_before_evaluations, o, _, p_eval, _, _, _, combined_inner_product) =
        proof.oracles::<DefaultFqSponge<PallasParameters, PlonkSpongeConstants>, DefaultFrSponge<Fq, PlonkSpongeConstants>>(&index, &p_comm);

    sponge.absorb_fr(&[shift_scalar(combined_inner_product)]);

    CamlPastaFqPlonkOracles {
        o: o,
        p_eval: (p_eval[0][0], p_eval[1][0]),
        opening_prechallenges: proof
            .proof
            .prechallenges(&mut sponge)
            .into_iter()
            .map(|x| x.0)
            .collect(),
        digest_before_evaluations: digest_before_evaluations,
    }
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_oracles_dummy() -> RandomOracles<Fq> {
    RandomOracles::zero().into()
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_oracles_deep_copy(x: RandomOracles<Fq>) -> RandomOracles<Fq> {
    x
}
