use algebra::tweedle::{
    dum::{Affine as GAffine, TweedledumParameters},
    fq::Fq,
};

use oracle::{
    self,
    poseidon::PlonkSpongeConstants,
    sponge::{DefaultFqSponge, DefaultFrSponge},
    FqSponge,
};

use commitment_dlog::commitment::{shift_scalar, PolyComm};
use plonk_circuits::scalars::RandomOracles;
use plonk_protocol_dlog::{
    index::VerifierIndex as DlogVerifierIndex, prover::ProverProof as DlogProof,
};

use crate::tweedle_fq_plonk_verifier_index::CamlTweedleFqPlonkVerifierIndex;

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub struct CamlTweedleFqPlonkOracles {
    pub o: RandomOracles<Fq>,
    pub p_eval: (Fq, Fq),
    pub opening_prechallenges: Vec<Fq>,
    pub digest_before_evaluations: Fq,
}

#[ocaml::func]
pub fn caml_tweedle_fq_plonk_oracles_create(
    lgr_comm: Vec<PolyComm<GAffine>>,
    index: CamlTweedleFqPlonkVerifierIndex,
    proof: DlogProof<GAffine>,
) -> CamlTweedleFqPlonkOracles {
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
        proof.oracles::<DefaultFqSponge<TweedledumParameters, PlonkSpongeConstants>, DefaultFrSponge<Fq, PlonkSpongeConstants>>(&index, &p_comm);

    sponge.absorb_fr(&[shift_scalar(combined_inner_product)]);

    CamlTweedleFqPlonkOracles {
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
pub fn caml_tweedle_fq_plonk_oracles_dummy() -> RandomOracles<Fq> {
    RandomOracles::zero().into()
}

#[ocaml::func]
pub fn caml_tweedle_fq_plonk_oracles_deep_copy(x: RandomOracles<Fq>) -> RandomOracles<Fq> {
    x
}
