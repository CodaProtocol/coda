use algebra::tweedle::{
    dum::{Affine as GAffine, TweedledumParameters},
    fq::Fq,
};

use oracle::{
    self,
    poseidon::PlonkSpongeConstants,
    sponge::{DefaultFqSponge, DefaultFrSponge, ScalarChallenge},
};

use commitment_dlog::commitment::PolyComm;
use plonk_protocol_dlog::prover::ProverProof as DlogProof;

use crate::tweedle_dum::CamlTweedleDumPolyComm;
use crate::tweedle_fp::CamlTweedleFp;
use crate::tweedle_fq::CamlTweedleFq;
use crate::tweedle_fq_plonk_proof::CamlTweedleFqPlonkProof;
use crate::tweedle_fq_plonk_verifier_index::{
    CamlTweedleFqPlonkVerifierIndexPtr, CamlTweedleFqPlonkVerifierIndexRaw,
    CamlTweedleFqPlonkVerifierIndexRawPtr,
};

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlTweedleFqPlonkRandomOracles {
    pub beta: CamlTweedleFq,
    pub gamma: CamlTweedleFq,
    pub alpha_chal: CamlTweedleFq,
    pub alpha: CamlTweedleFq,
    pub zeta: CamlTweedleFq,
    pub v: CamlTweedleFq,
    pub u: CamlTweedleFq,
    pub zeta_chal: CamlTweedleFq,
    pub v_chal: CamlTweedleFq,
    pub u_chal: CamlTweedleFq,
}

impl From<CamlTweedleFqPlonkRandomOracles> for plonk_circuits::scalars::RandomOracles<Fq> {
    fn from(x: CamlTweedleFqPlonkRandomOracles) -> plonk_circuits::scalars::RandomOracles<Fq> {
        plonk_circuits::scalars::RandomOracles {
            beta: x.beta.0,
            gamma: x.gamma.0,
            alpha_chal: ScalarChallenge(x.alpha_chal.0),
            alpha: x.alpha.0,
            zeta: x.zeta.0,
            v: x.v.0,
            u: x.u.0,
            zeta_chal: ScalarChallenge(x.zeta_chal.0),
            v_chal: ScalarChallenge(x.v_chal.0),
            u_chal: ScalarChallenge(x.u_chal.0),
        }
    }
}

impl From<plonk_circuits::scalars::RandomOracles<Fq>> for CamlTweedleFqPlonkRandomOracles {
    fn from(x: plonk_circuits::scalars::RandomOracles<Fq>) -> CamlTweedleFqPlonkRandomOracles {
        CamlTweedleFqPlonkRandomOracles {
            beta: CamlTweedleFq(x.beta),
            gamma: CamlTweedleFq(x.gamma),
            alpha_chal: CamlTweedleFq(x.alpha_chal.0),
            alpha: CamlTweedleFq(x.alpha),
            zeta: CamlTweedleFq(x.zeta),
            v: CamlTweedleFq(x.v),
            u: CamlTweedleFq(x.u),
            zeta_chal: CamlTweedleFq(x.zeta_chal.0),
            v_chal: CamlTweedleFq(x.v_chal.0),
            u_chal: CamlTweedleFq(x.u_chal.0),
        }
    }
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlTweedleFqPlonkOracles {
    pub o: CamlTweedleFqPlonkRandomOracles,
    pub p_eval: (CamlTweedleFq, CamlTweedleFq),
    pub opening_prechallenges: Vec<CamlTweedleFq>,
    pub digest_before_evaluations: CamlTweedleFq,
}

#[ocaml::func]
pub fn caml_tweedle_fq_plonk_oracles_create_raw(
    lgr_comm: Vec<CamlTweedleDumPolyComm<CamlTweedleFp>>,
    index: CamlTweedleFqPlonkVerifierIndexRawPtr<'static>,
    proof: CamlTweedleFqPlonkProof,
) -> CamlTweedleFqPlonkOracles {
    let index = index.as_ref();
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
    let (mut sponge, digest_before_evaluations, o, _, p_eval, _, _) =
        proof.oracles::<DefaultFqSponge<TweedledumParameters, PlonkSpongeConstants>, DefaultFrSponge<Fq, PlonkSpongeConstants>>(&index.0, &p_comm);

    CamlTweedleFqPlonkOracles {
        o: o.into(),
        p_eval: (CamlTweedleFq(p_eval[0][0]), CamlTweedleFq(p_eval[1][0])),
        opening_prechallenges: proof
            .proof
            .prechallenges(&mut sponge)
            .into_iter()
            .map(From::from)
            .collect(),
        digest_before_evaluations: CamlTweedleFq(digest_before_evaluations),
    }
}

#[ocaml::func]
pub fn caml_tweedle_fq_plonk_oracles_create(
    lgr_comm: Vec<CamlTweedleDumPolyComm<CamlTweedleFp>>,
    index: CamlTweedleFqPlonkVerifierIndexPtr,
    proof: CamlTweedleFqPlonkProof,
) -> CamlTweedleFqPlonkOracles {
    let index: CamlTweedleFqPlonkVerifierIndexRaw = index.into();
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
    let (mut sponge, digest_before_evaluations, o, _, p_eval, _, _) =
        proof.oracles::<DefaultFqSponge<TweedledumParameters, PlonkSpongeConstants>, DefaultFrSponge<Fq, PlonkSpongeConstants>>(&index.0, &p_comm);

    CamlTweedleFqPlonkOracles {
        o: o.into(),
        p_eval: (CamlTweedleFq(p_eval[0][0]), CamlTweedleFq(p_eval[1][0])),
        opening_prechallenges: proof
            .proof
            .prechallenges(&mut sponge)
            .into_iter()
            .map(From::from)
            .collect(),
        digest_before_evaluations: CamlTweedleFq(digest_before_evaluations),
    }
}
