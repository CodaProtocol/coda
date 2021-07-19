use crate::arkworks::{CamlFq, CamlGPallas};
use crate::pasta_fq_plonk_index::CamlPastaFqPlonkIndexPtr;
use crate::pasta_fq_plonk_verifier_index::CamlPastaFqPlonkVerifierIndex;
use crate::pasta_fq_vector::CamlPastaFqVector;
use ark_ec::AffineCurve;
use ark_ff::One;
use commitment_dlog::commitment::caml::CamlPolyComm;
use commitment_dlog::commitment::{CommitmentCurve, OpeningProof, PolyComm};
use groupmap::GroupMap;
use mina_curves::pasta::{
    fp::Fp,
    fq::Fq,
    pallas::{Affine as GAffine, PallasParameters},
};
use oracle::{
    poseidon::PlonkSpongeConstants,
    sponge::{DefaultFqSponge, DefaultFrSponge},
};
use plonk_circuits::scalars::ProofEvaluations as DlogProofEvaluations;
use plonk_protocol_dlog::index::{Index as DlogIndex, VerifierIndex as DlogVerifierIndex};
use plonk_protocol_dlog::prover::caml::CamlProverProof;
use plonk_protocol_dlog::prover::{ProverCommitments as DlogCommitments, ProverProof as DlogProof};

#[ocaml::func]
pub fn caml_pasta_fq_plonk_proof_create(
    index: CamlPastaFqPlonkIndexPtr<'static>,
    primary_input: CamlPastaFqVector,
    auxiliary_input: CamlPastaFqVector,
    prev_challenges: Vec<CamlFq>,
    prev_sgs: Vec<CamlGPallas>,
) -> CamlProverProof<CamlGPallas, CamlFq> {
    // TODO: Should we be ignoring this?!
    let _primary_input = primary_input;

    let prev: Vec<(Vec<Fq>, PolyComm<GAffine>)> = {
        if prev_challenges.len() == 0 {
            Vec::new()
        } else {
            let challenges_per_sg = prev_challenges.len() / prev_sgs.len();
            prev_sgs
                .into_iter()
                .enumerate()
                .map(|(i, sg)| {
                    (
                        prev_challenges[(i * challenges_per_sg)..(i + 1) * challenges_per_sg]
                            .iter()
                            .map(Into::<Fq>::into)
                            .collect(),
                        PolyComm::<GAffine> {
                            unshifted: vec![sg.into()],
                            shifted: None,
                        },
                    )
                })
                .collect()
        }
    };

    let auxiliary_input: &Vec<Fq> = &*auxiliary_input;
    let index: &DlogIndex<GAffine> = &index.as_ref().0;

    // NB: This method is designed only to be used by tests. However, since creating a new reference will cause `drop` to be called on it once we are done with it. Since `drop` calls `caml_shutdown` internally, we *really, really* do not want to do this, but we have no other way to get at the active runtime.
    let runtime = unsafe { ocaml::Runtime::recover_handle() };

    // Release the runtime lock so that other threads can run using it while we generate the proof.
    runtime.releasing_runtime(|| {
        let map = GroupMap::<Fp>::setup();
        let proof = DlogProof::create::<
            DefaultFqSponge<PallasParameters, PlonkSpongeConstants>,
            DefaultFrSponge<Fq, PlonkSpongeConstants>,
        >(&map, auxiliary_input, index, prev)
        .unwrap();
        proof.into()
    })
}

pub fn proof_verify(
    lgr_comm: Vec<PolyComm<GAffine>>,
    index: &DlogVerifierIndex<GAffine>,
    proof: DlogProof<GAffine>,
) -> bool {
    let group_map = <GAffine as CommitmentCurve>::Map::setup();

    DlogProof::verify::<
        DefaultFqSponge<PallasParameters, PlonkSpongeConstants>,
        DefaultFrSponge<Fq, PlonkSpongeConstants>,
    >(
        &group_map,
        &[(
            index,
            &lgr_comm.into_iter().map(From::from).collect(),
            &proof,
        )]
        .to_vec(),
    )
    .is_ok()
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_proof_verify(
    lgr_comm: Vec<CamlPolyComm<CamlGPallas>>,
    index: CamlPastaFqPlonkVerifierIndex,
    proof: CamlProverProof<CamlGPallas, CamlFq>,
) -> bool {
    let lgr_comm = lgr_comm.into_iter().map(|x| x.into()).collect();
    proof_verify(lgr_comm, &index.into(), proof.into())
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_proof_batch_verify(
    lgr_comms: Vec<Vec<CamlPolyComm<CamlGPallas>>>,
    indexes: Vec<CamlPastaFqPlonkVerifierIndex>,
    proofs: Vec<CamlProverProof<CamlGPallas, CamlFq>>,
) -> bool {
    let ts: Vec<_> = indexes
        .into_iter()
        .zip(lgr_comms.into_iter())
        .zip(proofs.into_iter())
        .map(|((i, l), p)| (i.into(), l.into_iter().map(Into::into).collect(), p.into()))
        .collect();
    let ts: Vec<_> = ts.iter().map(|(i, l, p)| (i, l, p)).collect();
    let group_map = GroupMap::<Fp>::setup();

    DlogProof::<GAffine>::verify::<
        DefaultFqSponge<PallasParameters, PlonkSpongeConstants>,
        DefaultFrSponge<Fq, PlonkSpongeConstants>,
    >(&group_map, &ts)
    .is_ok()
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_proof_dummy() -> CamlProverProof<CamlGPallas, CamlFq> {
    let g = || GAffine::prime_subgroup_generator();
    let comm = || PolyComm {
        shifted: Some(g()),
        unshifted: vec![g(), g(), g()],
    };
    let dlogproof = DlogProof {
        prev_challenges: vec![
            (vec![Fq::one(), Fq::one()], comm()),
            (vec![Fq::one(), Fq::one()], comm()),
            (vec![Fq::one(), Fq::one()], comm()),
        ],
        proof: OpeningProof {
            lr: vec![(g(), g()), (g(), g()), (g(), g())],
            z1: Fq::one(),
            z2: Fq::one(),
            delta: g(),
            sg: g(),
        },
        commitments: DlogCommitments {
            l_comm: comm(),
            r_comm: comm(),
            o_comm: comm(),
            z_comm: comm(),
            t_comm: comm(),
        },
        public: vec![Fq::one(), Fq::one()],
        evals: {
            let evals = || vec![Fq::one(), Fq::one(), Fq::one(), Fq::one()];
            let evals = || DlogProofEvaluations {
                l: evals(),
                r: evals(),
                o: evals(),
                z: evals(),
                t: evals(),
                f: evals(),
                sigma1: evals(),
                sigma2: evals(),
            };
            [evals(), evals()]
        },
    };
    dlogproof.into()
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_proof_deep_copy(
    x: CamlProverProof<CamlGPallas, CamlFq>,
) -> CamlProverProof<CamlGPallas, CamlFq> {
    x
}
