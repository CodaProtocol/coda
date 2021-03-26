use algebra::{
    curves::AffineCurve,
    pasta::{
        pallas::{Affine as GAffine, PallasParameters},
        fq::Fq,
        fp::Fp,
    },
    One,
};

use plonk_plookup_circuits::scalars::ProofEvaluations as DlogProofEvaluations;

use oracle::{
    poseidon_5_wires::PlonkSpongeConstants,
    sponge_5_wires::{DefaultFqSponge, DefaultFrSponge},
};

use groupmap::GroupMap;

use commitment_dlog::commitment::{CommitmentCurve, OpeningProof, PolyComm};
use plonk_plookup_protocol_dlog::index::{Index as DlogIndex, VerifierIndex as DlogVerifierIndex};
use plonk_plookup_protocol_dlog::prover::{ProverCommitments as DlogCommitments, ProverProof as DlogProof};

use crate::pasta_fq_plonk_plookup_index::CamlPastaFqPlonkIndexPtr;
use crate::pasta_fq_plonk_plookup_verifier_index::CamlPastaFqPlonkVerifierIndex;
use crate::pasta_fq_vector::CamlPastaFqVector;

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_proof_create(
    index: CamlPastaFqPlonkIndexPtr<'static>,
    primary_input: CamlPastaFqVector,
    auxiliary_input: (Vec<Fq>, Vec<Fq>, Vec<Fq>, Vec<Fq>, Vec<Fq>),
    prev_challenges: Vec<Fq>,
    prev_sgs: Vec<GAffine>,
) -> DlogProof<GAffine> {
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
                            .map(|x| *x)
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

    let witness = [auxiliary_input.0, auxiliary_input.1, auxiliary_input.2, auxiliary_input.3, auxiliary_input.4];
    let index: &DlogIndex<GAffine> = &index.as_ref().0;

    ocaml::runtime::release_lock();

    let map = GroupMap::<Fp>::setup();
    let proof = DlogProof::create::<
        DefaultFqSponge<PallasParameters, PlonkSpongeConstants>,
        DefaultFrSponge<Fq, PlonkSpongeConstants>,
    >(&map, &witness, index, prev)
    .unwrap();

    ocaml::runtime::acquire_lock();

    proof
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
pub fn caml_pasta_fq_plonk_plookup_proof_verify(
    lgr_comm: Vec<PolyComm<GAffine>>,
    index: CamlPastaFqPlonkVerifierIndex,
    proof: DlogProof<GAffine>,
) -> bool {
    proof_verify(lgr_comm, &index.into(), proof)
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_proof_batch_verify(
    lgr_comms: Vec<Vec<PolyComm<GAffine>>>,
    indexes: Vec<CamlPastaFqPlonkVerifierIndex>,
    proofs: Vec<DlogProof<GAffine>>,
) -> bool {
    let ts: Vec<_> = indexes
        .into_iter()
        .zip(lgr_comms.into_iter())
        .zip(proofs.into_iter())
        .map(|((i, l), p)| (i.into(), l.into_iter().map(From::from).collect(), p.into()))
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
pub fn caml_pasta_fq_plonk_plookup_proof_dummy() -> DlogProof<GAffine> {
    let g = || GAffine::prime_subgroup_generator();
    let comm = || PolyComm {
        shifted: Some(g()),
        unshifted: vec![g(), g(), g()],
    };
    DlogProof {
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
            w_comm: [comm(), comm(), comm(), comm(), comm()],
            z_comm: comm(),
            t_comm: comm(),
            l_comm: comm(),
            lw_comm: comm(),
            h1_comm: comm(),
            h2_comm: comm(),
        },
        public: vec![Fq::one(), Fq::one()],
        evals: {
            let evals = || vec![Fq::one(), Fq::one(), Fq::one(), Fq::one()];
            let evals = || DlogProofEvaluations {
                w: [evals(), evals(), evals(), evals(), evals()],
                z: evals(),
                t: evals(),
                f: evals(),
                s: [evals(), evals(), evals(), evals()],
                l: evals(),
                lw: evals(),
                h1: evals(),
                h2: evals(),
                tb: evals(),
            };
            [evals(), evals()]
        },
    }
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_proof_deep_copy(x: DlogProof<GAffine>) -> DlogProof<GAffine> {
    x
}
