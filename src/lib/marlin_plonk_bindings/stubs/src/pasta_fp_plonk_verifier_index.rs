use crate::index_serialization;
use crate::pasta_fp_plonk_index::CamlPastaFpPlonkIndexPtr;
use crate::pasta_fp_urs::CamlPastaFpUrs;
use crate::plonk_verifier_index::{
    CamlPlonkDomain, CamlPlonkVerificationEvals, CamlPlonkVerificationShifts,
    CamlPlonkVerifierIndex,
};
use crate::{
    arkworks::{CamlFp, CamlGVesta},
    caml_pointer,
};
use ark_ec::AffineCurve;
use ark_ff::One;
use ark_poly::{EvaluationDomain, Radix2EvaluationDomain as Domain};
use commitment_dlog::commitment::caml::CamlPolyComm;
use commitment_dlog::{
    commitment::PolyComm,
    srs::{SRSValue, SRS},
};
use mina_curves::pasta::{fp::Fp, pallas::Affine as GAffineOther, vesta::Affine as GAffine};
use plonk_circuits::constraints::{zk_polynomial, zk_w, ConstraintSystem};
use plonk_protocol_dlog::index::VerifierIndex as DlogVerifierIndex;
use std::rc::Rc;
use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Seek, SeekFrom::Start},
};

//
// CamlPastaFpPlonkVerifierIndex
//

pub type CamlPastaFpPlonkVerifierIndex =
    CamlPlonkVerifierIndex<CamlFp, CamlPastaFpUrs, CamlPolyComm<CamlGVesta>>;

//
// (Rc<SRS<GAffine>>, DlogVerifierIndex<'a, GAffine>) -> CamlPastaFpPlonkVerifierIndex
//

pub fn to_ocaml<'a>(
    urs: &Rc<SRS<GAffine>>,
    vi: DlogVerifierIndex<'a, GAffine>,
) -> CamlPastaFpPlonkVerifierIndex {
    let [sigma_comm0, sigma_comm1, sigma_comm2] = vi.sigma_comm;
    let [rcm_comm0, rcm_comm1, rcm_comm2] = vi.rcm_comm;
    CamlPastaFpPlonkVerifierIndex {
        domain: CamlPlonkDomain {
            log_size_of_group: vi.domain.log_size_of_group as isize,
            group_gen: CamlFp(vi.domain.group_gen),
        },
        max_poly_size: vi.max_poly_size as isize,
        max_quot_size: vi.max_quot_size as isize,
        urs: caml_pointer::create(Rc::clone(urs)),
        evals: CamlPlonkVerificationEvals {
            sigma_comm0: sigma_comm0.into(),
            sigma_comm1: sigma_comm1.into(),
            sigma_comm2: sigma_comm2.into(),
            ql_comm: vi.ql_comm.into(),
            qr_comm: vi.qr_comm.into(),
            qo_comm: vi.qo_comm.into(),
            qm_comm: vi.qm_comm.into(),
            qc_comm: vi.qc_comm.into(),
            rcm_comm0: rcm_comm0.into(),
            rcm_comm1: rcm_comm1.into(),
            rcm_comm2: rcm_comm2.into(),
            psm_comm: vi.psm_comm.into(),
            add_comm: vi.add_comm.into(),
            mul1_comm: vi.mul1_comm.into(),
            mul2_comm: vi.mul2_comm.into(),
            emul1_comm: vi.emul1_comm.into(),
            emul2_comm: vi.emul2_comm.into(),
            emul3_comm: vi.emul3_comm.into(),
        },
        shifts: CamlPlonkVerificationShifts {
            r: vi.r.into(),
            o: vi.o.into(),
        },
    }
}

pub fn to_ocaml_copy<'a>(
    urs: &Rc<SRS<GAffine>>,
    vi: &DlogVerifierIndex<'a, GAffine>,
) -> CamlPastaFpPlonkVerifierIndex {
    let [sigma_comm0, sigma_comm1, sigma_comm2] = &vi.sigma_comm;
    let [rcm_comm0, rcm_comm1, rcm_comm2] = &vi.rcm_comm;
    CamlPlonkVerifierIndex {
        domain: CamlPlonkDomain {
            log_size_of_group: vi.domain.log_size_of_group as isize,
            group_gen: CamlFp(vi.domain.group_gen),
        },
        max_poly_size: vi.max_poly_size as isize,
        max_quot_size: vi.max_quot_size as isize,
        urs: caml_pointer::create(Rc::clone(urs)),
        evals: CamlPlonkVerificationEvals {
            sigma_comm0: sigma_comm0.clone().into(),
            sigma_comm1: sigma_comm1.clone().into(),
            sigma_comm2: sigma_comm2.clone().into(),
            ql_comm: vi.ql_comm.clone().into(),
            qr_comm: vi.qr_comm.clone().into(),
            qo_comm: vi.qo_comm.clone().into(),
            qm_comm: vi.qm_comm.clone().into(),
            qc_comm: vi.qc_comm.clone().into(),
            rcm_comm0: rcm_comm0.clone().into(),
            rcm_comm1: rcm_comm1.clone().into(),
            rcm_comm2: rcm_comm2.clone().into(),
            psm_comm: vi.psm_comm.clone().into(),
            add_comm: vi.add_comm.clone().into(),
            mul1_comm: vi.mul1_comm.clone().into(),
            mul2_comm: vi.mul2_comm.clone().into(),
            emul1_comm: vi.emul1_comm.clone().into(),
            emul2_comm: vi.emul2_comm.clone().into(),
            emul3_comm: vi.emul3_comm.clone().into(),
        },
        shifts: CamlPlonkVerificationShifts {
            r: CamlFp(vi.r),
            o: CamlFp(vi.o),
        },
    }
}

//
// ... -> (DlogVerifierIndex<'a, GAffine>, Rc<SRS<GAffine>)
//

pub fn of_ocaml<'a>(
    max_poly_size: ocaml::Int,
    max_quot_size: ocaml::Int,
    log_size_of_group: ocaml::Int,
    urs: CamlPastaFpUrs,
    evals: CamlPlonkVerificationEvals<CamlPolyComm<CamlGVesta>>,
    shifts: CamlPlonkVerificationShifts<CamlFp>,
) -> (DlogVerifierIndex<'a, GAffine>, Rc<SRS<GAffine>>) {
    let urs_copy = Rc::clone(&*urs);
    let urs_copy_outer = Rc::clone(&*urs);
    let srs = {
        // We know that the underlying value is still alive, because we never convert any of our
        // Rc<_>s into weak pointers.
        SRSValue::Ref(unsafe { &*Rc::into_raw(urs_copy) })
    };
    let (endo_q, _endo_r) = commitment_dlog::srs::endos::<GAffineOther>();
    let domain = Domain::<Fp>::new(1 << log_size_of_group).unwrap();
    let index = DlogVerifierIndex::<GAffine> {
        domain,
        w: zk_w(domain),
        zkpm: zk_polynomial(domain),
        max_poly_size: max_poly_size as usize,
        max_quot_size: max_quot_size as usize,
        srs,
        sigma_comm: [
            evals.sigma_comm0.into(),
            evals.sigma_comm1.into(),
            evals.sigma_comm2.into(),
        ],
        ql_comm: evals.ql_comm.into(),
        qr_comm: evals.qr_comm.into(),
        qo_comm: evals.qo_comm.into(),
        qm_comm: evals.qm_comm.into(),
        qc_comm: evals.qc_comm.into(),
        rcm_comm: [
            evals.rcm_comm0.into(),
            evals.rcm_comm1.into(),
            evals.rcm_comm2.into(),
        ],
        psm_comm: evals.psm_comm.into(),
        add_comm: evals.add_comm.into(),
        mul1_comm: evals.mul1_comm.into(),
        mul2_comm: evals.mul2_comm.into(),
        emul1_comm: evals.emul1_comm.into(),
        emul2_comm: evals.emul2_comm.into(),
        emul3_comm: evals.emul3_comm.into(),
        r: shifts.r.into(),
        o: shifts.o.into(),
        fr_sponge_params: oracle::pasta::fp::params(),
        fq_sponge_params: oracle::pasta::fq::params(),
        endo: endo_q,
    };
    (index, urs_copy_outer)
}

impl From<CamlPastaFpPlonkVerifierIndex> for DlogVerifierIndex<'_, GAffine> {
    fn from(index: CamlPastaFpPlonkVerifierIndex) -> Self {
        of_ocaml(
            index.max_poly_size,
            index.max_quot_size,
            index.domain.log_size_of_group,
            index.urs,
            index.evals,
            index.shifts,
        )
        .0
    }
}

pub fn read_raw<'a>(
    offset: Option<ocaml::Int>,
    urs: CamlPastaFpUrs,
    path: String,
) -> Result<(DlogVerifierIndex<'a, GAffine>, Rc<SRS<GAffine>>), ocaml::Error> {
    match File::open(path) {
        Err(_) => Err(ocaml::Error::invalid_argument(
            "caml_pasta_fp_plonk_verifier_index_raw_read",
        )
        .err()
        .unwrap()),
        Ok(file) => {
            let mut r = BufReader::new(file);
            match offset {
                Some(offset) => {
                    r.seek(Start(offset as u64))?;
                }
                None => (),
            };
            let (endo_q, _endo_r) = commitment_dlog::srs::endos::<GAffineOther>();
            let urs_copy = Rc::clone(&*urs);
            let urs_copy2 = Rc::clone(&urs_copy);
            let t = index_serialization::read_plonk_verifier_index(
                oracle::pasta::fp::params(),
                oracle::pasta::fq::params(),
                endo_q,
                Rc::into_raw(urs_copy2),
                &mut r,
            )?;
            Ok((t, Rc::clone(&urs_copy)))
        }
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_verifier_index_read(
    offset: Option<ocaml::Int>,
    urs: CamlPastaFpUrs,
    path: String,
) -> Result<CamlPastaFpPlonkVerifierIndex, ocaml::Error> {
    let (vi, urs) = read_raw(offset, urs, path)?;
    Ok(to_ocaml(&urs, vi))
}

pub fn write_raw(
    append: Option<bool>,
    index: &DlogVerifierIndex<GAffine>,
    path: String,
) -> Result<(), ocaml::Error> {
    match OpenOptions::new().append(append.unwrap_or(true)).open(path) {
        Err(_) => Err(ocaml::Error::invalid_argument(
            "caml_pasta_fp_plonk_verifier_index_raw_read",
        )
        .err()
        .unwrap()),
        Ok(file) => {
            let mut w = BufWriter::new(file);

            Ok(index_serialization::write_plonk_verifier_index(
                index, &mut w,
            )?)
        }
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_verifier_index_write(
    append: Option<bool>,
    index: CamlPastaFpPlonkVerifierIndex,
    path: String,
) -> Result<(), ocaml::Error> {
    write_raw(append, &index.into(), path)
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_verifier_index_create(
    index: CamlPastaFpPlonkIndexPtr,
) -> CamlPastaFpPlonkVerifierIndex {
    let index = index.as_ref();
    to_ocaml(&index.1, index.0.verifier_index())
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_verifier_index_shifts(
    log2_size: ocaml::Int,
) -> CamlPlonkVerificationShifts<CamlFp> {
    let domain = Domain::new(1 << log2_size).unwrap();
    let (a, b): (Fp, Fp) = ConstraintSystem::sample_shifts(&domain);
    CamlPlonkVerificationShifts {
        r: a.into(),
        o: b.into(),
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_verifier_index_dummy() -> CamlPastaFpPlonkVerifierIndex {
    let g = || GAffine::prime_subgroup_generator();
    let comm = || PolyComm {
        shifted: Some(g()),
        unshifted: vec![g(), g(), g()],
    };
    CamlPlonkVerifierIndex {
        domain: CamlPlonkDomain {
            log_size_of_group: 1,
            group_gen: Fp::one().into(),
        },
        max_poly_size: 0,
        max_quot_size: 0,
        urs: caml_pointer::create(Rc::new(SRS::create(0))),
        evals: CamlPlonkVerificationEvals {
            sigma_comm0: comm().into(),
            sigma_comm1: comm().into(),
            sigma_comm2: comm().into(),
            ql_comm: comm().into(),
            qr_comm: comm().into(),
            qo_comm: comm().into(),
            qm_comm: comm().into(),
            qc_comm: comm().into(),
            rcm_comm0: comm().into(),
            rcm_comm1: comm().into(),
            rcm_comm2: comm().into(),
            psm_comm: comm().into(),
            add_comm: comm().into(),
            mul1_comm: comm().into(),
            mul2_comm: comm().into(),
            emul1_comm: comm().into(),
            emul2_comm: comm().into(),
            emul3_comm: comm().into(),
        },
        shifts: CamlPlonkVerificationShifts {
            r: Fp::one().into(),
            o: Fp::one().into(),
        },
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_verifier_index_deep_copy(
    x: CamlPastaFpPlonkVerifierIndex,
) -> CamlPastaFpPlonkVerifierIndex {
    x
}
