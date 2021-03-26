use crate::caml_pointer;
use crate::index_serialization_plookup;
use crate::plonk_verifier_index::{CamlPlonkDomain};
use crate::plonk_plookup_verifier_index::{
    CamlPlonkVerificationEvals, CamlPlonkVerificationShifts,
    CamlPlonkVerifierIndex,
};
use crate::pasta_fq_plonk_plookup_index::CamlPastaFqPlonkIndexPtr;
use crate::pasta_fq_urs::CamlPastaFqUrs;
use algebra::{
    curves::AffineCurve,
    pasta::{pallas::Affine as GAffine, vesta::Affine as GAffineOther, fq::Fq},
    One,
};

use ff_fft::{EvaluationDomain, Radix2EvaluationDomain as Domain};

use commitment_dlog::{commitment::PolyComm, srs::SRS};
use plonk_plookup_circuits::constraints::{zk_polynomial, zk_w1, zk_w3, ConstraintSystem};
use plonk_plookup_protocol_dlog::index::{SRSValue, VerifierIndex as DlogVerifierIndex};

use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Seek, SeekFrom::Start},
};

use std::rc::Rc;

pub type CamlPastaFqPlonkVerifierIndex =
    CamlPlonkVerifierIndex<Fq, CamlPastaFqUrs, PolyComm<GAffine>>;

pub fn to_ocaml<'a>(
    urs: &Rc<SRS<GAffine>>,
    vi: DlogVerifierIndex<'a, GAffine>,
) -> CamlPastaFqPlonkVerifierIndex {
    let [sigma_comm0, sigma_comm1, sigma_comm2, sigma_comm3, sigma_comm4] = vi.sigma_comm;
    let [rcm_comm0, rcm_comm1, rcm_comm2, rcm_comm3, rcm_comm4] = vi.rcm_comm;
    let [ql_comm, qr_comm, qo_comm, qq_comm, qp_comm] = vi.qw_comm;
    let [s0, s1, s2, s3, s4] = vi.shift;
    CamlPlonkVerifierIndex {
        domain: CamlPlonkDomain {
            log_size_of_group: vi.domain.log_size_of_group as isize,
            group_gen: vi.domain.group_gen,
        },
        max_poly_size: vi.max_poly_size as isize,
        max_quot_size: vi.max_quot_size as isize,
        urs: caml_pointer::create(Rc::clone(urs)),
        evals: CamlPlonkVerificationEvals {
            sigma_comm0: sigma_comm0,
            sigma_comm1: sigma_comm1,
            sigma_comm2: sigma_comm2,
            sigma_comm3: sigma_comm3,
            sigma_comm4: sigma_comm4,
            ql_comm: ql_comm,
            qr_comm: qr_comm,
            qo_comm: qo_comm,
            qq_comm: qq_comm,
            qp_comm: qp_comm,
            qm_comm: vi.qm_comm,
            qc_comm: vi.qc_comm,
            rcm_comm0: rcm_comm0,
            rcm_comm1: rcm_comm1,
            rcm_comm2: rcm_comm2,
            rcm_comm3: rcm_comm3,
            rcm_comm4: rcm_comm4,
            psm_comm: vi.psm_comm,
            add_comm: vi.add_comm,
            double_comm: vi.double_comm,
            mul1_comm: vi.mul1_comm,
            mul2_comm: vi.mul2_comm,
            emul_comm: vi.emul_comm,
            pack_comm: vi.pack_comm,
            lkp_comm: vi.lkp_comm,
            table_comm: vi.table_comm,
        },
        shifts: CamlPlonkVerificationShifts {
            s0: s0,
            s1: s1,
            s2: s2,
            s3: s3,
            s4: s4,
        },
    }
}

pub fn to_ocaml_copy<'a>(
    urs: &Rc<SRS<GAffine>>,
    vi: &DlogVerifierIndex<'a, GAffine>,
) -> CamlPastaFqPlonkVerifierIndex {
    let [sigma_comm0, sigma_comm1, sigma_comm2, sigma_comm3, sigma_comm4] = &vi.sigma_comm;
    let [rcm_comm0, rcm_comm1, rcm_comm2, rcm_comm3, rcm_comm4] = &vi.rcm_comm;
    let [ql_comm, qr_comm, qo_comm, qq_comm, qp_comm] = &vi.qw_comm;
    let [s0, s1, s2, s3, s4] = vi.shift;
    CamlPlonkVerifierIndex {
        domain: CamlPlonkDomain {
            log_size_of_group: vi.domain.log_size_of_group as isize,
            group_gen: vi.domain.group_gen,
        },
        max_poly_size: vi.max_poly_size as isize,
        max_quot_size: vi.max_quot_size as isize,
        urs: caml_pointer::create(Rc::clone(urs)),
        evals: CamlPlonkVerificationEvals {
            sigma_comm0: sigma_comm0.clone(),
            sigma_comm1: sigma_comm1.clone(),
            sigma_comm2: sigma_comm2.clone(),
            sigma_comm3: sigma_comm3.clone(),
            sigma_comm4: sigma_comm4.clone(),
            ql_comm: ql_comm.clone(),
            qr_comm: qr_comm.clone(),
            qo_comm: qo_comm.clone(),
            qq_comm: qq_comm.clone(),
            qp_comm: qp_comm.clone(),
            qm_comm: vi.qm_comm.clone(),
            qc_comm: vi.qc_comm.clone(),
            rcm_comm0: rcm_comm0.clone(),
            rcm_comm1: rcm_comm1.clone(),
            rcm_comm2: rcm_comm2.clone(),
            rcm_comm3: rcm_comm3.clone(),
            rcm_comm4: rcm_comm4.clone(),
            psm_comm: vi.psm_comm.clone(),
            add_comm: vi.add_comm.clone(),
            double_comm: vi.double_comm.clone(),
            mul1_comm: vi.mul1_comm.clone(),
            mul2_comm: vi.mul2_comm.clone(),
            emul_comm: vi.emul_comm.clone(),
            pack_comm: vi.pack_comm.clone(),
            lkp_comm: vi.lkp_comm.clone(),
            table_comm: vi.table_comm.clone(),
        },
        shifts: CamlPlonkVerificationShifts {
            s0: s0,
            s1: s1,
            s2: s2,
            s3: s3,
            s4: s4,
        },
    }
}

pub fn of_ocaml<'a>(
    max_poly_size: ocaml::Int,
    max_quot_size: ocaml::Int,
    log_size_of_group: ocaml::Int,
    urs: CamlPastaFqUrs,
    evals: CamlPlonkVerificationEvals<PolyComm<GAffine>>,
    shifts: CamlPlonkVerificationShifts<Fq>,
) -> (DlogVerifierIndex<'a, GAffine>, Rc<SRS<GAffine>>) {
    let urs_copy = Rc::clone(&*urs);
    let urs_copy_outer = Rc::clone(&*urs);
    let srs = {
        // We know that the underlying value is still alive, because we never convert any of our
        // Rc<_>s into weak pointers.
        SRSValue::Ref(unsafe { &*Rc::into_raw(urs_copy) })
    };
    let (endo_q, _endo_r) = commitment_dlog::srs::endos::<GAffineOther>();
    let domain = Domain::<Fq>::new(1 << log_size_of_group).unwrap();
    let index = DlogVerifierIndex::<GAffine> {
        domain,
        w1: zk_w1(domain),
        w3: zk_w3(domain),
        zkpm: zk_polynomial(domain),
        max_poly_size: max_poly_size as usize,
        max_quot_size: max_quot_size as usize,
        srs,
        sigma_comm: [
            evals.sigma_comm0,
            evals.sigma_comm1,
            evals.sigma_comm2,
            evals.sigma_comm3,
            evals.sigma_comm4,
        ],
        qw_comm: [
            evals.ql_comm,
            evals.qr_comm,
            evals.qo_comm,
            evals.qq_comm,
            evals.qp_comm,
        ],
        qm_comm: evals.qm_comm,
        qc_comm: evals.qc_comm,
        rcm_comm: [
            evals.rcm_comm0,
            evals.rcm_comm1,
            evals.rcm_comm2,
            evals.rcm_comm3,
            evals.rcm_comm4,
        ],
        psm_comm: evals.psm_comm,
        add_comm: evals.add_comm,
        double_comm: evals.double_comm,
        mul1_comm: evals.mul1_comm,
        mul2_comm: evals.mul2_comm,
        emul_comm: evals.emul_comm,
        pack_comm: evals.pack_comm,
        lkp_comm: evals.lkp_comm,
        table_comm: evals.table_comm,
        shift: [shifts.s0, shifts.s1, shifts.s2, shifts.s3, shifts.s4],
        fr_sponge_params: oracle::pasta::fq5::params(),
        fq_sponge_params: oracle::pasta::fp5::params(),
        endo: endo_q,
    };
    (index, urs_copy_outer)
}

impl From<CamlPastaFqPlonkVerifierIndex> for DlogVerifierIndex<'_, GAffine> {
    fn from(index: CamlPastaFqPlonkVerifierIndex) -> Self {
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
    urs: CamlPastaFqUrs,
    path: String,
) -> Result<(DlogVerifierIndex<'a, GAffine>, Rc<SRS<GAffine>>), ocaml::Error> {
    match File::open(path) {
        Err(_) => Err(ocaml::Error::invalid_argument(
            "caml_pasta_fq_plonk_plookup_verifier_index_raw_read",
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
            let t = index_serialization_plookup::read_plonk_verifier_index(
                oracle::pasta::fq5::params(),
                oracle::pasta::fp5::params(),
                endo_q,
                Rc::into_raw(urs_copy2),
                &mut r,
            )?;
            Ok((t, Rc::clone(&urs_copy)))
        }
    }
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_verifier_index_read(
    offset: Option<ocaml::Int>,
    urs: CamlPastaFqUrs,
    path: String,
) -> Result<CamlPastaFqPlonkVerifierIndex, ocaml::Error> {
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
            "caml_pasta_fq_plonk_plookup_verifier_index_raw_read",
        )
        .err()
        .unwrap()),
        Ok(file) => {
            let mut w = BufWriter::new(file);

            Ok(index_serialization_plookup::write_plonk_verifier_index(
                index, &mut w,
            )?)
        }
    }
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_verifier_index_write(
    append: Option<bool>,
    index: CamlPastaFqPlonkVerifierIndex,
    path: String,
) -> Result<(), ocaml::Error> {
    write_raw(append, &index.into(), path)
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_verifier_index_create(
    index: CamlPastaFqPlonkIndexPtr,
) -> CamlPastaFqPlonkVerifierIndex {
    let index = index.as_ref();
    to_ocaml(&index.1, index.0.verifier_index())
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_verifier_index_shifts(
    log2_size: ocaml::Int,
) -> CamlPlonkVerificationShifts<Fq> {
    let sh = ConstraintSystem::sample_shifts(&Domain::new(1 << log2_size).unwrap(), 4);
    CamlPlonkVerificationShifts {
        s0: Fq::one(),
        s1: sh[0],
        s2: sh[1],
        s3: sh[2],
        s4: sh[3],
    }
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_verifier_index_dummy() -> CamlPastaFqPlonkVerifierIndex {
    let g = || GAffine::prime_subgroup_generator();
    let comm = || PolyComm {
        shifted: Some(g()),
        unshifted: vec![g(), g(), g()],
    };
    CamlPlonkVerifierIndex {
        domain: CamlPlonkDomain {
            log_size_of_group: 1,
            group_gen: Fq::one(),
        },
        max_poly_size: 0,
        max_quot_size: 0,
        urs: caml_pointer::create(Rc::new(SRS::create(0))),
        evals: CamlPlonkVerificationEvals {
            sigma_comm0: comm(),
            sigma_comm1: comm(),
            sigma_comm2: comm(),
            sigma_comm3: comm(),
            sigma_comm4: comm(),
            ql_comm: comm(),
            qr_comm: comm(),
            qo_comm: comm(),
            qq_comm: comm(),
            qp_comm: comm(),
            qm_comm: comm(),
            qc_comm: comm(),
            rcm_comm0: comm(),
            rcm_comm1: comm(),
            rcm_comm2: comm(),
            rcm_comm3: comm(),
            rcm_comm4: comm(),
            psm_comm: comm(),
            add_comm: comm(),
            double_comm: comm(),
            mul1_comm: comm(),
            mul2_comm: comm(),
            emul_comm: comm(),
            pack_comm: comm(),
            lkp_comm: comm(),
            table_comm: comm(),
        },
        shifts: CamlPlonkVerificationShifts {
            s0: Fq::one(),
            s1: Fq::one(),
            s2: Fq::one(),
            s3: Fq::one(),
            s4: Fq::one(),
        },
    }
}

#[ocaml::func]
pub fn caml_pasta_fq_plonk_plookup_verifier_index_deep_copy(
    x: CamlPastaFqPlonkVerifierIndex,
) -> CamlPastaFqPlonkVerifierIndex {
    x
}
