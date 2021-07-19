use crate::{
    arkworks::{CamlFp, CamlGVesta},
    caml_pointer::{self, CamlPointer},
};
use ark_ff::{One, Zero};
use ark_poly::UVPolynomial;
use ark_poly::{univariate::DensePolynomial, EvaluationDomain, Evaluations};
use commitment_dlog::{
    commitment::{b_poly_coefficients, caml::CamlPolyComm},
    srs::SRS,
};
use mina_curves::pasta::{fp::Fp, vesta::Affine as GAffine};
use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Seek, SeekFrom::Start},
    rc::Rc,
};

pub type CamlPastaFpUrs = CamlPointer<Rc<SRS<GAffine>>>;

#[ocaml::func]
pub fn caml_pasta_fp_urs_create(depth: ocaml::Int) -> CamlPastaFpUrs {
    caml_pointer::create(Rc::new(SRS::create(depth as usize)))
}

#[ocaml::func]
pub fn caml_pasta_fp_urs_write(
    append: Option<bool>,
    urs: CamlPastaFpUrs,
    path: String,
) -> Result<(), ocaml::Error> {
    match OpenOptions::new().append(append.unwrap_or(true)).open(path) {
        Err(_) => Err(ocaml::Error::invalid_argument("caml_pasta_fp_urs_write")
            .err()
            .unwrap()),
        Ok(file) => {
            let file = BufWriter::new(file);
            let urs: &SRS<GAffine> = &*urs;
            let _ = (*urs).write(file);
            Ok(())
        }
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_urs_read(
    offset: Option<ocaml::Int>,
    path: String,
) -> Result<Option<CamlPastaFpUrs>, ocaml::Error> {
    match File::open(path) {
        Err(_) => Err(ocaml::Error::invalid_argument("caml_pasta_fp_urs_read")
            .err()
            .unwrap()),
        Ok(file) => {
            let mut file = BufReader::new(file);
            match offset {
                Some(offset) => {
                    file.seek(Start(offset as u64))?;
                }
                None => (),
            };
            match SRS::<GAffine>::read(file) {
                Err(_) => Ok(None),
                Ok(urs) => Ok(Some(caml_pointer::create(Rc::new(urs)))),
            }
        }
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_urs_lagrange_commitment(
    urs: CamlPastaFpUrs,
    domain_size: ocaml::Int,
    i: ocaml::Int,
) -> Result<CamlPolyComm<CamlGVesta>, ocaml::Error> {
    match EvaluationDomain::<Fp>::new(domain_size as usize) {
        None => Err(
            ocaml::Error::invalid_argument("caml_pasta_fp_urs_lagrange_commitment")
                .err()
                .unwrap(),
        ),
        Some(x_domain) => {
            let evals = (0..domain_size)
                .map(|j| if i == j { Fp::one() } else { Fp::zero() })
                .collect();
            let p = Evaluations::<Fp>::from_vec_and_domain(evals, x_domain).interpolate();
            Ok((*urs).commit_non_hiding(&p, None).into())
        }
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_urs_commit_evaluations(
    urs: CamlPastaFpUrs,
    domain_size: ocaml::Int,
    evals: Vec<CamlFp>,
) -> Result<CamlPolyComm<CamlGVesta>, ocaml::Error> {
    match EvaluationDomain::<Fp>::new(domain_size as usize) {
        None => Err(
            ocaml::Error::invalid_argument("caml_pasta_fp_urs_commit_evaluations")
                .err()
                .unwrap(),
        ),
        Some(x_domain) => {
            let evals = evals.into_iter().map(Into::into).collect();
            let p = Evaluations::<Fp>::from_vec_and_domain(evals, x_domain).interpolate();
            Ok((*urs).commit_non_hiding(&p, None).into())
        }
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_urs_b_poly_commitment(
    urs: CamlPastaFpUrs,
    chals: Vec<CamlFp>,
) -> Result<CamlPolyComm<CamlGVesta>, ocaml::Error> {
    let chals: Vec<Fp> = chals.into_iter().map(Into::into).collect();
    let coeffs = b_poly_coefficients(&chals);
    let p = DensePolynomial::<Fp>::from_coefficients_vec(coeffs);
    Ok((*urs).commit_non_hiding(&p, None).into())
}

#[ocaml::func]
pub fn caml_pasta_fp_urs_batch_accumulator_check(
    urs: CamlPastaFpUrs,
    comms: Vec<CamlGVesta>,
    chals: Vec<CamlFp>,
) -> bool {
    crate::urs_utils::batch_dlog_accumulator_check(
        &*urs,
        &comms.into_iter().map(Into::into).collect(),
        &chals.into_iter().map(Into::into).collect(),
    )
}

#[ocaml::func]
pub fn caml_pasta_fp_urs_h(urs: CamlPastaFpUrs) -> CamlGVesta {
    (*urs).h.into()
}
