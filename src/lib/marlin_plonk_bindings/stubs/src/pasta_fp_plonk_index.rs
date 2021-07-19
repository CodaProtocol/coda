use crate::arkworks::CamlFp;
use crate::index_serialization;
use crate::pasta_fp_urs::CamlPastaFpUrs;
use crate::plonk_gate::{CamlPlonkCol, CamlPlonkGate, CamlPlonkWire};
use ark_poly::{EvaluationDomain, Radix2EvaluationDomain as Domain};
use commitment_dlog::srs::{SRSSpec, SRS};
#[allow(unused_imports)]
use mina_curves::pasta::{
    fp::Fp,
    pallas::Affine as GAffineOther,
    vesta::{Affine as GAffine, VestaParameters},
};
use plonk_circuits::constraints::ConstraintSystem;
use plonk_circuits::gate::{CircuitGate, Gate};
use plonk_circuits::wires::{Col::*, GateWires, Wire};
use plonk_protocol_dlog::index::Index as DlogIndex;
use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Seek, SeekFrom::Start},
    rc::Rc,
};

pub struct CamlPastaFpPlonkGateVector(Vec<Gate<Fp>>);
pub type CamlPastaFpPlonkGateVectorPtr<'a> = ocaml::Pointer<'a, CamlPastaFpPlonkGateVector>;

extern "C" fn caml_pasta_fp_plonk_gate_vector_finalize(v: ocaml::Raw) {
    unsafe {
        let v: CamlPastaFpPlonkGateVectorPtr = v.as_pointer();
        v.drop_in_place()
    };
}

ocaml::custom!(CamlPastaFpPlonkGateVector {
    finalize: caml_pasta_fp_plonk_gate_vector_finalize,
});

#[ocaml::func]
pub fn caml_pasta_fp_plonk_gate_vector_create() -> CamlPastaFpPlonkGateVector {
    CamlPastaFpPlonkGateVector(Vec::new())
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_gate_vector_add(
    mut v: CamlPastaFpPlonkGateVectorPtr,
    gate: CamlPlonkGate<Vec<CamlFp>>,
) {
    v.as_mut().0.push(Gate {
        typ: gate.typ.into(),
        wires: gate.wires.into(),
        c: gate.c.iter().map(Into::into).collect(),
    });
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_gate_vector_get(
    v: CamlPastaFpPlonkGateVectorPtr,
    i: ocaml::Int,
) -> CamlPlonkGate<Vec<CamlFp>> {
    let gate = &(v.as_ref().0)[i as usize];
    let c = gate.c.iter().map(Into::into).collect();
    CamlPlonkGate {
        typ: (&gate.typ).into(),
        wires: (&gate.wires).into(),
        c,
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_gate_vector_wrap(
    mut v: CamlPastaFpPlonkGateVectorPtr,
    t: CamlPlonkWire,
    h: CamlPlonkWire,
) {
    match t.col {
        CamlPlonkCol::L => {
            (v.as_mut().0)[t.row as usize].wires.l = Wire {
                row: h.row as usize,
                col: h.col.into(),
            }
        }
        CamlPlonkCol::R => {
            (v.as_mut().0)[t.row as usize].wires.r = Wire {
                row: h.row as usize,
                col: h.col.into(),
            }
        }
        CamlPlonkCol::O => {
            (v.as_mut().0)[t.row as usize].wires.o = Wire {
                row: h.row as usize,
                col: h.col.into(),
            }
        }
    }
}

/* Boxed so that we don't store large proving indexes in the OCaml heap. */

pub struct CamlPastaFpPlonkIndex<'a>(pub Box<DlogIndex<'a, GAffine>>, pub Rc<SRS<GAffine>>);
pub type CamlPastaFpPlonkIndexPtr<'a> = ocaml::Pointer<'a, CamlPastaFpPlonkIndex<'a>>;

extern "C" fn caml_pasta_fp_plonk_index_finalize(v: ocaml::Raw) {
    unsafe {
        let mut v: CamlPastaFpPlonkIndexPtr = v.as_pointer();
        v.as_mut_ptr().drop_in_place();
    }
}

ocaml::custom!(CamlPastaFpPlonkIndex<'a> {
    finalize: caml_pasta_fp_plonk_index_finalize,
});

#[ocaml::func]
pub fn caml_pasta_fp_plonk_index_create(
    gates: CamlPastaFpPlonkGateVectorPtr,
    public: ocaml::Int,
    urs: CamlPastaFpUrs,
) -> Result<CamlPastaFpPlonkIndex<'static>, ocaml::Error> {
    let n = match Domain::<Fp>::compute_size_of_domain(gates.as_ref().0.len()) {
        None => Err(
            ocaml::Error::invalid_argument("caml_pasta_fp_plonk_index_create")
                .err()
                .unwrap(),
        )?,
        Some(n) => n,
    };
    let wire = |w: Wire| -> usize {
        match w.col {
            L => w.row,
            R => w.row + n,
            O => w.row + 2 * n,
        }
    };

    let gates: Vec<_> = gates
        .as_ref()
        .0
        .iter()
        .map(|gate| CircuitGate::<Fp> {
            typ: gate.typ.clone(),
            wires: GateWires {
                l: (gate.wires.row, wire(gate.wires.l)),
                r: (gate.wires.row + n, wire(gate.wires.r)),
                o: (gate.wires.row + 2 * n, wire(gate.wires.o)),
            },
            c: gate.c.clone(),
        })
        .collect();

    let (endo_q, _endo_r) = commitment_dlog::srs::endos::<GAffineOther>();
    let cs =
        match ConstraintSystem::<Fp>::create(gates, oracle::pasta::fp::params(), public as usize) {
            None => Err(ocaml::Error::failwith(
                "caml_pasta_fp_plonk_index_create: could not create constraint system",
            )
            .err()
            .unwrap())?,
            Some(cs) => cs,
        };
    let urs_copy = Rc::clone(&*urs);
    let urs_copy_outer = Rc::clone(&*urs);
    let srs = {
        // We know that the underlying value is still alive, because we never convert any of our
        // Rc<_>s into weak pointers.
        SRSSpec::Use(unsafe { &*Rc::into_raw(urs_copy) })
    };
    Ok(CamlPastaFpPlonkIndex(
        Box::new(DlogIndex::<GAffine>::create(
            cs,
            oracle::pasta::fq::params(),
            endo_q,
            srs,
        )),
        urs_copy_outer,
    ))
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_index_max_degree(index: CamlPastaFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.srs.get_ref().max_degree() as isize
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_index_public_inputs(index: CamlPastaFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.cs.public as isize
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_index_domain_d1_size(index: CamlPastaFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.cs.domain.d1.size() as isize
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_index_domain_d4_size(index: CamlPastaFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.cs.domain.d4.size() as isize
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_index_domain_d8_size(index: CamlPastaFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.cs.domain.d8.size() as isize
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_index_read(
    offset: Option<ocaml::Int>,
    urs: CamlPastaFpUrs,
    path: String,
) -> Result<CamlPastaFpPlonkIndex<'static>, ocaml::Error> {
    let file = match File::open(path) {
        Err(_) => Err(
            ocaml::Error::invalid_argument("caml_pasta_fp_plonk_index_read")
                .err()
                .unwrap(),
        )?,
        Ok(file) => file,
    };
    let mut r = BufReader::new(file);
    match offset {
        Some(offset) => {
            r.seek(Start(offset as u64))?;
        }
        None => (),
    };
    let urs_copy = Rc::clone(&*urs);
    let urs_copy_outer = Rc::clone(&*urs);
    let srs = {
        // We know that the underlying value is still alive, because we never convert any of our
        // Rc<_>s into weak pointers.
        unsafe { &*Rc::into_raw(urs_copy) }
    };
    let t = index_serialization::read_plonk_index(
        oracle::pasta::fp::params(),
        oracle::pasta::fq::params(),
        srs,
        &mut r,
    )?;
    Ok(CamlPastaFpPlonkIndex(Box::new(t), urs_copy_outer))
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_index_write(
    append: Option<bool>,
    index: CamlPastaFpPlonkIndexPtr<'static>,
    path: String,
) -> Result<(), ocaml::Error> {
    let file = match OpenOptions::new().append(append.unwrap_or(true)).open(path) {
        Err(_) => Err(
            ocaml::Error::invalid_argument("caml_pasta_fp_plonk_index_write")
                .err()
                .unwrap(),
        )?,
        Ok(file) => file,
    };
    let mut w = BufWriter::new(file);
    index_serialization::write_plonk_index(&index.as_ref().0, &mut w)?;
    Ok(())
}
