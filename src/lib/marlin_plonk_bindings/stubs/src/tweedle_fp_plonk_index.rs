#[allow(unused_imports)]
use algebra::tweedle::{
    dee::{Affine as GAffine, TweedledeeParameters},
    dum::Affine as GAffineOther,
    fp::Fp,
};

use plonk_circuits::constraints::ConstraintSystem;
use plonk_circuits::gate::{CircuitGate, Gate};
use plonk_circuits::wires::{Col::*, GateWires, Wire};

use ff_fft::{EvaluationDomain, Radix2EvaluationDomain as Domain};

use commitment_dlog::srs::SRS;
use plonk_protocol_dlog::index::{Index as DlogIndex, SRSSpec};

use std::{
    fs::{File, OpenOptions},
    io::{BufReader, BufWriter, Seek, SeekFrom::Start},
    rc::Rc,
};

use crate::index_serialization;
use crate::plonk_gate::{CamlPlonkCol, CamlPlonkGate, CamlPlonkWire};
use crate::tweedle_fp_urs::CamlTweedleFpUrs;

pub struct CamlTweedleFpPlonkGateVector(Vec<Gate<Fp>>);
pub type CamlTweedleFpPlonkGateVectorPtr<'a> = ocaml::Pointer<'a, CamlTweedleFpPlonkGateVector>;

extern "C" fn caml_tweedle_fp_plonk_gate_vector_finalize(v: ocaml::Raw) {
    unsafe {
        let v: CamlTweedleFpPlonkGateVectorPtr = v.as_pointer();
        v.drop_in_place()
    };
}

ocaml::custom!(CamlTweedleFpPlonkGateVector {
    finalize: caml_tweedle_fp_plonk_gate_vector_finalize,
});

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_gate_vector_create() -> CamlTweedleFpPlonkGateVector {
    CamlTweedleFpPlonkGateVector(Vec::new())
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_gate_vector_add(
    mut v: CamlTweedleFpPlonkGateVectorPtr,
    gate: CamlPlonkGate<Vec<Fp>>,
) {
    v.as_mut().0.push(Gate {
        typ: gate.typ.into(),
        wires: gate.wires.into(),
        c: gate.c,
    });
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_gate_vector_get(
    v: CamlTweedleFpPlonkGateVectorPtr,
    i: ocaml::Int,
) -> CamlPlonkGate<Vec<Fp>> {
    let gate = &(v.as_ref().0)[i as usize];
    let c = gate.c.iter().map(|x| *x).collect();
    CamlPlonkGate {
        typ: (&gate.typ).into(),
        wires: (&gate.wires).into(),
        c,
    }
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_gate_vector_wrap(
    mut v: CamlTweedleFpPlonkGateVectorPtr,
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

pub struct CamlTweedleFpPlonkIndex<'a>(pub Box<DlogIndex<'a, GAffine>>, pub Rc<SRS<GAffine>>);
pub type CamlTweedleFpPlonkIndexPtr<'a> = ocaml::Pointer<'a, CamlTweedleFpPlonkIndex<'a>>;

extern "C" fn caml_tweedle_fp_plonk_index_finalize(v: ocaml::Raw) {
    unsafe {
        let mut v: CamlTweedleFpPlonkIndexPtr = v.as_pointer();
        v.as_mut_ptr().drop_in_place();
    }
}

ocaml::custom!(CamlTweedleFpPlonkIndex<'a> {
    finalize: caml_tweedle_fp_plonk_index_finalize,
});

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_index_create(
    gates: CamlTweedleFpPlonkGateVectorPtr,
    public: ocaml::Int,
    urs: CamlTweedleFpUrs,
) -> Result<CamlTweedleFpPlonkIndex<'static>, ocaml::Error> {
    let n = match Domain::<Fp>::compute_size_of_domain(gates.as_ref().0.len()) {
        None => Err(
            ocaml::Error::invalid_argument("caml_tweedle_fp_plonk_index_create")
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
        match ConstraintSystem::<Fp>::create(gates, oracle::tweedle::fp::params(), public as usize)
        {
            None => Err(ocaml::Error::failwith(
                "caml_tweedle_fp_plonk_index_create: could not create constraint system",
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
    Ok(CamlTweedleFpPlonkIndex(
        Box::new(DlogIndex::<GAffine>::create(
            cs,
            oracle::tweedle::fq::params(),
            endo_q,
            srs,
        )),
        urs_copy_outer,
    ))
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_index_max_degree(index: CamlTweedleFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.srs.get_ref().max_degree() as isize
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_index_public_inputs(index: CamlTweedleFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.cs.public as isize
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_index_domain_d1_size(index: CamlTweedleFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.cs.domain.d1.size() as isize
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_index_domain_d4_size(index: CamlTweedleFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.cs.domain.d4.size() as isize
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_index_domain_d8_size(index: CamlTweedleFpPlonkIndexPtr) -> ocaml::Int {
    index.as_ref().0.cs.domain.d8.size() as isize
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_index_read(
    offset: Option<ocaml::Int>,
    urs: CamlTweedleFpUrs,
    path: String,
) -> Result<CamlTweedleFpPlonkIndex<'static>, ocaml::Error> {
    let file = match File::open(path) {
        Err(_) => Err(
            ocaml::Error::invalid_argument("caml_tweedle_fp_plonk_index_read")
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
        oracle::tweedle::fp::params(),
        oracle::tweedle::fq::params(),
        srs,
        &mut r,
    )?;
    Ok(CamlTweedleFpPlonkIndex(Box::new(t), urs_copy_outer))
}

#[ocaml::func]
pub fn caml_tweedle_fp_plonk_index_write(
    append: Option<bool>,
    index: CamlTweedleFpPlonkIndexPtr<'static>,
    path: String,
) -> Result<(), ocaml::Error> {
    let file = match OpenOptions::new().append(append.unwrap_or(true)).open(path) {
        Err(_) => Err(
            ocaml::Error::invalid_argument("caml_tweedle_fp_plonk_index_write")
                .err()
                .unwrap(),
        )?,
        Ok(file) => file,
    };
    let mut w = BufWriter::new(file);
    index_serialization::write_plonk_index(&index.as_ref().0, &mut w)?;
    Ok(())
}
