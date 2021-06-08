use algebra::{
    curves::AffineCurve,
    fields::{Field, PrimeField},
    FromBytes, One, ToBytes,
};

use commitment_dlog::{
    commitment::{CommitmentCurve, CommitmentField, PolyComm},
    srs::SRS,
};
use ff_fft::{DensePolynomial, EvaluationDomain, Evaluations, Radix2EvaluationDomain as Domain};
use oracle::poseidon::ArithmeticSpongeParams;
use plonk_5_wires_circuits::{
    constraints::{zk_polynomial, zk_w, ConstraintSystem as PlonkConstraintSystem},
    domains::EvaluationDomains as PlonkEvaluationDomains,
    wires::COLUMNS,
};
use plonk_5_wires_protocol_dlog::index::{
    Index as PlonkIndex, SRSValue as PlonkSRSValue, VerifierIndex as PlonkVerifierIndex,
};
use std::io::{Error, ErrorKind, Read, Result as IoResult, Write};

pub fn write_vec<A: ToBytes, W: Write>(v: &Vec<A>, mut writer: W) -> IoResult<()> {
    u64::write(&(v.len() as u64), &mut writer)?;
    for x in v {
        x.write(&mut writer)?;
    }
    Ok(())
}

pub fn read_vec<A: FromBytes, R: Read>(mut reader: R) -> IoResult<Vec<A>> {
    let mut v = vec![];
    let n = u64::read(&mut reader)? as usize;
    for _ in 0..n {
        v.push(A::read(&mut reader)?);
    }
    Ok(v)
}

pub fn write_option<A: ToBytes, W: Write>(a: &Option<A>, mut w: W) -> IoResult<()> {
    match a {
        None => u8::write(&0, &mut w),
        Some(a) => {
            u8::write(&1, &mut w)?;
            A::write(a, &mut w)
        }
    }
}

pub fn read_option<A: FromBytes, R: Read>(mut r: R) -> IoResult<Option<A>> {
    match u8::read(&mut r)? {
        0 => Ok(None),
        1 => Ok(Some(A::read(&mut r)?)),
        _ => panic!("read_option: expected 0 or 1"),
    }
}

pub fn write_poly_comm<A: ToBytes + AffineCurve, W: Write>(
    p: &PolyComm<A>,
    mut w: W,
) -> IoResult<()> {
    write_vec(&p.unshifted, &mut w)?;
    write_option(&p.shifted, &mut w)
}

pub fn read_poly_comm<A: FromBytes + AffineCurve, R: Read>(mut r: R) -> IoResult<PolyComm<A>> {
    let unshifted = read_vec(&mut r)?;
    let shifted = read_option(&mut r)?;
    Ok(PolyComm { unshifted, shifted })
}

pub fn write_dense_polynomial<A: ToBytes + Field, W: Write>(
    p: &DensePolynomial<A>,
    w: W,
) -> IoResult<()> {
    write_vec(&p.coeffs, w)
}

pub fn read_dense_polynomial<A: ToBytes + Field, R: Read>(r: R) -> IoResult<DensePolynomial<A>> {
    let coeffs = read_vec(r)?;
    Ok(DensePolynomial { coeffs })
}

pub fn write_domain<A: ToBytes + PrimeField, W: Write>(d: &Domain<A>, mut w: W) -> IoResult<()> {
    (d.size as u64).write(&mut w)?;
    Ok(())
}

pub fn read_domain<A: ToBytes + PrimeField, R: Read>(mut r: R) -> IoResult<Domain<A>> {
    let size = u64::read(&mut r)?;
    match Domain::new(size as usize) {
        Some(d) => Ok(d),
        None => Err(Error::new(
            ErrorKind::Other,
            format!("Invalid domain size {}", size),
        )),
    }
}

pub fn write_plonk_evaluations<A: ToBytes + PrimeField, W: Write>(
    e: &Evaluations<A, Domain<A>>,
    mut w: W,
) -> IoResult<()> {
    write_vec(&e.evals, &mut w)?;
    Ok(())
}

pub fn read_plonk_evaluations<F: ToBytes + PrimeField, R: Read>(
    mut r: R,
) -> IoResult<Evaluations<F, Domain<F>>> {
    let evals = read_vec(&mut r)?;
    let domain = Domain::new(evals.len()).unwrap();
    assert_eq!(evals.len(), domain.size());
    Ok(Evaluations::<F, Domain<F>>::from_vec_and_domain(
        evals, domain,
    ))
}

pub fn write_plonk_index<'a, G: CommitmentCurve, W: Write>(
    k: &PlonkIndex<'a, G>,
    mut w: W,
) -> IoResult<()>
where
    G::ScalarField: CommitmentField + ToBytes,
{
    write_plonk_constraint_system::<G, &mut W>(&k.cs, &mut w)?;
    (k.max_poly_size as u64).write(&mut w)?;
    (k.max_quot_size as u64).write(&mut w)?;
    Ok(())
}

pub fn read_plonk_index<'a, G: CommitmentCurve, R: Read>(
    fr_sponge_params: ArithmeticSpongeParams<G::ScalarField>,
    fq_sponge_params: ArithmeticSpongeParams<G::BaseField>,
    srs: *const SRS<G>,
    mut r: R,
) -> IoResult<PlonkIndex<'a, G>>
where
    G::ScalarField: CommitmentField + FromBytes,
{
    let cs = read_plonk_constraint_system::<G, &mut R>(fr_sponge_params, &mut r)?;
    let max_poly_size = u64::read(&mut r)? as usize;
    let max_quot_size = u64::read(&mut r)? as usize;

    let srs = PlonkSRSValue::Ref(unsafe { &(*srs) });
    Ok(PlonkIndex {
        cs,
        srs,
        max_poly_size,
        max_quot_size,
        fq_sponge_params,
    })
}

pub fn write_plonk_verifier_index<'a, G: CommitmentCurve, W: Write>(
    vk: *const PlonkVerifierIndex<'a, G>,
    mut w: W,
) -> IoResult<()>
where
    G::ScalarField: CommitmentField + ToBytes,
{
    let vk = unsafe { &(*vk) };
    write_domain(&vk.domain, &mut w)?;
    u64::write(&(vk.max_poly_size as u64), &mut w)?;
    u64::write(&(vk.max_quot_size as u64), &mut w)?;

    for i in 0..COLUMNS {
        write_poly_comm(&vk.sigma_comm[i], &mut w)?
    }
    for i in 0..COLUMNS {
        write_poly_comm(&vk.qw_comm[i], &mut w)?
    }
    write_poly_comm(&vk.qm_comm, &mut w)?;
    write_poly_comm(&vk.qc_comm, &mut w)?;
    for i in 0..COLUMNS {
        write_poly_comm(&vk.rcm_comm[i], &mut w)?
    }
    write_poly_comm(&vk.psm_comm, &mut w)?;
    write_poly_comm(&vk.add_comm, &mut w)?;
    write_poly_comm(&vk.double_comm, &mut w)?;
    write_poly_comm(&vk.mul1_comm, &mut w)?;
    write_poly_comm(&vk.mul2_comm, &mut w)?;
    write_poly_comm(&vk.emul_comm, &mut w)?;
    write_poly_comm(&vk.pack_comm, &mut w)?;

    for i in 1..COLUMNS {
        G::ScalarField::write(&vk.shift[i], &mut w)?
    }

    Ok(())
}

pub fn read_plonk_verifier_index<'a, G: CommitmentCurve, R: Read>(
    fr_sponge_params: ArithmeticSpongeParams<G::ScalarField>,
    fq_sponge_params: ArithmeticSpongeParams<G::BaseField>,
    endo: G::ScalarField,
    srs: *const SRS<G>,
    mut r: R,
) -> IoResult<PlonkVerifierIndex<'a, G>>
where
    G::ScalarField: CommitmentField + FromBytes,
{
    let domain = read_domain(&mut r)?;
    let max_poly_size = u64::read(&mut r)? as usize;
    let max_quot_size = u64::read(&mut r)? as usize;

    let sigma_comm = {
        let c0 = read_poly_comm(&mut r)?;
        let c1 = read_poly_comm(&mut r)?;
        let c2 = read_poly_comm(&mut r)?;
        let c3 = read_poly_comm(&mut r)?;
        let c4 = read_poly_comm(&mut r)?;
        [c0, c1, c2, c3, c4]
    };
    let qw_comm = {
        let c0 = read_poly_comm(&mut r)?;
        let c1 = read_poly_comm(&mut r)?;
        let c2 = read_poly_comm(&mut r)?;
        let c3 = read_poly_comm(&mut r)?;
        let c4 = read_poly_comm(&mut r)?;
        [c0, c1, c2, c3, c4]
    };

    let qm_comm = read_poly_comm(&mut r)?;
    let qc_comm = read_poly_comm(&mut r)?;
    let rcm_comm = {
        let c0 = read_poly_comm(&mut r)?;
        let c1 = read_poly_comm(&mut r)?;
        let c2 = read_poly_comm(&mut r)?;
        let c3 = read_poly_comm(&mut r)?;
        let c4 = read_poly_comm(&mut r)?;
        [c0, c1, c2, c3, c4]
    };
    let psm_comm = read_poly_comm(&mut r)?;
    let add_comm = read_poly_comm(&mut r)?;
    let double_comm = read_poly_comm(&mut r)?;
    let mul1_comm = read_poly_comm(&mut r)?;
    let mul2_comm = read_poly_comm(&mut r)?;
    let emul_comm = read_poly_comm(&mut r)?;
    let pack_comm = read_poly_comm(&mut r)?;

    let shift = {
        let c1 = G::ScalarField::read(&mut r)?;
        let c2 = G::ScalarField::read(&mut r)?;
        let c3 = G::ScalarField::read(&mut r)?;
        let c4 = G::ScalarField::read(&mut r)?;
        [G::ScalarField::one(), c1, c2, c3, c4]
    };

    let srs = PlonkSRSValue::Ref(unsafe { &(*srs) });
    let vk = PlonkVerifierIndex {
        domain,
        w: zk_w(domain),
        zkpm: zk_polynomial(domain),
        max_poly_size,
        max_quot_size,
        srs,
        sigma_comm,
        qw_comm,
        qm_comm,
        qc_comm,
        rcm_comm,
        psm_comm,
        add_comm,
        double_comm,
        mul1_comm,
        mul2_comm,
        emul_comm,
        pack_comm,
        shift,
        fr_sponge_params,
        fq_sponge_params,
        endo,
    };
    Ok(vk)
}

pub fn write_plonk_constraint_system<G: CommitmentCurve, W: Write>(
    c: &PlonkConstraintSystem<G::ScalarField>,
    mut w: W,
) -> IoResult<()>
where
    G::ScalarField: CommitmentField + FromBytes,
{
    (c.public as u64).write(&mut w)?;
    {
        let PlonkEvaluationDomains { d1, d4, d8 } = c.domain;
        write_domain(&d1, &mut w)?;
        write_domain(&d4, &mut w)?;
        write_domain(&d8, &mut w)?;
    };

    write_vec(&c.gates, &mut w)?;

    for i in 0..COLUMNS {
        write_dense_polynomial(&c.sigmam[i], &mut w)?
    }
    for i in 0..COLUMNS {
        write_dense_polynomial(&c.qwm[i], &mut w)?
    }

    write_dense_polynomial(&c.qmm, &mut w)?;
    write_dense_polynomial(&c.qc, &mut w)?;

    for i in 0..COLUMNS {
        write_dense_polynomial(&c.rcm[i], &mut w)?
    }

    write_dense_polynomial(&c.psm, &mut w)?;
    write_dense_polynomial(&c.addm, &mut w)?;
    write_dense_polynomial(&c.doublem, &mut w)?;
    write_dense_polynomial(&c.mul1m, &mut w)?;
    write_dense_polynomial(&c.mul2m, &mut w)?;
    write_dense_polynomial(&c.emulm, &mut w)?;
    write_dense_polynomial(&c.packm, &mut w)?;

    for i in 0..COLUMNS {
        write_plonk_evaluations(&c.qwl[i], &mut w)?
    }
    write_plonk_evaluations(&c.qml, &mut w)?;

    for i in 0..COLUMNS {
        write_vec(&c.sigmal1[i], &mut w)?
    }
    for i in 0..COLUMNS {
        write_plonk_evaluations(&c.sigmal8[i], &mut w)?
    }

    write_vec(&c.sid, &mut w)?;

    write_plonk_evaluations(&c.ps4, &mut w)?;
    write_plonk_evaluations(&c.ps8, &mut w)?;
    write_plonk_evaluations(&c.addl, &mut w)?;
    write_plonk_evaluations(&c.doublel, &mut w)?;
    write_plonk_evaluations(&c.mul1l, &mut w)?;
    write_plonk_evaluations(&c.mul2l, &mut w)?;
    write_plonk_evaluations(&c.emull, &mut w)?;
    write_plonk_evaluations(&c.packl, &mut w)?;

    write_plonk_evaluations(&c.l1, &mut w)?;
    write_plonk_evaluations(&c.l04, &mut w)?;
    write_plonk_evaluations(&c.l08, &mut w)?;
    write_plonk_evaluations(&c.zero4, &mut w)?;
    write_plonk_evaluations(&c.zero8, &mut w)?;

    for i in 1..COLUMNS {
        G::ScalarField::write(&c.shift[i], &mut w)?
    }
    c.endo.write(&mut w)?;

    Ok(())
}

pub fn read_plonk_constraint_system<G: CommitmentCurve, R: Read>(
    fr_sponge_params: ArithmeticSpongeParams<G::ScalarField>,
    mut r: R,
) -> IoResult<PlonkConstraintSystem<G::ScalarField>>
where
    G::ScalarField: CommitmentField + FromBytes,
{
    let public = u64::read(&mut r)? as usize;
    let domain = {
        let d1 = read_domain(&mut r)?;
        let d4 = read_domain(&mut r)?;
        let d8 = read_domain(&mut r)?;
        PlonkEvaluationDomains { d1, d4, d8 }
    };

    let gates = read_vec(&mut r)?;

    let sigmam = {
        let s0 = read_dense_polynomial(&mut r)?;
        let s1 = read_dense_polynomial(&mut r)?;
        let s2 = read_dense_polynomial(&mut r)?;
        let s3 = read_dense_polynomial(&mut r)?;
        let s4 = read_dense_polynomial(&mut r)?;
        [s0, s1, s2, s3, s4]
    };

    let qwm = {
        let s0 = read_dense_polynomial(&mut r)?;
        let s1 = read_dense_polynomial(&mut r)?;
        let s2 = read_dense_polynomial(&mut r)?;
        let s3 = read_dense_polynomial(&mut r)?;
        let s4 = read_dense_polynomial(&mut r)?;
        [s0, s1, s2, s3, s4]
    };
    let qmm = read_dense_polynomial(&mut r)?;
    let qc = read_dense_polynomial(&mut r)?;

    let rcm = {
        let s0 = read_dense_polynomial(&mut r)?;
        let s1 = read_dense_polynomial(&mut r)?;
        let s2 = read_dense_polynomial(&mut r)?;
        let s3 = read_dense_polynomial(&mut r)?;
        let s4 = read_dense_polynomial(&mut r)?;
        [s0, s1, s2, s3, s4]
    };

    let psm = read_dense_polynomial(&mut r)?;
    let addm = read_dense_polynomial(&mut r)?;
    let doublem = read_dense_polynomial(&mut r)?;
    let mul1m = read_dense_polynomial(&mut r)?;
    let mul2m = read_dense_polynomial(&mut r)?;
    let emulm = read_dense_polynomial(&mut r)?;
    let packm = read_dense_polynomial(&mut r)?;

    let qwl = {
        let s0 = read_plonk_evaluations(&mut r)?;
        let s1 = read_plonk_evaluations(&mut r)?;
        let s2 = read_plonk_evaluations(&mut r)?;
        let s3 = read_plonk_evaluations(&mut r)?;
        let s4 = read_plonk_evaluations(&mut r)?;
        [s0, s1, s2, s3, s4]
    };
    let qml = read_plonk_evaluations(&mut r)?;

    let sigmal1 = {
        let s0 = read_vec(&mut r)?;
        let s1 = read_vec(&mut r)?;
        let s2 = read_vec(&mut r)?;
        let s3 = read_vec(&mut r)?;
        let s4 = read_vec(&mut r)?;
        [s0, s1, s2, s3, s4]
    };

    let sigmal8 = {
        let s0 = read_plonk_evaluations(&mut r)?;
        let s1 = read_plonk_evaluations(&mut r)?;
        let s2 = read_plonk_evaluations(&mut r)?;
        let s3 = read_plonk_evaluations(&mut r)?;
        let s4 = read_plonk_evaluations(&mut r)?;
        [s0, s1, s2, s3, s4]
    };

    let sid = read_vec(&mut r)?;

    let ps4 = read_plonk_evaluations(&mut r)?;
    let ps8 = read_plonk_evaluations(&mut r)?;

    let addl = read_plonk_evaluations(&mut r)?;
    let doublel = read_plonk_evaluations(&mut r)?;
    let mul1l = read_plonk_evaluations(&mut r)?;
    let mul2l = read_plonk_evaluations(&mut r)?;
    let emull = read_plonk_evaluations(&mut r)?;
    let packl = read_plonk_evaluations(&mut r)?;

    let l1 = read_plonk_evaluations(&mut r)?;
    let l04 = read_plonk_evaluations(&mut r)?;
    let l08 = read_plonk_evaluations(&mut r)?;
    let zero4 = read_plonk_evaluations(&mut r)?;
    let zero8 = read_plonk_evaluations(&mut r)?;

    let shift = {
        let c1 = G::ScalarField::read(&mut r)?;
        let c2 = G::ScalarField::read(&mut r)?;
        let c3 = G::ScalarField::read(&mut r)?;
        let c4 = G::ScalarField::read(&mut r)?;
        [G::ScalarField::one(), c1, c2, c3, c4]
    };
    let endo = G::ScalarField::read(&mut r)?;

    let zkpm = zk_polynomial(domain.d1);
    let zkpl = zkpm.evaluate_over_domain_by_ref(domain.d8);
    Ok(PlonkConstraintSystem {
        zkpm,
        zkpl,
        public,
        domain,
        gates,
        sigmam,
        qwm,
        qmm,
        qc,
        rcm,
        psm,
        addm,
        doublem,
        mul1m,
        mul2m,
        emulm,
        packm,
        qwl,
        qml,
        sigmal1,
        sigmal8,
        sid,
        ps4,
        ps8,
        addl,
        doublel,
        mul1l,
        mul2l,
        emull,
        packl,
        l04,
        l08,
        l1,
        zero4,
        zero8,
        shift,
        endo,
        fr_sponge_params,
    })
}
