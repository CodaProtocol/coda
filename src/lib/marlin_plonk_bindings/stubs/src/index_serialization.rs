use ark_ec::AffineCurve;
use ark_ff::{Field, FromBytes, PrimeField, ToBytes};
use ark_poly::{
    univariate::DensePolynomial, EvaluationDomain, Evaluations, Radix2EvaluationDomain as Domain,
};
use commitment_dlog::{
    commitment::{CommitmentCurve, CommitmentField, PolyComm},
    srs::{SRSValue as PlonkSRSValue, SRS},
};
use oracle::poseidon::ArithmeticSpongeParams;
use plonk_circuits::{
    constraints::{zk_polynomial, zk_w, ConstraintSystem as PlonkConstraintSystem},
    domains::EvaluationDomains as PlonkEvaluationDomains,
};
use plonk_protocol_dlog::index::{Index as PlonkIndex, VerifierIndex as PlonkVerifierIndex};
use std::io::{Error, ErrorKind, Read, Result as IoResult, Write};

/// Serializes a [Vec] of some type `A` as `[8-byte length in little-endian, data]`
/// where `data` is the type serialized using its defined serialization ([ToBytes])
pub fn write_vec<A: ToBytes, W: Write>(v: &Vec<A>, mut writer: W) -> IoResult<()> {
    if v.len() > (u64::MAX as usize) {
        return Err(Error::new(ErrorKind::Other, "write_vec: vector too large"));
    }
    (v.len() as u64).write(&mut writer)?;
    for x in v {
        x.write(&mut writer)?;
    }
    Ok(())
}

/// Deserializes a [Vec] of some type `A` from a buffer `[8-byte length in little-endian, data]`
/// where `data` can be deserialized using the type's defined deserialization ([FromBytes])
pub fn read_vec<A: FromBytes, R: Read>(mut reader: R) -> IoResult<Vec<A>> {
    let mut v = vec![];
    let n = u64::read(&mut reader)? as usize;
    for _ in 0..n {
        v.push(A::read(&mut reader)?);
    }
    Ok(v)
}

/// Serializes an [Option] as `[0]` for [None] and `[1, data]`
/// where `data` is the serialized type using its defined serialization ([ToBytes])
pub fn write_option<A: ToBytes, W: Write>(a: &Option<A>, mut w: W) -> IoResult<()> {
    match a {
        None => u8::write(&0, &mut w),
        Some(a) => {
            1u8.write(&mut w)?;
            A::write(a, &mut w)
        }
    }
}

/// Deserializes an [Option] by reading `[0]` as [None] and `[1, data]` as [Some] `data`
/// where `data` can be deserialized using the type's defined deserialization ([FromBytes])
pub fn read_option<A: FromBytes, R: Read>(mut r: R) -> IoResult<Option<A>> {
    match u8::read(&mut r)? {
        0 => Ok(None),
        1 => Ok(Some(A::read(&mut r)?)),
        _ => Err(Error::new(ErrorKind::Other, "read_option: expected 0 or 1")),
    }
}

/// Serializes a [PolyComm] as a vector of `unshifted` (see [write_vec])
/// and an option of `shifted` (see [write_option])
pub fn write_poly_comm<A: ToBytes + AffineCurve, W: Write>(
    p: &PolyComm<A>,
    mut w: W,
) -> IoResult<()> {
    write_vec(&p.unshifted, &mut w)?;
    write_option(&p.shifted, &mut w)
}

/// Deserializes a [PolyComm] (see [write_poly_comm]).
pub fn read_poly_comm<A: FromBytes + AffineCurve, R: Read>(mut r: R) -> IoResult<PolyComm<A>> {
    let unshifted = read_vec(&mut r)?;
    let shifted = read_option(&mut r)?;
    Ok(PolyComm { unshifted, shifted })
}

/// Serializes a [DensePolynomial] as a vector (see [write_vec])
pub fn write_dense_polynomial<A: ToBytes + Field, W: Write>(
    p: &DensePolynomial<A>,
    w: W,
) -> IoResult<()> {
    write_vec(&p.coeffs, w)
}

/// Deserializes a [DensePolynomial] (see [write_dense_polynomial])
pub fn read_dense_polynomial<A: ToBytes + Field, R: Read>(r: R) -> IoResult<DensePolynomial<A>> {
    let coeffs = read_vec(r)?;
    Ok(DensePolynomial { coeffs })
}

/// Serializes a [Domain] as `size`
pub fn write_domain<A: ToBytes + PrimeField, W: Write>(d: &Domain<A>, mut w: W) -> IoResult<()> {
    (d.size as u64).write(&mut w)?;
    Ok(())
}

/// Deserializes a [Domain] (see [write_domain])
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

    {
        write_poly_comm(&vk.sigma_comm[0], &mut w)?;
        write_poly_comm(&vk.sigma_comm[1], &mut w)?;
        write_poly_comm(&vk.sigma_comm[2], &mut w)?;
    };
    write_poly_comm(&vk.ql_comm, &mut w)?;
    write_poly_comm(&vk.qr_comm, &mut w)?;
    write_poly_comm(&vk.qo_comm, &mut w)?;
    write_poly_comm(&vk.qm_comm, &mut w)?;
    write_poly_comm(&vk.qc_comm, &mut w)?;
    {
        write_poly_comm(&vk.rcm_comm[0], &mut w)?;
        write_poly_comm(&vk.rcm_comm[1], &mut w)?;
        write_poly_comm(&vk.rcm_comm[2], &mut w)?;
    };
    write_poly_comm(&vk.psm_comm, &mut w)?;
    write_poly_comm(&vk.add_comm, &mut w)?;
    write_poly_comm(&vk.mul1_comm, &mut w)?;
    write_poly_comm(&vk.mul2_comm, &mut w)?;
    write_poly_comm(&vk.emul1_comm, &mut w)?;
    write_poly_comm(&vk.emul2_comm, &mut w)?;
    write_poly_comm(&vk.emul3_comm, &mut w)?;

    G::ScalarField::write(&vk.r, &mut w)?;
    G::ScalarField::write(&vk.o, &mut w)?;

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
        let s0 = read_poly_comm(&mut r)?;
        let s1 = read_poly_comm(&mut r)?;
        let s2 = read_poly_comm(&mut r)?;
        [s0, s1, s2]
    };
    let ql_comm = read_poly_comm(&mut r)?;
    let qr_comm = read_poly_comm(&mut r)?;
    let qo_comm = read_poly_comm(&mut r)?;
    let qm_comm = read_poly_comm(&mut r)?;
    let qc_comm = read_poly_comm(&mut r)?;
    let rcm_comm = {
        let s0 = read_poly_comm(&mut r)?;
        let s1 = read_poly_comm(&mut r)?;
        let s2 = read_poly_comm(&mut r)?;
        [s0, s1, s2]
    };
    let psm_comm = read_poly_comm(&mut r)?;
    let add_comm = read_poly_comm(&mut r)?;
    let mul1_comm = read_poly_comm(&mut r)?;
    let mul2_comm = read_poly_comm(&mut r)?;
    let emul1_comm = read_poly_comm(&mut r)?;
    let emul2_comm = read_poly_comm(&mut r)?;
    let emul3_comm = read_poly_comm(&mut r)?;

    // the <_ as FromBytes> is to disambiguate the FromBytes trait from the Read trait
    // https://doc.rust-lang.org/rust-by-example/trait/disambiguating.html
    let r_value = <G::ScalarField as FromBytes>::read(&mut r)?;
    let o = <G::ScalarField as FromBytes>::read(&mut r)?;
    let srs = PlonkSRSValue::Ref(unsafe { &(*srs) });
    let vk = PlonkVerifierIndex {
        domain,
        w: zk_w(domain),
        zkpm: zk_polynomial(domain),
        max_poly_size,
        max_quot_size,
        srs,
        sigma_comm,
        ql_comm,
        qr_comm,
        qo_comm,
        qm_comm,
        qc_comm,
        rcm_comm,
        psm_comm,
        add_comm,
        mul1_comm,
        mul2_comm,
        emul1_comm,
        emul2_comm,
        emul3_comm,
        r: r_value,
        o,
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

    write_dense_polynomial(&c.sigmam[0], &mut w)?;
    write_dense_polynomial(&c.sigmam[1], &mut w)?;
    write_dense_polynomial(&c.sigmam[2], &mut w)?;

    write_dense_polynomial(&c.qlm, &mut w)?;
    write_dense_polynomial(&c.qrm, &mut w)?;
    write_dense_polynomial(&c.qom, &mut w)?;
    write_dense_polynomial(&c.qmm, &mut w)?;
    write_dense_polynomial(&c.qc, &mut w)?;

    write_dense_polynomial(&c.rcm[0], &mut w)?;
    write_dense_polynomial(&c.rcm[1], &mut w)?;
    write_dense_polynomial(&c.rcm[2], &mut w)?;

    write_dense_polynomial(&c.psm, &mut w)?;
    write_dense_polynomial(&c.addm, &mut w)?;
    write_dense_polynomial(&c.mul1m, &mut w)?;
    write_dense_polynomial(&c.mul2m, &mut w)?;
    write_dense_polynomial(&c.emul1m, &mut w)?;
    write_dense_polynomial(&c.emul2m, &mut w)?;
    write_dense_polynomial(&c.emul3m, &mut w)?;

    write_plonk_evaluations(&c.qll, &mut w)?;
    write_plonk_evaluations(&c.qrl, &mut w)?;
    write_plonk_evaluations(&c.qol, &mut w)?;
    write_plonk_evaluations(&c.qml, &mut w)?;

    write_vec(&c.sigmal1[0], &mut w)?;
    write_vec(&c.sigmal1[1], &mut w)?;
    write_vec(&c.sigmal1[2], &mut w)?;

    write_plonk_evaluations(&c.sigmal4[0], &mut w)?;
    write_plonk_evaluations(&c.sigmal4[1], &mut w)?;
    write_plonk_evaluations(&c.sigmal4[2], &mut w)?;

    write_vec(&c.sid, &mut w)?;

    write_plonk_evaluations(&c.ps4, &mut w)?;
    write_plonk_evaluations(&c.ps8, &mut w)?;
    write_plonk_evaluations(&c.addl4, &mut w)?;
    write_plonk_evaluations(&c.mul1l, &mut w)?;
    write_plonk_evaluations(&c.mul2l, &mut w)?;
    write_plonk_evaluations(&c.emul1l, &mut w)?;
    write_plonk_evaluations(&c.emul2l, &mut w)?;
    write_plonk_evaluations(&c.emul3l, &mut w)?;
    write_plonk_evaluations(&c.l04, &mut w)?;
    write_plonk_evaluations(&c.l08, &mut w)?;
    write_plonk_evaluations(&c.l1, &mut w)?;

    c.r.write(&mut w)?;
    c.o.write(&mut w)?;
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
        [s0, s1, s2]
    };

    let qlm = read_dense_polynomial(&mut r)?;
    let qrm = read_dense_polynomial(&mut r)?;
    let qom = read_dense_polynomial(&mut r)?;
    let qmm = read_dense_polynomial(&mut r)?;
    let qc = read_dense_polynomial(&mut r)?;

    let rcm = {
        let s0 = read_dense_polynomial(&mut r)?;
        let s1 = read_dense_polynomial(&mut r)?;
        let s2 = read_dense_polynomial(&mut r)?;
        [s0, s1, s2]
    };

    let psm = read_dense_polynomial(&mut r)?;
    let addm = read_dense_polynomial(&mut r)?;
    let mul1m = read_dense_polynomial(&mut r)?;
    let mul2m = read_dense_polynomial(&mut r)?;
    let emul1m = read_dense_polynomial(&mut r)?;
    let emul2m = read_dense_polynomial(&mut r)?;
    let emul3m = read_dense_polynomial(&mut r)?;

    let qll = read_plonk_evaluations(&mut r)?;
    let qrl = read_plonk_evaluations(&mut r)?;
    let qol = read_plonk_evaluations(&mut r)?;
    let qml = read_plonk_evaluations(&mut r)?;

    let sigmal1 = {
        let s0 = read_vec(&mut r)?;
        let s1 = read_vec(&mut r)?;
        let s2 = read_vec(&mut r)?;
        [s0, s1, s2]
    };

    let sigmal4 = {
        let s0 = read_plonk_evaluations(&mut r)?;
        let s1 = read_plonk_evaluations(&mut r)?;
        let s2 = read_plonk_evaluations(&mut r)?;
        [s0, s1, s2]
    };

    let sid = read_vec(&mut r)?;

    let ps4 = read_plonk_evaluations(&mut r)?;
    let ps8 = read_plonk_evaluations(&mut r)?;

    let addl4 = read_plonk_evaluations(&mut r)?;
    let mul1l = read_plonk_evaluations(&mut r)?;
    let mul2l = read_plonk_evaluations(&mut r)?;
    let emul1l = read_plonk_evaluations(&mut r)?;
    let emul2l = read_plonk_evaluations(&mut r)?;
    let emul3l = read_plonk_evaluations(&mut r)?;

    let l04 = read_plonk_evaluations(&mut r)?;
    let l08 = read_plonk_evaluations(&mut r)?;
    let l1 = read_plonk_evaluations(&mut r)?;

    // the <_ as FromBytes> is to disambiguate the FromBytes trait from the Read trait
    // https://doc.rust-lang.org/rust-by-example/trait/disambiguating.html
    let r_value = <G::ScalarField as FromBytes>::read(&mut r)?;
    let o = <G::ScalarField as FromBytes>::read(&mut r)?;
    let endo = <G::ScalarField as FromBytes>::read(&mut r)?;

    let zkpm = zk_polynomial(domain.d1);
    let zkpl = zkpm.evaluate_over_domain_by_ref(domain.d8);
    Ok(PlonkConstraintSystem {
        zkpm,
        zkpl,
        public,
        domain,
        gates,
        sigmam,
        qlm,
        qrm,
        qom,
        qmm,
        qc,
        rcm,
        psm,
        addm,
        mul1m,
        mul2m,
        emul1m,
        emul2m,
        emul3m,
        qll,
        qrl,
        qol,
        qml,
        sigmal1,
        sigmal4,
        sid,
        ps4,
        ps8,
        addl4,
        mul1l,
        mul2l,
        emul1l,
        emul2l,
        emul3l,
        l04,
        l08,
        l1,
        r: r_value,
        o,
        endo,
        fr_sponge_params,
    })
}
