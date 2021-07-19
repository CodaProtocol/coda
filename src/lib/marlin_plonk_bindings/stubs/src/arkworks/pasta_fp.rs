use crate::arkworks::CamlBigInteger256;
use ark_ff::{FftField, Field, FpParameters, One, PrimeField, SquareRootField, UniformRand, Zero};
use ark_poly::{EvaluationDomain, Radix2EvaluationDomain as Domain};
use mina_curves::pasta::fp::{Fp, FpParameters as Fp_params};
use num_bigint::BigUint;
use rand::rngs::StdRng;
use std::{
    cmp::Ordering::{Equal, Greater, Less},
    convert::{TryFrom, TryInto},
    ops::Deref,
};

#[derive(Clone, Copy, Debug)]
pub struct CamlFp(pub Fp);

unsafe impl<'a> ocaml::FromValue<'a> for CamlFp {
    fn from_value(value: ocaml::Value) -> Self {
        let x: ocaml::Pointer<Self> = ocaml::FromValue::from_value(value);
        x.as_ref().clone()
    }
}

impl CamlFp {
    unsafe extern "C" fn caml_pointer_finalize(v: ocaml::Raw) {
        let ptr = v.as_pointer::<Self>();
        ptr.drop_in_place()
    }

    unsafe extern "C" fn ocaml_compare(x: ocaml::Raw, y: ocaml::Raw) -> i32 {
        let x = x.as_pointer::<Self>();
        let y = y.as_pointer::<Self>();
        match x.as_ref().0.cmp(&y.as_ref().0) {
            core::cmp::Ordering::Less => -1,
            core::cmp::Ordering::Equal => 0,
            core::cmp::Ordering::Greater => 1,
        }
    }
}

ocaml::custom!(CamlFp {
    finalize: CamlFp::caml_pointer_finalize,
    compare: CamlFp::ocaml_compare,
});

impl Deref for CamlFp {
    type Target = Fp;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

//
// Handy implementations
//

impl From<Fp> for CamlFp {
    fn from(fp: Fp) -> Self {
        CamlFp(fp)
    }
}

impl From<&Fp> for CamlFp {
    fn from(fp: &Fp) -> Self {
        CamlFp(*fp)
    }
}

impl Into<Fp> for CamlFp {
    fn into(self) -> Fp {
        self.0
    }
}

impl Into<Fp> for &CamlFp {
    fn into(self) -> Fp {
        self.0
    }
}

impl TryFrom<CamlBigInteger256> for CamlFp {
    type Error = ocaml::Error;
    fn try_from(x: CamlBigInteger256) -> Result<Self, Self::Error> {
        Fp::from_repr(x.0)
            .map(Into::into)
            .ok_or(ocaml::Error::Message(
                "TryFrom<CamlBigInteger256>: integer is larger than order",
            ))
    }
}

//
// Helpers
//

#[ocaml::func]
pub fn caml_pasta_fp_size_in_bits() -> ocaml::Int {
    Fp_params::MODULUS_BITS as isize
}

#[ocaml::func]
pub fn caml_pasta_fp_size() -> CamlBigInteger256 {
    Fp_params::MODULUS.into()
}

//
// Arithmetic methods
//

#[ocaml::func]
pub fn caml_pasta_fp_add(x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) -> CamlFp {
    CamlFp(x.as_ref().0 + y.as_ref().0)
}

#[ocaml::func]
pub fn caml_pasta_fp_sub(x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) -> CamlFp {
    CamlFp(x.as_ref().0 - y.as_ref().0)
}

#[ocaml::func]
pub fn caml_pasta_fp_negate(x: ocaml::Pointer<CamlFp>) -> CamlFp {
    CamlFp(-x.as_ref().0)
}

#[ocaml::func]
pub fn caml_pasta_fp_mul(x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) -> CamlFp {
    CamlFp(x.as_ref().0 * y.as_ref().0)
}

#[ocaml::func]
pub fn caml_pasta_fp_div(x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) -> CamlFp {
    CamlFp(x.as_ref().0 / y.as_ref().0)
}

#[ocaml::func]
pub fn caml_pasta_fp_inv(x: ocaml::Pointer<CamlFp>) -> Option<CamlFp> {
    x.as_ref().0.inverse().map(CamlFp)
}

#[ocaml::func]
pub fn caml_pasta_fp_square(x: ocaml::Pointer<CamlFp>) -> CamlFp {
    CamlFp(x.as_ref().0.square())
}

#[ocaml::func]
pub fn caml_pasta_fp_is_square(x: ocaml::Pointer<CamlFp>) -> bool {
    let s = x.as_ref().0.pow(Fp_params::MODULUS_MINUS_ONE_DIV_TWO);
    s.is_zero() || s.is_one()
}

#[ocaml::func]
pub fn caml_pasta_fp_sqrt(x: ocaml::Pointer<CamlFp>) -> Option<CamlFp> {
    x.as_ref().0.sqrt().map(CamlFp)
}

#[ocaml::func]
pub fn caml_pasta_fp_of_int(i: ocaml::Int) -> CamlFp {
    CamlFp(Fp::from(i as u64))
}

//
// Conversion methods
//

#[ocaml::func]
pub fn caml_pasta_fp_to_string(x: ocaml::Pointer<CamlFp>) -> String {
    CamlBigInteger256(x.as_ref().into_repr()).to_string()
}

#[ocaml::func]
pub fn caml_pasta_fp_of_string(s: &[u8]) -> Result<CamlFp, ocaml::Error> {
    let biguint = BigUint::parse_bytes(s, 10).ok_or(ocaml::Error::Message(
        "caml_pasta_fp_of_string: couldn't parse input",
    ))?;
    let camlbigint: CamlBigInteger256 = biguint
        .try_into()
        .map_err(|_| ocaml::Error::Message("caml_pasta_fp_of_string: Biguint is too large"))?;
    CamlFp::try_from(camlbigint).map_err(|_| ocaml::Error::Message("caml_pasta_fp_of_string"))
}

//
// Data methods
//

#[ocaml::func]
pub fn caml_pasta_fp_print(x: ocaml::Pointer<CamlFp>) {
    println!(
        "{}",
        CamlBigInteger256(x.as_ref().0.into_repr()).to_string()
    );
}

#[ocaml::func]
pub fn caml_pasta_fp_copy(mut x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) {
    *x.as_mut() = y.as_ref().clone()
}

#[ocaml::func]
pub fn caml_pasta_fp_mut_add(mut x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) {
    x.as_mut().0 += y.as_ref().0;
}

#[ocaml::func]
pub fn caml_pasta_fp_mut_sub(mut x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) {
    x.as_mut().0 -= y.as_ref().0;
}

#[ocaml::func]
pub fn caml_pasta_fp_mut_mul(mut x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) {
    x.as_mut().0 *= y.as_ref().0;
}

#[ocaml::func]
pub fn caml_pasta_fp_mut_square(mut x: ocaml::Pointer<CamlFp>) {
    x.as_mut().0.square_in_place();
}

#[ocaml::func]
pub fn caml_pasta_fp_compare(x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) -> ocaml::Int {
    match x.as_ref().0.cmp(&y.as_ref().0) {
        Less => -1,
        Equal => 0,
        Greater => 1,
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_equal(x: ocaml::Pointer<CamlFp>, y: ocaml::Pointer<CamlFp>) -> bool {
    x.as_ref().0 == y.as_ref().0
}

#[ocaml::func]
pub fn caml_pasta_fp_random() -> CamlFp {
    let fp: Fp = UniformRand::rand(&mut rand::thread_rng());
    CamlFp(fp)
}

#[ocaml::func]
pub fn caml_pasta_fp_rng(i: ocaml::Int) -> CamlFp {
    // We only care about entropy here, so we force a conversion i32 -> u32.
    let i: u64 = (i as u32).into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    let fp: Fp = UniformRand::rand(&mut rng);
    CamlFp(fp)
}

#[ocaml::func]
pub fn caml_pasta_fp_to_bigint(x: ocaml::Pointer<CamlFp>) -> CamlBigInteger256 {
    CamlBigInteger256(x.as_ref().0.into_repr())
}

#[ocaml::func]
pub fn caml_pasta_fp_of_bigint(x: CamlBigInteger256) -> Result<CamlFp, ocaml::Error> {
    Fp::from_repr(x.0).map(CamlFp).ok_or(ocaml::Error::Message(
        "caml_pasta_fp_of_bigint was given an invalid CamlBigInteger256",
    ))
}

#[ocaml::func]
pub fn caml_pasta_fp_two_adic_root_of_unity() -> CamlFp {
    let res: Fp = FftField::two_adic_root_of_unity();
    CamlFp(res)
}

#[ocaml::func]
pub fn caml_pasta_fp_domain_generator(log2_size: ocaml::Int) -> Result<CamlFp, ocaml::Error> {
    Domain::new(1 << log2_size)
        .map(|x| CamlFp(x.group_gen))
        .ok_or(ocaml::Error::Message("caml_pasta_fp_domain_generator"))
}

#[ocaml::func]
pub fn caml_pasta_fp_to_bytes(x: ocaml::Pointer<CamlFp>) -> ocaml::Value {
    let len = std::mem::size_of::<CamlFp>();
    let str = unsafe { ocaml::sys::caml_alloc_string(len) };
    unsafe {
        core::ptr::copy_nonoverlapping(x.as_ptr() as *const u8, ocaml::sys::string_val(str), len);
        ocaml::Value::new(str)
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_of_bytes(x: &[u8]) -> Result<CamlFp, ocaml::Error> {
    let len = std::mem::size_of::<CamlFp>();
    if x.len() != len {
        ocaml::Error::failwith("caml_pasta_fp_of_bytes")?;
    };
    let x = unsafe { *(x.as_ptr() as *const CamlFp) };
    Ok(x)
}

#[ocaml::func]
pub fn caml_pasta_fp_deep_copy(x: CamlFp) -> CamlFp {
    x
}
