use crate::bigint_384;
use algebra::biginteger::BigInteger384;
use algebra::{
    bn_382::fq::{Fq, FqParameters as Fq_params},
    fields::{Field, FpParameters, PrimeField, SquareRootField},
    FftField, One, UniformRand, Zero,
};
use ff_fft::{EvaluationDomain, Radix2EvaluationDomain as Domain};
use num_bigint::BigUint;
use rand::rngs::StdRng;
use std::cmp::Ordering::{Equal, Greater, Less};

#[derive(Copy, Clone)]
pub struct CamlBn382Fq(pub Fq);

pub type CamlBn382FqPtr<'a> = ocaml::Pointer<'a, CamlBn382Fq>;

extern "C" fn caml_bn_382_fq_compare_raw(x: ocaml::Raw, y: ocaml::Raw) -> libc::c_int {
    let x: CamlBn382FqPtr = unsafe { x.as_pointer() };
    let y: CamlBn382FqPtr = unsafe { y.as_pointer() };

    match x.as_ref().0.cmp(&y.as_ref().0) {
        Less => -1,
        Equal => 0,
        Greater => 1,
    }
}

impl From<&CamlBn382Fq> for BigInteger384 {
    fn from(x: &CamlBn382Fq) -> BigInteger384 {
        x.0.into_repr()
    }
}

impl From<&BigInteger384> for CamlBn382Fq {
    fn from(x: &BigInteger384) -> CamlBn382Fq {
        CamlBn382Fq(Fq::from_repr(*x))
    }
}

impl std::fmt::Display for CamlBn382Fq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        bigint_384::to_biguint(&self.0.into_repr()).fmt(f)
    }
}

ocaml::custom!(CamlBn382Fq {
    compare: caml_bn_382_fq_compare_raw,
});

#[ocaml::func]
pub fn caml_bn_382_fq_size_in_bits() -> ocaml::Int {
    Fq_params::MODULUS_BITS as isize
}

#[ocaml::func]
pub fn caml_bn_382_fq_size() -> BigInteger384 {
    Fq_params::MODULUS
}

#[ocaml::func]
pub fn caml_bn_382_fq_add(x: CamlBn382FqPtr, y: CamlBn382FqPtr) -> CamlBn382Fq {
    CamlBn382Fq(x.as_ref().0 + y.as_ref().0)
}

#[ocaml::func]
pub fn caml_bn_382_fq_sub(x: CamlBn382FqPtr, y: CamlBn382FqPtr) -> CamlBn382Fq {
    CamlBn382Fq(x.as_ref().0 - y.as_ref().0)
}

#[ocaml::func]
pub fn caml_bn_382_fq_negate(x: CamlBn382FqPtr) -> CamlBn382Fq {
    CamlBn382Fq(-x.as_ref().0)
}

#[ocaml::func]
pub fn caml_bn_382_fq_mul(x: CamlBn382FqPtr, y: CamlBn382FqPtr) -> CamlBn382Fq {
    CamlBn382Fq(x.as_ref().0 * y.as_ref().0)
}

#[ocaml::func]
pub fn caml_bn_382_fq_div(x: CamlBn382FqPtr, y: CamlBn382FqPtr) -> CamlBn382Fq {
    CamlBn382Fq(x.as_ref().0 / y.as_ref().0)
}

#[ocaml::func]
pub fn caml_bn_382_fq_inv(x: CamlBn382FqPtr) -> Option<CamlBn382Fq> {
    match x.as_ref().0.inverse() {
        Some(x) => Some(CamlBn382Fq(x)),
        None => None,
    }
}

#[ocaml::func]
pub fn caml_bn_382_fq_square(x: CamlBn382FqPtr) -> CamlBn382Fq {
    CamlBn382Fq(x.as_ref().0.square())
}

#[ocaml::func]
pub fn caml_bn_382_fq_is_square(x: CamlBn382FqPtr) -> bool {
    let s = x.as_ref().0.pow(Fq_params::MODULUS_MINUS_ONE_DIV_TWO);
    s.is_zero() || s.is_one()
}

#[ocaml::func]
pub fn caml_bn_382_fq_sqrt(x: CamlBn382FqPtr) -> Option<CamlBn382Fq> {
    match x.as_ref().0.sqrt() {
        Some(x) => Some(CamlBn382Fq(x)),
        None => None,
    }
}

#[ocaml::func]
pub fn caml_bn_382_fq_of_int(i: ocaml::Int) -> CamlBn382Fq {
    CamlBn382Fq(Fq::from(i as u64))
}

#[ocaml::func]
pub fn caml_bn_382_fq_to_string(x: CamlBn382FqPtr) -> String {
    x.as_ref().to_string()
}

#[ocaml::func]
pub fn caml_bn_382_fq_of_string(s: &[u8]) -> Result<CamlBn382Fq, ocaml::Error> {
    match BigUint::parse_bytes(s, 10) {
        Some(data) => Ok(CamlBn382Fq::from(&(bigint_384::of_biguint(&data)))),
        None => Err(ocaml::Error::invalid_argument("caml_bn_382_fq_of_string")
            .err()
            .unwrap()),
    }
}

#[ocaml::func]
pub fn caml_bn_382_fq_print(x: CamlBn382FqPtr) {
    println!("{}", x.as_ref());
}

#[ocaml::func]
pub fn caml_bn_382_fq_copy(mut x: CamlBn382FqPtr, y: CamlBn382FqPtr) {
    *x.as_mut() = *y.as_ref()
}

#[ocaml::func]
pub fn caml_bn_382_fq_mut_add(mut x: CamlBn382FqPtr, y: CamlBn382FqPtr) {
    x.as_mut().0 += y.as_ref().0;
}

#[ocaml::func]
pub fn caml_bn_382_fq_mut_sub(mut x: CamlBn382FqPtr, y: CamlBn382FqPtr) {
    x.as_mut().0 -= y.as_ref().0;
}

#[ocaml::func]
pub fn caml_bn_382_fq_mut_mul(mut x: CamlBn382FqPtr, y: CamlBn382FqPtr) {
    x.as_mut().0 *= y.as_ref().0;
}

#[ocaml::func]
pub fn caml_bn_382_fq_mut_square(mut x: CamlBn382FqPtr) {
    x.as_mut().0.square_in_place();
}

#[ocaml::func]
pub fn caml_bn_382_fq_compare(x: CamlBn382FqPtr, y: CamlBn382FqPtr) -> ocaml::Int {
    match x.as_ref().0.cmp(&y.as_ref().0) {
        Less => -1,
        Equal => 0,
        Greater => 1,
    }
}

#[ocaml::func]
pub fn caml_bn_382_fq_equal(x: CamlBn382FqPtr, y: CamlBn382FqPtr) -> bool {
    x.as_ref().0 == y.as_ref().0
}

#[ocaml::func]
pub fn caml_bn_382_fq_random() -> CamlBn382Fq {
    CamlBn382Fq(UniformRand::rand(&mut rand::thread_rng()))
}

#[ocaml::func]
pub fn caml_bn_382_fq_rng(i: ocaml::Int) -> CamlBn382Fq {
    // We only care about entropy here, so we force a conversion i32 -> u32.
    let i: u64 = (i as u32).into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    CamlBn382Fq(UniformRand::rand(&mut rng))
}

#[ocaml::func]
pub fn caml_bn_382_fq_to_bigint(x: CamlBn382FqPtr) -> BigInteger384 {
    x.as_ref().into()
}

#[ocaml::func]
pub fn caml_bn_382_fq_of_bigint(x: ocaml::Pointer<BigInteger384>) -> CamlBn382Fq {
    x.as_ref().into()
}

#[ocaml::func]
pub fn caml_bn_382_fq_two_adic_root_of_unity() -> CamlBn382Fq {
    CamlBn382Fq(FftField::two_adic_root_of_unity())
}

#[ocaml::func]
pub fn caml_bn_382_fq_domain_generator(log2_size: ocaml::Int) -> Result<CamlBn382Fq, ocaml::Error> {
    match Domain::new(1 << log2_size) {
        Some(x) => Ok(CamlBn382Fq(x.group_gen)),
        None => Err(
            ocaml::Error::invalid_argument("caml_bn_382_fq_domain_generator")
                .err()
                .unwrap(),
        ),
    }
}

#[ocaml::func]
pub fn caml_bn_382_fq_to_bytes(x: CamlBn382FqPtr) -> ocaml::Value {
    let len = std::mem::size_of::<CamlBn382Fq>();
    let str = unsafe { ocaml::sys::caml_alloc_string(len) };
    unsafe {
        core::ptr::copy_nonoverlapping(x.as_ptr() as *const u8, ocaml::sys::string_val(str), len);
        ocaml::Value::new(str)
    }
}

#[ocaml::func]
pub fn caml_bn_382_fq_of_bytes(x: &[u8]) -> Result<CamlBn382Fq, ocaml::Error> {
    let len = std::mem::size_of::<CamlBn382Fq>();
    if x.len() != len {
        ocaml::Error::failwith("caml_bn_382_fq_of_bytes")?;
    };
    let x = unsafe { *(x.as_ptr() as *const CamlBn382Fq) };
    Ok(x)
}
