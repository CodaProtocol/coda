use crate::bigint_256;
use algebra::biginteger::BigInteger256;
use algebra::{
    fields::{Field, FpParameters, PrimeField, SquareRootField},
    tweedle::fq::{Fq, FqParameters as Fq_params},
    FftField, One, UniformRand, Zero,
};
use ff_fft::{EvaluationDomain, Radix2EvaluationDomain as Domain};
use num_bigint::BigUint;
use rand::rngs::StdRng;
use std::cmp::Ordering::{Equal, Greater, Less};

#[ocaml::func]
pub fn caml_tweedle_fq_size_in_bits() -> ocaml::Int {
    Fq_params::MODULUS_BITS as isize
}

#[ocaml::func]
pub fn caml_tweedle_fq_size() -> BigInteger256 {
    Fq_params::MODULUS
}

#[ocaml::func]
pub fn caml_tweedle_fq_add(x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) -> Fq {
    *x.as_ref() + *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fq_sub(x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) -> Fq {
    *x.as_ref() - *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fq_negate(x: ocaml::Pointer<Fq>) -> Fq {
    -(*x.as_ref())
}

#[ocaml::func]
pub fn caml_tweedle_fq_mul(x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) -> Fq {
    *x.as_ref() * *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fq_div(x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) -> Fq {
    *x.as_ref() / *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fq_inv(x: ocaml::Pointer<Fq>) -> Option<Fq> {
    x.as_ref().inverse()
}

#[ocaml::func]
pub fn caml_tweedle_fq_square(x: ocaml::Pointer<Fq>) -> Fq {
    x.as_ref().square()
}

#[ocaml::func]
pub fn caml_tweedle_fq_is_square(x: ocaml::Pointer<Fq>) -> bool {
    let s = x.as_ref().pow(Fq_params::MODULUS_MINUS_ONE_DIV_TWO);
    s.is_zero() || s.is_one()
}

#[ocaml::func]
pub fn caml_tweedle_fq_sqrt(x: ocaml::Pointer<Fq>) -> Option<Fq> {
    x.as_ref().sqrt()
}

#[ocaml::func]
pub fn caml_tweedle_fq_of_int(i: ocaml::Int) -> Fq {
    Fq::from(i as u64)
}

#[ocaml::func]
pub fn caml_tweedle_fq_to_string(x: ocaml::Pointer<Fq>) -> String {
    bigint_256::to_biguint(&x.as_ref().into_repr()).to_string()
}

#[ocaml::func]
pub fn caml_tweedle_fq_of_string(s: &[u8]) -> Result<Fq, ocaml::Error> {
    match BigUint::parse_bytes(s, 10) {
        Some(data) => Ok(Fq::from_repr(bigint_256::of_biguint(&data))),
        None => Err(ocaml::Error::invalid_argument("caml_tweedle_fq_of_string")
            .err()
            .unwrap()),
    }
}

#[ocaml::func]
pub fn caml_tweedle_fq_print(x: ocaml::Pointer<Fq>) {
    println!("{}", bigint_256::to_biguint(&x.as_ref().into_repr()));
}

#[ocaml::func]
pub fn caml_tweedle_fq_copy(mut x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) {
    *x.as_mut() = *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fq_mut_add(mut x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) {
    *x.as_mut() += *y.as_ref();
}

#[ocaml::func]
pub fn caml_tweedle_fq_mut_sub(mut x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) {
    *x.as_mut() -= *y.as_ref();
}

#[ocaml::func]
pub fn caml_tweedle_fq_mut_mul(mut x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) {
    *x.as_mut() *= *y.as_ref();
}

#[ocaml::func]
pub fn caml_tweedle_fq_mut_square(mut x: ocaml::Pointer<Fq>) {
    x.as_mut().square_in_place();
}

#[ocaml::func]
pub fn caml_tweedle_fq_compare(x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) -> ocaml::Int {
    match x.as_ref().cmp(&y.as_ref()) {
        Less => -1,
        Equal => 0,
        Greater => 1,
    }
}

#[ocaml::func]
pub fn caml_tweedle_fq_equal(x: ocaml::Pointer<Fq>, y: ocaml::Pointer<Fq>) -> bool {
    *x.as_ref() == *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fq_random() -> Fq {
    UniformRand::rand(&mut rand::thread_rng())
}

#[ocaml::func]
pub fn caml_tweedle_fq_rng(i: ocaml::Int) -> Fq {
    // We only care about entropy here, so we force a conversion i32 -> u32.
    let i: u64 = (i as u32).into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    UniformRand::rand(&mut rng)
}

#[ocaml::func]
pub fn caml_tweedle_fq_to_bigint(x: ocaml::Pointer<Fq>) -> BigInteger256 {
    x.as_ref().into_repr()
}

#[ocaml::func]
pub fn caml_tweedle_fq_of_bigint(x: BigInteger256) -> Fq {
    Fq::from_repr(x)
}

#[ocaml::func]
pub fn caml_tweedle_fq_two_adic_root_of_unity() -> Fq {
    FftField::two_adic_root_of_unity()
}

#[ocaml::func]
pub fn caml_tweedle_fq_domain_generator(log2_size: ocaml::Int) -> Result<Fq, ocaml::Error> {
    match Domain::new(1 << log2_size) {
        Some(x) => Ok(x.group_gen),
        None => Err(
            ocaml::Error::invalid_argument("caml_tweedle_fq_domain_generator")
                .err()
                .unwrap(),
        ),
    }
}

#[ocaml::func]
pub fn caml_tweedle_fq_to_bytes(x: ocaml::Pointer<Fq>) -> ocaml::Value {
    let len = std::mem::size_of::<Fq>();
    let str = unsafe { ocaml::sys::caml_alloc_string(len) };
    unsafe {
        core::ptr::copy_nonoverlapping(x.as_ptr() as *const u8, ocaml::sys::string_val(str), len);
        ocaml::Value::new(str)
    }
}

#[ocaml::func]
pub fn caml_tweedle_fq_of_bytes(x: &[u8]) -> Result<Fq, ocaml::Error> {
    let len = std::mem::size_of::<Fq>();
    if x.len() != len {
        ocaml::Error::failwith("caml_tweedle_fq_of_bytes")?;
    };
    let x = unsafe { *(x.as_ptr() as *const Fq) };
    Ok(x)
}

#[ocaml::func]
pub fn caml_tweedle_fq_deep_copy(x: Fq) -> Fq {
    x
}
