use crate::bigint_256;
use algebra::biginteger::BigInteger256;
use algebra::{
    fields::{Field, FpParameters, PrimeField, SquareRootField},
    tweedle::fp::{Fp, FpParameters as Fp_params},
    FftField, One, UniformRand, Zero,
};
use ff_fft::{EvaluationDomain, Radix2EvaluationDomain as Domain};
use num_bigint::BigUint;
use rand::rngs::StdRng;
use std::cmp::Ordering::{Equal, Greater, Less};

#[ocaml::func]
pub fn caml_tweedle_fp_size_in_bits() -> ocaml::Int {
    Fp_params::MODULUS_BITS as isize
}

#[ocaml::func]
pub fn caml_tweedle_fp_size() -> BigInteger256 {
    Fp_params::MODULUS
}

#[ocaml::func]
pub fn caml_tweedle_fp_add(x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) -> Fp {
    *x.as_ref() + *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fp_sub(x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) -> Fp {
    *x.as_ref() - *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fp_negate(x: ocaml::Pointer<Fp>) -> Fp {
    -(*x.as_ref())
}

#[ocaml::func]
pub fn caml_tweedle_fp_mul(x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) -> Fp {
    *x.as_ref() * *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fp_div(x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) -> Fp {
    *x.as_ref() / *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fp_inv(x: ocaml::Pointer<Fp>) -> Option<Fp> {
    x.as_ref().inverse()
}

#[ocaml::func]
pub fn caml_tweedle_fp_square(x: ocaml::Pointer<Fp>) -> Fp {
    x.as_ref().square()
}

#[ocaml::func]
pub fn caml_tweedle_fp_is_square(x: ocaml::Pointer<Fp>) -> bool {
    let s = x.as_ref().pow(Fp_params::MODULUS_MINUS_ONE_DIV_TWO);
    s.is_zero() || s.is_one()
}

#[ocaml::func]
pub fn caml_tweedle_fp_sqrt(x: ocaml::Pointer<Fp>) -> Option<Fp> {
    x.as_ref().sqrt()
}

#[ocaml::func]
pub fn caml_tweedle_fp_of_int(i: ocaml::Int) -> Fp {
    Fp::from(i as u64)
}

#[ocaml::func]
pub fn caml_tweedle_fp_to_string(x: ocaml::Pointer<Fp>) -> String {
    bigint_256::to_biguint(&x.as_ref().into_repr()).to_string()
}

#[ocaml::func]
pub fn caml_tweedle_fp_of_string(s: &[u8]) -> Result<Fp, ocaml::Error> {
    match BigUint::parse_bytes(s, 10) {
        Some(data) => Ok(Fp::from_repr(bigint_256::of_biguint(&data))),
        None => Err(ocaml::Error::invalid_argument("caml_tweedle_fp_of_string")
            .err()
            .unwrap()),
    }
}

#[ocaml::func]
pub fn caml_tweedle_fp_print(x: ocaml::Pointer<Fp>) {
    println!("{}", bigint_256::to_biguint(&x.as_ref().into_repr()));
}

#[ocaml::func]
pub fn caml_tweedle_fp_copy(mut x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) {
    *x.as_mut() = *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fp_mut_add(mut x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) {
    *x.as_mut() += *y.as_ref();
}

#[ocaml::func]
pub fn caml_tweedle_fp_mut_sub(mut x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) {
    *x.as_mut() -= *y.as_ref();
}

#[ocaml::func]
pub fn caml_tweedle_fp_mut_mul(mut x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) {
    *x.as_mut() *= *y.as_ref();
}

#[ocaml::func]
pub fn caml_tweedle_fp_mut_square(mut x: ocaml::Pointer<Fp>) {
    x.as_mut().square_in_place();
}

#[ocaml::func]
pub fn caml_tweedle_fp_compare(x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) -> ocaml::Int {
    match x.as_ref().cmp(&y.as_ref()) {
        Less => -1,
        Equal => 0,
        Greater => 1,
    }
}

#[ocaml::func]
pub fn caml_tweedle_fp_equal(x: ocaml::Pointer<Fp>, y: ocaml::Pointer<Fp>) -> bool {
    *x.as_ref() == *y.as_ref()
}

#[ocaml::func]
pub fn caml_tweedle_fp_random() -> Fp {
    UniformRand::rand(&mut rand::thread_rng())
}

#[ocaml::func]
pub fn caml_tweedle_fp_rng(i: ocaml::Int) -> Fp {
    // We only care about entropy here, so we force a conversion i32 -> u32.
    let i: u64 = (i as u32).into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    UniformRand::rand(&mut rng)
}

#[ocaml::func]
pub fn caml_tweedle_fp_to_bigint(x: ocaml::Pointer<Fp>) -> BigInteger256 {
    x.as_ref().into_repr()
}

#[ocaml::func]
pub fn caml_tweedle_fp_of_bigint(x: BigInteger256) -> Fp {
    Fp::from_repr(x)
}

#[ocaml::func]
pub fn caml_tweedle_fp_two_adic_root_of_unity() -> Fp {
    FftField::two_adic_root_of_unity()
}

#[ocaml::func]
pub fn caml_tweedle_fp_domain_generator(log2_size: ocaml::Int) -> Result<Fp, ocaml::Error> {
    match Domain::new(1 << log2_size) {
        Some(x) => Ok(x.group_gen),
        None => Err(
            ocaml::Error::invalid_argument("caml_tweedle_fp_domain_generator")
                .err()
                .unwrap(),
        ),
    }
}

#[ocaml::func]
pub fn caml_tweedle_fp_to_bytes(x: ocaml::Pointer<Fp>) -> ocaml::Value {
    let len = std::mem::size_of::<Fp>();
    let str = unsafe { ocaml::sys::caml_alloc_string(len) };
    unsafe {
        core::ptr::copy_nonoverlapping(x.as_ptr() as *const u8, ocaml::sys::string_val(str), len);
        ocaml::Value::new(str)
    }
}

#[ocaml::func]
pub fn caml_tweedle_fp_of_bytes(x: &[u8]) -> Result<Fp, ocaml::Error> {
    let len = std::mem::size_of::<Fp>();
    if x.len() != len {
        ocaml::Error::failwith("caml_tweedle_fp_of_bytes")?;
    };
    let x = unsafe { *(x.as_ptr() as *const Fp) };
    Ok(x)
}

#[ocaml::func]
pub fn caml_tweedle_fp_deep_copy(x: Fp) -> Fp {
    x
}
