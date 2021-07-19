use wasm_bindgen::prelude::*;
use wasm_bindgen::convert::{OptionIntoWasmAbi, IntoWasmAbi, FromWasmAbi};
use crate::bigint_256::{self, WasmBigInteger256};
use algebra::biginteger::BigInteger256;
use algebra::{ToBytes, FromBytes};
use mina_curves::pasta::fq::{Fq, FqParameters as Fq_params};
use algebra::{
    fields::{Field, FpParameters, PrimeField, SquareRootField},
    FftField, One, UniformRand, Zero,
};
use ff_fft::{EvaluationDomain, Radix2EvaluationDomain as Domain};
use num_bigint::BigUint;
use rand::rngs::StdRng;
use std::cmp::Ordering::{Equal, Greater, Less};

#[derive(Clone, Copy)]
pub struct WasmPastaFq(pub Fq);

impl wasm_bindgen::describe::WasmDescribe for WasmPastaFq {
    fn describe() { <Vec<u8> as wasm_bindgen::describe::WasmDescribe>::describe() }
}

impl FromWasmAbi for WasmPastaFq {
    type Abi = <Vec<u8> as FromWasmAbi>::Abi;
    unsafe fn from_abi(js: Self::Abi) -> Self {
        let bytes: Vec<u8> = FromWasmAbi::from_abi(js);
        WasmPastaFq(FromBytes::read(bytes.as_slice()).unwrap())
    }
}

impl IntoWasmAbi for WasmPastaFq {
    type Abi = <Vec<u8> as FromWasmAbi>::Abi;
    fn into_abi(self) -> Self::Abi {
        let mut bytes: Vec<u8> = vec![];
        self.0.write(&mut bytes);
        bytes.into_abi()
    }
}

impl OptionIntoWasmAbi for WasmPastaFq {
    fn none() -> Self::Abi {
        let max_bigint = WasmBigInteger256(BigInteger256([u64::MAX, u64::MAX, u64::MAX, u64::MAX]));
        max_bigint.into_abi()
    }
}

#[wasm_bindgen]
pub fn caml_pasta_fq_size_in_bits() -> isize {
    Fq_params::MODULUS_BITS as isize
}

#[wasm_bindgen]
pub fn caml_pasta_fq_size() -> WasmBigInteger256 {
    WasmBigInteger256(Fq_params::MODULUS)
}

#[wasm_bindgen]
pub fn caml_pasta_fq_add(x: WasmPastaFq, y: WasmPastaFq) -> WasmPastaFq {
    WasmPastaFq(x.0 + y.0)
}

#[wasm_bindgen]
pub fn caml_pasta_fq_sub(x: WasmPastaFq, y: WasmPastaFq) -> WasmPastaFq {
    WasmPastaFq(x.0 - y.0)
}

#[wasm_bindgen]
pub fn caml_pasta_fq_negate(x: WasmPastaFq) -> WasmPastaFq {
    WasmPastaFq(-x.0)
}

#[wasm_bindgen]
pub fn caml_pasta_fq_mul(x: WasmPastaFq, y: WasmPastaFq) -> WasmPastaFq {
    WasmPastaFq(x.0 * y.0)
}

#[wasm_bindgen]
pub fn caml_pasta_fq_div(x: WasmPastaFq, y: WasmPastaFq) -> WasmPastaFq {
    WasmPastaFq(x.0 / y.0)
}

#[wasm_bindgen]
pub fn caml_pasta_fq_inv(x: WasmPastaFq) -> Option<WasmPastaFq> {
    x.0.inverse().map(|x| { WasmPastaFq(x) })
}

#[wasm_bindgen]
pub fn caml_pasta_fq_square(x: WasmPastaFq) -> WasmPastaFq {
    WasmPastaFq(x.0.square())
}

#[wasm_bindgen]
pub fn caml_pasta_fq_is_square(x: WasmPastaFq) -> bool {
    let s = x.0.pow(Fq_params::MODULUS_MINUS_ONE_DIV_TWO);
    s.is_zero() || s.is_one()
}

#[wasm_bindgen]
pub fn caml_pasta_fq_sqrt(x: WasmPastaFq) -> Option<WasmPastaFq> {
    x.0.sqrt().map(|x| { WasmPastaFq(x) })
}

#[wasm_bindgen]
pub fn caml_pasta_fq_of_int(i: i32) -> WasmPastaFq {
    WasmPastaFq(Fq::from(i as u64))
}

#[wasm_bindgen]
pub fn caml_pasta_fq_to_string(x: WasmPastaFq) -> String {
    bigint_256::to_biguint(&x.0.into_repr()).to_string()
}

#[wasm_bindgen]
pub fn caml_pasta_fq_of_string(s: String) -> WasmPastaFq {
    match BigUint::parse_bytes(&s.into_bytes(), 10) {
        Some(data) => WasmPastaFq(Fq::from_repr(bigint_256::of_biguint(&data))),
        None => panic!("caml_pasta_fq_of_string"),
    }
}

#[wasm_bindgen]
pub fn caml_pasta_fq_print(x: WasmPastaFq) {
    println!("{}", bigint_256::to_biguint(&(x.0.into_repr())));
}

#[wasm_bindgen]
pub fn caml_pasta_fq_compare(x: WasmPastaFq, y: WasmPastaFq) -> i32 {
    match x.0.cmp(&y.0) {
        Less => -1,
        Equal => 0,
        Greater => 1,
    }
}

#[wasm_bindgen]
pub fn caml_pasta_fq_equal(x: WasmPastaFq, y: WasmPastaFq) -> bool {
    x.0 == y.0
}

#[wasm_bindgen]
pub fn caml_pasta_fq_random() -> WasmPastaFq {
    WasmPastaFq(UniformRand::rand(&mut rand::thread_rng()))
}

#[wasm_bindgen]
pub fn caml_pasta_fq_rng(i: i32) -> WasmPastaFq {
    // We only care about entropy here, so we force a conversion i32 -> u32.
    let i: u64 = (i as u32).into();
    let mut rng: StdRng = rand::SeedableRng::seed_from_u64(i);
    WasmPastaFq(UniformRand::rand(&mut rng))
}

#[wasm_bindgen]
pub fn caml_pasta_fq_to_bigint(x: WasmPastaFq) -> WasmBigInteger256 {
    WasmBigInteger256(x.0.into_repr())
}

#[wasm_bindgen]
pub fn caml_pasta_fq_of_bigint(x: WasmBigInteger256) -> WasmPastaFq {
    WasmPastaFq(Fq::from_repr(x.0))
}

#[wasm_bindgen]
pub fn caml_pasta_fq_two_adic_root_of_unity() -> WasmPastaFq {
    WasmPastaFq(FftField::two_adic_root_of_unity())
}

#[wasm_bindgen]
pub fn caml_pasta_fq_domain_generator(log2_size: i32) -> WasmPastaFq {
    match Domain::new(1 << log2_size) {
        Some(x) => WasmPastaFq(x.group_gen),
        None => panic!("caml_pasta_fq_domain_generator"),
    }
}

#[wasm_bindgen]
pub fn caml_pasta_fq_to_bytes(x: WasmPastaFq) -> Vec<u8> {
    let len = std::mem::size_of::<Fq>();
    let mut str: Vec<u8> = Vec::with_capacity(len);
    str.resize(len, 0);
    let str_as_fq : *mut Fq = str.as_mut_ptr().cast::<Fq>();
    unsafe {
        *str_as_fq = x.0;
    }
    str
}

#[wasm_bindgen]
pub fn caml_pasta_fq_of_bytes(x: &[u8]) -> WasmPastaFq {
    let len = std::mem::size_of::<Fq>();
    if x.len() != len {
        panic!("caml_pasta_fq_of_bytes");
    };
    let x = unsafe { *(x.as_ptr() as *const Fq) };
    WasmPastaFq(x)
}

#[wasm_bindgen]
pub fn caml_pasta_fq_deep_copy(x: WasmPastaFq) -> WasmPastaFq {
    x
}
