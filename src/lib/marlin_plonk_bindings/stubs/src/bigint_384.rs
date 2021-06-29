use algebra::biginteger::{BigInteger, BigInteger384};
use num_bigint::BigUint;
use std::cmp::Ordering::{Equal, Greater, Less};
use std::convert::TryInto;

const BIGINT384_NUM_BITS: i32 = 384;
const BIGINT384_LIMB_BITS: i32 = 64;
const BIGINT384_LIMB_BYTES: i32 = BIGINT384_LIMB_BITS / 8;
const BIGINT384_NUM_LIMBS: i32 =
    (BIGINT384_NUM_BITS + BIGINT384_LIMB_BITS - 1) / BIGINT384_LIMB_BITS;
const BIGINT384_NUM_BYTES: usize = (BIGINT384_NUM_LIMBS as usize) * 8;

pub fn to_biguint(x: &BigInteger384) -> BigUint {
    let x_ = x.0.as_ptr() as *const u8;
    let x_ = unsafe { std::slice::from_raw_parts(x_, BIGINT384_NUM_BYTES) };
    num_bigint::BigUint::from_bytes_le(x_)
}

pub fn of_biguint(x: &BigUint) -> BigInteger384 {
    let mut bytes = x.to_bytes_le();
    bytes.resize(BIGINT384_NUM_BYTES, 0);
    let limbs = bytes.as_ptr();
    let limbs = limbs as *const [u64; BIGINT384_NUM_LIMBS as usize];
    let limbs = unsafe { &(*limbs) };
    BigInteger384(*limbs)
}

#[ocaml::func]
pub fn caml_bigint_384_of_numeral(
    s: &[u8],
    _len: ocaml::Int,
    base: ocaml::Int,
) -> Result<BigInteger384, ocaml::Error> {
    match BigUint::parse_bytes(s, base.try_into().unwrap()) {
        Some(data) => Ok(of_biguint(&data)),
        None => Err(ocaml::Error::invalid_argument("caml_bigint_384_of_numeral")
            .err()
            .unwrap()),
    }
}

#[ocaml::func]
pub fn caml_bigint_384_of_decimal_string(s: &[u8]) -> Result<BigInteger384, ocaml::Error> {
    match BigUint::parse_bytes(s, 10) {
        Some(data) => Ok(of_biguint(&data)),
        None => Err(
            ocaml::Error::invalid_argument("caml_bigint_384_of_decimal_string")
                .err()
                .unwrap(),
        ),
    }
}

#[ocaml::func]
pub fn caml_bigint_384_num_limbs() -> ocaml::Int {
    return BIGINT384_NUM_LIMBS.try_into().unwrap();
}

#[ocaml::func]
pub fn caml_bigint_384_bytes_per_limb() -> ocaml::Int {
    return BIGINT384_LIMB_BYTES.try_into().unwrap();
}

#[ocaml::func]
pub fn caml_bigint_384_div(
    x: ocaml::Pointer<BigInteger384>,
    y: ocaml::Pointer<BigInteger384>,
) -> BigInteger384 {
    let res: BigUint = to_biguint(x.as_ref()) / to_biguint(y.as_ref());
    of_biguint(&res)
}

#[ocaml::func]
pub fn caml_bigint_384_compare(
    x: ocaml::Pointer<BigInteger384>,
    y: ocaml::Pointer<BigInteger384>,
) -> ocaml::Int {
    match x.as_ref().cmp(y.as_ref()) {
        Less => -1,
        Equal => 0,
        Greater => 1,
    }
}

#[ocaml::func]
pub fn caml_bigint_384_print(x: ocaml::Pointer<BigInteger384>) {
    println!("{}", to_biguint(x.as_ref()));
}

#[ocaml::func]
pub fn caml_bigint_384_to_string(x: ocaml::Pointer<BigInteger384>) -> String {
    to_biguint(x.as_ref()).to_string()
}

#[ocaml::func]
pub fn caml_bigint_384_test_bit(
    x: ocaml::Pointer<BigInteger384>,
    i: ocaml::Int,
) -> Result<bool, ocaml::Error> {
    match i.try_into() {
        Ok(i) => Ok(x.as_ref().get_bit(i)),
        Err(_) => Err(ocaml::Error::invalid_argument("caml_bigint_384_test_bit")
            .err()
            .unwrap()),
    }
}

#[ocaml::func]
pub fn caml_bigint_384_to_bytes(x: ocaml::Pointer<BigInteger384>) -> ocaml::Value {
    let len = std::mem::size_of::<BigInteger384>();
    let str = unsafe { ocaml::sys::caml_alloc_string(len) };
    let x_ptr: *const BigInteger384 = x.as_ref();
    unsafe {
        core::ptr::copy_nonoverlapping(x_ptr as *const u8, ocaml::sys::string_val(str), len);
        ocaml::Value::new(str)
    }
}

#[ocaml::func]
pub fn caml_bigint_384_of_bytes(x: &[u8]) -> Result<BigInteger384, ocaml::Error> {
    let len = std::mem::size_of::<BigInteger384>();
    if x.len() != len {
        ocaml::Error::failwith("caml_bigint_384_of_bytes")?;
    };
    let x = unsafe { *(x.as_ptr() as *const BigInteger384) };
    Ok(x)
}

#[ocaml::func]
pub fn caml_bigint_384_deep_copy(x: BigInteger384) -> BigInteger384 {
    x
}
