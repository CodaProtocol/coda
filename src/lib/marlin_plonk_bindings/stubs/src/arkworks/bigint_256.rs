use ark_ff::{BigInteger as ark_BigInteger, BigInteger256};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use num_bigint::BigUint;
use std::cmp::Ordering::{Equal, Greater, Less};
use std::convert::{TryFrom, TryInto};
use std::ops::Deref;

//
// Handy constants
//

const BIGINT256_NUM_BITS: i32 = 256;
const BIGINT256_LIMB_BITS: i32 = 64;
const BIGINT256_LIMB_BYTES: i32 = BIGINT256_LIMB_BITS / 8;
const BIGINT256_NUM_LIMBS: i32 =
    (BIGINT256_NUM_BITS + BIGINT256_LIMB_BITS - 1) / BIGINT256_LIMB_BITS;

//
// Wrapper struct to implement OCaml bindings
//

#[derive(Clone, Copy, Debug)]
pub struct CamlBigInteger256(pub BigInteger256);

impl From<BigInteger256> for CamlBigInteger256 {
    fn from(big: BigInteger256) -> Self {
        Self(big)
    }
}

unsafe impl<'a> ocaml::FromValue<'a> for CamlBigInteger256 {
    fn from_value(value: ocaml::Value) -> Self {
        let x: ocaml::Pointer<Self> = ocaml::FromValue::from_value(value);
        x.as_ref().clone()
    }
}

impl CamlBigInteger256 {
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

ocaml::custom!(CamlBigInteger256 {
    finalize: CamlBigInteger256::caml_pointer_finalize,
    compare: CamlBigInteger256::ocaml_compare,
});

impl Deref for CamlBigInteger256 {
    type Target = BigInteger256;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

//
// BigUint handy methods
//

impl Into<BigUint> for CamlBigInteger256 {
    fn into(self) -> BigUint {
        self.0.into()
    }
}

impl Into<BigUint> for &CamlBigInteger256 {
    fn into(self) -> BigUint {
        self.0.clone().into()
    }
}

impl TryFrom<BigUint> for CamlBigInteger256 {
    type Error = String;

    fn try_from(x: BigUint) -> Result<Self, Self::Error> {
        Ok(Self(BigInteger256::try_from(x)?))
    }
}

impl TryFrom<&BigUint> for CamlBigInteger256 {
    type Error = String;

    fn try_from(x: &BigUint) -> Result<Self, Self::Error> {
        Ok(Self(BigInteger256::try_from(x.clone())?))
    }
}

impl ToString for CamlBigInteger256 {
    fn to_string(&self) -> String {
        Into::<BigUint>::into(self).to_string()
    }
}

//
// OCaml stuff
//

#[ocaml::func]
pub fn caml_bigint_256_of_numeral(
    s: &[u8],
    _len: ocaml::Int,
    base: ocaml::Int,
) -> Result<CamlBigInteger256, ocaml::Error> {
    match BigUint::parse_bytes(
        s,
        base.try_into()
            .map_err(|_| ocaml::Error::Message("caml_bigint_256_of_numeral"))?,
    ) {
        Some(data) => CamlBigInteger256::try_from(data)
            .map_err(|_| ocaml::Error::Message("caml_bigint_256_of_numeral")),
        None => Err(ocaml::Error::Message("caml_bigint_256_of_numeral")),
    }
}

#[ocaml::func]
pub fn caml_bigint_256_of_decimal_string(s: &[u8]) -> Result<CamlBigInteger256, ocaml::Error> {
    match BigUint::parse_bytes(s, 10) {
        Some(data) => CamlBigInteger256::try_from(data)
            .map_err(|_| ocaml::Error::Message("caml_bigint_256_of_decimal_string")),
        None => Err(ocaml::Error::Message("caml_bigint_256_of_decimal_string")),
    }
}

#[ocaml::func]
pub fn caml_bigint_256_num_limbs() -> ocaml::Int {
    return BIGINT256_NUM_LIMBS.try_into().unwrap();
}

#[ocaml::func]
pub fn caml_bigint_256_bytes_per_limb() -> ocaml::Int {
    return BIGINT256_LIMB_BYTES.try_into().unwrap();
}

#[ocaml::func]
pub fn caml_bigint_256_div(x: CamlBigInteger256, y: CamlBigInteger256) -> CamlBigInteger256 {
    let x: BigUint = x.into();
    let y: BigUint = y.into();
    let res: BigUint = x / y;
    let inner: BigInteger256 = res.try_into().expect("BigUint division has a bug");
    inner.into()
}

#[ocaml::func]
pub fn caml_bigint_256_compare(
    x: ocaml::Pointer<CamlBigInteger256>,
    y: ocaml::Pointer<CamlBigInteger256>,
) -> ocaml::Int {
    match x.as_ref().cmp(y.as_ref()) {
        Less => -1,
        Equal => 0,
        Greater => 1,
    }
}

#[ocaml::func]
pub fn caml_bigint_256_print(x: CamlBigInteger256) {
    println!("{}", x.to_string());
}

#[ocaml::func]
pub fn caml_bigint_256_to_string(x: CamlBigInteger256) -> String {
    x.to_string()
}

#[ocaml::func]
pub fn caml_bigint_256_test_bit(
    x: ocaml::Pointer<CamlBigInteger256>,
    i: ocaml::Int,
) -> Result<bool, ocaml::Error> {
    match i.try_into() {
        Ok(i) => Ok(x.as_ref().get_bit(i)),
        Err(_) => Err(ocaml::Error::invalid_argument("caml_bigint_256_test_bit")
            .err()
            .unwrap()),
    }
}

#[ocaml::func]
pub fn caml_bigint_256_to_bytes(x: ocaml::Pointer<CamlBigInteger256>) -> ocaml::Value {
    let len = std::mem::size_of::<BigInteger256>();
    let str = unsafe { ocaml::sys::caml_alloc_string(len) };
    let x_ptr: *const CamlBigInteger256 = x.as_ref();
    unsafe {
        let mut input_bytes = vec![];
        (*x_ptr)
            .0
            .serialize(&mut input_bytes)
            .expect("serialize failed");
        core::ptr::copy_nonoverlapping(
            input_bytes.as_ptr(),
            ocaml::sys::string_val(str),
            input_bytes.len(),
        );
        ocaml::Value::new(str)
    }
}

#[ocaml::func]
pub fn caml_bigint_256_of_bytes(x: &[u8]) -> Result<CamlBigInteger256, ocaml::Error> {
    let len = std::mem::size_of::<BigInteger256>();
    if x.len() != len {
        ocaml::Error::failwith("caml_bigint_256_of_bytes")?;
    };
    let result = BigInteger256::deserialize(&mut &x[..])
        .map_err(|_| ocaml::Error::Message("deserialization error"))?;
    Ok(CamlBigInteger256(result))
}

#[ocaml::func]
pub fn caml_bigint_256_deep_copy(x: CamlBigInteger256) -> CamlBigInteger256 {
    x
}

//
// Tests
//

#[cfg(test)]
mod tests {
    use super::*;
    use num_bigint::ToBigUint;

    #[test]
    fn biguint() {
        let x = 10000.to_biguint().unwrap();
        println!("biguint.to_string: {}", x.to_string());
        let y = CamlBigInteger256::try_from(x.clone()).unwrap();
        println!("camlbigint.to_string: {}", y.to_string());
        //assert!(&y.to_string() == "10000");
        let x2: BigUint = y.into();
        assert!(x2 == x);
        println!("biguint.to_string: {}", x2.to_string());
    }
}
