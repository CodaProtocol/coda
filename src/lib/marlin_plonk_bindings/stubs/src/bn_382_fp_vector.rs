use crate::bn_382_fp::{CamlBn382Fp, CamlBn382FpPtr};
use algebra::bn_382::fp::Fp;
use std::convert::TryInto;

pub struct CamlBn382FpVector(pub Vec<Fp>);
pub type CamlBn382FpVectorPtr<'a> = ocaml::Pointer<'a, CamlBn382FpVector>;

/* Note: The vector header is allocated in the OCaml heap, but the data held in
   the vector elements themselves are stored in the rust heap.
*/

extern "C" fn caml_bn_382_fp_vector_finalize(v: ocaml::Raw) {
    unsafe {
        let mut v: CamlBn382FpVectorPtr = v.as_pointer();
        v.as_mut_ptr().drop_in_place();
    }
}

ocaml::custom!(CamlBn382FpVector {
    finalize: caml_bn_382_fp_vector_finalize,
});

#[ocaml::func]
pub fn caml_bn_382_fp_vector_create() -> CamlBn382FpVector {
    CamlBn382FpVector(Vec::new())
}

#[ocaml::func]
pub fn caml_bn_382_fp_vector_length(v: CamlBn382FpVectorPtr) -> ocaml::Int {
    v.as_ref().0.len() as isize
}

#[ocaml::func]
pub fn caml_bn_382_fp_vector_emplace_back(mut v: CamlBn382FpVectorPtr, x: CamlBn382FpPtr) {
    v.as_mut().0.push(x.as_ref().0);
}

#[ocaml::func]
pub fn caml_bn_382_fp_vector_get(
    v: CamlBn382FpVectorPtr,
    i: ocaml::Int,
) -> Result<Option<CamlBn382Fp>, ocaml::Error> {
    match TryInto::<usize>::try_into(i) {
        Ok(i) => match v.as_ref().0.get(i) {
            Some(x) => Ok(Some(CamlBn382Fp(*x))),
            None => Ok(None),
        },
        Err(_) => Err(ocaml::Error::invalid_argument("caml_bn_382_fp_vector_get")
            .err()
            .unwrap()),
    }
}
