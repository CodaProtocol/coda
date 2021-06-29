use crate::bn_382_fq::{CamlBn382Fq, CamlBn382FqPtr};
use algebra::bn_382::fq::Fq;
use std::convert::TryInto;

pub struct CamlBn382FqVector(pub Vec<Fq>);
pub type CamlBn382FqVectorPtr<'a> = ocaml::Pointer<'a, CamlBn382FqVector>;

/* Note: The vector header is allocated in the OCaml heap, but the data held in
   the vector elements themselves are stored in the rust heap.
*/

extern "C" fn caml_bn_382_fq_vector_finalize(v: ocaml::Raw) {
    unsafe {
        let mut v: CamlBn382FqVectorPtr = v.as_pointer();
        v.as_mut_ptr().drop_in_place();
    }
}

ocaml::custom!(CamlBn382FqVector {
    finalize: caml_bn_382_fq_vector_finalize,
});

#[ocaml::func]
pub fn caml_bn_382_fq_vector_create() -> CamlBn382FqVector {
    CamlBn382FqVector(Vec::new())
}

#[ocaml::func]
pub fn caml_bn_382_fq_vector_length(v: CamlBn382FqVectorPtr) -> ocaml::Int {
    v.as_ref().0.len() as isize
}

#[ocaml::func]
pub fn caml_bn_382_fq_vector_emplace_back(mut v: CamlBn382FqVectorPtr, x: CamlBn382FqPtr) {
    v.as_mut().0.push(x.as_ref().0);
}

#[ocaml::func]
pub fn caml_bn_382_fq_vector_get(
    v: CamlBn382FqVectorPtr,
    i: ocaml::Int,
) -> Result<Option<CamlBn382Fq>, ocaml::Error> {
    match TryInto::<usize>::try_into(i) {
        Ok(i) => match v.as_ref().0.get(i) {
            Some(x) => Ok(Some(CamlBn382Fq(*x))),
            None => Ok(None),
        },
        Err(_) => Err(ocaml::Error::invalid_argument("caml_bn_382_fq_vector_get")
            .err()
            .unwrap()),
    }
}
