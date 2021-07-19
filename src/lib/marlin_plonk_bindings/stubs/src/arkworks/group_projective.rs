use std::ops::{Add, Deref, Neg, Sub};

use mina_curves::pasta::{
    pallas::Projective as ProjectivePallas, vesta::Projective as ProjectiveVesta,
};

// Pallas

#[derive(Clone, Copy)]
pub struct CamlGroupProjectivePallas(pub ProjectivePallas);

unsafe impl<'a> ocaml::FromValue<'a> for CamlGroupProjectivePallas {
    fn from_value(value: ocaml::Value) -> Self {
        let x: ocaml::Pointer<Self> = ocaml::FromValue::from_value(value);
        x.as_ref().clone()
    }
}

impl CamlGroupProjectivePallas {
    unsafe extern "C" fn caml_pointer_finalize(v: ocaml::Raw) {
        let ptr = v.as_pointer::<Self>();
        ptr.drop_in_place()
    }
}

ocaml::custom!(CamlGroupProjectivePallas {
    finalize: CamlGroupProjectivePallas::caml_pointer_finalize,
});

impl Deref for CamlGroupProjectivePallas {
    type Target = ProjectivePallas;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Handy implementations

impl From<ProjectivePallas> for CamlGroupProjectivePallas {
    fn from(x: ProjectivePallas) -> Self {
        CamlGroupProjectivePallas(x)
    }
}

impl From<&ProjectivePallas> for CamlGroupProjectivePallas {
    fn from(x: &ProjectivePallas) -> Self {
        CamlGroupProjectivePallas(*x)
    }
}

impl Into<ProjectivePallas> for CamlGroupProjectivePallas {
    fn into(self) -> ProjectivePallas {
        self.0
    }
}

impl Into<ProjectivePallas> for &CamlGroupProjectivePallas {
    fn into(self) -> ProjectivePallas {
        self.0
    }
}

impl Add for CamlGroupProjectivePallas {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

impl Add for &CamlGroupProjectivePallas {
    type Output = CamlGroupProjectivePallas;

    fn add(self, other: Self) -> Self::Output {
        CamlGroupProjectivePallas(self.0 + other.0)
    }
}

impl Sub for CamlGroupProjectivePallas {
    type Output = CamlGroupProjectivePallas;

    fn sub(self, other: Self) -> Self::Output {
        CamlGroupProjectivePallas(self.0 - other.0)
    }
}

impl Sub for &CamlGroupProjectivePallas {
    type Output = CamlGroupProjectivePallas;

    fn sub(self, other: Self) -> Self::Output {
        CamlGroupProjectivePallas(self.0 - other.0)
    }
}

impl Neg for CamlGroupProjectivePallas {
    type Output = CamlGroupProjectivePallas;

    fn neg(self) -> Self::Output {
        CamlGroupProjectivePallas(-self.0)
    }
}

impl Neg for &CamlGroupProjectivePallas {
    type Output = CamlGroupProjectivePallas;

    fn neg(self) -> Self::Output {
        CamlGroupProjectivePallas(-self.0)
    }
}

// Vesta

#[derive(Clone, Copy)]
pub struct CamlGroupProjectiveVesta(pub ProjectiveVesta);

unsafe impl<'a> ocaml::FromValue<'a> for CamlGroupProjectiveVesta {
    fn from_value(value: ocaml::Value) -> Self {
        let x: ocaml::Pointer<Self> = ocaml::FromValue::from_value(value);
        x.as_ref().clone()
    }
}

impl CamlGroupProjectiveVesta {
    unsafe extern "C" fn caml_pointer_finalize(v: ocaml::Raw) {
        let ptr = v.as_pointer::<Self>();
        ptr.drop_in_place()
    }
}

ocaml::custom!(CamlGroupProjectiveVesta {
    finalize: CamlGroupProjectiveVesta::caml_pointer_finalize,
});

impl Deref for CamlGroupProjectiveVesta {
    type Target = ProjectiveVesta;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

//
// Handy implementations
//

impl From<ProjectiveVesta> for CamlGroupProjectiveVesta {
    fn from(x: ProjectiveVesta) -> Self {
        CamlGroupProjectiveVesta(x)
    }
}

impl From<&ProjectiveVesta> for CamlGroupProjectiveVesta {
    fn from(x: &ProjectiveVesta) -> Self {
        CamlGroupProjectiveVesta(*x)
    }
}

impl Into<ProjectiveVesta> for CamlGroupProjectiveVesta {
    fn into(self) -> ProjectiveVesta {
        self.0
    }
}

impl Into<ProjectiveVesta> for &CamlGroupProjectiveVesta {
    fn into(self) -> ProjectiveVesta {
        self.0
    }
}

impl Add for CamlGroupProjectiveVesta {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl Add for &CamlGroupProjectiveVesta {
    type Output = CamlGroupProjectiveVesta;

    fn add(self, other: Self) -> Self::Output {
        CamlGroupProjectiveVesta(self.0 + other.0)
    }
}

impl Sub for CamlGroupProjectiveVesta {
    type Output = CamlGroupProjectiveVesta;

    fn sub(self, other: Self) -> Self::Output {
        CamlGroupProjectiveVesta(self.0 - other.0)
    }
}

impl Sub for &CamlGroupProjectiveVesta {
    type Output = CamlGroupProjectiveVesta;

    fn sub(self, other: Self) -> Self::Output {
        CamlGroupProjectiveVesta(self.0 - other.0)
    }
}

impl Neg for CamlGroupProjectiveVesta {
    type Output = CamlGroupProjectiveVesta;

    fn neg(self) -> Self::Output {
        CamlGroupProjectiveVesta(-self.0)
    }
}

impl Neg for &CamlGroupProjectiveVesta {
    type Output = CamlGroupProjectiveVesta;

    fn neg(self) -> Self::Output {
        CamlGroupProjectiveVesta(-self.0)
    }
}
