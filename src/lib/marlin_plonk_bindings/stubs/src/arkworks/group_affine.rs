use crate::arkworks::{CamlFp, CamlFq};
use ark_ff::Zero;
use mina_curves::pasta::{pallas::Affine as AffinePallas, vesta::Affine as AffineVesta};

//
// handy types
//

pub type CamlGVesta = CamlGroupAffine<CamlFq>;
pub type CamlGPallas = CamlGroupAffine<CamlFp>;

//
// GroupAffine<G> <-> CamlGroupAffine<F>
//

#[derive(Clone, Copy, ocaml::IntoValue, ocaml::FromValue)]
pub enum CamlGroupAffine<F> {
    Infinity,
    Finite((F, F)),
}

// Conversions from/to AffineVesta

impl From<AffineVesta> for CamlGVesta {
    fn from(point: AffineVesta) -> Self {
        if point.infinity {
            Self::Infinity
        } else {
            Self::Finite((point.x.into(), point.y.into()))
        }
    }
}

impl From<&AffineVesta> for CamlGVesta {
    fn from(point: &AffineVesta) -> Self {
        if point.infinity {
            Self::Infinity
        } else {
            Self::Finite((point.x.into(), point.y.into()))
        }
    }
}

impl Into<AffineVesta> for CamlGVesta {
    fn into(self) -> AffineVesta {
        match self {
            Self::Infinity => AffineVesta::zero(),
            Self::Finite((x, y)) => AffineVesta::new(x.into(), y.into(), false),
        }
    }
}

impl Into<AffineVesta> for &CamlGVesta {
    fn into(self) -> AffineVesta {
        match self {
            CamlGroupAffine::Infinity => AffineVesta::zero(),
            CamlGroupAffine::Finite((x, y)) => AffineVesta::new(x.into(), y.into(), false),
        }
    }
}

// Conversion from/to AffinePallas

impl From<AffinePallas> for CamlGPallas {
    fn from(point: AffinePallas) -> Self {
        if point.infinity {
            Self::Infinity
        } else {
            Self::Finite((point.x.into(), point.y.into()))
        }
    }
}

impl From<&AffinePallas> for CamlGPallas {
    fn from(point: &AffinePallas) -> Self {
        if point.infinity {
            Self::Infinity
        } else {
            Self::Finite((point.x.into(), point.y.into()))
        }
    }
}

impl Into<AffinePallas> for CamlGPallas {
    fn into(self) -> AffinePallas {
        match self {
            Self::Infinity => AffinePallas::zero(),
            Self::Finite((x, y)) => AffinePallas::new(x.into(), y.into(), false),
        }
    }
}

impl Into<AffinePallas> for &CamlGPallas {
    fn into(self) -> AffinePallas {
        match self {
            CamlGroupAffine::Infinity => AffinePallas::zero(),
            CamlGroupAffine::Finite((x, y)) => AffinePallas::new(x.into(), y.into(), false),
        }
    }
}
