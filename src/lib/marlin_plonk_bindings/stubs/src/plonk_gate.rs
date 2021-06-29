use plonk_circuits::gate::{GateType, GateType::*};
use plonk_circuits::wires::{Col, Col::*, Wire, Wires};

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub enum CamlPlonkGateType {
    Zero,    // zero gate
    Generic, // generic arithmetic gate

    Poseidon, // Poseidon permutation gate

    Add1, // Gate constraining EC addition in Affine form
    Add2, // Gate constraining EC point abscissa distinctness

    Vbmul1, // Gate constraining EC variable base scalar multiplication
    Vbmul2, // Gate constraining EC variable base scalar multiplication
    Vbmul3, // Gate constraining EC variable base scalar multiplication

    Endomul1, // Gate constraining EC variable base scalar multiplication with group endomorphim optimization
    Endomul2, // Gate constraining EC variable base scalar multiplication with group endomorphim optimization
    Endomul3, // Gate constraining EC variable base scalar multiplication with group endomorphim optimization
    Endomul4, // Gate constraining EC variable base scalar multiplication with group endomorphim optimization
}

impl From<&GateType> for CamlPlonkGateType {
    fn from(gate_type: &GateType) -> Self {
        match gate_type {
            Zero => CamlPlonkGateType::Zero,
            Generic => CamlPlonkGateType::Generic,
            Poseidon => CamlPlonkGateType::Poseidon,
            Add1 => CamlPlonkGateType::Add1,
            Add2 => CamlPlonkGateType::Add2,
            Vbmul1 => CamlPlonkGateType::Vbmul1,
            Vbmul2 => CamlPlonkGateType::Vbmul2,
            Vbmul3 => CamlPlonkGateType::Vbmul3,
            Endomul1 => CamlPlonkGateType::Endomul1,
            Endomul2 => CamlPlonkGateType::Endomul2,
            Endomul3 => CamlPlonkGateType::Endomul3,
            Endomul4 => CamlPlonkGateType::Endomul4,
        }
    }
}
impl From<GateType> for CamlPlonkGateType {
    fn from(gate_type: GateType) -> Self {
        Self::from(&gate_type)
    }
}

impl From<&CamlPlonkGateType> for GateType {
    fn from(gate_type: &CamlPlonkGateType) -> Self {
        match gate_type {
            CamlPlonkGateType::Zero => Zero,
            CamlPlonkGateType::Generic => Generic,
            CamlPlonkGateType::Poseidon => Poseidon,
            CamlPlonkGateType::Add1 => Add1,
            CamlPlonkGateType::Add2 => Add2,
            CamlPlonkGateType::Vbmul1 => Vbmul1,
            CamlPlonkGateType::Vbmul2 => Vbmul2,
            CamlPlonkGateType::Vbmul3 => Vbmul3,
            CamlPlonkGateType::Endomul1 => Endomul1,
            CamlPlonkGateType::Endomul2 => Endomul2,
            CamlPlonkGateType::Endomul3 => Endomul3,
            CamlPlonkGateType::Endomul4 => Endomul4,
        }
    }
}
impl From<CamlPlonkGateType> for GateType {
    fn from(gate_type: CamlPlonkGateType) -> Self {
        Self::from(&gate_type)
    }
}

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub enum CamlPlonkCol {
    L,
    R,
    O,
}

impl From<&Col> for CamlPlonkCol {
    fn from(col: &Col) -> Self {
        match col {
            L => CamlPlonkCol::L,
            R => CamlPlonkCol::R,
            O => CamlPlonkCol::O,
        }
    }
}

impl From<Col> for CamlPlonkCol {
    fn from(col: Col) -> Self {
        Self::from(&col)
    }
}

impl From<&CamlPlonkCol> for Col {
    fn from(col: &CamlPlonkCol) -> Self {
        match col {
            CamlPlonkCol::L => L,
            CamlPlonkCol::R => R,
            CamlPlonkCol::O => O,
        }
    }
}

impl From<CamlPlonkCol> for Col {
    fn from(col: CamlPlonkCol) -> Self {
        Self::from(&col)
    }
}

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub struct CamlPlonkWire {
    pub row: ocaml::Int,   // wire row
    pub col: CamlPlonkCol, // wire column
}

impl From<&Wire> for CamlPlonkWire {
    fn from(wire: &Wire) -> Self {
        CamlPlonkWire {
            row: wire.row as isize,
            col: (&wire.col).into(),
        }
    }
}
impl From<Wire> for CamlPlonkWire {
    fn from(wire: Wire) -> Self {
        Self::from(&wire)
    }
}

impl From<&CamlPlonkWire> for Wire {
    fn from(wire: &CamlPlonkWire) -> Self {
        Wire {
            row: wire.row as usize,
            col: (&wire.col).into(),
        }
    }
}
impl From<CamlPlonkWire> for Wire {
    fn from(wire: CamlPlonkWire) -> Self {
        Self::from(&wire)
    }
}

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub struct CamlPlonkWires {
    pub row: ocaml::Int,  // gate wire row
    pub l: CamlPlonkWire, // left input wire permutation
    pub r: CamlPlonkWire, // right input wire permutation
    pub o: CamlPlonkWire, // output input wire permutation
}

impl From<&Wires> for CamlPlonkWires {
    fn from(wires: &Wires) -> Self {
        CamlPlonkWires {
            row: wires.row as isize,
            l: (&wires.l).into(),
            r: (&wires.r).into(),
            o: (&wires.o).into(),
        }
    }
}
impl From<Wires> for CamlPlonkWires {
    fn from(wires: Wires) -> Self {
        Self::from(&wires)
    }
}

impl From<&CamlPlonkWires> for Wires {
    fn from(wires: &CamlPlonkWires) -> Self {
        Wires {
            row: wires.row as usize,
            l: (&wires.l).into(),
            r: (&wires.r).into(),
            o: (&wires.o).into(),
        }
    }
}
impl From<CamlPlonkWires> for Wires {
    fn from(wires: CamlPlonkWires) -> Self {
        Self::from(&wires)
    }
}

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub struct CamlPlonkGate<T> {
    pub typ: CamlPlonkGateType, // type of the gate
    pub wires: CamlPlonkWires,  // gate wires
    pub c: T,                   // constraints vector
}
