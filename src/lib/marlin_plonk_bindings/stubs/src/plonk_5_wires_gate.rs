use plonk_5_wires_circuits::gate::{GateType, GateType::*};
use plonk_5_wires_circuits::wires::{GateWires, Wire};

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub enum CamlPlonkGateType {
    Zero,     // zero gate
    Generic,  // generic arithmetic gate
    Poseidon, // Poseidon permutation gate
    Add,      // Gate constraining EC addition in Affine form
    Double,   // Gate constraining EC point doubling in Affine form
    Vbmul1,   // Gate constraining EC variable base scalar multiplication
    Vbmul2,   // Gate constraining unpacking EC variable base scalar multiplication
    Endomul,  // Gate constraining EC variable base scalar multiplication with group endomorphim optimization
    Pack,     // Gate constraining packing
}

impl From<&GateType> for CamlPlonkGateType {
    fn from(gate_type: &GateType) -> Self {
        match gate_type {
            Zero => CamlPlonkGateType::Zero,
            Generic => CamlPlonkGateType::Generic,
            Poseidon => CamlPlonkGateType::Poseidon,
            Add => CamlPlonkGateType::Add,
            Double => CamlPlonkGateType::Double,
            Vbmul1 => CamlPlonkGateType::Vbmul1,
            Vbmul2 => CamlPlonkGateType::Vbmul2,
            Endomul => CamlPlonkGateType::Endomul,
            Pack => CamlPlonkGateType::Pack,
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
            CamlPlonkGateType::Add => Add,
            CamlPlonkGateType::Double => Double,
            CamlPlonkGateType::Vbmul1 => Vbmul1,
            CamlPlonkGateType::Vbmul2 => Vbmul2,
            CamlPlonkGateType::Endomul => Endomul,
            CamlPlonkGateType::Pack => Pack,
        }
    }
}
impl From<CamlPlonkGateType> for GateType {
    fn from(gate_type: CamlPlonkGateType) -> Self {
        Self::from(&gate_type)
    }
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlPlonkWire {
    pub row: ocaml::Int, // wire row
    pub col: ocaml::Int, // wire column
}

impl From<&Wire> for CamlPlonkWire {
    fn from(wire: &Wire) -> Self {
        CamlPlonkWire {
            row: wire.row as isize,
            col: wire.col as isize,
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
            col: wire.col as usize,
        }
    }
}
impl From<CamlPlonkWire> for Wire {
    fn from(wire: CamlPlonkWire) -> Self {
        Self::from(&wire)
    }
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlPlonkWires {
    pub l: CamlPlonkWire,
    pub r: CamlPlonkWire,
    pub o: CamlPlonkWire,
    pub q: CamlPlonkWire,
    pub p: CamlPlonkWire,
}

impl From<&GateWires> for CamlPlonkWires {
    fn from(wires: &GateWires) -> Self {
        CamlPlonkWires {
            l: wires[0].into(),
            r: wires[1].into(),
            o: wires[2].into(),
            q: wires[3].into(),
            p: wires[4].into(),
        }
    }
}
impl From<GateWires> for CamlPlonkWires {
    fn from(wires: GateWires) -> Self {
        Self::from(&wires)
    }
}

impl From<&CamlPlonkWires> for GateWires {
    fn from(wires: &CamlPlonkWires) -> Self {
        [
            (&wires.l).into(),
            (&wires.r).into(),
            (&wires.o).into(),
            (&wires.q).into(),
            (&wires.p).into(),
        ]
    }
}

impl From<CamlPlonkWires> for GateWires {
    fn from(wires: CamlPlonkWires) -> Self {
        Self::from(&wires)
    }
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlPlonkGate<T> {
    pub typ: CamlPlonkGateType, // type of the gate
    pub row: isize,
    pub wires: CamlPlonkWires,
    pub c: T, // constraints vector
}
