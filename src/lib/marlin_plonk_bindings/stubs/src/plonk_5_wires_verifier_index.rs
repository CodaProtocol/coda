use crate::plonk_verifier_index::{CamlPlonkDomain};

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlPlonkVerificationEvals<PolyComm> {
    pub sigma_comm0: PolyComm,
    pub sigma_comm1: PolyComm,
    pub sigma_comm2: PolyComm,
    pub sigma_comm3: PolyComm,
    pub sigma_comm4: PolyComm,
    pub ql_comm: PolyComm,
    pub qr_comm: PolyComm,
    pub qo_comm: PolyComm,
    pub qq_comm: PolyComm,
    pub qp_comm: PolyComm,
    pub qm_comm: PolyComm,
    pub qc_comm: PolyComm,
    pub rcm_comm0: PolyComm,
    pub rcm_comm1: PolyComm,
    pub rcm_comm2: PolyComm,
    pub rcm_comm3: PolyComm,
    pub rcm_comm4: PolyComm,
    pub psm_comm: PolyComm,
    pub add_comm: PolyComm,
    pub double_comm: PolyComm,
    pub mul1_comm: PolyComm,
    pub mul2_comm: PolyComm,
    pub emul_comm: PolyComm,
    pub pack_comm: PolyComm,
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlPlonkVerificationShifts<Fr> {
    pub s0: Fr,
    pub s1: Fr,
    pub s2: Fr,
    pub s3: Fr,
    pub s4: Fr,
}

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlPlonkVerifierIndex<Fr, URS, PolyComm> {
    pub domain: CamlPlonkDomain<Fr>,
    pub max_poly_size: ocaml::Int,
    pub max_quot_size: ocaml::Int,
    pub urs: URS,
    pub evals: CamlPlonkVerificationEvals<PolyComm>,
    pub shifts: CamlPlonkVerificationShifts<Fr>,
}
