#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub struct CamlPlonkDomain<Fr> {
    pub log_size_of_group: ocaml::Int,
    pub group_gen: Fr,
}

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub struct CamlPlonkVerificationEvals<PolyComm> {
    pub sigma_comm0: PolyComm,
    pub sigma_comm1: PolyComm,
    pub sigma_comm2: PolyComm,
    pub ql_comm: PolyComm,
    pub qr_comm: PolyComm,
    pub qo_comm: PolyComm,
    pub qm_comm: PolyComm,
    pub qc_comm: PolyComm,
    pub rcm_comm0: PolyComm,
    pub rcm_comm1: PolyComm,
    pub rcm_comm2: PolyComm,
    pub psm_comm: PolyComm,
    pub add_comm: PolyComm,
    pub mul1_comm: PolyComm,
    pub mul2_comm: PolyComm,
    pub emul1_comm: PolyComm,
    pub emul2_comm: PolyComm,
    pub emul3_comm: PolyComm,
}

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub struct CamlPlonkVerificationShifts<Fr> {
    pub r: Fr,
    pub o: Fr,
}

#[derive(ocaml::IntoValue, ocaml::FromValue)]
pub struct CamlPlonkVerifierIndex<Fr, URS, PolyComm> {
    pub domain: CamlPlonkDomain<Fr>,
    pub max_poly_size: ocaml::Int,
    pub max_quot_size: ocaml::Int,
    pub urs: URS,
    pub evals: CamlPlonkVerificationEvals<PolyComm>,
    pub shifts: CamlPlonkVerificationShifts<Fr>,
}
