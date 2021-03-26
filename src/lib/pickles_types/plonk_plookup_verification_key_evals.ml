open Core_kernel
module H_list = Snarky_backendless.H_list

[%%versioned
module Stable = struct
  module V1 = struct
    type 'comm t =
          'comm Marlin_plonk_bindings_types.Plonk_plookup_verification_evals.t =
      { sigma_comm_0: 'comm
      ; sigma_comm_1: 'comm
      ; sigma_comm_2: 'comm
      ; sigma_comm_3: 'comm
      ; sigma_comm_4: 'comm
      ; ql_comm: 'comm
      ; qr_comm: 'comm
      ; qo_comm: 'comm
      ; qq_comm: 'comm
      ; qp_comm: 'comm
      ; qm_comm: 'comm
      ; qc_comm: 'comm
      ; rcm_comm_0: 'comm
      ; rcm_comm_1: 'comm
      ; rcm_comm_2: 'comm
      ; rcm_comm_3: 'comm
      ; rcm_comm_4: 'comm
      ; psm_comm: 'comm
      ; add_comm: 'comm
      ; double_comm: 'comm
      ; mul1_comm: 'comm
      ; mul2_comm: 'comm
      ; emul_comm: 'comm
      ; pack_comm: 'comm
      ; lkp_comm: 'comm
      ; table_comm: 'comm
      }
    [@@deriving sexp, eq, compare, hash, yojson, hlist, fields]
  end
end]

let map
    { sigma_comm_0
    ; sigma_comm_1
    ; sigma_comm_2
    ; sigma_comm_3
    ; sigma_comm_4
    ; ql_comm
    ; qr_comm
    ; qo_comm
    ; qq_comm
    ; qp_comm
    ; qm_comm
    ; qc_comm
    ; rcm_comm_0
    ; rcm_comm_1
    ; rcm_comm_2
    ; rcm_comm_3
    ; rcm_comm_4
    ; psm_comm
    ; add_comm
    ; double_comm
    ; mul1_comm
    ; mul2_comm
    ; emul_comm
    ; pack_comm
    ; lkp_comm
    ; table_comm
    } ~f =
  { sigma_comm_0= f sigma_comm_0
  ; sigma_comm_1= f sigma_comm_1
  ; sigma_comm_2= f sigma_comm_2
  ; sigma_comm_3= f sigma_comm_3
  ; sigma_comm_4= f sigma_comm_4
  ; ql_comm= f ql_comm
  ; qr_comm= f qr_comm
  ; qo_comm= f qo_comm
  ; qq_comm= f qq_comm
  ; qp_comm= f qp_comm
  ; qm_comm= f qm_comm
  ; qc_comm= f qc_comm
  ; rcm_comm_0= f rcm_comm_0
  ; rcm_comm_1= f rcm_comm_1
  ; rcm_comm_2= f rcm_comm_2
  ; rcm_comm_3= f rcm_comm_3
  ; rcm_comm_4= f rcm_comm_4
  ; psm_comm= f psm_comm
  ; add_comm= f add_comm
  ; double_comm= f double_comm
  ; mul1_comm= f mul1_comm
  ; mul2_comm= f mul2_comm
  ; emul_comm= f emul_comm
  ; pack_comm= f pack_comm
  ; lkp_comm= f lkp_comm
  ; table_comm= f table_comm
  }

let map2 t1 t2 ~f =
  { sigma_comm_0= f t1.sigma_comm_0 t2.sigma_comm_0
  ; sigma_comm_1= f t1.sigma_comm_1 t2.sigma_comm_1
  ; sigma_comm_2= f t1.sigma_comm_2 t2.sigma_comm_2
  ; sigma_comm_3= f t1.sigma_comm_3 t2.sigma_comm_3
  ; sigma_comm_4= f t1.sigma_comm_4 t2.sigma_comm_4
  ; ql_comm= f t1.ql_comm t2.ql_comm
  ; qr_comm= f t1.qr_comm t2.qr_comm
  ; qo_comm= f t1.qo_comm t2.qo_comm
  ; qq_comm= f t1.qq_comm t2.qq_comm
  ; qp_comm= f t1.qp_comm t2.qp_comm
  ; qm_comm= f t1.qm_comm t2.qm_comm
  ; qc_comm= f t1.qc_comm t2.qc_comm
  ; rcm_comm_0= f t1.rcm_comm_0 t2.rcm_comm_0
  ; rcm_comm_1= f t1.rcm_comm_1 t2.rcm_comm_1
  ; rcm_comm_2= f t1.rcm_comm_2 t2.rcm_comm_2
  ; rcm_comm_3= f t1.rcm_comm_3 t2.rcm_comm_3
  ; rcm_comm_4= f t1.rcm_comm_4 t2.rcm_comm_4
  ; psm_comm= f t1.psm_comm t2.psm_comm
  ; add_comm= f t1.add_comm t2.add_comm
  ; double_comm= f t1.double_comm t2.double_comm
  ; mul1_comm= f t1.mul1_comm t2.mul1_comm
  ; mul2_comm= f t1.mul2_comm t2.mul2_comm
  ; emul_comm= f t1.emul_comm t2.emul_comm
  ; pack_comm= f t1.pack_comm t2.pack_comm
  ; lkp_comm= f t1.lkp_comm t2.lkp_comm
  ; table_comm= f t1.table_comm t2.table_comm
  }

let typ g =
  Snarky_backendless.Typ.of_hlistable
    [g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g; g]
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
    ~value_of_hlist:of_hlist
