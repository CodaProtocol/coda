open Marlin_plonk_bindings_types

module Gate_vector = struct
  type t

  external create :
    unit -> t
    = "caml_pasta_fq_plonk_5_wires_gate_vector_create"

  external add :
    t -> Marlin_plonk_bindings_pasta_fq.t Plonk_5_wires_gate.t -> unit
    = "caml_pasta_fq_plonk_5_wires_gate_vector_add"

  external get :
    t -> int -> Marlin_plonk_bindings_pasta_fq.t Plonk_5_wires_gate.t
    = "caml_pasta_fq_plonk_5_wires_gate_vector_get"

  external wrap :
    t -> Plonk_5_wires_gate.Wire.t -> Plonk_5_wires_gate.Wire.t -> unit
    = "caml_pasta_fq_plonk_5_wires_gate_vector_wrap"
end

type t

external create :
  Gate_vector.t -> int -> Marlin_plonk_bindings_pasta_fq_urs.t -> t
  = "caml_pasta_fq_plonk_5_wires_index_create"

external max_degree : t -> int = "caml_pasta_fq_plonk_5_wires_index_max_degree"

external public_inputs :
  t -> int
  = "caml_pasta_fq_plonk_5_wires_index_public_inputs"

external domain_d1_size :
  t -> int
  = "caml_pasta_fq_plonk_5_wires_index_domain_d1_size"

external domain_d4_size :
  t -> int
  = "caml_pasta_fq_plonk_5_wires_index_domain_d4_size"

external domain_d8_size :
  t -> int
  = "caml_pasta_fq_plonk_5_wires_index_domain_d8_size"

external read :
  ?offset:int -> Marlin_plonk_bindings_pasta_fq_urs.t -> string -> t
  = "caml_pasta_fq_plonk_5_wires_index_read"

external write :
  ?append:bool -> t -> string -> unit
  = "caml_pasta_fq_plonk_5_wires_index_write"
