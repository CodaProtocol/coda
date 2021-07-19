open Marlin_plonk_bindings_types

module Gate_vector = struct
  type t

  external create : unit -> t = "caml_pasta_fp_plonk_gate_vector_create"

  external add : t -> Marlin_plonk_bindings_pasta_fp.t Plonk_gate.t -> unit
    = "caml_pasta_fp_plonk_gate_vector_add"

  external get : t -> int -> Marlin_plonk_bindings_pasta_fp.t Plonk_gate.t
    = "caml_pasta_fp_plonk_gate_vector_get"

  external wrap : t -> Plonk_gate.Wire.t -> Plonk_gate.Wire.t -> unit
    = "caml_pasta_fp_plonk_gate_vector_wrap"

  let%test "gate vector" =
    let vec = create () in
    let l : Plonk_gate.Wire.t = { row = 1; col = L } in
    let r : Plonk_gate.Wire.t = { row = 1; col = R } in
    let o : Plonk_gate.Wire.t = { row = 1; col = O } in
    let wires : Plonk_gate.Wires.t = { row = 1; l; r; o } in
    let el : _ Plonk_gate.t =
      { kind = Zero; wires; c = [| Marlin_plonk_bindings_pasta_fp.random () |] }
    in
    add vec el ;
    let t : Plonk_gate.Wire.t = { row = 0; col = L } in
    let h : Plonk_gate.Wire.t = { row = 8; col = O } in
    wrap vec t h ;
    let z = get vec 0 in
    z.wires.l.row = 8 && z.wires.l.col = O
end

type t

external create :
  Gate_vector.t -> int -> Marlin_plonk_bindings_pasta_fp_urs.t -> t
  = "caml_pasta_fp_plonk_index_create"

external max_degree : t -> int = "caml_pasta_fp_plonk_index_max_degree"

external public_inputs : t -> int = "caml_pasta_fp_plonk_index_public_inputs"

external domain_d1_size : t -> int = "caml_pasta_fp_plonk_index_domain_d1_size"

external domain_d4_size : t -> int = "caml_pasta_fp_plonk_index_domain_d4_size"

external domain_d8_size : t -> int = "caml_pasta_fp_plonk_index_domain_d8_size"

external read :
  ?offset:int -> Marlin_plonk_bindings_pasta_fp_urs.t -> string -> t
  = "caml_pasta_fp_plonk_index_read"

external write : ?append:bool -> t -> string -> unit
  = "caml_pasta_fp_plonk_index_write"
