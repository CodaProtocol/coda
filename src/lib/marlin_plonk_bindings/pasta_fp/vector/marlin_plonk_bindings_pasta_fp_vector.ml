type t

type elt = Marlin_plonk_bindings_pasta_fp.t

external create : unit -> t = "caml_pasta_fp_vector_create"

external length : t -> int = "caml_pasta_fp_vector_length"

external emplace_back : t -> elt -> unit = "caml_pasta_fp_vector_emplace_back"

external get : t -> int -> elt = "caml_pasta_fp_vector_get"

let%test "vectors" =
  let vec = create () in
  let y = Marlin_plonk_bindings_pasta_fp.of_int 3 in
  emplace_back vec y ;
  emplace_back vec y ;
  get vec 0 = y && get vec 1 = y && length vec = 2
