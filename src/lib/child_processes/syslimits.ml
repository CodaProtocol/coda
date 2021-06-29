(* syslimits.ml -- operating system limits *)

(** maximum length of a file path, as used in the OCaml compiler *)
external path_max : unit -> int = "caml_syslimits_path_max"
