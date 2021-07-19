type t

module Affine = struct
  type t =
    (Marlin_plonk_bindings_pasta_fp.t * Marlin_plonk_bindings_pasta_fp.t)
    Marlin_plonk_bindings_types.Or_infinity.t
end

external one : unit -> t = "caml_pasta_pallas_one"

external add : t -> t -> t = "caml_pasta_pallas_add"

external sub : t -> t -> t = "caml_pasta_pallas_sub"

external negate : t -> t = "caml_pasta_pallas_negate"

external double : t -> t = "caml_pasta_pallas_double"

external scale : t -> Marlin_plonk_bindings_pasta_fq.t -> t
  = "caml_pasta_pallas_scale"

external random : unit -> t = "caml_pasta_pallas_random"

external rng : int -> t = "caml_pasta_pallas_rng"

external to_affine : t -> Affine.t = "caml_pasta_pallas_to_affine"

external of_affine : Affine.t -> t = "caml_pasta_pallas_of_affine"

external of_affine_coordinates :
  Marlin_plonk_bindings_pasta_fp.t -> Marlin_plonk_bindings_pasta_fp.t -> t
  = "caml_pasta_pallas_of_affine_coordinates"

external endo_base : unit -> Marlin_plonk_bindings_pasta_fp.t
  = "caml_pasta_pallas_endo_base"

external endo_scalar : unit -> Marlin_plonk_bindings_pasta_fq.t
  = "caml_pasta_pallas_endo_scalar"

external affine_deep_copy : Affine.t -> Affine.t
  = "caml_pasta_pallas_affine_deep_copy"

(* tests *)

let%test_module _ =
  ( module struct
    let%test "affine dump_copy" =
      let x = random () |> to_affine in
      affine_deep_copy x = x

    let is_same_point point (x_string, y_string) =
      let open Marlin_plonk_bindings_types.Or_infinity in
      match to_affine point with
      | Infinity ->
          Format.printf "infinity point\n%!" ;
          false
      | Finite (x, y) ->
          let same_x = x_string = Marlin_plonk_bindings_pasta_fp.to_string x in
          let same_y = y_string = Marlin_plonk_bindings_pasta_fp.to_string y in
          same_x && same_y

    let%test "one" =
      let point = one () in
      is_same_point point
        ( "1"
        , "12418654782883325593414442427049395787963493412651469444558597405572177144507"
        )

    let%test "scale" =
      let x = one () in
      let k = Marlin_plonk_bindings_pasta_fq.of_int 5 in
      let point = scale x k in
      is_same_point point
        ( "2043704922874314040385013091576698457103021424623870194379792173147242419946"
        , "5363959817269906935062331974892998553523781697031104933152007068461890921147"
        )
  end )
