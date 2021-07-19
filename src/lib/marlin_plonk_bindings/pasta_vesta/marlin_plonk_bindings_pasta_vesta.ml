type t

module Affine = struct
  type t =
    (Marlin_plonk_bindings_pasta_fq.t * Marlin_plonk_bindings_pasta_fq.t)
    Marlin_plonk_bindings_types.Or_infinity.t
end

external one : unit -> t = "caml_pasta_vesta_one"

external add : t -> t -> t = "caml_pasta_vesta_add"

external sub : t -> t -> t = "caml_pasta_vesta_sub"

external negate : t -> t = "caml_pasta_vesta_negate"

external double : t -> t = "caml_pasta_vesta_double"

external scale : t -> Marlin_plonk_bindings_pasta_fp.t -> t
  = "caml_pasta_vesta_scale"

external random : unit -> t = "caml_pasta_vesta_random"

external rng : int -> t = "caml_pasta_vesta_rng"

external to_affine : t -> Affine.t = "caml_pasta_vesta_to_affine"

external of_affine : Affine.t -> t = "caml_pasta_vesta_of_affine"

external of_affine_coordinates :
  Marlin_plonk_bindings_pasta_fq.t -> Marlin_plonk_bindings_pasta_fq.t -> t
  = "caml_pasta_vesta_of_affine_coordinates"

external endo_base : unit -> Marlin_plonk_bindings_pasta_fq.t
  = "caml_pasta_vesta_endo_base"

external endo_scalar : unit -> Marlin_plonk_bindings_pasta_fp.t
  = "caml_pasta_vesta_endo_scalar"

external affine_deep_copy : Affine.t -> Affine.t
  = "caml_pasta_vesta_affine_deep_copy"

(* tests*)

let%test_module _ =
  ( module struct
    let%test "affine deep_copy" =
      let x = random () |> to_affine in
      affine_deep_copy x = x

    let is_same_point point (x_string, y_string) =
      let open Marlin_plonk_bindings_types.Or_infinity in
      match to_affine point with
      | Infinity ->
          Format.printf "infinity point\n%!" ;
          false
      | Finite (x, y) ->
          let same_x = x_string = Marlin_plonk_bindings_pasta_fq.to_string x in
          let same_y = y_string = Marlin_plonk_bindings_pasta_fq.to_string y in
          same_x && same_y

    let%test "one" =
      let point = one () in
      is_same_point point
        ( "1"
        , "11426906929455361843568202299992114520848200991084027513389447476559454104162"
        )

    let%test "scale" =
      let x = one () in
      let k = Marlin_plonk_bindings_pasta_fp.of_int 5 in
      let point = scale x k in
      is_same_point point
        ( "13486096822601787212198473175598632436276692382164101871628408648731182981277"
        , "605204994206592296804149234467312053390246487302138089171444931383680327812"
        )
  end )
