open Marlin_plonk_bindings.Types
open Intf
open Core_kernel

module type Input_intf = sig
  module BaseField : sig
    type t
  end

  module ScalarField : sig
    type t
  end

  module Affine : sig
    type t = (BaseField.t * BaseField.t) Or_infinity.t
  end

  type t

  val to_affine : t -> Affine.t

  val of_affine_coordinates : BaseField.t -> BaseField.t -> t

  val add : t -> t -> t

  val double : t -> t

  val scale : t -> ScalarField.t -> t

  val sub : t -> t -> t

  val negate : t -> t

  val random : unit -> t

  val one : unit -> t
end

module type Field_intf = sig
  module Stable : sig
    module Latest : sig
      type t [@@deriving bin_io, equal, sexp, compare, yojson, hash]
    end
  end

  type t = Stable.Latest.t

  val ( + ) : t -> t -> t

  val ( * ) : t -> t -> t

  val one : t

  val square : t -> t

  val is_square : t -> bool

  val sqrt : t -> t

  val random : unit -> t
end

module Make
    (BaseField : Field_intf) (ScalarField : sig
        type t
    end) (Params : sig
      val a : BaseField.t

      val b : BaseField.t
    end)
    (C : Input_intf
         with module BaseField := BaseField
          and module ScalarField := ScalarField) =
struct
  include (
    C : module type of C with type t = C.t with module Affine := C.Affine )

  let one = one ()

  let zero = sub one one

  let y_squared x =
    let open BaseField in
    Params.b + (x * (Params.a + square x))

  module Affine = struct
    module Backend = struct
      include C.Affine

      let zero () = Or_infinity.Infinity

      let create x y = Or_infinity.Finite (x, y)
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type t = BaseField.Stable.Latest.t * BaseField.Stable.Latest.t
          [@@deriving
            version {asserted}, equal, bin_io, sexp, compare, yojson, hash]
        end

        include T

        exception Invalid_curve_point of t

        include Binable.Of_binable
                  (T)
                  (struct
                    let on_curve (x, y) =
                      BaseField.Stable.Latest.equal (y_squared x)
                        (BaseField.square y)

                    type t = T.t

                    let to_binable = Fn.id

                    let of_binable t =
                      if not (on_curve t) then raise (Invalid_curve_point t) ;
                      t
                  end)
      end

      module Latest = V1
    end

    let%test "cannot deserialize invalid points" =
      (* y^2 = x^3 + a x + b

        pick c at random
        let (x, y) = (c^2, c^3)

        Then the above equation becomes
        c^6 = c^6 + (a c^2 + b)

        a c^3 + b is almost certainly nonzero (and for our curves, with a = 0, it always is)
        so this point is almost certainly (and for our curves, always) invalid
    *)
      let invalid =
        let open BaseField in
        let c = random () in
        let c2 = square c in
        (c2, c2 * c)
      in
      match
        Binable.to_string (module Stable.Latest) invalid
        |> Binable.of_string (module Stable.Latest)
      with
      | exception Stable.V1.Invalid_curve_point _ ->
          true
      | _ ->
          false

    include Stable.Latest

    let to_backend = Fn.id

    let of_backend = Fn.id
  end

  let to_affine_or_infinity = C.to_affine

  let to_affine_exn t =
    match C.to_affine t with
    | Infinity ->
        failwith "to_affine_exn: Got identity"
    | Finite c ->
        c

  let of_affine (x, y) = C.of_affine_coordinates x y

  include Binable.Of_binable
            (Affine)
            (struct
              type nonrec t = t

              let to_binable = to_affine_exn

              let of_binable = of_affine
            end)

  let ( + ) = add

  let ( * ) s t = scale t s

  let find_y x =
    let open BaseField in
    let y2 = y_squared x in
    if is_square y2 then Some (sqrt y2) else None

  let point_near_x (x : BaseField.t) =
    let rec go x = function
      | Some y ->
          of_affine (x, y)
      | None ->
          let x' = BaseField.(one + x) in
          go x' (find_y x')
    in
    go x (find_y x)
end
