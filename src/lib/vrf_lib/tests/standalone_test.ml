open Core_kernel
open Vrf_lib.Standalone

let%test_module "vrf-test" =
  ( module struct
    (* Nothing in here is secure, it's just for the test *)
    module Impl = Snark_params.Tick
    module Other_impl = Snark_params.Tock
    module B = Bigint

    module Scalar = struct
      include (
        Snark_params.Tick.Inner_curve.Scalar :
          module type of Snark_params.Tick.Inner_curve.Scalar
          with type t = Snark_params.Tick.Inner_curve.Scalar.t
           and type var := Snark_params.Tick.Inner_curve.Scalar.var
          with module Checked := Snark_params.Tick.Inner_curve.Scalar.Checked )

      let of_bits = Other_impl.Field.project

      include (Other_impl.Field : Sexpable.S with type t := t)

      include Binable.Of_sexpable (Other_impl.Field)

      let length_in_bits = size_in_bits

      open Impl

      type var = Boolean.var list

      let typ =
        Typ.transport
          (Typ.list ~length:size_in_bits Boolean.typ)
          ~there:Other_impl.Field.unpack ~back:Other_impl.Field.project

      let gen : t Quickcheck.Generator.t =
        Quickcheck.Generator.map
          (B.gen_incl B.one B.(Other_impl.Field.size - one))
          ~f:(fun x -> Other_impl.Bigint.(to_field (of_bignum_bigint x)))

      module Checked = struct
        let to_bits xs = Bitstring_lib.Bitstring.Lsb_first.of_list xs

        module Assert = struct
          let equal a b = Bitstring_checked.Assert.equal a b
        end
      end
    end

    module Curve = struct
      open Impl

      type var = Field.Var.t * Field.Var.t

      include (
        Snark_params.Tick.Inner_curve :
          module type of Snark_params.Tick.Inner_curve
          with type var := Snark_params.Tick.Inner_curve.var
          with module Checked := Snark_params.Tick.Inner_curve.Checked )

      type 'a or_infinity = 'a Marlin_plonk_bindings_types.Or_infinity.t =
        | Infinity
        | Finite of 'a
      [@@deriving equal]

      let equal x y =
        Snark_params.Tick.Inner_curve.(
          equal_or_infinity Affine.equal (to_affine_or_infinity x)
            (to_affine_or_infinity y))

      let scale_field = scale

      module Checked = struct
        include Snarky_curves.Make_weierstrass_checked
                  (Snarky_field_extensions.Field_extensions.F (Impl)) (Scalar)
                  (Snark_params.Tick.Inner_curve)
                  (Snark_params.Tick.Inner_curve.Params)
                  (struct
                    let add = None
                  end)

        let add_known_unsafe t x = add_unsafe t (constant x)
      end

      let typ = Checked.typ
    end

    module Group = struct
      open Impl

      module T = struct
        type t = Curve.t

        include Sexpable.Of_sexpable (struct
                    type t = Field.t * Field.t [@@deriving sexp]
                  end)
                  (struct
                    type t = Curve.t

                    let to_sexpable = Curve.to_affine_exn

                    let of_sexpable = Curve.of_affine
                  end)
      end

      include T
      include Binable.Of_sexpable (T)

      let equal = Curve.equal

      let zero = Curve.zero

      let add = Curve.add

      let negate = Curve.negate

      let scale = Curve.scale_field

      let generator = Curve.one

      type var = Curve.var

      let typ = Curve.typ

      let to_bits (t : t) =
        let x, y = Curve.to_affine_exn t in
        List.hd_exn (Field.unpack y) :: Field.unpack x

      let gen =
        let open Quickcheck.Generator.Let_syntax in
        let%map s = Scalar.gen in
        scale generator s

      let%test_unit "inv works" =
        Quickcheck.test gen ~f:(fun x ->
            let res = add x (negate x) in
            if not (equal res zero) then
              failwithf
                !"inv failured, x = %{sexp:t}, x + inv x = %{sexp:t}, \
                  expected %{sexp:t}"
                x res zero () )

      let%test_unit "scaling associates" =
        let open Quickcheck in
        test ~trials:50 (Generator.tuple2 Scalar.gen Scalar.gen)
          ~f:(fun (a, b) ->
            assert (
              equal
                (scale generator (Scalar.mul a b))
                (scale (scale generator a) b) ) )

      module Checked = struct
        include Curve.Checked

        let to_bits ((x, y) : var) =
          let%map x =
            Field.Checked.choose_preimage_var ~length:Field.size_in_bits x
          and y =
            Field.Checked.choose_preimage_var ~length:Field.size_in_bits y
          in
          List.hd_exn y :: x
      end
    end

    let params =
      Array.init (5 * Impl.Field.size_in_bits) ~f:(fun _ ->
          let t = Curve.random () in
          let tt = Curve.double t in
          (t, tt, Curve.add t tt, Curve.double tt) )

    module Pedersen =
      Snarky.Pedersen.Make (Impl) (Curve)
        (struct
          let params =
            Array.map
              ~f:(Tuple_lib.Quadruple.map ~f:Curve.to_affine_exn)
              params
        end)

    module Message = struct
      open Impl

      type t = Curve.t

      include Sexpable.Of_sexpable (struct
                  type t = Field.t * Field.t [@@deriving sexp]
                end)
                (struct
                  type t = Curve.t

                  let to_sexpable = Curve.to_affine_exn

                  let of_sexpable = Curve.of_affine
                end)

      type value = t [@@deriving sexp]

      type var = Curve.var

      let typ = Curve.typ

      let hash_to_group x = x

      module Checked = struct
        let hash_to_group = Impl.Checked.return
      end
    end

    let rec bits_to_triples ~default = function
      | b0 :: b1 :: b2 :: bs ->
          (b0, b1, b2) :: bits_to_triples ~default bs
      | [] ->
          []
      | [b] ->
          [(b, default, default)]
      | [b1; b2] ->
          [(b1, b2, default)]

    let hash_bits bits =
      List.foldi ~init:Curve.zero (bits_to_triples ~default:false bits)
        ~f:(fun i acc triple ->
          Curve.add acc
            (Snarky.Pedersen.local_function ~negate:Curve.negate params.(i)
               triple) )
      |> Curve.to_affine_exn |> fst

    let hash_bits_checked bits =
      let open Impl.Checked in
      Pedersen.hash
        ~init:(0, `Value Curve.zero)
        (bits_to_triples bits ~default:Impl.Boolean.false_)
      >>| Pedersen.digest

    module Output_hash = struct
      type t = Impl.Field.t

      type var = Pedersen.Digest.var

      let hash message pt = hash_bits (Group.to_bits message @ Group.to_bits pt)

      module Checked = struct
        let hash message pt =
          let open Impl in
          let%bind message = Group.Checked.to_bits message
          and pt = Group.Checked.to_bits pt in
          hash_bits_checked (message @ pt)
      end
    end

    module Hash = struct
      open Impl

      let hash_for_proof m g1 g2 g3 =
        let x = hash_bits (List.concat_map ~f:Group.to_bits [m; g1; g2; g3]) in
        Scalar.of_bits (List.take (Field.unpack x) Scalar.length_in_bits)

      module Checked = struct
        let hash_for_proof m g1 g2 g3 =
          let%bind bs =
            Checked.map
              (Checked.List.map ~f:Group.Checked.to_bits [m; g1; g2; g3])
              ~f:List.concat
          in
          let%map xs =
            Checked.(hash_bits_checked bs >>= Pedersen.Digest.choose_preimage)
          in
          List.take (xs :> Boolean.var list) Scalar.length_in_bits
      end
    end

    module Vrf = Make (Impl) (Scalar) (Group) (Message) (Output_hash) (Hash)

    let%test_unit "completeness" =
      let gen =
        let open Quickcheck.Generator.Let_syntax in
        let%map private_key = Scalar.gen and message = Group.gen in
        (private_key, Group.(scale generator private_key), message)
      in
      Quickcheck.test gen ~trials:50 ~f:(fun (priv, public_key, message) ->
          let eval = Vrf.Evaluation.create priv message in
          let ctx : Vrf.Context.t = {message; public_key} in
          if not (Option.is_some (Vrf.Evaluation.verified_output eval ctx))
          then
            failwithf
              !"%{sexp:Vrf.Context.t}, %{sexp:Vrf.Evaluation.t}"
              ctx eval () )
  end )
