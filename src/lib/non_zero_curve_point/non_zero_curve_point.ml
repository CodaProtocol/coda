[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick

let parity y = Bigint.(test_bit (of_field y) 0)

[%%else]

open Snark_params_nonconsensus

let parity y = Field.parity y

module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

let gen_uncompressed =
  Quickcheck.Generator.filter_map Field.gen_uniform ~f:(fun x ->
      let open Option.Let_syntax in
      let%map y = Inner_curve.find_y x in
      (x, y) )

module Compressed = struct
  open Compressed_poly

  module Arg = struct
    (* module with same type t as Stable below, to give as functor argument *)
    [%%versioned_asserted
    module Stable = struct
      module V1 = struct
        type t = (Field.t, bool) Poly.Stable.V1.t

        let to_latest = Fn.id

        let description = "Non zero curve point compressed"

        let version_byte =
          Base58_check.Version_bytes.non_zero_curve_point_compressed
      end

      module Tests = struct
        (* actual tests in Stable below *)
      end
    end]
  end

  let compress (x, y) = {Poly.x; is_odd= parity y}

  [%%versioned_asserted
  module Stable = struct
    module V1 = struct
      type t = (Field.t, bool) Poly.Stable.V1.t [@@deriving eq, compare, hash]

      (* dummy type for inserting constraint
         adding constraint to t produces "unused rec" error
      *)
      type unused = unit constraint t = Arg.Stable.V1.t

      let to_latest = Fn.id

      module Base58 = Codable.Make_base58_check (Arg.Stable.V1)
      include Base58

      (* sexp representation is a Base58Check string, like the yojson representation *)
      let sexp_of_t t = to_base58_check t |> Sexp.of_string

      let t_of_sexp sexp = Sexp.to_string sexp |> of_base58_check_exn

      let gen =
        let open Quickcheck.Generator.Let_syntax in
        let%map uncompressed = gen_uncompressed in
        compress uncompressed
    end

    module Tests = struct
      (* these tests check not only whether the serialization of the version-asserted type has changed,
         but also whether the serializations for the consensus and nonconsensus code are identical
       *)

      [%%if
      curve_size = 255]

      let%test "nonzero_curve_point_compressed v1" =
        let point =
          Quickcheck.random_value
            ~seed:(`Deterministic "nonzero_curve_point_compressed-seed") V1.gen
        in
        let known_good_digest = "35c836b0252293061bf974490f5bd515" in
        Ppx_version_runtime.Serialization.check_serialization
          (module V1)
          point known_good_digest

      [%%else]

      let%test "nonzero_curve_point_compressed v1" =
        failwith "Unknown curve size"

      [%%endif]
    end
  end]

  module Poly = Poly
  include Comparable.Make_binable (Stable.Latest)
  include Hashable.Make_binable (Stable.Latest)
  include Stable.Latest.Base58

  let to_string = to_base58_check

  [%%define_locally
  Stable.Latest.(sexp_of_t, t_of_sexp, gen)]

  let compress (x, y) = {Poly.x; is_odd= parity y}

  let empty = Poly.{x= Field.zero; is_odd= false}

  let to_input {Poly.x; is_odd} =
    {Random_oracle.Input.field_elements= [|x|]; bitstrings= [|[is_odd]|]}

  [%%ifdef
  consensus_mechanism]

  (* snarky-dependent *)

  type var = (Field.Var.t, Boolean.var) Poly.t

  let typ : (var, t) Typ.t =
    Typ.of_hlistable [Field.typ; Boolean.typ] ~var_to_hlist:Poly.to_hlist
      ~var_of_hlist:Poly.of_hlist ~value_to_hlist:Poly.to_hlist
      ~value_of_hlist:Poly.of_hlist

  let var_of_t ({x; is_odd} : t) : var =
    {x= Field.Var.constant x; is_odd= Boolean.var_of_value is_odd}

  let assert_equal (t1 : var) (t2 : var) =
    let%map () = Field.Checked.Assert.equal t1.x t2.x
    and () = Boolean.Assert.(t1.is_odd = t2.is_odd) in
    ()

  module Checked = struct
    let equal t1 t2 =
      let%bind x_eq = Field.Checked.equal t1.Poly.x t2.Poly.x in
      let%bind odd_eq = Boolean.equal t1.is_odd t2.is_odd in
      Boolean.(x_eq && odd_eq)

    let to_input = to_input

    let if_ cond ~then_:t1 ~else_:t2 =
      let%map x = Field.Checked.if_ cond ~then_:t1.Poly.x ~else_:t2.Poly.x
      and is_odd = Boolean.if_ cond ~then_:t1.is_odd ~else_:t2.is_odd in
      Poly.{x; is_odd}

    module Assert = struct
      let equal t1 t2 =
        let%map () = Field.Checked.Assert.equal t1.Poly.x t2.Poly.x
        and () = Boolean.Assert.(t1.is_odd = t2.is_odd) in
        ()
    end
  end

  (* end snarky-dependent *)
  [%%endif]
end

module Uncompressed = struct
  let decompress ({x; is_odd} : Compressed.t) =
    Option.map (Inner_curve.find_y x) ~f:(fun y ->
        let y_parity = parity y in
        let y = if Bool.(is_odd = y_parity) then y else Field.negate y in
        (x, y) )

  let decompress_exn t =
    match decompress t with
    | Some d ->
        d
    | None ->
        failwith
          (sprintf "Compressed public key %s could not be decompressed"
             (Yojson.Safe.to_string @@ Compressed.to_yojson t))

  let of_base58_check_decompress_exn pk_str =
    let pk = Compressed.of_base58_check_exn pk_str in
    decompress_exn pk |> ignore ;
    pk

  let compress = Compressed.compress

  [%%versioned_binable
  module Stable = struct
    module V1 = struct
      type t = Field.t * Field.t [@@deriving eq, compare, hash]

      let to_latest = Fn.id

      include Binable.Of_binable
                (Compressed.Stable.V1)
                (struct
                  type nonrec t = t

                  let of_binable = decompress_exn

                  let to_binable = compress
                end)

      let gen : t Quickcheck.Generator.t = gen_uncompressed

      let of_bigstring bs =
        let open Or_error.Let_syntax in
        let%map elem, _ = Bigstring.read_bin_prot bs bin_reader_t in
        elem

      let to_bigstring elem =
        let bs =
          Bigstring.create (bin_size_t elem + Bin_prot.Utils.size_header_length)
        in
        let _ = Bigstring.write_bin_prot bs bin_writer_t elem in
        bs

      (* We reuse the Base58check-based yojson (de)serialization from the
         compressed representation. *)

      let of_yojson json =
        let open Result in
        Compressed.of_yojson json
        >>= fun compressed ->
        Result.of_option ~error:"couldn't decompress, curve point invalid"
          (decompress compressed)

      let to_yojson t = Compressed.to_yojson @@ compress t

      (* as for yojson, use the Base58check-based sexps from the compressed representation *)
      let sexp_of_t t = Compressed.sexp_of_t @@ compress t

      let t_of_sexp sexp =
        Option.value_exn (decompress @@ Compressed.t_of_sexp sexp)
    end
  end]

  (* so we can make sets of public keys *)
  include Comparable.Make_binable (Stable.Latest)

  [%%define_locally
  Stable.Latest.
    (of_bigstring, to_bigstring, sexp_of_t, t_of_sexp, to_yojson, of_yojson)]

  let gen : t Quickcheck.Generator.t = gen_uncompressed

  let ( = ) = equal

  let of_inner_curve_exn = Inner_curve.to_affine_exn

  let to_inner_curve = Inner_curve.of_affine

  let%test_unit "point-compression: decompress . compress = id" =
    Quickcheck.test gen ~f:(fun pk ->
        assert (equal (decompress_exn (compress pk)) pk) )

  [%%ifdef
  consensus_mechanism]

  (* snarky-dependent *)

  type var = Field.Var.t * Field.Var.t

  let assert_equal var1 var2 =
    let open Field.Checked.Assert in
    let v1_f1, v1_f2 = var1 in
    let v2_f1, v2_f2 = var2 in
    let%bind () = equal v1_f1 v2_f1 in
    let%map () = equal v1_f2 v2_f2 in
    ()

  let var_of_t (x, y) = (Field.Var.constant x, Field.Var.constant y)

  let typ : (var, t) Typ.t = Typ.(field * field)

  let parity_var y =
    let%map bs = Field.Checked.unpack_full y in
    List.hd_exn (Bitstring_lib.Bitstring.Lsb_first.to_list bs)

  let decompress_var' ({x; is_odd} : Compressed.var) =
    let open Let_syntax in
    let%bind y, on_curve =
      let%bind x2 = Field.Checked.square x in
      let%bind x3_ax =
        Field.Checked.(mul x (x2 + Field.Var.constant Inner_curve.Params.a))
      in
      Field.Checked.(
        sqrt_check (x3_ax + Field.Var.constant Inner_curve.Params.b))
    in
    let%bind parity = parity_var y in
    let%bind y =
      let%bind sgn =
        let ( <> ) = Boolean.( lxor ) in
        let%bind switch = parity <> is_odd in
        Field.Checked.if_ switch
          ~then_:(Field.Var.constant Field.(negate one))
          ~else_:(Field.Var.constant Field.one)
      in
      Field.Checked.mul sgn y
    in
    let gx, gy = Inner_curve.(to_affine_exn one) in
    let%bind x =
      Field.Checked.if_ on_curve ~then_:x ~else_:(Field.Var.constant gx)
    in
    let%bind y =
      Field.Checked.if_ on_curve ~then_:y ~else_:(Field.Var.constant gy)
    in
    return ((x, y), on_curve)

  let decompress_var ({x; is_odd} as c : Compressed.var) =
    let open Let_syntax in
    let%bind y =
      exists Typ.field
        ~compute:
          As_prover.(
            map (read Compressed.typ c) ~f:(fun c -> snd (decompress_exn c)))
    in
    let%map () = Inner_curve.Checked.Assert.on_curve (x, y)
    and () = parity_var y >>= Boolean.Assert.(( = ) is_odd) in
    (x, y)

  let%snarkydef compress_var ((x, y) : var) : (Compressed.var, _) Checked.t =
    let open Compressed_poly in
    let%map is_odd = parity_var y in
    {Poly.x; is_odd}

  (* end snarky-dependent *)
  [%%endif]
end

include Uncompressed
