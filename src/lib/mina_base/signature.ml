[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick

[%%else]

open Snark_params_nonconsensus
module Hex = Hex_nonconsensus.Hex
module Rosetta_coding = Rosetta_coding_nonconsensus

[%%endif]

module Arg = struct
  type t = (Field.t, Inner_curve.Scalar.t) Signature_poly.Stable.Latest.t
  [@@deriving bin_io_unversioned]

  let description = "Signature"

  let version_byte = Base58_check.Version_bytes.signature
end

[%%versioned_asserted
module Stable = struct
  module V1 = struct
    type t = (Field.t, Inner_curve.Scalar.t) Signature_poly.Stable.V1.t
    [@@deriving sexp, compare, equal, hash]

    type _unused = unit constraint t = Arg.t

    include Codable.Make_base58_check (Arg)

    let to_latest = Fn.id

    let gen = Quickcheck.Generator.tuple2 Field.gen Inner_curve.Scalar.gen
  end

  module Tests = struct
    [%%if
    curve_size = 255]

    let%test "signature serialization v1 (curve_size=255)" =
      let signature =
        Quickcheck.random_value
          ~seed:(`Deterministic "signature serialization") V1.gen
      in
      let known_good_digest = "88a094d50a90b5054152af85bd6e60e8" in
      Ppx_version_runtime.Serialization.check_serialization
        (module V1)
        signature known_good_digest

    [%%else]

    let%test "signature serialization v1" =
      failwith "No test for this curve size"

    [%%endif]
  end
end]

let dummy = (Field.one, Inner_curve.Scalar.one)

module Raw = struct
  open Rosetta_coding.Coding

  let encode (field, scalar) = of_field field ^ of_scalar scalar

  let decode raw =
    let len = String.length raw in
    let field_len = len / 2 in
    let field_enc = String.sub raw ~pos:0 ~len:field_len in
    let scalar_enc = String.sub raw ~pos:field_len ~len:field_len in
    try Some (to_field field_enc, to_scalar scalar_enc) with _ -> None

  let%test_unit "partial isomorphism" =
    Quickcheck.test ~trials:300 Stable.Latest.gen ~f:(fun signature ->
        [%test_eq: t option] (Some signature) (encode signature |> decode) )
end

[%%ifdef
consensus_mechanism]

type var = Field.Var.t * Inner_curve.Scalar.var

[%%endif]

[%%define_locally
Stable.Latest.(of_base58_check_exn, of_base58_check, of_yojson, to_yojson)]
