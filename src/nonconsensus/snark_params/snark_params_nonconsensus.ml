(* snark_params_nonconsensus.ml *)

[%%import
"/src/config.mlh"]

[%%ifdef
consensus_mechanism]

[%%error
"Snark_params_nonconsensus should not be compiled if there's a consensus \
 mechanism"]

[%%endif]

open Snarkette

[%%if
curve_size = 255]

(* only size we should be building nonconsensus code for *)

[%%else]

[%%show
curve_size]

[%%error
"invalid value for \"curve_size\""]

[%%endif]

[%%inject
"ledger_depth", ledger_depth]

module Field = struct
  open Core_kernel

  [%%versioned_asserted
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t = Pasta.Fp.t [@@deriving equal, compare, yojson, sexp, hash]

      let to_latest x = x
    end

    module Tests = struct end
  end]

  include Pasta.Fp

  let size = order |> Snarkette.Nat.to_string |> Bigint.of_string

  let size_in_bits = length_in_bits

  let unpack t = to_bits t

  let project bits =
    Core_kernel.Option.value_exn ~message:"project: invalid bits"
      (of_bits bits)
end

module Tock = struct
  module Field = struct
    type t = Pasta.Fq.t

    let unpack (t : t) = Pasta.Fq.to_bits t

    let size_in_bits = Pasta.Fq.length_in_bits

    let project bits =
      Core_kernel.Option.value_exn
        ~message:"Snark_params_nonconsensus.Tock.Field.project"
        (Pasta.Fq.of_bits bits)
  end
end

module Inner_curve = struct
  module C = Pasta.Pallas

  type t = C.t [@@deriving sexp]

  module Coefficients = C.Coefficients

  let find_y x =
    let open Field in
    let y2 = (x * square x) + (Coefficients.a * x) + Coefficients.b in
    if is_square y2 then Some (sqrt y2) else None

  [%%define_locally
  C.(of_affine, to_affine, to_affine_exn, one, ( + ), negate)]

  module Scalar = struct
    (* though we have bin_io, not versioned here; this type exists for Private_key.t,
       where it is versioned-asserted and its serialization tested
     *)
    type t = Pasta.Fq.t [@@deriving bin_io_unversioned, sexp]

    type _unused = unit constraint t = Tock.Field.t

    let size = Pasta.Fq.order

    [%%define_locally
    Pasta.Fq.
      ( to_string
      , of_string
      , equal
      , compare
      , size
      , zero
      , one
      , ( + )
      , ( - )
      , ( * )
      , gen_uniform_incl
      , negate
      , hash_fold_t )]

    (* Pasta.Fq.gen uses the interval starting at zero
       here we follow the gen in Snark_params.Make_inner_curve_scalar, using
         an interval starting at one
    *)

    let gen = Pasta.Fq.(gen_incl one (zero - one))

    let gen_uniform = gen_uniform_incl one (zero - one)

    let unpack t = Tock.Field.unpack t

    let of_bits bits = Tock.Field.project bits

    let project = of_bits
  end

  let scale (t : t) (scalar : Scalar.t) = C.scale t (scalar :> Nat.t)

  let scale_field (t : t) x = scale t (Pasta.Fq.of_bigint x :> Scalar.t)
end
