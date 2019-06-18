open Core_kernel
open Async_kernel
open Snark_params

module Stable = struct
  module V1 = struct
    module T = struct
      type t = Tick.Inner_curve.Scalar.t
      [@@deriving bin_io, sexp, version {asserted}]
    end

    include T
  end

  module Latest = V1
end

type t = Stable.Latest.t [@@deriving sexp]

let create () =
  (* This calls into libsnark which uses /dev/urandom *)
  Tick.Inner_curve.Scalar.random ()

let gen =
  let open Bignum_bigint in
  Quickcheck.Generator.map
    (gen_incl one (Snark_params.Tick.Inner_curve.Scalar.size - one))
    ~f:Snark_params.Tock.Bigint.(Fn.compose to_field of_bignum_bigint)

let of_bigstring_exn = Binable.of_bigstring (module Stable.Latest)

let to_bigstring = Binable.to_bigstring (module Stable.Latest)

let version_byte = '\x5A'

let to_base58_check t =
  let payload = to_bigstring t |> Bigstring.to_string in
  Base58_check.encode ~version_byte ~payload

let of_base58_check_exn s =
  let vb, decoded = Base58_check.decode_exn s in
  if not (Char.equal vb version_byte) then
    failwith "of.encode_exn: unexpected version byte" ;
  decoded |> Bigstring.of_string |> of_bigstring_exn
