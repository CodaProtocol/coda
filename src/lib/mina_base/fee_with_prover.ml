open Core_kernel
open Signature_lib

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      {fee: Currency.Fee.Stable.V1.t; prover: Public_key.Compressed.Stable.V1.t}
    [@@deriving sexp, yojson, hash]

    let to_latest = Fn.id

    module T = struct
      type typ = t [@@deriving sexp]

      type t = typ [@@deriving sexp]

      (* TODO: Compare in a better way than with public key, like in transaction pool *)
      let compare t1 t2 =
        let r = compare t1.fee t2.fee in
        if Int.( <> ) r 0 then r
        else Public_key.Compressed.compare t1.prover t2.prover
    end

    include Comparable.Make (T)
  end
end]

include Comparable.Make (Stable.V1.T)

let gen =
  Quickcheck.Generator.map2 Currency.Fee.gen Public_key.Compressed.gen
    ~f:(fun fee prover -> {fee; prover})
