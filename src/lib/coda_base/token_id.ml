[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick

[%%else]

module Random_oracle = Random_oracle_nonconsensus
open Snark_params_nonconsensus

[%%endif]

[%%versioned
module Stable = struct
  module V1 = struct
    type t = Field.t [@@deriving version {asserted}, sexp, eq, hash, compare]

    let to_latest = Fn.id

    let to_yojson x = `String (Field.to_string x)

    let of_yojson = function
      | `String x ->
          Ok (Field.of_string x)
      | _ ->
          Error "Coda_base.Account.Token_id.of_yojson"
  end
end]

type t = Stable.Latest.t [@@deriving sexp, compare]

let to_input (x : t) = Random_oracle.Input.field x

let to_string = Field.to_string

let of_string = Field.of_string

let invalid = Field.zero

let default = Field.one

let next x = Field.(x + one)

let gen = Field.gen_uniform

let unpack = Field.unpack

include Comparable.Make (Stable.Latest)

[%%ifdef
consensus_mechanism]

type var = Field.Var.t

let typ = Field.typ

let var_of_t = Field.Var.constant

module Checked = struct
  open Snark_params.Tick

  let to_input (x : var) = Random_oracle.Input.field x

  let equal = Field.Checked.equal

  let if_ = Field.Checked.if_

  module Assert = struct
    let equal = Field.Checked.Assert.equal

    let not_equal = Field.Checked.Assert.not_equal
  end
end

[%%endif]
