open Pickles_types

type t = (Nat.N2.n, Nat.N2.n) Pickles.Proof.t
[@@deriving sexp, compare, yojson]

val blockchain_dummy : t

val transaction_dummy : t

[%%versioned:
module Stable : sig
  [@@@no_toplevel_latest_type]

  module V1 : sig
    type nonrec t = t [@@deriving compare, sexp, yojson]
  end
end]
