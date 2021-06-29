open Core_kernel

module Stable : sig
  module V1 : sig
    type t =
      { public_key: Public_key.Stable.V1.t
      ; private_key: Private_key.Stable.V1.t sexp_opaque }
    [@@deriving sexp, bin_io, version, to_yojson]
  end

  module Latest = V1
end

type t = Stable.Latest.t =
  {public_key: Public_key.t; private_key: Private_key.t sexp_opaque}
[@@deriving sexp, compare, to_yojson]

include Comparable.S with type t := t

val of_private_key_exn : Private_key.t -> t

val create : unit -> t

val gen : t Quickcheck.Generator.t

module And_compressed_pk : sig
  type nonrec t = t * Public_key.Compressed.t [@@deriving sexp]

  include Comparable.S with type t := t
end
