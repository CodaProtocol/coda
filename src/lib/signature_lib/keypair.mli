open Core_kernel

type t = {public_key: Public_key.t; private_key: Private_key.t sexp_opaque}
[@@deriving sexp, compare]

include Comparable.S with type t := t

val of_private_key_exn : Private_key.t -> t

val create : unit -> t

module And_compressed_pk : sig
  type nonrec t = t * Public_key.Compressed.t [@@deriving sexp, compare]

  include Comparable.S with type t := t
end
