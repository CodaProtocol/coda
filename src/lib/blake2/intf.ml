open Core_kernel

module type S = sig
  val digest_size_in_bits : int

  val digest_size_in_bytes : int

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp, compare, hash, equal]
    end
  end]

  include Hashable.S with type t := t

  include Comparable.S with type t := t

  val bits_to_string : bool array -> string

  val string_to_bits : string -> bool array

  val to_raw_string : t -> string

  val to_hex : t -> string

  val digest_string : ?off:int -> ?len:int -> String.t -> t
end
