module type S = sig
  type t [@@deriving equal, sexp]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, equal, sexp]
    end

    module Latest : module type of V1
  end
end

module type Hash = sig
  type t [@@deriving compare]

  include S with type t := t

  val merge : height:int -> t -> t -> t
end

module type Key = S

module type Account = sig
  type hash

  include S

  val data_hash : t -> hash
end
