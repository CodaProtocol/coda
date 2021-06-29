module type S = sig
  type hash

  type elem = [`Left of hash | `Right of hash] [@@deriving sexp, equal]

  val elem_hash : elem -> hash

  type t = elem list [@@deriving sexp, equal]

  val implied_root : t -> hash -> hash
end
