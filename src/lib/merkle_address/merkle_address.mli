open Core_kernel

type t [@@deriving sexp, hash, equal, compare, to_yojson]

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving sexp, bin_io, hash, equal, compare, to_yojson, version]
  end

  module Latest : module type of V1
end

include Hashable.S_binable with type t := t

val of_byte_string : string -> t

val of_directions : Direction.t list -> t

val root : unit -> t

val slice : t -> int -> int -> t

val get : t -> int -> int

val copy : t -> t

val parent : t -> t Or_error.t

val child : ledger_depth:int -> t -> Direction.t -> t Or_error.t

val child_exn : ledger_depth:int -> t -> Direction.t -> t

val parent_exn : t -> t

val dirs_from_root : t -> Direction.t list

val sibling : t -> t

val next : t -> t Option.t

val prev : t -> t Option.t

val is_leaf : ledger_depth:int -> t -> bool

val is_parent_of : t -> maybe_child:t -> bool

val serialize : ledger_depth:int -> t -> Bigstring.t

val to_string : t -> string

val pp : Format.formatter -> t -> unit

module Range : sig
  type nonrec t = t * t

  val fold :
       ?stop:[`Inclusive | `Exclusive]
    -> t
    -> init:'a
    -> f:(Stable.Latest.t -> 'a -> 'a)
    -> 'a

  val subtree_range : ledger_depth:int -> Stable.Latest.t -> t

  val subtree_range_seq :
    ledger_depth:int -> Stable.Latest.t -> Stable.Latest.t Sequence.t
end

val depth : t -> int

val height : ledger_depth:int -> t -> int

val to_int : t -> int

val of_int_exn : ledger_depth:int -> int -> t
