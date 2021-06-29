(** A non-empty list that is safe by construction. *)

module Stable : sig
  module V1 : sig
    type 'a t [@@deriving sexp, compare, equal, hash, bin_io, version]
  end

  module Latest = V1
end

(* no bin_io on purpose *)
type 'a t = 'a Stable.Latest.t [@@deriving sexp, compare, equal, hash]

(** Create a non-empty list by proving you have a head element *)
val init : 'a -> 'a list -> 'a t

(** Create a non-empty list with a single element *)
val singleton : 'a -> 'a t

(** Deconstruct a non-empty list into the head and tail *)
val uncons : 'a t -> 'a * 'a list

(** Prepend a new element *)
val cons : 'a -> 'a t -> 'a t

(** The first element of the container *)
val head : 'a t -> 'a

(** The zero or more tail elements of the container *)
val tail : 'a t -> 'a list

val last : 'a t -> 'a

(** The reverse ordered list *)
val rev : 'a t -> 'a t

(** Convert a list into a non-empty-list, returning [None] if the list is
 * empty *)
val of_list_opt : 'a list -> 'a t option

(** Get the tail as a non-empty-list *)
val tail_opt : 'a t -> 'a t option

(** Apply a function to each element of the non empty list *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(* The following functions are computed from {!module:Base.Container.Make}. See
 * {!modtype:Base.Container_intf} for more information *)

val find : 'a t -> f:('a -> bool) -> 'a option

val find_map : 'a t -> f:('a -> 'b option) -> 'b option

val fold : 'a t -> init:('a -> 'accum) -> f:('accum -> 'a -> 'accum) -> 'accum

val iter : 'a t -> f:('a -> unit) -> unit

val length : 'a t -> int

(** Note: This is O(1) not O(n) like on most container *)
val to_list : 'a t -> 'a list

val append : 'a t -> 'a t -> 'a t

val take : 'a t -> int -> 'a t option

val min_elt : compare:('a -> 'a -> int) -> 'a t -> 'a

val max_elt : compare:('a -> 'a -> int) -> 'a t -> 'a

val iter_deferred :
     'a t
  -> f:('a -> unit Async_kernel.Deferred.t)
  -> unit Async_kernel.Deferred.t
