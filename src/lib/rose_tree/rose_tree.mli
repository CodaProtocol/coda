(**
 * A [Rose_tree.t] is a tree with at least 1 element where a node
 * has a variable number of successors.
 *
 * @see <https://en.wikipedia.org/wiki/Rose_tree> Wikipedia Article
 *)

open Core_kernel
open Async_kernel

type 'a t = T of 'a * 'a t list [@@deriving yojson]

val root : 'a t -> 'a

val children : 'a t -> 'a t list

val print : ?whitespace:int -> element_to_string:('a -> string) -> 'a t -> unit

val of_list_exn : ?subtrees:'a t list -> 'a list -> 'a t

val of_non_empty_list : ?subtrees:'a t list -> 'a Non_empty_list.t -> 'a t

val equal : f:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val equiv : f:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val iter : 'a t -> f:('a -> unit) -> unit

val map : 'a t -> f:('a -> 'b) -> 'b t

val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

val fold_map_over_subtrees : 'a t -> init:'b -> f:('b -> 'a t -> 'b) -> 'b t

val flatten : 'a t -> 'a list

module Deferred : sig
  val iter : 'a t -> f:('a -> unit Deferred.t) -> unit Deferred.t

  val map : 'a t -> f:('a -> 'b Deferred.t) -> 'b t Deferred.t

  val fold_map :
    'a t -> init:'b -> f:('b -> 'a -> 'b Deferred.t) -> 'b t Deferred.t

  val all : 'a Deferred.t t -> 'a t Deferred.t

  module Or_error : sig
    val iter :
      'a t -> f:('a -> unit Deferred.Or_error.t) -> unit Deferred.Or_error.t

    val map :
      'a t -> f:('a -> 'b Deferred.Or_error.t) -> 'b t Deferred.Or_error.t

    val fold_map :
         'a t
      -> init:'b
      -> f:('b -> 'a -> 'b Deferred.Or_error.t)
      -> 'b t Deferred.Or_error.t

    val fold_map_over_subtrees :
         'a t
      -> init:'b
      -> f:('b -> 'a t -> 'b Deferred.Or_error.t)
      -> 'b t Deferred.Or_error.t
  end
end

module Or_error : sig
  val iter : 'a t -> f:('a -> unit Or_error.t) -> unit Or_error.t

  val map : 'a t -> f:('a -> 'b Or_error.t) -> 'b t Or_error.t

  val fold_map :
    'a t -> init:'b -> f:('b -> 'a -> 'b Or_error.t) -> 'b t Or_error.t

  val fold_map_over_subtrees :
    'a t -> init:'b -> f:('b -> 'a t -> 'b Or_error.t) -> 'b t Or_error.t
end
