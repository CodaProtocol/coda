open Core_kernel
open Quickcheck

(** [map_gens ls ~f] maps over [ls], building one list generator using each generator
 *  returned from successive applications of [f] over elements of [ls]. *)
val map_gens : 'a list -> f:('a -> 'b Generator.t) -> 'b list Generator.t

(** [imperative_fixed_point root ~f] creates a fixed point generator which enables imperative
 *  logic (where previously generated values effect future generated values) by generating and
 *  applying a series of nested closures. *)
val imperative_fixed_point :
  'a -> f:(('a -> 'b) Generator.t -> ('a -> 'b) Generator.t) -> 'b Generator.t

val gen_pair : 'a Generator.t -> ('a * 'a) Generator.t

(** [gen_division n k] generates a list of [k] integers which sum to [n] *)
val gen_division : int -> int -> int list Generator.t

(** [gen_imperative_list ~p head_gen elem_gen] generates an imperative list
 *  structure generated by [elem_gen], starting from an initial element
 *  generated by [head_gen]. [head_gen] generates a function that will be called
 *  with a pervious node in order to build the list *)
val gen_imperative_list :
  'a Generator.t -> ('a -> 'a) Generator.t -> 'a list Generator.t

(** [gen_imperative_ktree ~p root_gen node_gen] generates an imperative ktree structure
 *  as a flat list of nodes, generated by [node_gen], starting from a root generated by [root_gen].
 *  [node_gen] generates a function that will be called with the parent node in order to build
 *  the tree. The value [p] is the geometric distribution (or "radioactive decay") probability
 *  that is determines the number of forks at each node. Sizes of forks in the tree are
 *  distributed uniformly. *)
val gen_imperative_ktree :
  ?p:float -> 'a Generator.t -> ('a -> 'a) Generator.t -> 'a list Generator.t

val gen_imperative_rose_tree :
     ?p:float
  -> 'a Generator.t
  -> ('a -> 'a) Generator.t
  -> 'a Rose_tree.t Generator.t
