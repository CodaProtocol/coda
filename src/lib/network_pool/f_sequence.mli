(** A sequence type based on finger trees.

      They are a purely functional data structure that provides amortized O(1)
      cons, snoc, uncons, and unsnoc; O(log (min (d1, d2))) splitting and
      indexing, where d1 and d2 are the distances from the first and and last
      elements respectively, and O(log(min(n1, n2))) appends where n1 and n2 are
      the sizes of argument trees.

      We want this because we need efficient deque operations for multiple
      transactions queued from the same sender, and we need to be able to drop
      any transactions after one we replace efficiently.

      See "Finger Trees, a simple general-purpose data structure" by Ralf Hinze
      and Ross Paterson for more details.

      http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf

      Helpful diagrams here:
      http://www.staff.city.ac.uk/~ross/papers/FingerTree/more-trees.html
*)
open Core

type 'e t [@@deriving compare, equal, sexp_of]

val is_empty : 'e t -> bool

val length : 'e t -> int

val head_exn : 'e t -> 'e

val last_exn : 'e t -> 'e

val uncons : 'e t -> ('e * 'e t) option

val unsnoc : 'e t -> ('e t * 'e) option

val foldl : ('a -> 'e -> 'a) -> 'a -> 'e t -> 'a

val foldr : ('e -> 'a -> 'a) -> 'a -> 'e t -> 'a

val iter : 'e t -> f:('e -> unit) -> unit

val to_seq : 'e t -> 'e Sequence.t

val to_list : 'e t -> 'e list

val empty : 'e t

val singleton : 'e -> 'e t

val cons : 'e -> 'e t -> 'e t

val snoc : 'e t -> 'e -> 'e t

(** Split a sequence at a given index. The first component of the pair will
    be a sequence containing all elements with index < i, the second will
    contain all elements with index >= i *)
val split_at : 'e t -> int -> 'e t * 'e t

val find : 'e t -> f:('e -> bool) -> 'e option
