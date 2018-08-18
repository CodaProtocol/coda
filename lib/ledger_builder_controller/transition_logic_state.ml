open Core_kernel

module type S = sig
  type transition

  type tip

  module Transition_tree : Coda.Ktree_intf with type elem := transition

  type t [@@deriving bin_io]

  val locked_tip : t -> tip

  val longest_branch_tip : t -> tip

  val ktree : t -> Transition_tree.t option

  module Change : sig
    type t =
      | Locked_tip of tip
      | Longest_branch_tip of tip
      | Ktree of Transition_tree.t
  end

  val apply_all : t -> Change.t list -> t
  (** Invariant: Changes must be applied to atomically result in a consistent state *)

  val create : tip -> t
end

module Make (Security : sig
  val max_depth : [`Infinity | `Finite of int]
end) (Transition : sig
  type t [@@deriving compare, sexp, bin_io]
end) (Tip : sig
  type t [@@deriving bin_io]

  val is_materialization_of : t -> Transition.t -> bool
end) :
  S with type tip := Tip.t and type transition := Transition.t =
struct
  module Transition_tree = Ktree.Make (Transition) (Security)

  module Change = struct
    type t =
      | Locked_tip of Tip.t
      | Longest_branch_tip of Tip.t
      | Ktree of Transition_tree.t
  end

  open Change

  (**
   *       /-----
   *      *
   *      ^\-------
   *      |      \----
   *      O          ^
   *                 |
   *                 O
   *
   *    The ktree represents the fork tree. We annotate
   *    the root and longest_branch with Tip.t's.
   *)
  type t =
    { locked_tip: Tip.t
    ; longest_branch_tip: Tip.t
    ; ktree: Transition_tree.t option
    (* TODO: This impl assumes we have the original Ouroboros assumption. In
       order to work with the Praos assumption we'll need to keep a linked
       list as well at the prefix of size (#blocks possible out of order)
     *)
    }
  [@@deriving fields, bin_io]

  let apply t = function
    | Locked_tip h -> {t with locked_tip= h}
    | Longest_branch_tip h -> {t with longest_branch_tip= h}
    | Ktree k -> {t with ktree= Some k}

  (* Invariant: state is consistent after change applications *)
  let state_valid t =
    match t.ktree with
    | None -> true
    | Some ktree ->
      match Transition_tree.longest_path ktree with
      | [] -> failwith "Impossible, paths are non-empty"
      | [x] ->
          Tip.is_materialization_of t.locked_tip x
          && Tip.is_materialization_of t.longest_branch_tip x
      | x :: y :: rest ->
          let last = List.last_exn (y :: rest) in
          Tip.is_materialization_of t.locked_tip x
          && Tip.is_materialization_of t.longest_branch_tip last

  let apply_all t changes =
    assert (state_valid t) ;
    let t' = List.fold changes ~init:t ~f:apply in
    assert (state_valid t') ;
    t'

  let create genesis_heavy =
    {locked_tip= genesis_heavy; longest_branch_tip= genesis_heavy; ktree= None}
end
