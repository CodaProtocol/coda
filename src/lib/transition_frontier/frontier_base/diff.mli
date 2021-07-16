open Mina_base
open Mina_transition

type full = Full

type lite = Lite

(** A node can be represented in two different formats.
 *  A full node representation is a breadcrumb, which
 *  contains both an external transition and a computed
 *  staged ledger (masked off of the node parent's
 *  staged ledger). A lite node representation is merely
 *  the external transition itself. The staged ledger
 *  can be recomputed if needed, though not cheaply.
 *  The purpose of the separation of these two formats
 *  is required due to the fact that a breadcrumb cannot
 *  be serialized with bin_io. Only the external transition
 *  can be serialized and persisted to disk. This node
 *  representation type is used to parameterize the diff
 *  type over which representation is being used so that
 *  the diff format can be shared between both the in memory
 *  transition frontier and the persistent transition frontier.
 *)
module Node : sig
  type _ t =
    | Full : Breadcrumb.t -> full t
    | Lite : External_transition.Validated.t -> lite t
end

module Node_list : sig
  type full_node =
    { transition : External_transition.Validated.t
    ; scan_state : Staged_ledger.Scan_state.t
    }

  type lite_node = State_hash.Stable.V1.t

  type _ t = Full : full_node list -> full t | Lite : lite_node list -> lite t

  type 'repr node_list = 'repr t

  val to_lite : full_node list -> lite_node list

  module Lite : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t = lite node_list
      end
    end]
  end
end

(** A root transition is a representation of the
 *  change that occurs in a transition frontier when the
 *  root is transitioned. It contains a pointer to the new
 *  root, as well as pointers to all the nodes which are removed
 *  by transitioning the root.
 *)
module Root_transition : sig
  type 'repr t = { new_root : Root_data.Limited.t; garbage : 'repr Node_list.t }

  type 'repr root_transition = 'repr t

  module Lite : sig
    [%%versioned:
    module Stable : sig
      module V2 : sig
        type t = lite root_transition
      end

      module V1 : sig
        type t

        val to_latest : t -> V2.t
      end
    end]
  end
end

(** A transition frontier diff represents a single item
 *  of mutation that can be or has been performed on
 *  a transition frontier. Each diff is associated with
 *  a type parameter that represents a "diff mutant".
 *  A "diff mutant" is any information related to the
 *  correct application of a diff which is not encapsulated
 *  directly within the itself. This is used for computing
 *  the transition frontier incremental hash. For example,
 *  if some diff adds some new information, the diff itself
 *  would contain the information it's adding, but if the
 *  act of adding that information correctly to the transition
 *  frontier depends on some other state at the time the
 *  diff is applied, that state should be represented in mutant
 *  parameter for that diff.
 *)
type ('repr, 'mutant) t =
  | New_node : 'repr Node.t -> ('repr, unit) t
      (** A diff representing new nodes which are added to
   *  the transition frontier. This has no mutant as adding
   *  a node merely depends on its parent being in the
   *  transition frontier already. If the parent wasn't
   *  already in the transition frontier, attempting to
   *  process this diff would generate an error instead. *)
  | Root_transitioned : 'repr Root_transition.t -> ('repr, State_hash.t) t
      (** A diff representing that the transition frontier root
   *  has been moved forward. The diff contains the state hash
   *  of the new root, as well as state hashes of all nodes that
   *  were garbage collected by this root change. Garbage is
   *  topologically sorted from oldest to youngest. The old root
   *  should not be included in the garbage since it is implicitly
   *  removed. The mutant for this diff is the state hash of the
   *  old root. This ensures that all transition frontiers agreed
   *  on the old roots value at the time of processing this diff.
   *)
  | Best_tip_changed : State_hash.t -> (_, State_hash.t) t
      (** A diff representing that there is a new best tip in
   *  the transition frontier. The mutant for this diff is
   *  the state hash of the old best tip. This ensures that
   *  all transition frontiers agreed on the old best tip
   *  pointer at the time of processing this diff.
   *)

type ('repr, 'mutant) diff = ('repr, 'mutant) t

val to_yojson : ('repr, 'mutant) t -> Yojson.Safe.t

val to_lite : (full, 'mutant) t -> (lite, 'mutant) t

module Lite : sig
  type 'mutant t = (lite, 'mutant) diff

  module E : sig
    [%%versioned:
    module Stable : sig
      module V2 : sig
        type t = E : (lite, 'mutant) diff -> t
      end

      module V1 : sig
        type t
      end
    end]
  end
end

module Full : sig
  type 'mutant t = (full, 'mutant) diff

  module E : sig
    type t = E : (full, 'mutant) diff -> t [@@deriving to_yojson]

    val to_lite : t -> Lite.E.t
  end

  module With_mutant : sig
    type t = E : (full, 'mutant) diff * 'mutant -> t
  end
end
