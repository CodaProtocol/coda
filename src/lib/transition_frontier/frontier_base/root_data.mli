open Mina_base
open Mina_transition

(* Historical root data is similar to Limited root data, except that it also
 * contains a recording of some extra computed staged ledger properties that
 * were available on a breadcrumb in the transition frontier when this was
 * created. *)
module Historical : sig
  [%%versioned:
  module Stable : sig
    module V2 : sig
      type t
    end

    module V1 : sig
      type t
    end
  end]

  val transition : t -> External_transition.Validated.t

  val scan_state : t -> Staged_ledger.Scan_state.t

  val pending_coinbase : t -> Pending_coinbase.t

  val staged_ledger_target_ledger_hash : t -> Ledger_hash.t

  val of_breadcrumb : Breadcrumb.t -> t
end

(* Limited root data is similar to Minimal root data, except that it contains
 * the full validated transition at a root instead of just a pointer to one and protocol states for the root scan state *)
module Limited : sig
  [%%versioned:
  module Stable : sig
    module V2 : sig
      type t [@@deriving to_yojson]
    end

    module V1 : sig
      type t

      val to_latest : t -> V2.t
    end
  end]

  val transition : t -> External_transition.Validated.t

  val hash : t -> State_hash.t

  val scan_state : t -> Staged_ledger.Scan_state.t

  val pending_coinbase : t -> Pending_coinbase.t

  val protocol_states :
    t -> (State_hash.t * Mina_state.Protocol_state.value) list

  val create :
       transition:External_transition.Validated.t
    -> scan_state:Staged_ledger.Scan_state.t
    -> pending_coinbase:Pending_coinbase.t
    -> protocol_states:(State_hash.t * Mina_state.Protocol_state.value) list
    -> t
end

(* Minimal root data contains the smallest amount of information about a root.
 * It contains a hash pointing to the root transition, and the auxilliary data
 * needed to reconstruct the staged ledger at that point (scan_state,
 * pending_coinbase).
 *)
module Minimal : sig
  [%%versioned:
  module Stable : sig
    module V2 : sig
      type t
    end

    module V1 : sig
      type t
    end
  end]

  val hash : t -> State_hash.t

  val scan_state : t -> Staged_ledger.Scan_state.t

  val pending_coinbase : t -> Pending_coinbase.t

  val of_limited : Limited.t -> t

  val upgrade :
       t
    -> transition:External_transition.Validated.t
    -> protocol_states:
         (Mina_base.State_hash.t * Mina_state.Protocol_state.Value.t) list
    -> Limited.t

  val create :
       hash:State_hash.t
    -> scan_state:Staged_ledger.Scan_state.t
    -> pending_coinbase:Pending_coinbase.t
    -> t
end

type t =
  { transition : External_transition.Validated.t
  ; staged_ledger : Staged_ledger.t
  ; protocol_states :
      (Mina_base.State_hash.t * Mina_state.Protocol_state.Value.t) list
  }

val minimize : t -> Minimal.t

val limit : t -> Limited.t
