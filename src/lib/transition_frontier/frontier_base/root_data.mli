open Coda_base
open Coda_transition

(* Limited root data is similar to Minimal root data, except that it contains
 * the full validated transition at a root instead of just a pointer to one *)
module Limited : sig
  module Stable : sig
    module V1 : sig
      type t =
        { transition: External_transition.Validated.Stable.V1.t
        ; scan_state: Staged_ledger.Scan_state.Stable.V1.t
        ; pending_coinbase: Pending_coinbase.Stable.V1.t }
      [@@deriving bin_io, version]
    end

    module Latest = V1
  end

  type t = Stable.Latest.t
end

(* Minimal root data contains the smallest amount of information about a root.
 * It contains a hash pointing to the root transition, and the auxilliary data
 * needed to reconstruct the staged ledger at that point (scan_state,
 * pending_coinbase).
 *)
module Minimal : sig
  module Stable : sig
    module V1 : sig
      type t =
        { hash: State_hash.Stable.V1.t
        ; scan_state: Staged_ledger.Scan_state.Stable.V1.t
        ; pending_coinbase: Pending_coinbase.Stable.V1.t }
      [@@deriving bin_io, version]
    end

    module Latest = V1
  end

  type t = Stable.Latest.t

  val of_limited : Limited.t -> t

  val upgrade : t -> External_transition.Validated.t -> Limited.t
end

type t =
  {transition: External_transition.Validated.t; staged_ledger: Staged_ledger.t}

val minimize : t -> Minimal.t

val limit : t -> Limited.t
