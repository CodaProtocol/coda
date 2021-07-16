open Core_kernel
open Mina_state

module type S = sig
  type t [@@deriving sexp, to_yojson]

  module Stable : sig
    module V1 : sig
      type t [@@deriving sexp, to_yojson, bin_io]
    end

    module Latest = V1
  end
  with type V1.t = t

  val create :
       snark_transition:Snark_transition.Value.t
    -> ledger_proof:Ledger_proof.t option
    -> prover_state:Consensus.Data.Prover_state.t
    -> staged_ledger_diff:Staged_ledger_diff.t
    -> t

  val snark_transition : t -> Snark_transition.Value.t

  val ledger_proof : t -> Ledger_proof.t option

  val prover_state : t -> Consensus.Data.Prover_state.t

  val staged_ledger_diff : t -> Staged_ledger_diff.t
end

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V2 = struct
    type t =
      { snark_transition : Snark_transition.Value.Stable.V2.t
      ; ledger_proof : Ledger_proof.Stable.V2.t option
      ; prover_state : Consensus.Data.Prover_state.Stable.V1.t
      ; staged_ledger_diff : Staged_ledger_diff.Stable.V2.t
      }

    let to_latest = Fn.id
  end

  module V1 = struct
    type t =
      { snark_transition : Snark_transition.Value.Stable.V1.t
      ; ledger_proof : Ledger_proof.Stable.V1.t option
      ; prover_state : Consensus.Data.Prover_state.Stable.V1.t
      ; staged_ledger_diff : Staged_ledger_diff.Stable.V1.t
      }

    let to_latest (t : t) : V2.t =
      { snark_transition =
          Snark_transition.Value.Stable.V1.to_latest t.snark_transition
      ; ledger_proof =
          Option.map ~f:Ledger_proof.Stable.V1.to_latest t.ledger_proof
      ; prover_state = t.prover_state
      ; staged_ledger_diff =
          Staged_ledger_diff.Stable.V1.to_latest t.staged_ledger_diff
      }
  end
end]

(* bin_io, version omitted *)
type t = Stable.Latest.t =
  { snark_transition : Snark_transition.Value.t
  ; ledger_proof : Ledger_proof.t option
  ; prover_state : Consensus.Data.Prover_state.t
  ; staged_ledger_diff : Staged_ledger_diff.t
  }
[@@deriving sexp, fields, to_yojson]

let create ~snark_transition ~ledger_proof ~prover_state ~staged_ledger_diff =
  { Stable.Latest.snark_transition
  ; ledger_proof
  ; staged_ledger_diff
  ; prover_state
  }
