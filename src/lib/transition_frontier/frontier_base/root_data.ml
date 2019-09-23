open Core_kernel
open Coda_base
open Coda_transition
open Module_version

module Limited = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type t =
          { transition: External_transition.Validated.Stable.V1.t
          ; scan_state: Staged_ledger.Scan_state.Stable.V1.t
          ; pending_coinbase: Pending_coinbase.Stable.V1.t }
        [@@deriving bin_io, version]
      end

      include T
      include Registration.Make_latest_version (T)
    end

    module Latest = V1

    module Module_decl = struct
      let name = "transition_frontier_limited_root_data"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  include Stable.Latest
end

module Minimal = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type t =
          { hash: State_hash.Stable.V1.t
          ; scan_state: Staged_ledger.Scan_state.Stable.V1.t
          ; pending_coinbase: Pending_coinbase.Stable.V1.t }
        [@@deriving bin_io, version]
      end

      include T
      include Registration.Make_latest_version (T)
    end

    module Latest = V1

    module Module_decl = struct
      let name = "transition_frontier_minimal_root_data"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  include Stable.Latest

  let of_limited {Limited.Stable.V1.transition; scan_state; pending_coinbase} =
    let hash = External_transition.Validated.state_hash transition in
    {hash; scan_state; pending_coinbase}

  let upgrade {hash; scan_state; pending_coinbase} transition =
    assert (
      State_hash.equal
        (External_transition.Validated.state_hash transition)
        hash ) ;
    {Limited.Stable.V1.transition; scan_state; pending_coinbase}
end

module Transition = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type t =
          {new_root: Minimal.Stable.V1.t; garbage: State_hash.Stable.V1.t list}
        [@@deriving bin_io, version]
      end

      include T
      include Registration.Make_latest_version (T)
    end

    module Latest = V1

    module Module_decl = struct
      let name = "transition_frontier_root_transition"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  include Stable.Latest
end

type t =
  {transition: External_transition.Validated.t; staged_ledger: Staged_ledger.t}

let minimize {transition; staged_ledger} =
  let open Minimal.Stable.Latest in
  { hash= External_transition.Validated.state_hash transition
  ; scan_state= Staged_ledger.scan_state staged_ledger
  ; pending_coinbase= Staged_ledger.pending_coinbase_collection staged_ledger
  }

let limit {transition; staged_ledger} =
  let open Limited.Stable.Latest in
  { transition
  ; scan_state= Staged_ledger.scan_state staged_ledger
  ; pending_coinbase= Staged_ledger.pending_coinbase_collection staged_ledger
  }
