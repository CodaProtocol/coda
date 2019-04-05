module State = Blockchain_state
open Core_kernel
open Coda_base
open Module_version

module type S = sig
  type t = {state: Consensus.Protocol_state.Value.t; proof: Proof.Stable.V1.t}
  [@@deriving bin_io, fields]

  val create :
    state:Consensus.Protocol_state.Value.t -> proof:Proof.Stable.V1.t -> t
end

module Protocol_state = Consensus.Protocol_state

module Stable = struct
  module V1 = struct
    module T = struct
      (* TODO : use version ppx *)
      let version = 1

      let __versioned__ = true

      type t =
        {state: Protocol_state.Value.Stable.V1.t; proof: Proof.Stable.V1.t}
      [@@deriving bin_io, fields]
    end

    include T
    include Registration.Make_latest_version (T)
  end

  module Latest = V1

  module Module_decl = struct
    let name = "blockchain"

    type latest = Latest.t
  end

  module Registrar = Registration.Make (Module_decl)
  module Registered_V1 = Registrar.Register (V1)
end

include Stable.Latest

let create ~state ~proof = {state; proof}
