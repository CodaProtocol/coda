open Core_kernel
open Mina_base

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V1 = struct
    type t = { state_hash : State_hash.Stable.V1.t }

    let to_latest = Fn.id
  end
end]

type t = Stable.Latest.t = { state_hash : State_hash.t } [@@deriving yojson]
