open Mina_base

[%%versioned:
module Stable : sig
  [@@@no_toplevel_latest_type]

  module V1 : sig
    type t = { state_hash : State_hash.t }
  end
end]

type t = Stable.Latest.t [@@deriving yojson]
