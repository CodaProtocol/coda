open Core_kernel

(* This represents the effects of a transaction on the protocol state
   apart from effects on the ledger. *)

[%%versioned
module Stable = struct
  module V1 = struct
    type t = {accounts_created: int; next_available_token: Token_id.Stable.V1.t}

    let to_latest = Fn.id
  end
end]
