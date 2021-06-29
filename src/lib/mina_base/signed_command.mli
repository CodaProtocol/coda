open Core_kernel
open Import
module Payload = Signed_command_payload

module Poly : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type ('payload, 'pk, 'signature) t =
        {payload: 'payload; signer: 'pk; signature: 'signature}
      [@@deriving sexp, hash, yojson, equal, compare]
    end
  end]
end

[%%versioned:
module Stable : sig
  [@@@no_toplevel_latest_type]

  module V1 : sig
    type t =
      ( Payload.Stable.V1.t
      , Public_key.Stable.V1.t
      , Signature.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving sexp, hash, yojson, version]

    val version_byte : char (* for base58_check *)

    include Comparable.S with type t := t

    include Hashable.S with type t := t

    val accounts_accessed :
      next_available_token:Token_id.t -> t -> Account_id.t list
  end
end]

include Signed_command_intf.S with type t = Stable.Latest.t
