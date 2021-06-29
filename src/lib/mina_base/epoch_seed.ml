open Core_kernel

include Data_hash.Make_full_size (struct
  let version_byte = Base58_check.Version_bytes.epoch_seed

  let description = "Epoch Seed"
end)

(* Data hash versioned boilerplate below *)

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V1 = struct
    module T = struct
      type t = Snark_params.Tick.Field.t
      [@@deriving sexp, compare, hash, version {asserted}]
    end

    include T

    let to_latest = Core.Fn.id

    [%%define_from_scope
    to_yojson, of_yojson]

    include Comparable.Make (T)
    include Hashable.Make_binable (T)
  end
end]
