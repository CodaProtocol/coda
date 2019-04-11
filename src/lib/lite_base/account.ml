open Core_kernel
open Module_version

module Balance = struct
  module V1_make = Nat.V1_64_make ()

  module Stable = struct
    module V1 = struct
      module T = struct
        type t = V1_make.Stable.V1.t
        [@@deriving bin_io, sexp, eq, compare, version]
      end

      include T
      include Registration.Make_latest_version (T)
    end

    module Latest = V1

    module Module_decl = struct
      let name = "balance_lite"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  include V1_make.Importable
end

module Nonce = struct
  module V1_make = Nat.V1_32_make ()

  module Stable = struct
    module V1 = struct
      module T = struct
        type t = V1_make.Stable.V1.t
        [@@deriving bin_io, sexp, eq, compare, version]
      end

      include T
      include Registration.Make_latest_version (T)
    end

    module Latest = V1

    module Module_decl = struct
      let name = "nonce_lite"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  include V1_make.Importable
end

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
        { public_key: Public_key.Compressed.Stable.V1.t
        ; balance: Balance.Stable.V1.t
        ; nonce: Nonce.Stable.V1.t
        ; receipt_chain_hash: Receipt.Chain_hash.t
        ; delegate: Public_key.Compressed.Stable.V1.t
        ; participated: bool }
      [@@deriving bin_io, sexp, eq, version {asserted}]
    end

    include T
  end

  module Latest = V1
end

type t = Stable.Latest.t [@@deriving sexp, eq]

let fold
    { Stable.Latest.public_key
    ; balance
    ; nonce
    ; receipt_chain_hash
    ; delegate
    ; participated } =
  let open Fold_lib.Fold in
  Public_key.Compressed.fold public_key
  +> Balance.fold balance +> Nonce.fold nonce
  +> Receipt.Chain_hash.fold receipt_chain_hash
  +> Public_key.Compressed.fold delegate
  +> Fold_lib.Fold.return (participated, false, false)
