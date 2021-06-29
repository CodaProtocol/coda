open Core_kernel

[%%versioned:
module Stable : sig
  [@@@no_toplevel_latest_type]

  module V1 : sig
    type t [@@deriving sexp, compare, hash, equal]
  end
end]

type t = Stable.Latest.t [@@deriving sexp, compare, hash, yojson, equal]

val of_base58_check : string -> t Or_error.t

val of_base58_check_exn : string -> t

val to_base58_check : t -> string

val hash_command : User_command.t -> t

val hash_fee_transfer : Fee_transfer.Single.t -> t

val hash_coinbase : Coinbase.t -> t

include Comparable.S with type t := t

module User_command_with_valid_signature : sig
  type hash = t [@@deriving sexp, compare, hash, yojson]

  [%%versioned:
  module Stable : sig
    [@@@no_toplevel_latest_type]

    module V1 : sig
      type t =
        private
        (User_command.Valid.Stable.V1.t, hash) With_hash.Stable.V1.t
      [@@deriving sexp, compare, hash, to_yojson]
    end
  end]

  type t = Stable.Latest.t [@@deriving sexp, compare, to_yojson]

  val create : User_command.Valid.t -> t

  val data : t -> User_command.Valid.t

  val command : t -> User_command.t

  val hash : t -> hash

  val forget_check : t -> (User_command.t, hash) With_hash.t

  include Comparable.S with type t := t

  val make : User_command.Valid.t -> hash -> t
end

module User_command : sig
  type hash = t [@@deriving sexp, compare, hash, yojson]

  [%%versioned:
  module Stable : sig
    [@@@no_toplevel_latest_type]

    module V1 : sig
      type t = private (User_command.Stable.V1.t, hash) With_hash.Stable.V1.t
      [@@deriving sexp, compare, hash, to_yojson]
    end
  end]

  type t = Stable.Latest.t [@@deriving sexp, compare, to_yojson]

  val create : User_command.t -> t

  val data : t -> User_command.t

  val command : t -> User_command.t

  val hash : t -> hash

  include Comparable.S with type t := t

  val of_checked : User_command_with_valid_signature.t -> t
end
