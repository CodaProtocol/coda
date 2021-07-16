open Core

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'command t =
        | Command of 'command
        | Fee_transfer of Fee_transfer.Stable.V1.t
        | Coinbase of Coinbase.Stable.V1.t
      [@@deriving sexp, compare, equal, hash, yojson]

      let to_latest = Fn.id

      let map t ~f =
        match t with
        | Command x ->
            Command (f x)
        | Fee_transfer x ->
            Fee_transfer x
        | Coinbase x ->
            Coinbase x
    end
  end]
end

module Valid = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t = User_command.Valid.Stable.V2.t Poly.Stable.V1.t
      [@@deriving sexp, compare, equal, hash, yojson]

      let to_latest = Fn.id
    end

    module V1 = struct
      type t = User_command.Valid.Stable.V1.t Poly.Stable.V1.t
      [@@deriving sexp, compare, equal, hash, yojson]

      let to_latest : t -> Latest.t =
        Poly.Stable.V1.map ~f:User_command.Valid.Stable.V1.to_latest
    end
  end]

  include Hashable.Make (Stable.Latest)
  include Comparable.Make (Stable.Latest)
end

[%%versioned
module Stable = struct
  module V2 = struct
    type t = User_command.Stable.V2.t Poly.Stable.V1.t
    [@@deriving sexp, compare, equal, hash, yojson]

    let to_latest = Fn.id
  end

  module V1 = struct
    type t = User_command.Stable.V1.t Poly.Stable.V1.t
    [@@deriving sexp, compare, equal, hash, yojson]

    let to_latest : t -> Latest.t =
      Poly.Stable.V1.map ~f:User_command.Stable.V1.to_latest
  end
end]

include Hashable.Make (Stable.Latest)
include Comparable.Make (Stable.Latest)

type 'command t_ = 'command Poly.t =
  | Command of 'command
  | Fee_transfer of Fee_transfer.t
  | Coinbase of Coinbase.t

let forget : Valid.t -> t = fun x -> (x :> t)

let fee_excess : t -> Fee_excess.t Or_error.t = function
  | Command (Signed_command t) ->
      Ok (Signed_command.fee_excess t)
  | Command (Parties _ps) ->
      failwith "TODO"
  | Fee_transfer t ->
      Fee_transfer.fee_excess t
  | Coinbase t ->
      Coinbase.fee_excess t

let supply_increase = function
  | Command _ | Fee_transfer _ ->
      Ok Currency.Amount.zero
  | Coinbase t ->
      Coinbase.supply_increase t

let public_keys : t -> _ = function
  | Command (Signed_command cmd) ->
      [ Signed_command.fee_payer_pk cmd
      ; Signed_command.source_pk cmd
      ; Signed_command.receiver_pk cmd
      ]
  | Command (Parties t) ->
      Parties.accounts_accessed t |> List.map ~f:Account_id.public_key
  | Fee_transfer ft ->
      Fee_transfer.receiver_pks ft
  | Coinbase cb ->
      Coinbase.accounts_accessed cb |> List.map ~f:Account_id.public_key

let accounts_accessed ~next_available_token : t -> _ = function
  | Command (Signed_command cmd) ->
      Signed_command.accounts_accessed ~next_available_token cmd
  | Command (Parties t) ->
      Parties.accounts_accessed t
  | Fee_transfer ft ->
      Fee_transfer.receivers ft
  | Coinbase cb ->
      Coinbase.accounts_accessed cb

let next_available_token (t : t) next_available_token =
  match t with
  | Command (Signed_command cmd) ->
      Signed_command.next_available_token cmd next_available_token
  | Command (Parties _t) ->
      failwith "TODO"
  | Fee_transfer _ ->
      next_available_token
  | Coinbase _ ->
      next_available_token

let fee_payer_pk (t : t) =
  match t with
  | Command (Signed_command cmd) ->
      Signed_command.fee_payer_pk cmd
  | Command (Parties t) ->
      Parties.fee_payer_pk t
  | Fee_transfer ft ->
      Fee_transfer.fee_payer_pk ft
  | Coinbase cb ->
      Coinbase.fee_payer_pk cb
