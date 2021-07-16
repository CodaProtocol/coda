[%%import "/src/config.mlh"]

open Core_kernel
open Mina_base

[%%ifdef consensus_mechanism]

open Snark_params.Tick

[%%endif]

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('state_hash, 'body) t =
        { previous_state_hash : 'state_hash; body : 'body }
      [@@deriving equal, ord, hash, sexp, yojson, hlist]
    end
  end]
end

let hash_abstract ~hash_body
    ({ previous_state_hash; body } : (State_hash.t, _) Poly.t) =
  let body : State_body_hash.t = hash_body body in
  Random_oracle.hash ~init:Hash_prefix.protocol_state
    [| (previous_state_hash :> Field.t); (body :> Field.t) |]
  |> State_hash.of_hash

module Body = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('state_hash, 'blockchain_state, 'consensus_state, 'constants) t =
          { genesis_state_hash : 'state_hash
          ; blockchain_state : 'blockchain_state
          ; consensus_state : 'consensus_state
          ; constants : 'constants
          }
        [@@deriving sexp, equal, compare, yojson, hash, version, hlist]
      end
    end]
  end

  module Value = struct
    [%%versioned
    module Stable = struct
      module V2 = struct
        type t =
          ( State_hash.Stable.V1.t
          , Blockchain_state.Value.Stable.V2.t
          , Consensus.Data.Consensus_state.Value.Stable.V1.t
          , Protocol_constants_checked.Value.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving equal, ord, bin_io, hash, sexp, yojson, version]

        let to_latest = Fn.id
      end

      module V1 = struct
        type t =
          ( State_hash.Stable.V1.t
          , Blockchain_state.Value.Stable.V1.t
          , Consensus.Data.Consensus_state.Value.Stable.V1.t
          , Protocol_constants_checked.Value.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving equal, ord, bin_io, hash, sexp, yojson, version]

        let to_latest (t : t) : V2.t =
          { t with
            blockchain_state =
              Blockchain_state.Value.Stable.V1.to_latest t.blockchain_state
          }
      end
    end]
  end

  type ('state_hash, 'blockchain_state, 'consensus_state, 'constants) t =
    ('state_hash, 'blockchain_state, 'consensus_state, 'constants) Poly.t

  type value = Value.t [@@deriving sexp, yojson]

  [%%ifdef consensus_mechanism]

  type var =
    ( State_hash.var
    , Blockchain_state.var
    , Consensus.Data.Consensus_state.var
    , Protocol_constants_checked.var )
    Poly.t

  let data_spec ~constraint_constants =
    Data_spec.
      [ State_hash.typ
      ; Blockchain_state.typ
      ; Consensus.Data.Consensus_state.typ ~constraint_constants
      ; Protocol_constants_checked.typ
      ]

  let typ ~constraint_constants =
    Typ.of_hlistable
      (data_spec ~constraint_constants)
      ~var_to_hlist:Poly.to_hlist ~var_of_hlist:Poly.of_hlist
      ~value_to_hlist:Poly.to_hlist ~value_of_hlist:Poly.of_hlist

  let to_input
      { Poly.genesis_state_hash : State_hash.t
      ; blockchain_state
      ; consensus_state
      ; constants
      } =
    Random_oracle.Input.(
      append
        (Blockchain_state.to_input blockchain_state)
        (Consensus.Data.Consensus_state.to_input consensus_state)
      |> append (field (genesis_state_hash :> Field.t))
      |> append (Protocol_constants_checked.to_input constants))

  let var_to_input
      { Poly.genesis_state_hash; blockchain_state; consensus_state; constants }
      =
    let%bind blockchain_state =
      Blockchain_state.var_to_input blockchain_state
    in
    let%bind constants = Protocol_constants_checked.var_to_input constants in
    let%map consensus_state =
      Consensus.Data.Consensus_state.var_to_input consensus_state
    in
    Random_oracle.Input.(
      append blockchain_state consensus_state
      |> append (field (State_hash.var_to_hash_packed genesis_state_hash))
      |> append constants)

  let hash_checked (t : var) =
    let%bind input = var_to_input t in
    make_checked (fun () ->
        Random_oracle.Checked.(
          hash ~init:Hash_prefix.protocol_state_body (pack_input input)
          |> State_body_hash.var_of_hash_packed))

  let consensus_state { Poly.consensus_state; _ } = consensus_state

  let view_checked (t : var) : Snapp_predicate.Protocol_state.View.Checked.t =
    let module C = Consensus.Proof_of_stake.Exported.Consensus_state in
    let cs : Consensus.Data.Consensus_state.var = t.consensus_state in
    { snarked_ledger_hash = t.blockchain_state.registers.ledger
    ; snarked_next_available_token =
        t.blockchain_state.registers.next_available_token
    ; timestamp = t.blockchain_state.timestamp
    ; blockchain_length = C.blockchain_length_var cs
    ; min_window_density = C.min_window_density_var cs
    ; last_vrf_output = ()
    ; total_currency = C.total_currency_var cs
    ; curr_global_slot = C.curr_global_slot_var cs
    ; global_slot_since_genesis = C.global_slot_since_genesis_var cs
    ; staking_epoch_data = C.staking_epoch_data_var cs
    ; next_epoch_data = C.next_epoch_data_var cs
    }

  [%%endif]

  let hash s =
    Random_oracle.hash ~init:Hash_prefix.protocol_state_body
      (Random_oracle.pack_input (to_input s))
    |> State_body_hash.of_hash

  let view (t : Value.t) : Snapp_predicate.Protocol_state.View.t =
    let module C = Consensus.Proof_of_stake.Exported.Consensus_state in
    let cs = t.consensus_state in
    { snarked_ledger_hash = t.blockchain_state.registers.ledger
    ; snarked_next_available_token =
        t.blockchain_state.registers.next_available_token
    ; timestamp = t.blockchain_state.timestamp
    ; blockchain_length = C.blockchain_length cs
    ; min_window_density = C.min_window_density cs
    ; last_vrf_output = ()
    ; total_currency = C.total_currency cs
    ; curr_global_slot = C.curr_global_slot cs
    ; global_slot_since_genesis = C.global_slot_since_genesis cs
    ; staking_epoch_data = C.staking_epoch_data cs
    ; next_epoch_data = C.next_epoch_data cs
    }
end

module Value = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t = (State_hash.Stable.V1.t, Body.Value.Stable.V2.t) Poly.Stable.V1.t
      [@@deriving sexp, hash, compare, equal, yojson]

      let to_latest = Fn.id
    end

    module V1 = struct
      type t = (State_hash.Stable.V1.t, Body.Value.Stable.V1.t) Poly.Stable.V1.t
      [@@deriving sexp, hash, compare, equal, yojson]

      let to_latest (t : t) : V2.t =
        { t with body = Body.Value.Stable.V1.to_latest t.body }
    end
  end]

  include Hashable.Make (Stable.Latest)
end

type value = Value.t [@@deriving sexp, yojson]

[%%ifdef consensus_mechanism]

type var = (State_hash.var, Body.var) Poly.t

[%%endif]

module Proof = Proof
module Hash = State_hash

let create ~previous_state_hash ~body =
  { Poly.Stable.Latest.previous_state_hash; body }

let create' ~previous_state_hash ~genesis_state_hash ~blockchain_state
    ~consensus_state ~constants =
  { Poly.Stable.Latest.previous_state_hash
  ; body =
      { Body.Poly.genesis_state_hash
      ; blockchain_state
      ; consensus_state
      ; constants
      }
  }

let create_value = create'

let body { Poly.Stable.Latest.body; _ } = body

let previous_state_hash { Poly.Stable.Latest.previous_state_hash; _ } =
  previous_state_hash

let blockchain_state
    { Poly.Stable.Latest.body = { Body.Poly.blockchain_state; _ }; _ } =
  blockchain_state

let consensus_state
    { Poly.Stable.Latest.body = { Body.Poly.consensus_state; _ }; _ } =
  consensus_state

let constants { Poly.Stable.Latest.body = { Body.Poly.constants; _ }; _ } =
  constants

[%%ifdef consensus_mechanism]

let create_var = create'

let data_spec ~constraint_constants =
  Data_spec.[ State_hash.typ; Body.typ ~constraint_constants ]

let typ ~constraint_constants =
  Typ.of_hlistable
    (data_spec ~constraint_constants)
    ~var_to_hlist:Poly.to_hlist ~var_of_hlist:Poly.of_hlist
    ~value_to_hlist:Poly.to_hlist ~value_of_hlist:Poly.of_hlist

let hash_checked ({ previous_state_hash; body } : var) =
  let%bind body = Body.hash_checked body in
  let%map hash =
    make_checked (fun () ->
        Random_oracle.Checked.hash ~init:Hash_prefix.protocol_state
          [| Hash.var_to_hash_packed previous_state_hash
           ; State_body_hash.var_to_hash_packed body
          |]
        |> State_hash.var_of_hash_packed)
  in
  (hash, body)

let genesis_state_hash_checked ~state_hash state =
  let%bind is_genesis =
    (*if state is in global_slot = 0 then this is the genesis state*)
    Consensus.Data.Consensus_state.is_genesis_state_var (consensus_state state)
  in
  (*get the genesis state hash from this state unless the state itself is the
    genesis state*)
  State_hash.if_ is_genesis ~then_:state_hash
    ~else_:state.body.genesis_state_hash

[%%endif]

let hash = hash_abstract ~hash_body:Body.hash

let hash_with_body t ~body_hash =
  hash_abstract ~hash_body:Fn.id
    { Poly.previous_state_hash = t.Poly.previous_state_hash; body = body_hash }

let genesis_state_hash ?(state_hash = None) state =
  (*If this is the genesis state then simply return its hash
    otherwise return its the genesis_state_hash*)
  if Consensus.Data.Consensus_state.is_genesis_state (consensus_state state)
  then match state_hash with None -> hash state | Some hash -> hash
  else state.body.genesis_state_hash

[%%if call_logger]

let hash s =
  Mina_debug.Call_logger.record_call "Protocol_state.hash" ;
  hash s

[%%endif]

let negative_one ~genesis_ledger ~genesis_epoch_data ~constraint_constants
    ~consensus_constants =
  { Poly.Stable.Latest.previous_state_hash =
      State_hash.of_hash Outside_hash_image.t
  ; body =
      { Body.Poly.blockchain_state =
          Blockchain_state.negative_one ~constraint_constants
            ~consensus_constants
            ~genesis_ledger_hash:
              (Mina_base.Ledger.merkle_root (Lazy.force genesis_ledger))
            ~snarked_next_available_token:
              (Mina_base.Ledger.next_available_token
                 (Lazy.force genesis_ledger))
      ; genesis_state_hash = State_hash.of_hash Outside_hash_image.t
      ; consensus_state =
          Consensus.Data.Consensus_state.negative_one ~genesis_ledger
            ~genesis_epoch_data ~constants:consensus_constants
            ~constraint_constants
      ; constants =
          Consensus.Constants.to_protocol_constants consensus_constants
      }
  }
