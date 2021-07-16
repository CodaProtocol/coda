open Core_kernel
open Mina_base

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ( 'blockchain_state
           , 'consensus_transition
           , 'pending_coinbase_update )
           t =
        { blockchain_state : 'blockchain_state
        ; consensus_transition : 'consensus_transition
        ; pending_coinbase_update : 'pending_coinbase_update
        }
      [@@deriving to_yojson, sexp, fields, hlist]
    end
  end]
end

module Value = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t =
        ( Blockchain_state.Value.Stable.V2.t
        , Consensus.Data.Consensus_transition.Value.Stable.V1.t
        , Pending_coinbase.Update.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end

    module V1 = struct
      type t =
        ( Blockchain_state.Value.Stable.V1.t
        , Consensus.Data.Consensus_transition.Value.Stable.V1.t
        , Pending_coinbase.Update.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, to_yojson]

      let to_latest (t : t) : V2.t =
        { t with
          blockchain_state =
            Blockchain_state.Value.Stable.V1.to_latest t.blockchain_state
        }
    end
  end]
end

[%%define_locally
Poly.
  ( blockchain_state
  , consensus_transition
  , pending_coinbase_update
  , to_hlist
  , of_hlist )]

type value = Value.t

type var =
  ( Blockchain_state.var
  , Consensus.Data.Consensus_transition.var
  , Pending_coinbase.Update.var )
  Poly.t

let create_value ~blockchain_state ~consensus_transition
    ~pending_coinbase_update () : Value.t =
  { blockchain_state; consensus_transition; pending_coinbase_update }

let genesis ~constraint_constants ~consensus_constants ~genesis_ledger : value =
  let genesis_ledger = Lazy.force genesis_ledger in
  { Poly.blockchain_state =
      Blockchain_state.genesis ~constraint_constants ~consensus_constants
        ~genesis_ledger_hash:(Ledger.merkle_root genesis_ledger)
        ~snarked_next_available_token:
          (Ledger.next_available_token genesis_ledger)
  ; consensus_transition = Consensus.Data.Consensus_transition.genesis
  ; pending_coinbase_update = Pending_coinbase.Update.genesis
  }

let typ =
  let open Snark_params.Tick.Typ in
  of_hlistable ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
    ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
    [ Blockchain_state.typ
    ; Consensus.Data.Consensus_transition.typ
    ; Pending_coinbase.Update.typ
    ]
