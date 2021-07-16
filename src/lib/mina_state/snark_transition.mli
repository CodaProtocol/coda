open Core_kernel
open Mina_base

module Poly : sig
  type ('blockchain_state, 'consensus_transition, 'pending_coinbase_update) t =
    { blockchain_state : 'blockchain_state
    ; consensus_transition : 'consensus_transition
    ; pending_coinbase_update : 'pending_coinbase_update
    }
  [@@deriving sexp, fields]

  module Stable : sig
    module V1 : sig
      type ( 'blockchain_state
           , 'consensus_transition
           , 'pending_coinbase_update )
           t
      [@@deriving bin_io, sexp, version]
    end

    module Latest : module type of V1
  end
  with type ( 'blockchain_state
            , 'consensus_transition
            , 'pending_coinbase_update )
            V1.t =
        ('blockchain_state, 'consensus_transition, 'pending_coinbase_update) t
end

module Value : sig
  [%%versioned:
  module Stable : sig
    module V2 : sig
      type t =
        ( Blockchain_state.Value.Stable.V2.t
        , Consensus.Data.Consensus_transition.Value.Stable.V1.t
        , Pending_coinbase.Update.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, to_yojson]
    end

    module V1 : sig
      type t =
        ( Blockchain_state.Value.Stable.V1.t
        , Consensus.Data.Consensus_transition.Value.Stable.V1.t
        , Pending_coinbase.Update.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, to_yojson]

      val to_latest : t -> V2.t
    end
  end]
end

type value = Value.t

type var =
  ( Blockchain_state.var
  , Consensus.Data.Consensus_transition.var
  , Pending_coinbase.Update.var )
  Poly.t

include
  Snark_params.Tick.Snarkable.S with type value := Value.t and type var := var

val create_value :
     blockchain_state:Blockchain_state.Value.t
  -> consensus_transition:Consensus.Data.Consensus_transition.Value.Stable.V1.t
  -> pending_coinbase_update:Pending_coinbase.Update.t
  -> unit
  -> Value.t

val genesis :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> consensus_constants:Consensus.Constants.t
  -> genesis_ledger:Ledger.t Lazy.t
  -> Value.t

val consensus_transition :
  (_, 'consensus_transition, _) Poly.t -> 'consensus_transition

val pending_coinbase_update :
  (_, _, 'pending_coinbase_action) Poly.t -> 'pending_coinbase_action

val blockchain_state : ('blockchain_state, _, _) Poly.t -> 'blockchain_state
