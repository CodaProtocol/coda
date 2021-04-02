module Vrf_output : sig
  module Truncated : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t [@@deriving sexp, eq, compare, hash, yojson]
      end
    end]

    val to_base58_check : t -> string

    val of_base58_check_exn : string -> t
  end
end

module Consensus_state : sig
  module Poly : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type ( 'length
             , 'vrf_output
             , 'amount
             , 'global_slot
             , 'global_slot_since_genesis
             , 'staking_epoch_data
             , 'next_epoch_data
             , 'bool
             , 'pk )
             t =
          { blockchain_length: 'length
          ; epoch_count: 'length
          ; min_window_density: 'length
          ; sub_window_densities: 'length list
          ; last_vrf_output: 'vrf_output
          ; total_currency: 'amount
          ; curr_global_slot: 'global_slot
          ; global_slot_since_genesis: 'global_slot_since_genesis
          ; staking_epoch_data: 'staking_epoch_data
          ; next_epoch_data: 'next_epoch_data
          ; has_ancestor_in_same_checkpoint_window: 'bool
          ; block_stake_winner: 'pk
          ; block_creator: 'pk
          ; coinbase_receiver: 'pk
          ; supercharge_coinbase: 'bool }
        [@@deriving sexp, eq, compare, hash, yojson, fields, hlist]
      end
    end]
  end

  module Value : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t =
          ( Mina_numbers.Length.Stable.V1.t
          , Vrf_output.Truncated.Stable.V1.t
          , Currency.Amount.Stable.V1.t
          , Global_slot.Stable.V1.t
          , Mina_numbers.Global_slot.Stable.V1.t
          , ( Mina_base.Epoch_ledger.Value.Stable.V1.t
            , Mina_base.Epoch_seed.Stable.V1.t
            , Mina_base.State_hash.Stable.V1.t
            , Mina_base.State_hash.Stable.V1.t
            , Mina_numbers.Length.Stable.V1.t )
            Mina_base.Epoch_data.Poly.Stable.V1.t
          , ( Mina_base.Epoch_ledger.Value.Stable.V1.t
            , Mina_base.Epoch_seed.Stable.V1.t
            , Mina_base.State_hash.Stable.V1.t
            , Mina_base.State_hash.Stable.V1.t
            , Mina_numbers.Length.Stable.V1.t )
            Mina_base.Epoch_data.Poly.Stable.V1.t
          , bool
          , Signature_lib.Public_key.Compressed.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving sexp, eq, compare, hash, yojson]
      end
    end]
  end
end

include
  Intf.S
  with type Data.Consensus_state.Value.Stable.V1.t =
              Consensus_state.Value.Stable.V1.t

(** Exported provides an easy way to publicly expose certain internal details
 *  of proof of stake which can be used when referring directly to the
 *  [Consensus.Proof_of_stake] consensus implementation rather than the default
 *  [Consensus] implementation
 *)
module Exported : sig
  module Global_slot = Global_slot

  module Block_data : sig
    include module type of Data.Block_data with type t = Data.Block_data.t
  end

  module Consensus_state : sig
    include
      module type of Data.Consensus_state
      with type Value.Stable.V1.t = Data.Consensus_state.Value.Stable.V1.t
       and type var = Data.Consensus_state.var

    val global_slot : Value.t -> Global_slot.t

    val total_currency : Value.t -> Currency.Amount.t

    val min_window_density : Value.t -> Mina_numbers.Length.t

    val staking_epoch_data : Value.t -> Mina_base.Epoch_data.Value.t

    val next_epoch_data : Value.t -> Mina_base.Epoch_data.Value.t

    (* unsafe modules for creating dummy states when doing vrf evaluations *)
    (* TODO: refactor code so that [Hooks.next_proposal] does not require a full [Consensus_state] *)
    module Unsafe : sig
      val dummy_advance :
           Value.t
        -> ?increase_epoch_count:bool
        -> new_global_slot:Global_slot.t
        -> Value.t
    end
  end
end
