open Core_kernel
open Tuple_lib
open Fold_lib

module type S = sig
  module Proof : sig
    type t [@@deriving bin_io, sexp]
  end

  module Local_state : sig
    type t
  end

  module Consensus_transition_data : sig
    type value [@@deriving bin_io, sexp]

    include Snark_params.Tick.Snarkable.S with type value := value

    val genesis : value
  end

  module Consensus_state : sig
    type value [@@deriving hash, eq, compare, bin_io, sexp]

    include Snark_params.Tick.Snarkable.S with type value := value

    val genesis : value

    val length_in_triples : int

    val var_to_triples :
         var
      -> ( Snark_params.Tick.Boolean.var Triple.t list
         , _ )
         Snark_params.Tick.Checked.t

    val fold : value -> bool Triple.t Fold.t
  end

  module Protocol_state :
    Nanobit_base.Protocol_state.S with module Consensus_state = Consensus_state

  module Snark_transition :
    Nanobit_base.Snark_transition.S
    with module Consensus_data = Consensus_transition_data
     and module Proof = Proof

  module Internal_transition :
    Nanobit_base.Internal_transition.S
    with module Snark_transition = Snark_transition

  module External_transition :
    Nanobit_base.External_transition.S
    with module Protocol_state = Protocol_state

  val genesis_protocol_state : Protocol_state.value

  (**
   * Generate a new protocol state and consensus specific transition data
   * for a new transition. Called from the proposer in order to generate
   * a new transition to propose to the network. Returns `None` if a new
   * transition cannot be generated.
   *)
  val generate_transition :
       previous_protocol_state:Protocol_state.value
    -> blockchain_state:Nanobit_base.Blockchain_state.value
    -> local_state:Local_state.t
    -> time:Int64.t
    -> transactions:Nanobit_base.Transaction.t list
    -> (Protocol_state.value * Consensus_transition_data.value) option

  (**
   * Create a checked boolean constraint for the validity of a transition.
   *)
  val is_transition_valid_checked :
       Snark_transition.var
    -> (Snark_params.Tick.Boolean.var, _) Snark_params.Tick.Checked.t

  (**
   * Create a constrained, checked var for the next consensus state of
   * a given consensus state and snark transition.
   *)
  val next_state_checked :
       Consensus_state.var
    -> Snark_transition.var
    -> (Consensus_state.var, _) Snark_params.Tick.Checked.t

  (**
   * Update the local state of a ledger builder controller tip given a
   * previous local state (if there is one), a previous consensus state,
   * a new consensus state, and the new ledger. The current local state may
   * not exist.
   *)
  val update_local_state :
       Local_state.t option
    -> previous_consensus_state:Consensus_state.value
    -> next_consensus_state:Consensus_state.value
    -> ledger:Nanobit_base.Ledger.t
    -> Local_state.t

  (**
   * Select between two ledger builder controller tips given the consensus
   * states for the two tips. Returns `\`Keep` if the first tip should be
   * kept, or `\`Take` if the second tip should be taken instead.
   *)
  val select : Consensus_state.value -> Consensus_state.value -> [`Keep | `Take]
end
