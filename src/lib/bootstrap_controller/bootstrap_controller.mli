open Async_kernel
open Mina_transition
open Pipe_lib
open Network_peer

type Structured_log_events.t += Bootstrap_complete
  [@@deriving register_event]

(** The entry point function for bootstrap controller. When bootstrap finished
    it would return a transition frontier with the root breadcrumb and a list
    of transitions collected during bootstrap. 
    
    Bootstrap controller would do the following steps to contrust the
    transition frontier:
    1. Download the root snarked_ledger.
    2. Download the scan state and pending coinbases. 
    3. Construct the staged ledger from the snarked ledger, scan state and
       pending coinbases.
    4. Synchronize the consensus local state if necessary.
    5. Close the old frontier and reload a new one from disk.
 *)
val run :
     logger:Logger.t
  -> trust_system:Trust_system.t
  -> verifier:Verifier.t
  -> network:Mina_networking.t
  -> consensus_local_state:Consensus.Data.Local_state.t
  -> transition_reader:External_transition.Initial_validated.t
                       Envelope.Incoming.t
                       Strict_pipe.Reader.t
  -> best_seen_transition:External_transition.Initial_validated.t
                          Envelope.Incoming.t
                          option
  -> persistent_root:Transition_frontier.Persistent_root.t
  -> persistent_frontier:Transition_frontier.Persistent_frontier.t
  -> initial_root_transition:External_transition.Validated.t
  -> precomputed_values:Precomputed_values.t
  -> catchup_mode:[`Normal | `Super]
  -> ( Transition_frontier.t
     * External_transition.Initial_validated.t Envelope.Incoming.t list )
     Deferred.t
