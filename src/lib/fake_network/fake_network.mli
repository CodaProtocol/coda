open Core
open Gadt_lib

(* There must be at least 2 peers to create a network *)
type 'n num_peers = 'n Peano.gt_1

type peer_state =
  { frontier: Transition_frontier.t
  ; consensus_local_state: Consensus.Data.Local_state.t }

type peer_network =
  {peer: Network_peer.Peer.t; state: peer_state; network: Mina_networking.t}

type nonrec 'n t =
  { fake_gossip_network: Mina_networking.Gossip_net.Fake.network
  ; peer_networks: (peer_network, 'n) Vect.t }
  constraint 'n = _ num_peers

val setup :
     ?logger:Logger.t
  -> ?trust_system:Trust_system.t
  -> ?time_controller:Block_time.Controller.t
  -> precomputed_values:Precomputed_values.t
  -> (peer_state, 'n num_peers) Vect.t
  -> 'n num_peers t

module Generator : sig
  open Quickcheck

  type peer_config =
       precomputed_values:Precomputed_values.t
    -> verifier:Verifier.t
    -> max_frontier_length:int
    -> peer_state Generator.t

  val fresh_peer : peer_config

  val peer_with_branch : frontier_branch_size:int -> peer_config

  val gen :
       precomputed_values:Precomputed_values.t
    -> verifier:Verifier.t
    -> max_frontier_length:int
    -> (peer_config, 'n num_peers) Vect.t
    -> 'n num_peers t Generator.t
end
