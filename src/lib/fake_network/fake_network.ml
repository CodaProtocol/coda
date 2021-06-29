open Async
open Core
open Mina_base
open Gadt_lib
open Signature_lib
open Network_peer
module Gossip_net = Mina_networking.Gossip_net

(* There must be at least 2 peers to create a network *)
type 'n num_peers = 'n Peano.gt_1

(* TODO: make transition frontier a mutable option *)
type peer_state =
  { frontier: Transition_frontier.t
  ; consensus_local_state: Consensus.Data.Local_state.t }

type peer_network =
  {peer: Network_peer.Peer.t; state: peer_state; network: Mina_networking.t}

type nonrec 'n t =
  { fake_gossip_network: Gossip_net.Fake.network
  ; peer_networks: (peer_network, 'n) Vect.t }
  constraint 'n = _ num_peers

module Constants = struct
  let init_ip = Int32.of_int_exn 1

  let init_discovery_port = 1337
end

let setup (type n) ?(logger = Logger.null ())
    ?(trust_system = Trust_system.null ())
    ?(time_controller = Block_time.Controller.basic ~logger)
    ~(precomputed_values : Precomputed_values.t)
    (states : (peer_state, n num_peers) Vect.t) : n num_peers t =
  let _, peers =
    Vect.fold_map states
      ~init:(Constants.init_ip, Constants.init_discovery_port)
      ~f:(fun (ip, libp2p_port) _ ->
        (* each peer has a distinct IP address, so we lookup frontiers by IP *)
        let peer =
          Network_peer.Peer.create
            (Unix.Inet_addr.inet4_addr_of_int32 ip)
            ~libp2p_port
            ~peer_id:
              (Peer.Id.unsafe_of_string
                 (sprintf "fake peer at port %d" libp2p_port))
        in
        ((Int32.( + ) Int32.one ip, libp2p_port + 1), peer) )
  in
  let fake_gossip_network =
    Gossip_net.Fake.create_network (Vect.to_list peers)
  in
  let config peer consensus_local_state =
    let open Mina_networking.Config in
    { logger
    ; trust_system
    ; time_controller
    ; consensus_local_state
    ; is_seed= Vect.is_empty peers
    ; genesis_ledger_hash=
        Ledger.merkle_root
          (Lazy.force (Precomputed_values.genesis_ledger precomputed_values))
    ; constraint_constants= precomputed_values.constraint_constants
    ; creatable_gossip_net=
        Gossip_net.Any.Creatable
          ( (module Gossip_net.Fake)
          , Gossip_net.Fake.create_instance fake_gossip_network peer )
    ; log_gossip_heard=
        {snark_pool_diff= true; transaction_pool_diff= true; new_state= true}
    }
  in
  let peer_networks =
    Vect.map2 peers states ~f:(fun peer state ->
        let frontier = state.frontier in
        let network =
          Thread_safe.block_on_async_exn (fun () ->
              (* TODO: merge implementations with mina_lib *)
              Mina_networking.create
                (config peer state.consensus_local_state)
                ~get_staged_ledger_aux_and_pending_coinbases_at_hash:
                  (fun query_env ->
                  let input = Envelope.Incoming.data query_env in
                  Deferred.return
                    (let open Option.Let_syntax in
                    let%map ( scan_state
                            , expected_merkle_root
                            , pending_coinbases
                            , protocol_states ) =
                      Sync_handler
                      .get_staged_ledger_aux_and_pending_coinbases_at_hash
                        ~frontier input
                    in
                    let staged_ledger_hash =
                      Staged_ledger_hash.of_aux_ledger_and_coinbase_hash
                        (Staged_ledger.Scan_state.hash scan_state)
                        expected_merkle_root pending_coinbases
                    in
                    [%log debug]
                      ~metadata:
                        [ ( "staged_ledger_hash"
                          , Staged_ledger_hash.to_yojson staged_ledger_hash )
                        ]
                      "sending scan state and pending coinbase" ;
                    ( scan_state
                    , expected_merkle_root
                    , pending_coinbases
                    , protocol_states )) )
                ~get_some_initial_peers:(fun _ -> Deferred.return [])
                ~answer_sync_ledger_query:(fun query_env ->
                  let ledger_hash, _ = Envelope.Incoming.data query_env in
                  Sync_handler.answer_query ~frontier ledger_hash
                    (Envelope.Incoming.map ~f:Tuple2.get2 query_env)
                    ~logger:(Logger.create ())
                    ~trust_system:(Trust_system.null ())
                  |> Deferred.map
                     (* begin error string prefix so we can pattern-match *)
                       ~f:
                         (Result.of_option
                            ~error:
                              (Error.createf
                                 !"%s for ledger_hash: %{sexp:Ledger_hash.t}"
                                 Mina_networking.refused_answer_query_string
                                 ledger_hash)) )
                ~get_ancestry:(fun query_env ->
                  Deferred.return
                    (Sync_handler.Root.prove
                       ~consensus_constants:
                         precomputed_values.consensus_constants ~logger
                       ~frontier
                       (Envelope.Incoming.data query_env)) )
                ~get_best_tip:(fun _ -> failwith "Get_best_tip unimplemented")
                ~get_node_status:(fun _ ->
                  failwith "Get_node_status unimplemented" )
                ~get_transition_knowledge:(fun _query ->
                  Deferred.return (Sync_handler.best_tip_path ~frontier) )
                ~get_transition_chain_proof:(fun query_env ->
                  Deferred.return
                    (Transition_chain_prover.prove ~frontier
                       (Envelope.Incoming.data query_env)) )
                ~get_transition_chain:(fun query_env ->
                  Deferred.return
                    (Sync_handler.get_transition_chain ~frontier
                       (Envelope.Incoming.data query_env)) ) )
        in
        {peer; state; network} )
  in
  {fake_gossip_network; peer_networks}

module Generator = struct
  open Quickcheck
  open Generator.Let_syntax

  type peer_config =
       precomputed_values:Precomputed_values.t
    -> verifier:Verifier.t
    -> max_frontier_length:int
    -> peer_state Generator.t

  let fresh_peer ~precomputed_values ~verifier ~max_frontier_length =
    let epoch_ledger_location =
      Filename.temp_dir_name ^/ "epoch_ledger"
      ^ (Uuid_unix.create () |> Uuid.to_string)
    in
    let genesis_ledger =
      Precomputed_values.genesis_ledger precomputed_values
    in
    let consensus_local_state =
      Consensus.Data.Local_state.create Public_key.Compressed.Set.empty
        ~genesis_ledger
        ~genesis_epoch_data:precomputed_values.genesis_epoch_data
        ~epoch_ledger_location
        ~ledger_depth:precomputed_values.constraint_constants.ledger_depth
        ~genesis_state_hash:
          (With_hash.hash precomputed_values.protocol_state_with_hash)
    in
    let%map frontier =
      Transition_frontier.For_tests.gen ~precomputed_values ~verifier
        ~consensus_local_state ~max_length:max_frontier_length ~size:0 ()
    in
    {frontier; consensus_local_state}

  let peer_with_branch ~frontier_branch_size ~precomputed_values ~verifier
      ~max_frontier_length =
    let epoch_ledger_location =
      Filename.temp_dir_name ^/ "epoch_ledger"
      ^ (Uuid_unix.create () |> Uuid.to_string)
    in
    let genesis_ledger =
      Precomputed_values.genesis_ledger precomputed_values
    in
    let consensus_local_state =
      Consensus.Data.Local_state.create Public_key.Compressed.Set.empty
        ~genesis_ledger
        ~genesis_epoch_data:precomputed_values.genesis_epoch_data
        ~epoch_ledger_location
        ~ledger_depth:precomputed_values.constraint_constants.ledger_depth
        ~genesis_state_hash:
          (With_hash.hash precomputed_values.protocol_state_with_hash)
    in
    let%map frontier, branch =
      Transition_frontier.For_tests.gen_with_branch ~precomputed_values
        ~verifier ~max_length:max_frontier_length ~frontier_size:0
        ~branch_size:frontier_branch_size ~consensus_local_state ()
    in
    Async.Thread_safe.block_on_async_exn (fun () ->
        Deferred.List.iter branch
          ~f:(Transition_frontier.add_breadcrumb_exn frontier) ) ;
    {frontier; consensus_local_state}

  let gen ~precomputed_values ~verifier ~max_frontier_length configs =
    let open Quickcheck.Generator.Let_syntax in
    let%map states =
      Vect.Quickcheck_generator.map configs ~f:(fun config ->
          config ~precomputed_values ~verifier ~max_frontier_length )
    in
    setup ~precomputed_values states
end
