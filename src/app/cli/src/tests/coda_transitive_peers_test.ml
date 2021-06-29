open Core
open Async

let name = "coda-transitive-peers-test"

let runtime_config = Runtime_config.Test_configs.split_snarkless

let main () =
  let logger = Logger.create () in
  let%bind precomputed_values, _runtime_config =
    Genesis_ledger_helper.inputs_from_config_file ~logger ~proof_level:None
      (Lazy.force runtime_config)
    >>| Or_error.ok_exn
  in
  let consensus_constants = precomputed_values.consensus_constants in
  let%bind program_dir = Unix.getcwd () in
  let n = 3 in
  let block_production_interval =
    consensus_constants.block_window_duration_ms |> Block_time.Span.to_ms
    |> Int64.to_int_exn
  in
  let acceptable_delay =
    Time.Span.of_ms
      ( block_production_interval
        * (Unsigned.UInt32.to_int consensus_constants.delta + 1)
      |> Float.of_int )
  in
  let work_selection_method =
    Cli_lib.Arg_type.Work_selection_method.Sequence
  in
  Coda_processes.init () ;
  let trace_dir = Unix.getenv "CODA_TRACING" in
  let max_concurrent_connections = None in
  let%bind configs =
    Coda_processes.local_configs n ~program_dir ~block_production_interval
      ~acceptable_delay ~chain_id:name ~snark_worker_public_keys:None
      ~block_production_keys:(Fn.const None) ~work_selection_method ~trace_dir
      ~max_concurrent_connections
      ~runtime_config:precomputed_values.runtime_config
  in
  let%bind workers = Coda_processes.spawn_local_processes_exn configs in
  (*generating n+1 configs because the first three will have the same ports as the previous nodes*)
  let%bind new_node_net_config = Coda_processes.net_configs (n + 1) in
  let new_node_addrs_and_ports_list, _ = new_node_net_config in
  let expected_peers_addrs_keypairs =
    List.map configs ~f:(fun c ->
        (Node_addrs_and_ports.of_display c.addrs_and_ports, c.libp2p_keypair)
    )
  in
  let expected_peers_addr, expected_peers =
    List.fold ~init:([], []) expected_peers_addrs_keypairs
      ~f:(fun (peer_addrs, peers) (p, k) ->
        ( Node_addrs_and_ports.to_multiaddr_exn p :: peer_addrs
        , Network_peer.Peer.create p.external_ip ~libp2p_port:p.libp2p_port
            ~peer_id:(Mina_net2.Keypair.to_peer_id k)
          :: peers ) )
  in
  let addrs_and_ports, libp2p_keypair =
    let addr_and_ports, k = List.nth_exn new_node_addrs_and_ports_list n in
    (Node_addrs_and_ports.to_display addr_and_ports, k)
  in
  [%log debug]
    !"connecting to peers %{sexp: string list}\n"
    expected_peers_addr ;
  let config =
    Coda_process.local_config ~is_seed:true ~peers:expected_peers_addr
      ~addrs_and_ports ~acceptable_delay ~chain_id:name ~libp2p_keypair
      ~net_configs:new_node_net_config ~snark_worker_key:None
      ~block_production_key:None ~program_dir ~work_selection_method ~trace_dir
      ~offset:Time.Span.zero () ~max_concurrent_connections
      ~is_archive_rocksdb:false ~archive_process_location:None
      ~runtime_config:precomputed_values.runtime_config
  in
  let%bind worker = Coda_process.spawn_exn config in
  let%bind _ = after (Time.Span.of_sec 10.) in
  let%bind peers = Coda_process.peers_exn worker in
  [%log debug]
    !"got peers %{sexp: Network_peer.Peer.t list} expected: %{sexp: \
      Network_peer.Peer.t list}\n"
    peers expected_peers ;
  let module S = Network_peer.Peer.Set in
  assert (S.equal (S.of_list peers) (S.of_list expected_peers)) ;
  let%bind () = Coda_process.disconnect worker ~logger in
  Deferred.List.iter workers ~f:(Coda_process.disconnect ~logger)

let command =
  Command.async
    ~summary:"test that second-degree peers show up in the peer list"
    (Command.Param.return main)
