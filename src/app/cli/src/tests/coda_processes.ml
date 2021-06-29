[%%import
"/src/config.mlh"]

open Core
open Async

let init () = Parallel.init_master ()

type ports = {communication_port: int; discovery_port: int; libp2p_port: int}

let net_configs n =
  File_system.with_temp_dir "coda-processes-generate-keys" ~f:(fun tmpd ->
      let%bind net =
        Mina_net2.create
          ~pids:(Child_processes.Termination.create_pid_table ())
          ~logger:(Logger.create ()) ~conf_dir:tmpd
          ~all_peers_seen_metric:false ~on_unexpected_termination:(fun () ->
            raise Child_processes.Child_died )
      in
      let net = Or_error.ok_exn net in
      let ips =
        List.init n ~f:(fun i ->
            Unix.Inet_addr.of_string @@ sprintf "127.0.0.1%i" i )
      in
      let%bind addrs_and_ports_list =
        Deferred.List.mapi ips ~f:(fun i ip ->
            let%map key = Mina_net2.Keypair.random net in
            let base = 23000 + (i * 2) in
            let libp2p_port = base in
            let client_port = base + 1 in
            ( { Node_addrs_and_ports.external_ip= ip
              ; bind_ip= ip
              ; peer=
                  Some
                    (Network_peer.Peer.create ip ~libp2p_port
                       ~peer_id:(Mina_net2.Keypair.to_peer_id key))
              ; libp2p_port
              ; client_port }
            , key ) )
      in
      let all_peers = addrs_and_ports_list in
      let peers =
        List.init n ~f:(fun i ->
            List.take all_peers i @ List.drop all_peers (i + 1) )
      in
      let%map () = Mina_net2.shutdown net in
      (addrs_and_ports_list, List.map ~f:(List.map ~f:fst) peers) )

let offset (consensus_constants : Consensus.Constants.t) =
  Core.Time.(
    diff (now ())
      (Block_time.to_time consensus_constants.genesis_state_timestamp))

let local_configs ?block_production_interval
    ?(block_production_keys = Fn.const None)
    ?(is_archive_rocksdb = Fn.const false)
    ?(archive_process_location = Fn.const None) n ~acceptable_delay ~chain_id
    ~program_dir ~snark_worker_public_keys ~work_selection_method ~trace_dir
    ~max_concurrent_connections ~runtime_config =
  let%map net_configs = net_configs n in
  let addrs_and_ports_list, peers = net_configs in
  let peers = [] :: List.drop peers 1 in
  let args = List.zip_exn addrs_and_ports_list peers in
  let offset =
    let genesis_state_timestamp =
      match
        Option.bind runtime_config.Runtime_config.genesis
          ~f:(fun {genesis_state_timestamp= ts; _} -> ts)
      with
      | Some timestamp ->
          Genesis_constants.genesis_timestamp_of_string timestamp
      | None ->
          (Lazy.force Precomputed_values.compiled_inputs).consensus_constants
            .genesis_state_timestamp |> Block_time.to_time
    in
    Core.Time.(diff (now ())) genesis_state_timestamp
  in
  let configs =
    List.mapi args ~f:(fun i ((addrs_and_ports, libp2p_keypair), peers) ->
        let public_key =
          Option.bind snark_worker_public_keys ~f:(fun keys ->
              List.nth_exn keys i )
        in
        let addrs_and_ports =
          Node_addrs_and_ports.to_display addrs_and_ports
        in
        let peers = List.map ~f:Node_addrs_and_ports.to_multiaddr_exn peers in
        Coda_process.local_config ?block_production_interval ~is_seed:true
          ~addrs_and_ports ~libp2p_keypair ~net_configs ~peers
          ~snark_worker_key:public_key ~program_dir ~acceptable_delay ~chain_id
          ~block_production_key:(block_production_keys i)
          ~work_selection_method ~trace_dir
          ~is_archive_rocksdb:(is_archive_rocksdb i)
          ~archive_process_location:(archive_process_location i)
          ~offset ~max_concurrent_connections ~runtime_config () )
  in
  configs

let stabalize_and_start_or_timeout ?(timeout_ms = 240000.) nodes =
  let ready () =
    let check_ready node =
      let%map peers = Coda_process.peers_exn node in
      List.length peers >= List.length nodes - 1
    in
    let rec go () =
      if%bind Deferred.List.for_all nodes ~f:check_ready then return ()
      else after (Time.Span.of_ms 100.) >>= go
    in
    go ()
  in
  let before_time = Time.now () in
  match%bind
    Deferred.any
      [ (after (Time.Span.of_ms timeout_ms) >>= fun () -> return `Timeout)
      ; (ready () >>= fun () -> return `Ready) ]
  with
  | `Timeout ->
      failwith @@ sprintf "Nodes couldn't initialize within %f ms" timeout_ms
  | `Ready ->
      let after_time = Time.now () in
      [%log' info (Logger.create ())]
        "Initialized nodes in $time_span_ms ms"
        ~metadata:
          [ ( "time_span_ms"
            , `Float (Time.Span.to_ms (Time.abs_diff before_time after_time))
            ) ] ;
      Deferred.List.iter nodes ~f:(fun node -> Coda_process.start_exn node)

let spawn_local_processes_exn ?(first_delay = 0.0) configs =
  match configs with
  | [] ->
      failwith "Configs should be non-empty"
  | first :: rest ->
      let%bind first_created = Coda_process.spawn_exn first in
      let%bind () = after (Time.Span.of_sec first_delay) in
      let%bind rest_created =
        Deferred.List.all
          (List.map rest ~f:(fun c -> Coda_process.spawn_exn c))
      in
      let all_created = first_created :: rest_created in
      let%map () = stabalize_and_start_or_timeout all_created in
      all_created
