[%%import
"/src/config.mlh"]

open Core
open Async

let init () = Parallel.init_master ()

type ports = {communication_port: int; discovery_port: int; libp2p_port: int}

let net_configs n =
  let ips = List.init n ~f:(fun _i -> Unix.Inet_addr.of_string "127.0.0.1") in
  let addrs_and_ports_list =
    List.mapi ips ~f:(fun i ip ->
        let base = 23000 + (i * 3) in
        let communication_port = base in
        let discovery_port = base + 1 in
        let libp2p_port = base + 2 in
        let client_port = 20000 + i in
        { Kademlia.Node_addrs_and_ports.external_ip= ip
        ; bind_ip= ip
        ; discovery_port
        ; communication_port
        ; libp2p_port
        ; client_port } )
  in
  let all_peers =
    List.map addrs_and_ports_list
      ~f:Kademlia.Node_addrs_and_ports.to_discovery_host_and_port
  in
  let peers =
    List.init n ~f:(fun i -> List.take all_peers i @ List.drop all_peers (i + 1)
    )
  in
  (addrs_and_ports_list, peers)

let offset =
  lazy
    Core.Time.(
      diff (now ())
        ( Consensus.Constants.genesis_state_timestamp
        |> Coda_base.Block_time.to_time ))

let local_configs ?proposal_interval ?(proposers = Fn.const None)
    ?(is_archive_rocksdb = Fn.const false) n ~acceptable_delay ~program_dir
    ~snark_worker_public_keys ~work_selection_method ~trace_dir
    ~max_concurrent_connections =
  let addrs_and_ports_list, peers = net_configs n in
  let peers = [] :: List.drop peers 1 in
  let args = List.zip_exn addrs_and_ports_list peers in
  let configs =
    List.mapi args ~f:(fun i (addrs_and_ports, peers) ->
        let public_key =
          Option.bind snark_worker_public_keys ~f:(fun keys ->
              List.nth_exn keys i )
        in
        Coda_process.local_config ?proposal_interval ~addrs_and_ports ~peers
          ~snark_worker_key:public_key ~program_dir ~acceptable_delay
          ~proposer:(proposers i) ~work_selection_method ~trace_dir
          ~is_archive_rocksdb:(is_archive_rocksdb i)
          ~offset:(Lazy.force offset) ~max_concurrent_connections () )
  in
  configs

let stabalize_and_start_or_timeout ?(timeout_ms = 10000.) nodes =
  let ready () =
    let check_ready node =
      let%map peers = Coda_process.peers_exn node in
      List.length peers = List.length nodes - 1
    in
    let rec go () =
      if%bind Deferred.List.for_all nodes ~f:check_ready then return ()
      else go ()
    in
    go ()
  in
  match%bind
    Deferred.any
      [ (after (Time.Span.of_ms timeout_ms) >>= fun () -> return `Timeout)
      ; (ready () >>= fun () -> return `Ready) ]
  with
  | `Timeout ->
      failwith @@ sprintf "Nodes couldn't initialize within %f ms" timeout_ms
  | `Ready ->
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
