[%%import
"../../config.mlh"]

open Core
open Async
open Network_peer
open O1trace
open Pipe_lib
open Mina_base.Rpc_intf

type ('q, 'r) dispatch =
  Versioned_rpc.Connection_with_menu.t -> 'q -> 'r Deferred.Or_error.t

module Connection_with_state = struct
  type t = Banned | Allowed of Rpc.Connection.t Ivar.t

  let value_map ~when_allowed ~when_banned t =
    match t with Allowed c -> when_allowed c | _ -> when_banned
end

module Config = struct
  type t =
    { timeout: Time.Span.t
    ; initial_peers: Mina_net2.Multiaddr.t list
    ; addrs_and_ports: Node_addrs_and_ports.t
    ; metrics_port: string option
    ; conf_dir: string
    ; chain_id: string
    ; logger: Logger.t
    ; unsafe_no_trust_ip: bool
    ; isolate: bool
    ; trust_system: Trust_system.t
    ; flooding: bool
    ; direct_peers: Mina_net2.Multiaddr.t list
    ; peer_exchange: bool
    ; mina_peer_exchange: bool
    ; seed_peer_list_url: Uri.t option
    ; max_connections: int
    ; validation_queue_size: int
    ; mutable keypair: Mina_net2.Keypair.t option
    ; all_peers_seen_metric: bool }
  [@@deriving make]
end

module type S = sig
  include Intf.Gossip_net_intf

  val create :
       Config.t
    -> pids:Child_processes.Termination.t
    -> Rpc_intf.rpc_handler list
    -> t Deferred.t
end

let rpc_transport_proto = "coda/rpcs/0.0.1"

let download_seed_peer_list uri =
  let%bind _resp, body = Cohttp_async.Client.get uri in
  let%map contents = Cohttp_async.Body.to_string body in
  Mina_net2.Multiaddr.of_file_contents ~contents

module Make (Rpc_intf : Mina_base.Rpc_intf.Rpc_interface_intf) :
  S with module Rpc_intf := Rpc_intf = struct
  open Rpc_intf

  module T = struct
    type t =
      { config: Config.t
      ; mutable added_seeds: Peer.Hash_set.t
      ; net2: Mina_net2.net Deferred.t ref
      ; first_peer_ivar: unit Ivar.t
      ; high_connectivity_ivar: unit Ivar.t
      ; ban_reader: Intf.ban_notification Linear_pipe.Reader.t
      ; message_reader:
          (Message.msg Envelope.Incoming.t * Mina_net2.Validation_callback.t)
          Strict_pipe.Reader.t
      ; subscription:
          Message.msg Mina_net2.Pubsub.Subscription.t Deferred.t ref
      ; restart_helper: unit -> unit }

    let create_rpc_implementations
        (Rpc_handler {rpc; f= handler; cost; budget}) =
      let (module Impl) = implementation_of_rpc rpc in
      let logger = Logger.create () in
      let log_rate_limiter_occasionally rl =
        let t = Time.Span.of_min 1. in
        every t (fun () ->
            [%log' debug logger]
              ~metadata:[("rate_limiter", Network_pool.Rate_limiter.summary rl)]
              !"%s $rate_limiter" Impl.name )
      in
      let rl = Network_pool.Rate_limiter.create ~capacity:budget in
      log_rate_limiter_occasionally rl ;
      let handler (peer : Network_peer.Peer.t) ~version q =
        let score = cost q in
        match
          Network_pool.Rate_limiter.add rl (Remote peer) ~now:(Time.now ())
            ~score
        with
        | `Capacity_exceeded ->
            failwithf "peer exceeded capacity: %s"
              (Network_peer.Peer.to_multiaddr_string peer)
              ()
        | `Within_capacity ->
            handler peer ~version q
      in
      Impl.implement_multi handler

    let prepare_stream_transport stream =
      (* Closing the connection calls close_read on the read
          pipe, which mina_net2 does not expect. To avoid this, add
          an extra pipe and don't propagate the close. We still want
          to close the connection because it flushes all the internal
          state machines and fills the `closed` ivar.

          Pipe.transfer isn't appropriate because it will close the
          real_r when read_w is closed, precisely what we don't want.
      *)
      let read_r, read_w = Pipe.create () in
      let underlying_r, underlying_w = Mina_net2.Stream.pipes stream in
      don't_wait_for
        (Pipe.iter underlying_r ~f:(fun msg ->
             Pipe.write_without_pushback_if_open read_w msg ;
             Deferred.unit )) ;
      let transport =
        Async_rpc_kernel.Pipe_transport.(
          create Kind.string read_r underlying_w)
      in
      transport

    (* peers_snapshot is updated every 30 seconds.
*)
    let peers_snapshot = ref []

    let peers_snapshot_max_staleness = Time.Span.of_sec 30.

    (* Creates just the helper, making sure to register everything
       BEFORE we start listening/advertise ourselves for discovery. *)
    let create_libp2p (config : Config.t) rpc_handlers first_peer_ivar
        high_connectivity_ivar ~added_seeds ~pids ~on_unexpected_termination =
      let%bind seeds_from_url =
        match config.seed_peer_list_url with
        | None ->
            Deferred.return []
        | Some u ->
            download_seed_peer_list u
      in
      let fail err =
        Error.tag err ~tag:"Failed to connect to libp2p_helper process"
        |> Error.raise
      in
      let conf_dir = config.conf_dir ^/ "mina_net2" in
      let%bind () = Unix.mkdir ~p:() conf_dir in
      match%bind
        Monitor.try_with ~here:[%here] ~rest:`Raise (fun () ->
            trace "mina_net2" (fun () ->
                Mina_net2.create
                  ~all_peers_seen_metric:config.all_peers_seen_metric
                  ~logger:config.logger ~conf_dir ~pids
                  ~on_unexpected_termination ) )
      with
      | Ok (Ok net2) -> (
          let open Mina_net2 in
          (* Make an ephemeral keypair for this session TODO: persist in the config dir *)
          let%bind me =
            match config.keypair with
            | Some kp ->
                return kp
            | None ->
                Keypair.random net2
          in
          let my_peer_id = Keypair.to_peer_id me |> Peer.Id.to_string in
          Logger.append_to_global_metadata
            [ ("peer_id", `String my_peer_id)
            ; ( "host"
              , `String
                  (Unix.Inet_addr.to_string config.addrs_and_ports.external_ip)
              )
            ; ("port", `Int config.addrs_and_ports.libp2p_port) ] ;
          ( match config.addrs_and_ports.peer with
          | Some _ ->
              ()
          | None ->
              config.addrs_and_ports.peer
              <- Some
                   (Peer.create config.addrs_and_ports.bind_ip
                      ~libp2p_port:config.addrs_and_ports.libp2p_port
                      ~peer_id:my_peer_id) ) ;
          [%log' info config.logger] "libp2p peer ID this session is $peer_id"
            ~metadata:[("peer_id", `String my_peer_id)] ;
          let ctr = ref 0 in
          let initializing_libp2p_result : _ Deferred.Or_error.t =
            [%log' debug config.logger] "(Re)initializing libp2p result" ;
            let open Deferred.Or_error.Let_syntax in
            let record_peer_connection () =
              [%log' trace config.logger] "Fired peer_connected callback" ;
              Ivar.fill_if_empty first_peer_ivar () ;
              if !ctr < 4 then incr ctr
              else Ivar.fill_if_empty high_connectivity_ivar ()
            in
            let seed_peers =
              List.dedup_and_sort ~compare:Mina_net2.Multiaddr.compare
                (List.concat
                   [ config.initial_peers
                   ; seeds_from_url
                   ; List.map
                       ~f:
                         (Fn.compose Mina_net2.Multiaddr.of_string
                            Peer.to_multiaddr_string)
                       (Hash_set.to_list added_seeds) ])
            in
            let%bind () =
              configure net2 ~me ~logger:config.logger
                ~metrics_port:config.metrics_port
                ~maddrs:
                  [ Multiaddr.of_string
                      (sprintf "/ip4/0.0.0.0/tcp/%d"
                         (Option.value_exn config.addrs_and_ports.peer)
                           .libp2p_port) ]
                ~external_maddr:
                  (Multiaddr.of_string
                     (sprintf "/ip4/%s/tcp/%d"
                        (Unix.Inet_addr.to_string
                           config.addrs_and_ports.external_ip)
                        (Option.value_exn config.addrs_and_ports.peer)
                          .libp2p_port))
                ~network_id:config.chain_id
                ~unsafe_no_trust_ip:config.unsafe_no_trust_ip ~seed_peers
                ~direct_peers:config.direct_peers
                ~peer_exchange:config.peer_exchange
                ~mina_peer_exchange:config.mina_peer_exchange
                ~flooding:config.flooding
                ~max_connections:config.max_connections
                ~validation_queue_size:config.validation_queue_size
                ~initial_gating_config:
                  Mina_net2.
                    { banned_peers=
                        Trust_system.peer_statuses config.trust_system
                        |> List.filter_map ~f:(fun (peer, status) ->
                               match status.banned with
                               | Banned_until _ ->
                                   Some peer
                               | _ ->
                                   None )
                    ; trusted_peers=
                        List.filter_map ~f:Mina_net2.Multiaddr.to_peer
                          config.initial_peers
                    ; isolate= config.isolate }
                ~on_peer_connected:(fun _ -> record_peer_connection ())
                ~on_peer_disconnected:ignore
            in
            let implementation_list =
              List.bind rpc_handlers ~f:create_rpc_implementations
            in
            let implementations =
              let handle_unknown_rpc conn_state ~rpc_tag ~version =
                Deferred.don't_wait_for
                  Trust_system.(
                    record config.trust_system config.logger conn_state
                      Actions.
                        ( Unknown_rpc
                        , Some
                            ( "Attempt to make unknown (fixed-version) RPC \
                               call \"$rpc\" with version $version"
                            , [ ("rpc", `String rpc_tag)
                              ; ("version", `Int version) ] ) )) ;
                `Close_connection
              in
              Rpc.Implementations.create_exn
                ~implementations:(Versioned_rpc.Menu.add implementation_list)
                ~on_unknown_rpc:(`Call handle_unknown_rpc)
            in
            (* We could keep this around to close just this listener if we wanted. We don't. *)
            let%bind _rpc_handler =
              Mina_net2.handle_protocol net2 ~on_handler_error:`Raise
                ~protocol:rpc_transport_proto (fun stream ->
                  let peer = Mina_net2.Stream.remote_peer stream in
                  let transport = prepare_stream_transport stream in
                  let open Deferred.Let_syntax in
                  match%bind
                    Async_rpc_kernel.Rpc.Connection.create ~implementations
                      ~connection_state:(Fn.const peer)
                      ~description:
                        (Info.of_thunk (fun () ->
                             sprintf "stream from %s" peer.peer_id ))
                      transport
                  with
                  | Error handshake_error ->
                      let%bind () =
                        Async_rpc_kernel.Rpc.Transport.close transport
                      in
                      don't_wait_for (Mina_net2.Stream.reset stream >>| ignore) ;
                      Trust_system.(
                        record config.trust_system config.logger peer
                          Actions.
                            ( Incoming_connection_error
                            , Some
                                ( "Handshake error: $exn"
                                , [ ( "exn"
                                    , `String (Exn.to_string handshake_error)
                                    ) ] ) ))
                  | Ok rpc_connection -> (
                      let%bind () =
                        Async_rpc_kernel.Rpc.Connection.close_finished
                          rpc_connection
                      in
                      let%bind () =
                        Async_rpc_kernel.Rpc.Connection.close
                          ~reason:(Info.of_string "connection completed")
                          rpc_connection
                      in
                      match%map Mina_net2.Stream.reset stream with
                      | Error e ->
                          [%log' warn config.logger]
                            "failed to reset stream (this means it was \
                             probably closed successfully): $error"
                            ~metadata:[("error", Error_json.error_to_yojson e)]
                      | Ok () ->
                          () ) )
            in
            let message_reader, message_writer =
              Strict_pipe.(
                create
                  ~name:"Gossip_net.Libp2p messages with validation callbacks"
                  Synchronous)
            in
            let%bind subscription =
              Mina_net2.Pubsub.subscribe_encode net2
                "coda/consensus-messages/0.0.1"
                (* Fix for #4097: validation is tied into a lot of complex control flow.
                   Instead of refactoring it to have validation up-front and decoupled,
                   we pass along a validation callback with the message. This ends up
                   ignoring the actual subscription message pipe, so drain it separately. *)
                ~should_forward_message:(fun envelope validation_callback ->
                  (* Messages from ourselves are valid. Don't try and reingest them. *)
                  match Envelope.Incoming.sender envelope with
                  | Local ->
                      Mina_net2.Validation_callback.fire_exn
                        validation_callback `Accept ;
                      Deferred.unit
                  | Remote sender ->
                      if not (Peer.Id.equal sender.peer_id my_peer_id) then
                        Strict_pipe.Writer.write message_writer
                          (envelope, validation_callback)
                      else (
                        Mina_net2.Validation_callback.fire_exn
                          validation_callback `Accept ;
                        Deferred.unit ) )
                ~bin_prot:Message.Latest.T.bin_msg
                ~on_decode_failure:
                  (`Call
                    (fun envelope (err : Error.t) ->
                      let peer =
                        Envelope.Incoming.sender envelope
                        |> Envelope.Sender.remote_exn
                      in
                      let metadata =
                        [ ("sender_peer_id", `String peer.peer_id)
                        ; ("error", Error_json.error_to_yojson err) ]
                      in
                      Trust_system.(
                        record config.trust_system config.logger peer
                          Actions.
                            ( Decoding_failed
                            , Some ("failed to decode gossip message", metadata)
                            ))
                      |> don't_wait_for ;
                      () ))
            in
            (* #4097 fix: drain the published message pipe, which we don't care about. *)
            don't_wait_for
              (Strict_pipe.Reader.iter
                 (Mina_net2.Pubsub.Subscription.message_pipe subscription)
                 ~f:(fun _envelope -> Deferred.unit)) ;
            let%map _ =
              (* XXX: this ALWAYS needs to be AFTER handle_protocol/subscribe
                 or it is possible to miss connections! *)
              listen_on net2
                (Multiaddr.of_string
                   (sprintf "/ip4/%s/tcp/%d"
                      ( config.addrs_and_ports.bind_ip
                      |> Unix.Inet_addr.to_string )
                      (Option.value_exn config.addrs_and_ports.peer)
                        .libp2p_port))
            in
            let add_many xs ~seed =
              Deferred.map
                (Deferred.List.iter ~how:`Parallel xs ~f:(fun x ->
                     let open Deferred.Let_syntax in
                     Mina_net2.add_peer ~seed net2 x >>| ignore ))
                ~f:(fun () -> Ok ())
            in
            don't_wait_for
              (Deferred.map
                 (let%bind () = add_many seed_peers ~seed:true
                  and () =
                    let seeds =
                      String.Hash_set.of_list
                        (List.map ~f:Multiaddr.to_string seed_peers)
                    in
                    add_many ~seed:false
                      (List.filter !peers_snapshot ~f:(fun p ->
                           not (Hash_set.mem seeds (Multiaddr.to_string p)) ))
                  in
                  let%bind () = Mina_net2.begin_advertising net2 in
                  return ())
                 ~f:(function
                   | Ok () ->
                       ()
                   | Error e ->
                       [%log' warn config.logger]
                         "starting libp2p up failed: $error"
                         ~metadata:[("error", Error_json.error_to_yojson e)] )) ;
            (subscription, message_reader)
          in
          match%map initializing_libp2p_result with
          | Ok (subscription, message_reader) ->
              (net2, subscription, message_reader, me)
          | Error e ->
              fail e )
      | Ok (Error e) ->
          fail e
      | Error e ->
          fail (Error.of_exn e)

    let peers t = !(t.net2) >>= Mina_net2.peers

    let create (config : Config.t) ~pids rpc_handlers =
      let first_peer_ivar = Ivar.create () in
      let high_connectivity_ivar = Ivar.create () in
      let message_reader, message_writer =
        Strict_pipe.create ~name:"libp2p_messages" Synchronous
      in
      let net2_ref = ref (Deferred.never ()) in
      let subscription_ref = ref (Deferred.never ()) in
      let restarts_r, restarts_w =
        Strict_pipe.create ~name:"libp2p-restarts"
          (Strict_pipe.Buffered (`Capacity 0, `Overflow Strict_pipe.Drop_head))
      in
      let added_seeds = Peer.Hash_set.create () in
      let%bind () =
        let rec on_libp2p_create res =
          net2_ref :=
            Deferred.map res ~f:(fun (n, _, _, _) ->
                ( match
                    Sys.getenv "MINA_LIBP2P_HELPER_RESTART_INTERVAL_BASE"
                  with
                | Some base_time ->
                    let restart_after =
                      let plus_or_minus initial ~delta =
                        initial +. (Random.float (2. *. delta) -. delta)
                      in
                      let base_time = Float.of_string base_time in
                      let delta =
                        Option.value_map ~f:Float.of_string
                          (Sys.getenv
                             "MINA_LIBP2P_HELPER_RESTART_INTERVAL_DELTA")
                          ~default:2.5
                        |> Float.min (base_time /. 2.)
                      in
                      Time.Span.(of_min (base_time |> plus_or_minus ~delta))
                    in
                    upon (after restart_after) (fun () ->
                        don't_wait_for
                          (let%bind () = Mina_net2.shutdown n in
                           on_unexpected_termination ()) )
                | None ->
                    () ) ;
                n ) ;
          subscription_ref := Deferred.map res ~f:(fun (_, s, _, _) -> s) ;
          upon res (fun (_, _, m, me) ->
              (* This is a hack so that we keep the same keypair across restarts. *)
              config.keypair <- Some me ;
              let logger = config.logger in
              [%log trace] ~metadata:[] "Successfully restarted libp2p" ;
              don't_wait_for (Strict_pipe.transfer m message_writer ~f:Fn.id)
          )
        and on_unexpected_termination () =
          on_libp2p_create
            (create_libp2p config rpc_handlers first_peer_ivar
               high_connectivity_ivar ~added_seeds ~pids
               ~on_unexpected_termination) ;
          Deferred.unit
        in
        let res =
          create_libp2p config rpc_handlers first_peer_ivar
            high_connectivity_ivar ~added_seeds ~pids
            ~on_unexpected_termination
        in
        on_libp2p_create res ;
        don't_wait_for
          (Strict_pipe.Reader.iter restarts_r ~f:(fun () ->
               let%bind n = !net2_ref in
               let%bind () = Mina_net2.shutdown n in
               let%bind () = on_unexpected_termination () in
               !net2_ref >>| ignore )) ;
        let%map _ = res in
        ()
      in
      let ban_configuration =
        ref {Mina_net2.banned_peers= []; trusted_peers= []; isolate= false}
      in
      let do_ban (banned_peer, expiration) =
        don't_wait_for
          ( Clock.at expiration
          >>= fun () ->
          let%bind net2 = !net2_ref in
          ban_configuration :=
            { !ban_configuration with
              banned_peers=
                List.filter !ban_configuration.banned_peers ~f:(fun p ->
                    not (Peer.equal p banned_peer) ) } ;
          Mina_net2.set_connection_gating_config net2 !ban_configuration
          |> Deferred.ignore_m ) ;
        (let%bind net2 = !net2_ref in
         ban_configuration :=
           { !ban_configuration with
             banned_peers= banned_peer :: !ban_configuration.banned_peers } ;
         Mina_net2.set_connection_gating_config net2 !ban_configuration)
        |> Deferred.ignore_m
      in
      let%map () =
        Deferred.List.iter (Trust_system.peer_statuses config.trust_system)
          ~f:(function
          | ( addr
            , {banned= Trust_system.Banned_status.Banned_until expiration; _} )
            ->
              do_ban (addr, expiration)
          | _ ->
              Deferred.unit )
      in
      let ban_reader, ban_writer = Linear_pipe.create () in
      don't_wait_for
        (let%map () =
           Strict_pipe.Reader.iter
             (Trust_system.ban_pipe config.trust_system)
             ~f:do_ban
         in
         Linear_pipe.close ban_writer) ;
      let t =
        { config
        ; added_seeds
        ; net2= net2_ref
        ; first_peer_ivar
        ; high_connectivity_ivar
        ; subscription= subscription_ref
        ; message_reader
        ; ban_reader
        ; restart_helper= (fun () -> Strict_pipe.Writer.write restarts_w ()) }
      in
      Clock.every' peers_snapshot_max_staleness (fun () ->
          let%map peers = peers t in
          Mina_metrics.(
            Gauge.set Network.peers (List.length peers |> Int.to_float)) ;
          peers_snapshot :=
            List.map peers
              ~f:
                (Fn.compose Mina_net2.Multiaddr.of_string
                   Peer.to_multiaddr_string) ) ;
      t

    let set_node_status t data =
      !(t.net2) >>= Fn.flip Mina_net2.set_node_status data

    let get_peer_node_status t peer =
      !(t.net2) >>= Fn.flip Mina_net2.get_peer_node_status peer

    let initial_peers t = t.config.initial_peers

    let add_peer t p ~seed =
      let open Mina_net2 in
      if seed then Hash_set.add t.added_seeds p ;
      !(t.net2)
      >>= Fn.flip (add_peer ~seed)
            (Multiaddr.of_string (Peer.to_multiaddr_string p))

    (* OPTIMIZATION: use fast n choose k implementation - see python or old flow code *)
    let random_sublist xs n = List.take (List.permute xs) n

    let random_peers t n =
      let%map peers = peers t in
      random_sublist peers n

    let random_peers_except t n ~except =
      let%map peers = peers t in
      random_sublist
        Hash_set.(diff (Peer.Hash_set.of_list peers) except |> to_list)
        n

    let try_call_rpc_with_dispatch : type r q.
           ?heartbeat_timeout:Time_ns.Span.t
        -> ?timeout:Time.Span.t
        -> rpc_name:string
        -> t
        -> Peer.t
        -> Async.Rpc.Transport.t
        -> (r, q) dispatch
        -> r
        -> q Deferred.Or_error.t =
     fun ?heartbeat_timeout ?timeout ~rpc_name t peer transport dispatch query ->
      let call () =
        Monitor.try_with ~here:[%here] (fun () ->
            (* Async_rpc_kernel takes a transport instead of a Reader.t *)
            Async_rpc_kernel.Rpc.Connection.with_close
              ~heartbeat_config:
                (Async_rpc_kernel.Rpc.Connection.Heartbeat_config.create
                   ~send_every:(Time_ns.Span.of_sec 10.)
                   ~timeout:
                     (Option.value ~default:(Time_ns.Span.of_sec 120.)
                        heartbeat_timeout))
              ~connection_state:(Fn.const ())
              ~dispatch_queries:(fun conn ->
                Versioned_rpc.Connection_with_menu.create conn
                >>=? fun conn' ->
                let d = dispatch conn' query in
                match timeout with
                | None ->
                    d
                | Some timeout ->
                    Deferred.choose
                      [ Deferred.choice d Fn.id
                      ; choice (after timeout) (fun () ->
                            Or_error.error_string "rpc timed out" ) ] )
              transport
              ~on_handshake_error:
                (`Call
                  (fun exn ->
                    let%map () =
                      Trust_system.(
                        record t.config.trust_system t.config.logger peer
                          Actions.
                            ( Outgoing_connection_error
                            , Some
                                ( "Handshake error: $exn"
                                , [("exn", `String (Exn.to_string exn))] ) ))
                    in
                    Or_error.error_string "handshake error" )) )
        >>= function
        | Ok (Ok result) ->
            (* call succeeded, result is valid *)
            Deferred.return (Ok result)
        | Ok (Error err) -> (
            (* call succeeded, result is an error *)
            [%log' warn t.config.logger] "RPC call error for $rpc"
              ~metadata:
                [ ("rpc", `String rpc_name)
                ; ("error", Error_json.error_to_yojson err) ] ;
            match (Error.to_exn err, Error.sexp_of_t err) with
            | ( _
              , Sexp.List
                  [ Sexp.List
                      [ Sexp.Atom "rpc_error"
                      ; Sexp.List [Sexp.Atom "Connection_closed"; _] ]
                  ; _connection_description
                  ; _rpc_tag
                  ; _rpc_version ] ) ->
                let%map () =
                  Trust_system.(
                    record t.config.trust_system t.config.logger peer
                      Actions.
                        ( Outgoing_connection_error
                        , Some ("Closed connection", []) ))
                in
                Error err
            | _ ->
                let%map () =
                  Trust_system.(
                    record t.config.trust_system t.config.logger peer
                      Actions.
                        ( Outgoing_connection_error
                        , Some
                            ( "RPC call failed, reason: $exn"
                            , [("exn", Error_json.error_to_yojson err)] ) ))
                in
                Error err )
        | Error monitor_exn ->
            (* call itself failed *)
            (* TODO: learn what other exceptions are raised here *)
            let exn = Monitor.extract_exn monitor_exn in
            let () =
              match Error.sexp_of_t (Error.of_exn exn) with
              | Sexp.List (Sexp.Atom "connection attempt timeout" :: _) ->
                  [%log' debug t.config.logger]
                    "RPC call for $rpc raised an exception"
                    ~metadata:
                      [ ("rpc", `String rpc_name)
                      ; ("exn", `String (Exn.to_string exn)) ]
              | _ ->
                  [%log' warn t.config.logger]
                    "RPC call for $rpc raised an exception"
                    ~metadata:
                      [ ("rpc", `String rpc_name)
                      ; ("exn", `String (Exn.to_string exn)) ]
            in
            Deferred.return (Or_error.of_exn exn)
      in
      call ()

    let try_call_rpc : type q r.
           ?heartbeat_timeout:Time_ns.Span.t
        -> ?timeout:Time.Span.t
        -> t
        -> Peer.t
        -> _
        -> (q, r) rpc
        -> q
        -> r Deferred.Or_error.t =
     fun ?heartbeat_timeout ?timeout t peer transport rpc query ->
      let (module Impl) = implementation_of_rpc rpc in
      try_call_rpc_with_dispatch ?heartbeat_timeout ?timeout
        ~rpc_name:Impl.name t peer transport Impl.dispatch_multi query

    let query_peer ?heartbeat_timeout ?timeout t (peer_id : Peer.Id.t) rpc
        rpc_input =
      let%bind net2 = !(t.net2) in
      match%bind
        Mina_net2.open_stream net2 ~protocol:rpc_transport_proto peer_id
      with
      | Ok stream ->
          let peer = Mina_net2.Stream.remote_peer stream in
          let transport = prepare_stream_transport stream in
          try_call_rpc ?heartbeat_timeout ?timeout t peer transport rpc
            rpc_input
          >>| fun data ->
          Connected (Envelope.Incoming.wrap_peer ~data ~sender:peer)
      | Error e ->
          return (Failed_to_connect e)

    let query_peer' (type q r) ?how ?heartbeat_timeout ?timeout t
        (peer_id : Peer.Id.t) (rpc : (q, r) rpc) (qs : q list) =
      let%bind net2 = !(t.net2) in
      match%bind
        Mina_net2.open_stream net2 ~protocol:rpc_transport_proto peer_id
      with
      | Ok stream ->
          let peer = Mina_net2.Stream.remote_peer stream in
          let transport = prepare_stream_transport stream in
          let (module Impl) = implementation_of_rpc rpc in
          try_call_rpc_with_dispatch ?heartbeat_timeout ?timeout
            ~rpc_name:Impl.name t peer transport
            (fun conn qs ->
              Deferred.Or_error.List.map ?how qs ~f:(fun q ->
                  Impl.dispatch_multi conn q ) )
            qs
          >>| fun data ->
          Connected (Envelope.Incoming.wrap_peer ~data ~sender:peer)
      | Error e ->
          return (Failed_to_connect e)

    let query_random_peers t n rpc query =
      let%map peers = random_peers t n in
      [%log' trace t.config.logger]
        !"Querying random peers: %s"
        (Peer.pretty_list peers) ;
      List.map peers ~f:(fun peer -> query_peer t peer.peer_id rpc query)

    let broadcast t msg =
      don't_wait_for
        (let%bind subscription = !(t.subscription) in
         Mina_net2.Pubsub.Subscription.publish subscription msg)

    let on_first_connect t ~f = Deferred.map (Ivar.read t.first_peer_ivar) ~f

    let on_first_high_connectivity t ~f =
      Deferred.map (Ivar.read t.high_connectivity_ivar) ~f

    let received_message_reader t = t.message_reader

    let ban_notification_reader t = t.ban_reader

    let ip_for_peer t peer_id =
      let%bind net2 = !(t.net2) in
      Mina_net2.lookup_peerid net2 peer_id
      >>| function Ok p -> Some p | Error _ -> None

    let connection_gating t =
      let%bind net2 = !(t.net2) in
      Mina_net2.connection_gating_config net2

    let set_connection_gating t config =
      let%bind net2 = !(t.net2) in
      Mina_net2.set_connection_gating_config net2 config

    let restart_helper t = t.restart_helper ()
  end

  include T
end
