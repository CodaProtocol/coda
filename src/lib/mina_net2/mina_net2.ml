open Core
open Async
open Async_unix
open Deferred.Let_syntax
open Pipe_lib
open Network_peer
module Timeout = Timeout_lib.Core_time_ns

module Validation_callback = struct
  type validation_result = [ `Accept | `Reject | `Ignore ] [@@deriving equal]

  type t = { expiration : Time_ns.t option; signal : validation_result Ivar.t }

  let create expiration =
    { expiration = Some expiration; signal = Ivar.create () }

  let create_without_expiration () =
    { expiration = None; signal = Ivar.create () }

  let is_expired cb =
    match cb.expiration with
    | None ->
        false
    | Some expires_at ->
        Time_ns.(now () >= expires_at)

  let await_timeout cb =
    if is_expired cb then Deferred.return ()
    else
      match cb.expiration with
      | None ->
          Deferred.never ()
      | Some expires_at ->
          after
            ( Time_ns.Span.to_span_float_round_nearest
            @@ Time_ns.diff expires_at (Time_ns.now ()) )

  let await cb =
    if is_expired cb then Deferred.return None
    else
      match cb.expiration with
      | None ->
          Ivar.read cb.signal >>| Option.some
      | Some expires_at -> (
          match%map
            Timeout.await ()
              ~timeout_duration:(Time_ns.diff expires_at (Time_ns.now ()))
              (Ivar.read cb.signal)
          with
          | `Ok result ->
              Some result
          | `Timeout ->
              None )

  let await_exn cb =
    match%map await cb with None -> failwith "timeout" | Some result -> result

  let fire_if_not_already_fired cb result =
    if not (is_expired cb) then (
      if Ivar.is_full cb.signal then
        [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
      Ivar.fill cb.signal result )

  let fire_exn cb result =
    if not (is_expired cb) then (
      if Ivar.is_full cb.signal then
        [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
      Ivar.fill cb.signal result )
end

(** simple types for yojson to derive, later mapped into a Peer.t *)
type peer_info = { libp2p_port : int; host : string; peer_id : string }
[@@deriving yojson]

type connection_gating =
  { banned_peers : Peer.t list; trusted_peers : Peer.t list; isolate : bool }
[@@deriving yojson]

let peer_of_peer_info peer_info =
  Peer.create
    (Unix.Inet_addr.of_string peer_info.host)
    ~libp2p_port:peer_info.libp2p_port
    ~peer_id:(Peer.Id.unsafe_of_string peer_info.peer_id)

let of_b64_data = function
  | `String s -> (
      match Base64.decode s with
      | Ok result ->
          Ok result
      | Error (`Msg s) ->
          Or_error.error_string ("invalid base64: " ^ s) )
  | _ ->
      Or_error.error_string "expected a string"

let to_b64_data (s : string) = Base64.encode_string ~pad:true s

let to_int_res x =
  match Yojson.Safe.Util.to_int_option x with
  | Some i ->
      Ok i
  | None ->
      Or_error.error_string "needed an int"

module Keypair0 = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { secret : string; public : string; peer_id : Peer.Id.Stable.V1.t }

      let to_latest = Fn.id
    end
  end]
end

type stream_state =
  | FullyOpen  (** Streams start in this state. Both sides can still write *)
  | HalfClosed of [ `Us | `Them ]
      (** Streams move from [FullyOpen] to [HalfClosed `Us] when the write pipe is closed. Streams move from [FullyOpen] to [HalfClosed `Them] when [Stream.reset] is called or the remote host closes their write stream. *)
  | FullyClosed
      (** Streams move from [HalfClosed peer] to FullyClosed once the party that isn't peer has their "close write" event. Once a stream is FullyClosed, its resources are released. *)
[@@deriving show]

type erased_magic = [ `Be_very_careful_to_be_type_safe ]

module Go_log = struct
  let ours_of_go lvl =
    let open Logger.Level in
    match lvl with
    | "error" | "panic" | "fatal" ->
        Error
    | "warn" ->
        Debug
    | "info" ->
        (* this is intentionally debug, because the go info logs are too verbose for our info *)
        Debug
    | "debug" ->
        Spam
    | _ ->
        Spam

  (* there should be no other levels. *)

  type record =
    { ts : string
    ; module_ : string [@key "logger"]
    ; level : string
    ; msg : string
    ; metadata : Yojson.Safe.t String.Map.t
    }

  let record_of_yojson (json : Yojson.Safe.t) =
    let open Result.Let_syntax in
    let prefix = "Mina_net2.Go_log.record_of_yojson: " in
    match json with
    | `Assoc fields ->
        let set_field field_name prev_value parse json =
          match prev_value with
          | Some _ ->
              Error
                (prefix ^ "Field '" ^ field_name ^ "' appears multiple times")
          | None ->
              parse json
              |> Result.map_error ~f:(fun err ->
                     prefix ^ "Could not parse field '" ^ field_name ^ "':"
                     ^ err)
              |> Result.map ~f:Option.return
        in
        let get_field field_name value =
          match value with
          | Some x ->
              Ok x
          | None ->
              Error (prefix ^ "Field '" ^ field_name ^ "' is required")
        in
        let string_of_yojson = function
          | `String s ->
              Ok s
          | _ ->
              Error "Expected a string"
        in
        let%bind ts, module_, level, msg, metadata =
          List.fold_result ~init:(None, None, None, None, String.Map.empty)
            fields ~f:(fun (ts, module_, level, msg, metadata) (field, json) ->
              match field with
              | "ts" ->
                  let%map ts = set_field "ts" ts string_of_yojson json in
                  (ts, module_, level, msg, metadata)
              | "logger" ->
                  let%map module_ =
                    set_field "logger" module_ string_of_yojson json
                  in
                  (ts, module_, level, msg, metadata)
              | "level" ->
                  let%map level =
                    set_field "level" level string_of_yojson json
                  in
                  (ts, module_, level, msg, metadata)
              | "msg" ->
                  let%map msg = set_field "msg" msg string_of_yojson json in
                  (ts, module_, level, msg, metadata)
              | _ ->
                  let field =
                    if String.equal field "error" then "go_error" else field
                  in
                  Ok
                    ( ts
                    , module_
                    , level
                    , msg
                    , Map.set ~key:field ~data:json metadata ))
        in
        let%bind ts = get_field "ts" ts in
        let%bind module_ = get_field "logger" module_ in
        let%bind level = get_field "level" level in
        let%map msg = get_field "msg" msg in
        { ts; module_; level; msg; metadata }
    | _ ->
        Error (prefix ^ "Expected a JSON object")

  let record_to_message r =
    Logger.Message.
      { timestamp = Time.of_string r.ts
      ; level = ours_of_go r.level
      ; source =
          Some
            (Logger.Source.create
               ~module_:(sprintf "Libp2p_helper.Go.%s" r.module_)
               ~location:"(not tracked)")
      ; message = r.msg
      ; metadata = r.metadata
      ; event_id = None
      }
end

(** Set of peers, represented as a host/port pair. We ignore the peer ID so
    that the same node restarting and attaining a new peer ID will not be
    double (or triple, etc.) counted.
*)
module Peers_no_ids = struct
  module T = struct
    type t = { libp2p_port : int; host : string }
    [@@deriving sexp, compare, yojson]
  end

  include T
  module Set = Set.Make (T)
end

module Helper = struct
  type t =
    { subprocess : Child_processes.t
    ; conf_dir : string
    ; outstanding_requests : (int, Yojson.Safe.t Or_error.t Ivar.t) Hashtbl.t
          (**
       seqno is used to assign unique IDs to our outbound requests and index the
       tables below.

       The helper can also generate sequence numbers- but they are not the same space
       of sequence numbers!

       In general, if a message contains a seqno/idx, the response should contain the
       same seqno/idx.

       Some types would make it harder to misuse these integers.
    *)
    ; mutable seqno : int
    ; mutable connection_gating : connection_gating
    ; logger : Logger.t
    ; me_keypair : Keypair0.t Ivar.t
    ; subscriptions : (int, erased_magic subscription) Hashtbl.t
    ; streams : (int, stream) Hashtbl.t
    ; protocol_handlers : (string, protocol_handler) Hashtbl.t
    ; mutable all_peers_seen : Peers_no_ids.Set.t option
    ; mutable banned_ips : Unix.Inet_addr.t list
    ; mutable peer_connected_callback : (string -> unit) option
    ; mutable peer_disconnected_callback : (string -> unit) option
    ; mutable finished : bool
    }

  and 'a subscription =
    { net : t
    ; topic : string
    ; idx : int
    ; mutable closed : bool
    ; validator :
        'a Envelope.Incoming.t -> Validation_callback.t -> unit Deferred.t
    ; encode : 'a -> string
    ; on_decode_failure :
        [ `Ignore | `Call of string Envelope.Incoming.t -> Error.t -> unit ]
    ; decode : string -> 'a Or_error.t
    ; write_pipe :
        ( 'a Envelope.Incoming.t
        , Strict_pipe.synchronous
        , unit Deferred.t )
        Strict_pipe.Writer.t
    ; read_pipe : 'a Envelope.Incoming.t Strict_pipe.Reader.t
    }

  and stream =
    { net : t
    ; idx : int
    ; mutable state : stream_state
    ; mutable state_lock : bool
    ; state_wait : unit Async.Condition.t
    ; protocol : string
    ; peer : Peer.t
    ; incoming_r : string Pipe.Reader.t
    ; incoming_w : string Pipe.Writer.t
    ; outgoing_r : string Pipe.Reader.t
    ; outgoing_w : string Pipe.Writer.t
    }

  and protocol_handler =
    { net : t
    ; protocol_name : string
    ; mutable closed : bool
    ; on_handler_error : [ `Raise | `Ignore | `Call of stream -> exn -> unit ]
    ; f : stream -> unit Deferred.t
    }

  module type Rpc = sig
    type input [@@deriving to_yojson]

    type output [@@deriving of_yojson]

    val name : string
  end

  type ('a, 'b) rpc = (module Rpc with type input = 'a and type output = 'b)

  module Data : sig
    type t [@@deriving yojson]

    val pack_data : string -> t

    val to_string : t -> string
  end = struct
    type t = string

    let encode_string t = Base64.encode_string ~pad:true t

    let decode_string s = Base64.decode_exn s

    let to_yojson s = `String (encode_string s)

    let of_yojson = function
      | `String s -> (
          try Ok (decode_string s)
          with exn -> Error Error.(to_string_hum (of_exn exn)) )
      | _ ->
          Error "expected a string"

    let pack_data s = s

    let to_string s = s
  end

  module Rpcs = struct
    module No_input = struct
      type input = unit

      let input_to_yojson () = `Assoc []
    end

    module Send_stream_msg = struct
      type input = { stream_idx : int; data : string } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "sendStreamMsg"
    end

    module Close_stream = struct
      type input = { stream_idx : int } [@@deriving yojson]

      type output = string [@@deriving yojson]

      (* This RPC remains unused, see below for the commented out
         Close_stream usage *)
      let[@warning "-32"] name = "closeStream"
    end

    module Remove_stream_handler = struct
      type input = { protocol : string } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "removeStreamHandler"
    end

    module Generate_keypair = struct
      include No_input

      type output = { sk : string; pk : string; peer_id : string }
      [@@deriving yojson]

      let name = "generateKeypair"
    end

    module Publish = struct
      type input = { topic : string; data : Data.t } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "publish"
    end

    module Subscribe = struct
      type input = { topic : string; subscription_idx : int }
      [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "subscribe"
    end

    module Unsubscribe = struct
      type input = { subscription_idx : int } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "unsubscribe"
    end

    module Set_gater_config = struct
      type input =
        { banned_ips : string list
        ; banned_peers : string list
        ; trusted_peers : string list
        ; trusted_ips : string list
        ; isolate : bool
        }
      [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "setGatingConfig"
    end

    module Configure = struct
      type input =
        { privk : string
        ; statedir : string
        ; ifaces : string list
        ; metrics_port : string
        ; external_maddr : string
        ; network_id : string
        ; unsafe_no_trust_ip : bool
        ; flood : bool
        ; direct_peers : string list
        ; peer_exchange : bool
        ; gating_config : Set_gater_config.input
        ; seed_peers : string list
        ; max_connections : int
        ; validation_queue_size : int
        ; mina_peer_exchange : bool
        }
      [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "configure"
    end

    module Listen = struct
      type input = { iface : string } [@@deriving yojson]

      type output = string list [@@deriving yojson]

      let name = "listen"
    end

    module Listening_addrs = struct
      include No_input

      type output = string list [@@deriving yojson]

      let name = "listeningAddrs"
    end

    module Reset_stream = struct
      type input = { idx : int } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "resetStream"
    end

    module Add_stream_handler = struct
      type input = { protocol : string } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "addStreamHandler"
    end

    module Open_stream = struct
      type input = { peer : string; protocol : string } [@@deriving yojson]

      type output = { stream_idx : int; peer : peer_info } [@@deriving yojson]

      let name = "openStream"
    end

    module Validation_complete = struct
      type input = { seqno : int; is_valid : string } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "validationComplete"
    end

    module Add_peer = struct
      type input = { multiaddr : string; seed : bool } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "addPeer"
    end

    module Begin_advertising = struct
      include No_input

      type output = string [@@deriving yojson]

      let name = "beginAdvertising"
    end

    module List_peers = struct
      include No_input

      type output = peer_info list [@@deriving yojson]

      let name = "listPeers"
    end

    module Set_node_status = struct
      type input = { data : string } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "setNodeStatus"
    end

    module Get_peer_node_status = struct
      type input = { peer_multiaddr : string } [@@deriving yojson]

      type output = string [@@deriving yojson]

      let name = "getPeerNodeStatus"
    end

    module Find_peer = struct
      type input = { peer_id : string } [@@deriving yojson]

      type output = peer_info [@@deriving yojson]

      let name = "findPeer"
    end
  end

  let gating_config_to_helper_format (config : connection_gating) =
    let trusted_ips =
      List.map
        ~f:(fun p -> Unix.Inet_addr.to_string p.host)
        config.trusted_peers
    in
    let banned_ips =
      let trusted = String.Set.of_list trusted_ips in
      List.filter_map
        ~f:(fun p ->
          let p = Unix.Inet_addr.to_string p.host in
          (* Trusted peers cannot be banned. *)
          if Set.mem trusted p then None else Some p)
        config.banned_peers
    in
    Rpcs.Set_gater_config.
      { banned_ips
      ; banned_peers = List.map ~f:(fun p -> p.peer_id) config.banned_peers
      ; trusted_ips
      ; trusted_peers = List.map ~f:(fun p -> p.peer_id) config.trusted_peers
      ; isolate = config.isolate
      }

  (** Generate the next sequence number for our side of the connection *)
  let genseq t =
    let v = t.seqno in
    t.seqno <- t.seqno + 1 ;
    v

  (** [do_rpc net rpc body] will encode [body] as JSON according to [rpc],
      send it to the helper, and return a deferred that resolves once the daemon
      gets around to replying. *)
  let do_rpc (type a b) t (rpc : (a, b) rpc) (body : a) : b Deferred.Or_error.t
      =
    let module M = (val rpc) in
    if
      (not t.finished)
      && (not @@ Writer.is_closed (Child_processes.stdin t.subprocess))
    then (
      let res = Ivar.create () in
      let seqno = genseq t in
      Hashtbl.add_exn t.outstanding_requests ~key:seqno ~data:res ;
      let actual_obj =
        `Assoc
          [ ("seqno", `Int seqno)
          ; ("method", `String M.name)
          ; ("body", M.input_to_yojson body)
          ]
      in
      let rpc = Yojson.Safe.to_string actual_obj in
      [%log' spam t.logger] "sending line to libp2p_helper: $line"
        ~metadata:
          [ ( "line"
            , `String (String.slice rpc 0 (Int.min (String.length rpc) 2048)) )
          ] ;
      Writer.write_line (Child_processes.stdin t.subprocess) rpc ;
      let%map res_json = Ivar.read res in
      Or_error.bind res_json
        ~f:(Fn.compose (Result.map_error ~f:Error.of_string) M.output_of_yojson)
      )
    else
      Deferred.Or_error.errorf "helper process already exited (doing RPC %s)"
        (M.input_to_yojson body |> Yojson.Safe.to_string)

  let stream_state_invariant stream logger =
    let us_closed = Pipe.is_closed stream.outgoing_w in
    let them_closed = Pipe.is_closed stream.incoming_w in
    [%log trace] "%sus_closed && %sthem_closed"
      (if us_closed then "" else "not ")
      (if them_closed then "" else "not ") ;
    match stream.state with
    | FullyOpen ->
        (not us_closed) && not them_closed
    | HalfClosed `Us ->
        us_closed && not them_closed
    | HalfClosed `Them ->
        (not us_closed) && them_closed
    | FullyClosed ->
        us_closed && them_closed

  (** Advance the stream_state automata, closing pipes as necessary. This
      executes atomically, using a bool + condition variable to synchronize
      updates. *)
  let advance_stream_state net (stream : stream) who_closed =
    let name_participant = function
      | `Us ->
          "the local host"
      | `Them ->
          "the remote host"
    in
    let rec acquire_lock () =
      if not stream.state_lock then (
        stream.state_lock <- true ;
        Deferred.unit )
      else
        let%bind () = Async.Condition.wait stream.state_wait in
        acquire_lock ()
    in
    let%bind () = acquire_lock () in
    let old_state = stream.state in
    Monitor.protect
      ~finally:(fun () ->
        stream.state_lock <- false ;
        Async.Condition.signal stream.state_wait () ;
        Deferred.unit)
      (fun () ->
        let%map () =
          match who_closed with
          | `Us ->
              (* FIXME related to https://github.com/libp2p/go-libp2p-circuit/issues/18
                 "preemptive" or half-closing a stream doesn't actually seem supported:
                 after closing it we can't read anymore.*)
              (*
              match%map
                do_rpc net (module Rpcs.Close_stream) {stream_idx= stream.idx}
              with
              | Ok "closeStream success" ->
                  ()
              | Ok v ->
                  failwithf "helper broke RPC protocol: closeStream got %s" v
                    ()
              | Error e ->
                  Error.raise e )
              *)
              Deferred.unit
          | `Them ->
              (* Helper notified us that the Go side closed its write pipe. *)
              Pipe.close stream.incoming_w ;
              Deferred.unit
        in
        let double_close () =
          [%log' error net.logger]
            "stream with index $index closed twice by $party"
            ~metadata:
              [ ("index", `Int stream.idx)
              ; ("party", `String (name_participant who_closed))
              ] ;
          stream.state
        in
        let release () =
          match Hashtbl.find_and_remove net.streams stream.idx with
          | Some _ ->
              ()
          | None ->
              [%log' error net.logger]
                "tried to release stream $idx but it was already gone"
                ~metadata:[ ("idx", `Int stream.idx) ]
        in
        stream.state <-
          ( match old_state with
          | FullyOpen ->
              HalfClosed who_closed
          | HalfClosed other ->
              if [%equal: [ `Us | `Them ]] other who_closed then
                ignore (double_close () : stream_state)
              else release () ;
              FullyClosed
          | FullyClosed ->
              double_close () ) ;
        (* TODO: maybe we can check some invariants on the Go side too? *)
        if not (stream_state_invariant stream net.logger) then
          [%log' error net.logger]
            "after $who_closed closed the stream, stream state invariant broke \
             (previous state: $old_stream_state)"
            ~metadata:
              [ ("who_closed", `String (name_participant who_closed))
              ; ("old_stream_state", `String (show_stream_state old_state))
              ])

  (** Track a new stream.

      This is used for both newly created outbound streams and incoming streams, and
      spawns the task that sends outbound messages to the helper.

      Our writing end of the stream will be automatically be closed once the
      write pipe is closed.
  *)
  let make_stream net idx protocol remote_peer_info =
    let incoming_r, incoming_w = Pipe.create () in
    let outgoing_r, outgoing_w = Pipe.create () in
    let peer =
      Peer.create
        (Unix.Inet_addr.of_string remote_peer_info.host)
        ~libp2p_port:remote_peer_info.libp2p_port
        ~peer_id:(Peer.Id.unsafe_of_string remote_peer_info.peer_id)
    in
    let stream =
      { net
      ; idx
      ; state = FullyOpen
      ; state_lock = false
      ; state_wait = Async.Condition.create ()
      ; peer
      ; protocol
      ; incoming_r
      ; incoming_w
      ; outgoing_r
      ; outgoing_w
      }
    in
    let outgoing_loop () =
      let%bind () =
        Pipe.iter outgoing_r ~f:(fun msg ->
            match%map
              do_rpc net
                (module Rpcs.Send_stream_msg)
                { stream_idx = idx; data = to_b64_data msg }
            with
            | Ok "sendStreamMsg success" ->
                ()
            | Ok v ->
                failwithf "helper broke RPC protocol: sendStreamMsg got %s" v ()
            | Error e ->
                [%log' error net.logger]
                  "error sending message on stream $idx: $error"
                  ~metadata:
                    [ ("idx", `Int idx)
                    ; ("error", Error_json.error_to_yojson e)
                    ] ;
                Pipe.close outgoing_w)
      in
      advance_stream_state net stream `Us
    in
    don't_wait_for (outgoing_loop ()) ;
    stream

  (** Parses a normal RPC response and resolves the deferred it answers. *)
  let handle_response t v =
    let open Yojson.Safe.Util in
    let open Or_error.Let_syntax in
    let%bind seq = v |> member "seqno" |> to_int_res in
    let err = v |> member "error" in
    let res = v |> member "success" in
    if not (Int.equal seq 0) then
      let fill_result =
        match (err, res) with
        | `Null, r ->
            Ok r
        | e, `Null ->
            Or_error.errorf "RPC #%d failed: %s" seq (Yojson.Safe.to_string e)
        | _, _ ->
            Or_error.errorf "unexpected response to RPC #%d: %s" seq
              (Yojson.Safe.to_string v)
      in
      match Hashtbl.find_and_remove t.outstanding_requests seq with
      | Some ivar ->
          (* This fill should be okay because we "found and removed" the request *)
          if Ivar.is_full ivar then
            [%log' error t.logger] "Ivar.fill bug is here!" ;
          Ivar.fill ivar fill_result ;
          Ok ()
      | None ->
          Or_error.errorf "spurious reply to RPC #%d: %s" seq
            (Yojson.Safe.to_string v)
    else (
      [%log' error t.logger] "important info from helper: %s"
        (Yojson.Safe.to_string err) ;
      Ok () )

  (** Parses an "upcall" and performs it.

      An upcall is like an RPC from the helper to us.*)

  module Upcall = struct
    module Publish = struct
      type t =
        { upcall : string
        ; subscription_idx : int
        ; sender : peer_info option
        ; data : Data.t
        }
      [@@deriving yojson]
    end

    module Validate = struct
      type t =
        { sender : peer_info option
        ; data : Data.t
        ; expiration : int64
        ; seqno : int
        ; upcall : string
        ; subscription_idx : int
        }
      [@@deriving yojson]
    end

    module Stream_lost = struct
      type t = { upcall : string; stream_idx : int; reason : string }
      [@@deriving yojson]
    end

    module Stream_read_complete = struct
      type t = { upcall : string; stream_idx : int } [@@deriving yojson]
    end

    module Incoming_stream_msg = struct
      type t = { upcall : string; stream_idx : int; data : Data.t }
      [@@deriving yojson]
    end

    module Incoming_stream = struct
      type t =
        { upcall : string
        ; peer : peer_info
        ; stream_idx : int
        ; protocol : string
        }
      [@@deriving yojson]
    end

    module Peer_connected = struct
      type t = { upcall : string; peer_id : string } [@@deriving yojson]
    end

    module Peer_disconnected = struct
      type t = { upcall : string; peer_id : string } [@@deriving yojson]
    end

    let or_error (t : ('a, string) Result.t) =
      match t with
      | Ok a ->
          Ok a
      | Error s ->
          Or_error.errorf !"Error converting from json: %s" s
  end

  let lookup_peerid net peer_id =
    match%map do_rpc net (module Rpcs.Find_peer) { peer_id } with
    | Ok peer_info ->
        Ok
          (Peer.create
             (Unix.Inet_addr.of_string peer_info.host)
             ~libp2p_port:peer_info.libp2p_port ~peer_id:peer_info.peer_id)
    | Error e ->
        Error e

  let handle_upcall t v =
    let open Yojson.Safe.Util in
    let open Or_error.Let_syntax in
    let open Upcall in
    let wrap sender data =
      match sender with
      | Some sender ->
          if
            String.equal sender.host "127.0.0.1"
            && Int.equal sender.libp2p_port 0
          then Envelope.Incoming.local data
          else
            Envelope.Incoming.wrap_peer ~sender:(peer_of_peer_info sender) ~data
      | None ->
          Envelope.Incoming.local data
    in
    match member "upcall" v |> to_string with
    (* Message published on one of our subscriptions *)
    | "publish" -> (
        let%bind m = Publish.of_yojson v |> or_error in
        let _me =
          Ivar.peek t.me_keypair
          |> Option.value_exn
               ~message:
                 "How did we receive pubsub before configuring our keypair?"
        in
        (*if
          Option.fold m.sender ~init:false ~f:(fun _ sender ->
              Peer.Id.equal sender.peer_id me.peer_id )
          then (
          [%log trace]
            "not handling published message originated from me";
          (* elide messages that we sent *) return () )
          else*)
        let idx = m.subscription_idx in
        let data = m.data in
        match Hashtbl.find t.subscriptions idx with
        | Some sub ->
            if not sub.closed then (
              let raw_data = Data.to_string data in
              let decoded = sub.decode raw_data in
              match decoded with
              | Ok data ->
                  (* TAKE CARE: doing anything with the return
                     value here except ignore is UNSOUND because
                     write_pipe has a cast type. We don't remember
                     what the original 'return was. *)
                  if Strict_pipe.Writer.is_closed sub.write_pipe then
                    [%log' error t.logger]
                      "subscription writer for $topic unexpectedly closed. \
                       dropping message."
                      ~metadata:[ ("topic", `String sub.topic) ]
                  else
                    ignore
                      ( Strict_pipe.Writer.write sub.write_pipe
                          (wrap m.sender data)
                        : unit Deferred.t )
              | Error e ->
                  ( match sub.on_decode_failure with
                  | `Ignore ->
                      ()
                  | `Call f ->
                      f (wrap m.sender raw_data) e ) ;
                  [%log' error t.logger]
                    "failed to decode message published on subscription $topic \
                     ($idx): $error"
                    ~metadata:
                      [ ("topic", `String sub.topic)
                      ; ("idx", `Int idx)
                      ; ("error", Error_json.error_to_yojson e)
                      ] ;
                  ()
              (* TODO: add sender to Publish.t and include it here. *)
              (* TODO: think about exposing the PeerID of the originator as well? *)
              )
            else
              [%log' debug t.logger]
                "received msg for subscription $sub after unsubscribe, was it \
                 still in the stdout pipe?"
                ~metadata:[ ("sub", `Int idx) ] ;
            Ok ()
        | None ->
            Or_error.errorf "message published with inactive subsubscription %d"
              idx )
    (* Validate a message received on a subscription *)
    | "validate" -> (
        let%bind m = Validate.of_yojson v |> or_error in
        let idx = m.subscription_idx in
        let seqno = m.seqno in
        match Hashtbl.find t.subscriptions idx with
        | Some sub ->
            (let open Deferred.Let_syntax in
            let raw_data = Data.to_string m.data in
            let decoded = sub.decode raw_data in
            let%bind action_opt =
              match decoded with
              | Ok data ->
                  let expiration_time =
                    Int63.of_int64_exn m.expiration
                    |> Time_ns.Span.of_int63_ns |> Time_ns.of_span_since_epoch
                  in
                  let validation_callback =
                    Validation_callback.create expiration_time
                  in
                  let%bind () =
                    sub.validator (wrap m.sender data) validation_callback
                  in
                  Validation_callback.await validation_callback
              | Error e ->
                  ( match sub.on_decode_failure with
                  | `Ignore ->
                      ()
                  | `Call f ->
                      f (wrap m.sender raw_data) e ) ;
                  [%log' error t.logger]
                    "failed to decode message published on subscription $topic \
                     ($idx): $error"
                    ~metadata:
                      [ ("topic", `String sub.topic)
                      ; ("idx", `Int idx)
                      ; ("error", Error_json.error_to_yojson e)
                      ] ;
                  return (Some `Reject)
            in
            match action_opt with
            | None ->
                [%log' warn t.logger]
                  "validation callback timed out before we could respond" ;
                Deferred.unit
            | Some action -> (
                match%map
                  do_rpc t
                    (module Rpcs.Validation_complete)
                    { seqno
                    ; is_valid =
                        ( match action with
                        | `Accept ->
                            "accept"
                        | `Reject ->
                            "reject"
                        | `Ignore ->
                            "ignore" )
                    }
                with
                | Ok "validationComplete success" ->
                    ()
                | Ok v ->
                    failwithf
                      "helper broke RPC protocol: validationComplete got %s" v
                      ()
                | Error e ->
                    [%log' error t.logger]
                      "error during validationComplete, ignoring and \
                       continuing: $error"
                      ~metadata:[ ("error", Error_json.error_to_yojson e) ] ))
            |> don't_wait_for ;
            Ok ()
        | None ->
            Or_error.errorf
              "asked to validate message for unregistered subscription idx %d"
              idx )
    (* A new inbound stream was opened *)
    | "incomingStream" -> (
        let%bind m = Incoming_stream.of_yojson v |> or_error in
        let stream_idx = m.stream_idx in
        let protocol = m.protocol in
        Option.iter t.all_peers_seen ~f:(fun all_peers_seen ->
            let all_peers_seen =
              Set.add all_peers_seen
                { libp2p_port = m.peer.libp2p_port; host = m.peer.host }
            in
            t.all_peers_seen <- Some all_peers_seen ;
            Mina_metrics.(
              Gauge.set Network.all_peers
                (Set.length all_peers_seen |> Int.to_float))) ;
        let stream = make_stream t stream_idx protocol m.peer in
        match Hashtbl.find t.protocol_handlers protocol with
        | Some ph ->
            if not ph.closed then (
              Hashtbl.add_exn t.streams ~key:stream_idx ~data:stream ;
              don't_wait_for
                (let open Deferred.Let_syntax in
                (* Call the protocol handler. If it throws an exception,
                   handle it according to [on_handler_error]. Mimics
                   [Tcp.Server.create]. See [handle_protocol] doc comment.
                *)
                match%map
                  Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
                      ph.f stream)
                with
                | Ok () ->
                    ()
                | Error e -> (
                    try
                      match ph.on_handler_error with
                      | `Raise ->
                          raise e
                      | `Ignore ->
                          ()
                      | `Call f ->
                          f stream e
                    with handler_exn ->
                      ph.closed <- true ;
                      don't_wait_for
                        ( do_rpc t
                            (module Rpcs.Remove_stream_handler)
                            { protocol }
                        >>| fun _ -> Hashtbl.remove t.protocol_handlers protocol
                        ) ;
                      raise handler_exn )) ;
              Ok () )
            else
              (* silently ignore new streams for closed protocol handlers.
                 these are buffered stream open RPCs that were enqueued before
                 our close went into effect. *)
              (* TODO: we leak the new pipes here*)
              Ok ()
        | None ->
            (* TODO: punish *)
            Or_error.errorf "incoming stream for protocol we don't know about?"
        )
    | "peerConnected" ->
        let%map p = Peer_connected.of_yojson v |> or_error in
        Option.iter t.peer_connected_callback ~f:(fun cb -> cb p.peer_id)
    | "peerDisconnected" ->
        let%map p = Peer_disconnected.of_yojson v |> or_error in
        Option.iter t.peer_disconnected_callback ~f:(fun cb -> cb p.peer_id)
    (* Received a message on some stream *)
    | "incomingStreamMsg" -> (
        let%bind m = Incoming_stream_msg.of_yojson v |> or_error in
        match Hashtbl.find t.streams m.stream_idx with
        | Some { incoming_w; _ } ->
            don't_wait_for
              (Pipe.write_if_open incoming_w (Data.to_string m.data)) ;
            Ok ()
        | None ->
            Or_error.errorf
              "incoming stream message for stream we don't know about?" )
    (* Stream was reset, either by the remote peer or an error on our end. *)
    | "streamLost" ->
        let%bind m = Stream_lost.of_yojson v |> or_error in
        let stream_idx = m.stream_idx in
        [%log' trace t.logger]
          "Encountered error while reading stream $idx: $error"
          ~metadata:[ ("error", `String m.reason); ("idx", `Int stream_idx) ] ;
        Ok ()
    (* The remote peer closed its write end of one of our streams *)
    | "streamReadComplete" -> (
        let%bind m = Stream_read_complete.of_yojson v |> or_error in
        let stream_idx = m.stream_idx in
        match Hashtbl.find t.streams stream_idx with
        | Some stream ->
            advance_stream_state t stream `Them |> don't_wait_for ;
            Ok ()
        | None ->
            Or_error.errorf
              "streamReadComplete for stream we don't know about %d" stream_idx
        )
    | s ->
        Or_error.errorf "unknown upcall %s" s
end
(* Warning 30 is about field labels being defined in multiple types.
   It means more disambiguation has to happen sometimes but it doesn't
   seem to be a big deal. *)
[@warning "-30"]

type net = Helper.t

module Keypair = struct
  include Keypair0

  let random net =
    match%map Helper.do_rpc net (module Helper.Rpcs.Generate_keypair) () with
    | Ok { sk; pk; peer_id } ->
        (let open Or_error.Let_syntax in
        let%bind secret = of_b64_data (`String sk) in
        let%map public = of_b64_data (`String pk) in
        ({ secret; public; peer_id = Peer.Id.unsafe_of_string peer_id } : t))
        |> Or_error.ok_exn
    | Error e ->
        Error.tag e ~tag:"Other RPC error generateKeypair" |> Error.raise

  let secret_key_base64 ({ secret; _ } : t) = to_b64_data secret

  let to_string ({ secret; public; peer_id } : t) =
    String.concat ~sep:","
      [ to_b64_data secret; to_b64_data public; Peer.Id.to_string peer_id ]

  let of_string s =
    let parse_with_sep sep =
      match String.split s ~on:sep with
      | [ secret_b64; public_b64; peer_id ] ->
          let open Or_error.Let_syntax in
          let%map secret = of_b64_data (`String secret_b64)
          and public = of_b64_data (`String public_b64) in
          ({ secret; public; peer_id = Peer.Id.unsafe_of_string peer_id } : t)
      | _ ->
          Or_error.errorf "%s is not a valid Keypair.to_string output" s
    in
    let with_semicolon = parse_with_sep ';' in
    let with_comma = parse_with_sep ',' in
    if Or_error.is_error with_semicolon then with_comma else with_semicolon

  let to_peer_id ({ peer_id; _ } : t) = peer_id
end

module Multiaddr = struct
  type t = string [@@deriving compare, bin_io_unversioned]

  let to_string t = t

  let of_string t = t

  let to_peer t =
    match String.split ~on:'/' t with
    | [ ""; "ip4"; ip4_str; "tcp"; tcp_str; "p2p"; peer_id ] -> (
        try
          let host = Unix.Inet_addr.of_string ip4_str in
          let libp2p_port = Int.of_string tcp_str in
          Some (Network_peer.Peer.create host ~libp2p_port ~peer_id)
        with _ -> None )
    | _ ->
        None

  let valid_as_peer t =
    match String.split ~on:'/' t with
    | [ ""; protocol; _; "tcp"; _; "p2p"; _ ]
      when List.mem
             [ "ip4"; "ip6"; "dns4"; "dns6" ]
             protocol ~equal:String.equal ->
        true
    | _ ->
        false

  let of_file_contents ~(contents : string) : t list =
    String.split ~on:'\n' contents
    |> List.filter ~f:(fun s ->
           if valid_as_peer s then true
           else if String.is_empty s then false
           else (
             [%log' error (Logger.create ())]
               "Invalid peer $peer found in peers list"
               ~metadata:[ ("peer", `String s) ] ;
             false ))
end

type discovered_peer = { id : Peer.Id.t; maddrs : Multiaddr.t list }

module Pubsub = struct
  let publish net ~topic ~data =
    match%map
      Helper.do_rpc net
        (module Helper.Rpcs.Publish)
        { topic; data = Helper.Data.pack_data data }
    with
    | Ok "publish success" ->
        ()
    | Ok v ->
        failwithf "helper broke RPC protocol: publish got %s" v ()
    | Error e ->
        [%log' error net.logger]
          "error while publishing message on $topic: $err"
          ~metadata:
            [ ("topic", `String topic); ("err", Error_json.error_to_yojson e) ]

  module Subscription = struct
    type 'a t = 'a Helper.subscription =
      { net : Helper.t
      ; topic : string
      ; idx : int
      ; mutable closed : bool
      ; validator :
          'a Envelope.Incoming.t -> Validation_callback.t -> unit Deferred.t
      ; encode : 'a -> string
      ; on_decode_failure :
          [ `Ignore | `Call of string Envelope.Incoming.t -> Error.t -> unit ]
      ; decode : string -> 'a Or_error.t
      ; write_pipe :
          ( 'a Envelope.Incoming.t
          , Strict_pipe.synchronous
          , unit Deferred.t )
          Strict_pipe.Writer.t
      ; read_pipe : 'a Envelope.Incoming.t Strict_pipe.Reader.t
      }

    let publish { net; topic; encode; _ } message =
      publish net ~topic ~data:(encode message)

    let unsubscribe ({ net; idx; write_pipe; _ } as t) =
      if not t.closed then (
        t.closed <- true ;
        Strict_pipe.Writer.close write_pipe ;
        match%map
          Helper.do_rpc net
            (module Helper.Rpcs.Unsubscribe)
            { subscription_idx = idx }
        with
        | Ok "unsubscribe success" ->
            Ok ()
        | Ok v ->
            failwithf "helper broke RPC protocol: unsubscribe got %s" v ()
        | Error e ->
            Error e )
      else Deferred.Or_error.error_string "already unsubscribed"

    let message_pipe { read_pipe; _ } = read_pipe
  end

  let subscribe_raw (net : net) (topic : string) ~should_forward_message ~encode
      ~decode ~on_decode_failure =
    let subscription_idx = Helper.genseq net in
    let read_pipe, write_pipe =
      Strict_pipe.(
        create ~name:(sprintf "subscription to topic «%s»" topic) Synchronous)
    in
    let sub =
      { Subscription.net
      ; topic
      ; idx = subscription_idx
      ; closed = false
      ; encode
      ; on_decode_failure
      ; decode
      ; validator = should_forward_message
      ; write_pipe
      ; read_pipe
      }
    in
    (* Linear scan over all subscriptions. Should generally be small, probably not a problem. *)
    let already_exists_error =
      Hashtbl.fold net.subscriptions ~init:None ~f:(fun ~key:_ ~data acc ->
          if Option.is_some acc then acc
          else if String.equal data.topic topic then (
            Strict_pipe.Writer.close write_pipe ;
            Some (Or_error.errorf "already subscribed to topic %s" topic) )
          else acc)
    in
    match already_exists_error with
    | Some err ->
        return err
    | None -> (
        let%bind _ =
          match
            Hashtbl.add net.subscriptions ~key:subscription_idx
              ~data:(Obj.magic sub : erased_magic Subscription.t)
          with
          | `Ok ->
              return (Ok ())
          | `Duplicate ->
              failwith "fresh genseq was already present in subscription table?"
        in
        match%map
          Helper.do_rpc net
            (module Helper.Rpcs.Subscribe)
            { topic; subscription_idx }
        with
        | Ok "subscribe success" ->
            Ok sub
        | Ok j ->
            Strict_pipe.Writer.close write_pipe ;
            failwithf "helper broke RPC protocol: subscribe got %s" j ()
        | Error e ->
            Strict_pipe.Writer.close write_pipe ;
            Error e )

  let subscribe_encode net topic ~should_forward_message ~bin_prot
      ~on_decode_failure =
    subscribe_raw
      ~decode:(fun msg_str ->
        let b = Bigstring.of_string msg_str in
        Bigstring.read_bin_prot b bin_prot.Bin_prot.Type_class.reader
        |> Or_error.map ~f:fst)
      ~encode:(fun msg ->
        Bin_prot.Utils.bin_dump ~header:true bin_prot.Bin_prot.Type_class.writer
          msg
        |> Bigstring.to_string)
      ~should_forward_message ~on_decode_failure net topic

  let subscribe =
    subscribe_raw ~encode:Fn.id ~decode:Or_error.return
      ~on_decode_failure:`Ignore
end

let me (net : Helper.t) = Ivar.read net.me_keypair

let set_node_status net data =
  match%map Helper.do_rpc net (module Helper.Rpcs.Set_node_status) { data } with
  | Ok "setNodeStatus success" ->
      Ok ()
  | Ok v ->
      failwithf "helper broke RPC protocol: setNodeStatus got %s" v ()
  | Error e ->
      Error e

let get_peer_node_status net peer =
  Helper.do_rpc net
    (module Helper.Rpcs.Get_peer_node_status)
    { peer_multiaddr = Peer.to_multiaddr_string peer }

let list_peers net =
  match%map Helper.do_rpc net (module Helper.Rpcs.List_peers) () with
  | Ok peers ->
      (* FIXME #4039: filter_map shouldn't be necessary *)
      List.filter_map peers ~f:(fun { host; libp2p_port; peer_id } ->
          if Int.equal libp2p_port 0 then None
          else
            Some
              (Peer.create
                 (Unix.Inet_addr.of_string host)
                 ~libp2p_port
                 ~peer_id:(Peer.Id.unsafe_of_string peer_id)))
  | Error error ->
      [%log' error net.logger]
        "Encountered $error while asking libp2p_helper for peers"
        ~metadata:[ ("error", Error_json.error_to_yojson error) ] ;
      []

(* `on_new_peer` fires whenever a peer connects OR disconnects *)
let configure net ~logger:_ ~me ~external_maddr ~maddrs ~network_id
    ~metrics_port ~on_peer_connected ~on_peer_disconnected ~unsafe_no_trust_ip
    ~flooding ~direct_peers ~peer_exchange ~mina_peer_exchange ~seed_peers
    ~initial_gating_config ~max_connections ~validation_queue_size =
  net.Helper.peer_connected_callback <-
    Some (fun peer_id -> on_peer_connected (Peer.Id.unsafe_of_string peer_id)) ;
  net.Helper.peer_disconnected_callback <-
    Some
      (fun peer_id -> on_peer_disconnected (Peer.Id.unsafe_of_string peer_id)) ;
  match%map
    Helper.do_rpc net
      (module Helper.Rpcs.Configure)
      { privk = Keypair.secret_key_base64 me
      ; statedir = net.conf_dir
      ; ifaces = List.map ~f:Multiaddr.to_string maddrs
      ; metrics_port = Option.value metrics_port ~default:""
      ; external_maddr = Multiaddr.to_string external_maddr
      ; network_id
      ; unsafe_no_trust_ip
      ; flood = flooding
      ; direct_peers = List.map ~f:Multiaddr.to_string direct_peers
      ; seed_peers = List.map ~f:Multiaddr.to_string seed_peers
      ; peer_exchange
      ; mina_peer_exchange
      ; max_connections
      ; validation_queue_size
      ; gating_config =
          Helper.gating_config_to_helper_format initial_gating_config
      }
  with
  | Ok "configure success" ->
      Ivar.fill_if_empty net.me_keypair me ;
      Ok ()
  | Ok j ->
      failwithf "helper broke RPC protocol: configure got %s" j ()
  | Error e ->
      Error e

(** List of all peers we are currently connected to. *)
let peers (net : net) = list_peers net

let listen_on net iface =
  match%map Helper.do_rpc net (module Helper.Rpcs.Listen) { iface } with
  | Ok maddrs ->
      Ok maddrs
  | Error e ->
      Error e

let listening_addrs net =
  match%map Helper.do_rpc net (module Helper.Rpcs.Listening_addrs) () with
  | Ok maddrs ->
      Ok maddrs
  | Error e ->
      Error e

(** TODO: graceful shutdown. Reset all our streams, sync the databases, then
    shutdown. Replace kill invocation with an RPC. *)
let shutdown (net : net) =
  net.finished <- true ;
  Deferred.ignore_m (Child_processes.kill net.subprocess)

module Stream = struct
  type t = Helper.stream

  let pipes ({ incoming_r; outgoing_w; _ } : t) = (incoming_r, outgoing_w)

  let reset ({ net; idx; _ } : t) =
    (* NOTE: do not close the pipes here. Reset_stream should end up
       notifying us that streamReadComplete. We can reset the stream (telling
       the remote peer to stop writing) and still be sending data ourselves. *)
    match%map Helper.do_rpc net (module Helper.Rpcs.Reset_stream) { idx } with
    | Ok "resetStream success" ->
        Ok ()
    | Ok v ->
        Or_error.errorf "helper broke RPC protocol: resetStream got %s" v
    | Error e ->
        Error e

  let remote_peer ({ peer; _ } : t) = peer
end

module Protocol_handler = struct
  type t = Helper.protocol_handler

  let handling_protocol ({ protocol_name; _ } : t) = protocol_name

  let is_closed ({ closed; _ } : t) = closed

  let close_connections (net : net) for_protocol =
    Hashtbl.filter_inplace net.streams ~f:(fun stream ->
        if not (String.equal stream.protocol for_protocol) then true
        else (
          don't_wait_for
            (* TODO: this probably needs to be more thorough than a reset. Also force the write pipe closed? *)
            (let%map _ = Stream.reset stream in
             ()) ;
          false ))

  let close ?(reset_existing_streams = false) ({ net; protocol_name; _ } : t) =
    Hashtbl.remove net.protocol_handlers protocol_name ;
    let close_connections =
      if reset_existing_streams then close_connections else fun _ _ -> ()
    in
    match%map
      Helper.do_rpc net
        (module Helper.Rpcs.Remove_stream_handler)
        { protocol = protocol_name }
    with
    | Ok "removeStreamHandler success" ->
        close_connections net protocol_name
    | Ok v ->
        close_connections net protocol_name ;
        failwithf "helper broke RPC protocol: addStreamHandler got %s" v ()
    | Error e ->
        [%log' info net.logger]
          "error while closing handler for $protocol, closing connections \
           anyway: $err"
          ~metadata:
            [ ("protocol", `String protocol_name)
            ; ("err", Error_json.error_to_yojson e)
            ] ;
        close_connections net protocol_name
end

let handle_protocol net ~on_handler_error ~protocol f =
  let ph : Protocol_handler.t =
    { net; closed = false; on_handler_error; f; protocol_name = protocol }
  in
  if Hashtbl.find net.protocol_handlers protocol |> Option.is_some then
    Deferred.Or_error.errorf "already handling protocol %s" protocol
  else
    match%map
      Helper.do_rpc net (module Helper.Rpcs.Add_stream_handler) { protocol }
    with
    | Ok "addStreamHandler success" ->
        Hashtbl.add_exn net.protocol_handlers ~key:protocol ~data:ph ;
        Ok ph
    | Ok v ->
        failwithf "helper broke RPC protocol: addStreamHandler got %s" v ()
    | Error e ->
        Error e

let open_stream net ~protocol peer =
  match%map
    Helper.(
      do_rpc net
        (module Rpcs.Open_stream)
        { peer = Peer.Id.to_string peer; protocol })
  with
  | Ok { stream_idx; peer } ->
      let stream = Helper.make_stream net stream_idx protocol peer in
      Hashtbl.add_exn net.streams ~key:stream_idx ~data:stream ;
      Ok stream
  | Error e ->
      Error e

let add_peer net maddr ~seed =
  match%map
    Helper.(
      do_rpc net
        (module Rpcs.Add_peer)
        { multiaddr = Multiaddr.to_string maddr; seed })
  with
  | Ok "addPeer success" ->
      Ok ()
  | Ok v ->
      failwithf "helper broke RPC protocol: addPeer got %s" v ()
  | Error e ->
      Error e

let begin_advertising net =
  match%map Helper.(do_rpc net (module Rpcs.Begin_advertising) ()) with
  | Ok "beginAdvertising success" ->
      Ok ()
  | Ok v ->
      failwithf "helper broke RPC protocol: beginAdvertising got %s" v ()
  | Error e ->
      Error e

let lookup_peerid = Helper.lookup_peerid

let connection_gating_config net = Deferred.return net.Helper.connection_gating

let set_connection_gating_config net (config : connection_gating) =
  match%map
    Helper.(
      do_rpc net
        (module Rpcs.Set_gater_config)
        (gating_config_to_helper_format config))
  with
  | Ok "ok" ->
      net.connection_gating <- config ;
      config
  | Ok v ->
      failwithf "helper broke RPC protocol: setGatingConfig got %s" v ()
  | Error e ->
      Error.tag e ~tag:"Unexpected error doing setGatingConfig" |> Error.raise

let banned_ips net = Deferred.return net.Helper.banned_ips

let create ~all_peers_seen_metric ~on_unexpected_termination ~logger ~pids
    ~conf_dir =
  let outstanding_requests = Hashtbl.create (module Int) in
  let termination_hack_ref : Helper.t option ref = ref None in
  match%bind
    Child_processes.start_custom ~logger ~name:"libp2p_helper"
      ~git_root_relative_path:"src/app/libp2p_helper/result/bin/libp2p_helper"
      ~conf_dir ~args:[]
      ~stdout:(`Log Logger.Level.Spam, `Pipe, `Filter_empty)
      ~stderr:(`Don't_log, `Pipe, `Filter_empty)
      ~termination:
        (`Handler
          (fun ~killed process e ->
            Hashtbl.iter outstanding_requests ~f:(fun iv ->
                Ivar.fill_if_empty iv
                  (Or_error.error_string
                     "libp2p_helper process died before answering")) ;
            Hashtbl.clear outstanding_requests ;
            Child_processes.Termination.remove pids (Process.pid process) ;
            if
              (not killed)
              && not
                   (Option.value_map ~default:false
                      ~f:(fun t -> t.finished)
                      !termination_hack_ref)
            then (
              match e with
              | Ok ((Error (`Exit_non_zero _) | Error (`Signal _)) as e) ->
                  [%log fatal]
                    !"libp2p_helper process died unexpectedly: $exit_status"
                    ~metadata:
                      [ ( "exit_status"
                        , `String (Unix.Exit_or_signal.to_string_hum e) )
                      ] ;
                  Option.iter !termination_hack_ref ~f:(fun t ->
                      t.finished <- true) ;
                  on_unexpected_termination ()
              | Error err ->
                  [%log fatal]
                    !"Child processes library could not track libp2p_helper \
                      process: $err"
                    ~metadata:[ ("err", Error_json.error_to_yojson err) ] ;
                  Option.iter !termination_hack_ref ~f:(fun t ->
                      t.finished <- true) ;
                  let%bind () =
                    match !termination_hack_ref with
                    | Some { subprocess; _ } ->
                        Deferred.ignore_m (Child_processes.kill subprocess)
                    | None ->
                        Deferred.unit
                  in
                  on_unexpected_termination ()
              | Ok (Ok ()) ->
                  [%log error]
                    "libp2p helper process exited peacefully but it should \
                     have been killed by shutdown!" ;
                  Deferred.unit )
            else
              let exit_status =
                match e with
                | Ok e ->
                    `String (Unix.Exit_or_signal.to_string_hum e)
                | Error err ->
                    Error_json.error_to_yojson err
              in
              [%log info]
                !"libp2p_helper process killed successfully: $exit_status"
                ~metadata:[ ("exit_status", exit_status) ] ;
              Deferred.unit))
  with
  | Error e ->
      Deferred.Or_error.fail
        (Error.tag e
           ~tag:
             "Could not start libp2p_helper. If you are a dev, did you forget \
              to `make libp2p_helper` and set MINA_LIBP2P_HELPER_PATH? Try \
              MINA_LIBP2P_HELPER_PATH=$PWD/src/app/libp2p_helper/result/bin/libp2p_helper.")
  | Ok subprocess ->
      Child_processes.register_process pids subprocess Libp2p_helper ;
      let t : Helper.t =
        { subprocess
        ; conf_dir
        ; logger
        ; banned_ips = []
        ; connection_gating =
            { banned_peers = []; trusted_peers = []; isolate = false }
        ; me_keypair = Ivar.create ()
        ; outstanding_requests
        ; subscriptions = Hashtbl.create (module Int)
        ; streams = Hashtbl.create (module Int)
        ; all_peers_seen =
            (if all_peers_seen_metric then Some Peers_no_ids.Set.empty else None)
        ; peer_connected_callback = None
        ; peer_disconnected_callback = None
        ; protocol_handlers = Hashtbl.create (module String)
        ; seqno = 1
        ; finished = false
        }
      in
      termination_hack_ref := Some t ;
      Strict_pipe.Reader.iter (Child_processes.stderr_lines subprocess)
        ~f:(fun line ->
          ( match
              Or_error.try_with (fun () -> Yojson.Safe.from_string line)
              |> Or_error.map ~f:Go_log.record_of_yojson
            with
          | Ok (Ok record) -> (
              let r = Go_log.(record_to_message record) in
              try Logger.raw logger r
              with _exn ->
                Logger.raw logger
                  { r with
                    message =
                      "(go log message was not valid for logger; see $line)"
                  ; metadata = String.Map.singleton "line" (`String r.message)
                  } )
          | Error err ->
              [%log error]
                ~metadata:
                  [ ("line", `String line)
                  ; ("error", Error_json.error_to_yojson err)
                  ]
                "failed to parse log line $line from helper stderr as json"
          | Ok (Error err) ->
              [%log debug]
                ~metadata:[ ("line", `String line); ("error", `String err) ]
                "failed to parse log line $line from helper stderr" ) ;
          Deferred.unit)
      |> don't_wait_for ;
      Strict_pipe.Reader.iter (Child_processes.stdout_lines subprocess)
        ~f:(fun line ->
          let open Yojson.Safe.Util in
          let v = Or_error.try_with (fun () -> Yojson.Safe.from_string line) in
          ( match
              Or_error.map v ~f:(fun v ->
                  if Yojson.Safe.equal (member "upcall" v) `Null then
                    Helper.handle_response t v
                  else Helper.handle_upcall t v)
            with
          | Ok (Ok ()) ->
              ()
          | Error err ->
              [%log error]
                ~metadata:
                  [ ("line", `String line)
                  ; ("error", Error_json.error_to_yojson err)
                  ]
                "failed to parse log line $line from helper stderr as json"
          | Ok (Error e) ->
              [%log error] "handling line from helper failed! $err"
                ~metadata:
                  [ ("line", `String line)
                  ; ("err", Error_json.error_to_yojson e)
                  ] ) ;
          Deferred.unit)
      |> don't_wait_for ;
      ( if all_peers_seen_metric then
        let log_all_peers_interval = Time.Span.of_hr 2.0 in
        let log_message_batch_size = 50 in
        every log_all_peers_interval (fun () ->
            Option.iter t.all_peers_seen ~f:(fun all_peers_seen ->
                let num_batches, num_in_batch, batches, batch =
                  Set.fold_right all_peers_seen ~init:(0, 0, [], [])
                    ~f:(fun peer (num_batches, num_in_batch, batches, batch) ->
                      if num_in_batch >= log_message_batch_size then
                        (num_batches + 1, 1, batch :: batches, [ peer ])
                      else
                        (num_batches, num_in_batch + 1, batches, peer :: batch))
                in
                let num_batches, batches =
                  if num_in_batch > 0 then (num_batches + 1, batch :: batches)
                  else (num_batches, batches)
                in
                List.iteri batches ~f:(fun batch_num batch ->
                    [%log info]
                      "All peers seen by this node, batch \
                       $batch_num/$num_batches"
                      ~metadata:
                        [ ("batch_num", `Int batch_num)
                        ; ("num_batches", `Int num_batches)
                        ; ( "peers"
                          , `List (List.map ~f:Peers_no_ids.to_yojson batch) )
                        ]))) ) ;
      Deferred.Or_error.return t

let%test_module "coda network tests" =
  ( module struct
    let logger = Logger.create ()

    let testmsg =
      "This is a test. This is a test of the Outdoor Warning System. This is \
       only a test."

    let pids = Child_processes.Termination.create_pid_table ()

    let setup_two_nodes network_id =
      let%bind a_tmp = Unix.mkdtemp "p2p_helper_test_a" in
      let%bind b_tmp = Unix.mkdtemp "p2p_helper_test_b" in
      let%bind c_tmp = Unix.mkdtemp "p2p_helper_test_c" in
      let%bind a =
        create ~all_peers_seen_metric:false
          ~logger:(Logger.extend logger [ ("name", `String "a") ])
          ~conf_dir:a_tmp ~pids
          ~on_unexpected_termination:(fun () ->
            raise Child_processes.Child_died)
        >>| Or_error.ok_exn
      in
      let%bind b =
        create ~all_peers_seen_metric:false
          ~logger:(Logger.extend logger [ ("name", `String "b") ])
          ~conf_dir:b_tmp ~pids
          ~on_unexpected_termination:(fun () ->
            raise Child_processes.Child_died)
        >>| Or_error.ok_exn
      in
      let%bind c =
        create ~all_peers_seen_metric:false
          ~logger:(Logger.extend logger [ ("name", `String "c") ])
          ~conf_dir:c_tmp ~pids
          ~on_unexpected_termination:(fun () ->
            raise Child_processes.Child_died)
        >>| Or_error.ok_exn
      in
      let%bind kp_a = Keypair.random a in
      let%bind kp_b = Keypair.random b in
      let%bind kp_c = Keypair.random c in
      let maddrs = [ "/ip4/127.0.0.1/tcp/0" ] in
      let%bind () =
        configure a ~logger ~external_maddr:(List.hd_exn maddrs) ~me:kp_a
          ~maddrs ~network_id ~peer_exchange:true ~mina_peer_exchange:true
          ~direct_peers:[] ~seed_peers:[] ~on_peer_connected:Fn.ignore
          ~on_peer_disconnected:Fn.ignore ~flooding:false ~metrics_port:None
          ~unsafe_no_trust_ip:true ~max_connections:50
          ~validation_queue_size:150
          ~initial_gating_config:
            { trusted_peers = []; banned_peers = []; isolate = false }
        >>| Or_error.ok_exn
      in
      let%bind raw_seed_peers = listening_addrs a >>| Or_error.ok_exn in
      let seed_peer =
        Printf.sprintf "%s/p2p/%s"
          (List.hd_exn raw_seed_peers)
          (Keypair.to_peer_id kp_a)
      in
      [%log error] ~metadata:[ ("peer", `String seed_peer) ] "Seed_peer: $peer" ;
      let%bind () =
        configure b ~logger ~external_maddr:(List.hd_exn maddrs) ~me:kp_b
          ~maddrs ~network_id ~peer_exchange:true ~mina_peer_exchange:true
          ~direct_peers:[] ~seed_peers:[ seed_peer ]
          ~on_peer_connected:Fn.ignore ~on_peer_disconnected:Fn.ignore
          ~flooding:false ~metrics_port:None ~unsafe_no_trust_ip:true
          ~max_connections:50 ~validation_queue_size:150
          ~initial_gating_config:
            { trusted_peers = []; banned_peers = []; isolate = false }
        >>| Or_error.ok_exn
      and () =
        configure c ~logger ~external_maddr:(List.hd_exn maddrs) ~me:kp_c
          ~maddrs ~network_id ~peer_exchange:true ~mina_peer_exchange:true
          ~direct_peers:[] ~seed_peers:[ seed_peer ]
          ~on_peer_connected:Fn.ignore ~on_peer_disconnected:Fn.ignore
          ~flooding:false ~metrics_port:None ~unsafe_no_trust_ip:true
          ~max_connections:50 ~validation_queue_size:150
          ~initial_gating_config:
            { trusted_peers = []; banned_peers = []; isolate = false }
        >>| Or_error.ok_exn
      in
      let%bind b_advert = begin_advertising b in
      Or_error.ok_exn b_advert ;
      let%bind c_advert = begin_advertising c in
      Or_error.ok_exn c_advert ;
      (* Give the helpers time to announce and discover each other on localhost *)
      let%map () = after (Time.Span.of_sec 2.5) in
      let shutdown () =
        let%bind () = shutdown a in
        let%bind () = shutdown b in
        let%bind () = shutdown c in
        let%bind () = File_system.remove_dir a_tmp in
        let%bind () = File_system.remove_dir b_tmp in
        File_system.remove_dir c_tmp
      in
      (b, c, shutdown)

    (*
    let%test_unit "does_b_see_c" =
      let () = Core.Backtrace.elide := false in
      ignore testmsg ;
      let test_def =
        let open Deferred.Let_syntax in
        let%bind b, c, shutdown = setup_two_nodes "test_stream" in
        let%bind b_peers = peers b in
        let%bind c_peerid = me c >>| Keypair.to_peer_id in
        assert (
          b_peers
          |> List.map ~f:(fun p -> p.Peer.peer_id)
          |> fun l -> List.mem l c_peerid ~equal:Peer.Id.equal ) ;
        let%bind c_peers = peers c in
        let%bind b_peerid = me b >>| Keypair.to_peer_id in
        assert (
          c_peers
          |> List.map ~f:(fun p -> p.Peer.peer_id)
          |> fun l -> List.mem l b_peerid ~equal:Peer.Id.equal ) ;
        shutdown ()
      in
      Async.Thread_safe.block_on_async_exn (fun () -> test_def)

    let%test_unit "b_stream_c" =
      let () = Core.Backtrace.elide := false in
      ignore testmsg ;
      let test_def =
        let open Deferred.Let_syntax in
        let%bind b, c, shutdown = setup_two_nodes "test_stream" in
        let%bind b_peerid = me b >>| Keypair.to_peer_id in
        let handler_finished = ref false in
        let%bind _echo_handler =
          handle_protocol b ~on_handler_error:`Raise ~protocol:"read_bytes"
            (fun stream ->
              let r, w = Stream.pipes stream in
              let rec go i =
                if i = 0 then return ()
                else
                  let%bind _s =
                    match%map Pipe.read' ~max_queue_length:1 r with
                    | `Eof ->
                        failwith "Eof"
                    | `Ok q ->
                        Base.Queue.peek_exn q
                  in
                  go (i - 1)
              in
              let%map () = go 1000 in
              Pipe.write_without_pushback w "done" ;
              Pipe.close w ;
              handler_finished := true )
          |> Deferred.Or_error.ok_exn
        in
        let%bind stream =
          open_stream c ~protocol:"read_bytes" b_peerid >>| Or_error.ok_exn
        in
        let r, w = Stream.pipes stream in
        for i = 0 to 999 do
          Pipe.write_without_pushback w (Printf.sprintf "%d" i)
        done ;
        Pipe.close w ;
        (* HACK: let our messages send before we reset.
           It would be more principled to add flushing to
           the stream interface. *)
        let%bind () = after (Time.Span.of_sec 5.) in
        let%bind _ = Stream.reset stream in
        let%bind _msgs = Pipe.read_all r in
        assert !handler_finished ;
        shutdown ()
      in
      Async.Thread_safe.block_on_async_exn (fun () -> test_def)
  *)

    let%test_unit "stream" =
      let () = Core.Backtrace.elide := false in
      let test_def =
        let open Deferred.Let_syntax in
        let%bind b, c, shutdown = setup_two_nodes "test_stream" in
        let%bind b_peerid = me b >>| Keypair.to_peer_id in
        let handler_finished = ref false in
        let%bind echo_handler =
          handle_protocol b ~on_handler_error:`Raise ~protocol:"echo"
            (fun stream ->
              let r, w = Stream.pipes stream in
              let%map () = Pipe.transfer r w ~f:Fn.id in
              Pipe.close w ;
              handler_finished := true)
          |> Deferred.Or_error.ok_exn
        in
        let%bind stream =
          open_stream c ~protocol:"echo" b_peerid >>| Or_error.ok_exn
        in
        let r, w = Stream.pipes stream in
        Pipe.write_without_pushback w testmsg ;
        Pipe.close w ;
        (* HACK: let our messages send before we reset.
           It would be more principled to add flushing to
           the stream interface. *)
        let%bind () = after (Time.Span.of_sec 1.) in
        let%bind _ = Stream.reset stream in
        let%bind msg = Pipe.read_all r in
        (* give time for [a] to notice the reset finish. *)
        let%bind () = after (Time.Span.of_sec 1.) in
        let msg = Queue.to_list msg |> String.concat in
        assert (String.equal msg testmsg) ;
        assert !handler_finished ;
        let%bind () = Protocol_handler.close echo_handler in
        let%map () = shutdown () in
        ()
      in
      Async.Thread_safe.block_on_async_exn (fun () -> test_def)

    (* NOTE: these tests are not relevant in the current libp2p setup
             due to how validation is implemented (see #4796)

       let unwrap_eof = function
       | `Eof ->
          failwith "unexpected EOF"
       | `Ok a ->
          Envelope.Incoming.data a

       module type Pubsub_config = sig
       type msg [@@deriving equal, compare, sexp, bin_io]

       val subscribe :
        net -> string -> msg Pubsub.Subscription.t Deferred.Or_error.t

       val a_sent : msg

       val b_sent : msg
       end

       let make_pubsub_test name (module M : Pubsub_config) =
       let open Deferred.Let_syntax in
       let%bind a, b, shutdown = setup_two_nodes ("test_pubsub_" ^ name) in
       let%bind a_sub = M.subscribe a "test" |> Deferred.Or_error.ok_exn in
       let%bind b_sub = M.subscribe b "test" |> Deferred.Or_error.ok_exn in
       let%bind a_peers = peers a in
       let%bind b_peers = peers b in
       [%log fatal] "a peers = $apeers, b peers = $bpeers"
        ~metadata:
          [ ("apeers", `List (List.map ~f:Peer.to_yojson a_peers))
          ; ("bpeers", `List (List.map ~f:Peer.to_yojson b_peers)) ] ;
       let a_r = Pubsub.Subscription.message_pipe a_sub in
       let b_r = Pubsub.Subscription.message_pipe b_sub in
       (* Give the subscriptions time to propagate *)
       let%bind () = after (sec 2.) in
       let%bind () = Pubsub.Subscription.publish a_sub M.a_sent in
       (* Give the publish time to propagate *)
       let%bind () = after (sec 2.) in
       let%bind a_recv = Strict_pipe.Reader.read a_r in
       let%bind b_recv = Strict_pipe.Reader.read b_r in
       [%test_eq: M.msg] M.a_sent (unwrap_eof a_recv) ;
       [%test_eq: M.msg] M.a_sent (unwrap_eof b_recv) ;
       let%bind () = Pubsub.Subscription.publish b_sub M.b_sent in
       let%bind () = after (sec 2.) in
       let%bind a_recv = Strict_pipe.Reader.read a_r in
       let%bind b_recv = Strict_pipe.Reader.read b_r in
       [%test_eq: M.msg] M.b_sent (unwrap_eof a_recv) ;
       [%test_eq: M.msg] M.b_sent (unwrap_eof b_recv) ;
       shutdown ()

       let should_forward_message _ = return true

       let%test_unit "pubsub_raw" =
       let test_def =
        make_pubsub_test "raw"
          ( module struct
            type msg = string [@@deriving equal, compare, sexp, bin_io]

            let subscribe net topic =
              Pubsub.subscribe ~should_forward_message net topic

            let a_sent = "msg from a"

            let b_sent = "msg from b"
          end )
       in
       Async.Thread_safe.block_on_async_exn (fun () -> test_def)

       let%test_unit "pubsub_bin_prot" =
       let test_def =
        make_pubsub_test "bin_prot"
          ( module struct
            type msg = {a: int; b: string option}
            [@@deriving bin_io, equal, sexp, compare]

            let subscribe net topic =
              Pubsub.subscribe_encode ~should_forward_message ~bin_prot:bin_msg
                ~on_decode_failure:`Ignore net topic

            let a_sent = {a= 0; b= None}

            let b_sent = {a= 1; b= Some "foo"}
          end )
       in
       Async.Thread_safe.block_on_async_exn (fun () -> test_def)
    *)
  end )
