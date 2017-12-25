open Core_kernel
open Async_kernel

(* TODO Change this to Peer.t when bin_io works on it *)
type peer = Host_and_port.t [@@deriving bin_io, sexp]

module type S = sig
  type t

  val connect
    : initial_peers:peer list -> me:Host_and_port.t -> t Deferred.t

  val peers : t -> peer list

  val changes : t -> Peer.Event.t Pipe.Reader.t
end

module Node = struct
  type state = Dead | Alive [@@deriving bin_io, sexp]

  type t =
    { peer : peer
    ; mutable state : state
    } [@@deriving bin_io, sexp]

  let is_alive t = t.state = Alive
  let is_dead t = t.state = Dead

  let alive peer = { peer ; state = Alive }


  let dead peer = { peer ; state = Dead }
end

module type RandomIterer_intf = sig
  type 'a t

  val create : initial:'a list -> 'a t

  (* Add an element that may at random be chosen for an iteration that's already happenning *)
  val add : 'a t -> 'a -> unit

  (* Semantics:
   * 1. Merge all pending adds into the list
   * 2. Permute the list randomly
   * 3. Start iterating slowly through the frozen list of size `n`
   * 4. During iteration, new entries could have been added to our `t` structure
   * Whenever there exists `k` items that have been added, with
   * probability `k / (n + k)` choose an element `x` from the new entries
   * without replacement and use that instead
   *)
  val effectful_iter_async : 'a t -> f:('a -> unit Deferred.t) -> unit Deferred.t
end

(* Rough sketch of a possible idea *)
module RandomIterer = struct
  type 'a collection = Todo

  type 'a t =
    { frozen : 'a list
    ; mutable consuming : 'a collection
    ; mutable pending : 'a collection
    }

  let create ~initial = failwith "TODO"

  let add t x = failwith "TODO"

  let effectful_iter_async t ~f = failwith "TODO"
end

module type NetworkState_intf = sig
  type t

  (* A slice of state for network delivery *)
  type slice [@@deriving bin_io, sexp]

  val create : Log.logger -> t

  val add : t -> Node.t -> unit

  val add_slice : t -> slice -> unit

  (* Pure getter for live_nodes *)
  val live_nodes : t -> peer Set.Poly.t

  (* Effectfully (mutating t) extracts a slice of the network state
   * maintaining necessary SWIM invariants *)
  val extract : t -> transmit_limit:int -> Host_and_port.t -> slice
end

(* Taken mostly from mirage-swim *)
module NetworkState : NetworkState_intf = struct
  module E = struct
    type t = Node.t [@@deriving sexp]
    type id = peer

    let invalidates (node : t) (node' : t) : bool =
      node.peer = node'.peer && node.state <> node'.state

    let skip (node : t) peer =
      node.peer = peer

    let size = Node.bin_size_t
  end

  type broadcast_queue_t = (int * E.t) list [@@deriving sexp]
  type t =
    { mutable broadcast_queue : broadcast_queue_t
    ; mutable live_nodes : peer Set.Poly.t
    ; logger : Log.logger
    }

  type slice = Node.t list [@@deriving bin_io, sexp]

  let create logger : t =
    { broadcast_queue = []
    ; live_nodes = Set.Poly.empty
    ; logger
    }

  let update_live t (node : E.t) =
    if node.state = Node.Alive then
      t.live_nodes <- Set.Poly.add t.live_nodes node.peer
    else
      t.live_nodes <- Set.Poly.remove t.live_nodes node.peer

  let add (t : t) (node : E.t) =
    update_live t node;
    if not (List.exists t.broadcast_queue ~f:(fun (_, node') -> node = node')) then begin
      let q = List.filter t.broadcast_queue ~f:(fun (_, node') -> not (E.invalidates node node')) in
      t.broadcast_queue <- ((0, node)::q)
    end

  let add_slice t slice =
    List.iter slice (add t)

  let live_nodes t = t.live_nodes

  let extract (t : t) ~transmit_limit addr =
    let select memo elem =
      let transmit, elems, bytes_left = memo in
      let transmit_count, broadcast = elem in
      let size = E.size broadcast in
      if transmit_count > transmit_limit then
        (transmit, elems, bytes_left)
      else if size > bytes_left || E.skip broadcast addr then
        (transmit, elem::elems, bytes_left)
      else
        (broadcast::transmit, (transmit_count+1, broadcast)::elems, bytes_left - size)
    in
    let transmit, bq', _ = List.fold_left t.broadcast_queue ~f:select ~init:([], [], 65507) in
    t.broadcast_queue <- List.sort ~cmp:(fun e e' -> Int.compare (fst e) (fst e')) bq';
    transmit
end

module Request_or_ack = struct
  type 'a t = Ack | Request of 'a [@@deriving bin_io, sexp]
end

module Payload = struct
  type t = Ping | Ping_req of Host_and_port.t [@@deriving bin_io, sexp]
end

module AckTable = struct
  type t = (int, unit Ivar.t) Hashtbl.t
end

module Messager : (sig
  type t
  type msg = peer * Payload.t Request_or_ack.t * NetworkState.slice * int [@@deriving bin_io, sexp]

  val create : incoming:msg Pipe.Reader.t
    -> net_state:NetworkState.t
    -> get_transmit_limit:(unit -> int)
    (* to send messages *)
    -> send_msg:(recipient:Host_and_port.t -> msg -> unit Or_error.t Deferred.t)
    (* handle incoming msgs (with seq_no) *)
    -> handle_msg:(t -> Payload.t * int -> [`Stop | `Want_ack] Deferred.t)
    -> me: Host_and_port.t
    -> t

  val send : t -> (Host_and_port.t * Payload.t) list -> seq_no:int -> timeout:Time.Span.t -> [ `Timeout | `Acked ] Deferred.t
end) = struct

  type msg = peer * Payload.t Request_or_ack.t * NetworkState.slice * int [@@deriving bin_io, sexp]

  type t =
    { table : AckTable.t
    ; net_state : NetworkState.t
    ; get_transmit_limit : unit -> int
    ; send_msg : recipient:Host_and_port.t -> msg -> unit Or_error.t Deferred.t
    ; me : Host_and_port.t
    (* TODO(bkase): Remember to handle different msgs having diff timeouts *)
    }

  let raw_send t addr ackable_msg seq_no : _ Or_error.t Deferred.t =
    let open Let_syntax in
    t.send_msg ~recipient:addr (t.me, ackable_msg, NetworkState.extract t.net_state ~transmit_limit:(t.get_transmit_limit ()) addr, seq_no)

  let create : incoming:msg Pipe.Reader.t
    -> net_state:NetworkState.t
    -> get_transmit_limit:(unit -> int)
    (* to send messages *)
    -> send_msg:(recipient:Host_and_port.t -> msg -> unit Or_error.t Deferred.t)
    (* handle incoming msgs (with seq_no) *)
    -> handle_msg:(t -> Payload.t * int -> [`Stop | `Want_ack] Deferred.t)
    -> me: Host_and_port.t
    -> t = fun ~incoming ~net_state ~get_transmit_limit ~send_msg ~handle_msg ~me ->
    let table = Hashtbl.Poly.create () in
    let t =
      { table
      ; net_state
      ; get_transmit_limit
      ; send_msg
      ; me
      }
    in

    don't_wait_for begin
      (* TODO imeckler for bkase: Is not pushing back the right thing to do?
         (I changed to [iter_without_pushback] because that was what this was doing anyway)
      *)
      Pipe.iter_without_pushback incoming (fun (from, p, slice, seq_no) ->
        let open Let_syntax in
        let open Request_or_ack in

        (* piggybacked state *)
        NetworkState.add_slice net_state slice;
        (* the sender must have been alive *)
        NetworkState.add net_state {peer = from; state = Alive};

        match p with
        | Request x ->
           (* TODO: Reviewer: If we don't_wait_for here, then we could overload
            * system resources (open sockets etc) if we receive packets faster than
            * we can send them.
            *
            * On the other side, if we wait then we certainly will be overloaded
            * with incoming packets and we won't respond fast enough.
            *
            * This may have to get more complex to properly handle flow control,
            * but I think that udp packates should be sent fast enough that it
            * should be okay?

              imeckler: Yeah this is tricky. Let's leave it as is for now I think.
            *)
           don't_wait_for begin
             match%bind handle_msg t (x, seq_no) with
             | `Stop -> return ()
             | `Want_ack ->
               let%map _ = raw_send t from Ack seq_no in
               ()
           end
        | Ack ->
          Option.iter (Hashtbl.Poly.find t.table seq_no) (fun ivar ->
              Hashtbl.Poly.remove t.table seq_no;
              Ivar.fill_if_empty ivar ()
          )
      )
    end;
    t

  let send t msgs ~seq_no ~timeout =
    let open Let_syntax in
    let wait_ack = Deferred.create (fun _ivar ->
      Hashtbl.Poly.set ~key:seq_no t.table ~data:_ivar;
    ) in
    List.iter msgs (fun (addr, payload) ->
      (* We're implicitly waiting for an ack, so no risk for async pressure *)
      don't_wait_for(
        match%map raw_send t addr (Request_or_ack.Request payload) seq_no with
        | Ok () -> ()
        | Error e -> printf "Failed with err %s" (Error.to_string_hum e)
      );
    );
    match%map Async.with_timeout timeout wait_ack with
    | `Timeout ->
        Hashtbl.Poly.remove t.table seq_no;
        `Timeout
    | `Result () -> `Acked

end

(* TODO(bkase): Replace with real networking code (or functorize) *)
module NetMake (Msg : sig
  type t [@@deriving bin_io, sexp]
end) = struct
  module MyLog = struct include Log end
  open Async
  open Core

  let send : logger:MyLog.logger -> recipient:Host_and_port.t -> Msg.t -> unit Or_error.t Deferred.t = fun ~logger ~recipient msg ->
    let socket = Socket.create Socket.Type.udp in
    let addr = Socket.Address.Inet.create (Unix.Inet_addr.of_string (Host_and_port.host recipient)) ~port:(Host_and_port.port recipient) in
    let iobuf = Iobuf.create ~len:(Msg.bin_size_t msg) in
    logger#logf Debug "Making an iobuf to send of size %d" (Msg.bin_size_t msg);
    Iobuf.Fill.bin_prot Msg.bin_writer_t iobuf msg;
    Iobuf.flip_lo iobuf;
    (* TODO: Actually send full data! *)
    let open Or_error.Let_syntax in
    match Udp.sendto () with
    | Ok sendto ->
        let open Deferred.Let_syntax in
        let%map () = sendto (Socket.fd socket) iobuf addr in
        logger#logf Debug "Sent buf %s over socket" (msg |> Msg.sexp_of_t |> Sexp.to_string);
        Socket.shutdown socket `Both;
        Ok ()
    | Error _ as e -> Deferred.return e
    (*printf "Got sendto error code of %d" code;*)

  let listen : logger:MyLog.logger -> port:int -> Msg.t Pipe.Reader.t Deferred.t =
    fun ~logger ~port ->
      let socket_addr =
        Socket.Address.Inet.create
          (Unix.Inet_addr.of_string "127.0.0.1")
          ~port:port
      in
      let open Deferred.Let_syntax in
      let%map socket = Udp.bind socket_addr in
      let (r,w) = Pipe.create () in
      don't_wait_for begin
        Udp.recvfrom_loop (Socket.fd socket) (fun buf addr ->
          let msg = Iobuf.Consume.bin_prot Msg.bin_reader_t buf in
          logger#logf Debug "Got msg %s on socket" (msg |> Msg.sexp_of_t |> Sexp.to_string);
          (* TODO: Do we need pushback here? *)
          Pipe.write_without_pushback w msg
        )
      end;
      r
end

module Udp : S = struct
  type config =
    { indirect_ping_count : int
    ; protocol_period : Time.Span.t
    ; rtt : Time.Span.t
    }

  (* TODO: Make this a parameter *)
  let my_config : config =
    { indirect_ping_count = 6
    ; protocol_period = Time.Span.of_int_sec 2
    ; rtt = Time.Span.of_sec 0.5
    }

  type t =
    { messager : Messager.t
    ; net_state  : NetworkState.t
    ; mutable seq_no : int
    ; logger : Log.logger
    }

  let fresh_seq_no t =
    let seq_no = t.seq_no in
    t.seq_no <- t.seq_no + 1;
    seq_no

  let prob_node t (m_i : Node.t) =
    let sample_nodes xs k exclude =
      xs |> Set.Poly.to_list
      |> List.filter ~f:(fun n -> n <> exclude)
      |> List.permute
      |> fun l -> List.take l k
    in

    let seq_no = fresh_seq_no t in
    t.logger#logf Info "Begin round %d probing node %s" t.seq_no (m_i |> Node.sexp_of_t |> Sexp.to_string);
    match%bind Messager.send t.messager [ (m_i.peer, Ping) ] ~seq_no ~timeout:my_config.rtt with
    | `Acked ->
        t.logger#logf Info "Round %d acked" t.seq_no;
        return ()
    | `Timeout ->
        t.logger#logf Info "Round %d timeout" t.seq_no;
        let m_ks = sample_nodes (NetworkState.live_nodes t.net_state) my_config.indirect_ping_count m_i.peer in
        match%map Messager.send t.messager (List.map m_ks ~f:(fun m_k -> (m_k, Payload.Ping_req m_i.peer))) ~seq_no ~timeout:Time.Span.(my_config.protocol_period - my_config.rtt) with
        | `Acked ->
            t.logger#logf Info "Round %d secondary acked" t.seq_no
        | `Timeout ->
            m_i.state <- Node.Dead;
            t.logger#logf Info "Round %d m_i died" t.seq_no

  (* TODO: Round-Robin extension *)
  let rec failure_detect (t : t) : _ Deferred.t =
    t.logger#logf Debug "Start failure_detect";
    (* TODO: WTF if I don't flush stdout here, nothing ever gets logged *)
    Out_channel.flush stdout;
    let%bind () =
      match Set.Poly.length (NetworkState.live_nodes t.net_state) with
      | 0 ->
        Async.after my_config.protocol_period
      | _ ->
        let choose xs =
          Option.value_exn (List.nth xs (Random.int (List.length xs))) in

        let m_i = choose (NetworkState.live_nodes t.net_state |> Set.Poly.to_list) in
        let%map ((), ()) = Deferred.both
          (prob_node t {peer=m_i;state=Alive})
          (Async.after my_config.protocol_period) in
        ()
    in
    failure_detect t

  (* From mirage-swim *)
  let transmit_limit net_state =
    let live_count = Set.Poly.length (NetworkState.live_nodes net_state) in
    if live_count = 0 then
      0
    else
      live_count
      |> Float.of_int
      |> log
      |> Float.round_up
      |> Float.to_int

  module Net = NetMake(struct
    type t = Messager.msg [@@deriving bin_io, sexp]
  end)

  let connect ~initial_peers ~me =
    let logger = new Log.logger (Printf.sprintf "Swim:%s" (me |> Host_and_port.port |> string_of_int)) in
    let%map incoming = Net.listen logger (Host_and_port.port me) in
    let net_state = NetworkState.create logger in
    let rec handle_msg messager = function
      | (Payload.Ping, _) -> Deferred.return `Want_ack
      | (Payload.Ping_req addr_i, seq_no) ->
          match%map Messager.send messager [(addr_i, Payload.Ping)] ~seq_no:seq_no ~timeout:my_config.rtt with
          | `Timeout -> `Stop
          | `Acked -> `Want_ack
    and make_messager () =
      Messager.create
        ~incoming
        ~net_state
        ~get_transmit_limit:(fun () -> transmit_limit net_state)
        ~send_msg:(Net.send ~logger)
        ~handle_msg
        ~me
    in
    let t =
      { messager = make_messager ()
      ; net_state
      (* Assume all initial_peers start alive *)
      ; seq_no = 0
      ; logger
      }
    in
    (* Assume initial peers are alive *)
    List.iter initial_peers ~f:(fun peer ->
      NetworkState.add t.net_state (Node.alive peer);
    );

    (* TODO: An explicit join_network shouldn't be necessary since failure_detect will implicitly join when pings happen *)
    don't_wait_for(schedule' (fun () -> failure_detect t));

    t

  let peers t = NetworkState.live_nodes t.net_state |> Set.Poly.to_list

  let changes t = failwith "TODO"
end
