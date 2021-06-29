(** An interface to limited libp2p functionality for Coda to use.

    A subprocess is spawned to run the go-libp2p code. This module communicates
    with that subprocess over an ad-hoc RPC protocol.

    TODO: separate internal helper errors from underlying libp2p errors.

    In general, functions in this module return ['a Deferred.Or_error.t]. Unless
    otherwise mentioned, the deferred is resolved immediately once the RPC action
    to the libp2p helper is finished. Unless otherwise mentioned, everything can
    throw an exception due to an internal helper error. These indicate a bug in
    this module/the helper, and not misuse.

    Some errors can arise from calling certain functions before [configure] has been
    called. In general, anything that returns an [Or_error] can fail in this manner.

    A [Mina_net2.t] has the following lifecycle:

    - Fresh: the result of [Mina_net2.create]. This spawns the helper process but
      does not connect to any network. Few operations can be done on fresh nets,
      only [Keypair.random] for now.

    - Configured: after calling [Mina_net2.configure]. Configure creates the libp2p
      objects and can start listening on network sockets. This doesn't join any DHT
      or attempt peer connections. Configured networks can do everything but any
      pubsub messages may have very limited reach without being in the DHT.

    - Active: after calling [Mina_net2.begin_advertising]. This joins the DHT,
      announcing our existence to our peers and initiating local mDNS discovery.

    - Closed: after calling [Mina_net2.shutdown]. This flushes all the pending RPC

    TODO: consider encoding the network state in the types.

    A note about connection limits:

    In the original coda_net, connection limits were enforced synchronously on
    every received connection. Right now with mina_net2, connection management is
    asynchronous and post-hoc. In the background, once per minute it checks the
    connection count. If it is above the "high water mark", it will close
    ("trim") eligible connections until it reaches the "low water mark". All
    connections start with a "grace period" where they won't be closed. Peer IDs
    can be marked as "protected" which prevents them being trimmed. Ember believes this
    is vulnerable to resource exhaustion by opening many new connections.

*)

open Core
open Async
open Pipe_lib
open Network_peer

(** Handle to all network functionality. *)
type net

module Validation_callback : sig
  type validation_result = [`Accept | `Reject | `Ignore] [@@deriving equal]

  type t

  val create : Time_ns.t -> t

  val create_without_expiration : unit -> t

  val is_expired : t -> bool

  val await : t -> validation_result option Deferred.t

  val await_exn : t -> validation_result Deferred.t

  (** May return a deferred that never resolves, in the case of callbacks without expiration. *)
  val await_timeout : t -> unit Deferred.t

  val fire_if_not_already_fired : t -> validation_result -> unit

  val fire_exn : t -> validation_result -> unit
end

module Keypair : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t
    end
  end]

  (** Securely generate a new keypair. *)
  val random : net -> t Deferred.t

  (** Formats this keypair to a comma-separated list of public key, secret key, and peer_id. *)
  val to_string : t -> string

  (** Undo [to_string t].

      Only fails if the string has the wrong format, not if the embedded
      keypair data is corrupt. *)
  val of_string : string -> t Core.Or_error.t

  val to_peer_id : t -> Peer.Id.t
end

(** A "multiaddr" is libp2p's extensible encoding for network addresses.

    They generally look like paths, and are read left-to-right. Each protocol
    type defines how to decode its address format, and everything leftover is
    encapsulated inside that protocol.

    Some example multiaddrs:

    - [/p2p/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC]
    - [/ip4/127.0.0.1/tcp/1234/p2p/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC]
    - [/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21]
*)
module Multiaddr : sig
  type t [@@deriving compare, bin_io]

  val to_string : t -> string

  val of_string : string -> t

  val to_peer : t -> Network_peer.Peer.t option

  (** can a multiaddr plausibly be used as a Peer.t?
      a syntactic check only; a return value of
       true does not guarantee that the multiaddress can
       be used as a peer by libp2p
  *)
  val valid_as_peer : t -> bool

  val of_file_contents : contents:string -> t list
end

type discovered_peer = {id: Peer.Id.t; maddrs: Multiaddr.t list}

module Pubsub : sig
  (** A subscription to a pubsub topic. *)
  module Subscription : sig
    type 'a t

    (** Publish a message to this pubsub topic.
     *
     * Returned deferred is resolved once the publish is enqueued locally.
     * This function continues to work even if [unsubscribe t] has been called.
     * It is exactly [Pubsub.publish] with the topic this subscription was
     * created for, and fails in the same way. *)
    val publish : 'a t -> 'a -> unit Deferred.t

    (** Unsubscribe from this topic, closing the write pipe.
     *
     * Returned deferred is resolved once the unsubscription is complete.
     * This can fail if already unsubscribed. *)
    val unsubscribe : _ t -> unit Deferred.Or_error.t

    (** The pipe of messages received about this topic. *)
    val message_pipe : 'a t -> 'a Envelope.Incoming.t Strict_pipe.Reader.t
  end

  (** Publish a message to a topic.
   *
   * Returned deferred is resolved once the publish is enqueued.
   * This can fail if signing the message failed.
   *  *)
  val publish : net -> topic:string -> data:string -> unit Deferred.t

  (** Subscribe to a pubsub topic.
    *
    * Fails if already subscribed. If it succeeds, incoming messages for that
    * topic will be written to the [Subscription.message_pipe t]. Returned deferred
    * is resolved with [Ok sub] as soon as the subscription is enqueued.
    *
    * [should_forward_message] will be called once per new message, and will
    * not be called again until the deferred it returns is resolved. The helper
    * process waits 5 seconds for the result of [should_forward_message] to be
    * reported, otherwise it will not forward it.
  *)
  val subscribe :
       net
    -> string
    -> should_forward_message:(   string Envelope.Incoming.t
                               -> Validation_callback.t
                               -> unit Deferred.t)
    -> string Subscription.t Deferred.Or_error.t

  (** Like [subscribe], but knows how to stringify/destringify
    *
    * Fails if already subscribed. If it succeeds, incoming messages for that
    * topic will be written to the [Subscription.message_pipe t]. Returned deferred
    * is resolved with [Ok sub] as soon as the subscription is enqueued.
    *
    * [should_forward_message] will be called once per new message, and will
    * not be called again until the deferred it returns is resolved. The helper
    * process waits 5 seconds for the result of [should_forward_message] to be
    * reported, otherwise it will not forward it.
  *)
  val subscribe_encode :
       net
    -> string
    -> should_forward_message:(   'a Envelope.Incoming.t
                               -> Validation_callback.t
                               -> unit Deferred.t)
    -> bin_prot:'a Bin_prot.Type_class.t
    -> on_decode_failure:[ `Ignore
                         | `Call of
                           string Envelope.Incoming.t -> Error.t -> unit ]
    -> 'a Subscription.t Deferred.Or_error.t
end

(** [create ~logger ~conf_dir] starts a new [net] storing its state in [conf_dir]
  *
  * The new [net] isn't connected to any network until [configure] is called.
  *
  * This can fail for a variety of reasons related to spawning the subprocess.
*)
val create :
     all_peers_seen_metric:bool
  -> on_unexpected_termination:(unit -> unit Deferred.t)
  -> logger:Logger.t
  -> pids:Child_processes.Termination.t
  -> conf_dir:string
  -> net Deferred.Or_error.t

(** State for the connection gateway. It will disallow connections from IPs
    or peer IDs in [banned_peers], except for those listed in [trusted_peers]. If
    [isolate] is true, only connections to [trusted_peers] are allowed. *)
type connection_gating =
  {banned_peers: Peer.t list; trusted_peers: Peer.t list; isolate: bool}

(** Configure the network connection.
  *
  * Listens on each address in [maddrs].
  *
  * This will only connect to peers that share the same [network_id]. [on_new_peer], if present,
  * will be called for each peer we connect to. [unsafe_no_trust_ip], if true, will not attempt to
  * report trust actions for the IPs of observed connections.
  *
  * Whenever the connection list gets too small, [seed_peers] will be
  * candidates for reconnection for peer discovery.
  *
  * This fails if initializing libp2p fails for any reason.
*)
val configure :
     net
  -> logger:Logger.t
  -> me:Keypair.t
  -> external_maddr:Multiaddr.t
  -> maddrs:Multiaddr.t list
  -> network_id:string
  -> metrics_port:string option
  -> on_peer_connected:(Peer.Id.t -> unit)
  -> on_peer_disconnected:(Peer.Id.t -> unit)
  -> unsafe_no_trust_ip:bool
  -> flooding:bool
  -> direct_peers:Multiaddr.t list
  -> peer_exchange:bool
  -> mina_peer_exchange:bool
  -> seed_peers:Multiaddr.t list
  -> initial_gating_config:connection_gating
  -> max_connections:int
  -> validation_queue_size:int
  -> unit Deferred.Or_error.t

(** The keypair the network was configured with.
  *
  * Resolved once configuration succeeds.
*)
val me : net -> Keypair.t Deferred.t

(** List of all peers we know about. *)
val peers : net -> Peer.t list Deferred.t

(** Set node status to be served to peers requesting node status. *)
val set_node_status : net -> string -> unit Deferred.Or_error.t

(** Get node status from given peer. *)
val get_peer_node_status : net -> Peer.t -> string Deferred.Or_error.t

(** Try to connect to a peer ID, returning a [Peer.t]. *)
val lookup_peerid : net -> Peer.Id.t -> Peer.t Deferred.Or_error.t

(** An open stream.

    Close the write pipe when you are done. This won't close the reading end.
    The reading end will be closed when the remote peer closes their writing
    end. Once both write ends are closed, the stream ends.

    Long-lived connections are likely to get closed by the remote peer if
    they reach their connection limit. See the module-level notes about
    connection limiting.

    IMPORTANT NOTE: A single write to the stream will not necessarily result
    in a single read on the other side. libp2p may fragment messages arbitrarily.
*)
module Stream : sig
  type t

  (** [pipes t] returns the reader/writer pipe for our half of the stream. *)
  val pipes : t -> string Pipe.Reader.t * string Pipe.Writer.t

  (** [reset t] informs the other peer to close the stream.

      The returned [Deferred.Or_error.t] is fulfilled with [Ok ()] immediately
      once the reset is performed. It does not wait for the other host to
      acknowledge.
  *)
  val reset : t -> unit Deferred.Or_error.t

  val remote_peer : t -> Peer.t
end

(** [Protocol_handler.t] is the rough equivalent to [Tcp.Server.t].

    This lets one stop handling a protocol.
*)
module Protocol_handler : sig
  type t

  (** Returns the protocol string being handled. *)
  val handling_protocol : t -> string

  (** Whether [close t] has been called. *)
  val is_closed : t -> bool

  (** Stop handling new streams on this protocol.

      [reset_existing_streams] controls whether open streams for this protocol
      will be reset, and defaults to [false].
  *)
  val close : ?reset_existing_streams:bool -> t -> unit Deferred.t
end

(** Opens a stream with a peer on a particular protocol.

    Close the write pipe when you are done. This won't close the reading end.
    The reading end will be closed when the remote peer closes their writing
    end. Once both write ends are closed, the connection terminates.

    This can fail if the peer isn't reachable, doesn't implement the requested
    protocol, and probably for other reasons.
*)
val open_stream :
  net -> protocol:string -> Peer.Id.t -> Stream.t Deferred.Or_error.t

(** Handle incoming streams for a protocol.

    [on_handler_error] determines what happens if the handler throws an
    exception. If an exception is raised by [on_handler_error] (either explicitly
    via [`Raise], or in the function passed via [`Call]), [Protocol_handler.close] will
    be called.

    The function in `Call will be passed the stream that faulted.
*)
val handle_protocol :
     net
  -> on_handler_error:[`Raise | `Ignore | `Call of Stream.t -> exn -> unit]
  -> protocol:string
  -> (Stream.t -> unit Deferred.t)
  -> Protocol_handler.t Deferred.Or_error.t

(** Try listening on a multiaddr.
 *
 * If successful, returns the list of all addresses this net is listening on
 * For example, if listening on ["/ip4/127.0.0.1/tcp/0"], it might return
 * ["/ip4/127.0.0.1/tcp/35647"] after the OS selects an available listening
 * port.
 *
 * This can be called many times.
*)
val listen_on : net -> Multiaddr.t -> Multiaddr.t list Deferred.Or_error.t

(** The list of addresses this net is listening on.

    This returns the same thing that [listen_on] does, without listening
    on an address.
*)
val listening_addrs : net -> Multiaddr.t list Deferred.Or_error.t

(** Connect to a peer, ensuring it enters our peerbook and DHT.

    This can fail if the connection fails. *)
val add_peer : net -> Multiaddr.t -> seed:bool -> unit Deferred.Or_error.t

(** Join the DHT and announce our existence.

    Call this after using [add_peer] to add any bootstrap peers. *)
val begin_advertising : net -> unit Deferred.Or_error.t

(** Stop listening, close all connections and subscription pipes, and kill the subprocess. *)
val shutdown : net -> unit Deferred.t

(** Configure the connection gateway.

    This will fail if any of the trusted or banned peers are on IPv6. *)
val set_connection_gating_config :
  net -> connection_gating -> connection_gating Deferred.t

val connection_gating_config : net -> connection_gating Deferred.t

(** List of currently banned IPs. *)
val banned_ips : net -> Unix.Inet_addr.t list Deferred.t
