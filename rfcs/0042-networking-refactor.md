# Mina Networking Layer Refactor

## Summary
[summary]: #summary

This RFC proposes an overhauling refactor for how our libp2p helper and daemon processes interface. This document will cover a new IPC interface, model for separation of concerns, and code abstraction details that should give us more performance options and flexibility as we continue to build on top of our existing gossip network implementation.

NOTE: This RFC is kept abstract of IPC details related to moving towards bitswap. Additions to this IPC design will be discussed in a separate RFC for bitswap after this RFC is completed and agreed upon.

## Motivation
[motivation]: #motivation

Over the development lifecycle of Mina, we have migrated between various gossip network systems, and while we are now settled on libp2p as our gossip network toolkit, we are continuing to improve the way in which we use it by utilizing more features to optimize our network traffic and reliability. These improvements will bring even more changes in how our existing OCaml codebase will interact with our gossip network layer. However, at the moment, due to our regular migrations and changes to networking, our gossip network interface inside of OCaml is factured into 3 layers. There is quite a bit of code that is entirely outdated. Furthermore, the protocol we use to communicate between our Go and OCaml processes has become rather muddy, and as we have learned more about the performance characteristics of the 2 processes, we have realized that we need to make some serious updates to this protocol in order to prevent it from being a bottleneck in our blockchain software.

## Detailed design
[detailed-design]: #detailed-design

In order to achieve our goals, this RFC introduces an updated libp2p helper IPC protocol and details the new abstraction/structure of the OCaml side networking interface.

In service of removing our existing bottlenecks around the IPC protocol, we will be removing stream state awareness from the OCaml side of the code, preferring to have the Go helper processes be the only one that dynamically manages streams (including opening, closing, and reusing multiplexed streams). In this world, the OCaml process will be fully abstracted to a request/response level of interacting with other peers on the network.

We will also be moving the peer management logic outside of OCaml and into the Go helper process. This means the Go process is now responsible for seed management, trustlist/banlist management, and even peer selection for most requests. The OCaml process will still be the main director of peer scoring, but will no longer manage the state of peers itself (and some of the more basic peer scoring properties, such as overall message rate limiting, can just live on the Go side). There will still be edge cases in which the OCaml process will instruct the Go helper process to send requests to specific peers, but for all requests where the OCaml process does not need a specific peer to respond (eg bootstrap), the Go helper process will manage the selection logic for those peers.

NOTE: The scope of the design discussed in this document is the full refactor, including parts which would require a hard fork to implement properly. There is a section at the end of the design section which details how we can develop this design in 2 stages such that we will be able to isolate the network-interface breaking changes that would need to be shipped in a hard fork.

### Security

Up front, let's identify the security aspects we are aiming to achieve in this RFC. This RFC will not cover security issues relating to rate limiting (which will be covered by the trust scoring RFC), nor issues relating to the maximum message size (which will be covered by the bitswap RFC). Our main security goals we are considering in this RFC are focused around the IPC protocol between the daemon and helper processes. Specifically, the design proposed in this RFC intentially avoids situations in which adversarial nodes could control the number of IPC messages sent between the daemon and the helper (independent of the number of messages sent over the network). In other words, this design is such that the number of IPC messages exchanged between the processes is O(1) in relation to the number of incoming network messages (this is not true of the existing design). In addition, this design limits synchronized state between the 2 processes, which prevents vulnerabilities in which an adversary may be able to desynchronize state between the daemon and helper processes.

### IPC

The new libp2p helper IPC protocol improves upon the previous design in a number of ways. It updates the serialization format so that there is no longer a need to base64 encode/decode messages on each side of the protocol, and it also replaces the singular bidirectional data stream over stdin/stdout with a system with multiple concurrent data streams between the two processes. In order to achieve the latter of these, the libp2p helper process will now be need to be aware of message types for messages it receives over the network (see [#8725](https://github.com/MinaProtocol/mina/pull/8725)).

#### Data Streams

In order to facilitate staging this work into both soft-fork and hard-fork changesets, we will abstract over the concept of a "data stream" for any unidirectional stream of communication between the helper and daemon processes. Doing so, we can discuss the long-term communication architecture for the IPC interface, but be able to write the code in a way such that we can easily swap out this architecture. The code should be implemented such that it is easy to change which messages are expected and sent over which data streams without modifying the protocol logic itself. This allows us to implement a partial version of the architecture until we are able to take the hard-fork changes. Data streams should also be implemented abstract from transport mechanism, so that we can more easily consider upgrades to our transport layer in the future (such as supporting TCP sockets and remote helpers processes). The remainder of this section will only focus on the long-term architecture (more details about how this work will be broken up and staged is available in the "Staging the Compatible and Hard Fork Changes" section of this RFC).

#### Communication Architecture

The helper and daemon will now exchange messages over a variety of data streams, allowing each process to prioritize data streams differently. Correctly optimizing this prioritization on the daemon side is important, since OCaml is single threaded by nature (for now). In particular, the daemon needs to be able to priotize processing and validating certain network messages in order to ensure that the node keeps in sync with the network and forwards relevant information for others to stay in sync. Specifically, the daemon needs to prioritize processing and validating new block gossips so that they can be forwarded to other nodes on the network in a timely manor.

The transport layer we will use for these data streams will be Unix pipes. The parent daemon process can create all the required Unix pipes for the various data streams, and pass the correct file descriptors (either the write or read descriptors depending on the direction of the pipe) to the child helper process when it initializes. Pipes are considered preferable over shared memory for the data streams since they already provide synchronization primitives for reading/writing and are easier to implement correctly, though shared memory would be likely be slightly more optimized.

Below is a proposed set of data streams we would setup for the helper IPC protocol. Keep in mind that some of these pipes require some form of message type awareness in order to be implemented. We have ongoing work that adds message type awareness to the helper process, but this work requires a hard fork. If we want to split up message-specific pipes before a hard fork, we would need to add support for message peeking to the helper (which would involve making the helper aware of at least part of the encoding format for RPC messages).

- stdin (used only for initialization message, then closed)
- stdout (used only for helper logging)
- stderr (used only for helper logging)
- stats\_in (publishes helper stats to daemon on an interval)
- block\_gossip\_in (incoming block gossip messages)
- mempool\_gossip\_in (other incoming gossip messages, related to mempool state)
- response\_in (incoming RPC responses)
- request\_in (incoming RPC requests)
- validation\_out (all validations except request validations, which are bundled with responses)
- response\_out (outgoing RPC responses)
- broadcast\_out (outgoing broadcast messages)

The rough priorities for reading the incoming pipes from the daemon process would be:

- block\_gossip\_in
- response\_in
- request\_in
- mempool\_gossip\_in

NOTE: It is critical in the implementation of this that the prioritization scheme we choose here does not allow the mempool gossip pipe to be starved. The main thing to keep in mind to avoid this is to ensure that we do not over weight reading the incoming requests, so that another node on the network cannot delay (or potentially censor) txns and snarks we are receiving over gossip. One approach towards this could be to limit the parallelism per pipe while keeping the maximum parallel messages we handle from IPC high enough such that we can always schedule new mempool gossip jobs even when there are a lot of requests.

CONSIDER: Is it important that IPC messages include timestamps so that the daemon and helper processes can perform staleness checks as they read messages? For example: if we haven't read a mempool gossip in a while, and read one, discovering that the first message on the pipe is rather old, should we further prioritize this pipe for a bit until we catchup? A potential risk of this system is that it would be hard to guarantee that none of the data streams aren't succeptible to starvation attacks.

#### Serialization Format

The new serialization format will be [Cap'N Proto](https://capnproto.org/). There are already [Go](https://github.com/capnproto/go-capnproto2) and [OCaml](https://github.com/capnproto/capnp-ocaml) libraries for the Cap'N Proto serialization format, which generate code for each language based on a common schema definition of the protocol. Using Cap'N Proto instead of JSON will allow us to embed raw binary data in our IPC messages, which will avoid the rather costly and constant base64 encoding/decoding we currently do for all binary data we transfer between the processes. It's possible to keep some pipes in JSON if preferable, but within the current plan, all messages would be converted over to Cap'N Proto to avoid having to support tooling for keeping both serialization formats in sync between the processes.

NOTE: The [OCaml Cap'N Proto library](https://github.com/capnproto/capnp-ocaml) currently has an inefficient way of handling binary data embeded in Cap'N Proto messages. It uses `Bytes.t` as the backing type for the packed data, and `string` as the type for representing the unpacked data. @mrmr1993 pointed out in the RFC review that we would save 2 memory copies if we used `Bigstring.t` as the backing type for packed data, and slices of that `Bigstring.t` for the unpacked data. These changes are fairly straightforward to make, and can be done in a fork of the library we maintain.

#### Entrypoints

The new libp2p helper interface would support separate entrypoints for specific libp2p tasks, which will simplify some of the IPC interface by removing one-off RPC calls from OCaml to Go. Now, there will be 3 entrypoints, 2 of which will briefly run some computation and exit the process with a result over stdout, and the last of which starts the actual helper process we will use to connect to the network. These interfaces will be accessed directly via CLI arguments rather than being triggered by IPC messages. In otherwords, the IPC system is only active when the helper is run in `gossip_network` mode.

The supported entrypoints will be:
- `generate_keypair`
- `validate_keypair --keypair={keypair}`
- `gossip_network`

#### Protocol

When the lip2p helper process is first started by the Daemon (in `gossip_network` mode), the daemon will send an `init(config Config)` is written once over stdin. The information sent in this message could theoretically be passed via the CLI arguments, but doing this would lose some type safety, so we prefer to send this data over as an IPC message. Once this message has been received by the helper process, the helper process will open ports, join the network, and begin participating in the main protocol loop. In this main protocol loop, either process should expect to receive any IPC messages over any data streams at any time.

Here is a list of the IPC messages that will be supported in each direction, along with some relevant type definitions in Go:

```txt
Daemon -> Helper
  // it's possible to just remove this message as it is just a specialized case of `sendRequests`
  sendRequestToPeer(requestId RequestId, to Peer, msgType MsgType, rawData []byte)
  sendRequests(requestId RequestId, to AbstractPeerGraph, msgType MsgType, rawData []byte)
  sendResponse(requestId RequestId, status ValidationStatus, rawData []byte)
  broadcast(msgType MsgType, rawData []byte)
  validate(validation ValidationHandle, status ValidationStatus)

Helper -> Daemon
  handleRequest(requestId RequestId, from Peer, rawData []byte)
  handleResponse(requestId RequestId, validation ValidationHandle, rawData []byte)
  handleGossip(from Peer, validation ValidationHandle, rawData []byte)
  stats(stats Stats)
```

```go
// == The `Config` struct is sent with the `init` message at the start of the protocol.

// The following old fields have been completely removed:
//   - `metricsPort` (moving over to push-based stats syncing, where we will sync any metrics we want to expose)
//   - `unsafeNotTrustIp` (only used as a hack in old integration test framework; having it makes p2p code harder to reason about)
//   - `gaterConfig` (moving towards more abstracted interface in which Go manages gating state data)
type Config struct {
  networkId           string      // unique network identifier
  privateKey          string      // libp2p id private key
  stateDirectory      string      // directory to store state in (peerstore and dht will be stored/loaded from here)
  listenOn            []Multiaddr // interfaces we listen on
  externalAddr        Multiaddr   // interface we advertise for other nodes to connect to
  floodGossip         bool        // enables gossip flooding (should only be turned on for protected nodes hidden behind a sentry node)
  directPeers         []Multiaddr // forces the node to maintain connections with peers in this list (typically only used for sentry node setups and other specific networking scenarios; these peers are automatically trustlisted)
  seedPeers           []Multiaddr // list of seed peers to connect to initially (seeds are automatically trustlisted)
  maxConnections      int         // maximum number of connections allowed before the connection manager begins trimming open connections
  validationQueueSize int         // size of the queue of active pending validation messages
  // TODO: peerExchange bool vs minaPeerExchange bool (seems like at least one of these should be deprecated)
  //   - peerExchange == enable libp2p's concept of peer exchange in the pubsub options
  //   - minaPeerExchange == write random peers to connecting peers
}

// == The `Stats` struct is sent on an interval via the `stats` message.
// == It contains metrics and statistics relevant to the helper process,
// == to be further exposed by the daemon as prometheus metrics.

type Stats struct {
  storedPeerCount       int
  connectedPeerCount    int
  messageMinSize        float64
  messageMaxSize        float64
  messageAvgSize        float64
  latencyMin            float64
  latencyMax            float64
  latencyAvg            float64
  totalBandwidthIn      float64
  totalBandwidthOut     float64
  totalBandwidthRateIn  float64
  totalBandwidthRateOut float64
}

// == A `ValidationStatus` notifies the helper process of whether or not
// == a message was valid (or relevant).

type ValidationStatus int
const (
  VALIDATION_ACCEPT ValidationStatus = iota
  VALIDATION_REJECT
  VALIDATION_IGNORE
)

// == These types define the concept of an `AbstractPeerGraph`, which
// == describes a peer traversal algorithm for the helper to perform
// == as when finding a successful response to an RPC query.

// Each node of the graph either allows any peer to query, or it
// identifies a specific node to query..
type AbstractPeerType int
const (
  ANY_PEER      AbstractPeerType = iota
  SPECIFIC_PEER
)

// This is essentially an ADT, but we cannot encode an ADT directly
// in Go or Cap'N Proto. An equivalent ADT definition would be:
//   type abstract_peer_node =
//     | AnyPeer
//     | SpecificPeer of Peer
type AbstractPeerNode struct {
  typ  AbstractPeerType
  peer Peer             // nil unless type == SPECIFIC_PEER
}

type AbstractPeerEdge struct {
  src int
  dst int
}

// Alternative (safer) representations are possible, but this is
// simplest to encode in the Cap'N Proto shared schema language.
type AbstractPeerGraph struct {
  nodes []AbstractPeerNode
  edges []AbstractPeerEdge
}
```

#### Query Control Flow

In contrast to the prior implementation, the query control flow in the new protocol always follows the following pattern:

1) The daemon sends 1 message to the helper to begin the query (this message may instruct the helper to begin sending out 1 or more requests, with control over maximum parallelism).
2) The helper continuously and concurrently runs the following protocol until a successful response is found:
  2.a) The helper picks a peer it has not already queried based on the daemon's request and sends a request to this peer.
  2.b) The helper streams the response back to the daemon.
  2.c) The daemon sends a validation callback to the helper.

Keeping the peer selection logic on the helper side allows the daemon to avoid asking the helper for peers before it sends the request. Since the trust scoring state is also already on the helper process, the helper can also select peers based on their score (more details on this to come in the trust scoring RFC). The daemon can still instruct the helper process to query specifc peers, in which cases the daemon will already know of the specific peer and will not need to ask the helper for any additional information.

#### Validation Control Flow

The daemon has to validate incoming network messages of all types (gossip, requests, responses). As such, each IPC message from the helper process that is communicating an incoming gossip message or response includes a `ValidationHandle`, and the daemon is expected to send a `validate` message back to the helper to acknowledge the network message with a `ValidationStatus`. Incoming RPC requests are a special case, however. Since the daemon will already send a message back to the helper in the form of a `response` to the incoming RPC request, the `ValidationStatus` is provided there instead. In this case, a specific `ValidationHandle` is not required, since there is already a `RequestId` that uniquely identifies the response with the request we are validating.

In summary, the new validation control flow is:
- gossip and response validation
  - `handle{Gossip,Response}` message is sent to daemon
  - `validate` is sent back to helper
- request validation
  - `handleRequest` message is sent to daemon
  - `sendResponse` message is sent back to helper, which contains both the response and validation state

### Staging the Compatible and Hard Fork Changes

Some of the key changes proposed in this RFC require a hard fork in order to be released to the network. However, the next hard fork may be a while out. We could just implement this work off of `develop` and wait until the next hard fork to ship it, but this would mean that any immediate improvements we make to the networking code on `compatible` will conflict with our refactor work on `develop`. Overall, it is still a benefit to have this refactor on `compatible` so that we can benefit from it immediately in the soft fork world while keeping the code more or less in line with the future hard fork we want to take.

Accordingly, in order to break this work up, we can do this refactor in 2 passes: first, perform the main module and organization refactor off of `compatible`, then once that is complete and merged, perform the hard fork specific refactor off of `develop`. The `compatible` portion of the refactor can include all changes that do not effect or rely on changes to messages sent over the gossip network. Below is a detailed list of what would be included in each phase of the refactor.

- `compatible`
  - Transport layer abstraction
  - OCaml module conslidation w/ new interface
  - Daemon/Libp2p protocol refactor (peer abstraction et al)
  - Validation, Response, Gossip, and Request pipe split
- `develop`
  - Message type awareness
  - Message type based pipe split
  - Trust scoring based peer-selection (requires trust system)

### OCaml Implementation

Structure wise, the OCaml implementation will continue to model the network interface abstractly so that a dummy implementation may be used for unit testing purposes. We will continue to have an [interface](../src/lib/gossip_net/intf.ml) along with a [existential wrapper](../src/lib/gossip_net/any.ml) that provides indirection over the selected gossip network implementation. A [stub implementation](../src/lib/gossip_net/fake.ml) will also continue to exist.

In order to maintain reasonable separation of concerns, the libp2p helper implementation of the gossip network interface will be split into 2 main modules.
- `Libp2p` :: direct low-level access to `libp2p_helper` process management and protocol
- `Mina_net` :: high-level networking interface which defines supported RPCs and exposes networking functionality to the rest of the code (publicly exposed to the rest of the code)

The RPC interface would continue to be defined under the [current GADT based setup](../src/lib/mina_networking/mina_networking.ml). This type setup will also be extended so that `Rpc.implementation`
modules can be passed in when the gossip network subsystem is initialized. This will be an improvement to the current system in which ad-hoc functions are defined at the [Mina_lib](../src/lib/mina_lib/mina_lib.ml) layer. This module based approach will also provide a mechanism through which we can define global validation logic for RPC query responses that will automatically be applied to all RPC queries of that type. RPC queries will still be able to provide their own per-request validation logic in addition to this.

Below is an example of what the new gossip network interface would look like from the perspective of the rest of the daemon code. Note that it is much more abstract than before, modeling our new design choices regarding migrating state from OCaml to Go.

```ocaml
module Mina_net : sig
  module Config : sig
    type t = (* omitted *)
  end

  module Gossip_pipes : sig
    type t =
      { blocks: External_transition.t Strict_pipe.Reader.t
      ; txn_pool_diffs: Transaction_pool.Diff.t Strict_pipe.Reader.t
      ; snark_pool_diffs: Snark_pool.Diff.t Strict_pipe.Reader.t }
  end

  module Stats : sig
    type t = (* omitted *)
  end

  module Abstract_peer_graph =
    Graph.Persistent.Digraph.ConcreteBidirectional (struct
      type t =
        | AnyPeer
        | SpecificPeer of
      [@@deriving equal, hash]
    end)

  type t

  (* We can construct the gossip network subsystem using the configuration,
   * the set of RPC implementations, a state which is shared with the
   * handlers of the provided RPC implementations. Once constructed, the
   * gossip network handle will be returned along with pipes for reading
   * incoming gossip network messages. *)
  val create :
       Config.t
    -> ('state Rpc_intf.t_with_implementation) list
    -> 'state
    -> (t * Gossip_pipes.t) Deferred.Or_error.t

  val stats : t -> Stats.t

  (* Query operations now have the ability to express additional validation
   * logic on a per-request basis, in addition to RPC-wide validation logic
   * that is defined  *)
  val query_peer :
       t
    -> Peer.t
    -> ('q, 'r) Rpc_intf.rpc
    -> 'q
    -> ?f:('r Envelope.Incoming.t Deferred.t -> Validation_status.t Deferred.t)
    -> 'r Envelope.Incoming.t Deferred.Or_error.t

  val query_peers :
       t
    -> Abstract_peer_graph.t
    -> ('q, 'r) Rpc_intf.rpc
    -> 'q
    -> ?f:('r Envelope.Incoming.t Deferred.t -> Validation_status.t Deferred.t)
    -> 'r Envelope.Incoming.t Deferred.Or_error.t

  val broadcast : t -> Gossip_message.t -> unit Deferred.t

  val ban_peer : t -> Peer.t -> unit Deferred.t
end
```

### Go Implementation

The Go implementation will be fairly similar to how it's structured today. The scope of the state it maintains is more or less the same, the biggest changes introduced in this RFC effect the OCaml code more.  The main work in Go will just be switching it to use Cap'N Proto and use the new message format instead of the old one.

One notable change that can be made is that, since we are moving to a push-based model for libp2p helper metrics, we no longer need to host a prometheus server from Go. However, we will still want the ability to optionally host an http server that exposes the [pprof](https://golang.org/pkg/net/http/pprof/) debugger interface, which we currently support in the metrics server we run.

## Drawbacks
[drawbacks]: #drawbacks

- a refactor of this scope will take some time to test (given historical context for libp2p work, this could be significant)
  - COUNTER: we will have to do something like this eventually anyway, better to do it now than later

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

- instead of refactoring our current go process integration, we could replace our go helper with a rust process now that there is better libp2p support in rust
  - would alleviate us of our current go issues, and move to a better language that more people on the team know and can contribute to
  - certainly less bugs, but certainly harder to build
  - this would be a lot more work and would likely take even longer to test
  - more risk associated with this route
- [flatbuffers](https://google.github.io/flatbuffers/) could be an alternative serialization format (with some advantages and tradeoffs vs Cap'N Proto)
  - there are no existing OCaml libraries for this

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- how should reconfiguration work? we currently support that, but should we just restart the helper process instead?
