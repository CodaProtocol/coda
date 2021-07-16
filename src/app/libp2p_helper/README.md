# mina go-libp2p helper

## libp2p_helper hints

If you are adding new `methodIdx` values, edit `generate_methodidx/main.go`
(search for `TypesAndValues`) with the names of the new values. Then, run `go
run generate_methodidx/main.go > libp2p_helper/methodidx_jsonenum.go`.

## building

### Makefile

```
$ make
$ make clean
```

### Bazel

```
$ bazel build src/generate_methodidx
$ bazel run src/generate_methodidx
$ bazel build src:codanet
$ bazel build src/libp2p_helper
```

#### using as an external repo

Add a repository rule to your root WORKSPACE(.bazel) file to import this repo.  For example, if you embed this repo as a git submodule:

```
local_repository(
    name = "libp2p_helper",
    path = "path/to/libp2p_helper"
)
```

Copy the contents of WORKSPACE.bazel to your root WORKSPACE(.bazel)
file. Change the line: `load("//bzl/libp2p:deps.bzl",
"libp2p_bootstrap")` to use the fully-qualified label, e.g.
`load("@libp2p_helper//bzl/libp2p:deps.bzl", "libp2p_bootstrap")`.

#### maintenance

Install [Gazelle](https://github.com/bazelbuild/bazel-gazelle).

Update the bootstrap code responsible for loading Go deps:

`$ bazel run //:gazelle -- update-repos -from_file=src/go.mod -to_macro bzl/libp2p/deps.bzl%libp2p_bootstrap`

Update the build files: `$ bazel run //:gazelle update`

Go libs containing protobuf stuff: add `build_file_proto_mode =
"disable_global"` to the `go_library` rule in `//bzl/deps.bzl`. Also
add `# gazelle:proto disable_global` to the root WORKSPACE.bazel file.
See [Go Protocol buffers - avoiding
conflicts](https://github.com/bazelbuild/rules_go/blob/master/proto/core.rst#avoiding-conflicts): [Option 2: Use pre-generated .pb.go files](https://github.com/bazelbuild/rules_go/blob/master/proto/core.rst#option-2-use-pre-generated-pb-go-files).
for more info.

# How it works

libp2p_helper serves as a middleware between libp2p and Ocaml process. They communicate using a number of internal messages.
Below we enumerate all message types along with description of how Helper handles the message.

 * configure
    * Accept configuration, launch p2p manager and metrics server (if configured).
    * Among other things, start listening to peers on the `ListenOn` list.
 * listen
    * Start listening to the new peers.
 * publish
    * Message consists of topic `t` and data `d`
    * Join a topic `t`
    * Publish a message `d`
 * subscribe
    * Join a topic
    * Setup a handler on topic messages to process each message though validator
      * To validate a message a `validate` call is made to the OCaml process
      * Validation time is capped by `validationTimeout`, timeout is treated as the signal that message is invalid, unless `UnsafeNoTrustIP` flag is set.
      * Unsatisfied validations are kept in a map, always accessed under mutex.
    * Subscrube to a topic (this is different from joining)
    * Launch a subroutine that reads each message and logs an error if a message fails to be read
 * unsubscribe
    * Cancel the subscription, clean up associated resources
 * validationComplete
    * Fullfill the validation with result
    * Performs the action under app-global `ValidatorMutex`
    * Logs an error if validation has already timed out
 * generateKeypair
    * Generates a new key pair, along with peer id
    * Returns the generated key pair
 * openStream
    * Initiates a new stream to a given peer over the given protocol (specified by protocol id)
    * Launches a subroutine that reads the stream:
      * Read the length `length` of an incoming message
      * On EOF, send a `streamReadComplete` message to the Ocaml process
      * On any other issue with receiving the length message, send `streamLost` message to the Ocaml process
      * Set stream to state `STREAM_DATA_UNEXPECTED` (setting is performed under `StreamsMutex`)
      * Update metrics with the `length`
      * Read all of the chunks of data on the stream (total of `length` bytes), send each chunk with the `incomingStreamMsg` message to the OCaml process
      * If any of the chunks is not read, send a `streamLost` exception
      * Send each chunk with `incomingStreamMsg` to the Ocaml process immediately after reading it (not waiting for the other chunks)
      * Note that the stream is left in `STREAM_DATA_UNEXPECTED` state (until the reply to the received message is sent via `sendStreamMsg` message)
 * closeStream
    * Takes `StreamsMutex` mutex (releases on the end of processing of the message)
    * Close the specified stream and clean up the resources
 * resetStream
    * Calls `Reset` method on a specified stream
 * sendStreamMsg
    * Takes `StreamsMutex` mutex (releases on the end of processing of the message)
    * Send `length`
    * Send data of the message
    * Important: global mutex is taken for the whole duration of message sending
 * addStreamHandler
    * Sets a handler for the incoming streams on a given protocol (specified by the protocol id)
    * On stream open, allocates a new stream id and sends an `incomingStream` message to the Ocaml process
    * Takes `StreamsMutex` mutex (releases on the end of processing of the message)
    * Important: mutex is not released until a message to the Ocaml process is sent
    * Launches a subroutine that reads the stream with same behaviour as one described for `openStream`
 * removeStreamHandler
    * Removes the stream handler for the given protocol
 * listeningAddrs
    * Returns set of addresses from the p2p layer
 * addPeer
    * Adds a peer
    * Makes the peer trusted
    * If `Seed` flag is specified, also adds it to the seeds
    * Runs p2p's connect for the peer
 * beginAdvertising
    * Connects to all added peers
    * Launches a subroutine to "report discovery peers" (TODO: write in more details)
 * findPeer
    * If there is a connection to the specified peer, return its information
    * Error is returned otherwise
 * listPeers
    * Return a list of peer information for each open connection
 * setGatingConfig
    * Sets a new gating config (banned and trusted ids/ips)
 * setNodeStatus
    * Sets a node status
    * Node status is a bytestring without particular structure (as of the libp2p_helper's view)
 * getPeerNodeStatus 
    * Opens a stream to the other node, retrieves its status, closes the stream and returns the status to the OCaml process
