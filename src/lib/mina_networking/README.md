# Mina Networking

## Typing Conventions

In the context of this document, we will describe query and response types using a psuedo type-system. Tuples of data are in the form `(a, ..., b)`, lists are in the form `[a]`, and polymorphic types are represented as functions returning types. For example, we use the standard polymorphic types `optional :: type -> type` and `result :: type -> type -> type` throughout this document. The `optional` type constructor means that a value can be null, and a `result` type constructor means that there is 1 of 2 possible return types (typically a success type and an error type). For example, `optional int` might be an int or null, where as `result int error` is either an int or an error.

### Relevant types

- `error` == generic errors represented in OCaml format
- `peer` == host, port, and peer id of a remote peer
- `state_hash` == a hash which identifies a block in the blockchain
- `state_body_hashes` == intermediate hashes of blocks which (a `state_hash` is a merkle list of `state_body_hash`es)
- `ledger_hash` == a hash which identifies a ledger in the blockchain
- `consensus_state` == the consensus specific contents of a block (contains timing, strength, quality, and production information)
- `protocol_state` == the proven contents of a block (contains `consensus_state`)
- `block` == an entire block (contains `protocol_state` and the staged ledger diff for that block)
- `staged_ledger` == the data structure which represents the intermediate (unsnarked) ledger state of the network (large)
- `pending_coinbase` == a auxilliary hash which identifies some state related to the staged ledger
- `sync_ledger_query` == queries for performing sync ledger protocol (requests for hashes or batches of subtrees of a merkle tree)
- `sync_ledger_response` == responses for handling sync ledger protocol (responses of hashes or batches of subtrees of a merkle tree)
- `transaction_pool_diff` == a bundle of multiple transactions to be included into the blockchain
- `snark_pool_diff` == a bundle of 1-2 snark works to be included into the blockchain
- `node_status` == a bundle of information about the status of a node (used for telemetry)

## Broadcast Messages

### states

**Data**: `block`

Broadcasts newly produced blocks throughout the network.

### transaction\_pool\_diffs

**Data**: `transaction_pool_diff`

Broadcasts transactions from mempools throughout the network. Nodes broadcast locally submitted transactions on an interval for a period of time after creation, as well as rebroadcast externally submitted transactions if they were relevant and could be added to the their mempool.

### snark\_pool\_diffs

**Data**: `snark_pool_diff`

Broadcasts snark work from mempools throughout the network. Snark coordinator's broadcast locally produced snarks on an interval for a period of time after creation, and all nodes rebroadcast externally produced snarks if they were relevant and could be added to the their mempool.

## RPC Messages

### get\_some\_initial\_peers

##### TODO: deprecate -- this should no longer be required with modern libp2p\_helper

**Query**: `unit`

**Response**: `[peers]`

Returns known peers to the requester. Currently used to help nodes quickly connect to the network.

### get\_staged\_ledger\_aux\_and\_pending\_coinbases\_at\_hash

##### TODO: utilize bitswap for this

**Query**: `state_hash`

**Response**: `optional (staged_ledger, ledger_hash, pending_coinbase, [protocol_state])`

Returns the staged ledger data associated with a particular block on the blockchain. This is requested by bootstrapping nodes and is used to construct the root staged ledger of the frontier. Subsequent staged ledgers are built from the root using the staged ledger diff information contained in blocks.

### answer\_sync\_ledger\_query

##### TODO: utilize bitswap for this

**Query**: `(ledger_hash, sync_ledger_query)`

**Response**: `result sync_ledger_response error`

Serves merkle ledger information over a "sync ledger" protocol. The sync ledger protocol works by allowing nodes to request intermediate hashes and leaves of data in a target ledger they want to synchronize with. Using the protocol, requesters are able to determine which parts of their existing ledger need to be updated, and can avoid downloading data in the ledger which is already up to date. Requests are distributed to multiple peers to spread the load of serving sync ledgers throughout the network.

### get\_transition\_chain

##### TODO: utilize bitswap for this

**Query**: `[state_hash]`

**Response**: `optional [block]`

Returns a bulk bulk set of blocks associated with a provided set of state hashes. This is used by the catchup routine when it is downloading old blocks to re synchronize with the network over short distances of missing information. At the current moment, the maximum number of blocks that can be requested in a single batch is 20 (requesting more than 20 will result in no response).

### get\_transition\_chain\_proof

**Query**: `state_hash`

**Response**: `optional (state_hash, [state_body_hash])` (a merkle proof of block hashes)

Returns a transition chain proof for a specified block on the blockchain. A transition chain proof proves that, given some block `b1`, there exists a block `b0` which is `k` blocks preceeding `b1`. To prove this, a node receives `H(b1)`, and returns `H(b0)` along with a merkle proof of all the intermediate "state body hashes" along the path `b0 -> b1`. The requester checks this proof by re-computing `H(b1)` from the provided merkle proof.

### get\_transition\_chain\_knowledge

##### TODO: consider renaming

**Query**: `unit`

**Response**: `[state_hash]`

Returns the a list of `k` state hashes of blocks from the root of the frontier (point of finality) up to the current best tip (most recent block on the canonical chain).

### get\_ancestry

**Query**: `(consensus_state, state_hash)`

**Response**: `optional (block, [state_body_hash], block)` (two blocks and a merkle proof of their connectivity)

Returns the target block corresponding to the provided state hash along with the root block of the frontier, and a merkle proof that attests to the connection of blocks linking the target block to the root block.

### ban\_notify

##### TODO: remove as direct daemon RPC in favor direct libp2p point2point notification before disconnect

**Query**: `time`

**Response**: `unit`

Notifies a peer that they have been banned. The request contains the time at which the requester banned the recipient.

### get\_best\_tip

**Query**: `unit`

**Response**: `optional (block, [state_body_hash], block)` (two blocks and a merkle proof of their connectivity)

Returns the best tip block along with the root block of the frontier, and a merkle proof that attests to the connection of blocks linking the target block to the root block. If the root has been finalized, this proof will be of length `k-1` to attest to the fact that the best tip block is `k` blocks on top of the root block.

### get\_node\_status

##### NOTE: we are actively investigating this one and may remove it in the near future in favor of an alternative system

**Query**: `unit`

**Response**: `result node_status error`

This acts as a telemetry RPC which asks a peer to provide invformation about their node status. Daemons do not have to respond to this request, and node operators may pass a command line flag to opt-out of responding to these node status requests.
