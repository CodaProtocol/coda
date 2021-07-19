# Transition Frontier

The transition frontier is a hybrid in-memory/on-disk data structure which represents all known states on the network up to the point of finality. This data structure plays an essential role in tracking states produced by consensus protocol in a way that automatically garbage collects orphaned states which will never become part of the canonical chain, and does so in a way that optimizes for high read/write performance while also persisting information in the background for future recovery. As such, the transition frontier can be thought of as both an in-memory data structure, and a concurrent subsystem which maintains a persistent on-disk copy of the datastructure. To help maintain this viewpoint, the implementation is split into 2 main parts: a full frontier, which stores the entire expanded state of each block in-memory, and a persistent frontier, which asynchronously processes state transitions to the full frontier, proxying those operations to a RocksDB representation of the frontier in the background.

In terms of the data structure, the transition frontier can be thought of as a combination of the following pieces of information:

1) A rose tree that contains all blockchains (including forks) up to `k` in length from the most recently finalized block.
2) A history of recently finalized blocks.
3) A snarked ledger for the most recently finalized block.
4) A series of ledger masks, chained off of the aforementioned snarked ledger, to represent intermediate staged ledger states achieved by blocks tracked past the most recently finalized block.
5) The auxiliary scan state information associated with each block tracked past the most recently finalized block.

Importantly, the transition frontier also can identify which of the states it is tracking is the strongest state, which is referred to as the "best tip". The consensus mechanism informs the transition frontier of how to compare blocks for strength.

## Formal Spec

TODO

## Glossary

| Name                     | Description |
|--------------------------|-------------|
| Best Tip                 | The best state in a frontier. This is always a tip (leaf) of the frontier, by nature of the consensus selection rules |
| Breadcrumb               | A fully expanded state for a block. Contains a validated block and a make-chained staged ledger, with some metadata. |
| Frontier Diff            | A representation of a state transition to perform on a frontier data structure. |
| Frontier Root Data       | Fat, auxiliary information stored for the root of a persistent frontier. Stored in a single file, it contains a serialized scan state and pending coinbase state, which can be used to reconstruct a staged ledger at the root of the persistent frontier. |
| Frontier Root Identifier | Light, auxiliary information stored for the persistent root. Stored in a single file, it identifies the root state hash currently associated with a persistent root. |
| Full Frontier            | The in-memory representation of the transition frontier, with fully expanded states at every node (breadcrumbs). |
| Persistent Frontier      | An on-disk representation of the transition frontier. Stores block information in RocksDB, which is asynchronously updated by processing frontier diffs applied to the in memory full frontier representation. |
| Persistent Root          | An on-disk ledger where the root snarked ledger of the transition frontier is stored. The ledger serves as the root ledger in the full frontier's mask chain, and is actively mutated by the full frontier as new roots are committed. |
| Root Snarked Ledger      | Synonymous with persistent root (in the case of full frontier), this is the fully snarked ledger at the root of a frontier |

## Architecture

### Frontier Spec

#### Frontier Invariants

All frontiers must hold the following invariants at every state:

- all paths leading from the root of the frontier are no more than `k` in length
- the best tip of the frontier is stronger than all other nodes in the frontier (selected via consensus)
- (for full frontier) all masks contained in breadcrumb staged ledgers are sequentially chained in a topology that matches the frontier's structure, ultimately rooted back to the snarked ledger stored in the root data

#### Frontier Read Interace

Each frontier must expose the following operations:

- find the root node in O(1)
- find the best tip node in O(1)
- find a node by block hash in O(1)
- access the successor hashes of a node in O(1)

Some other operations are also required, but these operations are more or less helpers on top of the operations described above.

#### Frontier State Transitions (Frontier Diffs)

There are 3 types of state transitions that can be performed on a frontier. Frontier diffs serve as a data representation format for these 3 types of state transitions, and does so in a way that allows for the diffs to specify state transitions on different types of frontier nodes (as not all frontiers store the same node type). The supported frontier state transitions are:

- add a node to the frontier
- transition the root to a successor
- update the best tip

When these diffs are applied to frontiers, they are applied in a more-or-less blind fashion (as in, the frontier applying the diffs is not checking that the state reached after the application will still hold all of the frontier's invariants). Instead, the responsibility for maintaining frontier invariants is on the function which computes the diffs to apply.

### Persistent Root

The persistent root stores and maintains the root snarked ledger of a frontier. This is the oldest ledger maintained by a frontier, and is persisted on-disk in the form of a RocksDB ledger. This ledger is loaded into the full frontier upon initialization and serves as the basis for ledger information for all ledgers maintained by the full frontier.

### Root Data

TODO

### Full Frontier

The full frontier is a fully expanded in memory frontier implementation. It is created from frontier root data and the root snarked ledger. The full frontier maintains a hash-indexed k-tree of breadcrumbs, implemented as a hashtable. Each breadcrumb in the frontier, including the root breadcrumb, consists of a block and a staged ledger associated with that block. The staged ledger's ledger state is built using ledger masks, where each ledger mask is chained off of the preceeding breadcrumbs staged ledger mask, and the root's staged ledger mask is chained off of the root's snarked ledger (the persistent root).

TODO: mask maintenance on root transition & mask chaining diagram

### Persistent Frontier

TODO: add the new rules here for izzy's root hack

The persistent frontier is an on-disk, limited representation of a frontier, along with a concurrent subsystem for synchronizing it with the full frontier's state. To maintain a reasonable level of disk I/O, the persistent frontier stores only blocks and not fully expanded breadcrumbs. It maintains neither the auxiliary scan state or the ledger required to construct the staged ledger. Instead, it relies on additional auxiliary information, "minimal root data", to also be available. This information is more expensive to write (larger) than the normal database synchronization operations, but occurs less often. Because the root data in the database is not necessarily kept in sync with the other information for the persistent frontier, and there is no guarantee that the persisted root (which is required for building the root staged ledger) will be in sync, it is important that the persistent frontier can recover from desynchronizations. The daemon attempts to always synchronize this data when it shuts down, but in the case of a crash, sometimes this will not happen.

The persistent frontier receives a notification every time diffs are applied to the full frontier. When this notification is received, the persistent frontier writes any diffs that were applied into a diff buffer. At a later point in time, this diff buffer is flushed, and all of the recorded diffs are performed against the persistent frontier's database. All diffs are processed in the buffer, but the auxiliary root data stored in the persistent frontier is only updated 1 time per flush.

![](./res/persistent_frontier_concurrency.dot.png)

#### Diff Buffer Flush Rules

The diff buffer parameterized with 3 values: the preferred flush capacity, the maximum capacity, and a maximum latency. The diff buffer will attempt to flush as soon as the flush capacity is exceeded, so long as there is not an active flush job. If there is an active flush job, the diff buffer will continue accumulating diffs until that job has succeeded, up until it reaches the maximum capacity, at which point the daemon will crash. To ensure that the persistent frontier is still updated even when there is a low amount of activity on the network, the diff buffer will also be flushed after the maximum latency has been exceeded.

#### Database Representation

The database supports the following schema:

| Key                                   | Args           | Value                       | Description |
|---------------------------------------|----------------|-----------------------------|-------------|
| `Db_version`                          | `()`           | `int`                       | The current schema version stored in the database. |
| `Root`                                | `()`           | `Root_data.Minimal.t`       | The auxiliary root data. |
| `Best_tip`                            | `()`           | `State_hash.t`              | Pointer to the current best tip. |
| `Protocol_states_for_root_scan_state` | `()`           | `Protocol_state.value list` | Auxilliary block headers required for constructing the scan state at the root |
| `Transition`                          | `State_hash.t` | `External_transition.t`     | Block storage by state hash. |
| `Arcs`                                | `State_hash.t` | `State_hash.t list`         | Successor hash storage by predecessor hash. |

#### Resynchronization

TODO

### Root History

TOOD

TODO: note about the fact it is currently an extension

### Extensions

TODO

### Consensus Hooks

TODO

### Transition Frontier

The transition frontier combines together the full frontier, persistent frontier, root history, and extensions into a single interface. It configures the root history to contain at most `2*k` previous roots, giving a total span of `3*k` blocks in the chains stored at any given time. This is done so that nodes can serve bootstrap requests (proofs of finality) to nodes within `2*k` blocks of the best tip.

![](./res/transition_frontier_diagram.conv.tex.png)

## Code Directory

TODO: extensions

| Name                                | File                                                                     | Description |
|-------------------------------------|--------------------------------------------------------------------------|-------------|
| Breadcrumb                          | [frontier\_base/breadcrumb.ml](./frontier_base/breadcrumb.ml)            | The breadcrumb data structure. |
| Frontier Interface                  | [frontier\_base/frontier\_intf.ml](./frontier_base/frontier_intf.ml)     | The external interface which frontiers must provide to. |
| Diff                                | [frontier\_base/diff.ml](./frontier_base/diff.ml)                        | The representation of frontier diffs. |
| Root Data                           | [frontier\_base/root\_data.ml](./frontier_base/root_data.ml)             | The representation of frontier root data, at varying levels of detail/size. |
| Root Identifier                     | [frontier\_base/root\_identifier.ml](./frontier_base/root_identifier.ml) | The representation of frontier root identifiers. |
| Full Frontier                       | [full\_frontier/full\_frontier.ml](./full_frontier/full_frontier.ml)     | The in memory, fully expanded frontier data structure. |
| Persistent Frontier Database        | [database.ml](./persistent_frontier/database.ml)                         | The RocksDB database that the persistent frontier is stored in. |
| Persistent Frontier Diff Buffer     | [diff\_buffer.ml](./persistent_frontier/diff_buffer.ml)                  | The diff buffer used as part of the persistent frontier synchronization subsystem. |
| Persistent Frontier Synchronization | [sync.ml](./persistent_frontier/sync.ml)                                 | The persistent frontier synchronization subsystem. |
| Persistent Frontier Worker          | [worker.ml](./persistent_frontier/worker.ml)                             | The persistent frontier synchronization subsystem worker. Responsible for applying diffs flushed from the diff to the persistent frontier database. |
| Persistent Frontier                 | [diff\_buffer.ml](./persistent_frontier/diff_buffer.ml)                  | The persistent frontier instance and singleton factory. |
| Transition Frontier                 | [transition\_frontier.ml](./transition_frontier.ml)                      | The library entrypoint which ties together all of the transition frontier concepts. |

## Future Plans

[RFC 0028](../../../rfcs/0028-frontier-synchronization.md) describes a long-term solution to a class of async race-conditions that are possible when consuming transition frontier extensions. We plan on implementing this work in the transition frontier at some point in the future.

As mentioned in the root history section, there is some tech debt to refactor the root history as an extension or rip it out of extensions altogether.

TODO: dump the state of desync recovery here
