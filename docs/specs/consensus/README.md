# Consensus

Mina uses [Ouroboros Samasika](https://eprint.iacr.org/2020/352.pdf) for consensus, hereafter referred to as Samasika.  The three fundamental guarantees delivered are
* High decentralization - Self-bootstrap, uncapped participation and dynamic availability
* Succinctness - Constant-time synchronization with full-validation and high interoperability
* Universal composability - Proven security for interacting with other protocols, no slashing required

Samasika extends the ideas from [Ouroboros Genesis](https://eprint.iacr.org/2018/378.pdf) and [Ouroboros Praos](https://eprint.iacr.org/2017/573.pdf) to the succinct blockchain setting, where the complexity of fully verifying the entire blockchain is independent of chain length.  The name Samasika comes from the Sanskrit word, meaning small or succinct.

This documents specifies required structures, algorithms and protocol details.

**Table of Contents**
* [1 Constants](#1-constants)
* [2 Structures](#2-structures)
  * [2.1 `External_transition`](#21-external_transition)
  * [2.2 `Protocol_state`](#22-protocol_state)
  * [2.3 `Consensus_state`](#23-consensus_state)
  * [2.4 `Epoch_data`](#24-epoch_data)
  * [2.5 Example block](#25-example-block)
* [3 Algorithms](#3-algorithms)
  * [3.1 Common](#31-common)
    * [3.1.1 `top`](#311-top)
    * [3.1.2 `cState`](#312-cstate)
    * [3.1.3 `globalSlot`](#313-globalslot)
    * [3.1.4 `epochSlot`](#314-epochslot)
    * [3.1.5 `length`](#315-length)
    * [3.1.6 `lastVRF`](#316-lastvrf)
    * [3.1.7 `stateHash`](#317-statehash)
  * [3.2 Chain selection](#32-chain-selection-rules)
    * [3.2.1 Short-range fork rule](#321-short-range-fork-rule)
    * [3.2.2 Long-range fork rule](#322-long-range-fork-rule)
  * [3.3 Decentralized checkpointing](#33-decentralized-checkpointing)
    * [3.3.1 `initCheckpoints`](#331-initcheckpoints)
    * [3.3.2 `updateCheckpoints`](#332-updatecheckpoints)
    * [3.3.3 `isShortRange`](#333-isshortrange)
  * [3.4 Window min-density](#34-window-min-density)
    * [3.4.1 `isWindowStop`](#341-iswindowstop)
    * [3.4.2 `shiftWindow`](#342-shiftwindow)
    * [3.4.3 `initSubWindowDensities`](#343-initsubwindowdensities)
    * [3.4.4 `updateSubWindowDensities`](#344-updatesubwindowdensities)
    * [3.4.5 `getMinDen`](#345-getminden)
* [4 Protocol](#4-protocol)
  * [4.1 Initialize consensus](#41-initialize-consensus)
  * [4.2 Select chain](#42-select-chain)
  * [4.3 Produce block](#43-produce-block)

**Conventions**
* We use the terms _top_ and _last_ interchangeably to refer to the block with the greatest height on a given chain
* We use the term _epoch slot number_ to refer to the intra-epoch slot number that resets to 1 every epoch
* We use _global slot number_ to refer to the global slot number since genesis starting at 1

**Notations**
* `a⌢b` - Concatenation of `a` and `b`
* `T[N]` - Array of type `T` containing `N` elements
* `T[v; N]` - Array of type `T` containing `N` elements of value `v`
* `x[i]` - Element `i` of array `x`, starting at index `0`
* `x[a..b]` - Slice of vector `x` containing elements from indexes `[a, b)`

# 1. Constants

These are the parameters Mina uses for Samasika

| Field | Value | Description |
| - | - | - |
| `delta`                         | `0`                     | Maximum permissable delay of packets (in slots after the current) |
| `k`                             | `290`                   | Point of finality (number of confirmations) |
| `slots_per_epoch`               | `7140`                  | Number of slots per epoch |
| `slots_duration`                | `180000` (= 3m)         | Slot duration in ms |
| `epoch_duration`                | `1285200000` (= 14d21h) | Duration of an epoch in ms |
| `genesis_state_timestamp`       | `1615939200000` (Mar 17, 2021 00:00:00 GMT+0000) | Timestamp of genesis block in unixtime |
| `acceptable_network_delay`      | `180000` (= 3m)         | Acceptable network delay in ms |
| `slots_per_sub_window`          | `7`                     | Slots per sub window (see [Section 3.4](#34-window-min-density)) |
| `sub_windows_per_window`        | `11`                    | Sub windows per window (see [Section 3.4](#34-window-min-density)) |

# 2. Structures

The main structures used in Mina consensus are as follows

## 2.1 `External_transition`

This is Mina's block structure.  In Mina blocks are synonymous with transitions.  A block received from a peer is referred to as an external transition and a block generated and applied locally is referred to as an internal transition.

| Field                           | Type                               | Description |
| - | - | - |
| `version`                       | `u8` (= 0x01)                      | Block structure version |
| `protocol_state`                | `Protocol_state.Value.Stable.V1.t` | The blockchain state, including consensus and the ledger |
| `protocol_state_proof`          | `Proof.Stable.V1.t sexp_opaque`    | Proof that the protocol state and entire history of the chain is valid |
| `staged_ledger_diff`            | `Staged_ledger_diff.Stable.V1.t`   | Diff of the proposed next state of the blockchain |
| `delta_transition_chain_proof`  | `State_hash.Stable.V1.t * State_body_hash.Stable.V1.t list` | Proof that the block was produced within the allotted slot time |
| `current_protocol_version`      | `Protocol_version.Stable.V1.t`        | Current protocol version |
| `proposed_protocol_version_opt` | `Protocol_version.Stable.V1.t option` | Proposed protocol version |

## 2.2 `Protocol_state`

This structure can be thought of like the block header.  It contains the most essential information of a block.

| Field                 | Type                     | Description |
| - | - | - |
| `version`             | `u8` (= 0x01)            | Block structure version      |
| `previous_state_hash` | `State_hash.Stable.V1.t` | Commitment to previous block (hash of previous protocol state hash and body hash)|
| `body`                | `Protocol_state.Body.Value.Stable.V1` | The body of the protocol state |

### 2.2.1 `Protocol_state.Body`

| Field                 | Type                     | Description |
| - | - | - |
| `version`             | `u8` (= 0x01)            | Block structure version |
| `genesis_state_hash`  | `State_hash.Stable.V1.t` | Genesis protocol state hash (used for hardforks) |
| `blockchain_state`    | `Blockchain_state.Value.Stable.V1.t` | Ledger related state |
| `consensus_state`     | `Consensus.Data.Consensus_state.Value.Stable.V1.t` | Consensus related state |
| `constants`           | `Protocol_constants_checked.Value.Stable.V1.t` | Consensus constants |

## 2.3 `Consensus_state`

This structure encapsulates the succinct state of the consensus protocol.  The stake distribution information is contained by the `staking_epoch_data` field.  Due to its succinct nature, Samasika cannot look back into the past to obtain ledger snapshots for the stake distribution.  Instead, Samasika implements a novel approach where the future stake distribution snapshot is prepared by the current consensus epoch.  Samasika prepares the past for the future!  This future state is stored in the `next_epoch_data` field.

| Field                                    | Type                     | Description |
| - | - | - |
| `version`                                | `u8` (= 0x01)            | Block structure version |
| `blockchain_length`                      | `Length.Stable.V1.t` | Height of block |
| `epoch_count`                            | `Length.Stable.V1.t` | Epoch number |
| `min_window_density`                     | `Length.Stable.V1.t` | Minimum windows density observed on this chain (see [Section 3.2.2](#322-long-range-fork-rule)) |
| `sub_window_densities`                   | `Length.Stable.V1.t list` | Current sliding window of densities (see [Section 3.4](#34-window-min-density)) |
| `last_vrf_output`                        | `Vrf.Output.Truncated.Stable.V1.t` | Additional VRS output from leader (for seeding Random Oracle) |
| `total_currency`                         | `Amount.Stable.V1.t` | Total supply of currency |
| `curr_global_slot`                       | `Global_slot.Stable.V1.t` | Current global slot number relative to the current hard fork  |
| `global_slot_since_genesis`              | `Mina_numbers.Global_slot.Stable.V1.t` | Absolute global slot number since genesis |
| `staking_epoch_data`                     | `Epoch_data.Staking_value_versioned.Value.Stable.V1.t` | Epoch data for previous epoch |
| `next_epoch_data`                        | `Epoch_data.Next_value_versioned.Value.Stable.V1.t` | Epoch data for current epoch |
| `has_ancestor_in_same_checkpoint_window` | `bool` | |
| `block_stake_winner`                     | `Public_key.Compressed.Stable.V1.t` | Compressed public key of winning account |
| `block_creator`                          | `Public_key.Compressed.Stable.V1.t` | Compressed public key of the block producer |
| `coinbase_receiver`                      | `Public_key.Compressed.Stable.V1.t` | Compresed public key of account receiving the block reward |
| `supercharge_coinbase`                   | `bool` | `true` if `block_stake_winner` has no locked tokens, `false` otherwise |

## 2.4 `Epoch_data`

| Field              | Type                     | Description |
| - | - | - |
| `version`          | `u8` (= 0x01)            | Block structure version |
| `ledger`           | `Epoch_ledger.Value.Stable.V1.t` | |
| `seed`             | `Epoch_seed.Stable.V1.t` | |
| `start_checkpoint` | `State_hash.Stable.V1.t` | State hash of _first block_ of epoch (see [Section 3.3](#33-decentralized-checkpointing))|
| `lock_checkpoint`  | `State_hash.Stable.V1.t` | State hash of _last known block in the first 2/3 of epoch_ (see [Section 3.3](#33-decentralized-checkpointing))|
| `epoch_length`     | `Length.Stable.V1.t` | |

## 2.5 Example block

This is an example of a Mina block in JSON format

```json
{
  "external_transition": {
    "protocol_state": {
      "previous_state_hash": "3NLKJLNbD7rBAbGdjZz3tfNBPYxUJJaLmwCP9jMKR65KSz4RKV6b",
      "body": {
        "genesis_state_hash": "3NLxYrjb7zmHdoFgBrubCN8ijM8v7eT8kvLiPLc9DHt3M8XrDDEG",
        "blockchain_state": {
          "staged_ledger_hash": {
            "non_snark": {
              "ledger_hash": "jxV4SS44wHUVrGEucCsfxLisZyUC5QddsiokGH3kz5xm2hJWZ25",
              "aux_hash": "UmosfM82dH5xzqdckXgA1JoAvJ5tLxch2wsty4sXmiEPKnPTPq",
              "pending_coinbase_aux": "WLo8mDN6oBUTSyBkFCy7Fky7Na5fN4R6oGq4HMf3YoHCAj4cwY"
            },
            "pending_coinbase_hash": "2mze7iXKwA9JAqVDC1MVvgWfJDgvbgSexKtuShdkgqMfv1tjATQQ"
          },
          "snarked_ledger_hash": "jx9171AbMApHNG1guAcKct1E6nyUFweA7M4ZPCjBZpgNNrE21Nj",
          "genesis_ledger_hash": "jxX6VJ84HaafrKozFRA4qjnni4aPXqXC2H5vQLKSryNpKTXuz1R",
          "snarked_next_available_token": "2",
          "timestamp": "1611691710000"
        },
        "consensus_state": {
          "blockchain_length": "3852",
          "epoch_count": "1",
          "min_window_density": "1",
          "sub_window_densities": [
            "3",
            "1",
            "3",
            "1",
            "4",
            "2",
            "1",
            "2",
            "2",
            "4",
            "5"
          ],
          "last_vrf_output": "g_1vrXSXLhvn1e4Ap1Ey5e8yh3PFMJT0vZyhZLlTBAA=",
          "total_currency": "167255800000001000",
          "curr_global_slot": {
            "slot_number": "12978",
            "slots_per_epoch": "7140"
          },
          "global_slot_since_genesis": "12978",
          "staking_epoch_data": {
            "ledger": {
              "hash": "jxX6VJ84HaafrKozFRA4qjnni4aPXqXC2H5vQLKSryNpKTXuz1R",
              "total_currency": "165950000000001000"
            },
            "seed": "2vb1Mjvydod6sEwn7qpbejKCfRqugMgyG3MHXXRKcAkwQLRs9fj8",
            "start_checkpoint": "3NK2tkzqqK5spR2sZ7tujjqPksL45M3UUrcA4WhCkeiPtnugyE2x",
            "lock_checkpoint": "3NK5G8Xqn1Prh3XoTyZ2tqntJC6X2nVwruv5mEJCL3GaTk7jKUNo",
            "epoch_length": "1769"
          },
          "next_epoch_data": {
            "ledger": {
              "hash": "jx7XXjRfJj2mGXmiHQmpm6ZgTxz14udpugyFtw4DefJFpie7apN",
              "total_currency": "166537000000001000"
            },
            "seed": "2vavBR2GfJWvWkpC7yGJQFnts18nHaFjdVEr84r1Y9DQXvnJRhmd",
            "start_checkpoint": "3NLdAqxtBRYxYbCWMXxGu6j1hGDrpQwGkBDF9QvGxmtpziXQDADu",
            "lock_checkpoint": "3NL4Eis1pS1yrPdfCbiJcpCCYsHuXY3ZgEzHojPnFWfMK9gKmhZh",
            "epoch_length": "2084"
          },
          "has_ancestor_in_same_checkpoint_window": true,
          "block_stake_winner": "B62qpBrUYW8SHcKTFWLbHKD7d3FqYFvGRBaWRLQCgsr3V9pwsPSd7Ms",
          "block_creator": "B62qpBrUYW8SHcKTFWLbHKD7d3FqYFvGRBaWRLQCgsr3V9pwsPSd7Ms",
          "coinbase_receiver": "B62qpBrUYW8SHcKTFWLbHKD7d3FqYFvGRBaWRLQCgsr3V9pwsPSd7Ms",
          "supercharge_coinbase": true
        },
        "constants": {
          "k": "290",
          "slots_per_epoch": "7140",
          "slots_per_sub_window": "7",
          "delta": "0",
          "genesis_state_timestamp": "1609355670000"
        }
      }
    },
    "protocol_state_proof": "<opaque>",
    "staged_ledger_diff": "<opaque>",
    "delta_transition_chain_proof": "<opaque>",
    "current_protocol_version": "1.1.0",
    "proposed_protocol_version": "<None>"
  }
}
```

# 3. Algorithms

This section outlines the main algorithms and constructs used by Samasikia.

## 3.1 Common

This section outlines some commonly used helpers.

### 3.1.1 `top`

This function returns the last block of a given chain.  The input is a chain `C` and the output is last block of `C` (i.e. the block with greatest height).

```rust
fn top(C) -> Block
{
   return last block of C
}
```

### 3.1.2 `cState`

The function returns the consensus state of a block or chain.  The input is a block or chain `X` and the output is the consensus state.

```rust
fn cState(X) -> Consensus_state
{
    match X {
        Block => X.protocol_state.body.consensus_state,
        Chain => {
            cState(last block of X)
        }
    }
}
```

### 3.1.3 `globalSlot`

The function returns the _global slot number_ of a chain or block.  The input `X` is either a chain or block and the output is the global slot number.

```rust
fn globalSlot(X) -> u64
{
    return cState(X).curr_global_slot
}
```

### 3.1.4 `epochSlot`

The function computes the _epoch slot number_ of a block.  The input is the block `B` and the output is the epoch slot number in `[0, slots_per_epoch]`.

```rust
fn epochSlot(B) -> u32
{
   return globalSlot(B) mod slots_per_epoch
}
```

### 3.1.5 `length`

The function the length of a chain.  The input is the global chain `C` and the output is the length of the chain in blocks.

```rust
fn length(C) -> u64
{
   return cState(C).blockchain_length
}
```

### 3.1.6 `lastVRF`

This function returns the hex digest of the hash of the last VRF output of a given chain.  The input is a chain `C` and the output is the hash digest.

```rust
fn lastVRF(C) -> String
{
   return Digest(Blake2b(cState(C).last_vrf_output))
}
```

### 3.1.7 `stateHash`

This function returns hash of the top block's consensus state for a given chain.  The input is a chain `C` and the output is the hash.

```rust
fn stateHash(C) -> Hash
{
   return Blake2b(cState(C))
}
```

## 3.2 Chain selection rules

Samasika uses two consensus rules: one for *short-range forks* and one for *long-range forks*.

### 3.2.1 Short-range fork rule

This rule is triggered whenever the fork is such that the adversary has not yet had the opportunity to mutate the block density distribution.

```rust
Choose the longest chain
```

A fork is short-range if it occured less than `m` blocks ago.  The naı̈ve implemention of this rule is to always store the last `m` blocks, but for a succinct blockchain this is not desirable.  Mina Samasika adopts an approach that only requires information about two blocks.  The idea is a decentralized checkpointing algorithm, the details of which are given in [Section 3.3](#33-decentralized-checkpointing).

### 3.2.2 Long-range fork rule

Recall that when an adversary creates a long-range fork, over time it skews the leader selection distribution leading to a longer adversarial chain.  Initially the dishonest chain will have a lower density, but in time the adversary will work to increase it.  Thus, we can only rely on the density difference in the first few slots following the fork, the so-called *critical window*.  The idea is that in the critical window the honest chain the density is overwhelmingly likely to be higher because this contains the majority of stake.

As a succint blockchain, Mina does not have a chain into which it can look back on the fork point to observe the densities.  Moreover, the slot range of the desired densities cannot be know ahead of time.

Samasika overcomes this problem by storing a succinct summary of a sliding window of slots over each chain and then tracks the *minimum* of all densities observed for each sliding window.  The intuition is that if the adversary manages to increase the density on the dishonest chain, the tracked minimum density still points to the critical window following the fork.

[Section 3.4](#34-window-min-density) specifies how the sliding windows are tracked and how the minimum density is computed.  For now, we assume that each chain contains the minimum window density and describe the main idea of the long-range fork rule.

Given chain `C` let `C.min_density` be the minimum density observed in `C` so far.

Let `C1` be the local chain and `C2` be a [valid](../verification/README.md#1.1-isvalidchain) alternative chain; the main idea of the _long-range fork rule_ is

```rust
if C2.min_density > C1.min_density {
    Select C2
}
else {
    Continue with C1
}
```

The above pseudocode is only to provide intuition about how the chain selection rules work.  The actual chain selection algorithm is specified in [Section 4.2](#42-select-chain) and is designed to handle more complex cases

## 3.3 Decentralized checkpointing

<!--
; start_checkpoint: 'start_checkpoint
      (* The lock checkpoint is the hash of the latest state in the seed update range, not including
         the current state. *)
; lock_checkpoint: 'lock_checkpoint
-->

Samasika uses decentralized checkpointing to determine whether a fork is short- or long-range.  The idea is that each chain maintains two checkpoints in every epoch, which are used to estimate how long ago a fork has occured.

* **Start checkpoint** - First block of the epoch
* **Lock checkpoint** - Last known block in the first `2/3` of an epoch

```
epochs:         e1                e2                 e3
                                      ⤺lock
slots:  s1s2s3s4s5s6s7s8s9|s1s2s3s4s5s6s7s8s9|s1s2s3...
                     start⤻⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
```

These are located in the `start_checkpoint` and `lock_checkpoint` fields of the [`Epoch_data`](#24-epoch_data) structure, which is part of the [`Consensus_state`](#23-consensus_state) (See [Section 2.4](#24-epoch_data)).

As time progresses away from the first slot of the current epoch, the lock checkpoint is pushed along with the last known block until we reach the last block in the first `2/3` of the epoch and it is _frozen_. ❄

A fork is considered _short-range_ if either

1. the fork point of the candidate chains are in the same epoch
2. or the fork point is in the previous epoch with the same `lock_checkpoint`

Since the leader selection distribution for the current epoch is computed by the end of the first `2/3` of the slots in the previous epoch, an adversarial fork after the previous epoch's `lock_checkpoint` cannot skewed the distribution for the remainder of that epoch, nor the current epoch.  Anything before the previous epoch's `lock_checkpoint` _long-range_ fork.

Since Mina is succinct this means that it must stored the checkpoints for the current epoch in addition to the checkpoints for the previous epoch.  This is why the [`Consensus_state`](#23-consensus_state) structure contains two `Epoch_data` fields: `staking_epoch_data` and `next_epoch_data`.  The former contains the checkpoints for the previous epoch and the latter contains that of the current epoch.

### 3.3.1 `initCheckpoints`

This algorithm initializes the checkpoints for genesis block `G`

```rust
fn initCheckpoints(G) -> ()
{
    state_hash = hash(latest state ϵ S.next_epoch_data.seed's update range) ?
    cState(G).staking_epoch_data.lock_checkpoint = 0 (or empty hash?)
    cState(G).staking_epoch_data.start_checkpoint = 0 ?
    cState(G).next_epoch_data.start_checkpoint = state_hash ?
    cState(G).next_epoch_data.lock_checkpoint =  state_hash ?
}
```

### 3.3.2 `updateCheckpoints`

This algorithm updates the checkpoints of the block being created `B` based on its parent block `P`.  It inputs the blocks `P` and `B` and updates `B`'s checkpoints according to the description in [Section 3.3](#33-decentralized-checkpointing).

```rust
fn updateCheckpoints(P, B) -> ()
{
    state_hash = hash(latest state ϵ SP.next_epoch_data.seed's update range) ?
    if epochSlot(B) == 0 then
        cState(B).next_epoch_data.start_checkpoint = state_hash

    if 0 ≤ epochSlot(B) < 2/3*slots_per_epoch {
        cState(B).next_epoch_data.lock_checkpoint = state_hash
    }
}
```
Specifically, if the epoch slot of the new block `B` is the start of a new epoch, then the `start_checkpoint` of the current epoch data (`next_epoch_data`) is updated to the state hash from the previous block `P`.  Next, if the the new block's slot is also within the first `2/3` of the slots in the epoch ([`slots_per_epoch`](#1-constants)), then the `lock_checkpoint` of the current epoch data is also updated to the same value.

### 3.3.3 `isShortRange`

This algorithm uses the checkpoints to determine if the fork of two chains is short-range or long-range.  It inputs two chains with a fork `C1` and `C2` and outputs `true` if the fork is short-range, otherwise the fork is long-range and it outputs `false`.

```rust
fn isShortRange(C1, C2) -> bool
{
    if cState(C1).staking_epoch_data.lock_checkpoint == cState(C2).staking_epoch_data.lock_checkpoint {
        return true
    }
    else {
        return false
    }
}
```

## 3.4 Window min-density

This section describes how to compute the density windows and minimum density. Firstly we must define some terminology.

* We say a slot is _`filled`_ if it contains a valid non-orphaned block
* An `n-window` is a sequential list of slots s<sub>1</sub>,...,s<sub>n</sub> of length `n`
* The _`density`_ of a window is the number filled slots filled within it

The _`sliding window`_ is referred to as a `v`-shifting `w`-window and it characterisd by two parameters.

| Parameter | Description                                | Value |
| - | - | - |
| `v`       | Length by which the window shifts in slots (shift parameter) | [`slots_per_sub_window`](#1-constants) (= 7) |
| `w`       | Window length in slots                                       | [`slots_per_sub_window`](#1-constants)` * `[`sub_windows_per_window`](#1-constants) (= 7*11 = 77 slots) |

This is a `w`-long window that shifts `v`-slots at a time.  You can think of the `w`-length window as being comprised of `k` sub-windows (`sub_windows_per_window`), each of length `v` slots.  For the parameters given in the table above, the sliding window looks like this:

```
   |s1,...,s7|s8,...,s14| ... |s71,...,s77|
k:      1          2      ...      11
```
where `si` is slot `i`.

Samasika tracks the list of densities of the previous `k = 11` sub-windows and the current window density `dc`

```
                      |s1,...,s7|s8,...,s14| ... |s71,...,s77|s78,...
sub_window_densities:      d1        d2      ...       dk          dc
```

The value of `k` is defined by the [`sub_windows_per_window`](#1-constants) constant.

This list of window of densities is stored in each block, in the `sub_window_densities` field of the `Consensus_state` (see [Section 2.3](#23-consensus_state)).  This field is of type `Length.Stable.V1.t list` and because it must be written into blocks as part of the protocol, an implimentation MUST implement serialization for this type.

The values stored in `sub_window_densities` have this format.

| Index | Contents |
| - | - |
| `0` | Oldest window density |
| `...` | |
| `k - 1` | Previous window density |
| `k` | Current window density |

Each block also stores the minimum window density, found in the `min_window_density` field of the `Consensus_state` (see [Section 2.3](#23-consensus_state)).

### 3.4.1 `isWindowStop`

This algorithm detects whether we have reached the end of a sub-window.  It is used to decide if we must perform a `v`-shift.  It takes as input the current epoch slot number `s`, the shift parameter `v` and outputs `true` if we have reached the end of a sub-window and `false` otherwise.

```rust
fn isWindowStop(s, v) -> bool
{
    if s mod v == 0 {
        return true
    }
    else {
        return false
    }
}
```

### 3.4.2 `shiftWindow`

<!--
This algorithm is responsible for shifting the sliding window.  It inputs the sliding window `W` = [d<sub>1</sub>,...,d<sub>w</sub>] and the current sub window densities `C = [d<sub>w+1</sub>,...,d<sub>w+v</sub>] and then outputs the result `W'` = [d<sub>1+v</sub>,...,d<sub>w+v</sub>], where `v` is the shift parameter.

>```rust
fn shiftWindow(W, C) -> W'
{
    return W[4..]⌢C
}

-->

This algorithm is responsible for shifting the sliding window.  It inputs the sub-window densities `D` = [d<sub>0</sub>,...,d<sub>k</sub>] and then outputs the result `D'` = [d<sub>1</sub>,...,d<sub>k</sub>].

```rust
fn shiftWindow(D) -> D'
{
    return D[1..]
}
```

### 3.4.3 `initSubWindowDensities`

This algorithm initializes the sub-window densities and minimm window density for genesis block `G`

<!-- cState(G).sub_window_densities = u32[0]⌢u32[slots_per_window; sub_windows_per_window - 1] -->

```rust
fn initSubWindowDensities(G) -> ()
{
    cState(G).sub_window_densities = u32[0, slots_per_window, slots_per_window, ..., slots_per_window]
    //                                      \_______________________________________________________/
    //                                                   sub_windows_per_window - 1 times

    cState(G).min_window_density = slots_per_window
}
```

### 3.4.4 `updateSubWindowDensities`

This algorithm updates the sub-window densities of the block being created `B` based on its parent block `P`.  It inputs the blocks `P` and `B` and updates `B`'s sub window densities according to the description in [Section 3.4](#34-window-min-density).

```rust
fn updateSubWindowDensities(P, B) -> ()
{
    cState(B).sub_window_densities = cState(P).sub_window_densities
    cState(B).sub_window_densities[-1] += 1
}
```

### 3.4.5 `getMinDen`

This function returns the current minimum density of a chain.  It inputs a chain `C` and the `max_slot` observed between `C`  and the alternative chain (See [selectSecureChain](#42-select-chain)).

```rust
fn getMinDen(C, max_slot) -> density
{
    if globalSlot(C) == max_slot {
        return cState(C).min_window_density
    }
    else {
        prev_global_sub_window = globalSlot(C) / slots_per_sub_window
        prev_relative_sub_window = prev_global_sub_window mod sub_windows_per_window

        next_global_sub_window = max_slot / slots_per_sub_window
        next_relative_sub_window = next_global_sub_window mod sub_windows_per_window

        if prev_global_sub_window == next_relative_sub_window {
            // Same sub window
        }

        if prev_global_sub_window + sub_windows_per_window >= next_global_sub_window {
            // Same window
        }
     }
}
```

# 4 Protocol

This section specifies the consensus protocol in terms of events and how they MUST be implemented by a compatible peer.  The required events are:
* [`Initialize consensus`](#41-initialize-consensus)
* [`Select chain`](#42-select-chain)
* [`Produce block`](#43-produce-block)

Additionally there are certain local data members that all peers MUST maintain in order to participate in consensus.

| Parameter   | Description |
| - | - |
| `genesis_block` | The initial block in the blockchain |
| `neighbors`     | Set of connections to neighboring peers |
| `chains`        | Set of known (succinct) candidate chains |
| `tip`           | Currently selected chain according to the chain selection algorithm (i.e. secure chain) |

How these are represented is up to the implementation, but careful consideration must be given to scalability.

In the following description we use _dot notation_ to refer the local data members of peers. For example, given peer `P`, we use `P.genesis_block` and `P.tip`, to refer to the genesis block and currently selected chain, respectively.

## 4.1 Initialize consensus

Things a peer MUST do to initialize consensus includes
* Load the genesis block
* Get head of the current chain
* Decide if peer should bootstrap or sync

## 4.2 Select chain

The _select chain_ event occurs every time a peer's chains are updated.  A chain is said to be _updated_ anytime a valid block is added or removed from its head.  All compatible peers MUST select chains as described here.

Assuming an update to either `P.tip` or `P.chains`, the peer `P` must update its `tip` like this

```rust
P.tip = selectSecureChain(P.tip, P.chains)
```
The `selectSecureChain` algorithm, presented below, takes as input the peer's current best chain `P.tip` and its set of known valid chains `P.chains` and outputs the most secure chain according to the [Chain Selection Rules Section](#32-chain-selection-rules) described in [Section 3.2](#32-chain-selection-rules).

In addition to the high-level idea given in Section 3.2, the algorithm employs some additional tiebreak logic when comparing chains of equal length or equal minimum density.

```rust
fn selectSecureChain(tip, chains) -> Chain
{
    // Compare each candidate in chains with best tip
    for candidate in chains {
        if isShortRange(candidate, tip) {
            // short-range fork, select longer chain
            tip = selectLongerChain(tip, candidate)
        }
        else {
            // long-range fork, compare minimum window densities
            max_slot = max(globalSlot(tip), globalSlot(candidate))
            if getMinDen(candidate, max_slot) > getMinDen(tip, max_slot) {
                tip = candidate
            }
            else if getMinDen(candidate, max_slot) == getMinDen(tip, max_slot) {
                // tiebreak
                tip = selectLongerChain(tip, candidate)
            }
        }
    }

    return tip
}
```

It relies on the [`isShortRange`](#333-isshortrange) and [`getMinDen`](#345-getminden) algorithms (Section 3.3.3 and Section 3.4.5) and the `selectLongerChain` algorithm below.

```rust
fn selectLongerChain(tip, candidate) -> Chain
{
    if length(tip) < length(candidate) {
        return candidate
    }
    // tiebreak logic
    else if length(tip) == length(candidate) {
        // compare last VRF digests lexographically
        if lastVRF(candidate) > lastVRF(tip) {
            return candidate
        }
        else if lastVRF(candidate) == lastVRF(tip) {
            // compare consensus state hashes lexographically
            if stateHash(candidate) > stateHash(tip) {
                return candidate
            }
        }
    }

    return tip
}
```

## 4.3 Produce block
