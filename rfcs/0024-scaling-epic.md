# Scaling Epic RFC

## Summary
[summary]: #summary

There are a number of scaling goals which we would like to achieve, and a number of dependencies to scale the components required to achieve these goals. This RFC will serve as an Epic, describing the various scaling goals and the work that needs to be done to achieve them. From this RFC, a number of sub-RFCs will be created and linked back here. The scope of this RFC is only to lay out what needs to be done and describe the dependencies of the various work at a high level in order to coordinate discussions across the sub-RFCs.

## Dependency Graph

![](res/scaling_dependencies.dot.png)

#### Legend
| Item                  | Represents                   |
|-----------------------|------------------------------|
| Black Circle Nodes    | Scaling Goals                |
| Black Rectangle Nodes | Actionable Dependencies      |
| Blue Square Nodes     | Protocol Aspect Dependencies |
| Black Arcs            | Positive Correlation         |
| Red Arcs              | Negative Correlation         |

#### [Notion Table](https://www.notion.so/codaprotocol/bea78f9f670b4cef9c4e557bdd38981b?v=609389a5e4844fb9b3aa2777d735da06)

The notion table summarizes the dependencies in this graph with priorities. Priorities on the table should remain in sync with the priorities listed in this document.

## Scaling Goals
[scaling]: #scaling

### Number of Accounts in the Ledger
We need to scale the number of accounts in the ledger that the network can handle to at minimum 100k, and ideally somewhere in the range of 100m-10b.

### Transactions Per Second (TPS)
The network needs to support a reasonable TPS value. At minimum, TPS should be at least 2.

### Block Latency
Block latency refers to how long it takes for blocks to be added to the blockchain. Optimizing block latency helps increase the usability of the cryptocurrency by decreasing the amount of time until a wallet can probabilistically display to the user that their transactions have gone through/funds have been received. Decreasing block latency reduces the delay for changes in the stake distribution to take effect with respect to VRF evaluations, and delegation.

### Number of Delegators on one Account
Increasing the maximum number of delegators on one account allows for larget delegation pools to exist, which is important for usability of delegation since changes in delegation take a long time to take effect. If there is a low cap on the maximum number of delegators to a single account, delegation pools will need to be split up, and if a pool is overloaded and needs to offload onto another account, it will remain overloaded for a long time.

## Protocol Aspect Dependencies
[protocol-aspect-dependencies]: #protocol-aspect-dependencies

### Slot Time
As the slot time decreases, TPS can be increases and block latency decreases. However, slot time decreasing also reduces the maximum number of delegators on a single account since that means that there is less time to perform all the evaluations for a single slot. Put another way, the amount of time it takes to perform all the VRF evaluations for a single slot needs to always be less than the length of a slot, otherwise the node performing the VRF evaluations will not find slots it can propose in before the slot time is up.

### Scan State Size
As the scan state size increases, it increases TPS since it means that more transactions can be included in a single block. However, increasing the scan state size increases block latency. Specifically, it doesn't increase the amount of time it takes for a block to be finalized (or the probalistic model around finalization used by wallets), but it does increase the delay till stake distribution changes take effect. The scan state size being large also negatively impacts staged ledger diff application time, which in turn effects slot time since a multiple staged ledger diff applications need to be done within a single slot.

## Actionable Dependencies
[actionable-dependencies]: #actionable-dependencies

Actionable dependencies are prioritized on a scale of 1-3, 1 being most important, 3 being least important. All of these have to be done, but some of them have a wider impact or will take more time than others. Each of these actionable items will have their own RFC which is linked back to this RFC before it is closed.

### Optimize Epoch Ledger Synchronization
##### Priority: 1
##### Why this Priority?
This is the primary blocker in scalilng the number of accounts in the ledger.
##### Why is it important?
The amount of bandwidth used during epoch ledger synchronization is currently ~O(n^2) with respect to the number of accounts in the ledger. Furthermore, this data is currently sent in a single network request from a single peer. As such, this is the primary factor which will prevent us from scaling the number of accounts in the ledger.
##### Proposed Fix
Synchronize the Epoch Ledger using the same Syncable Ledger abstraction we use for synchronizing the Root Snarked Ledger of the Transition Frontier.
##### Child RFC: &#x1F534;

### Reduce Epoch Ledger Memory Usage
##### Priority: 3
##### Why this Priority?
Less important than optimizing epoch ledger synchronization time/bandwidth usage.
##### Why is it important?
Epoch ledgers are currently stored in memory, in their entirety, with no shared memory between the data structures or what is already stored on disk. This means that 2 full copies of the ledger are stored in memory, which puts insane memory requirements on consensus nodes on the network when the ledger is large.
##### Proposed Fix
There are multiple potential fixes that need to be weighed and compared for implementation time and resulting improvements. Here are some examples:
- persistent backwards chained masks (same as above, but on disk with an in-memory cache for relevant staking accounts)
- content addressed storage system (see @enolan's RFC for details)
##### Child RFC: &#x1F534;

### Reduce Scan State Memory Usage
##### Priority: 1
##### Why this Priority?
This is important and will cause issues at some point, but it looks like this will show itself as a bottleneck later than most of the other scaling dependencies.
##### Why is it important?
At high parameterization, the scan state will use an increadible amount of memory. The memory usage needs to be before we can significantly turn up the scan state's size.
##### Proposed Fix
There are a number of things that can be done in order to reduce the memory usage of the scan states in the transition frontier:
1. Use content addressed storage to share references to memory across transaction witnesses (which potentially saves a lot of duplicate intermediate hashes from being stored).
2. Only store scan state witnesses at the tips of the transition frontier, throwing out witnesses on scan states where there are already known successor blocks. This can also be combined with a probalistic "dead fork" algorithm which will prune witnesses from tips in the frontier which we don't think will be extended. If the tips that are pruned are extended later, we can always rebuild new witnesses.
3. If this is not sufficient, it is possible to persist scan states that have not been accessed recently in order to free them from memory, and reload them into memory when they are accessed.
##### Child RFC: &#x1F7E1; [Reduce Scan State Memory Usage](https://github.com/CodaProtocol/coda/pull/3980)

### Scan State Merklization
##### Priority: 1
##### Why this Priority?
Doing this will make some (potentially major) changes to the scan state's structure, so it should be done before any other major scan state changes are made. It's the first dependency for reducing scan state memory usage, which is a high priority.
##### Why is it important?
Scan state merklization will allow the scan state to be synchronized using what is currently the "sync ledger" abstraction (which will be modified to be a "merkle sync" abstraction instead; this part should be pretty easy). This helps divorce the scan state single slot synchronization requirement, while also reducing the amount of data a single node needs to provide to clients synchronizing their scan state during bootstrap. This also has potential applications in the non-consensus protocol since it allows nodes to generate merkle proofs of transactions in the staged ledger.
##### Proposed Fix
Wrap the current scan state structure with merklization logic, such that every node hash a hash which is a hash of it's contents. This allows the scan state to be plugged in as a merkle tree to other parts of the code base, for instance, "sync ledger". The scan state's structure doesn't exactly follow the same structure as a merkle tree since all of the intermediate nodes contain data in adition to pointers to their children, but it should be rather easy to write a more generalized "merkle DAG" or "dataful merkle tree" abstraction which is a more generic interface into a merklized tree structure (and both "merkle ledgers" and "merkle scan states" would implement this interface).
##### Child RFC: &#x1F534;

### Reduce Scan State Synchronization Time
##### Priority: 2
##### Why this Priority?
Depends on scan state merklization.
##### Why is it important?
The current implementation of scan state synchronization downloads a single bin\_io serialized copy of an entire scan state from a single peer. This does not scale as the scan state gets bigger since it puts too much bandwidth pressure on a signle peer, and long network downloads introduce more likelihood for network failures (which will require a full restart of the download to recover from).
##### Proposed Fix
Once the scan state is merklized, the scan state can be plugged into the "sync ledger" abstraction in order to synchronize only the diff of data against an existing structure and download the data in chunks from multiple peers at once (increasing speed, reducing bandwidth usage, and reducing the likelihood of network failures).
##### Child RFC: &#x1F534;

### Divorce Scan State Synchronization from the Single Slot Requirement
##### Priority: 2
##### Why this Priority?
Depends on scan state merklization.
##### Why is it important?
As the total size of the scan state increases, the existing single slot requirement will require the slot time to be increased to still allow the scan state to be downloadable.
##### Proposed Fix
Once the scan state is merklized, the scan state can be plugged into the "sync ledger" abstraction. This fixes the issue since the "sync ledger" supports dynamic retargetting of the data you are synchronizing.
##### Child RFC: &#x1F534;

### Optimize Staged Ledger Diff Application
##### Priority: 2
##### Why this Priority?
Staged ledger diff application is likely to be one of the major computational bottlenecks for consensus nodes participating on the network, regardless of snark working and block production.
##### Why is it important?
Staged ledger diff application has to be short enough that it can be done multiple times within a slot, otherwise the nodes on the conesnsus network are at risk of not being able to keep up with block production. Furthermore, there still needs to be enough time in the slot to produce blocks.
##### Proposed Fix
There are a number of mostly independent optimizations we can implement for staged ledger diff application (the sub-RFC should attempt to rank these by cost/benefit):
1. Perform witness generation hashes in parallel with no duplicates (see parallel poseidon hashing for details)
2. Batch verification of included completed work proofs
3. Cache completed work verification to avoid duplicate verification of completed work included across multiple forks
4. If transaction signature verification cost is significant, parallelize that as well
##### Child RFC: &#x1F7E1; [Transaction SNARK Bundling](https://github.com/CodaProtocol/coda/pull/3981)

### Optimize Snarked Ledger Commit
##### Priority: 2
##### Why this Priority?
As the ledger size increases, snarked ledger commit will likely become the main bottleneck for slot time in addition to staged ledger diff application. Making the slot time too long is not much of an option, so this will instead become the primary bottleneck for the number of accounts in the system.
##### Why is it important?
It's important that the amount of time it takes for a new snarked ledger to be commited to the root snarked ledger must be short enough that all nodes can realistically do it 1 time within a slot while still handling the multiple staged ledger diff applications it also must perform within the same time frame. Snarked ledger commit time increases as the scan state increases and as the spread of txns across accounts diversifies. The scan state being large means there are more transactions included in every snarked ledger commit, and there is more information to commit to the root snarked ledger as the number of accounts mutated by the full set of transactions included increases.
##### Proposed Fix
Tightly optimize the bulk writes to the rocksdb database. Perform mask hashing in parallel (if poseidon hashing can be parallelized out of process well) when constructing the mask to be committed to the database. Potentially split I/O out to separate process if it makes sense (data transfer to the other process might be too high though). If all of this is not enough, then we need to divorce the requirement that the snarked ledger commit happense in one slot. This is doable, but is tricky, as it involves keeping a mask in memory on top of the persisted root snarked ledger, querying that mask while it is active instead of the underlying database, and periodically writing (and invalidating) some information from the mask asynchronously. This will be rather tricky as it needs to be done in a way that is fault tolerant and blocks asynchronous reads as little as possible.
##### Child RFC: &#x1F534;

### Poseidon Hashing Time
##### Priority: 1
##### Why this Priority?
Potentially contractable work from SIMD experts, so RFC should be written ASAP.
##### Why is it important?
Poseidon hashing time is our primary computational bottleneck with respect to staged ledger diff application. The amount of time it takes to apply a staged ledger has an effect on both how many transactions we can include in a block, as well how short/long a slot can be. The number of transaction we can include in a block has a direct relationship to the size of the scan state that we choose (and if we choose to only limit the max number of transactions in a block by the maximum which can be included in a scan state at once, then the relationship is bidirectional). The staged ledger diff application time effects the length of a block since every consensus node on the network needs to apply the staged ledger diff for every block it hears about, and if that takes too long, short slot times could make block production rate such that nodes on the network could not keep up.
##### Proposed Fix
One potential way to address this is to have an efficient, non-OCaml process which is responsible for computing hashes in parallel. The deamon can build a DAG of hashes to perform, send it to the hashing process, and continue performing other async tasks while it awaits responses for those hashes. The hash responses could either stream back into the OCaml process, or be sent all in one response. We need to analyze the potential level of parallelization we can achieve during staged ledger diff application to weigh how much of a speed up this method would provide us and see if it is worth doing before implementing it, especially as we may contract an external expert to do at least part of this for us.
##### Child RFC: &#x1F534;

### Reduce VRF Evaluation Time for Many Accounts
##### Priority: 3
##### Why this Priority?
Scaling the number of delegators on one account is not as important as TPS or number of accounts.
##### Why is it important?
The current implementation of sequentially evaluating all VRFs for all slots until the first match is found and then stopping until a block is proposed will not scale well as we put more delegators (or keypairs) to evaluate VRFs for. Ideally, we want delegation to scale as much as possible.
##### Proposed Fix
Perform VRF evaluations in parallel, and also continue to do so asynchronously to block production. Side note: asynchronous VRF evaluations are already a requirement for the API.
##### Child RFC: &#x1F534;
