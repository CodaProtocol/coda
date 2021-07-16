## Summary
[summary]: #summary

I propose creating a new, generic, system for storing arbitrary Merkleized data.
It would unify and replace the existing system of mask, database, null, and
sparse ledgers, as well as the Merkle lists we use.

## Motivation

[motivation]: #motivation

The current system is complex, error prone and hard to debug. It also doesn't
exploit redundancy if you don't explicitly use masks. For example we expect the
epoch and snarked ledgers to be very similar, there is probably a power law
distribution on how active accounts are, the vast majority will not have changed
at all since the epoch snapshot was taken. But in the current system it'd be
very difficult to exploit that.

## Detailed design

[detailed-design]: #detailed-design

The system is composed of two layers. At the bottom is the Merkle storage system
itself. It stores and tracks immutable hashed data, both on disk and in RAM,
using reference counting to free storage appropriately. There is only one copy
of any item with a given hash, so sharing is automatic and implicit. The primary
object in storage is a `Ref` (name pending). A `Ref` represents some hashed
data which may reference other data by hash, if you have a `Ref`, then you have
the data with the associated hash. The `Ref` type has two parameters, A) a
phantom that is either `full` or `partial` indicating whether we are guaranteed
to have all the referenced data as well, and B) what the type of the data is.
There is a function that takes a `Ref` and ensures that it and all its
references are stored on disk. We call this "pinning" the `Ref`.

In the table below, the rows correspond to whether the data is pinned and the
columns correspond to the value of the "fullness" type parameter. The inner
elements are what those combinations are in the current system (for ledgers).

|        | Full                             | Partial         |
|--------|----------------------------------|-----------------|
| Disk   | `Merkle_ledger.Database`         | N/A             |
| Memory | `Ledger.Mask.Attached` (to null) | `Sparse_ledger` |

Most (maybe all?) of the in-memory ledgers we use in the daemon are actually
masks on top of a database ledger, which doesn't fit into the new model
described in this RFC.

The system for synchronizing Merkle structures over the network exists at this
level, replacing `syncable_ledger`.

On top of the underlying storage layer are the actual Merkle structures. In
order to store an Merkle object, the programmer writes a module that describes
the object, how to (de)serialize it, and how to find its references. They pass
that object into a functor and get the `Ref` machinery, specialized to the type
in question, back. When implementing this, we'd write a module that describes
the complete binary tree that is our ledger - the result of the functor would be
used to build something close to `Base_ledger_intf.S`. We could also use this
for a Merkleized parallel scan state, or to replace
`receipt_chain_database`.

### Storage management

There are two tables mapping hashes to objects: an in-memory hashtable and
RocksDB on disk. The in-memory table contains *weak* references, and entries are
removed from it by a finalizer when the associated data is garbage collected. We
can rely on OCaml's garbage collector for the in-memory data, but for the on
disk data we'll need to use reference counting and delete objects from the
database when their number of incoming pinned references goes to zero. (If they
have >0 memory references and 0 pinned references they need to be read from disk
into memory before being deleted from disk.)

A `Ref` can be constructed by providing the data it corresponds to and either
the hashes of or `Ref`s to anything it references. Or, you can look them up by
hash, and get a `Ref` back if it's already stored. When a `Ref` is created anew,
the object is *not* hashed immediately. It is instead added to a queue of
unhashed data that is discharged every major GC by hashing the items that are
still live and adding them to the hashtable. The principle here is that we may
make many changes to a structure in sequence and only care about the hash at the
end. The intermediate structures will be garbage collected, and the only live
reference in the queue will be to things we are currently holding references to.
There is a time-memory trade off here - hashing allows deduplication but takes
time. If we didn't want deduplication we could defer hashing until we actually
need the hash. But we do, hence the GC driven compromise. The queue must also be
drained whenever we look up an object by hash.

### Public key -> Merkle address mapping

There is a problem with this scheme however. Because our ledger is unordered,
you need the mapping of public keys to addresses in the tree in order to
efficiently read from it. So a `Ref` to the ledger alone is insufficient, we
need to store additional data alongside it. We could keep it separate, but we
want it persisted to disk, which would mean more database handling logic. So I
- reluctantly - think it's best to Merkleize the mapping as some binary search
tree and store that using the new system. The ledger would then be a combination
of the mapping and the complete binary tree we currently use. We'll continue to
only SNARK the non-indexed tree.

## Drawbacks
[drawbacks]: #drawbacks

This is a lot of engineering work. It'll be more complex than any individual
component of the current system, though less complex than all of them taken
together. 

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

* Why is this design the best in the space of possible designs?
  The alternative is continuing with the current system and continuing to extend
  it. It's possible to make the current system a bit more principled and less
  error prone, but it'll always be a drag on productivity. Masks are not really
  the "right" model for what we're talking about.
* What other designs have been considered and what is the rationale for not choosing them?
  A much gentler revision to how masks are handled is possible: remove the
  `unregister_mask_exn` function and use a finalizer to trigger that behavior.
  That would remove the possibility of unregistering them in the wrong order or
  forgetting to do so. It'd be an improvement, but a much smaller one than
  switching to this new system.
* What is the impact of not doing this?
  Continued pain working with the current system, especially as we add more
  stuff to it.

## Prior art
[prior-art]: #prior-art

The conceptual model here, of a generic Merkle DAG, is pretty much stolen from
IPFS. If IPFS itself weren't so heavy I'd consider just using it for the storage
system, but it is, and there are other issues with that.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

* What parts of the design do you expect to resolve through the RFC process before this gets merged?
 * I hate `Ref` as a name for the type. Can somebody suggest a better one?
 * How does this interact with versioning? We'd want to version both the RPCs
   for the new sync code and the data being synced.
 * How do changes to the OCaml Merkle system affect Snarky code?
* What parts of the design do you expect to resolve through the implementation of this feature before merge?
 * The actual types and signatures are still in flux.
* What related issues do you consider out of scope for this RFC that could be addressed in the future independently of the solution that comes out of this RFC?
