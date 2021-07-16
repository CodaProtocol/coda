## Summary
[summary]: #summary

Complicate the SNARK pool so that it can do "batch verification" - verification of several proofs at once.

## Motivation
[motivation]: #motivation

Verification of new SNARK work becomes a throughput bottleneck at high transaction-per-second configurations (saith [#3981](https://github.com/CodaProtocol/coda/pull/3981)). There is a more efficient "batch verification" that we can use - by verifying N SNARKs at once, we spend `600ms + N*10ms` instead of `N * 600ms` <sup>(citation needed)</sup>. This comes with a catch: if batch verification fails, we only know that _some_ SNARK in there failed to verify, but not which. In the case that a batch fails to verify, we need to do extra work to attribute the failure to a particular sender IP / prover, and know which of the SNARKs we can actually include in the pool. By sending incorrect or fraudulent proofs, an attacker can degrade the throughput of a block producer. Each transaction in the block needs to come with two (or occasionally one) proofs, and if not enough proofs are available, the block producer is forced into leaving profit on the table.

## Detailed design
[detailed-design]: #detailed-design

The proposed mechanism is to have two "batch modes": Batch All and Batch by Prover, and to split provers into two categories: Trusted and Untrusted.

Provers start out Untrusted, and have their proofs checked separately. Proofs from Untrusted provers are batched "by prover": for each prover, all their proofs are verified together in individual batches. If a batch fails to verify, that prover is banned, and none of their proofs will be used. After `TRUST_AMOUNT_PROOFS` proofs, provers are promoted into Trusted. All proofs from trusted provers are verified in a single batch. If that batch fails, the proofs are re-batched "by prover", and the provers of the failing batches are punished.

This changes how SNARK proofs are collected and verified: instead of verifying them immediately as they arrive, we wait for some time before collecting all the received proofs into their respective batches. We'll do this like so:

- Store a queue of pending SNARK work, starting out empty.
- Upon receipt of the first SNARK work into the queue, start a `MAX_SNARK_DELAY` second timer.
- When the queue hits `MAX_SNARK_BATCH_LENGTH`, or after the timer expires, clear the queue and start the batch verification process.

The timer ensures that, even when SNARK work is coming in slowly, we aren't adding too much latency into the gossip verification. 

In order to identify provers into those two categories, we need an unforgeable notion of prover identity. I propose we add a new signature of the SNARK work, using a key that is present in the best tip ledger. This adds a new failure mode to SNARK proofs: signing with a key that's not present in the ledger. Users should be discouraged from storing value in that account. By necessity, the private key will need to be relatively unprotected as it needs to be available on the SNARK coordinator to sign proofs before broadcasting them.

Concretely, this would change the `Snark_pool_diff` type from  `Work.t * Ledger_proof.t One_or_two.t Priced_proof.t` to `{signature: Signature.t; body: Snark_work_body.t}` where `Snark_work_body` is a stable type with contents `Public_key.t * Work.t * Ledger_proof.t One_or_two.t Priced_proof.t`.

By requiring provers to have an identity present in the ledger, and by banning the IPs _and identities_ of senders of bad proofs, we can avoid a Sybil attack where one party can arbitrarily degrade TPS for individual block producers for the low cost of an IP.

### Testing

In the integration tests: Add a way to spin up a new SNARK coordinator node, broadcast a bad proof, and verify that the node is banned, and that the "batch by prover" mode works. 

In the SNARK pool unit tests: manually inject some bogus snark pool diffs.

## Drawbacks
[drawbacks]: #drawbacks

Requiring an account to make proofs is a really nasty operational requirement. One possible mitigation: run a server with a massive horizontally scaled SNARK verifier pool that accepts proofs from anyone and signs it with its own account. SNARK pools (analogous to mining pools) could provide this service to their clients.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

Some alternatives to "batching by prover":

- Do a split-in-half-and-conquer on the batches. This can end up with a substantial amount of wasted work.
- Unbatch all and verify sequentially. This lets us keep good proofs from bad provers, but is somewhat contrary to the immediate punishment that follows.

An early idea was an extra "not yet known" state before nodes become Untrusted. In the extra state, we don't do any batching, so as to avoid commiting potentially a lot of time to verifying a bogus batch before any reputation is established. This was deemed unnecessary.

One idea was for identifying provers was to use the fee recipient account, since it's already encoded into the SoK. Without adding an additional signature this allows forgery as only public information is needed, and also if the proof fails to verify then nothing can be inferred from it. Using the fee recipient account itself seems unwise as it will be storing actual value, and having the private key just lying around with on the SNARK coordinator is risky (and a regression from today's world where SNARK coordinators and workers need no sensitive information).

It is not possible to use IP addresses alone for batching categories because the gossip-net-sender of the proof is unrelated to who proved it.

One idea that was discussed was broadcasting "SNARK fraud proofs", consisting of the signed-but-wrong proof, so the entire network could ban that prover. This seems unnecessary: it incurs a "we need to try and fail to verify this proof" cost on the entire network. This may still be preferable to letting the attacker poison as many batches as they want across the network while they get banned by each individual node.

We might want to instaban senders of work whose work isn't present in the best tip ledger. There are some complications around the network not actually having consensus on the best tip ledger yet. We could use the locked ledger instead, but this adds a lot of delay to becoming a new SNARK coordinator.

## Prior art
[prior-art]: #prior-art

I don't know.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

Is the signing key being present in the ledger an acceptable price to pay? Are there any other ways to avoid the sybil attack? Is the sybil attack worth mitigating?

All of the constants above need to be determined:

`MAX_SNARK_DELAY`: With randomsub, a network of 10k nodes has an expected diameter of around 4, and so a delay of around 5 seconds shouldn't put undue burden on the SNARK pool (effective diameter of 20s, versus our 3 minute slot time and roughly ~2 minute proving time). Assuming 600ms of overhead per batch, this is a 12% overhead compared to a 0.3% overhead if we were to somehow batch all proofs received in a 3 minute slot.  The 12% batching overhead can be cranked down by increasing the max batching window.

`MAX_SNARK_BATCH_LENGTH` can almost be computed from the longest amount of time we want to accept as a sunk cost for a bad batch, modulo the complication that untrusted provers have the extra fixed-cost-per-batch.

`TRUST_AMOUNT_PROOFS` is hard to set. It's not clear that there are notions of economic incentive to appeal to here (and [rationality is self-defeating](https://bford.info/2019/09/23/rational/)). Is there any pressing reason it shouldn't just be `1`?
