This RFC explains how we can reduce the time expended processing an individual transaction.

Let us define *time per transaction* or *transaction processing time* (*TPT* for short) as
the time spent by the daemon processing a single transaction. More precisely,
if over a given interval the daemon uses S seconds of CPU and processes
T transactions, then the TPT on that interval will be S/T.

Note that over a long enough interval this includes time spent on things which are not directly related to processing the transaction at the time of receipt. For example, for each transaction which enters the blockchain, we will later have to verify at least 2 proofs in the parallel scan state tree containing that transaction.

In order to achieve N transactions per second, we can spend at most 1 / N seconds processing a transaction. That is, our TPT must be at most 1/N seconds.

For the purposes of this document, let's just focus on achieving 1 transaction per second, which means the daemon can spend at most 1 second processing 1 transaction.

## SNARK verification

After some preliminary benchmarks, I believe the dominant component in TPT is SNARK verification.

The time spent on SNARK verification per transaction is roughly equal to

```
verification time per SNARK * number of SNARKs per transaction
```

Currently the number of SNARKs per transaction is at least 2 and the verification time is about 155ms (on my laptop). This means that at 1 TPS, SNARK verification eats up at least 310ms of our maximum TPT of 1 second.

Luckily both these numbers can be substantially pushed down. 

### Reducing number of SNARKs per transaction

The number of SNARKs per transaction is equal to the number of nodes in a parallel scan state tree divided by the number of transactions in that tree.

Let us say the number of transactions is N. Say there are B transactions per base proof and that the branching factor of the merge nodes is K.

Then the total number of nodes in a scan state tree is

```
(N / B) + \sum_{i=0}^{log_K(N / B) - 1} K^i
```

It's not totally obvious but the sum is a value which gets smaller with larger K. Thus there are two ways we can reduce the number of SNARKs per transaction, and in so doing reduce our overall TPS.

1. Increase the bundle size B of number of transactions per base proof. Right now it is 1. I think it would be reasonable to make it 4.
2. Increase the branching factor of the scan state.

### Reducing verification time per SNARK

I believe we should not concentrate on this right now because the underlying SNARK may change substantially in the next month. That said, I will record some of what I know here.

Right now verification time for the SNARK is about 155ms (on my laptop) and looks like

```
hashing: 3ms
g2 subgroup check: 26.5ms * 2
preprocess vk: 29ms
G2 precomp: 14ms * 2
miller loop: 8ms
double miller loop: 14ms * 2
final exp: 2ms * 2
```

Most of these checks can be batched across a list of proofs.

- The "preprocess vk" part can be done once at application launch and so can be eliminated entirely.
- The "g2 subgroup check" can be batched across proofs such that the marginal time is brought down to about 4.5ms per proof.
- Of the five miller loops, 2 can be eliminated. This means about 24ms for miller loops marginally.
- The final exp can be eliminated.

So at the end, the marginal time can probably be brought down to about (4.5ms + 24ms + 3ms + 28ms) = 59.5ms.

### Other considerations

It is worth noting that the same SNARKs are often verified at least twice.

1. When that SNARK enters into our SNARK pool.
2. When we process a block containing that SNARK during staged-ledger diff application.

It may be worth caching the verification of the SNARK so that it is shared between application of the staged-ledger diff and when it enters the SNARK pool.
Basically, there would be a hash table mapping `Transaction_snark.Statement.t * Sok_message.t` to a proof which validates against the given key.

## Priorities and recommendations

The biggest, easiest win would be to increase the bundle size B, the number of transactions per base proofs.
If we increase it to 4, we reduce the SNARK-verification part of our TPT by 1/4. 

I think it is not worth spending time implementing batch verification right now because our SNARK construction is likely to change substantially soon.

Increasing the branching factor is worth doing, but it is worth noting it at most improves SNARK-TPT  by a factor of 2. (You can see this by the equation for the number of nodes in a scan state tree. The sum is (N/B - 1) when branching factor is 2, as it is now, which means we still have N/B even if that sum gets eliminated entirely.
