# Staged Ledger

A staged ledger is a state that is the result of applying a block, specifically the transactions and snark work from a block. The transactions included in a block don't have proofs yet. They are added to the staged ledger as pending work for which snark workers generate proofs. The snarks included in a block are for transactions from previous blocks and correspond to pending work in the staged ledger.

Staged ledger mainly consists of-

  1. Ledger
  2. Scan state
  3. Pending coinbase collection

## Glossary
| Name | Description |
|------|-------------|
|Snarked ledger | A ledger state that can be verified by a snark|
|Proof | Used interchangeably with snark, refers to transaction snark|
| Snark work | A bundle of at most two proofs along with the prover pk and fees. Also referred to as completed work and is defined in transaction_snark_work.ml|
| Ledger proof | A transaction snark that certifies a ledger state. A ledger proof is emitted from the scan state when all the proofs for a set of transactions are included in blocks, certifying the ledger that is obtained from applying those transactions |
| Work statement/ Statement | A fact about the ledger state that is proven by transaction snark |
| Snark worker | A node in Mina that generates transaction snarks for a fee|
| Protocol state | Representation of the state in a chain, defined in `src/lib/mina_state/protocol_state.ml`|
| Protocol state view | A selected few fields from the protocol state that is required to update the staged ledger
| User command | user transactions namely payments, stake delegations, snapp transactions  |
| Fee transfer | A transaction created by block producers to pay transaction fees or snark fees|
| Coinbase | A transaction created by block producers to pay themselves the coinbase amount for winning the block|

## Ledger

A merkle ledger (<a>src/lib/mina_base/ledger.ml</a>) that has all the transactions (both snarked and unsnarked) in the chain that ends at a given block.

## Scan state

Scan state is a data structure that keeps track of all the partially proven and unproven ledger states in the chain. The scan state is a queue of trees where each tree holds all the data required to prove a set of transactions and thereby prove a ledger state resulting from applying those transactions. A proof can either be of transaction or of merging proofs of transactions. A ledger proof is a special case of merging proofs of transactions where it proves all the transactions added to a tree. The ledger state certified by a ledger proof is also called a snarked ledger.

In the event of a new block when the staged ledger from the previous block is updated, all the data required to prove the included transactions are added to the leaves of a tree in the scan state as pending work. All the snark work in the block that correspond to the pending work from previous blocks are also added to the scan state and create new pending work for merging proofs of transactions, unless it is the final proof aka ledger proof in which case it is simply returned as a result of updating the scan state.

Scan state has a maximum size that determines how many transactions can be included in a block. These are currently defined at compile time as `scan_state_transaction_capacity_log_2` or  `scan_state_tps_goal_x10`. It also specifies the order and the number of proofs required for every set of new transactions or pending work added. The goal is to complete existing pending work before adding new ones.
The abstract structure of scan state is defined in <a>src/lib/parallel_scan/</a> and instantiated in <a>src/lib/transaction_snark_scan_state/transaction_snark_scan_state.ml</a> with the values that are stored in it. The data structure itself is described in detail in [this](https://minaprotocol.com/blog/scanning-for-scans) blog post and in <a>src/lib/parallel_scan/scan_state.md</a>

## Pending coinbase collection

It is a collection of coinbase reward recipients and protocol state of each block in the chain. TODO: readme for pending coinbase

## Staged ledger functions

A staged-ledger-diff consists of all the user transactions, fee transfers that pays snark fees and transaction fees, coinbase transaction, and snark work included in a block.

The two main functions in this module are-

1. Generate staged-ledger-diff for a block (`create_diff`)
2. Apply staged-ledger-diff from a block (`apply` and `apply_diff_unchecked`)

### Generating a staged-ledger-diff

The `create_diff` function in this module creates a staged-ledger-diff for a new block that is valid against a given staged ledger. The user transactions passed here are retrieved from mempool and are in descending order of the fees. Also passed is a function to retrieve required snark work given a statement.

The generated diff should pass the following checks:

1. The number of transactions included in a diff (user or otherwise) should not exceed the max size set at compile time.
2. All the transactions must be valid against the ledger in the given staged ledger.
3. For all the transactions included there should be an equal amount of snark work included. For each spot taken by a transaction on the scan state ~2 proofs are required. This is encapsulated in the term snark work which represents a bundle of at most two proofs.
4. Snark fees, if any, is paid using the transaction fees and therefore the diff can only include snark work that can be paid for. Snark work required to add a coinbase transaction is paid for using the coinbase amount.
  a) Total transaction fees - snark fees (except the ones paid for using coinbase) = transaction fees to the block producer
  b) Total coinbase amount - snark fee to include coinbase = coinbase amount to the block producer
5. The transaction fees from all the user transactions included in the diff ia settled in the same diff.

Next, there are some invariants of the scan state that affect the diff creation process and are worth noting.

#### fee-excess

Since transaction fees or snark fees are paid to the recipients using separate transactions, a separate field in the statement of a transaction snark called fee-excess keeps track of fees resulting from the transaction that should be accounted for in another transaction (a fee transfer). The total fee excess from transactions within a block should be zero i.e., debited from the fee payer account and credited to the fee recipient account. This is ensured when creating the diff.
However, the zero fee excess is also required for transactions in a tree in the scan state. In other words, the statement of the ledger proof emitted by the scan state should have zero fee excess. 

For example, consider the following tree of depth 3 showing only the fee excess when all the transactions included in a block fit in one tree. A positive fee excess is from user transaction and negative fee excess is from fee transfers. Since the total fee excess from transactions within a block is zero, the total fee excess certified by the ledger proof for that tree is also zero (at root of the tree)

                        0
                4               -4
            2       2        2        -6
         1    1   1    1   1    1   -2  -4

Say, a block only had a few transactions and therefore filled a tree partially. The fee excess would at each level would look something like this: (`?` since some leaves are empty)

                        ?
                1                   ?
           2       -1           ?        ?
        1    1   1    -2    -1   _   _    _ 

Now when a new block arrives that has more than three transactions, some of the transactions are added to the first tree and the rest are added the next tree. Adding to the tree above, we'll get:

                         4
                 1                 3
           2        -1         1        2
        1    1   1    -2    -1   2   1    1                 -2 -2   _   _   _   _   _   _

In this case, although the fee excess from transactions within a block is zero, the total fee excess at the root of the first tree is not zero (the total fee excess in the second tree is also not zero).

To ensure every tree has a fee excess of zero, the diff is split into two parts (prediffs) where the first part has transactions and fee transfers that'll occupy the empty leaves of the first tree and the second part has transactions and fee transfers that'll occupy a new tree. In the example above where fee excess is non-zero, with the prediffs, will look like:

                         0
                 1                 -1
           2        -1         1         -2
        1    1   1    -2    -1   2    -1    -1                 2  -1   -1   _   _   _   _   _

## Coinbase splitting

A coinbase is included in a block if there is enough work available for one slot
A coinbase transaction includes a fee transfer to pay for the snark work (deducted from the coinbase amount) and therefore only needs one spot on the scan state tree. However, to include a user transaction, three spots are needed in the worst case (when every snark work is from a different prover; one slot for the user transaction and two slots for fee transfers)

When the diff is split into two prediffs and if after adding user transactions and fee transfers to it, the first prediff has two spots remaining and cannot not accommodate user transactions, then those spots are filled by two coinbase transactions that split the coinbase amount.
If it has one spot, then we simply add one coinbase transaction. It is also possible that the first prediff may have no slots left after adding transactions (For example, when there are three slots and all the required snark work is from a different prover), in which case, we simply add one coinbase as part of the second prediff.

It could also happen that after adding transactions to the first prediff, there is not enough work to add a coinbase transaction to the second prediff. In this case, the two prediffs are discarded and a single prediff is created with a coinbase transaction and how many ever user transactions possible

### Applying a staged-ledger-diff

Given a staged ledger `s` corresponding to a block `X` and a staged-ledger-diff from a new block generated off of `X`, the `apply` function applies the staged-ledger-diff to `s` to produce a staged ledger `s'` corresponding to the new block.

In the resulting staged ledger `s'`,

1. The ledger has all the transactions from the new block applied
2. Snark work from the diff is added to the scan state that'll mark some pending jobs as `done` and create new pending jobs for the newly added transactions and for merging the newly added proofs.
3. Pending coinbase collection is updated with the coinbase for the new block and protocol state of block `X`.

Along with the conditions mentioned [above](###Creating a staged-ledger-diff), the `apply` function also checks if the snark work is valid and that the invariants of the scan state are maintained.
If any of the validations fail, the block consisting of the staged-ledger-diff is rejected.

`apply_diff_unchecked` is the same as `apply` but is called for diffs generated by the node itself. It skips verification of snark work since they get verified before getting into the snark pool. If any of the validations fail (which suggests there is a bug in `create_diff`), the diff is dropped and no block gets created.
