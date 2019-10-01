# Become a Node Operator

!!! warning
    Node operator commands are still being stabilized, so these commands may change. Feel free to try them out and open a pull request to fix anything!


Now that we've set up our Coda node and sent our first transaction, let's turn our attention to the other ways we can interact with the Coda network - namely, participating in consensus, and helping compress data by generating zk-SNARKs. By operating a node that helps secure the network, you can receive coda for your efforts.

## Participating in Consensus

The Coda network is secured by [Proof-of-Stake consensus](/docs/glossary/#proof-of-stake). With this model of consensus, you don't need to have complex equipment like in Bitcoin mining. By simply having coda in our wallet, we can choose to either stake it ourselves, or delegate it to another node. Let's first see how to stake coda ourselves:

### Staking coda

<!-- Since we have some funds in our wallet from [the previous step](/docs/my-first-transaction), we can configure that wallet to stake its coda by issuing the following command, passing in the file path for the associated private key (we previously created the keypair in `keys/my-wallet`): -->

<!-- coda client set-staking -privkey-path keys/my-wallet -->

Since we have some funds in our wallet from [the previous step](/docs/my-first-transaction), we can now start the daemon with the `-propose-key` flag to begin staking coda. Let's stop our current daemon process, and restart it with the following command, passing in the file path for the associated private key (we previously created the keypair in `keys/my-wallet`):

    coda daemon -peer filet-mignon.o1test.net:8303 -propose-key keys/my-wallet

!!! note
    You can provide a list of key files to turn on staking for multiple wallets at the same time

We can always check which wallets we're currently staking with, by using the `coda client status` command:

    coda client status
    
    Coda Daemon Status 
    -----------------------------------
    
    Global Number of Accounts:                     18
    The Total Number of Blocks in the Blockchain:  1
    Local Uptime:                                  36s
    Ledger Merkle Root:                            ...
    Staged-ledger Hash:                            ...
    Staged Hash:                                   ...
    GIT SHA1:                                      ...
    Configuration Directory:                       ...
    Peers:                                         ...
    User_commands Sent:                            0
    Snark Worker Running:                          false
    Sync Status:                                   Offline
    Proposers Running:                             Total: 1 (8QnLUNW7sUxnApau4SLShwr25koiSKrECxtveu89PPmQW5pEyy3xK8YgRpZQkZEanc)
    Best Tip Consensus Time:                       0:0
    Consensus Time Now:                            11542:461
    Consensus Mechanism:                           proof_of_stake
    ...

The `Proposers Running` field in the response above returns the number of accounts currently staking, with the associated key.

!!! warning
    Keep in mind that if you are staking independently with funds in a wallet, you need to remain connected to the network at all times to be elected as a block producer. If you need to go offline frequently, it may be better to delegate your stake.

### Delegating coda

Delegating coda is an alternative option to staking it directly, with the benefit of not having to remain connected to the network at all times. However, keep in mind that:

- you will need to pay a small transaction fee to make a delegate change, as this change is issued as a transaction recorded on the blockchain
- the delegate staking for you may choose to charge you a commission for providing the staking service

To delegate your stake, instead of running your own staking node, run this command:

    coda client delegate-stake \
        -delegate <delegate-public-key> \
        -privkey-path <file> \
        -fee <fee>

The fields in this command:

- The `delegate` flag requires the public key of the delegate you've chosen
- `privkey-path` is the file path to your private key with the funds that you wish to delegate
- `fee` is the transaction fee to record your transaction on the blockchain

Delegating your stake might be useful if you're interested in:

- Running your own staking node that uses funds from a "cold wallet"
- Delegating to a "staking pool" which will provide token payouts periodically

!!! note
    There is a waiting period of a day before this change will come into effect to prevent abuse of the network

## Compressing data in the Coda network 

The Coda protocol is unique in that it doesn't require nodes to maintain the full history of the blockchain like other cryptocurrency protocols. By recursively using cryptographic proofs, the Coda protocol effectively compresses the blockchain to constant size. We call this compression, because it reduces terabytes of data to a few kilobytes.

However, this isn't data encoding or compression in the traditional sense - rather nodes "compress" data in the network by generating cryptographic proofs. Node operators play a crucial role in this process by designating themselves as "snark-workers" that generate zk-SNARKs for transactions that have been added to blocks.

When you [start the daemon](/docs/my-first-transaction/#start-up-a-node), set these extra arguments to also start a snark-worker:

    coda daemon \
        -peer filet-mignon.o1test.net:8303 \
        -run-snark-worker $CODA_PK \
        -snark-worker-fee <fee>

As a snark-worker, you get to share some of the block reward for each block your compressed transactions make it in to. The block producer is responsible for gathering compressed transactions before including them into a block, and will be incentivized by the protocol to reward snark-workers.

That about covers the roles and responsibilities as a Code node operator. Since Coda is a permissionless peer-to-peer network, everything is managed and run in a decentralized manner by nodes all over the world. Similarly, the Coda project is also distributed and permissionless to join. The code is all open source, and there is much work to be done, both technical and non-technical. To learn more about how you can get involved with Coda, please check out the [Contributing to Coda section](../contributing).
