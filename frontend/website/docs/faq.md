# FAQ

## Testnet

#### Do I need any specific hardware?

Currently, a 4-core processor and 8 GB of RAM are the minimum requirements.  In the near future though, GPUs may be required.

#### What about software?

You can download all the software required to run a Coda node [here](https://codaprotocol.com/docs/getting-started/#installation). Right now, we have builds that work on MacOS, as well as Debian and Ubuntu. You can also [build from source](https://github.com/codaprotocol/coda).

#### What can I do on the Coda network?

Any node can send and receive transactions on the Coda network. Additionally, any node can choose to be a "Node operator". Node operators play two specific roles:

1) Block producer - this is analogous to being a Bitcoin "miner" or a "validator" in other  proof-of-stake networks. By staking coda, you can be selected to produce a block and win the block reward

2) Snark worker - this job is what helps compress data in Coda's network. The snark worker nodes generate proofs of transactions, and the block producer buys these proofs on the network (we call it a "snarketplace" :)) - thus, the snark worker gets rewarded a bit of the block reward for their efforts.

#### If I'm a Snarker, how do I get paid for my SNARKs that I generate?

Block producers (the validators who add new blocks to the blockchain) are required to buy SNARKs from the network (or from what we call the Snarketplace) and will pay out some of their block reward as fees to the Snarkers who generated SNARKs. This creates a secondary incentive mechanism in the network to reward nodes that help compress transactions. 

#### What is the point of a testnet?

The Coda testnet's goal is to improve Coda's stability, improve the software through bug fixes and addressing user experience, and to test the economic incentive design in Coda. By participating in the testnet, you get to be the first participants in the Coda protocol, and help develop it from ground zero.

#### Where can I try out the testnet?

Head over to the [testnet landing page](/testnet.html) to learn more and get started.

#### My  daemon crashed -- where should I share the error log?

First, check out [Github issues](https://github.com/codaprotocol/coda/issues) to see if this is a known issue. If the error you experienced is a new issue, file a Github issue with the appropriate tags (daemon, bug). Coda developers will triage the issue and fix it in a future sprint -- thanks for your help!

#### How can I report other issues / get in touch with the development team?

- The Coda community's live discussion channel is the Discord server: [https://discord.gg/ShKhA7J](https://discord.gg/ShKhA7J)
- You can also create Github issues: [https://github.com/codaprotocol/coda/issues](https://github.com/codaprotocol/coda/issues)
- If you need to get in touch privately, you can email: contact@codaprotocol.org

#### Is there a block explorer?

Yes, check out the block explorer here: [https://codaexplorer.garethtdavies.com/](https://codaexplorer.garethtdavies.com/).

## General Questions

#### What consensus algorithm does Coda use?

Coda's consensus mechanism is an implementation of Ouroboros Proof-of-Stake. Due to Coda's unique compressed blockchain, certain aspects of the algorithm have diverged from the Ouroboros papers, and the version Coda uses is affectionately called Codaboros. Stay tuned for more details on Codaboros and some technical writeups on its details and implementation.

#### How does Coda achieve scalability?

Coda achieves scalability through the use of recursive zk-SNARKs. By generating a proof that attests to the validity of historic blockchain states, Coda can keep the blockchain size fixed. This allows for increased throughput due to block size limits not being as taxing on the network, thereby increasing the scalability of the network.

#### What is the maximum transaction throughput?

Practically speaking, the limiting factor ends up being bandwidth, so it depends on the average quality of the internet connection among block producers. If the average connection is a symmetric 2MB/s, then it’s about 2000 tps.

#### Will you require a trusted setup?

It very likely will. However, it is far less of an issue than it was a few years ago. The Zcash team has done a lot of great work on improving the process, and it’s now possible to perform a multi-party computation (MPC) ceremony with hundreds of participants. There would only be issues if every one of those participants were to collude - if just one participant is honest everything is fine, so there is a lot more confidence in the modern approach.

#### Does Coda support smart contracts? If so, what smart contracting languages does it support?

Coda does not support smart contracts currently. The development team is looking into smart contract models, and it is on the roadmap for future development.

#### Does this protocol offer secret transactions, like in Zcash?

No, Coda does not natively implement privacy features at the moment. However, privacy is key consideration for cryptocurrencies, and is also on the development roadmap.

#### Does Coda solve the data availability problem?

The data availability problem, while a concern for other blockchains, does not apply to the Coda network. This is because there is no concept of a "light client" or SPV in Coda. Every node is a full node, and fully verifies the state of the blockchain in a lightweight manner by verifying a zk-SNARK.

In other networks, malicious block producers can send just the block header containing hashes and merkle roots for a new block to light clients. They can then collude to prevent other full nodes from receiving the rest of the block data (hence the name data availability problem). This forces the other block producers to either stall the network, or revert back to a prior block, causing issues for all the light clients that believed the new state. In Coda, because every node can independently verify state, this divide and conquer strategy cannot work.
