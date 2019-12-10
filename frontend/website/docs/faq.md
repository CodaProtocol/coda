# FAQ

## Testnet

#### Do I need any specific hardware?

Currently, a 4-core processor and 8 GB of RAM are the minimum requirements.  In the near future though, GPUs may be required.

#### What about software?

You can download all the software required to run a Coda node [here](https://codaprotocol.com/docs/getting-started/#installation). Right now, we have builds that work on MacOS, as well as Debian and Ubuntu. You can also [build from source](https://github.com/codaprotocol/coda).

#### What can I do on the Coda network?

Any node can send and receive transactions on the Coda network. Additionally, any node can choose to be a "Node operator". Node operators play two specific roles:

1) Block Producer - this is analogous to being a Bitcoin "miner" or a "validator" in other  proof-of-stake networks. By staking coda, you can be selected to produce a block and win the block reward

2) Snark Worker - this job is what helps compress data in Coda's network. The snark worker nodes generate proofs of transactions, and the block producer buys these proofs on the network (we call it a "snarketplace" :)) - thus, the snark worker gets rewarded a bit of the block reward for their efforts.

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

#### Why did you develop a new protocol, rather than augmenting existing cryptocurrencies?

The reason for developing a new protocol instead of offering services to other blockchains is because adding SNARKs post-mortem to a Layer 1 project is not trivial. Even basic operations need to be optimized for performance inside a SNARK, and existing implementations are not able to be retrofit as such. If we look at hashing functions for example, SHA256 (used by Bitcoin) or Keccak (used by Ethereum) are extremely expensive inside a SNARK circuit, but Poseidon (what Coda uses) is optimized for performance. This and many other technical considerations make it infeasible to easily add recursive SNARKs to existing Layer 1’s, without entirely overhauling the base protocol.

#### Will you require a trusted setup?

It very likely will. However, it is far less of an issue than it was a few years ago. The Zcash team has done a lot of great work on improving the process, and it’s now possible to perform a multi-party computation (MPC) ceremony with hundreds of participants. There would only be issues if every one of those participants were to collude - if just one participant is honest everything is fine, so there is a lot more confidence in the modern approach.

#### Does Coda support smart contracts? If so, what smart contracting languages does it support?

Coda does not support smart contracts currently. The development team is looking into smart contract models, and it is on the roadmap for future development.

#### Does Coda support shielded transactions, like in Zcash?

No, Coda does not natively implement privacy features at the moment. However, privacy is key consideration for cryptocurrencies, and is also on the development roadmap.

## SNARKs and Snark Workers

#### If I run a Snark Worker, how do I get paid for my SNARKs that I generate?

Block producers (the validators who add new blocks to the blockchain) are required to buy SNARKs from the network (or from what we call the Snarketplace) and will pay out some of their block reward as fees to the snark workers who generated SNARKs. This creates a secondary incentive mechanism in the network to reward nodes that help compress transactions.

#### Are Snark Workers like Proof-of-Work (PoW) miners?

The only way in which Snark workers are similar to Pow miners is that both snark proving and mining (generating hashes) are parallelizable tasks, meaning that they are optimized for GPUs. However, beyond that, producing SNARKs is not similar to PoW mining. Given the same hardware and proving algorithm, producing Snark proofs always consumes a fixed amount of resources, regardless of how many other nodes are generating SNARKs. This is unlike PoW mining algorithms where the target difficulty increases as more participants join, and the cost of mining increases as a result. In addition, SNARK research is advancing quickly, and we anticipate that SNARK proving will only become cheaper over time as hardware and algorithms get more efficient.

#### Will Snark Workers require more storage and computing power over time? What about compared to Coda full nodes?

Snark workers will not need more storage or computing power over time. Snark workers simply query the mempool for pending transactions requiring Snark proofs, and then generate said proof -- this does not require syncing historical data. In addition, the underlying proving cost of Snark work doesn’t get more expensive with time.

If we are comparing Snark worker nodes with full nodes on Coda, then yes Snark workers will benefit from specialized hardware as generating SNARK proofs is currently compute intensive. Again, however, with the explosion of SNARK research, this is likely to change and become more accessible to consumer hardware.

#### Will a full node need to store all intermediate SNARK proofs? Eg. Will the storage requirements grow linearly with blocks?

Nope -- when a new block is generated, Coda computes the proof recursively over the prior proof and the new block. This is the advantage of recursive composition -- at any given time, nodes only need to store the most recent proof. Intermediate proofs are not needed. See this talk for more clarity on how this architecture emerged: [https://www.youtube.com/watch?v=eWVGATxEB6M](https://www.youtube.com/watch?v=eWVGATxEB6M).

