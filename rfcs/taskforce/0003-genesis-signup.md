# Genesis Signup Integration


# Goal

At the beginning of each testnet we have to collect the public keys from community members that will be staking in a given testnet phase. These are then delivered to the Engineering team in the form of an unvalidated CSV. This document proposes a simple integration with the new Testnet SDK that will automate the process of validating these inputs, adding them to a genesis ledger, and uploading the ledger and associated keys to kubernetes for deployment with a testnet.

# Design

The integration will consist of a simple Node script that uses the `@o1labs/testnet-sdk` package to:

1. Import and validate/sanitize the public keys from the CSV
2. Generate a new keyset with all the community keys annotated with their corresponding Discord user names
3. Generate a new keyset with new keypairs we will "own" and which will delegate their stake to the community keyset
4. Create a Genesis Ledger with both keysets
5. Upload the corresponding Genesis Ledger and included keypairs to kubernetes for deployment

For sanitizing the public keys, we'll simply pass them through a Regex that will strip any whitespaces and extract the public key. This functionality will be included in the testnet-sdk as part of the `Keyset.add` method.

# Outstanding Questions

What additional keysets should be included in the Genesis Ledger?

# Epic Link

Issue #

