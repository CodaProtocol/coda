Notes from Evan on Snark Verify

Questions:

- Can we make it easy to "prove" that some verification key corresponds to a given snark circuit (ie. that is shown to someone off chain)?

- Snark verify (do before predicates)
    - New fields in accounts
        - Verification key
            - Can be "None" or a verification key
        - Data field
            - Intended to be a hash of off chain state much of the time
        - permissioned_users
            - Can be "account_owner", or "any"
                - "account_owner" = txn must be signed by sender
                - "any" = anyone can sign the txn. Snark Verify just has to pass, note snark verify can include things such as checking signatures from one or more parties
    - New fields in transactions
        - Data field
            - intended to be a hash of a witness much of the time
        - other account (publicKey, tokenId, hash)
            - the (publicKey, tokenId) and hash of another account that the snark proof assumes and the recursive snark will verify (that the hash on the chain matches the assumed hash used in the snark)
        - snark_proof
    - New transaction types
        - set verification key
        - set account data field
    - Instrument snarky to make predicate proofs
    - check each transaction verifies:
        - if (transaction.other_account ≠ null)
            - assert(ledger[transaction.other_account].hash == transaction.other_account_hash));
        - if (transaction.protocol_state_hash ≠ null)
            - assert(protocol_state.last_protocol_state_hash == transaction.protocol_state_hash));
        - if (transaction.block_height ≠ null) # maybe feed in multiple of 10 also? So you have 10 blocks to get your proof in?
            - assert(protocol_state.block_height == transaction.block_height));
        - assert(verifies(proof=transaction.snark_proof, input=hash([ transaction.other_account_hash, transaction.transaction_type, transaction.data, account.hash, transaction.block_height, transaction.protocol_state ]), verification_key=account.verification_key))
    - JavaScript SDK for stitching together snark snippets (via something dumb like templates). Can call out to snarky + compiler as long as can be installed with npm - this can be as dumb as generate an OCaml file from a javascript object and call out to compile it.a
    - To extend this to work somewhat meaningfully without full programmability
        - allow for more options over inputs - ex. more than 1 other account, simple functions on accounts, simple functions on protocol_state, that way a snark can be produced that won't be affected if the ledger changes things besides the variables you care about (ex. block_height can be 11 or 12, snark just knows its > 10)
    - Need batch verification across different different verification keys
