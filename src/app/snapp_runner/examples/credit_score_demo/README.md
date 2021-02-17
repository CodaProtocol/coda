## Instructions

### Starting the daemon

* build a snark-enabled coda executable with
  `DUNE_PROFILE=testnet_postake_medium_curves make build`
* set up environment variables to pass the appropriate time offset to the daemon
  - `export now_time=$(date +%s)`
  - `export genesis_time=$(date -d "$(_build/default/src/app/cli/src/coda.exe advanced compile-time-constants | jq -r '.genesis_state_timestamp')" +%s)`
  - `export CODA_TIME_OFFSET=$(( $now_time - $genesis_time ))`
* create a configuration file `config.json` containing
  ```json
  {"ledger":{"accounts":[{"pk":"B62qrPN5Y5yq8kGE3FbVKbGTdTAJNdtNtB5sNVpxyRwWGcDEhpMzc8g","balance":"66000","sk":null,"delegate":null}]}}
  ```
* add the private key for the ledger key to a file (e.g. `demo-block-producer`)
  - `chmod 0600 demo-block-producer` to set the expected permissions
  ```json
  {"box_primitive":"xsalsa20poly1305","pw_primitive":"argon2i","nonce":"8jGuTAxw3zxtWasVqcD1H6rEojHLS1yJmG3aHHd","pwsalt":"AiUCrMJ6243h3TBmZ2rqt3Voim1Y","pwdiff":[134217728,6],"ciphertext":"DbAy736GqEKWe9NQWT4yaejiZUo9dJ6rsK7cpS43APuEf5AH1Qw6xb1s35z8D2akyLJBrUr6m"}
  ```
* start the daemon with the given configuration, using the given key
  - `_build/default/src/app/cli/src/coda.exe daemon -seed -working-dir $PWD -current-protocol-version 0.0.0 -block-producer-key demo-block-producer -config-file config.json -generate-genesis-proof true`
  - To use dedicated configuration and genesis ledger directories, add the `-config-directory $CODA_CONFIG_DIR` and `genesis-ledger-dir $CODA_GENESIS_DIR` directories
* import the demo-block-producer public key
  - `_build/default/src/app/cli/src/coda.exe accounts import -privkey-path demo-block-producer`
    + Add the `-config-directory $CODA_CONFIG_DIR` flag to match the one passed to the above, if given.
  - `_build/default/src/app/cli/src/coda.exe accounts list -privkey-path demo-block-producer`
    + This should show the imported key in the list
* unlock the demo-block-producer wallet
  - `_build/default/src/app/cli/src/coda.exe accounts unlock -public-key B62qrPN5Y5yq8kGE3FbVKbGTdTAJNdtNtB5sNVpxyRwWGcDEhpMzc8g`
  - Equivalently, issue the GraphQL mutation
    ```graphql
    mutation Unlock {
        unlockAccount(input: {publicKey: "B62qrPN5Y5yq8kGE3FbVKbGTdTAJNdtNtB5sNVpxyRwWGcDEhpMzc8g", password: ""})
    }
    ```

## Build the demo client
* `DUNE_PROFILE=testnet_postake_medium_curves dune build src/app/snapp_runner/examples/credit_score_demo/credit_score_demo.exe`

### Create a new account to attach the snapp to
* Open a GraphQL client directed to port 3085, or point a web browser at `http://localhost:3085/graphql`
* Send a GraphQL query to create a wallet:
  ```graphql
  mutation CreateSnappWallet {
    createAccount(input: {password: ""}) {
      account {
        publicKey
      }
    }
  }
  ```
* Unlock the new wallet
  ```graphql
  mutation UnlockSnappWallet {
    unlockAccount(input: {publicKey: "NEW_PK_HERE", password: ""})
  }
  ```
* Create an account for the snapp public key
  ```graphql
  mutation CreateSnappAccount {
    createTokenAccount(input:
      { fee: 10000000
      , feePayer: "B62qrPN5Y5yq8kGE3FbVKbGTdTAJNdtNtB5sNVpxyRwWGcDEhpMzc8g"
      , receiver:"NEW_PK_HERE"
      , token: "1"
      , tokenOwner:"B62qrPN5Y5yq8kGE3FbVKbGTdTAJNdtNtB5sNVpxyRwWGcDEhpMzc8g" })
  }
  ```
* When the new account has been successfully created, it will show in `_build/default/src/app/cli/src/coda.exe advanced dump-ledger`
  - This can also be checked programmatically using the `accounts` GraphQL query:
    ```graphql
    query FindPkAccounts {
      accounts(publicKey: "NEW_PK_HERE") {
        isTokenOwner
        token
      }
    }
    ```

### Set up the snapp account
* Generate a verification key using the demo
  - `_build/default/src/app/snapp_runner/examples/credit_score_demo/credit_score_demo.exe verification-key`
* Issue a GraphQL mutation to add the verification key to the snapp account:
  ```graphql
  mutation SetupSnappAccount {
    sendSnappCommand(input:
      { snappAccount:
          { balanceChange: 0
          , publicKey: "NEW_PK_HERE"
          , changes:
            { verificationKey: "VERIFICATION_KEY_HERE"
            , permissions:
                { edit_state: "signature"
                , receive: "none"
                , send: "either"
                , set_delegate: "signature"
                , set_permissions: "signature"
                , set_verification_key: "signature"
                , stake: true } }}
      , feePayment:
          { fee: "10000000"
          , publicKey: "B62qrPN5Y5yq8kGE3FbVKbGTdTAJNdtNtB5sNVpxyRwWGcDEhpMzc8g" }
      , token: "1" })
  }
  ```
  - The `verificationKey` field should be replaced with the generated verification key
  - The `snappAccount.publicKey` field should be replaced with the snapp account's public key
* Transfer some funds from the demo-block-producer to the snapp account:
  ```graphql
  mutation TransferToSnappAccount {
    sendPayment(input:
      { fee: "10000000"
      , amount: "100000000"
      , to: "NEW_PK_HERE"
      , from: "B62qrPN5Y5yq8kGE3FbVKbGTdTAJNdtNtB5sNVpxyRwWGcDEhpMzc8g" })
  }
  ```
* Lock the demo-block-producer's wallet.
  - `_build/default/src/app/cli/src/coda.exe accounts lock -public-key B62qrPN5Y5yq8kGE3FbVKbGTdTAJNdtNtB5sNVpxyRwWGcDEhpMzc8g`
  - This demonstrates that the client isn't generating a signature in order for
    the snapp command to be valid.

### Set up a wallet for the receiving account
* Send a GraphQL query to create a wallet:
  ```graphql
  mutation CreateReceiverWallet {
    createAccount(input: {password: ""}) {
      account {
        publicKey
      }
    }
  }
  ```

### Generate and send the snapp command
* Generate a proof using the demo
  - `_build/default/src/app/snapp_runner/examples/credit_score_demo/credit_score_demo.exe prove --score $MY_CREDIT_SCORE --snapp-public-key SNAPP_ACCOUNT_PK --receiver-public-key RECEIVER_ACCOUNT_PK --fee 10000000 --amount 10000000 --account-creation-fee 100000`
  - The amounts may be customised, but the account creation fee is set by the network and should not be changed. Omitting the `--account-creation-fee` argument causes the given default value to be used.
* Issue a GraphQL mutation to send a snapp command containing the proof:
  ```graphql
  mutation SendSnappCommand {
    sendSnappCommand(input:
      { snappAccount:
          { balanceChange: "-20100000"
          , publicKey: "SNAPP_ACCOUNT_PK"
          , predicate: { }
          , proof: "PROOF" }
      , otherAccount:
          { balanceChange: "10000000"
          , publicKey: "RECEIVER_ACCOUNT_PK" }
      , token: "1" })
  }
  ```
  - The `proof` field should be replaced with the generated proof.
  - The `publicKey` fields should be replaced with the relevant public keys.
  - The `snappAccount.balanceChange` field should be set to `-(fee+amount+account_creation_fee)`.
  - The `otherAccount.balanceChange` field should be set to `amount`.
