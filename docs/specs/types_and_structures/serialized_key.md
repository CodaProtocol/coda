# Serialized key

Table of Contents
* [File format](#file-format)
* [`header`](#header)
  * [`file_id`](#file_id)
  * [`json`](#json)
    * [`kind`](#kind)
    * [`constraint_constants`](#constraint_constants)
      * [`transaction_capacity`](#transaction_capacity)
      * [`fork`](#fork)
    * [`commits`](#commits)
  * [Example](#Example)
* [`body`](#body)
  * [Verifier key](#verifier-key)
  * [Prover key](#prover-key)

# File format

```
header
body
```

| Field      | Type  | Description |
| - | - | - |
| `header`   | `text`  | Text header |
| `body`     | `bytes` | Binary key  |

# `header`

```
file_id\n
json\n
```

| Field  | Type    | Description |
| - | - | - |
| `file_id`   | `text` | `"MINA_SNARK_KEYS"` |
| `json` | `text` | JSON key parameters |

## `file_id`

This is fixed to `"MINA_SNARK_KEYS"`

## `json`

Here is an example of `json` field in human-readable format.  The details are discussed below.

```json
{
    "header_version": 1,
    "kind": {
        "type": "step-verification-key",
        "identifier": "blockchain-snark-step"
    },
    "constraint_constants": {
        "sub_windows_per_window": 11,
        "ledger_depth": 20,
        "work_delay": 2,
        "block_window_duration_ms": 180000,
        "transaction_capacity": {
            "two_to_the": 7
        },
        "pending_coinbase_depth": 5,
        "coinbase_amount": "720000000000",
        "supercharged_coinbase_factor": 2,
        "account_creation_fee": "1000000000",
        "fork": {}
    },
    "commits": {
        "mina": "29b90544308c4db199640062508af3789867ce03",
        "marlin": "[DIRTY]29b90544308c4db199640062508af3789867ce03"
    },
    "length": 2130,
    "commit_date": "2021-02-17T10:52:33-08:00",
    "constraint_system_hash": "a6cec912699b70258202f2ed855cc0ef",
    "identifying_hash": "a6cec912699b70258202f2ed855cc0ef"
}
```

**Note:** The real `json` field is not in human-readable format and contains no newline characters.

| Field                    | Type  | Description |
| - | - | - |
| `header_version`         | `Number` | Version number for keyfile header format |
| `kind`                   | `Object` | Description of key stored in this file |
| `constraint_constants`   | `Object` | Constraint system constants |
| `commits`                | `Object` | Unique build identifiers |
| `length`                 | `Number` | Length of entire keyfile (including header) |
| `commit_data`            | `String` | Commit date of `mina` blockchain version (corresponds to hashes in `commits`) |
| `constraint_system_hash` | `String` | MD5 hash digest hex of all constraints comprising the zk proof system |
| `identifying_hash`       | `String` | Alias for `constraint_system_hash` |

The types `Number`, `Object` and `String` are the JSON basic data types [[1]](https://en.wikipedia.org/wiki/JSON#Data_types)

To help ensure no download errors have occurred, the `length` field should match the length of the keyfile.  Furthermore, the parameters extracted from `data` section should also match contents of the header.  This is detailed in the Key Loading and Verification Sections.

### `kind`

| Field        | Type  | Description |
| - | - | - |
| `type`       | `String` | Key type `in ["step-verification-key", "wrap-verification-key"]` |
| `identifier` | `String` | Key identifier `in ["blockchain-snark-step", "transaction-snark-merge", "transaction-snark-transaction", "blockchain-snark", "transaction-snark"]`|

### `constraint_constants`

| Field                          | Type     | Description |
| - | - | - |
| `sub_windows_per_window`       | `Number` | Number of sub windows per window in Ouroboros Samasika consensus |
| `ledger_depth`                 | `Number` | `log2(N)` where `N` is the maximum number of accounts |
| `work_delay`                   | `Number` | Minimum number of blocks to wait before performing scan state work added in any block |
| `block_window_duration_ms`     | `Number` | Milliseconds that each block producer has to create and distribute their block |
| `transaction_capacity`         | `Object` | Maximum number of transactions per block |
| `pending_coinbase_depth`       | `Number` | Number of confirmations before coinbase reward is spendable |
| `coinbase_amount`              | `String` | Starting block reward at genesis |
| `supercharged_coinbase_factor` | `Number` | Amount by which block reward is multiplied for supercharged accounts |
| `account_creation_fee`         | `String` | New account fee paid by recipient from payment amount |
| `fork`                         | `Object` | Optional hard fork resumption point |

#### `transaction_capacity`

| Field        | Type     | Description |
| - | - | - |
| `two_to_the` | `Number` | `log2(N)` where `N` is maximum number of transactions per block (e.g. `7`) |

#### `fork`

| Field                 | Type     | Description |
| - | - | - |
| `previous_state_hash`  | `String` | State hash in hex |
| `previous_length`      | `Number` | Height |
| `previous_global_slot` | `Number` | Slot |

### `commits`

| Field   | Type     | Description |
| - | - | - |
| `mina`   | `String` | Githash uniquely identifying `mina` blockchain build (e.g. `"29b90544308c4db199640062508af3789867ce03"`) |
| `marlin` | `String` | Githash uniquely identifying `marlin` library build (e.g. `"[DIRTY]29b90544308c4db199640062508af3789867ce03"`) |

## Example

This is an example of what the entire header looks like on disk.

```
MINA_SNARK_KEYS\n
{"header_version":1,"kind":{"type":"step-verification-key","identifier":"blockchain-snark-step"},"constraint_constants":{"sub_windows_per_window":11,"ledger_depth":20,"work_delay":2,"block_window_duration_ms":180000,"transaction_capacity":{"two_to_the":7},"pending_coinbase_depth":5,"coinbase_amount":"720000000000","supercharged_coinbase_factor":2,"account_creation_fee":"1000000000","fork":{}},"commits":{"mina":"29b90544308c4db199640062508af3789867ce03","marlin":"[DIRTY]29b90544308c4db199640062508af3789867ce03"},"length":      2130,"commit_date":"2021-02-17T10:52:33-08:00","constraint_system_hash":"a6cec912699b70258202f2ed855cc0ef","identifying_hash":"a6cec912699b70258202f2ed855cc0ef"}\n
```

# `body`

The `body` field contains the key in binary.  This can be a verifier key or a prover key.
