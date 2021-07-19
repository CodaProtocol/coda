[%%import
"/src/config.mlh"]

[%%ifndef
consensus_mechanism]

module Currency = Currency_nonconsensus.Currency

[%%endif]

(** This file consists of compile-time constants that are not in
    Genesis_constants.
    This file includes all of the constants defined at compile-time for both
    tests and production.
*)

[%%inject
"curve_size", curve_size]

[%%inject
"genesis_ledger", genesis_ledger]

[%%inject
"default_transaction_fee_string", default_transaction_fee]

[%%inject
"default_snark_worker_fee_string", default_snark_worker_fee]

[%%inject
"minimum_user_command_fee_string", minimum_user_command_fee]

[%%ifndef
compaction_interval]

let compaction_interval_ms = None

[%%else]

[%%inject
"compaction_interval", compaction_interval]

let compaction_interval_ms = Some compaction_interval

[%%endif]

let minimum_user_command_fee =
  Currency.Fee.of_formatted_string minimum_user_command_fee_string

let default_transaction_fee =
  Currency.Fee.of_formatted_string default_transaction_fee_string

let default_snark_worker_fee =
  Currency.Fee.of_formatted_string default_snark_worker_fee_string

[%%inject
"block_window_duration_ms", block_window_duration]

[%%inject
"vrf_poll_interval_ms", vrf_poll_interval]

let rpc_handshake_timeout_sec = 60.0

let rpc_heartbeat_timeout_sec = 60.0

let rpc_heartbeat_send_every_sec = 10.0 (*same as the default*)

[%%inject
"generate_genesis_proof", generate_genesis_proof]
