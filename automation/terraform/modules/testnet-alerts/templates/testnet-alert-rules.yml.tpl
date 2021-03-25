# $: rule_namespace - Grafanacloud rules config namespace for organizing rulesets (see: https://grafana.com/docs/grafana-cloud/alerts/grafana-cloud-alerting/namespaces-and-groups/)
# $: rule_filter - filter for subset of testnets to include in rule alert search space
# $: alerting_timeframe - range of time to inspect for alert rule violations

namespace: ${rule_namespace}
groups:
- name: Critical
  rules:
  - alert: WatchdogClusterCrashes
    expr: max by (testnet) (max_over_time(Coda_watchdog_cluster_crashes ${rule_filter} [${alerting_timeframe}])) > 0.5
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} cluster nodes have crashed"
      description: "{{ $value }} Cluster nodes have crashed on network {{ $labels.testnet }}."

  - alert: WatchdogNoNewLogs
    expr: max by (testnet) (Coda_watchdog_pods_with_no_new_logs) > 0
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has pods which have not logged in 10 minutes"
      description: "There are no new logs in the last 10 minutes for {{ $value }} pods on network {{ $labels.testnet }}."

  - alert: SeedListDown
    expr: min by (testnet) (min_over_time(Coda_watchdog_seeds_reachable ${rule_filter} [${alerting_timeframe}])) == 0
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} seed list is down (no seeds are reachable)"
      description: "Seed list is down on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/SeedListDown-d8d4e14609884c63a7086309336f3462"

  - alert: BlockStorageBucketNoNewBlocks
    expr: min by (testnet) (min_over_time(Coda_watchdog_recent_google_bucket_blocks ${rule_filter} [${alerting_timeframe}])) >= 30*60
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has no new blocks posted to the google block storage bucket recently"
      description: "{{ $value }} new blocks posted to the google storage bucket for {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/BlockStorageBucketNoNewBlock-80ddaf0fa7944fb4a9c5a4ffb4bbd6e2"

  - alert: ProverErrors
    expr: max by (testnet) (max_over_time(Coda_watchdog_prover_errors_total ${rule_filter} [${alerting_timeframe}])) > 0
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has observed a prover error"
      description: "{{ $value }} Prover errors on network {{ $labels.testnet }}."

  - alert: NodesNotSynced
    expr: min by (testnet) (min_over_time(Coda_watchdog_nodes_synced ${rule_filter} [${alerting_timeframe}])) <= .5
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has <= 50% of nodes synced"
      description: "Nodes sync rate of {{ $value }} is <= 50% on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/Nodes-not-synced-34e4d4eeaeaf47e381de660bab9ce7b7"

  - alert: NodesOutOfSync
    expr: min by (testnet) (min_over_time(Coda_watchdog_nodes_synced_near_best_tip ${rule_filter} [${alerting_timeframe}])) < .75
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has < 90% of nodes that are synced on the same best tip"
      description: "< 90% of nodes that are synced are on the same best tip for  network {{ $labels.testnet }} with rate of {{ $value }}."
      runbook: "https://www.notion.so/minaprotocol/Nodes-out-of-sync-0f29c739e47c42e4adabe62a2a0316bd"

  - alert: LowPeerCount
    expr: min by (testnet) (Coda_Network_peers ${rule_filter}) < 3
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} avg. peer count is critically low"
      description: "Critically low peer count of {{ $value }} on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/LowPeerCount-3a66ae1ca6fd44b585eca37f9206d429"

  - alert: CriticallyLowMinWindowDensity
    expr: min by (testnet) (Coda_Transition_frontier_min_window_density ${rule_filter}) <= 30
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} min density is critically low"
      description: "Critically low min density of {{ $value }} on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/LowMinWindowDensity-Runbook-7908635be4754b44a862d9bec8edc239"

  - alert: LowFillRate
    expr: min by (testnet) (Coda_Transition_frontier_slot_fill_rate ${rule_filter}) < 0.75 * 0.75
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} slot fill rate is critically low"
      description: "Lower fill rate of {{ $value }} than expected on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/LowFillRate-36efb1cd9b5d461db6976bc1938fab9e"

  - alert: NoTransactionsInSeveralBlocks
    expr: max by (testnet) (Coda_Transition_frontier_empty_blocks_at_best_tip ${rule_filter}) >= 5
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has >= 5 blocks without transactions at the tip"
      description: "{{ $value }} blocks without transactions on tip of network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/No-Transactions-In-Several-Blocks-55ca13df38dd4c3491e11d8ea8020c08"

  - alert: NoCoinbaseInBlocks
    expr: min by (testnet) (min_over_time(Coda_Transition_frontier_best_tip_coinbase ${rule_filter} [10m])) < 1
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has blocks without coinbases"
      description: "{{ $value }} Blocks without coinbases on tip of network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/NoCoinbaseInBlocks-aacbc2a4f9334d0db2de20c2f77ac34f"

  - alert: LongFork
    expr: max by (testnet) (Coda_Transition_frontier_longest_fork ${rule_filter}) >= 16
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has a fork of length at least 16"
      description: "Fork of length {{ $value }} on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/LongFork-e65e5ad7437f4f4dbac201abbf9ace81"

  - alert: OldBestTip
    expr: min by (testnet) ((time() - 1609459200) - Coda_Transition_frontier_best_tip_slot_time_sec ${rule_filter}) >= 15 * 180
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }}: all nodes have best tips older than 15 slots"
      description: "All nodes have best tips older than 15 slots (45 minutes) on network {{ $labels.testnet }}. Best tip: {{ $value }}"
      runbook: "https://www.notion.so/minaprotocol/OldBestTip-8afa955101b642bd8356edfd0b03b640"

  - alert: NoNewSnarks
    expr: min by (testnet) ((time() - 1609459200) - Coda_Snark_work_useful_snark_work_received_time_sec ${rule_filter}) >= 2 * 180 and max by (testnet) (Coda_Snark_work_pending_snark_work ${rule_filter}) != 0
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }}: no new SNARK work seen for 2 slots."
      description: "No node has received SNARK work in the last 2 slots (6 minutes) on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/NoNewSnarks-f86d27c81af54954b2fb61378bff9d4d"

  - alert: NoNewTransactions
    expr: min by (testnet) ((time() - 1609459200) - Coda_Transaction_pool_useful_transactions_received_time_sec ${rule_filter}) >= 2 * 180
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }}: no new transactions seen for 2 slots."
      description: "No node has received transactions in their transaction pool in the last 2 slots (6 minutes) on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/NoNewTransactions-27dbeafab8ea4d659ee6f748acb2fd6c"

  - alert: HighUnparentedBlockCount
    expr: max by (testnet) (max_over_time(Coda_Archive_unparented_blocks ${rule_filter} [${alerting_timeframe}])) > 30
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has a critically high unparented block count"
      description: "{{ $value }} Unparented block count is critically high on network {{ $labels.testnet }}."

  - alert: HighMissingBlockCount
    expr: max by (testnet) (max_over_time(Coda_Archive_missing_blocks ${rule_filter} [${alerting_timeframe}])) > 30
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: critical
    annotations:
      summary: "{{ $labels.testnet }} has a critically high missing block count"
      description: "{{ $value }} Missing block count is critically high on network {{ $labels.testnet }}."

- name: Warnings
  rules:
  - alert: HighBlockGossipLatency
    expr: max by (testnet) (max_over_time(Coda_Block_latency_gossip_time ${rule_filter} [${alerting_timeframe}])) > 200
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "{{ $labels.testnet }} block gossip latency is high"
      description: "High block gossip latency of {{ $value }}(ms) within {{ $labels.testnet }} network."
      runbook: "https://www.notion.so/minaprotocol/HighBlockGossipLatency-2096501c7cf34032b44e903ec1a4d79c"

  - alert: SomewhatOldBestTip
    expr: count by (testnet) (((time() - 1609459200) - Coda_Transition_frontier_best_tip_slot_time_sec ${rule_filter}) >= 8 * 180) > 1
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "{{ $labels.testnet }}: at least 2 nodes have best tips older than 8 slots"
      description: "At least 2 nodes have best tips older than 8 slots (24 minutes) on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/SomewhatOldBestTip-bb1509582bdd4908bcda656eebf421b5"

  - alert: MediumFork
    expr: max by (testnet) (Coda_Transition_frontier_longest_fork ${rule_filter}) >= 8
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "{{ $labels.testnet }} has a fork of length at least 8"
      description: "Fork of length {{ $value }} on network {{ $labels.testnet }}."

  - alert: NoTransactionsInAtLeastOneBlock
    expr: max by (testnet) (max_over_time(Coda_Transition_frontier_empty_blocks_at_best_tip ${rule_filter} [${alerting_timeframe}])) > 0
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "{{ $labels.testnet }} has at least 1 block without transactions at the tip"
      description: "{{ $value }} Blocks without transactions on tip of network {{ $labels.testnet }}."

  - alert: LowMinWindowDensity
    expr: min by (testnet) (Coda_Transition_frontier_min_window_density ${rule_filter}) <= 35
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "{{ $labels.testnet }} min density is low"
      description: "Low min density on network {{ $labels.testnet }}."
      runbook: "https://www.notion.so/minaprotocol/LowMinWindowDensity-Runbook-7908635be4754b44a862d9bec8edc239"

  - alert: SeedListDegraded
    expr: min by (testnet) (min_over_time(Coda_watchdog_seeds_reachable ${rule_filter} [${alerting_timeframe}])) <= 0.5
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "{{ $labels.testnet }} seed list is degraded (less than 50% reachable)"
      description: "Seed list is degraded at {{ $value }} on network {{ $labels.testnet }}."

  - alert: FewBlocksPerHour
    expr: min by (testnet) (increase(Coda_Transition_frontier_max_blocklength_observed ${rule_filter} [${alerting_timeframe}])) < 1
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "{{ $labels.testnet }} block production is critically low (there has been less than 1 block in the last hour)"
      description: "{{ $value }} blocks have been produced on network {{ $labels.testnet }} in the last hour (according to some node)."

  - alert: LowPostgresBlockHeightGrowth
    expr: min by (testnet) (increase(Coda_Archive_max_block_height ${rule_filter} [${alerting_timeframe}])) < 1
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "{{ $labels.testnet }} rate of archival of network blocks in Postgres DB is lower than expected"
      description: "The rate of {{ $value }} new blocks observed by archive postgres instances is low on network {{ $labels.testnet }}."

  - alert: UnparentedBlocksObserved
    expr: max by (testnet) (max_over_time(Coda_Archive_unparented_blocks ${rule_filter} [${alerting_timeframe}])) > 1
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "Unparented blocks observed on {{ $labels.testnet }}"
      description: "{{ $value }} Unparented block(s) observed on network {{ $labels.testnet }}."

  - alert: MissingBlocksObserved
    expr: max by (testnet) (max_over_time(Coda_Archive_missing_blocks ${rule_filter} [${alerting_timeframe}])) > 0
    labels:
      testnet: "{{ $labels.testnet }}"
      severity: warning
    annotations:
      summary: "Missing blocks observed on {{ $labels.testnet }}"
      description: "{{ $value }} Missing block(s) observed on network {{ $labels.testnet }}."
