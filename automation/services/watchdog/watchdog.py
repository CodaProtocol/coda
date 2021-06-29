# Example of running locally
# SEED_PEERS_URL=https://storage.googleapis.com/seed-lists/mainnet_seeds.txt LOCAL_KUBERNETES=true KUBERNETES_NAMESPACE=watchdog-test METRICS_PORT=8000 python3 watchdog.py

from prometheus_client import start_http_server, Summary
import time
import os
import json
from prometheus_client import Counter
from prometheus_client import Gauge
import asyncio
import util

import metrics

# =================================================

def main():
  print('starting watchdog')

  port = int(os.environ['METRICS_PORT'])
  start_http_server(port)

  v1, namespace = util.get_kubernetes()

  cluster_crashes = Gauge('Coda_watchdog_cluster_crashes', 'Description of gauge')
  error_counter = Counter('Coda_watchdog_errors', 'Description of gauge')

  nodes_synced_near_best_tip = Gauge('Coda_watchdog_nodes_synced_near_best_tip', 'Description of gauge')
  nodes_synced = Gauge('Coda_watchdog_nodes_synced', 'Description of gauge')
  nodes_responded = Gauge('Coda_watchdog_nodes_responded', 'Number of nodes that responded to the last status query')
  prover_errors = Counter('Coda_watchdog_prover_errors', 'Description of gauge')
  pods_with_no_new_logs = Gauge('Coda_watchdog_pods_with_no_new_logs', 'Number of nodes whose latest log is older than 10 minutes')
  nodes_queried=Gauge('Coda_watchdog_nodes_queried', 'Number of nodes that were queried for node-status')
  seed_nodes_responded=Gauge('Coda_watchdog_nodes_responded_to_seed', 'Number of nodes that responded to the last status query on each seed', ['seed'])
  seed_nodes_queried=Gauge('Coda_watchdog_nodes_queried_by_seed', 'Number of nodes that were queried for node-status on each seed', ['seed'])
  context_deadline_exceeded=Gauge('Coda_watchdog_deadline_exceeded', 'Number of nodes that failed with the context-deadline-exceeded error to a node-status query')
  failed_security_protocol_negotiation=Gauge('Coda_watchdog_failed_negotiation', 'Number of nodes that failed with the security-protocol-negotiation error to a node-status query')
  connection_refused_errors=Gauge('Coda_watchdog_connection_refused', 'Number of nodes that failed with the connection-refused error to a node-status query')
  size_limit_exceeded_errors=Gauge('Coda_watchdog_size_limit_exceeded', 'Number of nodes that failed to a respond to a node-status query becuase of the data size')
  timed_out_errors=Gauge('Coda_watchdog_timed_out', 'Number of nodes that failed with the time-out error to a node-status query')
  stream_reset_errors=Gauge('Coda_watchdog_stream_reset', 'Number of nodes that failed with the stream-reset error to a node-status query')
  other_connection_errors=Gauge('Coda_watchdog_node_status_other_errors', 'Number of nodes that failed with an unexpected error to respond to a node-status query(look for it in the logs)')
  nodes_errored=Gauge('Coda_watchdog_node_status_errors', 'Number of nodes that failed to respond to a node-status query')

  recent_google_bucket_blocks = Gauge('Coda_watchdog_recent_google_bucket_blocks', 'Description of gauge')
  seeds_reachable = Gauge('Coda_watchdog_seeds_reachable', 'Description of gauge')

  # ========================================================================

  fns = [
    ( lambda: metrics.collect_cluster_crashes(v1, namespace, cluster_crashes), 30*60 ),
    ( lambda: metrics.collect_node_status_metrics(v1, namespace, nodes_synced_near_best_tip, nodes_synced, nodes_queried, nodes_responded, seed_nodes_queried, seed_nodes_responded, nodes_errored, context_deadline_exceeded, failed_security_protocol_negotiation, connection_refused_errors,size_limit_exceeded_errors, timed_out_errors, stream_reset_errors, other_connection_errors, prover_errors), 10*60 ),
    ( lambda: metrics.check_seed_list_up(v1, namespace, seeds_reachable), 60*60 ),
    ( lambda: metrics.pods_with_no_new_logs(v1, namespace, pods_with_no_new_logs), 60*10 ),
  ]

  if os.environ.get('CHECK_GCLOUD_STORAGE_BUCKET') is not None:
    fns += [ ( lambda: metrics.check_google_storage_bucket(v1, namespace, recent_google_bucket_blocks), 30*60 ) ]

  for fn, time_between in fns:
    asyncio.ensure_future(util.run_periodically(fn, time_between, error_counter))

  loop = asyncio.get_event_loop()
  loop.run_forever()

main()
