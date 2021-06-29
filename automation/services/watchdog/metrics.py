
import itertools
import datetime
import util
import asyncio
import random
import os
import sys
import traceback
import subprocess
import time
import json
import urllib.request
import ast

from google.cloud import storage

# ========================================================================

def collect_cluster_crashes(v1, namespace, cluster_crashes):
  print('collecting cluster crashes / restarts')
  pods = v1.list_namespaced_pod(namespace, watch=False)

  containers = list(itertools.chain(*[ pod.to_dict()['status']['container_statuses'] for pod in pods.items if pod.status.phase == 'Running' ]))
  mina_containers = list(filter(lambda c: c['name'] in [ 'coda', 'seed', 'coordinator', 'archive' ], containers))

  def restarted_recently(c):

    if c['restart_count'] == 0:
      return False

    terminated = c['last_state']['terminated']
    if terminated is None:
      return False

    restart_time = terminated['started_at']
    retart_age_seconds = (datetime.datetime.now(datetime.timezone.utc) - restart_time).total_seconds()

    # restarted less than 30 minutes ago
    return retart_age_seconds <= 30*60

  recently_restarted_containers = list(filter(lambda c: restarted_recently(c), mina_containers))

  fraction_recently_restarted = len(recently_restarted_containers)/len(mina_containers)
  print(len(recently_restarted_containers), 'of', len(mina_containers), 'recently restarted')

  cluster_crashes.set(fraction_recently_restarted)

# ========================================================================

def pods_with_no_new_logs(v1, namespace, nodes_with_no_new_logs):
  print('counting pods with no new logs')
  pods = v1.list_namespaced_pod(namespace, watch=False)

  one_hour = 60 * 60

  count = 0
  total_running_pods = 0
  for pod in pods.items:
    if pod.status.phase == 'Running':
      total_running_pods += 1
      containers = pod.status.container_statuses
      mina_containers = list(filter(lambda c: c.name in [ 'coda', 'seed', 'coordinator' ], containers))
      if len(mina_containers) != 0:
        name = pod.metadata.name
        recent_logs = v1.read_namespaced_pod_log(name=name, namespace=namespace, since_seconds=one_hour, container=mina_containers[0].name)
        if len(recent_logs) == 0:
          print("Pod {} has no logs for the last hour".format(name))
          count += 1
    else:
      print("Pod {} is not running. Phase: {}, reason: {}".format(pod.metadata.name,pod.status.phase, pod.status.reason))

  fraction_no_new_logs = float(count) / float(total_running_pods)
  print(count, 'of', total_running_pods, 'pods have no logs in the last 10 minutes')

  nodes_with_no_new_logs.set(fraction_no_new_logs)

# ========================================================================

from node_status_metrics import collect_node_status_metrics

# ========================================================================

def check_google_storage_bucket(v1, namespace, recent_google_bucket_blocks):
  print('checking google storage bucket')

  bucket = 'mina_network_block_data'
  now = time.time()

  storage_client = storage.Client()
  blobs = list(storage_client.list_blobs(bucket, prefix=namespace))

  blob_ages = [ now - b.generation/1e6 for b in blobs ]

  newest_age = min([ age for age in blob_ages ])

  recent_google_bucket_blocks.set(newest_age)

# ========================================================================

def daemon_containers(v1, namespace):
  pods = v1.list_namespaced_pod(namespace, watch=False)

  for pod in pods.items:
    if pod.status.phase == 'Running':
      containers = pod.status.container_statuses
      for c in containers:
        if c.name in [ 'coda', 'mina', 'seed']:
          yield (pod.metadata.name, c.name)

def get_chain_id(v1, namespace):
  for (pod_name, container_name) in daemon_containers(v1, namespace):
    try:
      resp = util.exec_on_pod(v1, namespace, pod_name, container_name, 'mina client status --json')      
      resp = resp.strip()
      if resp[0] != '{':
        #first line could be 'Using password from environment variable CODA_PRIVKEY_PASS'
        resp = resp.split("\n", 1)[1]
      resp_dict = ast.literal_eval(resp.strip())
      print("Chain ID: {}".format(resp_dict['chain_id']))
      return resp_dict['chain_id']
    except Exception as e:
      print("Exception when extracting chain id on pod {}: {}\n mina client status response: {}".format(pod_name, e, resp))
      continue

def check_seed_list_up(v1, namespace, seeds_reachable):
  print('checking seed list up')

  seed_peers_list_url = os.environ.get('SEED_PEERS_URL')

  with urllib.request.urlopen(seed_peers_list_url) as f:
    contents = f.read().decode('utf-8')

  seeds =  ' '.join(contents.split('\n'))
  #stdbuf -o0 is to disable buffering

  chain_id = get_chain_id(v1, namespace)
  if chain_id is None:
    print('could not get chain id')
  else:
    command = 'stdbuf -o0 check_libp2p/check_libp2p ' + chain_id + ' ' + seeds
    proc = subprocess.Popen(command,stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, text=True)
    for line in proc.stderr.readlines():
            print("check_libp2p error: {}".format(line))
    val = proc.stdout.read()
    print("check_libp2p output: {}".format(val))
    proc.stdout.close()
    proc.wait()

    res = json.loads(val)
    #checklibp2p returns whether or not the connection to a peerID errored
    fraction_up = sum(res.values())/len(res.values())
    seeds_reachable.set(fraction_up)

# ========================================================================
