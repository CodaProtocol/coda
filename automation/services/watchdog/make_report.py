#!/usr/bin/env python3

# example of running locally
# MAKE_REPORT_DISCORD_WEBHOOK_URL=""
# python3 services/watchdog/make_report.py -n $namespace --discord_webhook_url $MAKE_REPORT_DISCORD_WEBHOOK_URL -a "$(cat accounts.csv)"

import sys
import os
import traceback
import argparse
import time
import random
import itertools
import numpy as np
import math
import ast
import json
import csv
import util
from os import listdir
from os.path import isfile, join
from graphviz import Digraph
from datetime import datetime
import uuid
from collections import Counter

from kubernetes import client, config, stream
from discord_webhook import DiscordWebhook

namespace = ''
discord_webhook_url = None
discord_char_limit = 2000

def peer_to_multiaddr(peer):
  return '/ip4/{}/tcp/{}/p2p/{}'.format(
    peer['host'],
    peer['libp2p_port'],
    peer['peer_id'] )

def main():

    global discord_webhook_url
    global namespace
    parser = argparse.ArgumentParser(description="Make a report for the active network and optionally send to discord")
    parser.add_argument("-n", "--namespace", help="testnet namespace", required=False, type=str, dest="namespace")
    parser.add_argument("-ic", "--incluster", help="if we're running from inside the cluster", required=False, default=False, type=bool, dest="incluster")
    parser.add_argument("-d", "--discord_webhook_url", help="discord webhook url", required=False, type=str, dest="discord_webhook_url")
    parser.add_argument("-a", "--accounts", help="community accounts csv", required=False, type=str, dest="accounts_csv")
    parser.add_argument("-l", "--local", help="run with a local node", required=False, type=bool, default=False)
    parser.add_argument("-b", "--bin", help="local mina binary", required=False, type=str, default="mina", dest="binary")

    # ==========================================

    args = parser.parse_args(sys.argv[1:])


    if args.incluster:
        config.load_incluster_config()
        assert(args.namespace == '')
        with open('/var/run/secrets/kubernetes.io/serviceaccount/namespace', 'r') as f:
            args.namespace = f.read()
    else:
        config.load_kube_config()
    v1 = client.CoreV1Api()

    namespace = args.namespace

    if namespace.startswith('integration-test-'):
      return

    discord_webhook_url = args.discord_webhook_url
    if discord_webhook_url is not None:
      discord_webhook_url = discord_webhook_url.strip()

    # ==========================================
    # Crawl network

    pods = v1.list_namespaced_pod(args.namespace, watch=False)

    pod_names = [ p['metadata']['name'] for p in pods.to_dict()['items'] if p['status']['phase'] == 'Running' ]

    seeds = [ p for p in pod_names if 'seed' in p ]

    seed = seeds[-1]

    seed_pod = [ p for p in pods.to_dict()['items'] if p['metadata']['name'] == seed ][0]
    seed_daemon_container = [ c for c in seed_pod['spec']['containers'] if c['args'][0] == 'daemon' ][0]
    seed_vars_dict = [ v for v in seed_daemon_container['env'] ]
    seed_daemon_port = [ v['value'] for v in seed_vars_dict if v['name'] == 'DAEMON_CLIENT_PORT'][0]

    print('seed', seed)

    request_timeout_seconds = 600

    def exec_locally(command):
      command = command.replace('mina', args.binary)
      print(command)
      import subprocess
      subprocess = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, text=True)
      res = subprocess.stdout.read()
      print('result', len(res))

      return res

      #import IPython; IPython.embed()

      #print(command)
      #sys.exit()
      #pass

    def exec_on_seed(command):

      def exec_cmd(command, timeout):
        exec_command = [
          '/bin/bash',
          '-c',
          command,
        ]
        result = stream.stream(v1.connect_get_namespaced_pod_exec, seed, args.namespace, command=exec_command, container='coda', stderr=True, stdout=True, stdin=False, tty=False, _request_timeout=timeout)
        return result

      print('running command:', command)

      tmp_file = '/tmp/cns_command.' + str(uuid.uuid4()) + '.out'

      start = time.time()
      result = exec_cmd(command + ' &> ' + tmp_file, request_timeout_seconds)
      end = time.time()

      print('done running command')
      print('\tseconds to run:', end - start)

      file_len = int(exec_cmd('stat --printf="%s" ' + tmp_file, 10))
      print('\tfile length:', str(file_len/(1024*1024)) + 'MB')


      read_segment = lambda start, size: exec_cmd('cat ' + tmp_file + ' | head -c ' + str(start + size) + ' | tail -c ' + str(size), 240)
      chunk_size = int(20e6)
      read_chunk = lambda i: read_segment(i*chunk_size, min((i+1)*chunk_size, file_len) - i*chunk_size)
      num_chunks = math.ceil(file_len/chunk_size)

      start = time.time()
      chunks = list(map(read_chunk, range(num_chunks)))
      result = ''.join(chunks)
      end = time.time()

      print('\tseconds to get result:', end - start)

      exec_cmd('rm ' + tmp_file, 10)

      return result

    if args.local:
      exec_command = exec_locally
    else:
      exec_command = exec_on_seed

    peer_table = {}

    queried_peers = set()

    node_status_heartbeat_errors = []
    node_status_transport_stopped_errors = []
    node_status_handshake_errors = []
    node_status_libp2p_errors = []
    node_status_other_errors = []

    uptime_less_than_10_min = []
    uptime_less_than_30_min = []
    uptime_less_than_1_hour = []
    uptime_less_than_6_hour = []
    uptime_less_than_12_hour = []
    uptime_less_than_24_hour = []
    uptime_greater_than_24_hour = []

    def contains_error(resp):
      try:
        resp['error']
        return True
      except KeyError :
        return False

    def no_error(resp):
      return (not (contains_error(resp)))

    def add_resp(resp):
      # we use ast instead of json to handle properties with single quotes instead of double quotes (which the response seems to often contain)
      resps = [ ast.literal_eval(s) for s in resp.split('\n') if s != '' ]

      print ('Received %s node_status responses'%(str(len(resps))))

      peers = list(filter(no_error,resps))
      error_resps = list(filter(contains_error,resps))

      err_context_deadline = 0
      err_negotiate_security_protocol = 0
      err_connection_refused = 0
      err_time_out = 0
      err_stream_reset = 0
      err_size_limit_exceeded = 0
      err_others = 0

      for p in error_resps:
        try:
          error_str = p['error']['string']
          if 'context deadline exceeded' in error_str:
            #{'error': {'commit_id': 'baffb589965aa0a8552dca15e209d2a011af3d21', 'string': 'RPC #369385 failed: "context deadline exceeded"'}}
            err_context_deadline += 1
          elif 'failed to negotiate security protocol' in error_str:
            #{'error': {'commit_id': 'baffb589965aa0a8552dca15e209d2a011af3d21', 'string': 'RPC #369384 failed: "failed to dial 12D3KooWEsc3KyWrxmDt8J8cBXBwztRrLcYrPKdJXWU4YLdC8z5z: all dials failed\\n  * [/ip4/185.25.49.250/tcp/8302] failed to negotiate security protocol: peer id mismatch: expected 12D3KooWEsc3KyWrxmDt8J8cBXBwztRrLcYrPKdJXWU4YLdC8z5z, but remote key matches 12D3KooWBLcxkHd3KQGeLiNgwVQ8ViEb5EYg3cmSjQs5tDDXQfQb"'}}
            err_negotiate_security_protocol += 1
          elif 'connection refused' in error_str:
            #{'error': {'commit_id': 'baffb589965aa0a8552dca15e209d2a011af3d21', 'string': 'RPC #369418 failed: "failed to dial 12D3KooWKWzRb7BN7J3zXF6PkRn3sJMRBxvq58ujoTHSUHcNmWdc: all dials failed\\n  * [/ip4/178.170.47.23/tcp/35592] dial tcp4 178.170.47.23:35592: connect: connection refused"'}}
            err_connection_refused +=1
          elif 'timed out requesting node status data from peer' in error_str:
            err_time_out += 1
          elif 'node status data was greater than' in error_str:
            print("Errored response: {}".format(error_str))
            err_size_limit_exceeded +=1
          elif 'stream reset' in error_str:
            err_stream_reset += 1
          else:
            print("Errored response: {}".format(error_str))
            err_others += 1
        except:
          print("Errored response: {}".format(p))
          err_others += 1

      print('\t%s valid responses from peers'%(str(len(list(peers)))))
      print('\t%s error responses'%(str(len(list(error_resps)))))
      print('=========================')        
      print('\t%s context deadline exceeded'%(str(err_context_deadline)))
      print('\t%s failed to negotiate security protocol'%(str(err_negotiate_security_protocol)))
      print('\t%s connection refused'%(str(err_connection_refused)))
      print('\t%s timed out requesting node status data from peer'%(str(err_time_out)))
      print('\t%s node status data size exceed limit'%(str(err_size_limit_exceeded)))
      print('\t%s stream reset'%(str(err_stream_reset)))
      print('\t%s other errors'%(str(err_others)))
      print('=========================')
      #if len(errors) > 100:
      #  import IPython; IPython.embed()

      key_value_peers = [ ((p['node_ip_addr'], p['node_peer_id']), p) for p in peers ]

      for (k,v) in key_value_peers:
        if k not in peer_table:
          peer_table[k] = v

      queried_peers.update([ p['node_peer_id'] for p in peers ])
      for p in peers:
        uptime = int(p['uptime_minutes'])
        if uptime < 10 :
          uptime_less_than_10_min.append(uptime)
        elif uptime < 30 :
          uptime_less_than_30_min.append(uptime)
        elif uptime < 60 :
          uptime_less_than_1_hour.append(uptime)
        elif uptime < 3600 :
          uptime_less_than_6_hour.append(uptime)
        elif uptime < 7200 :
          uptime_less_than_12_hour.append(uptime)
        elif uptime < 14400 :
          uptime_less_than_24_hour.append(uptime)
        else :
          uptime_greater_than_24_hour.append(uptime)

      for e in error_resps:
        error = str(e['error'])
        if 'handshake error' in error:
          node_status_handshake_errors.append(e)
        elif 'heartbeats' in error:
          node_status_heartbeat_errors.append(e)
        elif 'transport stopped' in error:
          node_status_transport_stopped_errors.append(e)
        elif 'libp2p' in error:
          node_status_libp2p_errors.append(e)
        else:
          node_status_other_errors.append(e)

    print ('Gathering node_status from daemon peers')

    seed_status = exec_command("mina client status")
    if seed_status == '':
      raise Exception("unable to connect to seed node within " + str(request_timeout_seconds) + " seconds" )

    get_status_value = lambda key: [ s for s in seed_status.split('\n') if key in s ][0].split(':')[1].strip()

    blocks = int(get_status_value('Max observed block height'))
    slot_time = get_status_value('Consensus time now')
    epoch, slot = [ int(s.split('=')[1]) for s in slot_time.split(',') ]
    slots_per_epoch = int(get_status_value('Slots per epoch'))
    global_slot = epoch*slots_per_epoch + slot

    for seed in seeds:
      seed_pod = [ p for p in pods.to_dict()['items'] if p['metadata']['name'] == seed ][0]
      seed_daemon_container = [ c for c in seed_pod['spec']['containers'] if c['args'][0] == 'daemon' ][0]
      seed_vars_dict = [ v for v in seed_daemon_container['env'] ]
      seed_daemon_port = [ v['value'] for v in seed_vars_dict if v['name'] == 'DAEMON_CLIENT_PORT'][0]

      cmd = "mina advanced node-status -daemon-port " + seed_daemon_port + " -daemon-peers" + " -show-errors"
      resp = util.exec_on_pod(v1, namespace, seed, 'coda', cmd)

      add_resp(resp)

    peer_numbers = [ len(node['peers']) for node in peer_table.values() ]
    peer_percentiles = [ 0, 5, 25, 50, 95, 100 ]

    if len(peer_numbers) > 0:
      peer_percentile_numbers = list(zip(peer_percentiles, np.percentile(peer_numbers, [ 0, 5, 25, 50, 95, 100 ])))
    else:
      peer_percentile_numbers = []

    block_producers = list(itertools.chain(*[ pv['block_producers'] for pv in peer_table.values() ]))

    if len(peer_table) > 0 and 'k_block_hashes' in list(peer_table.values())[0]:
      peer_to_k_block_hashes = { p: pv['k_block_hashes'] for p,pv in  peer_table.items() }
    else:
      peer_to_k_block_hashes = { p: [ a[0] for a in pv['k_block_hashes_and_timestamps'] ] for p,pv in  peer_table.items() }

    fork_tree = {}

    for block_hashes in peer_to_k_block_hashes.values():
      parents = block_hashes
      children_or_none = [ [ c ] for c in block_hashes[1:] ] + [ [] ]
      for parent, child_or_none in zip(parents, children_or_none):
        fork_tree.setdefault(parent, { 'children': set(), 'peers': 0 })
        fork_tree[parent]['children'].update(child_or_none)
        fork_tree[parent]['peers'] += 1

    children = list(itertools.chain(*[ v['children'] for v in fork_tree.values() ]))
    roots = set(fork_tree.keys()).difference(children)

    summarized_fork_tree = {}

    def add_to_summarized_tree(parent):
      peers = fork_tree[parent]['peers']
      children = fork_tree[parent]['children']
      intermediate_nodes = 0
      while len(children) == 1 and fork_tree[list(children)[0]]['peers'] == peers:
        children_or_none = fork_tree[list(children)[0]]['children']
        if len(children_or_none) > 0:
          children = fork_tree[list(children)[0]]['children']
          intermediate_nodes += 1
        else:
          break
      summarized_fork_tree[parent] = { 'children': children, 'peers': peers, 'intermediate_nodes': intermediate_nodes }
      for child in children:
        add_to_summarized_tree(child)

    for root in roots:
      add_to_summarized_tree(root)

    def has_forks():
      roots_with_children = [ root for root in roots if len(fork_tree[root]['children']) > 0 ]
      # can be multiple roots because of nodes syncing from genesis; however there shouldn't be multiple roots with children, that would indicate a fork longer than k
      if len(roots_with_children) > 1:
        return True
      if len(roots_with_children) == 0:
        return False
      root = roots_with_children[0]
      tips = [ node for node,values in summarized_fork_tree.items() if len(values['children']) == 0 ]
      tip_parents = [ node for node,values in summarized_fork_tree.items() if len(set(tips).intersection(values['children'])) > 0 ]
      # there can be different tips (since nodes can be 1 slot out of sync). A fork longer than one slot indicates there is a fork going on though, either from malicious behavior or a bug in the protocol. Note as long as the fork is less than k blocks the protocol consensus is safe and the protocol will recover when the attack ends
      if len(tip_parents) > 1:
        return True
      return False

    if args.accounts_csv is None:
      has_participants = False
    else:
      has_participants = len(args.accounts_csv.strip()) > 0

    participants_online = []
    participants_offline = []

    if has_participants:
      rows = []
      reader = csv.reader(args.accounts_csv.strip().split('\n'), delimiter=",")
      for row in reader:
        rows.append(row)

      discord_to_keys = {}
      key_to_discord = {}
      for r in rows:
        discord = r[0]
        key = r[1]
        if key != '':
          discord_to_keys.setdefault(discord, set())
          discord_to_keys[discord].add(key)
          key_to_discord[key] = discord

      online_discord_counts = {
        discord: len([ k for k in keys if k in block_producers ]) for discord, keys in discord_to_keys.items()
      }

      participants_online = [ d for d,count in online_discord_counts.items() if count > 0 ]
      participants_offline = [ d for d,count in online_discord_counts.items() if count == 0 ]
    else:
      key_to_discord = {}
      discord_to_keys = {}
      online_discord_counts = {}

    version_counts = dict(Counter([ v['git_commit'] for v in peer_table.values() if 'git_commit' in v ]))

    # --------------------
    # collect long-running data

    value_to_discords = lambda v: [ key_to_discord.get(key, '') for key in v['block_producers'] ]

    peer_table_dict = { str(k): { 'block_producers': v['block_producers'],
                               'protocol_state_hash': v['protocol_state_hash'],
                               'discord(s)': value_to_discords(v)  } for k,v in peer_table.items() }

    new_fname = 'peer_table.' + str(time.time()) + '.txt'
    with open(new_fname, 'w') as wfile:
      wfile.write(json.dumps(peer_table_dict))

    files = [f for f in listdir('.') if isfile(join('.', f))]

    windows_in_hours = [ 1, 3, 6, 12, 24, 72, 24*7 ]

    responding_peers_by_window = {}
    responding_ips_by_window = {}
    responding_discords_by_window = {}

    oldest_report = time.time()
    now = time.time()

    for fname in files:
      if 'peer_table' not in fname or fname.startswith('.') or not fname.endswith('txt'):
        continue
      with open(fname, 'r') as f:
        contents = f.read()
        peers = ast.literal_eval(contents)

        file_timestamp = float('.'.join(fname.split('.')[1:-1]))

        oldest_report = min(oldest_report, file_timestamp)

        ip_peer_ids = set(peers.keys())
        ips = set(map(lambda x: ast.literal_eval(x)[0], ip_peer_ids))
        discords = set(itertools.chain(*map(lambda x: x['discord(s)'], peers.values())))

        for w in windows_in_hours:
          responding_peers_by_window.setdefault(w, set())
          responding_ips_by_window.setdefault(w, set())
          responding_discords_by_window.setdefault(w, set())

          if (now - file_timestamp) <= w*3600:
            responding_peers_by_window[w].update(ip_peer_ids)
            responding_ips_by_window[w].update(ips)
            responding_discords_by_window[w].update(discords)


    # ==========================================
    # Make report

    report = {
      "namespace": args.namespace,
      "queried_nodes": len(queried_peers),
      "responding_nodes": len(peer_table),
      "node_status_handshake_errors": len(node_status_handshake_errors),
      "node_status_heartbeat_errors": len(node_status_heartbeat_errors),
      "node_status_transport_stopped_errors": len(node_status_transport_stopped_errors),
      "node_status_libp2p_errors": len(node_status_libp2p_errors),
      "node_status_other_errors": len(node_status_other_errors),
      "uptime_less_than_10_min": len(uptime_less_than_10_min),
      "uptime_less_than_30_min": len(uptime_less_than_30_min),
      "uptime_less_than_1_hour": len(uptime_less_than_1_hour),
      "uptime_less_than_6_hour": len(uptime_less_than_6_hour),
      "uptime_less_than_12_hour": len(uptime_less_than_12_hour),
      "uptime_less_than_24_hour": len(uptime_less_than_24_hour),
      "uptime_greater_than_24_hour": len(uptime_greater_than_24_hour),
      "epoch": epoch,
      "epoch_slot": slot,
      "global_slot": global_slot,
      "blocks": blocks,
      "block_fill_rate": blocks / global_slot,
      "number_of_peer_percentiles": peer_percentile_numbers, # TODO add health indicator
      "summarized_block_tree": summarized_fork_tree,
      "has_forks": has_forks(),
      "has_participants": has_participants,
      "participants_online": participants_online,
      "participants_offline": participants_offline,
      "peer_table": peer_table,
      "oldest_responses_report": oldest_report,
      "responding_peers_by_window": responding_peers_by_window,
      "responding_ips_by_window": responding_ips_by_window,
      "responding_discords_by_window": responding_discords_by_window,
      "online_discord_counts": online_discord_counts,
      "version_counts": version_counts,
    }


    # TODO
    # * timing of block receipt with a health indicator
    # * nodes sync statuses
    # * transaction counts with a health indicator
    # * check that all blocks have a coinbase

    # ==========================================

    # TODO
    # display of network connectivity

    def make_block_tree_graph():
      g = Digraph("block_tree", format='png')
      g.attr('node', shape='circle')
      for block in summarized_fork_tree:
        g.node(block, label='block ' + block[-6:] + '\n' + str(summarized_fork_tree[block]['peers']) + ' nodes')
      g.attr('node', shape='rectangle', style='filled', color='lightgrey')
      for block in summarized_fork_tree:
        children = summarized_fork_tree[block]['children']
        intermediate_nodes = summarized_fork_tree[block]['intermediate_nodes']
        if len(children) > 0:
          if intermediate_nodes > 0:
            g.node(block + '_intermediate', label=str(intermediate_nodes) + ' in common blocks')
            g.edge(block, block + '_intermediate')
            for child in children:
              g.edge(block + '_intermediate', child)
          else:
            for child in children:
              g.edge(block, child)
      g.render(view=False)

    make_block_tree_graph()

    copy = [ 'namespace', 'queried_nodes', 'responding_nodes', 'epoch', 'epoch_slot', 'global_slot', 'blocks', 'block_fill_rate', 'has_forks', 'has_participants',
             'node_status_handshake_errors', 'node_status_heartbeat_errors', 'node_status_transport_stopped_errors', 'node_status_libp2p_errors', 'node_status_other_errors',
             'uptime_less_than_10_min', 'uptime_less_than_30_min', 'uptime_less_than_1_hour', 'uptime_less_than_6_hour', 'uptime_less_than_12_hour',
             'uptime_less_than_24_hour', 'uptime_greater_than_24_hour' ]
    json_report = {}
    for c in copy:
      json_report[c] = report[c]

    json_report['participants_online'] = len(report['participants_online'])
    json_report['participants_offline'] = len(report['participants_offline'])

    json_report['number_of_peer_percentiles'] = ' | '.join([ str(p) + '%: ' + str(v) for (p,v) in report['number_of_peer_percentiles'] ])
    json_report['oldest_responses_report'] = str((now - report['oldest_responses_report'])/3600) + ' hours old'

    def format_responses_in_window(responding_in_window):
      responses_in_window = { k: len(v) for (k,v) in responding_in_window.items() }
      string = ' | '.join([ str(p) + ' hours: ' + str(v) for (p,v) in responses_in_window.items() ])
      return string

    json_report['unique_responding_peers_in_window'] = format_responses_in_window(report['responding_peers_by_window'])
    json_report['unique_responding_ips_in_window'] = format_responses_in_window(report['responding_ips_by_window'])
    json_report['unique_responding_discords_in_window'] = format_responses_in_window(report['responding_discords_by_window'])

    if json_report['has_forks']:
      json_report['has_forks'] = str(json_report['has_forks']) + ' :warning:'

    if json_report['block_fill_rate'] < .75 - .10:
      json_report['block_fill_rate'] = str(json_report['block_fill_rate']) + ' :warning:'

    formatted_report = json.dumps(json_report, indent=2)

    print(formatted_report)

    if discord_webhook_url is not None and len(discord_webhook_url) > 0:
      if len(formatted_report) > discord_char_limit - 5:
        formatted_report = formatted_report[:discord_char_limit - 5] + '...'


      webhook = DiscordWebhook(url=discord_webhook_url, content=formatted_report)

      with open("block_tree.gv.png", "rb") as f:
        webhook.add_file(file=f.read(), filename='block_tree.gv.png')

      webhook.add_file(file=str(report['participants_online']), filename='particpants_online.txt')
      webhook.add_file(file=str(report['participants_offline']), filename='participants_offline.txt')
      webhook.add_file(file=str(report['online_discord_counts']), filename='online_discord_counts.txt')

      peer_table_str = json.dumps(peer_table_dict, indent=2)

      webhook.add_file(file=peer_table_str, filename='peer_table.txt')

      webhook.execute()

if __name__ == "__main__":
  try:
    main()
  except Exception as e:
    exc_type, exc_obj, exc_tb = sys.exc_info()
    trace = traceback.format_exc()
    print(str(namespace) + " exited with error", trace)
    if discord_webhook_url is not None and len(discord_webhook_url) > 0:
      msg = str(namespace) + " exited with error: " + str(trace)
      if len(msg) > discord_char_limit - 5:
        msg = msg[:discord_char_limit - 5] + '...'
      webhook = DiscordWebhook(url=discord_webhook_url, content=msg)
      response = webhook.execute()
