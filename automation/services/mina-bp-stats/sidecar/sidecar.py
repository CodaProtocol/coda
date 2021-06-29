#!/usr/bin/env python3

import os
import json
import logging
import time
import math
import urllib.request
import urllib.parse

logging.basicConfig(level=logging.INFO)

MINA_CONFIG_FILE = '/etc/mina-sidecar.json'
MINA_BLOCK_PRODUCER_URL_ENVAR = 'MINA_BP_UPLOAD_URL'
MINA_NODE_URL_ENVAR = 'MINA_NODE_URL'

FETCH_INTERVAL = 60 * 3 # Fetch updates every 3 mins
ERROR_SLEEP_INTERVAL = 30 # On errors, sleep for 30s before trying again
FINALIZATION_THRESHOLD = 12 # 12 blocks back is considered "finalized"

SYNC_STATUS_GRAPHQL = '''
query SyncStatus {
  daemonStatus {
    syncStatus
    blockchainLength
  }
}
'''

FETCH_BLOCK_GRAPHQL = '''
query FetchBlockData($blockID: Int!) {
  version
  daemonStatus {
    blockchainLength
    syncStatus
    chainId
    commitId
    highestBlockLengthReceived
    highestUnvalidatedBlockLengthReceived
    stateHash
    blockProductionKeys
    uptimeSecs
  }
  block(height: $blockID) {
    stateHash
  }
}
'''

upload_url, node_url = (None, None)

if os.path.exists(MINA_CONFIG_FILE):
    logging.info("Found {} on the filesystem, using config file".format(MINA_CONFIG_FILE))
    with open(MINA_CONFIG_FILE) as f:
        config_file = f.read().strip()
    parsed_config_file = json.loads(config_file)
    upload_url = parsed_config_file['uploadURL'].rstrip('/')
    node_url = parsed_config_file['nodeURL'].rstrip('/')

if MINA_BLOCK_PRODUCER_URL_ENVAR in os.environ:
    logging.info("Found {} in the environment, using envar".format(MINA_BLOCK_PRODUCER_URL_ENVAR))
    upload_url = os.environ[MINA_BLOCK_PRODUCER_URL_ENVAR]

if MINA_NODE_URL_ENVAR in os.environ:
    logging.info("Found {} in the environment, using envar".format(MINA_NODE_URL_ENVAR))
    node_url = os.environ[MINA_NODE_URL_ENVAR]

if upload_url is None:
    raise Exception("Could not find {} or {} environment variable is not set.".format(MINA_CONFIG_FILE, MINA_BLOCK_PRODUCER_URL_ENVAR))

if node_url is None:
    raise Exception("Could not find {} or {} environment variable is not set.".format(MINA_CONFIG_FILE, MINA_NODE_URL_ENVAR))

def fetch_mina_status():
    url = node_url + '/graphql'
    request = urllib.request.Request(
        url,
        headers={'Content-Type': 'application/json'},
        data=json.dumps({
            "query": SYNC_STATUS_GRAPHQL,
            "variables": {},
            "operationName": "SyncStatus"
        }).encode()
    )
    response = urllib.request.urlopen(request)
    response_body = response.read().decode('utf-8')
    parsed_body = json.loads(response_body)['data']

    return parsed_body['daemonStatus']['syncStatus'], parsed_body['daemonStatus']['blockchainLength']

def fetch_block(block_id):
    url = node_url + '/graphql'
    request = urllib.request.Request(
        url,
        headers={'Content-Type': 'application/json'},
        data=json.dumps({
            "query": FETCH_BLOCK_GRAPHQL,
            "variables": {'blockID': block_id},
            "operationName": "FetchBlockData"
        }).encode()
    )

    response = urllib.request.urlopen(request)
    response_body = response.read().decode('utf-8')
    response_data = json.loads(response_body)['data']
    if response_data is None:
        raise Exception("Response seems to be an error! {}".format(response_body))

    return response_data

def send_update(block_data, block_height):
    block_data.update({
        "retrievedAt": math.floor(time.time() * 1000),
        "blockHeight": block_height
    })
    request = urllib.request.Request(
        upload_url,
        headers={'Content-Type': 'application/json'},
        data=json.dumps(block_data).encode()
    )

    response = urllib.request.urlopen(request)

    assert response.getcode() == 200, "Non-200 from BP flush endpoint! [{}] - ".format(response.getcode(), response.read())

def check_mina_node_sync_state_and_fetch_head():
    while True:
        try:
            mina_sync_status, current_head = fetch_mina_status()
            if mina_sync_status == "SYNCED":
                logging.debug("Mina sync status is acceptable ({}), continuing!".format(mina_sync_status))
                break
            logging.info("Mina sync status is {}. Sleeping for 5s and trying again".format(mina_sync_status))
        except Exception as fetch_exception:
            logging.exception(fetch_exception)

        time.sleep(5)

    return current_head

if __name__ == '__main__':
    logging.info("Starting Mina Block Producer Sidecar")

    # On init ensure our node is synced and happy
    head_block_id = check_mina_node_sync_state_and_fetch_head()

    # Go back FINALIZATION_THRESHOLD blocks from the tip to have a finalized block
    current_finalized_tip = head_block_id - FINALIZATION_THRESHOLD

    # We're done with init to the point where we can start shipping off data
    while True:
        try:
            logging.info("Fetching block {}...".format(current_finalized_tip))

            block_data = fetch_block(current_finalized_tip)

            logging.info("Got block data ", block_data)

            send_update(block_data, current_finalized_tip)

            current_finalized_tip = block_data['daemonStatus']['blockchainLength'] - FINALIZATION_THRESHOLD # Go set a new finalized block

            logging.info("Finished! New tip {}...".format(current_finalized_tip))

            time.sleep(FETCH_INTERVAL)
        except Exception as e:
            # If we encounter an error at all, log it, sleep, and then kick
            # off the init process to go fetch the current tip/head to ensure
            # we never try to fetch past 290 blocks (k=290)
            logging.exception(e)

            logging.error("Sleeping for {}s and trying again".format(ERROR_SLEEP_INTERVAL))

            time.sleep(ERROR_SLEEP_INTERVAL)

            head_block_id = check_mina_node_sync_state_and_fetch_head()

            logging.info("Found new head at {}".format(head_block_id))

            current_finalized_tip = head_block_id - FINALIZATION_THRESHOLD

            logging.info("Continuing with finalized tip block of {}".format(current_finalized_tip))
