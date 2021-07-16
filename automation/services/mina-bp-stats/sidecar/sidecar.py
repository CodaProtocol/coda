#!/usr/bin/env python3

import os
import json
import logging
import time
import math
import urllib.request

logging.basicConfig(level=logging.INFO)

MINA_CONFIG_FILE = '/etc/mina-sidecar.json'
MINA_BLOCK_PRODUCER_URL_ENVAR = 'MINA_BP_UPLOAD_URL'
MINA_NODE_URL_ENVAR = 'MINA_NODE_URL'

REQUEST_TIMEOUT = 45 # 45 second timeout on http requests
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
    with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT) as response:
        response_data = json.load(response)['data']

    daemon_status = response_data['daemonStatus']
    return daemon_status['syncStatus'], daemon_status['blockchainLength']

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
    with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT) as response:
        response_data = json.load(response)['data']

    if response_data is None:
        raise RuntimeError("Response seems to be an error! {}".format(response_body))
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
    with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT) as response:
        if response.getcode() != 200:
            raise RuntimeError("Non-200 from BP flush endpoint! [{}] - ".format(response.getcode(), response.read()))

def main():
    logging.info("Starting Mina Block Producer Sidecar")
    last_head = None
    operation_description = None
    while True:
        try:
            operation_description = 'fetching status'
            mina_sync_status, current_head = fetch_mina_status()
            if mina_sync_status != "SYNCED":
                logging.info("Mina sync status is {}. Sleeping for 5s and trying again".format(mina_sync_status))
                last_head = None
                time.sleep(5)
                continue

            logging.debug("Mina sync status is acceptable ({}), continuing!".format(mina_sync_status))
            if last_head == current_head:
                logging.debug("Tip {} unchanged, sleeping for 5s and trying again".format(current_head))
                time.sleep(5)
                continue

            # Go back FINALIZATION_THRESHOLD blocks from the tip to have a finalized block
            current_finalized_tip = current_head - FINALIZATION_THRESHOLD

            logging.info("Fetching block {}...".format(current_finalized_tip))

            operation_description = 'fetching block'
            block_data = fetch_block(current_finalized_tip)
            logging.info("Got block data {}".format(block_data))

            operation_description = 'sending update'
            send_update(block_data, current_finalized_tip)
            logging.info("Finished sending update for tip {}".format(current_finalized_tip))

            last_head = current_head

        except Exception as exc:
            # If we encounter an error at all, log it, sleep, and try again
            logging.exception('Exception occurred while %s: %s', operation_description, exc)
            logging.error("Sleeping for 5s and trying again")

            last_head = None

            time.sleep(5)

if __name__ == '__main__':
    upload_url, node_url = None, None

    if os.path.exists(MINA_CONFIG_FILE):
        logging.info("Found {} on the filesystem, using config file".format(MINA_CONFIG_FILE))
        with open(MINA_CONFIG_FILE) as f:
            parsed_config_file = json.load(f)
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

    main()
