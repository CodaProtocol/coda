#!/bin/bash

set -eou pipefail

function cleanup
{
  echo "Killing archive.exe"
  kill $(ps aux | egrep '/mina-bin/.*archive.exe' | grep -v grep | awk '{ print $2 }') || true
  echo "Killing mina.exe"
  kill $(ps aux | egrep '/mina-bin/.*mina.exe'    | grep -v grep | awk '{ print $2 }') || true
  echo "Killing rosetta.exe"
  kill $(ps aux | egrep '/mina-bin/rosetta'       | grep -v grep | awk '{ print $2 }') || true
  echo "Stopping postgres"
  pg_ctlcluster 11 main stop
  exit
}

trap cleanup TERM
trap cleanup INT

PG_CONN=postgres://$USER:$USER@localhost:5432/archiver

# Start postgres
pg_ctlcluster 11 main start

# wait for it to settle
sleep 3

export MINA_PRIVKEY_PASS=""
export MINA_LIBP2P_HELPER_PATH=/mina-bin/libp2p_helper

export MINA_CONFIG_FILE=${MINA_CONFIG_FILE:=/data/config.json}
export PEER_ID=${PEER_ID:=/ip4/34.74.175.158/tcp/10001/ipfs/12D3KooWAFFq2yEQFFzhU5dt64AWqawRuomG9hL8rSmm5vxhAsgr/}
export MINA_PORT=${MINA_DAEMON_PORT:=10101}
DEFAULT_FLAGS="-generate-genesis-proof true -peer ${PEER_ID} -archive-address 0.0.0.0:3086 -insecure-rest-server -log-level debug -external-port ${MINA_PORT}"
export MINA_FLAGS=${MINA_FLAGS:=$DEFAULT_FLAGS}
PK=${MINA_PK:=ZsMSUuKL9zLAF7sMn951oakTFRCCDw9rDfJgqJ55VMtPXaPa5vPwntQRFJzsHyeh8R8}

echo "MINA Flags: $MINA_FLAGS -config-file ${MINA_CONFIG_FILE}"

# archive
/mina-bin/archive/archive.exe run \
  -postgres-uri $PG_CONN \
  -config-file ${MINA_CONFIG_FILE} \
  -server-port 3086 &

# wait for it to settle
sleep 3

# Daemon w/ mounted config file, initial file is phase 3 config.json
/mina-bin/cli/src/mina.exe daemon \
    -config-file ${MINA_CONFIG_FILE} \
    ${MINA_FLAGS} $@ &

# wait for it to settle
sleep 3

# rosetta
/mina-bin/rosetta/rosetta.exe \
  -archive-uri $PG_CONN \
  -graphql-uri http://localhost:3085/graphql \
  -log-level debug \
  -port 3087 &

# wait for a signal
sleep infinity
