#!/bin/bash

set -eo pipefail

if [[ -n ${LEDGER_OVERRIDE_URL} ]]; then
    echo "Genesis ledger override URL, ${LEDGER_OVERRIDE_URL}, provided. Overriding genesis ledger..."

    config_file="${CONFIG_FILE:-'/root/daemon.json'}"
    curl "${LEDGER_OVERRIDE_URL}" -o "$config_file"
    head "$config_file"

    mkdir -p /root/.mina-config && echo "{}" > /root/.mina-config/daemon.json
    mina daemon -config-file ${config_file} -generate-genesis-proof true
    mv ~/.mina-config/genesis/genesis_* /var/lib/coda/
fi
