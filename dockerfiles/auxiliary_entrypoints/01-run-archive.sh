#!/bin/bash

set -eo pipefail

if [[ -n ${RUN_ARCHIVE} ]]; then
    echo "Running Mina archive..."
    MINA_CONFIG_DIR=${MINA_CONFIG_DIR:-/root/.mina-config}
    MINA_CONFIG_FILE="${MINA_CONFIG_DIR}/daemon.json}"

    PK=${PK:-"B62qiZfzW27eavtPrnF6DeDSAKEjXuGFdkouC3T5STRa6rrYLiDUP2p"}
    SNARK_PK=${SNARK_PK:-"B62qjnkjj3zDxhEfxbn1qZhUawVeLsUr2GCzEz8m1MDztiBouNsiMUL"}

    echo "Contents of config file ${MINA_CONFIG_FILE}:"
    cat "${MINA_CONFIG_FILE}"

    CODA_TIME_OFFSET=${CODA_TIME_OFFSET:-0}
    MINA_TIME_OFFSET=${MINA_TIME_OFFSET:-0}
    
    CODA_PRIVKEY_PASS=${CODA_PRIVKEY_PASS:-""}
    MINA_PRIVKEY_PASS=${MINA_PRIVKEY_PASS:-""}

    exec mina daemon --generate-genesis-proof true --seed --demo-mode --config-dir ${MINA_CONFIG_DIR} --block-producer-pubkey ${PK} --run-snark-worker ${SNARK_PK} -insecure-rest-server $@

    export ARCHIVE_PID=$

    rc=$?
    echo "Exiting Mina demo." && exit $rc
fi
