#!/bin/bash

set -eo pipefail

if [[ -n ${RUN_DEMO} ]]; then
    # Demo keys and config file
    echo "Running Mina demo..."
    MINA_CONFIG_DIR=${MINA_CONFIG_DIR:-/root/.mina-config}
    MINA_CONFIG_FILE="${MINA_CONFIG_DIR}/daemon.json}"

    PK=${PK:-"B62qiZfzW27eavtPrnF6DeDSAKEjXuGFdkouC3T5STRa6rrYLiDUP2p"}
    SNARK_PK=${SNARK_PK:-"B62qjnkjj3zDxhEfxbn1qZhUawVeLsUr2GCzEz8m1MDztiBouNsiMUL"}

    CONFIG_TEMPLATE=${CONFIG_TEMPLATE:-daemon.json.template}

    mkdir /root/keys && chmod go-rwx /root/keys
    mkdir -p --mode=700 ${MINA_CONFIG_DIR}/wallets/store/
    echo "$PK" >  ${MINA_CONFIG_DIR}/wallets/store/$PK.pub
    echo '{"box_primitive":"xsalsa20poly1305","pw_primitive":"argon2i","nonce":"6pcvpWSLkMi393dT5VSLR6ft56AWKkCYRqJoYia","pwsalt":"ASoBkV3NsY7ZRuxztyPJdmJCiz3R","pwdiff":[134217728,6],"ciphertext":"Dmq1Qd8uNbZRT1NT7zVbn3eubpn9Myx9Je9ZQGTKDxUv4BoPNmZAGox18qVfbbEUSuhT4ZGDt"}' > ${MINA_CONFIG_DIR}/wallets/store/${PK}
    chmod go-rwx ${MINA_CONFIG_DIR}/wallets/store/${PK}
    echo '{"genesis": {"genesis_state_timestamp": "${GENESIS_STATE_TIMESTAMP}"},"ledger":{"name":"mina-demo","accounts":[{"pk":"'${PK}'","balance":"66000","sk":null,"delegate":null}]}}' > ${CONFIG_TEMPLATE}

    if [ -z "$GENESIS_STATE_TIMESTAMP" ]; then
        GENESIS_STATE_TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    fi
    echo "Genesis State Timestamp for this run is: ${GENESIS_STATE_TIMESTAMP}"

    echo "Rewriting config file from template ${CONFIG_TEMPLATE} to ${MINA_CONFIG_FILE}"
    envsubst < ${CONFIG_TEMPLATE} > ${MINA_CONFIG_FILE}

    echo "Contents of config file ${MINA_CONFIG_FILE}:"
    cat "${MINA_CONFIG_FILE}"

    MINA_TIME_OFFSET=${MINA_TIME_OFFSET:-0}
    MINA_TIME_OFFSET=${MINA_TIME_OFFSET:-0}
    
    CODA_PRIVKEY_PASS=${CODA_PRIVKEY_PASS:-""}
    MINA_PRIVKEY_PASS=${MINA_PRIVKEY_PASS:-""}

    exec mina daemon --generate-genesis-proof true --seed --demo-mode --proof-level none --config-dir ${MINA_CONFIG_DIR} --block-producer-pubkey ${PK} --run-snark-worker ${SNARK_PK} -insecure-rest-server $@

    rc=$?
    echo "Exiting Mina demo." && exit $rc
fi
