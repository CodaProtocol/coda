#!/bin/bash

set -e

BIN=../../../_build/default/src/app/cli/src/mina.exe
PK=B62qmnkbvNpNvxJ9FkSkBy5W6VkquHbgN2MDHh1P8mRVX3FQ1eWtcxV
SNARK_PK=B62qjnkjj3zDxhEfxbn1qZhUawVeLsUr2GCzEz8m1MDztiBouNsiMUL

genesis_time=$(date -d '2019-01-30 20:00:00.000000Z' '+%s')
now_time=$(date +%s)

export MINA_TIME_OFFSET=$(( $now_time - $genesis_time ))
export CODA_PRIVKEY_PASS=""
export MINA_CONFIG_FILE=/tmp/config.json

exec $BIN daemon -seed -demo-mode -block-producer-key /tmp/keys/demo-block-producer -run-snark-worker $SNARK_PK -config-file /tmp/config.json -insecure-rest-server $@
