#!/bin/bash

set -euo pipefail

# If glob doesn't match anything, return empty string rather than literal pattern
shopt -s nullglob


set -x

INPUT_ARGS="$@"

# These arrays can be overwritten or extended in scripts to adjust verbosity
# Example: LOG_FILES+=("${VERBOSE_LOG_FILES[@]}")
declare -a LOG_FILES=('mina.log')
declare -a VERBOSE_LOG_FILES=('.mina-config/mina-prover.log' '.mina-config/mina-verifier.log' '.mina-config/mina-best-tip.log')

# Attempt to execute or source custom entrypoint scripts accordingly
for script in /entrypoint.d/*; do
  if [ -x "$script" ]; then
    "$script" $INPUT_ARGS
  else
    source "$script"
  fi
done

# Mina daemon initialization
mkdir -p .mina-config
touch .mina-config/mina-prover.log
touch .mina-config/mina-verifier.log
touch .mina-config/mina-best-tip.log

while true; do
  rm -f .mina-config/.mina-lock
  mina $INPUT_ARGS 2>&1 >mina.log &
  mina_pid=$!

  tail -q -f "${LOG_FILES[@]}" &
  tail_pid=$!

  wait "$mina_pid"
  echo "Mina process exited with status code $?"

  kill "$tail_pid"
  if [ ! -f stay_alive ]; then
    sleep 10
    exit 0
  fi
done
