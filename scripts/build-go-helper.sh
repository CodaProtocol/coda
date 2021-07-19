#!/usr/bin/env bash

# Helper script for compiling GO code
# Targets for compilation are to be specified
# as arguments to the script

set -e

if [[ "$GO" == "" ]];then
  GO=go
fi

if [[ $# -lt 1 ]]; then
  echo "No build targets specified"
  exit 2
fi

RESULT_BIN="$PWD/result/bin"
mkdir -p "$RESULT_BIN"

for f in "$@"; do
  ( cd "src/$f" && "$GO" build -o "$RESULT_BIN/$f" )
done
