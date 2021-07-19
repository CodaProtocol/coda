#!/bin/bash

set -eo pipefail

if [[ $# -ne 2 ]]; then
    echo "Usage: $0 <dune-profile> <path-to-source-tests>"
    exit 1
fi

profile=$1
path=$2

source ~/.profile
eval $(opam env)

echo "--- Make build"
export LIBP2P_NIXLESS=1 PATH=/usr/lib/go/bin:$PATH GO=/usr/lib/go/bin/go
time make build

echo "--- Run unit tests"
time dune runtest "${path}" --profile="${profile}" -j16 || (./scripts/link-coredumps.sh && false)
