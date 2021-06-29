#!/bin/bash

set -eo pipefail

# execute pre-processing steps like zexe-standardize.sh if set
if [ -n "${PREPROCESSOR}" ]; then echo "--- Executing preprocessor" && ${PREPROCESSOR}; fi

eval `opam config env`
export PATH=/home/opam/.cargo/bin:/usr/lib/go/bin:$PATH
export GO=/usr/lib/go/bin/go

MINA_COMMIT_SHA1=$(git rev-parse HEAD)

# TODO: Stop building lib_p2p multiple times by pulling from buildkite-agent artifacts or docker or somewhere
echo "--- Build libp2p_helper TODO: use the previously uploaded build artifact"
make -C src/app/libp2p_helper

# Disabling to save ~1 minute, should not be necessary anymore
# echo "--- Generate runtime_genesis_ledger for mainnet"
# dune exec --profile=${DUNE_PROFILE} src/app/runtime_genesis_ledger/runtime_genesis_ledger.exe -- --config-file genesis_ledgers/mainnet.json

# echo "--- Upload genesis data"
# ./scripts/upload-genesis.sh

echo "--- Build all major tagets required for packaging"
echo "Building from Commit SHA: $MINA_COMMIT_SHA1"
dune build --profile=${DUNE_PROFILE} \
  src/app/logproc/logproc.exe \
  src/app/runtime_genesis_ledger/runtime_genesis_ledger.exe \
  src/app/generate_keypair/generate_keypair.exe \
  src/app/validate_keypair/validate_keypair.exe \
  src/app/cli/src/mina.exe \
  src/app/cli/src/mina_testnet_signatures.exe \
  src/app/cli/src/mina_mainnet_signatures.exe \
  src/app/rosetta/rosetta.exe \
  src/app/rosetta/rosetta_mainnet_signatures.exe \
  src/app/rosetta/rosetta_testnet_signatures.exe # 2>&1 | tee /tmp/buildocaml.log

echo "--- Build deb package without pvkeys"
make deb

echo "--- Upload deb to repo"
make publish_debs

echo "--- Copy artifacts to cloud"
# buildkite-agent artifact upload occurs outside of docker after this script exits
