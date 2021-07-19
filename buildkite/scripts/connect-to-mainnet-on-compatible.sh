#!/bin/bash

set -eo pipefail

if [ ! "$BUILDKITE_PULL_REQUEST_BASE_BRANCH" = "compatible" ]; then
  echo "Not pulling against compatible, not running the connect test"
  exit 0
fi

# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y git apt-transport-https ca-certificates tzdata curl

TESTNET_NAME="mainnet"

source buildkite/scripts/export-git-env-vars.sh

echo "Installing mina daemon package: mina-${TESTNET_NAME}=${MINA_DEB_VERSION}"
echo "deb [trusted=yes] http://packages.o1test.net $MINA_DEB_CODENAME $MINA_DEB_RELEASE" | tee /etc/apt/sources.list.d/mina.list
apt-get update
apt-get install --allow-downgrades -y "mina-${TESTNET_NAME}=${MINA_DEB_VERSION}"

# Remove lockfile if present
rm ~/.mina-config/.mina-lock ||:

# Restart in the background
mina daemon \
  --peer-list-url "https://storage.googleapis.com/seed-lists/${TESTNET_NAME}_seeds.txt" \
& # -background

# Attempt to connect to the GraphQL client every 10s for up to 4 minutes
num_status_retries=24
for ((i=1;i<=$num_status_retries;i++)); do
  sleep 10s
  set +e
  mina client status
  status_exit_code=$?
  set -e
  if [ $status_exit_code -eq 0 ]; then
    break
  elif [ $i -eq $num_status_retries ]; then
    exit $status_exit_code
  fi
done

# Check that the daemon has connected to peers and is still up after 2 mins
sleep 2m
mina client status
if [ $(mina advanced get-peers | wc -l) -gt 0 ]; then
    echo "Found some peers"
else
    echo "No peers found"
    exit 1
fi
