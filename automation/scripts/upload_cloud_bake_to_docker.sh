#!/bin/bash

# Set defaults before parsing args

# arguments are
# --docker-tag=, --testnet=, --config-file=, --automation-commit=, and --cloud=
while [ $# -gt 0 ]; do
  case "$1" in
    --testnet=*)
      TESTNET="${1#*=}"
      ;;
    --docker-tag=*)
      DOCKER_TAG="${1#*=}"
      ;;
    --automation-commit=*)
      AUTOMATION_PATHSPEC="${1#*=}"
      ;;
  esac
  shift
done

# DOCKER_TAG=0.0.17-beta10-4.1-hardfork-qa-5d1183a
# TESTNET=qa-4point2
# AUTOMATION_PATHSPEC=3ca9bdc

echo Testnet is ${TESTNET}
echo Initial Docker Image is codaprotocol/mina-daemon:${DOCKER_TAG}
echo Mina Automation Pathspec is ${AUTOMATION_PATHSPEC}
echo Config File Path is ${CONFIG_FILE}

first7=$(echo ${AUTOMATION_PATHSPEC} | cut -c1-7)

hub_baked_tag="codaprotocol/mina-daemon-baked:${DOCKER_TAG}-${TESTNET}-${first7}"
gcr_baked_tag="gcr.io/o1labs-192920/mina-daemon-baked:${DOCKER_TAG}-${TESTNET}-${first7}"

docker tag "$gcr_baked_tag" "$hub_baked_tag" 
docker push "$hub_baked_tag"
