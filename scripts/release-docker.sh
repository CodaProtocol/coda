#!/bin/bash

# Author's Note: Because the structure of this repo is inconsistent (Dockerfiles and build contexts placed willy-nilly)
# we have to trustlist and configure image builds individually because each one is going to be slightly different.
# This is needed as opposed to trusting the structure of the each project to be consistent for every deployable.

set -eo pipefail
set +x

CLEAR='\033[0m'
RED='\033[0;31m'
# Array of valid service names
VALID_SERVICES=('mina-archive', 'mina-daemon' 'mina-daemon-puppeteered' 'bot' 'mina-rosetta', 'leaderboard')

function usage() {
  if [ -n "$1" ]; then
    echo -e "${RED}☞  $1${CLEAR}\n";
  fi
  echo "Usage: $0 [-s service-to-release] [-v service-version] [-c commit-hash]"
  echo "  -s, --service             The Service being released to Dockerhub"
  echo "  -v, --version             The version to be used in the docker image tag"
  echo "  -c, --commit              The commit hash to be appended to the docker image tag"
  echo ""
  echo "Example: $0 --service faucet --version v0.1.0 --commit abf678"
  echo "Valid Services: ${VALID_SERVICES[*]}"
  exit 1
}

while [[ "$#" -gt 0 ]]; do case $1 in
  --no-upload) NOUPLOAD=1;;
  --build-rosetta) BUILD_ROSETTA=true;;
  -s|--service) SERVICE="$2"; shift;;
  -v|--version) VERSION="$2"; shift;;
  -c|--commit) COMMIT="$2"; shift;;
  --extra-args) EXTRA=${@:2}; shift $((${#}-1));;
  *) echo "Unknown parameter passed: $1"; exit 1;;
esac; shift; done

# Debug prints for visability
echo 'service="'$SERVICE'" version="'$VERSION'" commit="'$COMMIT'"'
echo $EXTRA

# Verify Required Parameters are Present
if [ -z "$SERVICE" ]; then usage "Service is not set!"; fi;
if [ -z "$VERSION" ]; then usage "Version is not set!"; fi;
if [ -z "$COMMIT" ]; then usage "Commit is not set!"; fi;
if [ -z "$EXTRA" ]; then EXTRA=""; fi;
if [ $(echo ${VALID_SERVICES[@]} | grep -o "$SERVICE" - | wc -w) -eq 0 ]; then usage "Invalid service!"; fi

case $SERVICE in
mina-archive)
  DOCKERFILE_PATH="dockerfiles/Dockerfile-mina-archive"
  DOCKER_CONTEXT="dockerfiles/"
  ;;
bot)
  DOCKERFILE_PATH="frontend/bot/Dockerfile"
  DOCKER_CONTEXT="frontend/bot"
  ;;
mina-daemon)
  DOCKERFILE_PATH="dockerfiles/Dockerfile-mina-daemon"
  DOCKER_CONTEXT="dockerfiles/"
  ;;
mina-daemon-puppeteered)
  DOCKERFILE_PATH="dockerfiles/Dockerfile-mina-daemon-puppeteered"
  DOCKER_CONTEXT="dockerfiles/puppeteer-context"
  ;;
mina-rosetta)
  if [[ "$BUILD_ROSETTA" != "true" ]]; then
    echo "BUILD_ROSETTA env var not set, short-circuiting to avoid slow builds."
    exit 0
  fi
  DOCKERFILE_PATH="dockerfiles/Dockerfile-rosetta"
  ;;
leaderboard)
  DOCKERFILE_PATH="frontend/leaderboard/Dockerfile"
  DOCKER_CONTEXT="frontend/leaderboard"
  ;;
*)
esac


# If DOCKER_CONTEXT is not specified, assume none and just pipe the dockerfile into docker build
extra_build_args=$(echo $EXTRA | tr -d '"')
if [ -z "$DOCKER_CONTEXT" ]; then
  cat $DOCKERFILE_PATH | docker build $extra_build_args -t codaprotocol/$SERVICE:$VERSION -
else
  docker build $extra_build_args $DOCKER_CONTEXT -t codaprotocol/$SERVICE:$VERSION -f $DOCKERFILE_PATH
fi

tag-and-push() {
  docker tag "codaprotocol/$SERVICE:$VERSION" "$1"
  docker push "$1"
}

if [ -z "$NOUPLOAD" ] || [ "$NOUPLOAD" -eq 0 ]; then
  docker push "codaprotocol/$SERVICE:$VERSION"
  tag-and-push "codaprotocol/$SERVICE:$VERSION-$COMMIT"
  tag-and-push "gcr.io/o1labs-192920/$SERVICE:$VERSION"
  tag-and-push "gcr.io/o1labs-192920/$SERVICE:$VERSION-$COMMIT"
  # TODO: Properly set up minaprotocol docker repo
  # tag-and-push "minaprotocol/$SERVICE:$VERSION"
  # tag-and-push "minaprotocol/$SERVICE:$VERSION-$COMMIT"
fi
