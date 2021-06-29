#!/bin/bash

set -eou pipefail

GITBRANCH=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD)
TAG=$(echo ${GITBRANCH} | sed 's!/!-!; s!_!-!g')
echo "--- Building and pushing gcr.io/o1labs-192920/mina-rosetta-build-deps:$TAG"

time cat dockerfiles/Dockerfile-rosetta | docker build \
  --target build-deps \
  --no-cache \
  -t gcr.io/o1labs-192920/mina-rosetta-build-deps:$TAG -

docker push gcr.io/o1labs-192920/mina-rosetta-build-deps:$TAG
