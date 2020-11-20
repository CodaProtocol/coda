#!/bin/bash

set -eou pipefail

GITBRANCH=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD)
GITCOMMIT=$(git log -1 --pretty=format:%h)
TAG=$(echo ${GITBRANCH} | sed 's!/!-!; s!_!-!g')

# --no-cache \
cat dockerfiles/Dockerfile-rosetta | docker build \
  --force-rm \
  --target build-deps \
  -t gcr.io/o1labs-192920/coda-rosetta-build-deps:develop -

cat dockerfiles/Dockerfile-rosetta | docker build \
  --force-rm \
  --target opam-deps \
  --cache-from gcr.io/o1labs-192920/coda-rosetta-build-deps:develop \
   -t gcr.io/o1labs-192920/coda-rosetta-opam-deps:$TAG -

# Build with the dev profile
cat dockerfiles/Dockerfile-rosetta | docker build \
  --force-rm \
  --target builder \
  --cache-from gcr.io/o1labs-192920/coda-rosetta-opam-deps:$TAG \
  --build-arg "DUNE_PROFILE=dev" \
  --build-arg "CODA_BRANCH=${GITBRANCH}" \
  --build-arg "CODA_COMMIT=${GITCOMMIT}" \
  -t gcr.io/o1labs-192920/coda-rosetta-builder:dev-$TAG -

cat dockerfiles/Dockerfile-rosetta | docker build \
  --force-rm \
  --target production \
  --cache-from gcr.io/o1labs-192920/coda-rosetta-builder:dev-$TAG \
  --build-arg "DUNE_PROFILE=dev" \
  --build-arg "CODA_BRANCH=${GITBRANCH}" \
  --build-arg "CODA_COMMIT=${GITCOMMIT}" \
  -t gcr.io/o1labs-192920/coda-rosetta:dev-$TAG -

# Also build with the default dune profile
cat dockerfiles/Dockerfile-rosetta | docker build \
  --force-rm \
  --target builder \
  --cache-from gcr.io/o1labs-192920/coda-rosetta-opam-deps:$TAG \
  --build-arg "CODA_BRANCH=${GITBRANCH}" \
  --build-arg "CODA_COMMIT=${GITCOMMIT}" \
  -t gcr.io/o1labs-192920/coda-rosetta-builder:medium-curves-$TAG -

cat dockerfiles/Dockerfile-rosetta | docker build \
  --force-rm \
  --target production \
  --cache-from gcr.io/o1labs-192920/coda-rosetta-builder:medium-curves-$TAG \
  --build-arg "CODA_BRANCH=${GITBRANCH}" \
  --build-arg "CODA_COMMIT=${GITCOMMIT}" \
  -t gcr.io/o1labs-192920/coda-rosetta:medium-curves-$TAG -

docker push gcr.io/o1labs-192920/coda-rosetta-build-deps:develop
docker push gcr.io/o1labs-192920/coda-rosetta-opam-deps:$TAG

if [[ $TAG == develop ]]; then
  # docker push gcr.io/o1labs-192920/coda-rosetta-builder:medium-curves-$TAG
  # docker push gcr.io/o1labs-192920/coda-rosetta-builder:dev-$TAG
  docker push gcr.io/o1labs-192920/coda-rosetta:dev-$TAG
  docker push gcr.io/o1labs-192920/coda-rosetta:medium-curves-$TAG
fi
