#!/bin/bash

# TODO: make sure terraform installed

# implicit flags needed to be passed in via runInDocker
# BUILDKITE_GS_APPLICATION_CREDENTIALS_JSON
# $AWS_ACCESS_KEY_ID $AWS_SECRET_ACCESS_KEY

source DOCKER_DEPLOY_ENV
DAEMON_TAG="gcr.io/o1labs-192920/coda-daemon:${CODA_VERSION}-${CODA_GIT_HASH}"
ARCHIVE_TAG="gcr.io/o1labs-192920/coda-archive:${CODA_VERSION}-${CODA_GIT_HASH}"

echo "Nightly ENV: $NIGHTLY"
echo "DAEMON_TAG: $DAEMON_TAG"
echo "ARCHIVE_TAG: $ARCHIVE_TAG"

kubectl config current-context

if [ ! -z $NIGHTLY ]; then
  echo "Deploying Nightly"

  cd automation/terraform/testnets/nightly
  terraform init
  terraform destroy -auto-approve
  sleep 1m
  terraform apply -var="coda_image=${DAEMON_TAG}" -var="coda_archive_image=${ARCHIVE_TAG}" -auto-approve

else
  echo "Not deploying Nightly"
fi
