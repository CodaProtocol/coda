#!/bin/bash

set -e

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"

$SCRIPTPATH/rebuild-docker.sh $1 $2

img=$1:latest
project=$(gcloud config get-value project)

docker tag $img gcr.io/$project/$img
gcloud docker -- push gcr.io/$project/$img

