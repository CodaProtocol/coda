#!/bin/bash
set -euxo pipefail

# script to use in ci to pull down current proving and verificaiton keys
# uses exising circleci GC creds
# run as sudo

# 
sudo apt install -y jq

# cloud sdk debian install
export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)" 

if [ ! -f /etc/apt/sources.list.d/google-cloud-sdk.list ]; then
    echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
fi
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - 
apt-get update -y && apt-get install google-cloud-sdk -y

# credentials (inside circleci)
echo $JSON_GCLOUD_CREDENTIALS > google_creds.json
/usr/bin/gcloud auth activate-service-account --key-file=google_creds.json
/usr/bin/gcloud config set project $(cat google_creds.json | jq -r .project_id)

# Download keys
/usr/bin/gsutil cp gs://proving-keys-stable/* /tmp/.

# Unpack keys
mkdir -p /var/lib/coda
cd /var/lib/coda
tar --strip-components=2 -xvf /tmp/build-*.tar.bz2
