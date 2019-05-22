#!/bin/bash
set -eo pipefail

# Get fixed set of PV keys (which needs to be updated when snark changes)
if [ -z "$JSON_GCLOUD_CREDENTIALS" ]; then
    echo "WARNING: JSON_GCLOUD_CREDENTIALS not set, static PV keys not used"
    exit 0
fi

# GC credentials
echo $JSON_GCLOUD_CREDENTIALS > google_creds.json
/usr/bin/gcloud auth activate-service-account --key-file=google_creds.json
/usr/bin/gcloud config set project $(cat google_creds.json | jq -r .project_id)

# Debug output
set -x

# Get cached keys
echo "------------------------------------------------------------"
echo "Downloading keys"
PINNED_KEY_COMMIT=temporary_hack
TARBALL="keys-${PINNED_KEY_COMMIT}-${DUNE_PROFILE}.tar.bz2"
/usr/bin/gsutil cp gs://proving-keys-stable/$TARBALL /tmp/.

# Unpack keys
echo "------------------------------------------------------------"
echo "Unapacking keys"
sudo mkdir -p /var/lib/coda
cd /var/lib/coda
sudo tar --strip-components=2 -xvf /tmp/$TARBALL
rm /tmp/$TARBALL
