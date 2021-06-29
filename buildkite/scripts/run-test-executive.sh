#!/bin/bash
set -o pipefail -x

TEST_NAME="$1"
MINA_IMAGE="gcr.io/o1labs-192920/mina-daemon:$MINA_VERSION-devnet-$MINA_GIT_HASH"
ARCHIVE_IMAGE="gcr.io/o1labs-192920/mina-archive:$MINA_VERSION-$MINA_GIT_HASH"

./test_executive.exe cloud "$TEST_NAME" \
  --coda-image "$MINA_IMAGE" \
  --archive-image "$ARCHIVE_IMAGE" \
  --coda-automation-location ./automation \
  | tee "$TEST_NAME.test.log" \
  | coda-logproc -i inline -f '!(.level in ["Debug", "Spam"])'
