#!/bin/bash
set -o pipefail -x

TEST_NAME="$1"
MINA_IMAGE="gcr.io/o1labs-192920/mina-daemon:$MINA_VERSION-devnet-$MINA_GIT_HASH"
ARCHIVE_IMAGE="gcr.io/o1labs-192920/mina-archive:$MINA_VERSION-devnet-$MINA_GIT_HASH"

./test_executive.exe cloud "$TEST_NAME" \
  --mina-image "$MINA_IMAGE" \
  --archive-image "$ARCHIVE_IMAGE" \
  --mina-automation-location ./automation \
  | tee "$TEST_NAME.test.log" \
  | coda-logproc -i inline -f '!(.level in ["Debug", "Spam"])'
