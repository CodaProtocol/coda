#!/bin/sh

set -e

# compare test signatures generated:
# - in native executable, using consensus code
# - in native executable, using nonconsensus code
# - in JS code, using nonconsensus code

# build executables

# native/consensus
echo "Building consensus native code..."
make client_sdk_test_sigs
echo "Running"
./_build/default/src/app/client_sdk/tests/test_signatures.exe > nat.consensus.json

# js/nonconsensus
echo "Building nonconsensus JS code..."
make client_sdk
echo "Running"
nodejs src/app/client_sdk/tests/test_signatures.js > js.nonconsensus.json

# we've been careful so that the output formatting of all the signatures is the same
# so we can use diff (rather parsing the JSON with Python or such)

diff nat.consensus.json js.nonconsensus.json

if [ $? -ne 0 ]; then
    echo "Consensus and JS code generate different signatures";
    exit 1
fi

echo "SUCCESS"
