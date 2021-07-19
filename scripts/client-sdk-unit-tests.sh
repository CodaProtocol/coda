#!/bin/bash

set -eo pipefail

# run client SDK tests in node

echo "Building client SDK..."
source ~/.profile
eval $(opam env)
make client_sdk
echo "Running unit tests in Javascript"
nodejs src/app/client_sdk/tests/run_unit_tests.js

# the Rosetta encodings are not part of the client SDK as such,
# but the SDK relies on them, so it's reasonable to compare
# the encodings here, rather than create another CI test

# native/consensus
echo "Building consensus native code for encodings..."
make rosetta_lib_encodings
echo "Running"
./_build/default/src/lib/rosetta_lib/test/test_encodings.exe > encodings.consensus

# js/nonconsensus
echo "Building nonconsensus Javascript code for encodings..."
make client_sdk
echo "Running"
nodejs src/app/client_sdk/tests/test_encodings.js > encodings.js.nonconsensus

diff encodings.consensus encodings.js.nonconsensus

if [ $? -ne 0 ]; then
    echo "Consensus and Javascript code generate different encodings";
    exit 1
fi

echo "SUCCESS"
