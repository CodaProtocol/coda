#!/usr/bin/env bash

set -e

out="$PWD/result"

ref_signer="$PWD/../../external/c-reference-signer"

mkdir -p "$out"/{headers,bin}
if [[ "$LIB_MINA_SIGNER" == "" ]]; then
  # No nix
  make -C "$ref_signer" libmina_signer.so
  cp "$ref_signer/libmina_signer.so" "$out"
else
  cp "$LIB_MINA_SIGNER" "$out"/libmina_signer.so
fi
cp "$ref_signer"/*.h "$out/headers"


if [[ "$1" == "test" ]]; then
  cd src
  LD_LIBRARY_PATH="$out" $GO test
else
  cd src/delegation_backend
  $GO build -o "$out/bin/delegation_backend"
  echo "to run use cmd: LD_LIBRARY_PATH=result ./result/bin/delegation_backend"
fi

