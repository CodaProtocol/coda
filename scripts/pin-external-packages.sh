#!/bin/sh

set -e

# update and pin packages, used by CI

PACKAGES="$1"

if [[ "$PACKAGES" == "" ]]; then
  PACKAGES="ocaml-sodium rpc_parallel ocaml-extlib ocaml-extlib async_kernel coda_base58 graphql_ppx ppx_deriving_yojson"
fi

git submodule sync && git submodule update --init --recursive

for pkg in $PACKAGES; do
    echo "Pinning package" $pkg
    opam pin -y add src/external/$pkg
done
