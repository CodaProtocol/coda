#!/bin/bash
set -x # echo on
set -eu

# Keep compile dirs to avoid recompiles
export OPAMKEEPBUILDDIR='true'
export OPAMREUSEBUILDDIR='true'
export OPAMYES=1

# Set term to xterm if not set
export TERM=${TERM:-xterm}

SWITCH='ocaml-variants.4.07.1+logoom'

if [[ -d ~/.opam ]]; then
  # ocaml environment
  eval $(opam config env)

  # check for cache'd opam
  SWITCH_LIST=$(opam switch list -s)

  # Check to see if we have explicit switch version
  SWITCH_FOUND=false
  for val in $SWITCH_LIST; do
    if [ $val == $SWITCH ]; then
      SWITCH_FOUND=true
    fi
  done
else
  # if there is no opam switch initialized, start from scratch
  SWITCH_FOUND=false
fi

pushd /home/opam/opam-repository && git pull && popd

if [ "$SWITCH_FOUND" = true ]; then
  # Add the o1-labs opam repository
  opam repository add --yes --all --set-default o1-labs https://github.com/o1-labs/opam-repository.git
  opam switch set $SWITCH
else
  # Build opam from scratch
  opam init
  # Add the o1-labs opam repository
  opam repository add --yes --all --set-default o1-labs https://github.com/o1-labs/opam-repository.git
  opam update
  opam switch create $SWITCH || true
  opam switch $SWITCH
fi

# All our ocaml packages
opam update
if [[ "$OSTYPE" == "darwin*" ]]; then
  PKG_CONFIG_PATH=$(brew --prefix openssl)/lib/pkgconfig LIBRARY_PATH=/usr/local/lib opam switch import src/opam.export
else
  opam switch import src/opam.export
fi

eval $(opam config env)

# Extlib gets automatically installed, but we want our pin, so we should
# uninstall here
opam uninstall extlib

# Our pins
opam pin add src/external/ocaml-sodium
opam pin add src/external/rpc_parallel
opam pin add src/external/ocaml-extlib

# workaround a permissions problem in rpc_parallel .git
sudo chmod -R u+rw ~/.opam

opam pin add src/external/async_kernel
opam pin add src/external/coda_base58
opam pin add src/external/graphql_ppx
eval $(opam config env)

# show switch list at end
echo "opam switch list"
opam switch list -s
