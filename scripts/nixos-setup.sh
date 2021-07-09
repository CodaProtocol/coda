#!/usr/bin/env bash


nix-shell --run "git submodule update --init --recursive"
nix-shell --run "opam init -y"
nix-shell --run "opam switch create -y ocaml-base-compiler.4.11.2"
nix-shell --run "opam switch import -y src/opam.export"

set -e

nix-shell --run 'NIX_SODIUM_LDFLAGS=$(pkg-config --libs-only-L libsodium) ./scripts/pin-external-packages.sh'
nix-shell --run "make build"
