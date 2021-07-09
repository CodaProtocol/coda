#!/usr/bin/env bash
set -e -o pipefail
marlin_submodule_dir=$(git submodule status | grep marlin | sed 's/^[-\ ]//g' | cut -d ' ' -f 2)
marlin_repo_sha=$(cd $marlin_submodule_dir && git rev-parse --short=8 --verify HEAD)

echo "let marlin_repo_sha = \"$marlin_repo_sha\"" >> "$1"
