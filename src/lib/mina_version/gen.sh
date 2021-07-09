#!/usr/bin/env bash
set -e

commit_id_short=$(git rev-parse --short=8 --verify HEAD)
CWD=$PWD

if [ -n "$MINA_BRANCH" ]; then
  branch="$MINA_BRANCH"
else
  branch=$(git rev-parse --verify --abbrev-ref HEAD || echo "<none found>")
fi

# we are nested 5 directories deep (_build/<context>/src/lib/mina_version)
pushd ../../../../..
  if [ -n "$MINA_COMMIT_SHA1" ]; then
    # pull from env var if set
    id="$MINA_COMMIT_SHA1"
  else
    if [ ! -e .git ]; then echo 'Error: git repository not found'; exit 1; fi
    id=$(git rev-parse --verify HEAD)
    if [ -n "$(git diff --stat)" ]; then id="[DIRTY]$id"; fi
  fi
  commit_date=$(git show HEAD -s --format="%cI")
  pushd src/lib/marlin
    marlin_commit_id=$(git rev-parse --verify HEAD)
    if [ -n "$(git diff --stat)" ]; then marlin_commit_id="[DIRTY]$id"; fi
    marlin_commit_id_short=$(git rev-parse --short=8 --verify HEAD)
    marlin_commit_date=$(git show HEAD -s --format="%cI")
  popd
popd

echo "let commit_id = \"$id\"" > "$1"
echo "let commit_id_short = \"$commit_id_short\"" >> "$1"
echo "let branch = \"$branch\"" >> "$1"
echo "let commit_date = \"$commit_date\"" >> "$1"

echo "let marlin_commit_id = \"$marlin_commit_id\"" >> "$1"
echo "let marlin_commit_id_short = \"$marlin_commit_id_short\"" >> "$1"
echo "let marlin_commit_date = \"$marlin_commit_date\"" >> "$1"

echo "let print_version () = Core_kernel.printf \"Commit %s on branch %s\n\" commit_id branch" >> "$1"
