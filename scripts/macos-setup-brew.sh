#!/bin/bash
set -eu

export HOMEBREW_NO_AUTO_UPDATE=1

NEEDED_PACKAGES=" bash boost cmake gmp gpatch jemalloc libffi libomp libsodium opam openssl@1.1 pkg-config zlib "
echo "Needed:  ${NEEDED_PACKAGES}"

CURRENT_PACKAGES=$(brew list | xargs)
echo "Current: ${CURRENT_PACKAGES}"

# Prune already installed packages from the todo list
for p in $CURRENT_PACKAGES; do
  NEEDED_PACKAGES=${NEEDED_PACKAGES// $p / }
done;

echo "Todo:    ${NEEDED_PACKAGES}"

# Remove old python
# https://discourse.brew.sh/t/python-2-eol-2020/4647
brew uninstall python@2

# only run if there's work to do
if [[ $NEEDED_PACKAGES = *[![:space:]]* ]]; then
  yes | brew install $NEEDED_PACKAGES
  brew update
else
  echo 'All required brew packages have already been installed.'
fi
