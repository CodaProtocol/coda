#!/bin/bash
set -eu

export HOMEBREW_NO_AUTO_UPDATE=1

# re-reads /usr/local/Homebrew if a cache has been put in place
brew update-reset

# remove some packages
brew uninstall python@2 || true

# install required packages
brew bundle install --file=scripts/Brewfile

# install rust
if rustup --version &>/dev/null; then
	echo "Rust is already installed"
else
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -- -y --default-toolchain stable
    export PATH="${HOME}/.cargo/bin:${PATH}"
fi

# export required paths
echo 'export PKG_CONFIG_PATH=$(brew --prefix openssl)/lib/pkgconfig' >> $HOME/.bash_profile
echo 'export PATH="$(brew --prefix openssl)/bin:$PATH"' >> $HOME/.bash_profile
