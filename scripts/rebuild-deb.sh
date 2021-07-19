#!/bin/bash

# Script collects binaries and keys and builds deb archives.

set -euo pipefail

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd "${SCRIPTPATH}/../_build"

GITHASH=$(git rev-parse --short=7 HEAD)
GITHASH_CONFIG=$(git rev-parse --short=8 --verify HEAD)

set +u
BUILD_NUM=${BUILDKITE_BUILD_NUM}
BUILD_URL=${BUILDKITE_BUILD_URL}
set -u

# Load in env vars for githash/branch/etc.
source "${SCRIPTPATH}/../buildkite/scripts/export-git-env-vars.sh"

cd "${SCRIPTPATH}/../_build"

# Set dependencies based on debian release
SHARED_DEPS="libssl1.1, libgmp10, libgomp1, libffi6"
case "${MINA_DEB_CODENAME}" in
  buster)
    DAEMON_DEPS=", libjemalloc2, libpq-dev, libprocps7"
    # buster deps that should only affect the toolchain container:
    # python3-sexpdata \
    # python-sexpdata \
    ;;
  stretch)
    DAEMON_DEPS=", libjemalloc1, libpq-dev, libprocps6"
    ;;
  *)
    echo "Unknown Debian codename provided: ${MINA_DEB_CODENAME}"; exit 1
    ;;
esac


BUILDDIR="deb_build"

##################################### GENERATE KEYPAIR PACKAGE #######################################

mkdir -p "${BUILDDIR}/DEBIAN"
cat << EOF > "${BUILDDIR}/DEBIAN/control"

Package: mina-generate-keypair
Version: ${MINA_DEB_VERSION}
License: Apache-2.0
Vendor: none
Architecture: amd64
Maintainer: o(1)Labs <build@o1labs.org>
Installed-Size:
Depends: ${SHARED_DEPS}
Section: base
Priority: optional
Homepage: https://minaprotocol.com/
Description: Utility to generate mina private/public keys in new format
 Utility to regenerate mina private public keys in new format
 Built from ${GITHASH} by ${BUILD_URL}
EOF

echo "------------------------------------------------------------"
echo "Control File:"
cat "${BUILDDIR}/DEBIAN/control"

# Binaries
mkdir -p "${BUILDDIR}/usr/local/bin"
cp ./default/src/app/generate_keypair/generate_keypair.exe "${BUILDDIR}/usr/local/bin/mina-generate-keypair"
cp ./default/src/app/validate_keypair/validate_keypair.exe "${BUILDDIR}/usr/local/bin/mina-validate-keypair"

# echo contents of deb
echo "------------------------------------------------------------"
echo "Deb Contents:"
find "${BUILDDIR}"

# Build the package
echo "------------------------------------------------------------"
fakeroot dpkg-deb --build "${BUILDDIR}" mina-generate-keypair_${MINA_DEB_VERSION}.deb
ls -lh mina*.deb

##################################### END GENERATE KEYPAIR PACKAGE #######################################

###### deb without the proving keys
echo "------------------------------------------------------------"
echo "Building mainnet deb without keys:"

rm -rf "${BUILDDIR}"
mkdir -p "${BUILDDIR}/DEBIAN"
cat << EOF > "${BUILDDIR}/DEBIAN/control"
Package: mina-mainnet
Version: ${MINA_DEB_VERSION}
Section: base
Priority: optional
Architecture: amd64
Depends: ${SHARED_DEPS}${DAEMON_DEPS}
Suggests: postgresql
Conflicts: mina-devnet
License: Apache-2.0
Homepage: https://minaprotocol.com/
Maintainer: o(1)Labs <build@o1labs.org>
Description: Mina Client and Daemon
 Mina Protocol Client and Daemon
 Built from ${GITHASH} by ${BUILD_URL}
EOF

echo "------------------------------------------------------------"
echo "Control File:"
cat "${BUILDDIR}/DEBIAN/control"

echo "------------------------------------------------------------"
# Binaries
mkdir -p "${BUILDDIR}/usr/local/bin"
sudo cp ./default/src/app/cli/src/mina_mainnet_signatures.exe "${BUILDDIR}/usr/local/bin/mina"
sudo cp ./default/src/app/rosetta/rosetta_mainnet_signatures.exe "${BUILDDIR}/usr/local/bin/mina-rosetta"

libp2p_location=../src/app/libp2p_helper/result/bin
ls -l ../src/app/libp2p_helper/result/bin || libp2p_location=$HOME/app/
p2p_path="${BUILDDIR}/usr/local/bin/coda-libp2p_helper"
cp $libp2p_location/libp2p_helper $p2p_path
chmod +w $p2p_path
# Only for nix builds
# patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 "${BUILDDIR}/usr/local/bin/coda-libp2p_helper"
chmod -w $p2p_path
cp ./default/src/app/logproc/logproc.exe "${BUILDDIR}/usr/local/bin/mina-logproc"
cp ./default/src/app/runtime_genesis_ledger/runtime_genesis_ledger.exe "${BUILDDIR}/usr/local/bin/mina-create-genesis"

mkdir -p "${BUILDDIR}/usr/lib/systemd/user"
cp ../scripts/mina.service "${BUILDDIR}/usr/lib/systemd/user/"

# Build Config
mkdir -p "${BUILDDIR}/etc/coda/build_config"
cp ../src/config/mainnet.mlh "${BUILDDIR}/etc/coda/build_config/BUILD.mlh"
rsync -Huav ../src/config/* "${BUILDDIR}/etc/coda/build_config/."

# Copy the genesis ledgers and proofs as these are fairly small and very valueable to have l
# Genesis Ledger/proof/epoch ledger Copy
mkdir -p "${BUILDDIR}/var/lib/coda"
for f in /tmp/coda_cache_dir/genesis*; do
    if [ -e "$f" ]; then
        mv /tmp/coda_cache_dir/genesis* "${BUILDDIR}/var/lib/coda/."
    fi
done

#copy config.json
cp '../genesis_ledgers/mainnet.json' "${BUILDDIR}/var/lib/coda/mainnet.json"
cp ../genesis_ledgers/devnet.json "${BUILDDIR}/var/lib/coda/devnet.json"
# The default configuration:
cp ../genesis_ledgers/mainnet.json "${BUILDDIR}/var/lib/coda/config_${GITHASH_CONFIG}.json"

# Bash autocompletion
# NOTE: We do not list bash-completion as a required package,
#       but it needs to be present for this to be effective
mkdir -p "${BUILDDIR}/etc/bash_completion.d"
cwd=$(pwd)
export PATH=${cwd}/${BUILDDIR}/usr/local/bin/:${PATH}
env COMMAND_OUTPUT_INSTALLATION_BASH=1 mina  > "${BUILDDIR}/etc/bash_completion.d/mina"

# echo contents of deb
echo "------------------------------------------------------------"
echo "Deb Contents:"
find "${BUILDDIR}"

# Build the package
echo "------------------------------------------------------------"
fakeroot dpkg-deb --build "${BUILDDIR}" mina-mainnet_${MINA_DEB_VERSION}.deb
ls -lh mina*.deb

###### deb with testnet signatures
echo "------------------------------------------------------------"
echo "Building testnet signatures deb without keys:"

cat << EOF > "${BUILDDIR}/DEBIAN/control"
Package: mina-devnet
Version: ${MINA_DEB_VERSION}
Section: base
Priority: optional
Architecture: amd64
Depends: ${SHARED_DEPS}${DAEMON_DEPS}
Suggests: postgresql
Conflicts: mina-mainnet
License: Apache-2.0
Homepage: https://minaprotocol.com/
Maintainer: o(1)Labs <build@o1labs.org>
Description: Mina Client and Daemon
 Mina Protocol Client and Daemon
 Built from ${GITHASH} by ${BUILD_URL}
EOF

echo "------------------------------------------------------------"
echo "Control File:"
cat "${BUILDDIR}/DEBIAN/control"


echo "------------------------------------------------------------"
# Overwrite binaries (sudo to fix permissions error)
sudo cp ./default/src/app/cli/src/mina_testnet_signatures.exe "${BUILDDIR}/usr/local/bin/mina"
sudo cp ./default/src/app/rosetta/rosetta_testnet_signatures.exe "${BUILDDIR}/usr/local/bin/mina-rosetta"

# Switch the default configuration to devnet.json:
sudo cp ../genesis_ledgers/devnet.json "${BUILDDIR}/var/lib/coda/config_${GITHASH_CONFIG}.json"

# echo contents of deb
echo "------------------------------------------------------------"
echo "Deb Contents:"
find "${BUILDDIR}"

# Build the package
echo "------------------------------------------------------------"
fakeroot dpkg-deb --build "${BUILDDIR}" mina-devnet_${MINA_DEB_VERSION}.deb
ls -lh mina*.deb

# TODO: Find a way to package keys properly without blocking/locking in CI
# TODO: Keys should be their own package, which this 'non-noprovingkeys' deb depends on
# For now, deleting keys in /tmp/ so that the complicated logic below for moving them short-circuits and both packages are built without keys
rm -rf /tmp/s3_cache_dir /tmp/coda_cache_dir

# Keys
# Identify actual keys used in build
# NOTE: Moving the keys from /tmp because of storage constraints. This is OK
# because building deb is the last step and therefore keys, genesis ledger, and
# proof are not required in /tmp
echo "Checking PV keys"
mkdir -p "${BUILDDIR}/var/lib/coda"
compile_keys=("step" "vk-step" "wrap" "vk-wrap" "tweedledee" "tweedledum")
for key in ${compile_keys[*]}
do
    echo -n "Looking for keys matching: ${key} -- "

    # Awkward, you can't do a filetest on a wildcard - use loops
    for f in  /tmp/s3_cache_dir/${key}*; do
        if [ -e "$f" ]; then
            echo " [OK] found key in s3 key set"
            mv /tmp/s3_cache_dir/${key}* "${BUILDDIR}/var/lib/coda/."
            break
        fi
    done

    for f in  /var/lib/coda/${key}*; do
        if [ -e "$f" ]; then
            echo " [OK] found key in stable key set"
            mv /var/lib/coda/${key}* "${BUILDDIR}/var/lib/coda/."
            break
        fi
    done

    for f in  /tmp/coda_cache_dir/${key}*; do
        if [ -e "$f" ]; then
            echo " [WARN] found key in compile-time set"
            mv /tmp/coda_cache_dir/${key}* "${BUILDDIR}/var/lib/coda/."
            break
        fi
    done
done

#remove build dir to prevent running out of space on the host machine
rm -rf "${BUILDDIR}"

# Build mina block producer sidecar 
../automation/services/mina-bp-stats/sidecar/build.sh
ls -lh mina*.deb
