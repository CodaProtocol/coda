#!/usr/bin/env bash

BUILDDIR="${BUILDDIR:-deb_build}"

# Get CWD if run locally or run through "source"
CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

rm -rf "${BUILDDIR}"

mkdir -p "${BUILDDIR}/DEBIAN"

cat << EOF > "${BUILDDIR}/DEBIAN/control"
Package: mina-bp-stats-sidecar
Version: ${MINA_DEB_VERSION}
License: Apache-2.0
Vendor: none
Architecture: all
Maintainer: o(1)Labs <build@o1labs.org>
Installed-Size:
Depends: python3, python3-certifi
Section: base
Priority: optional
Homepage: https://minaprotocol.com/
Description: A telemetry sidecar that ships stats about node status
 back to Mina HQ for analysis.
 Built from ${GITHASH} by ${BUILD_URL}
EOF

mkdir -p "${BUILDDIR}/usr/local/bin"
mkdir -p "${BUILDDIR}/etc"
mkdir -p "${BUILDDIR}/etc/systemd/system/"

cp "${CURRENT_DIR}/sidecar.py" "${BUILDDIR}/usr/local/bin/mina-bp-stats-sidecar"
cp "${CURRENT_DIR}/mina-sidecar-example.json" "${BUILDDIR}/etc/mina-sidecar.json"
cp "${CURRENT_DIR}/mina-bp-stats-sidecar.service" "${BUILDDIR}/etc/systemd/system/mina-bp-stats-sidecar.service"

fakeroot dpkg-deb --build "${BUILDDIR}" "mina-sidecar_${MINA_DEB_VERSION}.deb"

rm -rf "${BUILDDIR}"
