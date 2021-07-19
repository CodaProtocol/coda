#!/bin/bash

TEST_DIR=/workdir/src/app/replayer/test
PGPASSWORD=arbitraryduck

set -eo pipefail

echo "Updating apt, installing packages"
apt-get update
# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

# time zone = US Pacific
/bin/echo -e "12\n10" | apt-get install -y tzdata
apt-get install -y git postgresql apt-transport-https ca-certificates curl

source buildkite/scripts/export-git-env-vars.sh

echo "deb [trusted=yes] http://packages.o1test.net $MINA_DEB_CODENAME $MINA_DEB_RELEASE" | tee /etc/apt/sources.list.d/mina.list
apt-get update

echo "Installing archive node package: mina-archive=${MINA_DEB_VERSION}"
apt-get install --allow-downgrades -y mina-archive=${MINA_DEB_VERSION}

echo "Generating locale for Postgresql"
locale-gen en_US.UTF-8

echo "Starting Postgresql service"
service postgresql start

echo "Populating archive database"
cd ~postgres
su postgres -c psql < $TEST_DIR/archive_db.sql
echo "ALTER USER postgres PASSWORD '$PGPASSWORD';" | su postgres -c psql
cd /workdir

echo "Running replayer"
mina-replayer --archive-uri postgres://postgres:$PGPASSWORD@localhost:5432/archive \
	      --input-file $TEST_DIR/input.json --output-file /dev/null
