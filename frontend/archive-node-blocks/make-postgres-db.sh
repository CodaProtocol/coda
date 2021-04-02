#!/bin/bash

createdb archive_backup
#psql -d archive_test < create_schema.sql
psql archive_backup < pg_dump_archive_empty

sudo -u postgres psql -c "ALTER USER postgres WITH PASSWORD 'foobar';"


