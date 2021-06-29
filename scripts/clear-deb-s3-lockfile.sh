#!/bin/bash

S3_LOCKFILE_DATE="$(aws s3 ls s3://packages.o1test.net/dists/unstable/main/binary-/lockfile | awk '{print $1 " " $2}')"
if [[ "$S3_LOCKFILE_DATE" == "" ]]; then
    echo "Could not get the lockfile timestamp from the S3 bucket. Have you set your AWS credentials correctly?"
    exit 1
fi
S3_LOCKFILE_SECONDS=$(date -d "$S3_LOCKFILE_DATE" +%s)
NOW_SECONDS=$(date +%s)
TIME_DIFF=$(($NOW_SECONDS - $S3_LOCKFILE_SECONDS))
if [[ $TIME_DIFF > 300 ]]; then
    echo "Lockfile has been held for > 5 mins. The deb-s3 instance is likely to have died. Deleting lockfile.."
    aws s3 rm s3://packages.o1test.net/dists/unstable/main/binary-/lockfile
    echo "Lockfile deleted"
else
    echo "Lockfile is younger than 5 mins. There may be a deb-s3 instance actively using it. Refusing to delete."
    exit 1
fi
