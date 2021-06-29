#!/bin/bash

mina daemon --generate-genesis-proof true --peer-list-url https://storage.googleapis.com/mina-seed-lists/mainnet_seeds.txt --background

# wait 8 minutes for the node to hopefully come online and fully sync
sleep 480

echo "done sleeping"

# retry getting the staking ledger until the node is fully up and the command returns exit code 0
while true; do
  mina ledger export staking-epoch-ledger > staking_epoch_ledger.json
  if [ "$?" -eq 0 ] && [ "$(cat staking_epoch_ledger.json)" != "Ledger not found: current staking ledger not available" ]; then
    echo "staking epoch ledger dumped!"
    # cat staking_epoch_ledger.json
    break
  else
    echo "waiting for staking ledger to become available, sleeping for 30s"
    sleep 30
  fi
done

# get the next staking ledger.  no need to retry since the node is already up by now
mina ledger export next-epoch-ledger > next_epoch_ledger.json
echo "next epoch ledger dumped!"

# DATE="$(date +%F_%H%M)"
#extract the epoch number out of mina client status.  if the output format of mina client status changes, then this is gonna break
EPOCHNUM="$(mina client status | grep "Best tip consensus time" | grep -o "epoch=[0-9]*" | sed "s/[^0-9]*//g" )"

# rename the file in the required file name format
STAKING_HASH="$(mina ledger hash --ledger-file staking_epoch_ledger.json)"
STAKING_MD5="$(md5sum staking_epoch_ledger.json | cut -d " " -f 1 )"
LEDGER_FILENAME=staking-"$EPOCHNUM"-"$STAKING_HASH"-"$STAKING_MD5".json
mv ./staking_epoch_ledger.json ./$LEDGER_FILENAME

NEXT_STAKING_HASH="$(mina ledger hash --ledger-file next_epoch_ledger.json)"
NEXT_STAKING_MD5="$(md5sum next_epoch_ledger.json | cut -d " " -f 1 )"
NEXT_FILENAME=next-staking-"$EPOCHNUM"-"$NEXT_STAKING_HASH"-"$NEXT_STAKING_MD5".json
mv ./next_epoch_ledger.json ./$NEXT_FILENAME

echo "upload to a GCP cloud storage bucket"
gsutil -o Credentials:gs_service_key_file=/gcloud/keyfile.json cp $LEDGER_FILENAME gs://mina-staking-ledgers
gsutil -o Credentials:gs_service_key_file=/gcloud/keyfile.json cp $NEXT_FILENAME gs://mina-staking-ledgers
