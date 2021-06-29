#!/bin/bash

# fix-all-balances.sh -- patches bad combined fee transfer balances from genesis based on a swapped-balances.log file

# use the URI for your archive db
ARCHIVE_URI=${ARCHIVE_URI:-postgres://postgres@localhost:5432/archive}

# logs from the replayer
REPLAYER_LOG=${REPLAYER_LOG:-replayer.log}

# the log file generated with AWK script run on replayer log
LOG_FILE=${LOG_FILE:-swapped-balances.log}

# binary name/location for the mina replayer tool
REPLAYER=mina-replayer

# binary name/location for the mina swap bad balances tool
SWAPPER=mina-swap-bad-balances

REPLAYER_TEMPLATE=scripts/archive/replayer_template.json
REPLAYER_INPUT=replayer_input.json

read -p "State hash from canonical chain: " STATE_HASH

cp $REPLAYER_TEMPLATE $REPLAYER_INPUT

sed --in-place s/REPLACETHIS/$STATE_HASH/ $REPLAYER_INPUT

echo "---- Running replayer (takes several minutes)"
$REPLAYER --archive-uri "${ARCHIVE_URI}" --input-file replayer_input.json --output-file /dev/null > ${REPLAYER_LOG}

rm -f $REPLAYER_INPUT

echo "---- Finding swapped balances"
awk -f scripts/archive/find-swapped-balances.awk ${REPLAYER_LOG} > ${LOG_FILE}

BAD_COUNT=$(grep -- "-----" $LOG_FILE | wc -l)

echo "Found $BAD_COUNT swapped balances"

# Read over each line of the LOG_FILE and call SWAPPER on each STATE_HASH, SEQ_NUMBER pair
while read -r
do
  if [[ ${REPLY} != "---------------" ]]; then

    EXTRACTED_HASH=$(printf "%s\n" "$REPLY" | jq -rM .metadata.state_hash)
    case "$EXTRACTED_HASH" in
      null)
        EXTRACTED_MESSAGE=$(printf "%s\n" "$REPLY" | jq -rM .message)
        SEQ_NUMBER="${EXTRACTED_MESSAGE##* }"
        echo "---- Swapping balances for state hash ${STATE_HASH} at sequence number ${SEQ_NUMBER} ----"
        "${SWAPPER}" --archive-uri "${ARCHIVE_URI}" --state-hash "${STATE_HASH}" --sequence-no "${SEQ_NUMBER}"
        ;;

      *)
        STATE_HASH=$EXTRACTED_HASH
    esac
  fi
done <"$LOG_FILE"
