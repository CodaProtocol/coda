#!/bin/sh

/usr/local/bin/python test.py 2>&1 /dev/null
script_output=$?
if [ $script_output -lt 1 ]; then
	minutes_to_add=1
else
	minute_per_epoch=21420
	next_epoch_number=$script_output
	minutes_to_add=$((minute_per_epoch * next_epoch_number))
	minutes_to_add=$((minutes_to_add + (3500*3))
fi
str_minutes="${minutes_to_add}minutes"
genesis_t=$(date --file=genesis_time.txt)

next_job_time=$(date -d "$genesis_t+$str_minutes")
formatted_job_time=$(date -d "$next_job_time" "+%H:%M %m/%d/%y")

echo "sh /opt/minanet/payout_process/validation_scheduler.sh" | at $formatted_job_time