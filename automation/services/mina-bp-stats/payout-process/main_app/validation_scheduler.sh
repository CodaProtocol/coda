#!/bin/sh

/usr/local/bin/python payouts_validate.py 2>&1 /dev/null
script_output=$?
echo "epoch number in sh: "$script_output
if [ $script_output -lt 1 ]; then
	minutes_to_add=10
else
	minute_per_epoch=21420
	next_epoch_number=$script_output+1
	minutes_to_add=$((minute_per_epoch * next_epoch_number))
	minutes_to_add=$((minutes_to_add + (3000*3)))
fi
str_minutes="${minutes_to_add}minutes"
genesis_t=$(date --file=genesis_time.txt)
next_job_time=$(date -d "$genesis_t+$str_minutes")

current_date_time=$(date "+%Y%m%d%H%M%S")
#  date comaparision is only supported in number format
if [ "${current_date_time}" -ge $(date -d "$genesis_t+$str_minutes" "+%Y%m%d%H%M%S") ];
then
	str_minutes="10minutes"
	next_job_time=$(date -d "10 minutes" )
	
fi
# at support date in format --> %H:%M %m/%d/%y
formatted_job_time=$(date -d "$next_job_time" "+%H:%M %m/%d/%y")
echo "sh /opt/minanet/payout_process/validation_scheduler.sh" | at $formatted_job_time