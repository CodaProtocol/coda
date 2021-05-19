#!/bin/sh
#!/usr/local/bin/env python
cd /opt/minanet/
CWD="$(pwd)"
/usr/local/bin/python send_email.py

current_date_time=$(date +"%Y-%m-%dT%H:%M:%S%z")
genesis_t=$(cat genesis_time.txt)
StartDate=$(date --date $genesis_t +"%s")
FinalDate=$(date --date $current_date_time +"%s")
MPHR=60    # Minutes per hour.
MINUTES=$(( ($FinalDate - $StartDate) / $MPHR ))
minute_per_epoch=21420
next_epoch_number=$((($MINUTES/$minute_per_epoch) +1))

minutes_to_add=$(((minute_per_epoch * next_epoch_number)+100))
str_minutes="${minutes_to_add}minutes"
next_job_time=$(date -d "$genesis_t+$str_minutes")
echo $next_job_time
formatted_job_time=$(date -d "$next_job_time" "+%H:%M %m/%d/%y")
echo "sh /opt/minanet/email_job.sh" | at $formatted_job_time