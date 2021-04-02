FROM python:3
WORKDIR /opt/minanet

COPY minanet_app/download_batch_files.py .
COPY minanet_app/config.py .
COPY minanet_app/survey_collect.py .
COPY minanet_app/logger_util.py .
COPY minanet_app/mina-mainnet-303900-45050a0ba37b.json .
COPY requirements.txt .
COPY minanet_app/entrypoint .
COPY minanet_app/mina-cron.sh .
COPY minanet_app/mina-cron /etc/cron.d/mina-cron

RUN apt-get update && apt-get install -y rsyslog
RUN apt-get update && apt-get -qq -y install cron
RUN pip3 install -r requirements.txt
RUN chmod +x ./entrypoint
RUN chmod +x ./mina-cron.sh

RUN chmod +x  /etc/cron.d/mina-cron
RUN crontab /etc/cron.d/mina-cron > /opt/minanet/cron-setup.log
ENTRYPOINT ["/opt/minanet/entrypoint"]
RUN touch /var/log/cron.log
CMD cron && tail -f /var/log/cron.log




