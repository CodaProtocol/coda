FROM python:3
WORKDIR /opt/minanet/payout_process

COPY main_app/* ./
COPY requirements.txt .

#COPY main_app/mina_cron /etc/cron.d/mina_cron

RUN apt-get update && apt-get -qq -y install cron && apt-get -qq -y install at
RUN pip3 install -r requirements.txt
RUN chmod +x ./entrypoint
RUN chmod +x ./*.sh

ENTRYPOINT ["/opt/minanet/payout_process/entrypoint"]
RUN sh /opt/minanet/payout_process/calculation_scheduler.sh
RUN sh /opt/minanet/payout_process/validation_scheduler.sh








