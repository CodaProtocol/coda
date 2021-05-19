FROM python:3
WORKDIR /opt/minanet/payout_process

COPY requirements.txt .
RUN apt-get update && apt-get -qq -y install less at cron
RUN pip3 install -r requirements.txt

COPY main_app/* ./
COPY main_app/entrypoint ./entrypoint
RUN chmod +x ./entrypoint
RUN chmod +x ./*.sh

RUN touch /var/run/atd.pid
ENTRYPOINT ["/opt/minanet/payout_process/entrypoint"]
