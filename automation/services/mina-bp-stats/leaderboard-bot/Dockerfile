FROM python:3
WORKDIR /opt/minanet

COPY minanet_app/* ./
COPY requirements.txt .
COPY minanet_app/mina_cron /etc/cron.d/mina_cron

RUN apt-get update && apt-get -qq -y install cron
RUN pip3 install -r requirements.txt
RUN chmod +x ./entrypoint
RUN chmod +x ./mina_cron.sh

RUN chmod +x  /etc/cron.d/mina_cron
RUN crontab /etc/cron.d/mina_cron > /opt/minanet/cron-setup.log
ENTRYPOINT ["/opt/minanet/entrypoint"]
RUN touch /var/log/cron.log
CMD cron && tail -f /var/log/cron.log




