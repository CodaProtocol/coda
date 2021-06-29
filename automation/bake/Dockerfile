ARG BAKE_VERSION

FROM codaprotocol/coda-daemon:${BAKE_VERSION}

ARG COMMIT_HASH=master
ARG TESTNET_NAME=testworld
ARG CONFIG_FILE="/root/daemon.json"

RUN curl https://raw.githubusercontent.com/MinaProtocol/mina/${COMMIT_HASH}/automation/terraform/testnets/${TESTNET_NAME}/genesis_ledger.json -o "${CONFIG_FILE}"

RUN head ${CONFIG_FILE}

# Create the defautl config dir and an empty config
RUN mkdir -p /root/.mina-config
# && echo "{}" > /root/.mina-config/daemon.json

#RUN mina daemon -config-file ${CONFIG_FILE} -generate-genesis-proof true; \
#  mv ~/.mina-config/genesis/genesis_* /var/lib/coda/

#RUN curl https://raw.githubusercontent.com/MinaProtocol/mina/develop/dockerfiles/scripts/healthcheck-utilities.sh -o /healthcheck/utilities.sh

RUN echo '#!/bin/bash -x\n\
mkdir -p .mina-config\n\
touch .mina-config/mina-prover.log\n\
touch .mina-config/mina-verifier.log\n\
touch .mina-config/mina-best-tip.log\n\
command=$1 \n\
shift \n\
while true; do\n\
  rm -f /root/.mina-config/.mina-lock\n\
  mina "$command" -config-file "'${CONFIG_FILE}'" "$@" 2>&1 >mina.log &\n\
  mina_pid=$!\n\
  tail -q -f mina.log &\n\
  tail_pid=$!\n\
  wait "$mina_pid"\n\
  echo "Mina process exited with status code $?"\n\
  sleep 10\n\
  kill "$tail_pid"\n\
  if [ ! -f stay_alive ]; then\n\
    exit 0\n\
  fi\n\
done'\
> init_mina_baked.sh

RUN chmod +x init_mina_baked.sh

ENV CODA_TIME_OFFSET 0

ENTRYPOINT ["/usr/bin/dumb-init", "/root/init_mina_baked.sh"]
