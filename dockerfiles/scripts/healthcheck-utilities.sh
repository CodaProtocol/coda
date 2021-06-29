#
# Determine whether a local daemon is SYNCed with its network
#

function updateSyncStatusLabel() {
    status=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { syncStatus } " }' localhost:3085/graphql | \
            jq '.data.syncStatus'
    )
    str=$(echo ${status} | sed 's/"//g' )
    kubectl label --overwrite=true pod -l app=$1 syncStatus=${str}
}

function isDaemonSynced() {
    status=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { syncStatus } " }' localhost:3085/graphql | \
            jq '.data.syncStatus'
    )
    case ${status} in
      \"BOOTSTRAP\")
        ;&
      \"CATCHUP\")
        ;&
      \"CONNECTING\")
        ;&
      \"SYNCED\")
        return 0
        ;;
      *)
        DAEMON_CONFIG="/root/daemon.json"
        if [ -f "$DAEMON_CONFIG" ]; then
            now=$(date +%s)
            timestamp=$(grep 'timestamp' ${DAEMON_CONFIG} | awk '{print $2}' | sed -e s/\"//g)
            timestamp_second=$(date -d ${timestamp} +%s)

            [[ $now -le $timestamp_seconds ]] && return 0 # special case to claim synced before the genesis timestamp
        fi
        echo "Daemon is out of sync with status: ${status}"

        return 1
    esac
}

#
# Determine whether a local daemon has processed the highest observed block
#
function isChainlengthHighestReceived() {
    chainLength=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { daemonStatus { blockchainLength } }" }' localhost:3085/graphql | \
            jq '.data.daemonStatus.blockchainLength'
    )

    highestReceived=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { daemonStatus { highestBlockLengthReceived } }" }' localhost:3085/graphql | \
            jq '.data.daemonStatus.highestBlockLengthReceived'
    )
    
    [[ "${chainLength}" == "${highestReceived}" ]] && return 0 ||
        (echo "Daemon chain length[${chainLength}] is not at highest received[${highestReceived}]." && return 1)
}

#
# Determine whether a local daemon has a peer count greater than some threshold
#
function peerCountGreaterThan() {
    peerCountMinThreshold=${1:-2}
    peerCount=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { daemonStatus { peers } }" }' localhost:3085/graphql | \
            jq '.data.daemonStatus.peers | length'
    )
    
    [[ $peerCount -gt $peerCountMinThreshold ]] && return 0 ||
        (echo "Peer count[${peerCount}] is not greater than mininum threshold[${peerCountMinThreshold}]." && return 1) 
}

#
# Determine whether a local daemon owns a wallet account and has allocated funds
#
function ownsFunds() {
    ownedWalletCount=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { ownedWallets }" }' localhost:3085/graphql | \
            jq '.data.ownedWallets | length'
    )
    balanceTotal=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { ownedWallets { balance { total } } }" }' localhost:3085/graphql | \
            jq '.data.ownedWallets[0].balance.total'
    )
    # remove leading and trailing quotes for integer interpretation
    balanceTotal=$(echo $balanceTotal | sed -e 's/^"//' -e 's/"$//')
    
    [[ $ownedWalletCount -gt 0 ]] && [[ $balanceTotal -gt 0 ]] && return 0 ||
        (echo "Owned wallet count[${ownedWalletCount}] and/or balance total[${balanceTotal}] is insufficient." && return 1) 
}

#
# Determine whether a local daemon process has sent sufficient user commands
#
function hasSentUserCommandsGreaterThan() {
    userCmdMinThreshold=${1:-1}
    userCmdSent=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { daemonStatus { userCommandsSent } }" }' localhost:3085/graphql | \
            jq '.data.daemonStatus.userCommandsSent'
    )
    
    [[ $userCmdSent -gt $userCmdMinThreshold ]] && return 0 ||
        (echo "User commands sent[${userCmdSent}] is not greater than mininum threshold[${userCmdMinThreshold}]." && return 1)
}

#
# Determine whether a SNARK coordinator has an assigned SNARK-worker
#
function hasSnarkWorker() {
    snarkWorker=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { daemonStatus { snarkWorker } }" }' localhost:3085/graphql | \
            jq '.data.daemonStatus.snarkWorker'
    )
    rc=$?
    
    [[ $rc == 0 ]] && [[ -n "$snarkWorker" ]] && return 0 ||
        (echo "Snark worker error: ${rc} - $snarkWorker" && return 1)
}

#
# Determine whether an Archive node's highest observed block is in sync with its local Mina daemon
#
function isArchiveSynced() {
    ## "Usage: $0 [--db-host <host>] [--db-port <port>] [--db-user <user>] [--db-password <pass>]"

    while [[ "$#" -gt 0 ]]; do case $1 in
        --db-host) host="$2"; shift;;
        --db-port) port="$2"; shift;;
        --db-user) user="$2"; shift;;
        --db-password) password="$2"; shift;;
        *) echo "Unknown parameter passed: $1"; exit 1;;
    esac; shift; done

    highestObserved=$(
        PGPASSWORD=${password:-foobar} psql -qtAX -h ${host:-localhost} -p ${port:-5432} -d archive -U ${user:-postgres} \
            -w -c "SELECT height FROM blocks ORDER BY height DESC LIMIT 1"
    )
    highestReceived=$(
        curl --silent --show-error --header "Content-Type:application/json" -d'{ "query": "query { daemonStatus { highestBlockLengthReceived } }" }' localhost:3085/graphql | \
            jq '.data.daemonStatus.highestBlockLengthReceived'
    )
    
    [[ $highestObserved == $highestReceived ]] && return 0 || (echo "Archive[${highestObserved}] is out of sync with local daemon[${highestReceived}]" && return 1)
}
