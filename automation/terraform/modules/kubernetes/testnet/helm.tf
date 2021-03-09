provider "helm" {
  alias = "testnet_deploy"
  kubernetes {
    config_context = var.k8s_context
  }
}

locals {
  mina_helm_repo = "https://coda-charts.storage.googleapis.com"

  peers = var.additional_peers

  coda_vars = {
    runtimeConfig        = var.runtime_config
    image                = var.coda_image
    privkeyPass          = var.block_producer_key_pass
    seedPeers            = local.peers
    logLevel             = var.log_level
    logSnarkWorkGossip   = var.log_snark_work_gossip
    uploadBlocksToGCloud = var.upload_blocks_to_gcloud
    seedPeersURL         = var.seed_peers_url
  }

  seed_vars = {
    testnetName = var.testnet_name
    coda = {
      runtimeConfig = local.coda_vars.runtimeConfig
      image         = var.coda_image
      privkeyPass   = var.block_producer_key_pass
      // TODO: Change this to a better name
      seedPeers          = local.peers
      logLevel           = var.log_level
      logSnarkWorkGossip = var.log_snark_work_gossip
      ports = {
        client  = "8301"
        graphql = "3085"
        metrics = "8081"
        p2p     = var.seed_port
      }
      seedPeersURL         = var.seed_peers_url
      uploadBlocksToGCloud = var.upload_blocks_to_gcloud
    }

    seedConfigs = [
      for index, config in var.seed_configs : {
        name             = config.name
        class            = config.class
        libp2pSecret     = config.libp2p_secret
        privateKeySecret = config.private_key_secret
        externalPort     = config.external_port
        externalIp       = config.external_ip
        enableArchive    = config.enableArchive
        archiveAddress   = config.archiveAddress
      }
    ]
  }

  block_producer_vars = {
    testnetName = var.testnet_name

    coda = local.coda_vars

    userAgent = {
      image         = var.coda_agent_image
      minFee        = var.agent_min_fee
      maxFee        = var.agent_max_fee
      minTx         = var.agent_min_tx
      maxTx         = var.agent_max_tx
      txBatchSize   = var.agent_tx_batch_size
      sendEveryMins = var.agent_send_every_mins
      ports         = { metrics : 8000 }
    }

    bots = {
      image = var.coda_bots_image
      faucet = {
        amount = var.coda_faucet_amount
        fee    = var.coda_faucet_fee
      }
    }

    blockProducerConfigs = [
      for index, config in var.block_producer_configs : {
        name                 = config.name
        class                = config.class
        externalPort         = config.external_port
        runWithUserAgent     = config.run_with_user_agent
        runWithBots          = config.run_with_bots
        enableGossipFlooding = config.enable_gossip_flooding
        privateKeySecret     = config.private_key_secret
        libp2pSecret         = config.libp2p_secret
        enablePeerExchange   = config.enable_peer_exchange
        isolated             = config.isolated
        enableArchive        = config.enableArchive
        archiveAddress       = config.archiveAddress
      }
    ]
  }

  snark_worker_vars = {
    testnetName = var.testnet_name
    coda        = local.coda_vars
    worker = {
      active      = var.snark_worker_replicas > 0
      numReplicas = var.snark_worker_replicas
    }
    coordinator = {
      active        = var.snark_worker_replicas > 0
      deployService = var.snark_worker_replicas > 0
      publicKey     = var.snark_worker_public_key
      snarkFee      = var.snark_worker_fee
      hostPort      = var.snark_worker_host_port
    }
  }

  archive_node_vars = {
    testnetName = var.testnet_name
    coda = {
      image         = var.coda_image
      seedPeers     = local.peers
      runtimeConfig = local.coda_vars.runtimeConfig
      seedPeersURL  = var.seed_peers_url
    }
    node_configs = defaults(var.archive_configs, local.default_archive_node)
    postgresql   = { persistence = var.persistence_config }
  }

  watchdog_vars = {
    testnetName = var.testnet_name
    image       = var.watchdog_image
    coda = {
      image                = var.coda_image
      ports                = { metrics : 8000 }
      uploadBlocksToGCloud = var.upload_blocks_to_gcloud
    }
    restartEveryMins            = var.restart_nodes_every_mins
    restartNodes                = var.restart_nodes
    makeReports                 = var.make_reports
    makeReportEveryMins         = var.make_report_every_mins
    makeReportDiscordWebhookUrl = var.make_report_discord_webhook_url
    makeReportAccounts          = var.make_report_accounts
    seedPeersURL                = var.seed_peers_url
  }

}

# Cluster-Local Seed Node

resource "kubernetes_role_binding" "helm_release" {
  metadata {
    name      = "admin-role"
    namespace = kubernetes_namespace.testnet_namespace.metadata[0].name
  }

  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "ClusterRole"
    name      = "admin"
  }

  subject {
    kind      = "ServiceAccount"
    name      = "default"
    namespace = kubernetes_namespace.testnet_namespace.metadata[0].name
  }
}

resource "helm_release" "seeds" {
  provider = helm.testnet_deploy

  name       = "${var.testnet_name}-seeds"
  repository = var.use_local_charts ? "" : local.mina_helm_repo
  chart      = var.use_local_charts ? "../../../../helm/seed-node" : "seed-node"
  version    = "1.0.3"
  namespace  = kubernetes_namespace.testnet_namespace.metadata[0].name
  values = [
    yamlencode(local.seed_vars)
  ]
  wait    = false
  timeout = 600
  depends_on = [
    kubernetes_role_binding.helm_release
  ]
}

# Block Producer

resource "helm_release" "block_producers" {
  provider = helm.testnet_deploy
  count    = length(local.block_producer_vars.blockProducerConfigs) > 1 ? 1 : 0

  name       = "${var.testnet_name}-block-producers"
  repository = var.use_local_charts ? "" : local.mina_helm_repo
  chart      = var.use_local_charts ? "../../../../helm/block-producer" : "block-producer"
  version    = "0.5.3"
  namespace  = kubernetes_namespace.testnet_namespace.metadata[0].name
  values = [
    yamlencode(local.block_producer_vars)
  ]
  wait       = false
  timeout    = 600
  depends_on = [helm_release.seeds]
}

# Snark Worker

resource "helm_release" "snark_workers" {
  provider = helm.testnet_deploy
  count    = local.snark_worker_vars.coordinator.active ? 1 : 0

  name       = "${var.testnet_name}-snark-worker"
  repository = var.use_local_charts ? "" : local.mina_helm_repo
  chart      = var.use_local_charts ? "../../../../helm/snark-worker" : "snark-worker"
  version    = "0.4.9"
  namespace  = kubernetes_namespace.testnet_namespace.metadata[0].name
  values = [
    yamlencode(local.snark_worker_vars)
  ]
  wait       = false
  timeout    = 600
  depends_on = [helm_release.seeds]
}

# Archive Node

resource "helm_release" "archive_node" {
  provider = helm.testnet_deploy
  count    = length(local.archive_node_vars.node_configs)

  name       = "archive-${count.index + 1}"
  repository = var.use_local_charts ? "" : local.mina_helm_repo
  chart      = var.use_local_charts ? "../../../../helm/archive-node" : "archive-node"
  version    = "0.5.0"
  namespace  = kubernetes_namespace.testnet_namespace.metadata[0].name
  values = [
    yamlencode({
      testnetName = var.testnet_name
      coda        = local.archive_node_vars.coda
      archive     = local.archive_node_vars.node_configs[count.index]
      postgresql  = local.archive_node_vars.postgresql
    })
  ]

  wait       = false
  timeout    = 600
  depends_on = [helm_release.seeds]
}

# Watchdog

resource "helm_release" "watchdog" {
  provider = helm.testnet_deploy

  name       = "${var.testnet_name}-watchdog"
  repository = var.use_local_charts ? "" : local.mina_helm_repo
  chart      = var.use_local_charts ? "../../../../helm/watchdog" : "watchdog"
  version    = "0.1.0"
  namespace  = kubernetes_namespace.testnet_namespace.metadata[0].name
  values = [
    yamlencode(local.watchdog_vars)
  ]
  wait       = false
  timeout    = 600
  depends_on = [helm_release.seeds]
}

