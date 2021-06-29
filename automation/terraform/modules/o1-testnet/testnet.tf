resource "google_compute_address" "seed_static_ip" {
  count        = var.seed_count
  name         = "${var.testnet_name}-seed-static-ip-${count.index + 1}"
  address_type = "EXTERNAL"
  region       = var.cluster_region
  project      = "o1labs-192920"
}

data "aws_route53_zone" "selected" {
  name = "o1test.net."
}

resource "aws_route53_record" "seed_record" {
  count   = var.seed_count
  zone_id = data.aws_route53_zone.selected.zone_id
  name    = "seed-${count.index + 1}.${var.testnet_name}.${data.aws_route53_zone.selected.name}"
  type    = "A"
  ttl     = "300"
  records = [google_compute_address.seed_static_ip[count.index].address]
}

module "kubernetes_testnet" {
  providers = { google = google.gke }
  source    = "../kubernetes/testnet"

  cluster_name   = var.cluster_name
  cluster_region = var.cluster_region
  k8s_context    = var.k8s_context
  testnet_name   = var.testnet_name

  use_local_charts   = false
  coda_image         = var.coda_image
  coda_archive_image = var.coda_archive_image
  coda_agent_image   = var.coda_agent_image
  coda_bots_image    = var.coda_bots_image
  coda_points_image  = var.coda_points_image
  watchdog_image     = var.watchdog_image

  coda_faucet_amount = var.coda_faucet_amount
  coda_faucet_fee    = var.coda_faucet_amount

  log_level           = var.log_level
  log_txn_pool_gossip = var.log_txn_pool_gossip
  log_precomputed_blocks = var.log_precomputed_blocks

  agent_min_fee         = var.agent_min_fee
  agent_max_fee         = var.agent_max_fee
  agent_min_tx          = var.agent_min_tx
  agent_max_tx          = var.agent_max_tx
  agent_send_every_mins = var.agent_send_every_mins

  additional_peers = [for peer in values(local.static_peers) : peer.full_peer]
  runtime_config   = data.local_file.genesis_ledger.content

  seed_zone   = var.seed_zone
  seed_region = var.seed_region

  archive_configs = local.archive_node_configs

  mina_archive_schema = var.mina_archive_schema

  snark_worker_replicas   = var.snark_worker_replicas
  snark_worker_fee        = var.snark_worker_fee
  snark_worker_public_key = var.snark_worker_public_key
  snark_worker_host_port  = var.snark_worker_host_port

  block_producer_key_pass = var.block_producer_key_pass
  block_producer_configs = concat(
    [
      for i in range(var.whale_count) : {
        name                   = local.whale_block_producer_names[i]
        class                  = "whale"
        id                     = i + 1
        external_port          = local.static_peers[local.whale_block_producer_names[i]].port
        private_key_secret     = "online-whale-account-${i + 1}-key"
        libp2p_secret          = "online-whale-libp2p-${i + 1}-key"
        enable_gossip_flooding = false
        run_with_user_agent    = false
        run_with_bots          = false
        enable_peer_exchange   = true
        isolated               = false
        enableArchive          = false
        archiveAddress         = length(local.archive_node_configs) != 0 ? "${element(local.archive_node_configs, i)["name"]}:${element(local.archive_node_configs, i)["serverPort"]}" : ""
      }
    ],
    [
      for i in range(var.fish_count) : {
        name                   = local.fish_block_producer_names[i]
        class                  = "fish"
        id                     = i + 1
        external_port          = local.static_peers[local.fish_block_producer_names[i]].port
        private_key_secret     = "online-fish-account-${i + 1}-key"
        libp2p_secret          = "online-fish-libp2p-${i + 1}-key"
        enable_gossip_flooding = false
        run_with_user_agent    = true
        run_with_bots          = false
        enable_peer_exchange   = true
        isolated               = false
        enableArchive          = false
        archiveAddress         = length(local.archive_node_configs) != 0 ? "${element(local.archive_node_configs, i)["name"]}:${element(local.archive_node_configs, i)["serverPort"]}" : ""
      }
    ]
  )

  seed_configs = [
    for i in range(var.seed_count) : {
      name               = local.seed_names[i]
      class              = "seed"
      id                 = i + 1
      external_port      = local.static_peers[local.seed_names[i]].port
      external_ip        = google_compute_address.seed_static_ip[i].address
      private_key_secret = "online-seeds-account-${i + 1}-key"
      libp2p_secret      = "online-seeds-libp2p-${i + 1}-key"
      enableArchive      = length(local.archive_node_configs) > 0
      archiveAddress     = length(local.archive_node_configs) > 0 ? "${element(local.archive_node_configs, i)["name"]}:${element(local.archive_node_configs, i)["serverPort"]}" : ""
    }
  ]

  upload_blocks_to_gcloud         = var.upload_blocks_to_gcloud
  restart_nodes                   = var.restart_nodes
  restart_nodes_every_mins        = var.restart_nodes_every_mins
  make_reports                    = var.make_reports
  make_report_every_mins          = var.make_report_every_mins
  make_report_discord_webhook_url = var.make_report_discord_webhook_url
  make_report_accounts            = var.make_report_accounts
  seed_peers_url                  = var.seed_peers_url
}
