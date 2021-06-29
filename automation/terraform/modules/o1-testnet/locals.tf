locals {
  block_producer_static_peers = {
    for index, name in keys(data.local_file.libp2p_peers) : name => {
      full_peer = "/dns4/${name}.${var.testnet_name}/tcp/${var.block_producer_starting_host_port + index}/p2p/${trimspace(data.local_file.libp2p_peers[name].content)}",
      port      = var.block_producer_starting_host_port + index
      name      = name
    }
  }

  seed_static_peers = {
    for index, name in keys(data.local_file.libp2p_seed_peers) : name => {
      full_peer = "/dns4/${name}.${var.testnet_name}/tcp/${var.seed_starting_host_port + index}/p2p/${trimspace(data.local_file.libp2p_seed_peers[name].content)}",
      port      = var.seed_starting_host_port + index
      name      = name
    }
  }

  default_archive_node = {
    image                   = var.coda_archive_image
    serverPort              = "3086"
    externalPort            = "11010"
    enableLocalDaemon       = true
    enablePostgresDB        = true

    postgresHost            = "archive-1-postgresql"
    postgresPort            = 5432
    postgresDB              = "archive"
    postgresqlUsername      = "postgres"
    postgresqlPassword      = "foobar"
    remoteSchemaFile        = var.mina_archive_schema

    persistenceEnabled      = true
    persistenceSize         = "8Gi"
    persistenceStorageClass = "ssd-delete"
    persistenceAccessModes  = ["ReadWriteOnce"]
    preemptibleAllowed      = "false"
  }

  static_peers = merge(local.block_producer_static_peers, local.seed_static_peers)

  whale_block_producer_names = [for i in range(var.whale_count) : "whale-block-producer-${i + 1}"]
  fish_block_producer_names  = [for i in range(var.fish_count) : "fish-block-producer-${i + 1}"]
  seed_names                 = [for i in range(var.seed_count) : "seed-${i + 1}"]

  archive_node_configs = var.archive_configs != null ? [for item in var.archive_configs : merge(local.default_archive_node, item)] : [
    for i in range(1, var.archive_node_count + 1) : merge(local.default_archive_node, {
      name              = "archive-${i}"
      postgresHost      = "archive-${i}-postgresql"
    })
  ]
}
