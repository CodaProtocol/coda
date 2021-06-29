locals {
  graphql_ingress_dns = "${var.testnet_name}.graphql.test.o1test.net"
  snark_worker_host_port            = 10001
  block_producer_starting_host_port = 10010

  seed_peer = {
    multiaddr = "/dns4/seed.${var.testnet_name}/tcp/10401/p2p/12D3KooWCoGWacXE4FRwAX8VqhnWVKhz5TTEecWEuGmiNrDt2XLf",
    peerid = "2D3KooWCoGWacXE4FRwAX8VqhnWVKhz5TTEecWEuGmiNrDt2XLf",
    private_key_secretbox = "{\"box_primitive\":\"xsalsa20poly1305\",\"pw_primitive\":\"argon2i\",\"nonce\":\"7YSQmeRbo4fGd2nYh9aS4WzNWzSXsFmmkTqKy93\",\"pwsalt\":\"9QsEJdSkFbF8PUwLPo2ZLHpgT7ja\",\"pwdiff\":[134217728,6],\"ciphertext\":\"7o8WU4cBiuUqGPaF2fNA815XqhZM5j95dhns5zztCiSehb3xVzTUSbCj1nDXG5rAteu67pvMnaGbQ57cQw1HEPB2DDdrtAAWbCt7qYjmP6cNm2L7H9DC8NKHs1LYuWvthfjDvxBDHnVidpRCyqtMBg9TPWtMPkZy1UCVRFokAA5HaA2xkh4WFgy2SCVrAeWNP5BeUGq9u779KcM9BAtg9n6rqbKDTybX4h1aSZ9qA72tg1LHzENfHLAgzJXZQcxhjvw6b8H51m9rVhwFTKPCRRCNXfcyQqjEpH4fm\"}",
    secret = "seed-discovery-keys",
    port = 10401
  }

  seed_config = {
    name               = "seed",
    class              = "seed",
    libp2p_secret      = local.seed_peer.secret,
    external_port      = 10401,
    node_port          = null,
    external_ip        = null,
    private_key_secret = null,
    enableArchive      = false,
    archiveAddress     = null
  }

  snark_coordinator_name = "snark-coordinator-${lower(substr(var.snark_worker_public_key, length(var.snark_worker_public_key) - 6, 6))}"

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

  archive_node_configs = var.archive_configs != null ? [for item in var.archive_configs : merge(local.default_archive_node, item)] : [
    for i in range(1, var.archive_node_count + 1) : merge(local.default_archive_node, {
      name              = "archive-${i}"
      postgresHost      = "archive-${i}-postgresql"
    })
  ]

  archive_node_names         = var.archive_node_count == 0 ? [ "" ] : [for i in range(var.archive_node_count) : "archive-${i + 1}:3086"]
}
