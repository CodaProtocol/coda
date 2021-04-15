testnet_name       = "nightly"
seed_count         = 2
whale_count        = 2
fish_count         = 2
snark_worker_count = 2
archive_configs = [
  {
    name              = "archive-1"
    enableLocalDaemon = true
    enablePostgresDB  = true
    postgresHost      = "archive-1-postgresql"
  },
  {
    name              = "archive-2"
    enableLocalDaemon = false
    enablePostgresDB  = false
    postgresHost      = "archive-1-postgresql"
  }
]
upload_blocks_to_gcloud = true
make_reports = true
