testnet_name       = "nightly"
coda_image         = "codaprotocol/coda-daemon:1.1.3-compatible"
coda_archive_image = "codaprotocol/coda-archive:1.1.3-compatible"
seed_count         = 1
whale_count        = 1
fish_count         = 1
archive_configs = [
  {
    name              = "archive-1"
    enableLocalDaemon = false
    enablePostgresDB  = true
    postgresHost      = "archive-1-postgresql"
  }
]
