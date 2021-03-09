terraform {
  required_version = ">= 0.14.0"
  backend "s3" {
    key     = "terraform-mainnet.tfstate"
    encrypt = true
    region  = "us-west-2"
    bucket  = "o1labs-terraform-state"
    acl     = "bucket-owner-full-control"
  }
}

provider "aws" {
  region = "us-west-2"
}

provider "google" {
  alias   = "google-us-east4"
  project = "o1labs-192920"
  region  = "us-east4"
  zone    = "us-east4-b"
}

provider "google" {
  alias   = "google-us-east1"
  project = "o1labs-192920"
  region  = "us-east1"
  zone    = "us-east1-b"
}

provider "google" {
  alias   = "google-us-central1"
  project = "o1labs-192920"
  region  = "us-central1"
  zone    = "us-central1-c"
}


variable "whale_count" {
  type = number

  description = "Number of online whales for the network to run"
  default     = 0
}

variable "fish_count" {
  type = number

  description = "Number of online fish for the network to run"
  default     = 0
}

variable "seed_count" {
  default     = 6
}

locals {
  testnet_name = "mainnet"
  coda_image = "gcr.io/o1labs-192920/coda-daemon-baked:1.0.2-06f3c5c-finalfinal2-fd9fee4"
  coda_archive_image = "gcr.io/o1labs-192920/coda-archive:1.0.2-06f3c5c"

  # replace with `make_report_discord_webhook_url = ""` if not in use (will fail if file not present)
  make_report_discord_webhook_url = <<EOT
    ${file("../../../discord_webhook_url.txt")}
  EOT

  # replace with `make_report_accounts = ""` if not in use (will fail if file not present)
  # make_report_accounts = <<EOT
  #   ${file("../../../${local.testnet_name}-accounts.csv")}
  # EOT
  make_report_accounts = ""
}

module "mainnet" {
  providers = { google.gke = google.google-us-east1 }
  source    = "../../modules/o1-testnet"

  artifact_path = abspath(path.module)

  cluster_name   = "coda-infra-east"
  cluster_region = "us-east1"
  k8s_context    = "gke_o1labs-192920_us-east1_coda-infra-east"
  testnet_name   = local.testnet_name

  coda_image         = local.coda_image
  coda_archive_image = local.coda_archive_image
  watchdog_image     = "gcr.io/o1labs-192920/watchdog:0.4.1"

  block_producer_key_pass = ""

  archive_node_count  = 3
  mina_archive_schema = "https://raw.githubusercontent.com/MinaProtocol/mina/06f3c5cdeb635b2564fcb2edac4f10db85b93c36/src/app/archive/create_schema.sql"

  archive_configs       = [
    {
      name = "archive-1"
      enableLocalDaemon = false
      enablePostgresDB  = true
      postgresHost      = "archive-1-postgresql"
    },
    {
      name = "archive-2"
      enableLocalDaemon = false
      enablePostgresDB  = false
      postgresHost      = "archive-3-postgresql"
    },
    {
      name = "archive-3"
      enableLocalDaemon = false
      enablePostgresDB  = true
      postgresHost      = "archive-3-postgresql"
    }
  ]

  log_level           = "Info"
  log_txn_pool_gossip = false

  snark_worker_replicas = 0
  whale_count           = var.whale_count
  fish_count            = var.fish_count
  seed_count            = var.seed_count

  upload_blocks_to_gcloud         = true
  restart_nodes                   = false
  restart_nodes_every_mins        = "60"
  make_reports                    = true
  make_report_every_mins          = "30"
  make_report_discord_webhook_url = local.make_report_discord_webhook_url
  make_report_accounts            = local.make_report_accounts
  seed_peers_url                  = "https://storage.googleapis.com/seed-lists/mainnet_seeds.txt?456"
}
