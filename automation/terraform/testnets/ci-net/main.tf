terraform {
  required_version = ">= 0.14.0"
  backend "s3" {
    key     = "ci-net.tfstate"
    encrypt = true
    region  = "us-west-2"
    bucket  = "o1labs-terraform-state"
    acl     = "bucket-owner-full-control"
  }
}

provider "aws" {
  region = "us-west-2"
}

variable "testnet_name" {
  type    = string
  default = ""
}

variable "coda_image" {
  type = string
}

variable "coda_archive_image" {
  type = string
}

variable "seed_count" {
  type    = number
  default = 1
}

variable "whale_count" {
  type    = number
  default = 1
}

variable "fish_count" {
  type    = number
  default = 1
}

variable "archive_count" {
  type    = number
  default = 1
}

variable "archive_configs" {
  description = "individual archive-node deployment configurations"
  default     = null
}

variable "snark_worker_count" {
  type    = number
  default = 1
}

variable "cluster_region" {
  type    = string
  default = "us-west1"
}

variable "k8s_ctx" {
  type    = string
  default = "gke_o1labs-192920_us-west1_mina-integration-west1"
}

variable "artifact_path" {
  type    = string
  default = "/tmp"
}

module "ci_testnet" {
  source = "../../modules/o1-testnet"

  artifact_path = var.artifact_path

  # TODO: remove obsolete cluster_name var + cluster region
  cluster_region = var.cluster_region
  k8s_context    = var.k8s_ctx
  testnet_name   = length(var.testnet_name) > 0 ? var.testnet_name : "ci-net-${substr(sha256(terraform.workspace), 0, 7)}"

  coda_image         = var.coda_image
  coda_archive_image = var.coda_archive_image
  coda_agent_image   = "codaprotocol/coda-user-agent:0.1.8"
  coda_bots_image    = "codaprotocol/coda-bots:0.0.13-beta-1"
  coda_points_image  = "codaprotocol/coda-points-hack:32b.4"
  watchdog_image     = "gcr.io/o1labs-192920/watchdog:0.4.6"

  archive_configs     = var.archive_configs
  mina_archive_schema = "https://raw.githubusercontent.com/MinaProtocol/mina/fd3980820fb82c7355af49462ffefe6718800b77/src/app/archive/create_schema.sql"

  coda_faucet_amount = "10000000000"
  coda_faucet_fee    = "100000000"

  agent_min_fee         = "0.05"
  agent_max_fee         = "0.1"
  agent_min_tx          = "0.0015"
  agent_max_tx          = "0.0015"
  agent_send_every_mins = "1"

  log_level           = "Info"
  log_txn_pool_gossip = false

  block_producer_key_pass           = "naughty blue worm"
  block_producer_starting_host_port = 10501

  snark_worker_host_port  = 10401
  whale_count             = var.whale_count
  fish_count              = var.fish_count
  seed_count              = var.seed_count
  snark_worker_replicas   = 1
  snark_worker_fee        = "0.025"
  snark_worker_public_key = "B62qk4nuKn2U5kb4dnZiUwXeRNtP1LncekdAKddnd1Ze8cWZnjWpmMU"

  upload_blocks_to_gcloud         = false
  restart_nodes                   = false
  restart_nodes_every_mins        = "60"
  make_reports                    = true
  make_report_every_mins          = "5"
  make_report_discord_webhook_url = ""
  make_report_accounts            = false
  seed_peers_url                  = ""
}
