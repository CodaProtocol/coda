terraform {
  required_version = ">= 0.12.0"
  backend "s3" {
    key     = "o1-testnet-alerts.tfstate"
    encrypt = true
    region  = "us-west-2"
    bucket  = "o1labs-terraform-state"
    acl     = "bucket-owner-full-control"
  }
}

module "o1testnet_alerts" {
  source = "../modules/testnet-alerts"

  rule_filter            = "{testnet!~\"^(it-|ci-net|test-).+\"}" # omit testnets deployed by integration/CI tests
  alert_timeframe        = "1h"
  alert_duration         = "10m"
  pagerduty_alert_filter = "devnet2|mainnet"
}

output "testnet_alert_rules" {
  value = module.o1testnet_alerts.rendered_alerts_config
}

output "testnet_alert_receivers" {
  value     = module.o1testnet_alerts.rendered_receivers_config
  sensitive = true
}
