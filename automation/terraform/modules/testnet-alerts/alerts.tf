# TODO: enable general loading and rendering of alert templates with custom input VARs

data "template_file" "testnet_alerts" {
  template = file("${path.module}/templates/testnet-alert-rules.yml.tpl")
  vars = {
    rule_namespace     = var.rule_namespace
    rule_filter        = var.rule_filter
    alerting_timeframe = var.rule_timeframe
  }
}

data "template_file" "testnet_alert_receivers" {
  template = file("${path.module}/templates/testnet-alert-receivers.yml.tpl")
  vars = {
    pagerduty_service_key  = data.aws_secretsmanager_secret_version.pagerduty_testnet_primary_key.secret_string
    pagerduty_alert_filter = var.pagerduty_alert_filter

    discord_alert_webhook = data.aws_secretsmanager_secret_version.discord_testnet_alerts_webhook.secret_string
  }
}

# Setup

resource "local_file" "alert_rules_config" {
  content  = data.template_file.testnet_alerts.rendered
  filename = "${path.cwd}/alert_rules.yml"
}

resource "null_resource" "download_cortextool" {
  provisioner "local-exec" {
    working_dir = path.cwd
    command = "which cortextool || (curl --fail --show-error --location --output ${local.cortextool_install_dir} ${local.cortextool_download_url} && chmod a+x ${local.cortextool_install_dir})"
  }
}

# Lint alerting config

resource "null_resource" "alert_rules_lint" {
  provisioner "local-exec" {
    working_dir = path.cwd
    command     = "cortextool rules lint --rule-files alert_rules.yml"
  }

  depends_on = [local_file.alert_rules_config, null_resource.download_cortextool]
}

resource "null_resource" "alert_rules_check" {
  provisioner "local-exec" {
    command     = "cortextool rules check --rule-files alert_rules.yml"
  }

  depends_on = [local_file.alert_rules_config, null_resource.download_cortextool]
}

resource "docker_container" "verify_alert_receivers" {
  name  = "cortex_verify_receivers"
  image = local.cortex_image
  command = [
    "alerts",
    "verify",
    "--address=${jsondecode(data.aws_secretsmanager_secret_version.alertmanager_api_auth.secret_string)["auth_url"]}",
    "--id=${jsondecode(data.aws_secretsmanager_secret_version.alertmanager_api_auth.secret_string)["id"]}",
    "--key=${jsondecode(data.aws_secretsmanager_secret_version.alertmanager_api_auth.secret_string)["password"]}"
  ]

  rm = true
}

# Deploy alert updates

resource "docker_container" "sync_alert_rules" {
  name  = "cortex_rules_sync"
  image = local.cortex_image
  command = [
    "rules",
    "sync",
    "--rule-files=/config/rules.yml",
    "--address=${jsondecode(data.aws_secretsmanager_secret_version.prometheus_api_auth.secret_string)["auth_url"]}",
    "--id=${jsondecode(data.aws_secretsmanager_secret_version.prometheus_api_auth.secret_string)["id"]}",
    "--key=${jsondecode(data.aws_secretsmanager_secret_version.prometheus_api_auth.secret_string)["password"]}"
  ]

  upload {
    content = data.template_file.testnet_alerts.rendered
    file    = "/config/rules.yml"
  }

  rm         = true
  depends_on = [null_resource.alert_rules_lint, null_resource.alert_rules_check]
}

resource "docker_container" "update_alert_receivers" {
  name  = "cortex_receivers_update"
  image = local.cortex_image
  command = [
    "alertmanager",
    "load",
    "/config/receivers.yml",
    "--address=${jsondecode(data.aws_secretsmanager_secret_version.alertmanager_api_auth.secret_string)["auth_url"]}",
    "--id=${jsondecode(data.aws_secretsmanager_secret_version.alertmanager_api_auth.secret_string)["id"]}",
    "--key=${jsondecode(data.aws_secretsmanager_secret_version.alertmanager_api_auth.secret_string)["password"]}"
  ]

  upload {
    content = data.template_file.testnet_alert_receivers.rendered
    file    = "/config/receivers.yml"
  }

  rm         = true
  depends_on = [docker_container.verify_alert_receivers]
}
