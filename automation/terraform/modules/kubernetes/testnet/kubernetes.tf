data "google_client_config" "current" {}

provider "kubernetes" {
  alias          = "testnet_deploy"
  config_context = var.k8s_context
}

resource "kubernetes_namespace" "testnet_namespace" {
  metadata {
    name = var.testnet_name
  }

  timeouts {
    delete = "15m"
  }
}
