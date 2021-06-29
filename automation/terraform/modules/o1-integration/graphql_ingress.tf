resource "kubernetes_ingress" "testnet_graphql_ingress" {
  count = var.deploy_graphql_ingress ? 1 : 0
  depends_on = [
    module.kubernetes_testnet.testnet_namespace,
    module.kubernetes_testnet.seeds_release,
    module.kubernetes_testnet.block_producers_release,
    module.kubernetes_testnet.snark_workers_release
  ]

  metadata {
    name = "${var.testnet_name}-graphql-ingress"
    namespace = var.testnet_name
    annotations = {
      "kubernetes.io/ingress.class" = "nginx"
      "nginx.org/mergeable-ingress-type" = "minion"
      "nginx.ingress.kubernetes.io/rewrite-target" = "/$2"
    }
  }

  spec {
    rule {
      host = "${local.graphql_ingress_dns}"
      http {
        dynamic "path" {
          for_each = concat(
            [local.seed_config.name],
            [for config in var.block_producer_configs : config.name],
            var.snark_worker_replicas > 0 ? [local.snark_coordinator_name] : []
          )

          content {
            backend {
              service_name = "${path.value}-graphql"
              service_port = 80
            }

            path = "/${path.value}(/|$)(.*)"
          }
        }
      }
    }
  }

  # wait_for_load_balancer = true
  wait_for_load_balancer = false
}
