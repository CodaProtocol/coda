output "testnet_namespace" {
  value = kubernetes_namespace.testnet_namespace
}

output "seeds_release" {
  value = helm_release.seeds
}

output "block_producers_release" {
  value = helm_release.block_producers
}

output "snark_workers_release" {
  value = helm_release.snark_workers
}
