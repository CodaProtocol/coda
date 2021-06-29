resource "kubernetes_secret" "mina_account_keypairs" {
  depends_on = [module.kubernetes_testnet.testnet_namespace]
  for_each = {for config in var.block_producer_configs : config.name => config}

  metadata {
    name      = each.value.keypair_secret
    namespace = var.testnet_name
  }

  data = {
    pub = each.value.public_key
    key = each.value.private_key
  }
}

resource "kubernetes_secret" "libp2p_discovery_keys" {
  depends_on = [module.kubernetes_testnet.testnet_namespace]

  metadata {
    name      = local.seed_peer.secret
    namespace = var.testnet_name
  }

  data = {
    key = local.seed_peer.private_key_secretbox,
    pub = local.seed_peer.peerid
  }
}
