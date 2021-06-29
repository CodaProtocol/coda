locals {
  east1_region         = "us-east1"
  east1_k8s_context    = "gke_o1labs-192920_us-east1_coda-infra-east"
  bk_east1_k8s_context = "gke_o1labs-192920_us-east1_buildkite-infra-east1"

  east_prometheus_helm_values = {
    server = {
      global = {
        external_labels = {
          origin_prometheus = "east1-prometheus"
        }
      }
      persistentVolume = {
        size = "50Gi"
      }
      remoteWrite = [
        {
          url = jsondecode(data.aws_secretsmanager_secret_version.current_prometheus_remote_write_config.secret_string)["remote_write_uri"]
          basic_auth = {
            username = jsondecode(data.aws_secretsmanager_secret_version.current_prometheus_remote_write_config.secret_string)["remote_write_username"]
            password = jsondecode(data.aws_secretsmanager_secret_version.current_prometheus_remote_write_config.secret_string)["remote_write_password"]
          }
          write_relabel_configs = [
            {
              source_labels : ["__name__"]
              regex : "(buildkite.*|container.*|Coda.*|watchdog.*|go.*|process.*|Mina.*)"
              action : "keep"
            }
          ]
        }
      ]
    }
    # Disable per-region instances due to grafanacloud aggregator setup
    alertmanager = {
      enabled = false
    }
  }
}

provider "google" {
  alias   = "google_east"
  project = local.gcp_project
  region  = local.east1_region
}

provider "kubernetes" {
  alias          = "k8s_east1"
  config_context = local.east1_k8s_context
}

data "google_compute_zones" "east1_available" {
  project = local.gcp_project
  region  = local.east1_region
  status  = "UP"
}

### Testnets

resource "google_container_cluster" "coda_cluster_east" {
  provider           = google.google_east
  name               = "coda-infra-east"
  location           = local.east1_region
  min_master_version = "1.15"

  node_locations = data.google_compute_zones.east1_available.names

  # We can't create a cluster with no node pool defined, but we want to only use
  # separately managed node pools. So we create the smallest possible default
  # node pool and immediately delete it.
  remove_default_node_pool = true
  initial_node_count       = 1


  master_auth {
    username = ""
    password = ""

    client_certificate_config {
      issue_client_certificate = false
    }
  }
}

resource "google_container_node_pool" "east_primary_nodes" {
  provider   = google.google_east
  name       = "coda-infra-east"
  location   = local.east1_region
  cluster    = google_container_cluster.coda_cluster_east.name
  node_count = 4
  autoscaling {
    min_node_count = 0
    max_node_count = 7
  }
  node_config {
    preemptible  = false
    machine_type = local.node_type
    disk_size_gb = 500

    metadata = {
      disable-legacy-endpoints = "true"
    }

    oauth_scopes = [
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring",
    ]
  }
}

resource "google_container_node_pool" "east1_preemptible_nodes" {
  provider   = google.google_east
  name       = "mina-preemptible-east1"
  location   = local.east1_region
  cluster    = google_container_cluster.coda_cluster_east.name
  node_count = 5
  autoscaling {
    min_node_count = 0
    max_node_count = 20
  }
  node_config {
    preemptible  = true
    machine_type = "n1-standard-16"
    disk_size_gb = 500

    metadata = {
      disable-legacy-endpoints = "true"
    }

    oauth_scopes = [
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring",
    ]
  }
}

### Buildkite

resource "google_container_cluster" "buildkite_infra_east1" {
  provider           = google.google_east
  name               = "buildkite-infra-east1"
  location           = local.east1_region
  min_master_version = "1.15"

  node_locations = data.google_compute_zones.east1_available.names

  remove_default_node_pool = true
  initial_node_count       = 1

  master_auth {
    username = ""
    password = ""

    client_certificate_config {
      issue_client_certificate = false
    }
  }
}

resource "google_container_node_pool" "east1_compute_nodes" {
  provider = google.google_east
  name     = "buildkite-east1-compute"
  location = local.east1_region
  cluster  = google_container_cluster.buildkite_infra_east1.name

  # total nodes provisioned = node_count * # of AZs
  node_count = 5
  autoscaling {
    min_node_count = 2
    max_node_count = 10
  }
  node_config {
    preemptible  = true
    machine_type = "c2-standard-16"
    disk_size_gb = 500

    metadata = {
      disable-legacy-endpoints = "true"
    }

    oauth_scopes = [
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring",
      "https://www.googleapis.com/auth/cloud-platform"
    ]
  }
}

## Data Persistence

# TODO: deprecate below region based storage classes once OK to do so (i.e. all testnets have migrated to new classes)
resource "kubernetes_storage_class" "east1_ssd" {
  provider = kubernetes.k8s_east1

  count = length(local.storage_reclaim_policies)

  metadata {
    name = "${local.east1_region}-ssd-${lower(local.storage_reclaim_policies[count.index])}"
  }
  storage_provisioner = "kubernetes.io/gce-pd"
  reclaim_policy      = local.storage_reclaim_policies[count.index]
  volume_binding_mode = "WaitForFirstConsumer"
  parameters = {
    type = "pd-ssd"
  }
}

resource "kubernetes_storage_class" "east1_standard" {
  provider = kubernetes.k8s_east1

  count = length(local.storage_reclaim_policies)

  metadata {
    name = "${local.east1_region}-standard-${lower(local.storage_reclaim_policies[count.index])}"
  }

  storage_provisioner = "kubernetes.io/gce-pd"
  reclaim_policy      = local.storage_reclaim_policies[count.index]
  volume_binding_mode = "WaitForFirstConsumer"
  parameters = {
    type = "pd-standard"
  }
}

# ---

resource "kubernetes_storage_class" "east1_infra_ssd" {
  provider = kubernetes.k8s_east1

  count = length(local.storage_reclaim_policies)

  metadata {
    name = "ssd-${lower(local.storage_reclaim_policies[count.index])}"
  }

  storage_provisioner = "kubernetes.io/gce-pd"
  reclaim_policy      = local.storage_reclaim_policies[count.index]
  volume_binding_mode = "WaitForFirstConsumer"
  parameters = {
    type = "pd-ssd"
  }
}

resource "kubernetes_storage_class" "east1_infra_standard" {
  provider = kubernetes.k8s_east1

  count = length(local.storage_reclaim_policies)

  metadata {
    name = "standard-${lower(local.storage_reclaim_policies[count.index])}"
  }

  storage_provisioner = "kubernetes.io/gce-pd"
  reclaim_policy      = local.storage_reclaim_policies[count.index]
  volume_binding_mode = "WaitForFirstConsumer"
  parameters = {
    type = "pd-standard"
  }
}

## Monitoring 

provider helm {
  alias = "helm_east"
  kubernetes {
    config_context = local.east1_k8s_context
  }
}

provider helm {
  alias = "bk_helm_east"
  kubernetes {
    config_context = local.bk_east1_k8s_context
  }
}

resource "helm_release" "east1_prometheus" {
  provider = helm.helm_east

  name      = "east-prometheus"
  chart     = "stable/prometheus"
  namespace = "default"
  values = [
    yamlencode(local.east_prometheus_helm_values)
  ]
  wait         = true
  depends_on   = [google_container_cluster.coda_cluster_east]
  force_update = true
}

resource "helm_release" "bk_east1_prometheus" {
  provider = helm.bk_helm_east

  name      = "bk-east-prometheus"
  chart     = "stable/prometheus"
  namespace = "default"
  values = [
    yamlencode(local.east_prometheus_helm_values)
  ]
  wait         = true
  depends_on   = [google_container_cluster.coda_cluster_east]
  force_update = true
}
