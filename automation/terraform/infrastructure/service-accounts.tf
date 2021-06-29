locals {
  gke_project = "o1labs-192920"

  janitor_roles = [
    "roles/container.developer",
    "roles/container.viewer",
    "roles/compute.viewer",
    "roles/container.serviceAgent"
  ]
}

resource "google_service_account" "gcp_janitor_account" {
  account_id   = "gcp-janitor-svc"
  display_name = "GCP Janitor Service"
  description  = "GCP janitor service account for managing resource authorization"
  project      = local.gke_project
}

resource "google_project_iam_member" "janitor_iam_memberships" {
  count = length(local.janitor_roles)

  project = local.gke_project
  role    = local.janitor_roles[count.index]
  member  = "serviceAccount:${google_service_account.gcp_janitor_account.email}"
}

resource "google_service_account_key" "janitor_svc_key" {
  service_account_id = google_service_account.gcp_janitor_account.name
}
