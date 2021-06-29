provider kubernetes {
  alias = "bk_deploy"
  config_path = "~/.kube/config"
  config_context  = var.k8s_context
}

provider helm {
  alias = "bk_deploy"
  kubernetes {
    config_path = "~/.kube/config"
    config_context  = var.k8s_context
  }
}

# Helm Buildkite Agent Spec
locals {
  buildkite_config_envs = [
    # Buildkite EnvVars
    {
      "name" = "BUILDKITE_GS_APPLICATION_CREDENTIALS_JSON"
      "value" = var.enable_gcs_access ? base64decode(google_service_account_key.buildkite_svc_key[0].private_key) : var.google_app_credentials
    },
    {
      "name" = "GCLOUD_API_KEY"
      "value" = data.aws_secretsmanager_secret_version.testnet_logengine_apikey.secret_string
    },
    {
      "name" = "BUILDKITE_ARTIFACT_UPLOAD_DESTINATION"
      "value" = var.artifact_upload_path
    },
    {
      "name" = "BUILDKITE_API_TOKEN"
      "value" = data.aws_secretsmanager_secret_version.buildkite_api_token.secret_string
    },
    # Summon EnvVars
    {
      "name" = "SUMMON_DOWNLOAD_URL"
      "value" = var.summon_download_url
    },
    {
      "name" = "SECRETSMANAGER_DOWNLOAD_URL"
      "value" = var.secretsmanager_download_url
    },
    # Google Cloud EnvVars
    {
      # used by GSUTIL tool for accessing GCS data
      "name" = "CLUSTER_SERVICE_EMAIL"
      "value" = var.enable_gcs_access? google_service_account.gcp_buildkite_account[0].email : ""
    },
    {
      "name" = "GCLOUDSDK_DOWNLOAD_URL"
      "value" = var.gcloudsdk_download_url
    },
    {
      "name" = "UPLOAD_BIN"
      "value" = var.artifact_upload_bin
    },
    {
      "name" = "CODA_HELM_REPO"
      "value" = var.coda_helm_repo
    },
    {
      "name" = "KUBE_CONFIG_PATH"
      "value" = "/root/.kube/config"
    },
    # AWS EnvVars
    {
      "name" = "AWS_ACCESS_KEY_ID"
      "value" = aws_iam_access_key.buildkite_aws_key.id
    },
    {
      "name" = "AWS_SECRET_ACCESS_KEY"
      "value" = aws_iam_access_key.buildkite_aws_key.secret
    },
    {
      "name" = "AWS_REGION"
      "value" = "us-west-2"
    },
    # Docker EnvVars
    {
      "name" = "DOCKER_PASSWORD"
      "value" = data.aws_secretsmanager_secret_version.buildkite_docker_token.secret_string
    },
    # NPM EnvVars
    {
      "name" = "NPM_TOKEN"
      "value" = data.aws_secretsmanager_secret_version.npm_token.secret_string
    }
  ]
}

locals {
  default_agent_vars = {
    image = {
      tag        = var.agent_version
      pullPolicy = var.image_pullPolicy
    }

    privateSshKey = var.agent_vcs_privkey

    # Using Buildkite's config-setting <=> env-var mapping, convert all k,v's stored within agent config as extra environment variables
    # in order to specify custom configuration (see: https://buildkite.com/docs/agent/v3/configuration#configuration-settings)
    extraEnv = concat(local.buildkite_config_envs,
    [for key, value in var.agent_config : { "name" : "BUILDKITE_$(upper(key))", "value" : value }])

    dind = {
      enabled = var.dind_enabled
    }

    podAnnotations = {
      "prometheus.io/scrape" = "true"
      "prometheus.io/path" = "/metrics"
    }

    rbac = {
      create = true
      role = {
        rules = [
          {
            apiGroups = [
              "",
              "apps",
              "batch"
            ],
            resources = [
              "*"
            ],
            verbs = [
              "get",
              "list",
              "watch"
            ]
          }
        ]
      }
    }

    entrypointd = {
      "00-artifact-cache-helper" = <<-EOF
        #!/bin/bash

        set -o pipefail

        if [[ $1 ]]; then
          export BUILDKITE_ARTIFACT_UPLOAD_DESTINATION="gs://buildkite_k8s/coda/shared/$${BUILDKITE_JOB_ID}"
          FILE="$1"
          DOWNLOAD_CMD="buildkite-agent artifact download --build $${BUILDKITE_BUILD_ID} --include-retried-jobs"

          while [[ "$#" -gt 0 ]]; do case $1 in
            --upload) UPLOAD="true"; shift;;
            --miss-cmd) MISS_CMD="$${2}"; shift;;
          esac; shift; done

          # upload artifact if explicitly set and exit
          if [[ $UPLOAD ]]; then
            echo "--- Uploading artifact: $${FILE}"
            pushd $(dirname $FILE)
            buildkite-agent artifact upload "$(basename $FILE)"; popd
            exit
          fi

          set +e
          if [[ -f "$${FILE}" ]] || $${DOWNLOAD_CMD} "$${FILE}" .; then
            set -e
            echo "*** Cache Hit -- skipping step ***"
          elif [[ $${MISS_CMD} ]]; then
            set -e
            echo "*** Cache miss -- executing step ***"
            bash -c "$${MISS_CMD}"

            echo "--- Uploading artifact: $${FILE}"
            pushd $(dirname $FILE)
            buildkite-agent artifact upload "$${FILE}"; popd
          else
            echo "*** Cache miss -- failing since a miss command was NOT provided ***"
            exit 1
          fi
        else
          echo "*** Artifact not provided - skipping ***"
        fi
      EOF

      "01-install-gcloudsdk" = <<-EOF
        #!/bin/bash

        set -euo pipefail
        set +x

        if [[ ! -f $${UPLOAD_BIN} ]]; then
          echo "Downloading gcloud sdk because it doesn't exist"
          apt-get -y update && apt install -y wget python && wget $${GCLOUDSDK_DOWNLOAD_URL}

          tar -zxf $(basename $${GCLOUDSDK_DOWNLOAD_URL}) -C /usr/local/

          # create local user bin symlinks for easier PATH access
          ln --symbolic --force /usr/local/google-cloud-sdk/bin/gsutil /usr/local/bin/gsutil
          ln --symbolic --force /usr/local/google-cloud-sdk/bin/gcloud /usr/local/bin/gcloud
          ln --symbolic --force /usr/local/google-cloud-sdk/bin/docker-credential-gcloud /usr/local/bin/docker-credential-gcloud

          echo "$${BUILDKITE_GS_APPLICATION_CREDENTIALS_JSON}" > /tmp/gcp_creds.json

          export GOOGLE_APPLICATION_CREDENTIALS=/tmp/gcp_creds.json && /usr/local/google-cloud-sdk/bin/gcloud auth activate-service-account $${CLUSTER_SERVICE_EMAIL} --key-file /tmp/gcp_creds.json

          # enable GCR write access
          gcloud components install --quiet docker-credential-gcr
          gcloud auth configure-docker --quiet gcr.io
        fi
      EOF

      "01-install-summon" = <<-EOF
        #!/bin/bash

        set -euo pipefail
        set +x

        export SUMMON_BIN=/usr/local/bin/summon
        export SECRETSMANAGER_LIB=/usr/local/lib/summon/summon-aws-secrets

        # download and install summon binary executable
        if [[ ! -f $${SUMMON_BIN} ]]; then
          echo "Downloading summon because it doesn't exist"
          apt-get -y update && apt install -y wget && wget $${SUMMON_DOWNLOAD_URL}

          tar -xzf $(basename $${SUMMON_DOWNLOAD_URL}) -C /usr/local/bin/
        fi

        # download and install summon AWS Secrets provider
        if [[ ! -f $${SECRETSMANAGER_LIB} ]]; then
          echo "Downloading summon AWS secrets manager because it doesn't exist"
          wget $${SECRETSMANAGER_DOWNLOAD_URL}

          mkdir -p $(dirname $${SECRETSMANAGER_LIB})
          tar -xzf $(basename $${SECRETSMANAGER_DOWNLOAD_URL}) -C $(dirname $${SECRETSMANAGER_LIB})
        fi
      EOF

      "02-install-k8s-tools" = <<-EOF
        #!/bin/bash

        set -euo pipefail
        set +x

        export CI_SHARED_BIN="/var/buildkite/shared/bin"
        mkdir -p "$${CI_SHARED_BIN}"

        # Install kubectl
        apt-get update --yes && apt-get install --yes lsb-core apt-transport-https curl jq

        export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)"
        echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
        curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
        apt-get update -y && apt-get install kubectl -y
        cp --update --verbose $(which kubectl) "$${CI_SHARED_BIN}/kubectl"

        # Install helm
        curl https://baltocdn.com/helm/signing.asc | apt-key add -
        echo "deb https://baltocdn.com/helm/stable/debian/ all main" | tee /etc/apt/sources.list.d/helm-stable-debian.list
        apt-get update -y && apt-get install helm -y --allow-unauthenticated
        cp --update --verbose $(which helm) "$${CI_SHARED_BIN}/helm"
      EOF

      "02-install-terraform" = <<-EOF
        #!/bin/bash

        set -euo pipefail

        apt install -y unzip
        curl -sL https://releases.hashicorp.com/terraform/0.14.7/terraform_0.14.7_linux_amd64.zip -o terraform.zip
        unzip terraform.zip && mv terraform /usr/bin

        # Install custom versions of terraform in Buildkite shared DIR
        curl -sL https://releases.hashicorp.com/terraform/0.12.29/terraform_0.12.29_linux_amd64.zip -o terraform-0_12_29.zip
        mkdir -p /var/buildkite/shared/terraform/0.12.29
        unzip terraform-0_12_29.zip && mv terraform /var/buildkite/shared/terraform/0.12.29/terraform
      EOF

      "02-install-coda-network-tools" = <<-EOF
        #!/bin/bash

        set -euo pipefail

        # Download and install NodeJS
        curl -sL https://deb.nodesource.com/setup_12.x | bash -
        apt-get install -y nodejs libjemalloc-dev

        # Build coda-network library
        mkdir -p /tmp/mina && git clone https://github.com/MinaProtocol/mina.git /tmp/mina
        cd /tmp/mina/automation && npm install -g && npm install -g yarn
        yarn install && yarn build
        chmod +x bin/coda-network && ln --symbolic --force bin/coda-network /usr/local/bin/coda-network
      EOF

      "02-install-cortextool" = <<-EOF
        #!/bin/bash

        set -euo pipefail

        curl -fSL -o /usr/local/bin/cortextool "https://github.com/grafana/cortex-tools/releases/download/v0.7.2/cortextool_0.7.2_linux_x86_64"
        chmod a+x /usr/local/bin/cortextool
      EOF

      "03-setup-k8s-ctx" = <<-EOF
        #!/bin/bash

        set -euo pipefail

        # k8s_ctx = <gcloud_project>_<cluster-region>_<cluster-name>
        # k8s context mappings: <cluster-name> => <cluster-region>
        declare -A k8s_ctx_mappings=(
          ["coda-infra-east"]="us-east1"
          ["coda-infra-east4"]="us-east4"
          ["coda-infra-central1"]="us-central1"
          ["mina-integration-west1"]="us-west1"
        )
        for cluster in "$${!k8s_ctx_mappings[@]}"; do
            gcloud container clusters get-credentials "$${cluster}" --region "$${k8s_ctx_mappings[$cluster]}"
        done

        # Copy kube config to shared Docker path
        export CI_SHARED_CONFIG="/var/buildkite/shared/config"
        mkdir -p "$${CI_SHARED_CONFIG}"
        cp "$${KUBE_CONFIG_PATH:-/root/.kube/config}" "$${CI_SHARED_CONFIG}/.kube" && chmod ugo+rw "$${CI_SHARED_CONFIG}/.kube"

        # set agent default Kubernetes context for deployment
        kubectl config use-context ${var.testnet_k8s_ctx}
      EOF

      "03-setup-utiltiies" = <<-EOF
        #!/bin/bash

        set -euo pipefail

        # Ensure artifact cache helper tool is in PATH
        ln --symbolic --force /docker-entrypoint.d/00-artifact-cache-helper /usr/local/bin/artifact-cache-helper.sh

        # Install mina debian package tools
        echo "deb [trusted=yes] http://packages.o1test.net unstable main" > /etc/apt/sources.list.d/o1.list
        apt-get update && apt-get install -y "mina-testnet-postake-medium-curves"
      EOF
    }
  }
}

resource "kubernetes_namespace" "cluster_namespace" {
  provider = kubernetes.bk_deploy

  metadata {
    name = var.cluster_name
  }
}

resource "helm_release" "buildkite_agents" {
  for_each   = var.agent_topology

  provider   = helm.bk_deploy
 
  name              = "${var.cluster_name}-buildkite-${each.key}"
  repository        = "buildkite"
  chart             = var.helm_chart
  namespace         = var.cluster_namespace
  create_namespace  = true
  version           = var.chart_version

  values = [
    yamlencode(merge(local.default_agent_vars, each.value))
  ]

  wait = false
}
