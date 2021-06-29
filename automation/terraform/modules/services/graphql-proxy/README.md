## Mina Faucet Module

This is a Terraform module that will deploy a service containing two tasks, a Mina Daemon task and a GraphQL Proxy container.

## Variables

### Global Variables

`ecs_cluster_id`: The ECS cluster ID

`environment`: The environment the service is running in

`testnet`: The testnet that this daemon is connected to

### GraphQL Proxy Variables

`proxy_container_version`: The version of the container to be used when deploying the Daemon Service

`mina_graphql_host` (Default: "localhost"): The hostname of the Mina GraphQL Endpoint

`mina_graphql_port` (Default: "3085"): The port the Mina GraphQL Endpoint is listening on

### Daemon Variables

`mina_container_version`: The version of the container to be used when deploying the Faucet Service

`mina_wallet_keys`: A space-delimited list of AWS Secrets Manager secret IDs

`aws_access_key`: An Access Key granting read-only access to Testnet Secrets

`aws_secret_key`: The corresponding AWS Secret Key

`aws_default_region`: The region that the secrets are stored in

`mina_peer`: The initial peer to start the Daemon with

`mina_rest_port` (Default: 3085): The port that the GraphQL server will listen on

`mina_external_port` (Default: 10101): The port that the daemon will listen for RPC connections

`mina_metrics_port` (Default: 10000): The port that the daemon will expose prometheus metrics on

`mina_privkey_pass`: The password for the installed keys

## Deployment Considerations

In order to deploy a "new" version of this module, you must ensure that you have rebuilt the Mina Daemon image and *(optionally)* the Proxy image if it has changed.

The Mina Daemon image build is a two-step process, with the base Mina dockerfile being [here](https://github.com/MinaProtocol/mina/blob/develop/dockerfiles/Dockerfile-mina-daemon) and the more deployment-specific Dockerfile [here](https://github.com/MinaProtocol/mina/automation/blob/master/services/daemon/Dockerfile).

The manual commands to release each container are the following:

### Mina-Daemon Container

*(From the root of the `MinaProtocol/mina` repository)*

`./scripts/release-docker.sh -s mina-daemon -v <major>.<minor>.<patch> --extra-args "--build-arg deb_version=<MINA_VERSION>"`

### daemon Container

*(From the root of the `MinaProtocol/mina/automation` repository)*

`./scripts/release-docker.sh -s daemon -v <major>.<minor>.<patch> --extra-args "--build-arg base_image_tag=<docker tag created in first step>"`

The `--extra-args` argument is for passing additional parameters directly to the `docker build` command. It is used here to pass the required Dockerfile variable `base_image_tag` but can also be used to override Dockerfile variables with default values like so `--build-arg deb_repo=release`

The Faucet Dockerfile lives in the `MinaProtocol/mina` repository [here](https://github.com/MinaProtocol/mina/blob/develop/frontend/bot/Dockerfile) and you can release it with the following:

### Faucet Container

*(From the root of the `MinaProtocol/mina` repository)*

`./scripts/release-docker.sh -s graphql-public-proxy -v <major>.<minor>.<patch>`
