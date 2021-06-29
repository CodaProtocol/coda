## mina Daemon Module

This is a Terraform module that will deploy a mina Daemon container as a service in AWS ECS. 

## Variables 

`cluster_id`: The ECS cluster ID

`environment`: The environment the service is running in

`testnet`: The testnet that this daemon is connected to

`daemon_number`: A unique value that is not shared with another deployed daemon in this environment

`container_version`: The version of the container to be used when deploying the Daemon Service

`mina_wallet_keys`: A space-delimited list of AWS Secrets Manager secret IDs

`aws_access_key`: An Access Key granting read-only access to Testnet Secrets

`aws_secret_key`: The corresponding AWS Secret Key

`aws_default_region`: The region that the secrets are stored in

`daemon_peer`: The initial peer to start the Daemon with

`daemon_rest_port` (Default: 3085): The port that the GraphQL server will listen on

`daemon_external_port` (Default: 10101): The port that the daemon will listen for RPC connections

`daemon_metrics_port` (Default: 10000): The port that the daemon will expose prometheus metrics on

`mina_privkey_pass`: The password for the installed keys

## Deployment Considerations

In order to deploy a "new" version of this module, you must ensure that you have rebuilt said container. The container build is a two-step process, with the base mina dockerfile being [here](https://github.com/MinaProtocol/mina/blob/develop/dockerfiles/Dockerfile-mina-daemon) and the more deployment-specific Dockerfile [here](https://github.com/MinaProtocol/mina-automation/blob/master/services/daemon/Dockerfile).

The manual commands to release each container are the following: 

### Mina-Daemon Container

*(From the root of the `MinaProtocol/mina` repository)*
`./scripts/release-docker.sh -s mina-daemon -v <major>.<minor>.<patch> --extra-args "--build-arg deb_version=<DEB_VERSION> --build-arg deb_release=<mina package release channel, e.g. alpha>"`

Example:
`./scripts/release-docker.sh -s mina-daemon -v 0.0.10-beta4 --extra-args "--build-arg deb_version=0.0.10-beta4-fff3b856 --build-arg deb_release=alpha`

The `--extra-args` argument is for passing additional parameters directly to the `docker build` command. It is used here to pass the required Dockerfile variable `'deb_version` but can also be used to override Dockerfile variables with default values like so `--build-arg deb_repo=release`

### daemon Container

*(From the root of the `MinaProtocol/mina/automation` folder in the mina repository)*
`./scripts/release-docker.sh -s daemon -v <major>.<minor>.<patch> --extra-args "--build-arg base_image_tag=<docker tag created in first step> "`

Example:
`./scripts/release-docker.sh -s daemon -v 0.0.10-beta4 --extra-args "--build-arg base_image_tag=0.0.10-beta4"`
