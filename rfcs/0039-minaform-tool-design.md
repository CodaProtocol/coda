## Summary
[summary]: #summary

This rfc proposes a variety of changes to our existing testnet terraform modules and main.tf files to more reliably create and manage both QA nets and production deployments. Our existing setup is very hacky and requires knowledge of many tools to use effectively. The intention with this refactor/these changes is to leverage terraform's `.tfvars` files to configure networks but allow sharing a "qa.tf" file for qa nets and a "prod.tf" file for production networks. This approach, along with other tweaks to the terraform listed below, resolves major pain points with the existing setup, and standardizes a simpler process for folks to learn/understand.

## Motivation
[motivation]: #motivation

The goal is to standardize community-oriented and internal qa networks with a more structured terraform module/network to ensure that the process for creating and deploying a new testnet is code-reviewed, documented, and standard across all deployment scenarios. As of this RFC, additional requirements for community members themselves or external partners to make use of this tooling is out-of-scope but the goal is that this implementation will be easier to maintain and extend as needs arise.

The following issues with the existing system were reported by at least one person as "pain points" that consume time/debugging effort when deploying networks:

1) Production Networks vs. QA Nets
    - The process for deploying production testnets vs. qa nets has diverged in a few places and this means our day-to-day testing does not cover all of the services we need to deploy for public networks.
    - Resolved by creating standard terraform network configurations, so that each network when possible can be defined exclusively in a `.tfvars` file overriding common variables like docker image.
2) Determining network health is difficult/unclear/inconsistent
    - This is addressed with more health checks in k8s and more metrics / better dashboards in grafana, but integrating the deployment tooling with all of those checks can help make it more foolproof.
    - Resolved with Helm NOTES and/or Terraform outputs to provide links for grafana and stackdriver to provide more visibility to the monitoring we have.
3) No cleanup after networks have run their course, especially cleaning up coda-automation.
    - Resolved by more dilligently cleaning up after old networks. May also be worth investigating a lifespan for the network in terraform for automated cluster cleanup.
    - This issue should be less of a problem when networks are simply `.tfvars` files.
4) Generating Genesis Proofs is slow/resource intensive/hard to predict
    - Resolved by `scripts/bake-image.sh` which takes in the daemon image to start with and outputs a "baked" one w/ a genesis ledger, proof, and snark keys. `--cloud` flag uses google cloud build to execute the Dockerfile instead of running locally.
5) Hard to determine the correct docker tag from a given git SHA
    - Can be scripted to allow for a git tag or git SHA input, in addition to / in place of the docker tag.
    - Resolved by `scripts/docker-tag-from-git.sh`, simple script to take in a git commit sha and output the correct docker tag to use. `--wait` flag will wait for the docker image to be pushed.
6) Keys and other private info for deployment are not committed/shared with other team members
    - Resolved by https://github.com/MinaProtocol/mina/issues/6550 , simple feature of coda-network tool to download the genesis ledger and all associated keys from google cloud storage.
7) Incompatible helm/terraform versions
    - Can be addressed by 1) checking versioning before trying to use these tools and 2) bundling a canonical set of tools into a docker image
    - Resolved by building a docker image in the mina repo / coda-automation submodule with the source of coda-automation, helm, kubectl, terraform, etc.
8) Incompatible helm/terraform changes
    - Addressed by moving the charts to the mina repo and explicitly releasing them. Terraform specifies an individual version of each chart to depend on and will fetch the released chart before deploys.
    - Resolved with released helm chart version in mina buildkite pipeline, already working and in use
9) Helm string processing is inconsistent/hard
    - Now all charts are passing `helm lint` in the mina repo, and we can add `helm test` in the future as well. Charts in the mina repo also allow for us to build dhall types and tooling around our charts, but that is out-of-scope for this RFC.
    - Resolved with helm linting and rendering in buildkite to ensure the charts are usable. Should add `helm test` to that pipeline eventually as well.
10) Passing flags to the daemon process directly is difficult to debug/handle/manage
    -  Expose command line flags genericly as a list of command line arguments in helm, allowing deployments to replace the default flags as needed and clearly specify them.
    - Resolved by having the helm accept a list of arguments to the daemon, provide this functionality through `.tfvars`.

## Detailed design
[detailed-design]: #detailed-design

One of the major design goals is to introduce a set of default terraform network configurations for qa and production that will be the starting point for all networks we deploy, and will be maintained with PR's and strict code review, while still allowing any further modification to fit the needs of any individual deployment, testing, or debugging effort. The design revolves around a improvements to the existing coda-network tool and a few new bash scripts to address specific needs expressed by nathan, brandon, and andrew.  The entirety of mina-automation can then be bundled in a container alongside its dependencies for easy cross-platform use as soon as possible.

**Unlike in the original `minaform` design, none of these scripts are used to actually deploy or manage cluster resources. Terraform will be the only tool for starting or stopping networks, coda-network and these helper scripts just help get to that stage faster.**

### New coda-network features

- `keyset deploy` command for uploading keys as secrets to kubernetes to deprecate scripts/testnet-keys.py.
    - If we go this direction permanently, it could be integrated as a local-exec in terraform. See the below keypair download initContainer for another approach.
- Keyset, keypair, and genesis download to allow easy retrieval of the keys required for a given genesis ledger, or to retrieve any specific key from google cloud storage.
    - Tracking issue: https://github.com/MinaProtocol/mina/issues/6550
    - The mina-automation docker image w/ `coda-network keypair download` would allow for an initContainer in kubernetes to grab a key for a specific container, removing the need for `keyset deploy` entirely.
- Using ocaml generate-keypair binary or docker image to generate keys instead of relying on js library.
    - Tracking issue: https://github.com/MinaProtocol/mina/issues/6548
- More complete genesis-create command with support for vesting schedules and a yaml-based config file for defining genesis ledger requirements.
    - Tracking Issue: https://github.com/MinaProtocol/mina/issues/6551
    - Doing this properly is worthwhile, but a combination of bash + jq + the existing `coda-network genesis` command have unblocked deployments and will be used until a complete design + implementation is ready.

### ./scripts/create-network.sh

The first entrypoint to deploying a new network is a new script, `./scripts/create-network.sh <testnet_name>`, which will create a new testnet `.tfvars` file based on the new and improved terraform testnet examples, `qa` and `prod`, and then generate or download a set of keys + ledger to use with this new network. This system of standardizing qa and prod networks, and building all future networks from this shared starting point addresses Pain Point #1.

With the example terraform, `create-network.sh` can now use the `coda-network` tool (aka carey's key service, or the "Testnet SDK") to generate all of the neccessary keypairs for you, as well as the genesis ledger itself. In the initial implementation, this will be handled by the already ready to use but fairly hacky scripts `generate-keys-and-ledger.sh`, but as the coda-network tool becomes more fully featured then the sed+jq steps will be removed and this script will interract directly with coda-network.

The flag `--bake-image=true` will cause the tool to also run `./scripts/bake-image.sh` to build a new docker image with all of the required network configuration and s3-cached files baked-in.

Additional flags can be provided to the create-network command for changing the quantities of fish, whales, seeds, bots, etc. These quantaties are provided at this stage so that the genesis ledger can be populated with proper accounts for these services.

Once the network has been defined and set up, you can always make manual changes to the `.tfvars` to customize the deployment. Additionally, `./scripts/bake-image.sh` can be run at any time to re-package a new daemon image with the required genesis ledger, proof, and configuration.  Any future edits to the keypairs, keysets, or the genesis ledger can be performed directly with coda-network.

### ./scripts/bake-image.sh

The `./scripts/bake-image.sh <testnet_name> --docker-image <image>` script is a simple entrypoint for building a new image based on the provided one, but with the genesis ledger, proofs, and proving keys baked-in to reduce network startup time at the expense of this one-time step (Pain Point #4). If no --docker-image flag is specified, the script will use the docker_image variable in `<testnet_name>.tfvars`. This can be run on any existing network, and will produce a docker image with the tag `gcr.io/o1labs-192920/mina-daemon:<image>-baked-<sha256>` where sha256 is the hash of the genesis_ledger for `<testnet_name>`. The script will also insert the above image tag into the `baked_docker_image` field in `<testnet_name>.tfvars`. This `baked_docker_image` parameterr will allow the terraform to assume the genesis ledger is already included in the image and will not use a runtimeConfig to read the genesis_ledger. Using pre-baked images allows for all deployments to skip the generate-proof stage and avoids duplicate work of downloading and unpacking keys from s3, but this script should still make it easy to iterate on a network deployment when images have bugs and need to be updated.

This command can be outsourced to google cloud with the `--cloud` flag because these files including the image itself never really need to be on the same machine that operates the deployment. To this end, google cloud build (https://cloud.google.com/cloud-build/docs/quickstart-build#build_using_dockerfile) a serverless product that can be sent a dockerfile at any time to have google build it. There is some value in trying to handle other CI builds with google cloud build but its especially perfect for this scenario when the majority of the build time is spent on network transfers to/from google cloud, so that GCB can build much quicker than any other environment (and we pay per build minute over 120/day). For nearly 0 additional development effort we get to bake docker images in google cloud.

### mina-automation Docker image

To address pain points #7 and #8, and to some extent the larger problem of how to make it easy for anyone, from anywhere, to deploy networks, we need a mina-automation docker image that can be bundled alongside all of its dependencies (nodejs for coda-network, some of our deb packages like generate-keypair, terraform, helm, the mina-automation repo). These dependencies can then be handled by updates to the docker image, and anyone having trouble with version mismatches can mount their credentials into a docker container and get moving quickly.

In addition to use on any developer laptop, the docker image could be deployed directly into the cluster, google cloud shell, or a google cloud VM to allow for low-latency deployment to gcloud even from a high-latency network. This will allow us to move in the direction of this container running as a pod in the cluster and all deployments can be conducted from this pod.

## Drawbacks
[drawbacks]: #drawbacks



## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

Why is this design the best in the space of possible designs?

* Terraform is the agreed upon base for all of our deployment operations and management of a cluster, and a lot of time has already been invested in generating terraform from the integration test framework and other tools. This design allows us to lower the minimum terraform customization required to deploy a network, and does not introduce additional tooling beyond terraform, docker, and the coda-network tool. The proposed scripts could even be a readme with individual commands for starting a network as we have in coda-automation/README.md now, but the scripts allow for a more graceful transition behind the scenes as coda-network and the terraform mature with individual patches and improvements.

What other designs have been considered and what is the rationale for not choosing them?

* As I see it, the most ambitious but useful direction to go from here is a golang tool that incorporates native go libraries for terraform, helm, google cloud, prometheus, kubernetes, and docker with go modules to version of all dependencies (except coda-network). Our use of these tools would not change much from calling them on the command line, except that it can easily be type-checked, versioned, compiled, and optimized for our use case. As mentioned this approach is more ambitious, and when terraform itself can solve the majority of our issues the energy should first be spent there.

* An argument can be made as well for adding bake-image.sh and create-network.sh to the coda-network tool, but the reasonml tool would still be shelling out to terraform and docker.  One of my design goals with this plan has been to limit use of @figitaki's time, and if we are just scripting around cli tools anyway the solutions should be equivalent. When the developer cycles are there, we can always port the scripts from bash into coda-network subcommands. In the meantime, time and energy can be focused on improving the terraform as it is now to meet the needs of all networks we deploy.

What is the impact of not doing this?

* Continuing with `auto-deploy.sh` means continued confusion over how to start when creating a new network, how to generate keys and a ledger, and how to customize terraform files. This will remain hard to maintain until we standardize the process and expectations around deploying testnets.

## Prior art
[prior-art]: #prior-art

Many proposals from the Conner era have tackled this design space, and a variety of python scripts used in the existing setup came out of those designs, and to some extent coda-network is a result as well. See https://github.com/MinaProtocol/mina/issues/4723 for an outline of the work achieved and what was left to be accomplished.

To that end, this concept was inspired by https://github.com/MinaProtocol/mina/issues/5768 and intends to achieve some of the listed design goals. The interface defined in this RFC can be extended to be community-friendly (by allowing easy configuration of google cloud and kubernetes credentials) but those features are out-of-scope for the initial implementation. Easier to iterate quickly on is a high priority, as is modularity (by still allowing direct use of terraform and direct interraction with coda-network).

## Unresolved questions
[unresolved-questions]: #unresolved-questions

* What parts of the design do you expect to resolve through the RFC process before this gets merged?
* What parts of the design do you expect to resolve through the implementation of this feature before merge?
* What related issues do you consider out of scope for this RFC that could be addressed in the future independently of the solution that comes out of this RFC?
