# Summary

Currently, the test integration framework has the capabilities to upload and test a network to a cloud environment (specifically GCP) but in addition to this, we want to add functionality to deploy and test a network on a local user machine. The cloud integration uses Terraform and Helm to deploy to a GKE environment to deploy the network and specified nodes. While this method works for cloud environments, we would like a more lightweight solution to run locally. Thus, we have chosen `Docker Swarm` as the tool of choice for container orchestration.

Docker swarm can configure a "swarm" on a local machine to deploy and manage containers on that specific swarm. Docker Swarm takes as input a `docker-compose` file in which all container information is specified and will handle the deployment of all containers on that local swarm. When we want to run a network test locally, we can create a swarm and have all the containers deploy via a `docker-compose.json` file that is built from specified network configurations. Docker swarm also gives us the ability to get logs of all the containers running in an aggregated way, meaning we do not have to query individual containers for their logs. This gives us a way to apply event filters to specific nodes (block producers, snark workers, seed nodes, etc), and check for test success/failure in a portable way.

# Requirements

The new local testing framework should be run on a user's local system using Docker as its main engine to create a network and spawn nodes. This feature will be built on top of the existing test executive which runs our cloud integration tests. By implementing the interface specified in `src/lib/integration_test_lib/intf.ml`, we will have an abstract way to specify different testing engines when running the test executive.

The specific interface to implement would be:

```ocaml
(** The signature of integration test engines. An integration test engine
   *  provides the core functionality for deploying, monitoring, and
   *  interacting with networks.
   *)
  module type S = sig
    (* unique name identifying the engine (used in test executive cli) *)
    val name : string

    module Network_config : Network_config_intf

    module Network : Network_intf

    module Network_manager :
      Network_manager_intf
      with module Network_config := Network_config
       and module Network := Network

    module Log_engine : Log_engine_intf with module Network := Network
  end
end
```

To implement this interface, a new subdirectory will be created in `src/lib` named `integration_local_engine` to hold all implementation details for the local engine.

The new local testing engine must implement all existing features which include:

- Starting/Stopping nodes dynamically
- Sending GraphQL quries to running nodes
- Streaming event logs from nodes for further processing
- Spawning nodes based on a test configuration

Additionally, the test engine should take a Docker image as input in the CLI.

An example command of using the local testing framework could look like this:

```bash
$ test_executive local send-payment --mina-image codaprotocol/coda-daemon-puppeteered:
1.1.5-compatible --debug | tee test.log | logproc -i inline -f '!(.level in \["Spam", "Debug"\])'
```

Note that this is very similar to the current command of calling the cloud testing framework.

# Detailed Design

## Orchestration

To handle container orchestration, we will be utilizing `Docker Swarm` to spawn and manage containers. Docker Swarm lets us create a cluster and run containers on a cluster to manage availability. We have opted for Docker Swarm instead of other orchestration tools like Kubernetes due to Docker being much easier to run on a local machine while still giving us much of the same benefits. Kubernetes is more complex and is somewhat overkill for what we are trying to achieve with the local testing framework. Both Docker Swarm and Kubernetes can handle container orchestration but the complexity of dealing with Kubernetes does not give much payoff. Additionally, if we want community members to also use this tool, setting up Kubernetes on end-user systems would be even more of a hassle.

Docker Swarm takes a `docker-compose` file in which it will generate the desired network state. A cluster can be defined in Docker Swarm by issuing `docker swarm init` which creates the environment in which all containers will be orchestrated on. In the context of our system, we do not need to take advantage of different machines to run these containers on, rather we will run all containers on the local system. Thus, the end result of the swarm will be all containers running locally while Docker Swarm provides availability and other resource management options.

## Creating a docker-compose file for local instead of terraform on cloud

In the current cloud architecture, we launch a given network with `Terraform`. We specify a `Network_config.t` data structure which holds all necessary information about creating the network and then it is transformed into a `Terraform` file like so:

```ocaml
type terraform_config =
    { k8s_context: string
    ; cluster_name: string
    ; cluster_region: string
    ; aws_route53_zone_id: string
    ; testnet_name: string
    ; deploy_graphql_ingress: bool
    ; coda_image: string
    ; coda_agent_image: string
    ; coda_bots_image: string
    ; coda_points_image: string
    ; coda_archive_image: string
          (* this field needs to be sent as a string to terraform, even though it's a json encoded value *)
    ; runtime_config: Yojson.Safe.t
          [@to_yojson fun j -> `String (Yojson.Safe.to_string j)]
    ; block_producer_configs: block_producer_config list
    ; log_precomputed_blocks: bool
    ; archive_node_count: int
    ; mina_archive_schema: string
    ; snark_worker_replicas: int
    ; snark_worker_fee: string
    ; snark_worker_public_key: string }
  [@@deriving to_yojson]

type t =
{ coda_automation_location: string
; debug_arg: bool
; keypairs: Network_keypair.t list
; constants: Test_config.constants
; terraform: terraform_config }
[@@deriving to_yojson]
```

[https://github.com/MinaProtocol/mina/blob/67cc4205cc95138cf729a2f14b57b754f9e9204e/src/lib/integration_test_cloud_engine/coda_automation.ml#L35](https://github.com/MinaProtocol/mina/blob/67cc4205cc95138cf729a2f14b57b754f9e9204e/src/lib/integration_test_cloud_engine/coda_automation.ml#L35)

We launch the network after all configuration has been applied by running `terraform apply`

We can leverage some of this existing work by specifying a config for Docker Swarm instead. Docker Compose can use a `docker-compose` file (which can be specified as a `.json` file [https://docs.docker.com/compose/faq/#can-i-use-json-instead-of-yaml-for-my-compose-file](https://docs.docker.com/compose/faq/#can-i-use-json-instead-of-yaml-for-my-compose-file)) to launch containers on a given swarm environment. The interface can look mostly the same while cutting out a lot of the specific information needed by Terraform.

```ocaml
type docker_compose_config =
    {
    ; coda_image: string
    ; coda_agent_image: string
    ; coda_bots_image: string
    ; coda_points_image: string
    ; coda_archive_image: string
    ; runtime_config: Yojson.Safe.t
          [@to_yojson fun j -> `String (Yojson.Safe.to_string j)]
    ; block_producer_configs: block_producer_config list
    ; log_precomputed_blocks: bool
    ; archive_node_count: int
    ; mina_archive_schema: string
    ; snark_worker_replicas: int
    ; snark_worker_fee: string
    ; snark_worker_public_key: string }
  [@@deriving to_yojson]

type t =
{ coda_automation_location: string
; debug_arg: bool
; keypairs: Network_keypair.t list
; constants: Test_config.constants
; docker_compose: docker_compose_config }
[@@deriving to_yojson]
```

By taking a `Network_config.t` struct, we can transform the data structure into a corresponding `docker-compose` file that specifies all containers to run as well as any other configurations.
After computing the corresponding `docker-compose` file, we can simply call `docker stack deploy -c local-docker-compose.json testnet_name`

<img src="./res/local-test-integration-docker-compose.png" alt="drawing" width="500"/>

The resulting `docker-compose.json` file can have a service for each type of node that we want to spawn. Services in Docker Swarm are similar to pods in Kubernetes as they will schedule containers to nodes to run specified tasks.

A very generic example format of what the `docker-compose.json` could look as follows:

```bash
{
    "version":"3",
    "services":{
        "block-producer":{
            "image":"codaprotocol/coda-daemon-puppeteered",
            "entrypoint":"/mina-entrypoint.sh",
            "networks":[
                "mina_local_test_network"
            ],
            "deploy":{
                "replicas":2,
                "restart_policy":{
                    "condition":"on-failure"
                }
            }
        },
        "seed-node":{
            "image":"codaprotocol/coda-daemon-puppeteered",
            "entrypoint":"/mina-entrypoint.sh",
            "networks":[
                "mina_local_test_network"
            ],
            "deploy":{
                "replicas":1,
                "restart_policy":{
                    "condition":"on-failure"
                }
            }
        },
        "snark-worker":{
            "image":"codaprotocol/coda-daemon-puppeteered",
            "entrypoint":"/mina-entrypoint.sh",
            "networks":[
                "mina_local_test_network"
            ],
            "deploy":{
                "replicas":3,
                "restart_policy":{
                    "condition":"on-failure"
                }
            }
        }
    },
    "networks":{
        "mina_local_test_framework":null
    }
}
```

## Logging

For logging every single event that the network produces, we must be mindful of the volume logs that could potentially come through. Because the integration framework expects log messages for its wait conditions, we can not risk any missed log statements. Relying on the Docker API could prove to be problematic if there are any errors that occur due to any sort of latency. For this reason, we can adopt a pipe pushed-based approach where the test executives create a shared pipe and mounts the file in every container specified in the docker-compose file. This pipe then acts as the communication between all container logs and the test executive.

On startup, the test executive will create a pipe in the current directory and will include that file as a bind mount for each container in the docker-compose file. As a result, each container will be able to redirect all stdout to the specified pipe by using a puppeteer script which is then read by the test executive.

A further optimization we can do is apply `logproc` to all container output before it's written to the pipe. `logproc` can help us filter logs and reduce the load that the test executive has to process.

One important thing to note is that Docker will store all container logs on the user's local system by default. This can be an issue as the logs generated could potentially consume the disk of the user if there is no logging rotation set up. Docker by default sets its logging driver to be [json-file](https://docs.docker.com/config/containers/logging/json-file/) which means all logs gathered by using `docker container logs` are located at a specific path on the user in a json format with no logging rotation. Because all container stdout is being sent to a pipe for the test executive to read, storing the logs of the containers to use via the Docker CLI does not need full persistence. Instead, we can use the [local logging driver](https://docs.docker.com/config/containers/logging/local/) which is optimized for performance and disk use and we can additionally set a cap to the log file size so that Docker will rewrite the used log files instead of consuming all the disk space. We can add a flag as the `local` command for the max-file size a user wants to keep on their system in case they want less/more logs stored by Docker.

The following is a diagram outlining the architecture used for gathering logs:

<img src="./res/local-test-integration-logging.jpg" alt="drawing" width="500"/>

## Interface To Develop

The current logging for the cloud framework is done by creating a Google Stackdriver subscription and issuing poll requests for logs while doing some pre-defined filtering.

An example of this is shown below:

```ocaml
let rec pull_subscription_in_background ~logger ~network ~event_writer
    ~subscription =
  if not (Pipe.is_closed event_writer) then (
    [%log spam] "Pulling StackDriver subscription" ;
    let%bind log_entries =
      Deferred.map (Subscription.pull ~logger subscription) ~f:Or_error.ok_exn
    in
    if List.length log_entries > 0 then
      [%log spam] "Parsing events from $n logs"
        ~metadata:[("n", `Int (List.length log_entries))]
    else [%log spam] "No logs were pulled" ;
    let%bind () =
      Deferred.List.iter ~how:`Sequential log_entries ~f:(fun log_entry ->
          log_entry
          |> parse_event_from_log_entry ~network
          |> Or_error.ok_exn
          |> Pipe.write_without_pushback_if_open event_writer ;
          Deferred.unit )
    in
    let%bind () = after (Time.Span.of_ms 10000.0) in
    pull_subscription_in_background ~logger ~network ~event_writer
      ~subscription )
  else Deferred.unit
```

[https://github.com/MinaProtocol/mina/blob/67cc4205cc95138cf729a2f14b57b754f9e9204e/src/lib/integration_test_cloud_engine/stack_driver_log_engine.ml#L269](https://github.com/MinaProtocol/mina/blob/67cc4205cc95138cf729a2f14b57b754f9e9204e/src/lib/integration_test_cloud_engine/stack_driver_log_engine.ml#L269)

A similar interface can be written for Docker-Swarm instead. We can write an interface to read from the local pipe specified as log entries are sent from containers.

## Cleanup

Cleaning up a Docker Swarm is done by issuing `docker stack rm <stack-name>` and it will handle all teardown of services Docker created to originally spin up.
Additionally, we'll delete the created pipe by the test executive on the users machine.

# Drawbacks

- The test executive is vulnerable to being overloaded if there are too many logs being written in the pipe.

# Work Breakdown/Prio

The following will be a work breakdown of what needs to be done to see this feature to completion:

1. Implement the `Network_Config` interface to accept a network configuration and create a corresponding `docker-compose.json` file.
2. Implement the `Network_manager` interface to take a corresponding `docker-compose.json` file and create a local swarm with the specified container configuration
3. Implement the pipe logging interface that will be used by the test executive to read the forwarded stdout
4. Implement filter on test event functionality
5. Ensure that current integration test specs are able to run on the local framework with success

# Unresolved Questions

- ~~Is compiling a docker-compose file the right approach for scheduling the containers? The nice thing about using a docker-compose file is that all network management should be automatic.~~

- Is using a different service for each type of node the best effective approach? Would it be better to launch all nodes under the same service in the docker-compose file?

- ~~Is polling each service and then aggregating those logs the best approach? Would it be better to do filtering before aggregating?~~

- ~~Does this plan capture the overall direction we want the local testing framework to go?~~
