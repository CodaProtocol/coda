# O(1) Labs Next-Gen Deployments

## Definitions

**Coda Daemon** - Coda binary connected to the network 

**Block Producer** - Coda Daemon that is staking and producing blocks

**Regions** - Geographic areas, roughly analogous to AWS Regions

**Seed Node** - Coda Daemon deployed to facilitate peer discovery

**SNARK Coordinator** - Coda Daemon that allocates work to SNARK Workers

**SNARK Job** - One SNARK proof that has yet to be completed

**SNARK Work** - One SNARK proof that has been completed and may be included in a *block*

**SNARK Worker** - Coda Daemon completing SNARK jobs and producing SNARK work 

**Testnet** - A **test** **net**work of Coda Daemons

## Summary
In short, this system will be embodied by a series of container clusters running in various *regions*. 

Builds will be managed by a Container Build pipeline, releasing artifacts to one or more Docker container registries. 

To deploy a testnet, container *Tasks* representing various testnet components can be scheduled to the clusters. 

To facilitate testing, service *Tasks* may be scheduled alongside daemon *Tasks* that will manipulate the Daemon's GraphQL Endpoint and trigger/verify the desired behavior. 

The bulk of the monitoring workload will be handled by Prometheus which will scrape the metrics endpoints of the individual components. 

Logs will be redirected from the container's stdout/stderr and retained in an external log management system. 

Alerting will be handled by Grafana, watching prometheus metrics. 

## Motivation
O(1) Labs requires a deployment system that drastically reduces the human burden of deploying and running software infrastructure. It needs to free up the time of the Operations Engineers and empower O(1) Developers to manage and deploy their own *Testnets* and *Services*. 

## Why Containers
Google has set the bar when it comes to running containers at scale. As such, I feel it apropriate to quote them whenever possible for the purposes of this discussion.

The first, obvious question is, "What is a container?" 
> Containers offer a logical packaging mechanism in which applications can be abstracted from the environment in which they actually run. This decoupling allows container-based applications to be deployed easily and consistently, regardless of whether the target environment is a private data center, the public cloud, or even a developer’s personal laptop. Containerization provides a clean separation of concerns, as developers focus on their application logic and dependencies, while IT operations teams can focus on deployment and management without bothering with application details such as specific software versions and configurations specific to the app.
</br>
\- [Containers at Google](https://cloud.google.com/containers/)

"Why Containers?"
> Instead of virtualizing the hardware stack as with the virtual machines approach, containers virtualize at the operating system level, with multiple containers running atop the OS kernel directly. This means that containers are far more lightweight: they share the OS kernel, start much faster, and use a fraction of the memory compared to booting an entire OS.
</br>
\- [Containers at Google](https://cloud.google.com/containers/)

The three high-level reasons for why you'd want to use containers are as follows: 
- Consistent Environment
- Run Anywhere
- Isolation
  
For more details see the Containers @ Google article here: https://cloud.google.com/containers/


## Detailed Design

## Business Requirements

### Artifact Builds
- The system must support automated builds of each commit for designated branches to allow for detection of bugs introduced between commits.
- The system must store versioned build artifacts relevant to deployments so that they may be quickly retrieved at deploy time.

### QA Testnet 
- The system must support the deployment of testnets so that new features may be tested in an isolated environment.
- The system must support building *testnets* with no intervention from operations engineers so that there is as little friction as possible in deploying a QA network. (Self-Service)
- The system must support deployment of test services alongside a QA network so that protocol-level actions may be tested in-detail.

### Production Testnet
- The system must support deployment of *testnets* which allow peers external to the O(1) Labs to connect such that public *testnets* are possible. 

### Fault Tolerance
- The system must be able to deploy nodes in an arbitrary number of regions (at least 3) so that an outage in a single region will not affect the viability of a running network. 
- The system must support non-HA deployments so that networks can be deployed that do not require a high degree of fault-tolerance.

### Snark Work Pools
- The system must allow for the deployment of pools of *Snark Workers* connected to a single *Snark Coordinator* 
- The system must allow for *Snark Pools* to have configurable work assignment algorithms so that multiple kinds of *SNARK pools* may be deployed simultaneously.

### Network Latency
- The system must deploy nodes geographically far enough apart so that sufficient latency is incurred in normal network communications. (Realistic Network Topography)

### Observability
- The system must support the collection of application metrics from running daemons and other system components so that application-level issues may be detected and addressed in short order.
- The system must support the collection, retention, and querying of logs from running daemons so that engineers debugging issues may easily and quickly access relevant information. 

## Technical Requirements 
- Each daemon on the network must be publicly-addressable
- Each daemon requires two open communication ports (TCP and UDP)
- Credentials (private keys) must be downloaded from a secret manager at runtime
  - Initially, it is important that no two daemons are producing blocks with the same private keys due to a bug in the daemon: https://github.com/CodaProtocol/coda/issues/756

## Implementation Overview

### Infrastructure

#### Cloud Provider:

There are many reasons to choose one cloud provider over another, and it usually boils down to features, cost, and availability. 

***PROPOSED SOLUTION:***

*Amazon Web Services*

This is a simple decision, as O(1) Labs has service credit and experience with AWS.

*Google Cloud*

Once O(1) Labs has Kubernetes infrastructure on AWS, it becomes trivial to deploy that on Google Cloud, a tandem cloud approach will also effectively mitigate many kinds of potential outages that could arise in either cloud. 

#### Container Orchestration: 

"Container Orchestration" is described as a tool that automates the deployment, management, scaling, networking, and availability of container-based applications. The container landscape is vast, with many competing alternatives, the top three being Kubernetes, Apache Mesos, and AWS Elastic Container Service. 

***PROPOSED SOLUTION:***

[*Kubernetes*](https://kubernetes.io/)

Kubernetes (aka K8s) is the de-facto global standard for running containerized applications (at scale or otherwise). It has wide adoption, and the software industry is flush with engineers who have experience with kubernetes. 
The main arguments for selecting Kubernetes are as follows:  
- K8s has asserted its dominance as an industry standard Container Orchestration Platform
- Supported Natively on GCE and as simple add-on features to Azure and AWS
- [Google Kubernetes Engine](https://cloud.google.com/kubernetes-engine/) and [AWS EKS](https://aws.amazon.com/eks/) makes launching clusters incredibly easy

*Drawbacks:*
- Kubernetes is known to be complicated and opinionated which could be percieved as a pro or a con depending on how you examine it. 
- The networking model (necessarily) has high complexity, with several proxy layers routing traffic to containers. 

*ALTERNATIVES:*

[*Elastic Container Service*](https://aws.amazon.com/ecs/)

The container orchestration infrastructure will be implemented with AWS Elastic Container Service for the following reasons: 
- AWS's Global Footprint (ECS is avalible Globally)
- ECS's automated nature, reducing the overhead of launching dynamic container clusters
- O(1)'s Pre-Paid AWS Credit 
- Containers can be reused in other Orchestration Systems

*Drawbacks:*
- JSON-Based Service configuration is not reusable in other cloud platforms
- AWS has made some confounding architecture decisions when intergating ECS with other AWS offerings

*EC2 Instances*

The current model for deploying testnets involves installing Coda nodes on EC2 instances. The creation of a testnet can be fully automated, but custom scripts or  applications must be introduced to to monitor and keep the a *daemon* alive.

**Service Discovery**:

***PROPOSED SOLUTION:***

*DNS*

The Coda network handles peer discovery given an initial Seed peer. When nodes that must be exposed to the world via a name are deployed, a corresponding record in our DNS Server must be created. 

Internally to the O(1) Network, Seed Nodes (and other deployed services) will have internal DNS hostnames that will allow for identification of and communication with essential services. 

*ALTERNATIVES*

*Service Discovery Tool (etcd, Consul, etc.)*

As the system O(1) Labs operates becomes more complex, it will probably make sense to swap out the simple DNS discovery mechanism with a more robust distributed key-value store like etcd or Consul. These tools allow a service to register itself with the system and allows other services to query and retrieve the name/location of newly deployed services.

### Build

**Container Build Pipeline**:

***PROPOSED SOLUTION:***

*DockerHub Builds*
This is a robust (and free) solution that doesn't allow for much complexity when it comes to the pipelines you can run. However, it does one thing pretty well, and that's build Docker Images.  If all we need is to build a Docker Image based on a tag from a git repo, DockerHub fits the bill. Additionally, if we want a more advanced build pipeline, we can trigger it remotely after a series of constraints is met in CI. 

*ALTERNATIVES*

*CircleCI*
If more advanced functionality is required, we can build and release docker images via CircleCI without too much effort. 

(TODO BELOW THIS LINE)

## Drawbacks 

## Rationale and alternatives

## Prior Art

## Unresolved Questions