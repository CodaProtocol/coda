## Summary

[summary]: #summary

This RFC proposes a dynamic storage solution capable of being leveraged by Mina blockchain components to persist both application and chain state across redeployments, infrastructure and application level network updates, container/pod failures and other such destructive events and operations. The proposal entails integrating a native Kubernetes mechanism for persisting all testnet state as necessary into Mina's existing infrastructure setup built on top of Google Cloud Platform and its hosted Kubernetes service offering, GKE.

## Motivation

[motivation]: #motivation

Mina blockchain currently consists of several components or agent types each with a particular role or function; and all of which depend on either pre-compiled or runtime state in the form of genesis ledgers, wallet keys, static and runtime configurations and intermediary chain states (for syncing with and participating in block production protocols) for example. The infrastructure, however, on which these components are deployed, at least in its existing form, does not support stateful applications in the traditional sense in that state is not persisted across restarts, redeployments or generally speaking destructive operations of any kind on any of the component processes. While by design and implementation through the use of *k8s* `emptyDir` volumes, historically these two points have resulted in considerable inefficiencies if not blockers in application/code iteration while developers test feature and protocol/product changes, infrastructure adjusts resource and cloud environment settings and/or especially when community dynamics warrant operational tweaks during public testnet releases.

At a high level, the desired outcome of implementing such a solution should result in:
* a simple and automated process for both affiliating & persisting testnet component resources and artifacts to individual testnet deployments (manual as well automated) 
* a flexible and persistent application storage framework to be leveraged by infrastructure across multiple cloud providers 
* a layer of abstraction over Helm/K8s storage and persistent-volume primitives to be utilized by developers for integration testing, experimentation and of course persisting and sharing network state

## Detailed design

[detailed-design]: #detailed-design

Storage or state persistence for Kubernetes applications is largley a solved problem and can generally be thought as consisting of two cloud-provider agnostic, orthogonal though more often than not tightly interconnected concepts: 1. the physical storage layer at which application data is remotely persisted, and 2. the orchestration layer which regulates application "claims" to this underlying storage provider. The former, more commonly referred to as `Storage Classes` with respect to Kubernetes [primitives](https://kubernetes.io/docs/concepts/storage/storage-classes/), represents an abstraction over the type or "classes" of storage offered by a particular administrator to describe different characteristics of the options provided. These characteristics generally relate to storage I/O performance, quality-of-service levels, backup policies, or perhaps arbitrary policies customized for specific cluster profiles. The [latter](https://kubernetes.io/docs/concepts/storage/persistent-volumes/#persistentvolumeclaims), known as `PersistentVolumeClaims`, is manifested as a specification and status of claims to these storage classes and typically encapsulates storage size, application binding and input/output access properties.

### Storage Classes

Managed at the infrastructure level and constrained to either a self-hosted or Google Cloud Platform hosted storage provider service (due to infrastructure's current use of Google Kubernetes Engine or GKE), implementing and integrating storage classes within Mina's infrastructure entails installing *k8s* `StorageClass` objects, like the following, utilizing either a stand-alone Helm chart, integrated into a common/core component Helm chart (to what amounts to something like the following *yaml* definition):

```
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: <testnet-ssd | testnet-standard>
provisioner: kubernetes.io/gce-pd
parameters:
  type: <pd-ssd | pd-standard>
  fstype: ext4
  replication-type: none
volumeBindingMode: WaitForFirstConsumer
```

or explicitly defined as a Terraform `kubernetes_storage_class` [resource](https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/storage_class):

```
resource "kubernetes_storage_class" "infra_testnet_ssd" {
  metadata {
    name = "testnet-ssd | testnet-standard"
  }
  storage_provisioner = "kubernetes.io/gce-pd"
  parameters = {
    type = <pd-ssd | pd-standard>
    fstype = "ext4"
    replication-type = "none"
  }
  volume_binding_Mode = "WaitForFirstConsumer"
  allow_volume_expansion = true
}
```


The Terraform resource definition approach is preferred since the resources are themselves singular infrastructure components and can be implemented within existing infrastructure terraform definition files, organized by region. Moreover there doesn't currently exist a fitting location for common Helm objects and their associated Kubernetes resources and its debatable whether a common Helm application (vs. library) is desirable. The initial thinking is that a single storage class will need to be defined for all of infrastructure's storage purposes per supported type (standard + ssd) per testnet deploy region. This should facilitate initial experimentation as the classes' reliability and performance are vetted in practice for different use-cases as well as provide resiliency in the event of regional outages. Note that storage provided by a class is replicated between availability zones of the installation region which is itself scoped to a *k8s* context when applying via `terraform apply` or either `helm or kubectl` directly (hence the lack of a specification of zone or region within the above definitions). It may be worth creating separate storage classes for manually vs. automatically deployed testnets for book-keeping/organizational purposes though it doesn't quite seem worth it at the moment considering the availability of object metadata, labeling and annotations involved at the volume claim level to help with this organization.


#### Cost and Performance

According to GCP's disk pricing documentation, each GB of persistent storage costs approximately $0.17 on provisioned SSD disks and about $0.04 for standard HDD/SCSI disks per month. While the amount of storage a single testnet would claim is variable in nature and dependent on its scale (i.e. the number of wallet keys, snark keys, static/runtime configs and chain intermediary states), a significant portion of the storage space is expected to be consumed by either shared or singular resources (e.g. genesis and potentially epoch ledgers, shared keysets and archive-node postgres DBs). A reasonable, albeit rough, estimation currently exists of ~10GB per deployment (with a bit of variance forseeable once in practice). With this mind, a single testnet deployment maintained over an entire month would likely increase infrastructure costs by about $17 for SSD performance and $4 for standard performance. Considering that the average life-expectancy of a testnet generally is far from a month, even for public testnets (more on the order of a few days to a couple of weeks for manual deploys and minutes to hours for automated deployments for testing), the cost impact of this solution should be relatively negligible as long as infra sustains the default policy of delete volumes on pod/claim cleanup and proper hygiene and practices are followed for cleaning up obsolete testnets. __Note:__ recent reports show between ~$300-$410 per month of Google Cloud Storage costs. We also propose taking advantage of GCP Monitoring [Alerting Policies](https://console.cloud.google.com/monitoring/alerting/policies/create?project=o1labs-192920) to monitor and alert on defined thresholds being met for various categories including but not limited to overarching totals, per testnet allocations, automated testing runtime and idle usage.

With regards to performance and as previously mentioned, the current plan of record is to provision both standard and ssd storage types, leverage only ssd initially for testnet storage use-cases and at the same time gradually benchmark/test different use-cases on both ssd and standard types across regions. This should help us identify candidates to migrate from ssd to standard with very little to no impact on performance without compromising on performance in the present and potentially limiting engineering velocity anymore than necessary.

### Persistent Volume Claims

Defined and dynamically generated at the application level, *k8s* `PersistenceVolumeClaims` objects will be implemented as *Helm* named templates which, as demonstrated with the pending Healthcheck work, can be pre-configured with sane defaults and yet overridden at run or deploy time with custom values. These templates will be scoped to each particular use-case, whether providing a claim to persistence for a single entity within a testnet deployment or a shared testnet resource - again taking into consideration of the idea and goal of being able to provide defaults for the majority of cases and customization where deemed necessary.

#### Component specific state

State specific to individual testnet components will be set within each component's Helm chart, likely as an additional YAML doc included within deployments similar to the following: 

```
<component deployment specification>
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  testnet: {{ $.Release.Name }}
  name: "block-producer-{{ $config.name }}-runtime-state"
spec:
  accessModes:
    - ReadWriteMany
  volumeMode: Filesystem
  resources:
    requests:
      storage: {{ $.Values.storage.runtime.capacity }}
  storageClassName: {{ $.Values.storage.runtime.class }
```

Considering the use of named templates and the minimal single line footprint of their inclusion into files, we think it makes sense to avoid creating a separate chart file for PVCs at least for core volume claims.  

#### Testnet shared state

For shared resources, it makes sense to define within a Terraform `kubernetes_persistent_volume_claim` [resource](https://registry.terraform.io/providers/hashicorp/kubernetes/latest/docs/resources/persistent_volume_claim) implemented a part of the Terraform `testnet` module as opposed to a particular Helm chart. For example, something like the following would be generated once and then levereaged as mount volumes within all relevant pods within a testnet:

```
resource "kubernetes_persistent_volume_claim" "genesis_ledger" {
  metadata {
    name = "${testnet_name}-genesis-ledger"
  }
  spec {
    access_modes = ["ReadWriteOnce"]
    resources {
      requests = {
        storage = "${var.storage.genesis_ledger}
      }
    }
    storage_class_name = "<testnet-ssd | testnet-standard>"
  }
}
```

#### Persistent Volume Claim named templates

In order to minimize code redundancy and promote consistency across Mina Helm charts with regards to defining persistent volume claims for various testnet component storage needs, we propose leveraging Helm's [named templates](https://helm.sh/docs/chart_template_guide/named_templates/) feature. Named templates are a mechanism introduced in version 3 of *Helm* that are basically Helm templates defined in a file and namespaced. They enable re-use of a single persistent-volume claim (PVC) definition and allow for the setting of sane/standard defaults with added variability/customization through embedded values scoped to input template rendering arguments. The choice is based on this concept and the following:
  - enables single claim definitions and re-use throughout source chart and all others (external dependency charts, subcharts) within a Helm operation runtime (e.g. a PVC for each testnet's `genesis_ledger` and each component's chain intermediary state would be need to be set within each deployment though only defined once in a shared named template)
  - provides a single and consistent source of truth for standard PVCs throughout Mina's Helm charts
  - template definitions can be and are recommended to be namespaced by source chart (though namespace collisions are handled with last to load taking precedence)

Moreover, we also propose the creation of a common Mina Helm [library chart](https://helm.sh/docs/topics/library_charts/) to define standard Mina PVC types in as well as to be imported by component charts dependent on them. This libary chart would be versioned and included within the Helm CI pipeline, as all other active Mina charts, allowing for proper linting/testing when changed and intentional upgrades by dependencies. The following shows an example of the PVC type named templates to be implemented and included within this chart.

```
{{/*
Mina daemon wallet-keys PVC settings
*/}}
{{- define "testnet.pvc.walletKeys" }}
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  testnet: {{ $.Release.name }}
  name: "wallet-keys-{{ $.id }}"
spec:
  accessModes:
    - ReadWriteMany
  volumeMode: Filesystem
  resources:
    requests:
      storage: {{ $.capacity }}
  storageClassName: {{ $.Values.storage.walletKeys.storageClass }}
{{- end }}
```


#### Role Specific Volume Mounts

Of the available testnet roles, there are generally both common and role-specific persistent storage concerns which are candidates for application of the proposed persistence solution. We attempt to enumerate all roles and potential persistent storage applications currently of interest within this section in order to demonstrate the extent of the integration.

| Role  | Description | Storage Requirements |
| ------------- | ------------- | ------------- |
| Archive Node  | archival service tracking chain dynamics and state  | `{ 1. genesis ledger, 2. daemon config, 3. runtime state, 4. postgres database state }` |
| Block Producer  | active participant in a chain's block production protocol  | `{ 1. genesis ledger, 2. wallet keys, 3. daemon config, 4. runtime state }` |
| Seed Node  | standalone client acting as a bootstrapping and peer discovery source for network participants | `{ 1. genesis ledger, 2. daemon config, 3. runtime state }` |
| SNARK coordinator/worker  | entities responsible for producing SNARK proofs used in validating chain transactions  | `{ 1. genesis ledger, 2. wallet keys, 3. daemon config, 4. runtime state }` |
| User agent  | (test) bot tasked with sending chain transactions as a means of fabricating network/chain dynamics  | `{ 1. genesis ledger, 2. wallet keys, 3. daemon config, 4. runtime state }` |
| Faucet  | (test) bot tasked with dispersing testnet funds to participants as means of enabling network activity  | `{ 1. genesis ledger, 2. wallet keys, 3. daemon config, 4. runtime state }` |
| Echo Service  | (test) bot tasked with echo'ing participant transactions for simulating an end-to-end transaction experience  | `{ 1. genesis ledger, 2. wallet keys, 3. daemon config, 4. runtime state }` |

##### Genesis Ledger

_persistence type:_ common
_mount path:_ `/root/.coda-config/genesis/genesis_ledger*`
_expected size:_ xx
_access mode:_ ReadOnlyMany

##### Genesis Proof

_persistence type:_ common
_mount path:_ `/root/.coda-config/genesis/genesis_proof*`
_expected size:_ xx
_access mode:_ ReadOnlyMany

##### Epoch Ledger

_persistence type:_ common
_mount path:_ `/root/.coda-config/epoch_ledger.json`
_expected size:_ xx
_access mode:_ ReadOnlyMany

##### Daemon Config

_persistence type:_ common
_mount path:_ `/config/daemon.json`
_expected size:_ xx
_access mode:_ ReadOnlyMany

##### Wallet Keys

_persistence type:_ individual or common (keysets)
_mount path:_ `/root/wallet-keys`
_expected size:_ *12Ki*
_access mode:_ ReadOnlyMany

##### Runtime State

_persistence type:_ individual
_mount path:_ `/root/.coda-config/*`
_expected size:_ *2Gi* 
_access mode:_ ReadWriteOnce

##### Example Runtime (block-producer) Filehandles:

```
coda     11 root    7u      REG     8,1   373572603  656496 /root/.coda-config/coda.log                                                               [33/881]
coda     11 root    8u      REG     8,1     2800287  656427 /root/.coda-config/mina-best-tip.log                                                              
coda     11 root    9w      REG   0,152       15760 1702339 /tmp/coda_cache_dir/c0e9260e-2dcc-3355-ad60-2ffc0c4bb94d/LOG                                      
coda     11 root   10r      DIR   0,152        4096 1702338 /tmp/coda_cache_dir/c0e9260e-2dcc-3355-ad60-2ffc0c4bb94d                                          
coda     11 root   11uW     REG   0,152           0 1702340 /tmp/coda_cache_dir/c0e9260e-2dcc-3355-ad60-2ffc0c4bb94d/LOCK                                     
coda     11 root   12r      DIR   0,152        4096 1702338 /tmp/coda_cache_dir/c0e9260e-2dcc-3355-ad60-2ffc0c4bb94d                                          
coda     11 root   13w      REG   0,152       32565 1702344 /tmp/coda_cache_dir/c0e9260e-2dcc-3355-ad60-2ffc0c4bb94d/000003.log                                                                                                         
coda     11 root   18w      REG     8,1     8956787  656451 /root/.coda-config/trust/000003.log                                                               
coda     11 root   19w      REG     8,1       15664  656468 /root/.coda-config/receipt_chain/LOG                                                              
coda     11 root   20r      DIR     8,1        4096  656467 /root/.coda-config/receipt_chain                                                                  
coda     11 root   21uW     REG     8,1           0  656469 /root/.coda-config/receipt_chain/LOCK
coda     11 root   23w      REG     8,1           0  656473 /root/.coda-config/receipt_chain/000003.log

coda     11 root   24w      REG     8,1       15656  656476 /root/.coda-config/transaction/LOG
coda     11 root   28w      REG     8,1           0  656481 /root/.coda-config/transaction/000003.

coda     11 root   29w      REG     8,1       15724  656484 /root/.coda-config/external_transition_database/LOG
coda     11 root   31uW     REG     8,1           0  656485 /root/.coda-config/external_transition_database/LOCK
coda     11 root   33w      REG     8,1    22792019  656489 /root/.coda-config/external_transition_database/000003.log

coda     11 root   49w      REG     8,1      692991  656520 /root/.coda-config/frontier/LOG
coda     11 root   51uW     REG     8,1           0  656504 /root/.coda-config/frontier/LOCK
coda     11 root   54w      REG     8,1       26641  656522 /root/.coda-config/frontier/MANIFEST-000005
coda     11 root  543r      REG     8,1    15839261  656529 /root/.coda-config/frontier/000370.sst
coda     11 root  548r      REG     8,1    76433064  656537 /root/.coda-config/frontier/000365.sst
coda     11 root  552r      REG     8,1    15711505  656534 /root/.coda-config/frontier/000368.sst
coda     11 root  555w      REG     8,1    49954439  656521 /root/.coda-config/frontier/000371.log
coda     11 root  556r      REG     8,1      187430  656542 /root/.coda-config/frontier/000366.sst
coda     11 root  558r      REG     8,1    16822452  656533 /root/.coda-config/frontier/000372.sst

coda     11 root   56w      REG     8,1       16774  656508 /root/.coda-config/root/snarked_ledger/LOG
coda     11 root   58uW     REG     8,1           0  656513 /root/.coda-config/root/snarked_ledger/LOCK
coda     11 root   60r      REG     8,1       34928  656524 /root/.coda-config/root/snarked_ledger/000004.sst
coda     11 root   61w      REG     8,1         112  656525 /root/.coda-config/root/snarked_ledger/MANIFEST-000005
coda     11 root   62w      REG     8,1      370077  656515 /root/.coda-config/root/snarked_ledger/000006.log
```

##### Postgres DB State

_persistence type:_ individual
_mount path:_ xx
_expected size:_ xx
_access mode:_ ReadWriteOnce

#### Labels and Organization

As previously mentioned, each of the referenced storage persistence use-cases would make use of the Kubernetes `PersistentVolumeClaim` resource which represents an individual claim or reservation to a volume specification (e.g. storage capacity, list of mount options, custom identifier/name) from an underlying storage class. Properties of this resource are demonstrated below though we highlight several pertaining to the organization of this testnet(-role):`pvc` mapping.

`pvc.metadata.name`: <string> - identifier of the persistent volume claim resource. Must be unique within a single namespace and referenced within a pod's `volumes` list. e.g. *pickles-nightly-wallets-11232020*

`pvc.metadata.namespace`: <string> - testnet from which a persistent volume claim is requested. e.g. *pickles-nightly-11232020*

`pvc.spec.volumeName`: <string> - custom name identifier of the volume to issue a claim to. **Note:** Claims to volumes can be requested across namespaces allowing for shared storage resources between testnets provided the volume's retain policy permits retainment or rebinding. e.g. *pickles-nightly-wallets-11232020-volume*

**Persistent Volume Claim (PVC) Properties:**

```
FIELDS:                                                                                                                                                       
   apiVersion   <string>                                                                                                                                      
   kind <string>                                                                                                                                              
   metadata     <Object>                                                                                                                                      
      annotations       <map[string]string>                                                                                                                   
      clusterName       <string>                                                                                                                           
      labels    <map[string]string>                                                                                                                           
                                                                                                                                    
      name      <string>                                                                                                                                      
      namespace <string>                                                                                                                                      

   spec <Object>
      accessModes       <[]string>
      dataSource        <Object>
         apiGroup       <string>
         kind   <string>         name   <string>
      resources <Object>
         limits <map[string]string>
         requests       <map[string]string>
      selector  <Object>
         matchExpressions       <[]Object>
            key <string>
            operator    <string>
            values      <[]string>
         matchLabels    <map[string]string>
      storageClassName  <string>
      volumeMode        <string>
      volumeName        <string>
   status       <Object>
      accessModes       <[]string>
      capacity  <map[string]string>
      conditions        <[]Object>
         lastProbeTime  <string>
         lastTransitionTime     <string>
         message        <string>
         reason <string>
         status <string>
         type   <string>
      phase     <string>
```

Within each pod specification, the expectation is that each testnet role/deployment will include additional `volume` entries within the `pod.spec.volumes` listing indicating all `pvcs` generated within a particular testnet to be available to containers within the pod. The additional volume listings generally amount to slight modifications to the current `emptyDir` volumes found throughout certain testnet role charts. So far example, the following `yaml`:

```yaml
- name: wallet-keys
  emptyDir: {}
- name: config-dir
  emptyDir: {}
```

would become something like:
```yaml
- name: wallet-keys
  persistentVolumeClaim: pickles-nightly-wallets-11232020
- name: config-dir
  persistentVolumeClaim: pickles-nightly-config-11232020
```

Note that individual container volume mount specifications should not have to change as they are agnostic to the underlying volume type by design.

#### Testing

There currently exists linting and dry-run tests executed on each Helm chart change triggered through the Buildkite CI Mina pipeline. To ensure proper persistence of testnet resources on deployment in practice though, we propose leveraging Protocol's integration test framework to generate minimal Terraform-based test cases involving persistence variable injection (to be consumed by named templates), Helm testnet role releases and ideally Kubernetes job resources for enacting test setup and verification steps. This could amount to an additional configuration object within *mina*'s `integration_test_cloud_engine` library representing job configs. Similar to how Network_configs are defined though significantly lesser in scope, `Job_configs` would basically amount to a pointer to a script (within source or to be downloaded) to execute the setup and verification steps.  

## Drawbacks

[drawbacks]: #drawbacks

* further dependency on single cloud resource vendor (GCP) 

## Rationale and alternatives

[rationale-and-alternatives]: #rationale-and-alternatives

Why is this design the best in the space of possible designs?

We believe this to be the best design in the space due to its flexibility in application (due largely to the use of `named templates`) and simplicity of integration into the existing infrastructure setup based on our current affinity for `GCP` as a provider for cloud resources.  

What other designs have been considered and what is the rationale for not choosing them?

### Migration to other Cloud Provider K8s solutions

Considering the current project's time boundedness, development costs to migrate existing infrastructure setups in addition to potential increases in operational costs and the cognitive overhead of familiarizing developers with new cloud provider tools and user-interfaces, the benefits of migrating to another cloud provider k8s solution (e.g. AWS EKS or Azure's AKS), which generally consist of both potential cost + performance savings in the form of organization credits and/or more economical and efficient resource offerings, the benefits don't appear to outweigh the costs. Though considering the relatively portable nature of most infrastructure components, further investigation and analysis is suggested and planned as the project progresses.

### Stateful Sets

The decision away from StatefulSets comes down to flexibility (in both pod naming scheme and persistent-volume claim binding) and avoiding the added overhead of having to create a headless service to associate with each StatefulSet. StatefulSets are considerably bespoke when it comes to provisioned pod names (based on the ordinal naming scheme) and persistent-volume claim names as well as lack the same degree of customizability that we get by merely using templated PVCs and allowing per pod volume mounting scoped to common defaults or overridden if desired with specific values as expressed within Values.yaml. This allows for the scenario involving unique artifacts per testnet in addition to cases where one would want to launch a testnet making use of other artifacts/volumes for troubleshooting/experimentation purposes. Again the goal here is to allow flexibility while providing an out-of-the-box solution that works for most cases.

The overhead of having to manually create "headless" services for each StatefulSet isn't massive though again unnecessary and also requires that we migrate from using Deployments with additional overhead there without much gain and added constraints. It's probably worth noting that StatefulSets also don't automatically clean up storage/volume resources created during provisioning - that cleanup is left as a manual task and again speaks to the lack of reasoning to make the switch. Really what they provide is naming/identifier stickiness across nodes for the pod-pvc relationship though, again, PVCs are just resources that can be bound to without really worrying about which entity is requesting the bind or what the pods name is.

What is the impact of not doing this?

The impact of not implementing a persistent-storage solution such as what's proposed would amount to the continued experience of the aforementioned pain-points which result in considerable inefficiences and limitations when it comes to the robustness of testnets in the event of unexpected or in some cases expected destructive operations in addition to the ability for testnet operations to iterate on deployments in place without losing state.

## Prior art

[prior-art]: #prior-art

* [Refactor](https://github.com/MinaProtocol/coda-automation/issues/352) Block Producer Helm Chart should use StatefulSet
* Persistence investigations for [testing various chain scenarios](https://github.com/MinaProtocol/coda-automation/issues/391)

## Unresolved questions

[unresolved-questions]: #unresolved-questions

TBD
