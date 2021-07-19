# TODO LIST

- document basic actions
- document crash expectations per each DSL function
- generalized log searching with finds/filters/maps
- detail how assertions are performed at the end of tests
- determine how rolling assertions will work

# Protocol Testing

## Summary
[summary]: #summary

##### TODO

## Motivation
[motivation]: #motivation

##### TODO

As we march towards mainnet

#### Tests are not abstract over execution environment

The current integration test suite sits on top of a local process test harness which it uses to create a local network of nodes on the current machine. Tests can control and inspect these nodes by using RPCs. There is poor separation between the integration tests and the local process test harness, meaning that if we want to write tests that execute in a different envionment (e.g. in the cloud instead of local), we need to write new tests for that environment. Having our tests only support the local execution environment means that we cannot use our tests to benchmark aspects of the protocol (e.g. bootstrapping time, block propagation on real networks, TPS, etc...). We also can't run very large tests since all the processes have to share the same machine (and often we choose to run our tests without SNARKs on for this same reason). Having an abstraction over the execution environment will allow us to not only run large, distributed tests that we can bechmark with the same test description, but also it would allow us to reuse our testing infrastructure for other things, such as testnet deployment and validation. Afterall, what is a validated deployment other than a test that keeps the network running after success?

#### Tests are tightly coupled to build profile configuration

Our current tests are very tightly coupled to the build configuration set in the dune build profile associated with each test. This causes a variety of issues, from the mild annoyance of having to keep an external script (`scripts/test.py`) to keep track of the correct set of `profile:test` permutations, to more serious ones that have plagued us for a while now. Most notably, the issue that has repeatedly bitten us related to this is having difficult to detect logical mismatches between hard coded timeouts in the integration test code and the more realistic range of possible execution times created through a combination of build profile configuration values (delta, slot interval, k, etc...). This has caused everything from flakey tests (such as the infamous holy grail test, which Jiawei recently determined a bug from after it being disabled for months), to sudden CI blockers that get introduced to develop by small configuration/efficiency changes to the protocol (since it can pass on the branch where it's introduced even though it fails a majority of the time). This can be addressed by having a more principled DSL for describing tests which provides tools for more interactive and abstract ways to wait for things, and avoiding any kind of hard timeouts for tests all together.

#### Tests are difficult to debug

##### TODO: CLEANME

The integration tests we are using right now are very difficult to debug. Even when they find issues, it can take a long time to identify those issues. This is the case because errors thrown from integrations tests can often be seemingly unrelated to the changes that cause them. Obviously, these errors eventually make sense to us, but it takes a large amount of diagnosing and developer effort to get there in many cases. There are a few reasons for this, some of which are unavoidable. For instance, it is somewhat expected that there will be tracing to do to connect error messages with root causes in something as complex as a Coda. However, this is made more difficult by some things which are avoidable that we can fix with some effort.

1. Our Coda processes test harness contains many boilerplate assertions, where as better isolated tests can make it easier to find a minimum test case that fails to help narrow a lot of possibilities to check and track.
2. We do not have much information into what the test was doing when it failed out of the box. It's common practice to add temporary custom logging to identify where the test was.
3. Logs for all nodes and the test runner are all squished in a way that is annoying to deal with and debug. Logproc makes it easier, but it is not a commonplace tool among developers and is missing some important features to help alleviate this.
4. Timeouts in tests result in immediate cancellation of the test. Timeouts should fail a test, but letting a test run for a bit past a fail timeout is helpful in filtering out tests which start failing due to decreases in efficiency and not due to errors in the code. This is key as timeout adjustments are a very common fix to our tests, but currently take a lot of time to diagnose before making the decision to bump the timeout.

#### Tests require too much internal knowledge to write

Writing tests right now requires a lot of internal knowledge regarding the protocol. Interaction with nodes is done over a custom RPC interface, timeouts are raw millisecond values which require knowledge of consensus parameters, ouroboros proof of stake, and general network delay/interaction in order to determine the correct values, etc. The downside of this is that, while tests should be primarily written by Protocol Engineers, other teams in the organization, such as Product Engineers and Protocol Reliability Engineers (PREs) should be able to write tests when necessary. For instance, Product may want to add tests to ensure the API interacts as they expect it to under specific protocol conditions, and the PRE team may want to write a test to validate a bug they encountered in a testnet or to add a new validation step to the release process. This can be addressed by using a thoughtful DSL which focuses on abstracting the test description too a layer which requires as minimal internal knowledge as possible. If designed well, this DSL should even be approachable for non-OCaml developers to learn without having to learn OCaml in too much depth first.

## Requirements
[requirements]: #requirements

##### TODO: intro for reqs

### Benchmarks

- **TPS**
- **Bootstrap**
- Ledger Catchup
- SNARK Bubble Delay
- VRF w/ Delegation
- **Ledger Commit**
- Disk Usage

### Tests

- Existing
- **Bootstrap Bombardment**
- Better Bootstrap
- Better Catchup
- Persistence
- Multichain Tests (ensure different runtime parameters create incompatible networks)
- **Partition Rejoin**
- Doomsday Recovery
- **Hard Fork**
  - Protocol Upgrades
  - SNARK Upgrades
- **Adversarial Testing**
  - ...

### Validation

- sending all txn types
- all nodes are synced
- nodes can produce blocks? (if distribution allows this condition)
- network topology gut-checks
- services health checks
  - api
  - archive
  - etc...

### Unit Benchmarks

- app size
- mem usage (et al)
- algorithm timings (et al)
- disk usage

## User Stories
[user-stories]: #user-stories

##### TODO: rephrase in general context (talk about web app and CI auto dispatching missing tests/metermaid)
As an example query, let's say we wanted to validate a branch that intends to decrease the bootstrapping time of the network with relation to ledger size. We would build a query like "give me all bootstrap measurements for develop and \<HEAD OF NEW BRANCH\> for the configuations `num_initial_accounts=[10k, 100k, 1m], network_size=[5, 10, 15]`".

## Prerequisites
[prerequisites]: #prerequisites

- runtime configuration
- artificial neighbor population
- generalized firehose access (optional)

## Detailed design
[detailed-design]: #detailed-design

### Cross-Build Measurements Storage System

Measurements refers to the whole category of various benchmarks, metrics, and compute properties we want to record and view from various builds of our daemon. We will need some place to store measurements associated with different builds and configurations which is accessible to CI and our developers (at minimum). This storage system should support querying measurements by time, builds (git SHAs), and configuration matrices.

I'm not certain yet what the best tool is to use for this use case, but I imagine that a simple cloud hosted NoSQL database such as DynamoDB would work pretty well here. A time series database could be useful for tracking improvements across develop over time (where the timestamp for everything is the timestamp of the git commit, not when the test was run), but the win here seems minimal for the added cost of obscurity. It's ok if queries on this storage system are not super fast.

### Unit Benchmarks

Some of the metrics we want can be expressed as unit benchmarks, which are easy setup and begin recording today with minimal effort. The [benchmark runner script](https://github.com/CodaProtocol/coda/blob/develop/scripts/benchmarks.sh) can be extended to not only run the benchmarks, but to also collect the timing information from the output of the benchmarks and publish these numbers to the measurement storage system.

### Integration Test Architecture

Below is a diagram of the proposed testing architecture, showing a high level dataflow of the components involved in executing tests. Details are left abstract over where the tests will be running for now as the architecture is intended to remain the same, primarily swapping out the orchestra backend to change testing environments.

![](res/abstract_testing_architecture.png)

##### Orchestrator/Orchestra

The orchestrator is some process which allocates, monitors, and destroys nodes in an orchestra. It provides some interface to control when and what nodes are allocated, how to configure those nodes, and when to destroy them. During the cycle of a single test, the test executive will register a new orchestra with the orchestrator, which will only live so long as the test is executing. The orchestrator will automatically clean this orchestra up when the test is completed, unless it is told to do so earlier. Orchestrators support a number of configurations for nodes, most of which are mapped down to the runtime configuration fed to the daemon for that node. One important feature of the orchestrator's node configurations is the ability to optional specify a specific network topology for that node, which is to say, the orchestrator can control precisely which peers a node will see as neighbors on the gossip network.

The orchestrator does not necessarily need to be a separate process from the test executive, but it is separate in the architecture so that custom local process management ochestrators can be swapped out for cloud orchestrators with no changes to tests.

##### Test Executive

The test executive is the primary process which initializes, coordinates, and records the test's execution. It interprets the test specification DSL, sending messages to various other processes in the system in order to perform the necessary actions to run the test. It begins the test by registering a new orchestra with the orchestrator, then spawning the necessary metrics infrastructure and initial nodes in the test orchestra. Depending on the specification of the test, it may send various API messages to various nodes in the test network, or wait for certain events/conditions by subscribing to the event streams of various node, or some combination of the two. Eventually, the executive will terminate the test (either by failure/timeout, or by reaching an expected terminating network state). Once this happens, the executive will determine whether the test was successful and collect any relevant metrics for the test by parsing through the metrics and logs for the test. Finally, the orchestra will be torn down and the executive will record the final results for the test to a database, where we can observe and compare test results from multiple test runs at once.

### Orchestra Backends

TODO: prune first paragraph to update for Kubernetes decision

The new integration tests have the ability to support multiple implementations of the orchestrator which can be swapped out in place to execute the same test description in different testing environments. The primary orechestra backend that would be used in CI for many of the basic integration tests would be similar to the existing test harness in that it would create all the nodes locally on the machine. For running larger tests and tests we want to collect measurements from, we would use a cloud based backend for spawning individual virtual machine instances for all the nodes. One thinking on this is that we could use Kubernetes for this, but really, we can use any tool, Kubernetes just might save some work since it already fits many of the requirements for an orchestrator, meaning we would just need to write a thin wrapper to set it up as an orchestrator. In the future, we could also run more distributed tests by having an orchestrator which communicates with multiple cloud providers in multiple regions at once. If this is a strongly desirable capability now, it may be worth implementing the single cloud provider backend using something like terraform instead of Kubernetes so that we don't need to do extra work in the future.

Kubernetes based backend allows us to write 1 backend and run both live in the cloud and locally through the use of Minikube.

https://kubernetes.io/blog/2018/05/01/developing-on-kubernetes/

### Test Description Interface

This section details the full scope of the design for the new test description interface. Note that this will all be scoped down to a MVP subset of this interface that gives us what we need immediately for our goals before mainnet.

[Test DSL Specification](../docs/test_dsl_spec.md)

#### Requirements

- concurrent programming support
- automated action logging
- abstract waiting facilities with soft timeouts (don't fail tests too early)
- different runtime configurations per node
- explicit node topology descriptions
- end of test log filtering and metrics collection
- errors are collected and bundled together in a useful way

### Result Collection/Surfacing

### Development Workflow Integration

## Work Breakdown/Prioritization

- generalized flesh out log processing interface
- testing DSL
  - monad w/ non-fatal error accumulation and fatal error short circuiting
  - spawning/destroying nodes
  - interacting with nodes
  - `wait_for`
  - ...
- local orchestrator process backend
- cloud orchestrator backend
- convert existing tests into new DSL

## Drawbacks
[drawbacks]: #drawbacks

- likely have an increased maintenence cost short term due to complexity of moving parts
- may add additional time overhead to run tests due to docker + kubernetes (although using docker + kubernetes allows us to decouple builds from tests)

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

## Prior art
[prior-art]: #prior-art

## Unresolved questions
[unresolved-questions]: #unresolved-questions
