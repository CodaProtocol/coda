
<!-- DSL DESIGN SECTION -->
#### DSL Semantics

- error accumulation
- leftover nodes count as errors (ensures crashes are bundled as errors or are manually expected)
- analysis steps extraction
  - assertion steps
  - collection steps

##### Testing Phases

Running a test occurs in two distinct phases: the execution phase, and the analysis phase.

###### Execution

The execution phase is in which, as the name implies, the test is actually executed. This includes spawning nodes through the orchestrator, communicating with nodes to execute the test, and some runtime assertions which will stop the test (which are mostly hidden inside DSL primitives). The execution phase continues until the test DSL has been fully interpreted. If the test is stopped prematurely, the execution phase will skip the analysis phase and will merely tear down the orchestra and provide all errors accumulated thus far.

###### Analysis

The analysis phase occurs at the end of a test run. In this phase, the test executive consumes logs and metrics collected from nodes during the test and checks for various conditions asserted during the test. At the conclusion of this phase, any dangling members of the orchestra are cleaned up.

##### Error Accumulation

Two different levels of errors can occur when running a test: non-fatal errors, and fatal errors. Most errors when running a test are considered non-fatal. For instance, if the test asserts something happens and it does not, this is considered a non-fatal error by default since the rest of the test can continue executing and discover more errors. Similarly, a hitting soft-timeout when waiting for a condition is a non-fatal error, so the test will continue running if the condition triggers before a hard-timeout, helping to identify regressions or recently invalidated or even flakey timeouts. An example of a non-fatal error, besides the hard-timeout example stated previously, is when the test attempts to interact to a node which has crashed. By comparison, for another example of the non-fatal/fatal error division, destroying a node which has crashed is non-fatal. Fatal errors, by comparison, will immediately fail the test. Fatal errors can only occur during the execution phase of a test; any errors during the analysis phase are considered non-fatal. Non-fatal errors are accumulated as the test is run and are emitted at the end of the test. For obvious reasons, the definition of a "passing" test is a test which does finishes successfully without accumulating any non-fatal errors.

TODO: error severity ordering? seems tricky to get right, but super useful

<!-- DSL DESIGN SECTION -->
#### DSL Primitives

##### Decorations

```ocaml
val section : string -> node test -> node test
```

The `section` function associates a portion of DSL code with a name. This is useful for tracking the high level of "why" the test is doing what it is doing. The test DSL is able to provide human descriptions of what it is doing most of the time, but sections allow the tests to also have high level logical groupings of "why" we are doing the specific operations being described. These names get propagated to fatal failures and errors that are accumulated.

##### Orchestra Interactions

```ocaml
val spawn : node_config -> node_arguments -> node test
```

The `spawn` function communicates with the orchestrator to allocate and start a new node. This function takes a runtime config for the node and CLI arguments to execute the node with (not necessarily fed in via the CLI though, in the case of local tests). This function does not wait for the node to join the network; execution of the test will continue as soon as the node is started, regardless of errors.

```ocaml
val destroy : node -> unit test
```

The `destroy` function communicates with the orchestrator to destroy nodes that have been created. If the node attempting to be destroyed is already dead due to a crash, the test execution will continue, but the crash will accumulated as an error. If the node attempting to be destroyed has been previously destroyed via this function, an error is accumulated. Any nodes that were spawned and are not destroyed by the end of a test will cause errors to be accumulated (we want all spawned nodes to be asserted to have not crashed or to be expected to crash by the end of the test).

##### Concurrent Tasks

```ocaml
type task
val task : (unit test -> unit test) -> task test
val stop : task -> unit test
```

##### Synchronization

```ocaml
val wait_for :
     (* exclusive arguments *)
     ?status:node_status
  -> ?blocks:int
  -> ?epoch_reached:int
  -> ...
     (* non-exclusive arguments *)
  -> ?timeout:
       [ `Slots of int
       | `Epochs of int
       | `Milliseconds of int64 ]
  -> unit
  -> unit test
```

##### Analysis

```ocaml
val assert : ...
```

```ocaml
type collection = Logger.Log.t -> Metric.t list
val collect : node -> collection -> unit test
```

##### Node Interactions: TODO

<!-- DSL DESIGN SECTION -->
#### Pseudo-code Examples

##### Basic Bootstrap Test

```ocaml
let genesis_ledger = Genesis_ledger.of_balances [2_000] in
let config =
  { genesis_ledger
  ; proof_level= `Check
  ; slot_time= 20_000
  ; k= 6
  ; delta= 2 }
in
let args ?seed ?proposer () =
  { default_args with
    seed
  ; proposer_keypair= Option.map proposer (Genesis_ledger.get_keypair genesis_ledger) }
in
let test_bootstrap bootstrap_type expected_slots =
  section bootstrap_type (
    let%bind bootstrapping_node = spawn config (args ~seed ()) in
    let%bind () = wait_for ~status:`Synced ~timeout:(`Slots expected_slots) () in
    let%bind () = collect ~name:bootstrap_type ~node:bootstrapping_node ~metric:`Bootstrap_time () in
    destroy bootstrapping_node)
in
let%bind seed = spawn config (args ())
let%bind node1 = spawn config (args ~seed ~proposer:_0 ()) in
let%bind () = wait_for ~blocks:config.k () in
let%bind () = test_bootstrap "bootstrap after k blocks" 2 in
let%bind () = wait_for ~epoch_reached:2 () in
let%bind () = test_bootstrap "bootstrap after 2 epochs" 6 in
let%bind () = destroy seed in
destroy node1
```

##### Partition Rejoin Test

```ocaml
(* helper for performing peer actions across an entire partition *)
let partition_action partition peers ~f =
  DSL.List.iter partition ~f:(fun node ->
    DSL.List.iter peers_to_ban ~f:(fun peer ->
      f node ~peer))
in

let partition_size = 6 in
let genesis_ledger = Genesis_ledger.of_balances (List.make (partition_size * 2) ~f:(Fn.const 20_000)) in
let config =
  { genesis_ledger
  ; proof_level= `Check
  ; slot_time= 20_000
  ; k= 6
  ; delta= 2 }
in

(* spawn the network, fully connected *)
let%bind seed = spawn config default_args in
let spawn_proposer i =
  let proposer_keypair = Genesis_ledger.get_keypair genesis_ledger i in
  spawn config {default_args with seed; proposer_keypair}
in
let%bind nodes =
  DSL.List.map ~f:spawn_proposer
    (List.make (partition_size * 2) ~f:Fn.id)
in
let left_partition, right_partition = List.partition nodes partition_size in

(* TODO: assert that there is a minimum amount of peers on every box *)

(* wait until the frontier is full *)
let%bind () = wait_for ~blocks:config.k () in
(* this has an implied soft timeout of (`Slots (8 * config.k)) *)

(* disconnect the partitions by updating the blacklists on both sides *)
let%bind () = partition_action ~f:blacklist_peer left_partition right_partition in
let%bind () = partition_action ~f:blacklist_peer right_partition left_partition in

(* kill the seed node so that there is no bridge between the partitions *)
let%bind () = destroy seed in

(* wait for 2 finalizations while the networks are disconnected *)
let%bind () = wait_for ~blocks:(config.k * 2) () in

(* add blacklisted peers to the whitelist on both sides *)
let%bind () = partition_action ~f:whitelist_peer left_partition right_partition in
let%bind () = partition_action ~f:whitelist_peer right_partition left_partition in

(* wait for the 2 partitions to rejoin *)
let%bind () =
  wait_for
    ~common_prefix:((`Nodes (left_partition @ right_partition), `Distance config.delta))
    ~timeout:(`Blocks config.k)
    ()
in

(* tear down the network *)
DSL.List.iter (left_partition @ right_partition) ~f:destroy
```

ALTERNATIVE TEST: keep two partitions separate with a fixed topology, with 1-2 intermediate
nodes bridging the networks, then take the other bridge offline temporarily and then have them
rejoin the network without topologoical restrictions and see if the chains reconverge

##### Basic Hard Fork Test

The following example is incomplete and needs more work to think about how the testing DSL would work with multiple deployment artifacts which it has to be compatible with at once.

```ocaml
(* NOTE: pretty sure I will kill the Vect and Peano GADT stuff *)
let genesis_ledger =
  Genesis_ledger.of_balances
      (* 3 proposers *)
    [ 20_000; 20_000; 20_000
      (* 2 snark workers *)
    ; 1_000; 1_000
      (* txn sink *)
    ; 0 ]
in
let config =
  { genesis_ledger
  ; proof_level= `Full
  ; slot_time=180_000
  ; k=6
  ; delta=2 }
in

(* create the network *)
let%bind seed = spawn config (args ())
let proposer_args n = {default_args with seed; proposer_keypair= Genesis_ledger.get_keypair genesis_ledger n} in
let snarker_args n = {default_args with seed; snarker_keypair= Genesis_ledger.get_keypair genesis_ledger n} in
let node_args = Vect.[proposer_args; proposer_args; proposer_args; snarker_args; snarker_args] in
let nodes = Vect.mapi node_args ~f:(fun i args -> spawn config (args i)) in

(* send payments continually until the snarked ledger transitions *)
let%bind txn_task =
  task (fun txn_task ->
    let i, sender_node = Vect.randomi nodes in
    let%bind () =
      times 10 (
        send_user_command
          ~sender_node
          ~command:(`Payment 1)
          ~from:(Genesis_ledger.get_public_key genesis_ledger i)
          ~to:(Genesis_ledger.get_public_key genesis_ledger (Vect.length nodes)))
    in
    let%bind () = wait_for ~slots:1 () in
    txn_task)
in
let%bind () = wait_for ~snarked_ledger_commits:1 ~timeout:(`Epochs 6) () in
let%bind () = stop txn_task in

(* this is tricky... we need 2 builds so we can distribute the second one... *)
let%bind () = schedule_hard_fork ~node:seed .... in
```

##### TODO: Concurrency Example

