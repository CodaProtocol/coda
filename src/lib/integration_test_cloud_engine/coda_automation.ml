open Core
open Async
open Currency
open Signature_lib
open Mina_base
open Integration_test_lib

let aws_region = "us-west-2"

let aws_route53_zone_id = "ZJPR9NA6W9M7F"

let project_id = "o1labs-192920"

let cluster_id = "gke_o1labs-192920_us-west1_mina-integration-west1"

let cluster_name = "mina-integration-west1"

let cluster_region = "us-west1"

let cluster_zone = "us-west1a"

let list_init_in_batches size max_batch_size ~f ~after_batch =
  let num_batches = size / max_batch_size + if size mod max_batch_size > 0 then 1 else 0 in
  List.concat @@ List.init num_batches ~f:(fun i ->
    let offset = (num_batches - i - 1) * max_batch_size in
    let batch_size = min (size - offset) max_batch_size in
    let values = List.init batch_size ~f:(fun j -> f (offset + j)) in
    after_batch ~count:(offset + batch_size) ~values ;
    values)

module Network_config = struct
  module Cli_inputs = Cli_inputs

  type block_producer_config =
    { name: string
    ; id: string
    ; keypair: Network_keypair.Stable.Latest.t
    ; public_key: string
    ; private_key: string
    ; keypair_secret: string
    ; libp2p_secret: string }
  [@@deriving to_yojson]

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
    ; keypairs: Network_keypair.Stable.Latest.t list
    ; constants: Test_config.constants
    ; runtime_config: Runtime_config.t
    ; terraform: terraform_config }
  [@@deriving to_yojson]

  type 'a accounts = {block_producers: 'a list; additional_accounts: 'a list}

  let count_accounts {block_producers; additional_accounts} =
    List.length block_producers + List.length additional_accounts

  let flatten_accounts {block_producers; additional_accounts} =
    block_producers @ additional_accounts

  let fold_map_accounts accounts ~init:acc ~f =
    let acc', block_producers =
      List.fold_map accounts.block_producers ~init:acc ~f
    in
    let acc'', additional_accounts =
      List.fold_map accounts.additional_accounts ~init:acc' ~f
    in
    (acc'', {block_producers; additional_accounts})

  let id_mapi_accounts accounts ~f =
    { block_producers=
        List.mapi accounts.block_producers ~f:(f "block-producer")
    ; additional_accounts=
        List.mapi accounts.additional_accounts ~f:(f "additional-account") }

  let map_accounts accounts ~f =
    { block_producers= List.map accounts.block_producers ~f
    ; additional_accounts= List.map accounts.additional_accounts ~f }

  let zip_accounts_exn a1 a2 =
    { block_producers= List.zip_exn a1.block_producers a2.block_producers
    ; additional_accounts=
        List.zip_exn a1.additional_accounts a2.additional_accounts }

  let terraform_config_to_assoc t =
    let[@warning "-8"] (`Assoc assoc : Yojson.Safe.t) =
      terraform_config_to_yojson t
    in
    assoc

  let generate_account (account_keypairs : Keypair.t accounts)
      (this_account_keypair : Keypair.t)
      (this_account_config : Test_config.Account_config.t) :
      Runtime_config.Accounts.Single.t =
    let open Account.Timing in
    let format_pk =
      Fn.compose Public_key.Compressed.to_string Public_key.compress
    in
    let balance = Balance.of_formatted_string this_account_config.balance in
    let delegate =
      Option.map this_account_config.delegate ~f:(fun delegation ->
          let delegate_keypair =
            match delegation with
            | Block_producer n ->
                List.nth_exn account_keypairs.block_producers n
            | Additional_account n ->
                List.nth_exn account_keypairs.additional_accounts n
          in
          format_pk delegate_keypair.public_key )
    in
    let timing =
      match this_account_config.timing with
      | Untimed ->
          None
      | Timed t ->
          Some
            { Runtime_config.Accounts.Single.Timed.initial_minimum_balance=
                t.initial_minimum_balance
            ; cliff_time= t.cliff_time
            ; cliff_amount= t.cliff_amount
            ; vesting_period= t.vesting_period
            ; vesting_increment= t.vesting_increment }
    in
    let default = Runtime_config.Accounts.Single.default in
    { default with
      pk= Some (format_pk this_account_keypair.public_key)
    ; sk= None
    ; balance
    ; delegate
    ; timing }

  type prepared_keypair =
    { secret_name: string
    ; network_keypair: Network_keypair.t }

  let distribute_keypairs ~logger ~network_keypairs
      (account_configs : Test_config.Account_config.t accounts) :
      prepared_keypair accounts * Runtime_config.Accounts.Single.t accounts =
    let assign_keypair pool _ =
      match pool with
      | keypair :: rest ->
          (rest, keypair)
      | [] ->
          failwith "not enough network keypairs to generate test config"
      (* should probably log a message when this happens and generate them anyway, despite expense *)
      (* can be done upfront before we do the fold by simply counting the account configs *)
    in
    [%log spam] "Assigning keypairs" ;
    let _leftover_samples, assigned_keypairs =
      fold_map_accounts account_configs ~init:network_keypairs ~f:assign_keypair
    in
    [%log spam] "Generating accounts" ;
    let accounts =
      let keypairs = map_accounts assigned_keypairs ~f:(fun {Network_keypair.keypair; _} -> keypair) in
      zip_accounts_exn keypairs account_configs
      |> map_accounts ~f:(fun (keypair, config) ->
             generate_account keypairs keypair config )
    in
    let prepared_keypairs =
      id_mapi_accounts assigned_keypairs ~f:(fun id index network_keypair ->
        let secret_name = Printf.sprintf "test-keypair-%s-%d" id index in
        {secret_name; network_keypair})
    in
    (prepared_keypairs, accounts)

  let expand ~logger ~test_name ~(cli_inputs : Cli_inputs.t) ~(debug : bool)
      ~(test_config : Test_config.t) ~(images : Test_config.Container_images.t) ~network_keypairs
      =
    let { Test_config.k
        ; delta
        ; slots_per_epoch
        ; slots_per_sub_window
        ; proof_level
        ; txpool_max_size
        ; requires_graphql
        ; block_producers
        ; additional_accounts
        ; num_snark_workers
        ; num_archive_nodes
        ; log_precomputed_blocks
        ; snark_worker_fee
        ; snark_worker_public_key } =
      test_config
    in
    let user_from_env = Option.value (Unix.getenv "USER") ~default:"auto" in
    let user_sanitized =
      Str.global_replace (Str.regexp "\\W|_-") "" user_from_env
    in
    let user_len = Int.min 5 (String.length user_sanitized) in
    let user = String.sub user_sanitized ~pos:0 ~len:user_len in
    let git_commit = Mina_version.commit_id_short in
    (* see ./src/app/test_executive/README.md for information regarding the namespace name format and length restrictions *)
    let testnet_name = "it-" ^ user ^ "-" ^ git_commit ^ "-" ^ test_name in
    (* GENERATE ACCOUNTS AND KEYPAIRS *)
    [%log spam] "Distributing keypairs" ;
    let account_keypairs, runtime_accounts =
      distribute_keypairs ~logger ~network_keypairs {block_producers; additional_accounts}
    in
    (* DAEMON CONFIG *)
    [%log spam] "Building runtime config" ;
    let proof_config =
      (* TODO: lift configuration of these up Test_config.t *)
      { Runtime_config.Proof_keys.level= Some proof_level
      ; sub_windows_per_window= None
      ; ledger_depth= None
      ; work_delay= None
      ; block_window_duration_ms= None
      ; transaction_capacity= None
      ; coinbase_amount= None
      ; supercharged_coinbase_factor= None
      ; account_creation_fee= None
      ; fork= None }
    in
    let constraint_constants =
      Genesis_ledger_helper.make_constraint_constants
        ~default:Genesis_constants.Constraint_constants.compiled proof_config
    in
    let runtime_config =
      { Runtime_config.daemon=
          Some {txpool_max_size= Some txpool_max_size; peer_list_url= None}
      ; genesis=
          Some
            { k= Some k
            ; delta= Some delta
            ; slots_per_epoch= Some slots_per_epoch
            ; sub_windows_per_window=
                Some constraint_constants.supercharged_coinbase_factor
            ; slots_per_sub_window= Some slots_per_sub_window
            ; genesis_state_timestamp=
                Some Core.Time.(to_string_abs ~zone:Zone.utc (now ())) }
      ; proof=
          None
          (* was: Some proof_config; TODO: prebake ledger and only set hash *)
      ; ledger=
          Some
            { base= Accounts (flatten_accounts runtime_accounts)
            ; add_genesis_winner= None
            ; num_accounts= None
            ; balances= []
            ; hash= None
            ; name= None }
      ; epoch_data= None }
    in
    let genesis_constants =
      Or_error.ok_exn
        (Genesis_ledger_helper.make_genesis_constants ~logger
           ~default:Genesis_constants.compiled runtime_config)
    in
    let constants : Test_config.constants =
      {constraints= constraint_constants; genesis= genesis_constants}
    in
    (* BLOCK PRODUCER CONFIG *)
    let block_producer_config index keypair =
      { name= "test-block-producer-" ^ Int.to_string (index + 1)
      ; id= Int.to_string index
      ; keypair= keypair.network_keypair
      ; keypair_secret= keypair.secret_name
      ; public_key= keypair.network_keypair.public_key_file
      ; private_key= keypair.network_keypair.private_key_file
      ; libp2p_secret= "" }
    in
    (* NETWORK CONFIG *)
    let mina_archive_schema =
      "https://raw.githubusercontent.com/MinaProtocol/mina/develop/src/app/archive/create_schema.sql"
    in
    [%log spam] "Generating network config" ;
    { coda_automation_location= cli_inputs.coda_automation_location
    ; debug_arg= debug
    ; keypairs= flatten_accounts account_keypairs |> List.map ~f:(fun {network_keypair; _} -> network_keypair)
    ; constants
    ; runtime_config
    ; terraform=
        { cluster_name
        ; cluster_region
        ; k8s_context= cluster_id
        ; testnet_name
        ; deploy_graphql_ingress= requires_graphql
        ; coda_image= images.coda
        ; coda_agent_image= images.user_agent
        ; coda_bots_image= images.bots
        ; coda_points_image= images.points
        ; coda_archive_image= images.archive_node
        ; block_producer_configs=
            List.mapi account_keypairs.block_producers ~f:block_producer_config
        ; log_precomputed_blocks
        ; archive_node_count= num_archive_nodes
        ; mina_archive_schema
        ; snark_worker_replicas= num_snark_workers
        ; snark_worker_public_key
        ; snark_worker_fee
        ; aws_route53_zone_id } }

  let to_terraform network_config =
    let open Terraform in
    [ Block.Terraform
        { Block.Terraform.required_version= ">= 0.12.0"
        ; backend=
            Backend.S3
              { Backend.S3.key=
                  "terraform-" ^ network_config.terraform.testnet_name
                  ^ ".tfstate"
              ; encrypt= true
              ; region= aws_region
              ; bucket= "o1labs-terraform-state"
              ; acl= "bucket-owner-full-control" } }
    ; Block.Provider
        { Block.Provider.provider= "aws"
        ; region= aws_region
        ; zone= None
        ; project= None
        ; alias= None }
    ; Block.Provider
        { Block.Provider.provider= "google"
        ; region= cluster_region
        ; zone= Some cluster_zone
        ; project= Some project_id
        ; alias= None }
    ; Block.Module
        { Block.Module.local_name= "integration_testnet"
        ; providers= [("google.gke", "google")]
        ; source= "../../modules/o1-integration"
        ; args= terraform_config_to_assoc network_config.terraform } ]

  let testnet_log_filter network_config =
    Printf.sprintf
      {|
        resource.labels.project_id="%s"
        resource.labels.location="%s"
        resource.labels.cluster_name="%s"
        resource.labels.namespace_name="%s"
      |}
      project_id cluster_region cluster_name
      network_config.terraform.testnet_name
end

module Network_manager = struct
  type t =
    { logger: Logger.t
    ; cluster: string
    ; namespace: string
    ; testnet_dir: string
    ; testnet_log_filter: string
    ; constants: Test_config.constants
    ; seed_nodes: Kubernetes_network.Node.t list
    ; block_producer_nodes: Kubernetes_network.Node.t list
    ; snark_coordinator_nodes: Kubernetes_network.Node.t list
    ; archive_nodes: Kubernetes_network.Node.t list
    ; nodes_by_app_id: Kubernetes_network.Node.t String.Map.t
    ; mutable deployed: bool
    ; keypairs: Keypair.t list }

  let run_cmd t prog args = Util.run_cmd t.testnet_dir prog args

  let run_cmd_exn t prog args = Util.run_cmd_exn t.testnet_dir prog args

  let create ~logger (network_config : Network_config.t) =
    let%bind all_namespaces_str =
      Util.run_cmd_exn "/" "kubectl"
        ["get"; "namespaces"; "-ojsonpath={.items[*].metadata.name}"]
    in
    let all_namespaces = String.split ~on:' ' all_namespaces_str in
    let testnet_dir =
      network_config.coda_automation_location ^/ "terraform/testnets"
      ^/ network_config.terraform.testnet_name
    in
    let%bind () =
      if
        List.mem all_namespaces network_config.terraform.testnet_name
          ~equal:String.equal
      then
        let%bind () =
          if network_config.debug_arg then
            Util.prompt_continue
              "Existing namespace of same name detected, pausing startup. \
               Enter [y/Y] to continue on and remove existing namespace, \
               start clean, and run the test; press Cntrl-C to quit out: "
          else
            Deferred.return
              ([%log info]
                 "Existing namespace of same name detected; removing to start \
                  clean")
        in
        Util.run_cmd_exn "/" "kubectl"
          ["delete"; "namespace"; network_config.terraform.testnet_name]
        >>| Fn.const ()
      else return ()
    in
    let%bind () =
      if%bind File_system.dir_exists testnet_dir then (
        [%log info] "Old terraform directory found; removing to start clean" ;
        File_system.remove_dir testnet_dir )
      else return ()
    in
    [%log info] "Writing network configuration" ;
    let%bind () = Unix.mkdir testnet_dir in
    (* TODO: prebuild genesis proof and ledger *)
    (*
    let%bind inputs =
      Genesis_ledger_helper.Genesis_proof.generate_inputs ~proof_level ~ledger
        ~constraint_constants ~genesis_constants
    in
    let%bind (_, genesis_proof_filename) =
      Genesis_ledger_helper.Genesis_proof.load_or_generate ~logger ~genesis_dir 
        inputs
    in
    *)
    Out_channel.with_file ~fail_if_exists:true (testnet_dir ^/ "daemon.json")
      ~f:(fun ch ->
        network_config.runtime_config
        |> Runtime_config.to_yojson
        |> Yojson.Safe.to_string
        |> Out_channel.output_string ch) ;
    Out_channel.with_file ~fail_if_exists:true (testnet_dir ^/ "main.tf.json")
      ~f:(fun ch ->
        Network_config.to_terraform network_config
        |> Terraform.to_string
        |> Out_channel.output_string ch ) ;
    let testnet_log_filter =
      Network_config.testnet_log_filter network_config
    in
    let cons_node pod_id container_id network_keypair_opt =
      { Kubernetes_network.Node.testnet_name=
          network_config.terraform.testnet_name
      ; cluster= cluster_id
      ; namespace= network_config.terraform.testnet_name
      ; pod_id
      ; container_id
      ; graphql_enabled= network_config.terraform.deploy_graphql_ingress
      ; network_keypair= network_keypair_opt }
    in
    (* we currently only deploy 1 seed and coordinator per deploy (will be configurable later) *)
    let seed_nodes = [cons_node "seed" "coda" None] in
    let snark_coordinator_name =
      "snark-coordinator-"
      ^ String.lowercase
          (String.sub network_config.terraform.snark_worker_public_key
             ~pos:
               ( String.length network_config.terraform.snark_worker_public_key
               - 6 )
             ~len:6)
    in
    let snark_coordinator_nodes =
      if network_config.terraform.snark_worker_replicas > 0 then
        [cons_node snark_coordinator_name "coordinator" None]
      else []
    in
    let block_producer_nodes =
      List.map network_config.terraform.block_producer_configs
        ~f:(fun bp_config ->
          cons_node bp_config.name "coda" (Some bp_config.keypair) )
    in
    let archive_nodes =
      List.init network_config.terraform.archive_node_count ~f:(fun i ->
          cons_node (sprintf "archive-%d" (i + 1)) "archive" None )
    in
    let nodes_by_app_id =
      let all_nodes =
        seed_nodes @ snark_coordinator_nodes @ block_producer_nodes
        @ archive_nodes
      in
      all_nodes
      |> List.map ~f:(fun node -> (node.pod_id, node))
      |> String.Map.of_alist_exn
    in
    let t =
      { logger
      ; cluster= cluster_id
      ; namespace= network_config.terraform.testnet_name
      ; testnet_dir
      ; testnet_log_filter
      ; constants= network_config.constants
      ; seed_nodes
      ; block_producer_nodes
      ; snark_coordinator_nodes
      ; archive_nodes
      ; nodes_by_app_id
      ; deployed= false
      ; keypairs=
          List.map network_config.keypairs ~f:(fun {keypair; _} -> keypair) }
    in
    [%log info] "Initializing terraform" ;
    let%bind _ = run_cmd_exn t "terraform" ["init"] in
    let%map _ = run_cmd_exn t "terraform" ["validate"] in
    t

  let deploy t =
    if t.deployed then failwith "network already deployed" ;
    [%log' info t.logger] "Deploying network" ;
    let%map _ = run_cmd_exn t "terraform" ["apply"; "-auto-approve"] in
    t.deployed <- true ;
    let result =
      { Kubernetes_network.namespace= t.namespace
      ; constants= t.constants
      ; seeds= t.seed_nodes
      ; block_producers= t.block_producer_nodes
      ; snark_coordinators= t.snark_coordinator_nodes
      ; archive_nodes= t.archive_nodes
      ; nodes_by_app_id= t.nodes_by_app_id
      ; testnet_log_filter= t.testnet_log_filter
      ; keypairs= t.keypairs }
    in
    let nodes_to_string =
      Fn.compose (String.concat ~sep:", ")
        (List.map ~f:Kubernetes_network.Node.id)
    in
    [%log' info t.logger] "Network deployed" ;
    [%log' info t.logger] "testnet namespace: %s" t.namespace ;
    [%log' info t.logger] "snark coordinators: %s"
      (nodes_to_string result.snark_coordinators) ;
    [%log' info t.logger] "block producers: %s"
      (nodes_to_string result.block_producers) ;
    [%log' info t.logger] "archive nodes: %s"
      (nodes_to_string result.archive_nodes) ;
    result

  let destroy t =
    [%log' info t.logger] "Destroying network" ;
    if not t.deployed then failwith "network not deployed" ;
    let%bind _ = run_cmd_exn t "terraform" ["destroy"; "-auto-approve"] in
    t.deployed <- false ;
    Deferred.unit

  let cleanup t =
    let%bind () = if t.deployed then destroy t else return () in
    [%log' info t.logger] "Cleaning up network configuration" ;
    let%bind () = File_system.remove_dir t.testnet_dir in
    Deferred.unit
end
