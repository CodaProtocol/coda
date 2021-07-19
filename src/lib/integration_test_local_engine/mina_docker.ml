open Core
open Async
open Currency
open Signature_lib
open Mina_base
open Integration_test_lib

let docker_swarm_version = "3.9"

let postgres_image = "docker.io/bitnami/postgresql"

let mina_archive_schema =
  "https://raw.githubusercontent.com/MinaProtocol/mina/develop/src/app/archive/create_schema.sql"

let mina_create_schema = "create_schema.sql"

module Network_config = struct
  module Cli_inputs = Cli_inputs

  module Port = struct
    type t = string list

    let get_new_port t =
      match t with [] -> [ 7000 ] | x :: xs -> [ x + 1 ] @ [ x ] @ xs

    let get_latest_port t = match t with [] -> 7000 | x :: _ -> x

    let create_rest_port t =
      match t with [] -> "7000:3085" | x :: _ -> Int.to_string x ^ ":3085"
  end

  type docker_volume_configs = { name : string; data : string }
  [@@deriving to_yojson]

  type seed_config = { name : string; ports : (string * string) list }
  [@@deriving to_yojson]

  type block_producer_config =
    { name : string
    ; id : string
    ; keypair : Network_keypair.t
    ; public_key : string
    ; private_key : string
    ; keypair_secret : string
    ; libp2p_secret : string
    ; ports : (string * string) list
    }
  [@@deriving to_yojson]

  type snark_coordinator_configs =
    { name : string
    ; id : string
    ; public_key : string
    ; snark_worker_fee : string
    ; ports : (string * string) list
    }
  [@@deriving to_yojson]

  type archive_node_configs =
    { name : string
    ; id : string
    ; schema : string
    ; ports : (string * string) list
    }
  [@@deriving to_yojson]

  type docker_config =
    { docker_swarm_version : string
    ; stack_name : string
    ; mina_image : string
    ; mina_archive_image : string
    ; docker_volume_configs : docker_volume_configs list
    ; seed_configs : seed_config list
    ; block_producer_configs : block_producer_config list
    ; snark_coordinator_configs : snark_coordinator_configs list
    ; archive_node_configs : archive_node_configs list
    ; log_precomputed_blocks : bool
    ; archive_node_count : int
    ; mina_archive_schema : string
    ; runtime_config : Yojson.Safe.t
          [@to_yojson fun j -> `String (Yojson.Safe.to_string j)]
    }
  [@@deriving to_yojson]

  type t =
    { keypairs : Network_keypair.t list
    ; debug_arg : bool
    ; constants : Test_config.constants
    ; docker : docker_config
    }
  [@@deriving to_yojson]

  let expand ~logger ~test_name ~cli_inputs:_ ~(debug : bool)
      ~(test_config : Test_config.t) ~(images : Test_config.Container_images.t)
      =
    let { Test_config.k
        ; delta
        ; slots_per_epoch
        ; slots_per_sub_window
        ; proof_level
        ; txpool_max_size
        ; requires_graphql = _
        ; block_producers
        ; num_snark_workers
        ; num_archive_nodes
        ; log_precomputed_blocks
        ; snark_worker_fee
        ; snark_worker_public_key
        } =
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
    let stack_name = "it-" ^ user ^ "-" ^ git_commit ^ "-" ^ test_name in
    (* GENERATE ACCOUNTS AND KEYPAIRS *)
    let num_block_producers = List.length block_producers in
    let block_producer_keypairs, runtime_accounts =
      (* the first keypair is the genesis winner and is assumed to be untimed. Therefore dropping it, and not assigning it to any block producer *)
      let keypairs =
        List.drop (Array.to_list (Lazy.force Sample_keypairs.keypairs)) 1
      in
      if num_block_producers > List.length keypairs then
        failwith
          "not enough sample keypairs for specified number of block producers" ;
      let f index ({ Test_config.Block_producer.balance; timing }, (pk, sk)) =
        let runtime_account =
          let timing =
            match timing with
            | Account.Timing.Untimed ->
                None
            | Timed t ->
                Some
                  { Runtime_config.Accounts.Single.Timed.initial_minimum_balance =
                      t.initial_minimum_balance
                  ; cliff_time = t.cliff_time
                  ; cliff_amount = t.cliff_amount
                  ; vesting_period = t.vesting_period
                  ; vesting_increment = t.vesting_increment
                  }
          in
          let default = Runtime_config.Accounts.Single.default in
          { default with
            pk = Some (Public_key.Compressed.to_string pk)
          ; sk = None
          ; balance =
              Balance.of_formatted_string balance
              (* delegation currently unsupported *)
          ; delegate = None
          ; timing
          }
        in
        let secret_name = "test-keypair-" ^ Int.to_string index in
        let keypair =
          { Keypair.public_key = Public_key.decompress_exn pk
          ; private_key = sk
          }
        in
        ( Network_keypair.create_network_keypair ~keypair ~secret_name
        , runtime_account )
      in
      List.mapi ~f
        (List.zip_exn block_producers
           (List.take keypairs (List.length block_producers)))
      |> List.unzip
    in
    (* DAEMON CONFIG *)
    let proof_config =
      (* TODO: lift configuration of these up Test_config.t *)
      { Runtime_config.Proof_keys.level = Some proof_level
      ; sub_windows_per_window = None
      ; ledger_depth = None
      ; work_delay = None
      ; block_window_duration_ms = None
      ; transaction_capacity = None
      ; coinbase_amount = None
      ; supercharged_coinbase_factor = None
      ; account_creation_fee = None
      ; fork = None
      }
    in
    let constraint_constants =
      Genesis_ledger_helper.make_constraint_constants
        ~default:Genesis_constants.Constraint_constants.compiled proof_config
    in
    let runtime_config =
      { Runtime_config.daemon =
          Some { txpool_max_size = Some txpool_max_size; peer_list_url = None }
      ; genesis =
          Some
            { k = Some k
            ; delta = Some delta
            ; slots_per_epoch = Some slots_per_epoch
            ; slots_per_sub_window = Some slots_per_sub_window
            ; genesis_state_timestamp =
                Some Core.Time.(to_string_abs ~zone:Zone.utc (now ()))
            }
      ; proof =
          None
          (* was: Some proof_config; TODO: prebake ledger and only set hash *)
      ; ledger =
          Some
            { base = Accounts runtime_accounts
            ; add_genesis_winner = None
            ; num_accounts = None
            ; balances = []
            ; hash = None
            ; name = None
            }
      ; epoch_data = None
      }
    in
    let genesis_constants =
      Or_error.ok_exn
        (Genesis_ledger_helper.make_genesis_constants ~logger
           ~default:Genesis_constants.compiled runtime_config)
    in
    let constants : Test_config.constants =
      { constraints = constraint_constants; genesis = genesis_constants }
    in
    let open Docker_node_config.Services in
    let available_host_ports_ref = ref [] in
    (* BLOCK PRODUCER CONFIG *)
    let block_producer_config index keypair =
      available_host_ports_ref := Port.get_new_port !available_host_ports_ref ;
      let rest_port = Port.create_rest_port !available_host_ports_ref in
      { name = Block_producer.name ^ "-" ^ Int.to_string (index + 1)
      ; id = Int.to_string index
      ; keypair
      ; keypair_secret = keypair.secret_name
      ; public_key = keypair.public_key_file
      ; private_key = keypair.private_key_file
      ; libp2p_secret = ""
      ; ports = [ ("rest-port", rest_port) ]
      }
    in
    let block_producer_configs =
      List.mapi block_producer_keypairs ~f:block_producer_config
    in
    (* SEED NODE CONFIG *)
    available_host_ports_ref := Port.get_new_port !available_host_ports_ref ;
    let seed_rest_port = Port.create_rest_port !available_host_ports_ref in
    let seed_configs =
      [ { name = Seed.name; ports = [ ("rest-port", seed_rest_port) ] } ]
    in
    (* SNARK COORDINATOR CONFIG *)
    available_host_ports_ref := Port.get_new_port !available_host_ports_ref ;
    let snark_coord_rest_port =
      Port.create_rest_port !available_host_ports_ref
    in
    let snark_coordinator_configs =
      if num_snark_workers > 0 then
        List.mapi
          (List.init num_snark_workers ~f:(const 0))
          ~f:(fun index _ ->
            available_host_ports_ref :=
              Port.get_new_port !available_host_ports_ref ;
            let rest_port = Port.create_rest_port !available_host_ports_ref in
            { name = Snark_worker.name ^ "-" ^ Int.to_string (index + 1)
            ; id = Int.to_string index
            ; snark_worker_fee
            ; public_key = snark_worker_public_key
            ; ports = [ ("rest_port", rest_port) ]
            })
        (* Add one snark coordinator for all workers *)
        |> List.append
             [ { name = Snark_coordinator.name
               ; id = "1"
               ; snark_worker_fee
               ; public_key = snark_worker_public_key
               ; ports = [ ("rest-port", snark_coord_rest_port) ]
               }
             ]
      else []
    in
    (* ARCHIVE CONFIG *)
    let archive_node_configs =
      if num_archive_nodes > 0 then
        List.mapi
          (List.init num_archive_nodes ~f:(const 0))
          ~f:(fun index _ ->
            available_host_ports_ref :=
              Port.get_new_port !available_host_ports_ref ;
            let rest_port = Port.create_rest_port !available_host_ports_ref in
            { name = Archive_node.name ^ "-" ^ Int.to_string (index + 1)
            ; id = Int.to_string index
            ; schema = mina_archive_schema
            ; ports = [ ("rest-port", rest_port) ]
            })
      else []
    in
    (* DOCKER VOLUME CONFIG:
       Create a docker volume structure for the runtime_config and all block producer keys
       to mount in the specified docker services
    *)
    let docker_volume_configs =
      List.map block_producer_configs ~f:(fun config ->
          { name = "sk-" ^ config.name; data = config.private_key })
      @ [ { name = Docker_node_config.Volumes.Runtime_config.name
          ; data =
              Yojson.Safe.to_string (Runtime_config.to_yojson runtime_config)
          }
        ]
    in
    { debug_arg = debug
    ; keypairs = block_producer_keypairs
    ; constants
    ; docker =
        { docker_swarm_version
        ; stack_name
        ; mina_image = images.coda
        ; mina_archive_image = images.archive_node
        ; docker_volume_configs
        ; runtime_config = Runtime_config.to_yojson runtime_config
        ; seed_configs
        ; block_producer_configs
        ; snark_coordinator_configs
        ; archive_node_configs
        ; log_precomputed_blocks
        ; archive_node_count = num_archive_nodes
        ; mina_archive_schema
        }
    }

  (*
     Composes a docker_compose.json file from the network_config specification and writes to disk. This docker_compose
     file contains docker service definitions for each node in the local network. Each node service has different
     configurations which are specified as commands, environment variables, and docker bind volumes.
     We start by creating a runtime config volume to mount to each node service as a bind volume and then continue to create each
     node service. As we create each definition for a service, we specify the docker command, volume, and environment varibles to 
     be used (which are mostly defaults).
  *)
  let to_docker network_config =
    let open Docker_compose.Compose in
    let open Docker_node_config in
    let open Docker_node_config.Services in
    (* RUNTIME CONFIG DOCKER BIND VOLUME *)
    let runtime_config =
      Service.Volume.create Volumes.Runtime_config.name
        Volumes.Runtime_config.container_mount_target
    in
    (* BLOCK PRODUCER DOCKER SERVICE *)
    let block_producer_map =
      List.map network_config.docker.block_producer_configs ~f:(fun config ->
          (* BP KEYPAIR CONFIG DOCKER BIND VOLUME *)
          let private_key_config_name = "sk-" ^ config.name in
          let private_key_config =
            Service.Volume.create private_key_config_name
              ("/root/" ^ private_key_config_name)
          in
          let cmd =
            Cmd.(
              Block_producer
                (Block_producer.default
                   ~private_key_config:private_key_config.target))
          in
          let rest_port =
            List.find
              ~f:(fun ports -> String.equal (fst ports) "rest-port")
              config.ports
            |> Option.value_exn |> snd
          in
          ( config.name
          , { Service.image = network_config.docker.mina_image
            ; volumes = [ private_key_config; runtime_config ]
            ; command = Cmd.create_cmd cmd ~config_file:runtime_config.target
            ; ports = [ rest_port ]
            ; environment = Service.Environment.create Envs.base_node_envs
            } ))
      |> StringMap.of_alist_exn
    in
    (* SNARK COORD/WORKER DOCKER SERVICE *)
    let snark_worker_map =
      List.map network_config.docker.snark_coordinator_configs ~f:(fun config ->
          (* Assign different command and environment configs depending on coordinator or worker *)
          let command, environment =
            match
              String.substr_index config.name ~pattern:Snark_coordinator.name
            with
            | Some _ ->
                let cmd =
                  Cmd.(
                    Snark_coordinator
                      (Snark_coordinator.default
                         ~snark_coordinator_key:config.public_key
                         ~snark_worker_fee:config.snark_worker_fee))
                in
                let coordinator_command =
                  Cmd.create_cmd cmd ~config_file:runtime_config.target
                in
                let coordinator_environment =
                  Service.Environment.create
                    (Envs.snark_coord_envs
                       ~snark_coordinator_key:config.public_key
                       ~snark_worker_fee:config.snark_worker_fee)
                in
                (coordinator_command, coordinator_environment)
            | None ->
                let daemon_address = Snark_coordinator.name in
                let daemon_port = Snark_coordinator.default_port in
                let cmd =
                  Cmd.(
                    Snark_worker
                      (Snark_worker.default ~daemon_address ~daemon_port))
                in
                let worker_command =
                  Cmd.create_cmd cmd ~config_file:runtime_config.target
                in
                let worker_environment = Service.Environment.create [] in
                (worker_command, worker_environment)
          in
          let rest_port =
            List.find
              ~f:(fun ports -> String.equal (fst ports) "rest-port")
              config.ports
            |> Option.value_exn |> snd
          in
          ( config.name
          , { Service.image = network_config.docker.mina_image
            ; volumes = [ runtime_config ]
            ; command
            ; ports = [ rest_port ]
            ; environment
            } ))
      |> StringMap.of_alist_exn
    in
    (* ARCHIVE NODE SERVICE *)
    let archive_node_configs =
      List.mapi network_config.docker.archive_node_configs
        ~f:(fun index config ->
          let postgres_uri =
            Cmd.Cli_args.Postgres_uri.create
              ~host:
                ( network_config.docker.stack_name ^ "_"
                ^ Archive_node.postgres_name ^ "-"
                ^ Int.to_string (index + 1) )
            |> Cmd.Cli_args.Postgres_uri.to_string
          in
          let cmd =
            let server_port = Archive_node.server_port in
            Cmd.(Archive_node { Archive_node.postgres_uri; server_port })
          in
          let rest_port =
            List.find
              ~f:(fun ports -> String.equal (fst ports) "rest-port")
              config.ports
            |> Option.value_exn |> snd
          in
          ( config.name
          , { Service.image = network_config.docker.mina_archive_image
            ; volumes = [ runtime_config ]
            ; command = Cmd.create_cmd cmd ~config_file:runtime_config.target
            ; ports = [ rest_port ]
            ; environment = Service.Environment.create Envs.base_node_envs
            } ))
      (* Add an equal number of postgres containers as archive nodes *)
      @ List.mapi network_config.docker.archive_node_configs ~f:(fun index _ ->
            let pg_config = Cmd.Cli_args.Postgres_uri.default in
            (* Mount the archive schema on the /docker-entrypoint-initdb.d postgres entrypoint *)
            let archive_config =
              Service.Volume.create mina_create_schema
                (Archive_node.entrypoint_target ^/ mina_create_schema)
            in
            ( Archive_node.postgres_name ^ "-" ^ Int.to_string (index + 1)
            , { Service.image = postgres_image
              ; volumes = [ archive_config ]
              ; command = []
              ; ports = []
              ; environment =
                  Service.Environment.create
                    (Envs.postgres_envs ~username:pg_config.username
                       ~password:pg_config.password ~database:pg_config.db
                       ~port:pg_config.port)
              } ))
      |> StringMap.of_alist_exn
    in
    (* SEED NODE DOCKER SERVICE *)
    let seed_map =
      List.mapi network_config.docker.seed_configs ~f:(fun index config ->
          let command =
            if List.length network_config.docker.archive_node_configs > 0 then
              (* If an archive node is specified in the test plan, use the seed node to connect to the first mina-archive process *)
              Cmd.create_cmd Seed ~config_file:runtime_config.target
              @ Cmd.Seed.connect_to_archive
                  ~archive_node:
                    ( "archive-"
                    ^ Int.to_string (index + 1)
                    ^ ":" ^ Services.Archive_node.server_port )
            else Cmd.create_cmd Seed ~config_file:runtime_config.target
          in
          let rest_port =
            List.find
              ~f:(fun ports -> String.equal (fst ports) "rest-port")
              config.ports
            |> Option.value_exn |> snd
          in
          ( Seed.name
          , { Service.image = network_config.docker.mina_image
            ; volumes = [ runtime_config ]
            ; command
            ; ports = [ rest_port ]
            ; environment = Service.Environment.create Envs.base_node_envs
            } ))
      |> StringMap.of_alist_exn
    in
    let services =
      seed_map |> merge block_producer_map |> merge snark_worker_map
      |> merge archive_node_configs
    in
    { version = docker_swarm_version; services }
end

module Network_manager = struct
  type t =
    { stack_name : string
    ; logger : Logger.t
    ; testnet_dir : string
    ; testnet_log_filter : string
    ; constants : Test_config.constants
    ; seed_nodes : Swarm_network.Node.t list
    ; nodes_by_app_id : Swarm_network.Node.t String.Map.t
    ; block_producer_nodes : Swarm_network.Node.t list
    ; snark_coordinator_nodes : Swarm_network.Node.t list
    ; archive_node_nodes : Swarm_network.Node.t list
    ; mutable deployed : bool
    ; keypairs : Keypair.t list
    }

  let run_cmd t prog args = Util.run_cmd t.testnet_dir prog args

  let run_cmd_exn t prog args = Util.run_cmd_exn t.testnet_dir prog args

  (*
      Creates a docker swarm network by creating a local working directory and writing a
      docker_compose.json file to disk with all related resources (e.g. runtime_config and secret keys).

      First, check if there is a running swarm network and let the user remove it. Continue by creating a
      working directory and writing out all resources to be used as docker volumes. Additionally for each resource, specify
      file permissions of 600 so that the daemon can run properly.
  *)
  let create ~logger (network_config : Network_config.t) =
    let%bind all_stacks_str =
      Util.run_cmd_exn "/" "docker" [ "stack"; "ls"; "--format"; "{{.Name}}" ]
    in
    let all_stacks = String.split ~on:'\n' all_stacks_str in
    let testnet_dir = network_config.docker.stack_name in
    let%bind () =
      if
        List.mem all_stacks network_config.docker.stack_name ~equal:String.equal
      then
        let%bind () =
          if network_config.debug_arg then
            Util.prompt_continue
              "Existing stack name of same name detected, pausing startup. \
               Enter [y/Y] to continue on and remove existing stack name, \
               start clean, and run the test; press Ctrl-C to quit out: "
          else
            Deferred.return
              ([%log info]
                 "Existing stack of same name detected; removing to start clean")
        in
        Util.run_cmd_exn "/" "docker"
          [ "stack"; "rm"; network_config.docker.stack_name ]
        >>| Fn.const ()
      else return ()
    in
    let%bind () =
      if%bind File_system.dir_exists testnet_dir then (
        [%log info] "Old docker stack directory found; removing to start clean" ;
        File_system.remove_dir testnet_dir )
      else return ()
    in
    let%bind () = Unix.mkdir testnet_dir in
    [%log info] "Writing network configuration" ;
    Out_channel.with_file ~fail_if_exists:true (testnet_dir ^/ "compose.json")
      ~f:(fun ch ->
        Network_config.to_docker network_config
        |> Docker_compose.to_string
        |> Out_channel.output_string ch) ;
    List.iter network_config.docker.docker_volume_configs ~f:(fun config ->
        [%log info] "Writing volume config: %s" (testnet_dir ^/ config.name) ;
        Out_channel.with_file ~fail_if_exists:false (testnet_dir ^/ config.name)
          ~f:(fun ch -> config.data |> Out_channel.output_string ch) ;
        ignore (Util.run_cmd_exn testnet_dir "chmod" [ "600"; config.name ])) ;
    let cons_node swarm_name service_id ports network_keypair_opt =
      { Swarm_network.Node.swarm_name
      ; service_id
      ; ports
      ; graphql_enabled = true
      ; network_keypair = network_keypair_opt
      }
    in
    let seed_nodes =
      List.map network_config.docker.seed_configs ~f:(fun seed_config ->
          cons_node network_config.docker.stack_name
            (network_config.docker.stack_name ^ "_" ^ seed_config.name)
            seed_config.ports None)
    in
    let block_producer_nodes =
      List.map network_config.docker.block_producer_configs ~f:(fun bp_config ->
          cons_node network_config.docker.stack_name
            (network_config.docker.stack_name ^ "_" ^ bp_config.name)
            bp_config.ports (Some bp_config.keypair))
    in
    let snark_coordinator_nodes =
      List.map network_config.docker.snark_coordinator_configs
        ~f:(fun snark_config ->
          cons_node network_config.docker.stack_name
            (network_config.docker.stack_name ^ "_" ^ snark_config.name)
            snark_config.ports None)
    in
    let%bind _ =
      (* If any archive nodes are specified, write the archive schema to the working directory to use as an entrypoint for postgres *)
      if List.length network_config.docker.archive_node_configs > 0 then
        let cmd =
          Printf.sprintf "curl -Ls %s > %s"
            network_config.docker.mina_archive_schema mina_create_schema
        in
        Util.run_cmd_exn testnet_dir "bash" [ "-c"; cmd ]
      else return ""
    in
    let archive_node_nodes =
      List.map network_config.docker.archive_node_configs
        ~f:(fun archive_node ->
          cons_node network_config.docker.stack_name
            (network_config.docker.stack_name ^ "_" ^ archive_node.name)
            archive_node.ports None)
    in
    let nodes_by_app_id =
      let all_nodes =
        seed_nodes @ block_producer_nodes @ snark_coordinator_nodes
        @ archive_node_nodes
      in
      all_nodes
      |> List.map ~f:(fun node -> (node.service_id, node))
      |> String.Map.of_alist_exn
    in
    let t =
      { stack_name = network_config.docker.stack_name
      ; logger
      ; testnet_dir
      ; constants = network_config.constants
      ; seed_nodes
      ; block_producer_nodes
      ; snark_coordinator_nodes
      ; archive_node_nodes
      ; nodes_by_app_id
      ; deployed = false
      ; testnet_log_filter = ""
      ; keypairs =
          List.map network_config.keypairs ~f:(fun { keypair; _ } -> keypair)
      }
    in
    Deferred.return t

  let deploy t =
    if t.deployed then failwith "network already deployed" ;
    [%log' info t.logger] "Deploying network" ;
    [%log' info t.logger] "Stack_name in deploy: %s" t.stack_name ;
    let%map _ =
      run_cmd_exn t "docker"
        [ "stack"; "deploy"; "-c"; "compose.json"; t.stack_name ]
    in
    t.deployed <- true ;
    let result =
      { Swarm_network.namespace = t.stack_name
      ; constants = t.constants
      ; seeds = t.seed_nodes
      ; block_producers = t.block_producer_nodes
      ; snark_coordinators = t.snark_coordinator_nodes
      ; archive_nodes = t.archive_node_nodes
      ; nodes_by_app_id = t.nodes_by_app_id
      ; testnet_log_filter = t.testnet_log_filter
      ; keypairs = t.keypairs
      }
    in
    let nodes_to_string =
      Fn.compose (String.concat ~sep:", ") (List.map ~f:Swarm_network.Node.id)
    in
    [%log' info t.logger] "Network deployed" ;
    [%log' info t.logger] "testnet swarm: %s" t.stack_name ;
    [%log' info t.logger] "seed nodes: %s" (nodes_to_string result.seeds) ;
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
    let%bind _ = run_cmd_exn t "docker" [ "stack"; "rm"; t.stack_name ] in
    t.deployed <- false ;
    Deferred.unit

  let cleanup t =
    let%bind () = if t.deployed then destroy t else return () in
    [%log' info t.logger] "Cleaning up network configuration" ;
    let%bind () = File_system.remove_dir t.testnet_dir in
    Deferred.unit
end
