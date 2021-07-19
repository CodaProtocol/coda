open Base

module Envs = struct
  let base_node_envs =
    [ ("DAEMON_REST_PORT", "3085")
    ; ("DAEMON_CLIENT_PORT", "8301")
    ; ("DAEMON_METRICS_PORT", "10001")
    ; ("CODA_PRIVKEY_PASS", "naughty blue worm")
    ; ("CODA_LIBP2P_PASS", "")
    ]

  let snark_coord_envs ~snark_coordinator_key ~snark_worker_fee =
    [ ("CODA_SNARK_KEY", snark_coordinator_key)
    ; ("CODA_SNARK_FEE", snark_worker_fee)
    ; ("WORK_SELECTION", "seq")
    ]
    @ base_node_envs

  let postgres_envs ~username ~password ~database ~port =
    [ ("BITNAMI_DEBUG", "false")
    ; ("POSTGRES_USER", username)
    ; ("POSTGRES_PASSWORD", password)
    ; ("POSTGRES_DB", database)
    ; ("POSTGRESQL_PORT_NUMBER", port)
    ; ("POSTGRESQL_ENABLE_LDAP", "no")
    ; ("POSTGRESQL_ENABLE_TLS", "no")
    ; ("POSTGRESQL_LOG_HOSTNAME", "false")
    ; ("POSTGRESQL_LOG_CONNECTIONS", "false")
    ; ("POSTGRESQL_LOG_DISCONNECTIONS", "false")
    ; ("POSTGRESQL_PGAUDIT_LOG_CATALOG", "off")
    ; ("POSTGRESQL_CLIENT_MIN_MESSAGES", "error")
    ; ("POSTGRESQL_SHARED_PRELOAD_LIBRARIES", "pgaudit")
    ; ("POSTGRES_HOST_AUTH_METHOD", "trust")
    ]
end

module Volumes = struct
  module Runtime_config = struct
    let name = "runtime_config"

    let container_mount_target = "/root/" ^ name
  end
end

module Cmd = struct
  module Cli_args = struct
    module Log_level = struct
      type t = Debug

      let to_string t = match t with Debug -> "Debug"
    end

    module Peer = struct
      type t = string

      let default =
        "/dns4/seed/tcp/10401/p2p/12D3KooWCoGWacXE4FRwAX8VqhnWVKhz5TTEecWEuGmiNrDt2XLf"
    end

    module Postgres_uri = struct
      type t =
        { username : string
        ; password : string
        ; host : string
        ; port : string
        ; db : string
        }

      let create ~host =
        { username = "postgres"
        ; password = "password"
        ; host
        ; port = "5432"
        ; db = "archive"
        }

      (* Hostname should be dynamic based on the container ID in runtime. Ignore this field for default binding *)
      let default =
        { username = "postgres"
        ; password = "password"
        ; host = ""
        ; port = "5432"
        ; db = "archive"
        }

      let to_string t =
        Printf.sprintf "postgres://%s:%s@%s:%s/%s" t.username t.password t.host
          t.port t.db
    end

    module Proof_level = struct
      type t = Full

      let to_string t = match t with Full -> "Full"
    end
  end

  open Cli_args

  module Base = struct
    type t =
      { peer : Peer.t
      ; log_level : Log_level.t
      ; log_snark_work_gossip : bool
      ; log_txn_pool_gossip : bool
      ; generate_genesis_proof : bool
      ; client_port : string
      ; rest_port : string
      ; metrics_port : string
      ; config_file : string
      }

    let default ~config_file =
      { peer = Peer.default
      ; log_level = Log_level.Debug
      ; log_snark_work_gossip = true
      ; log_txn_pool_gossip = true
      ; generate_genesis_proof = true
      ; client_port = "8301"
      ; rest_port = "3085"
      ; metrics_port = "10001"
      ; config_file
      }

    let to_string t =
      [ "-config-file"
      ; t.config_file
      ; "-log-level"
      ; Log_level.to_string t.log_level
      ; "-log-snark-work-gossip"
      ; Bool.to_string t.log_snark_work_gossip
      ; "-log-txn-pool-gossip"
      ; Bool.to_string t.log_txn_pool_gossip
      ; "-generate-genesis-proof"
      ; Bool.to_string t.generate_genesis_proof
      ; "-client-port"
      ; t.client_port
      ; "-rest-port"
      ; t.rest_port
      ; "-metrics-port"
      ; t.metrics_port
      ; "-peer"
      ; Peer.default
      ; "-log-json"
      ; "--insecure-rest-server"
      ]

    let default_cmd ~config_file = default ~config_file |> to_string
  end

  module Seed = struct
    let cmd ~config_file = [ "daemon"; "-seed" ] @ Base.default_cmd ~config_file

    let connect_to_archive ~archive_node = [ "-archive-address"; archive_node ]
  end

  module Block_producer = struct
    type t =
      { block_producer_key : string
      ; enable_flooding : bool
      ; enable_peer_exchange : bool
      }

    let default ~private_key_config =
      { block_producer_key = private_key_config
      ; enable_flooding = true
      ; enable_peer_exchange = true
      }

    let cmd t ~config_file =
      [ "daemon"
      ; "-block-producer-key"
      ; t.block_producer_key
      ; "-enable-flooding"
      ; Bool.to_string t.enable_flooding
      ; "-enable-peer-exchange"
      ; Bool.to_string t.enable_peer_exchange
      ]
      @ Base.default_cmd ~config_file
  end

  module Snark_coordinator = struct
    type t =
      { snark_coordinator_key : string
      ; snark_worker_fee : string
      ; work_selection : string
      }

    let default ~snark_coordinator_key ~snark_worker_fee =
      { snark_coordinator_key; snark_worker_fee; work_selection = "seq" }

    let cmd t ~config_file =
      [ "daemon"
      ; "-run-snark-coordinator"
      ; t.snark_coordinator_key
      ; "-snark-worker-fee"
      ; t.snark_worker_fee
      ; "-work-selection"
      ; t.work_selection
      ]
      @ Base.default_cmd ~config_file
  end

  module Snark_worker = struct
    let name = "snark-worker"

    type t =
      { daemon_address : string
      ; daemon_port : string
      ; proof_level : Proof_level.t
      }

    let default ~daemon_address ~daemon_port =
      { daemon_address; daemon_port; proof_level = Proof_level.Full }

    let cmd t ~config_file =
      [ "internal"
      ; "snark-worker"
      ; "-proof-level"
      ; Proof_level.to_string t.proof_level
      ; "-daemon-address"
      ; t.daemon_address ^ ":" ^ t.daemon_port
      ]
      @ Base.default_cmd ~config_file
  end

  module Archive_node = struct
    type t = { postgres_uri : string; server_port : string }

    let default =
      { postgres_uri = Postgres_uri.(default |> to_string)
      ; server_port = "3086"
      }

    let create postgres_uri = { postgres_uri; server_port = "3086" }

    let cmd t ~config_file =
      [ "coda-archive"
      ; "run"
      ; "-postgres-uri"
      ; t.postgres_uri
      ; "-server-port"
      ; t.server_port
      ; "-config-file"
      ; config_file
      ]
  end

  type t =
    | Seed
    | Block_producer of Block_producer.t
    | Snark_coordinator of Snark_coordinator.t
    | Snark_worker of Snark_worker.t
    | Archive_node of Archive_node.t

  let create_cmd t ~config_file =
    match t with
    | Seed ->
        Seed.cmd ~config_file
    | Block_producer args ->
        Block_producer.cmd args ~config_file
    | Snark_coordinator args ->
        Snark_coordinator.cmd args ~config_file
    | Snark_worker args ->
        Snark_worker.cmd args ~config_file
    | Archive_node args ->
        Archive_node.cmd args ~config_file
end

module Services = struct
  module Seed = struct
    let name = "seed"

    let env = Envs.base_node_envs

    let cmd = Cmd.Seed
  end

  module Block_producer = struct
    let name = "block-producer"

    let secret_name = "keypair"

    let env = Envs.base_node_envs

    let cmd args = Cmd.Block_producer args
  end

  module Snark_coordinator = struct
    let name = "snark-coordinator"

    let default_port = "8301"

    let env = Envs.snark_coord_envs

    let cmd args = Cmd.Snark_coordinator args
  end

  module Snark_worker = struct
    let name = "snark-worker"

    let env = []

    let cmd args = Cmd.Snark_worker args
  end

  module Archive_node = struct
    let name = "archive"

    let postgres_name = "postgres"

    let server_port = "3086"

    let envs = Envs.base_node_envs

    let cmd args = Cmd.Archive_node args

    let entrypoint_target = "/docker-entrypoint-initdb.d"
  end
end
