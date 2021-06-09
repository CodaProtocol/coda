type delegation_config = Block_producer of int | Additional_account of int

module Container_images = struct
  type t =
    { coda: string
    ; archive_node: string
    ; user_agent: string
    ; bots: string
    ; points: string }
end

module Account_config = struct
  type t =
    { balance: string
    ; timing: Mina_base.Account_timing.t
    ; delegate: delegation_config option }
end

type constants =
  { constraints: Genesis_constants.Constraint_constants.t
  ; genesis: Genesis_constants.t }
[@@deriving to_yojson]

type t =
  { (* temporary flag to enable/disable graphql ingress deployments *)
    requires_graphql: bool
  ; k: int
  ; delta: int
  ; slots_per_epoch: int
  ; slots_per_sub_window: int
  ; proof_level: Runtime_config.Proof_keys.Level.t
  ; txpool_max_size: int
  ; block_producers: Account_config.t list
  ; additional_accounts: Account_config.t list
  ; num_snark_workers: int
  ; num_archive_nodes: int
  ; log_precomputed_blocks: bool
  ; snark_worker_fee: string
  ; snark_worker_public_key: string }

let default =
  { requires_graphql= false
  ; k= 20
  ; slots_per_epoch= 3 * 8 * 20
  ; slots_per_sub_window= 2
  ; delta= 0
  ; proof_level= Full
  ; txpool_max_size= 3000
  ; block_producers= []
  ; additional_accounts= []
  ; num_snark_workers= 0
  ; num_archive_nodes= 0
  ; log_precomputed_blocks= false
  ; snark_worker_fee= "0.025"
  ; snark_worker_public_key=
      (let pk, _ = (Lazy.force Mina_base.Sample_keypairs.keypairs).(0) in
       Signature_lib.Public_key.Compressed.to_string pk) }
