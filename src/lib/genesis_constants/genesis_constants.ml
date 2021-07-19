[%%import "/src/config.mlh"]

open Core_kernel

module Proof_level = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Full | Check | None [@@deriving equal]

      let to_latest = Fn.id
    end
  end]

  let to_string = function Full -> "full" | Check -> "check" | None -> "none"

  let of_string = function
    | "full" ->
        Full
    | "check" ->
        Check
    | "none" ->
        None
    | s ->
        failwithf "unrecognised proof level %s" s ()

  [%%inject "compiled", proof_level]

  let compiled = of_string compiled

  let for_unit_tests = Check
end

module Fork_constants = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { previous_state_hash : Pickles.Backend.Tick.Field.Stable.V1.t
        ; previous_length : Mina_numbers.Length.Stable.V1.t
        ; previous_global_slot : Mina_numbers.Global_slot.Stable.V1.t
        }
      [@@deriving sexp, equal, compare, yojson]

      let to_latest = Fn.id
    end
  end]
end

(** Constants that affect the constraint systems for proofs (and thus also key
    generation).

    Care must be taken to ensure that these match against the proving/
    verification keys when [proof_level=Full], otherwise generated proofs will
    be invalid.
*)
module Constraint_constants = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { sub_windows_per_window : int
        ; ledger_depth : int
        ; work_delay : int
        ; block_window_duration_ms : int
        ; transaction_capacity_log_2 : int
        ; pending_coinbase_depth : int
        ; coinbase_amount : Currency.Amount.Stable.V1.t
        ; supercharged_coinbase_factor : int
        ; account_creation_fee : Currency.Fee.Stable.V1.t
        ; fork : Fork_constants.Stable.V1.t option
        ; network_id : int
        }
      [@@deriving sexp, equal, compare, yojson]

      let to_latest = Fn.id
    end
  end]

  let to_snark_keys_header (t : t) : Snark_keys_header.Constraint_constants.t =
    { sub_windows_per_window = t.sub_windows_per_window
    ; ledger_depth = t.ledger_depth
    ; work_delay = t.work_delay
    ; block_window_duration_ms = t.block_window_duration_ms
    ; transaction_capacity = Log_2 t.transaction_capacity_log_2
    ; pending_coinbase_depth = t.pending_coinbase_depth
    ; coinbase_amount = Currency.Amount.to_uint64 t.coinbase_amount
    ; supercharged_coinbase_factor = t.supercharged_coinbase_factor
    ; account_creation_fee = Currency.Fee.to_uint64 t.account_creation_fee
    ; fork =
        ( match t.fork with
        | Some { previous_length; previous_state_hash; previous_global_slot } ->
            Some
              { previous_length = Unsigned.UInt32.to_int previous_length
              ; previous_state_hash =
                  Pickles.Backend.Tick.Field.to_string previous_state_hash
              ; previous_global_slot =
                  Unsigned.UInt32.to_int previous_global_slot
              }
        | None ->
            None )
    }

  (* Generate the compile-time constraint constants, using a signature to hide
     the optcomp constants that we import.
  *)
  include (
    struct
      [%%ifdef consensus_mechanism]

      [%%inject "sub_windows_per_window", sub_windows_per_window]

      [%%else]

      (* Invalid value, this should not be used by nonconsensus nodes. *)
      let sub_windows_per_window = -1

      [%%endif]

      [%%inject "ledger_depth", ledger_depth]

      [%%inject "coinbase_amount_string", coinbase]

      [%%inject "account_creation_fee_string", account_creation_fee_int]

      (** All the proofs before the last [work_delay] blocks must be
            completed to add transactions. [work_delay] is the minimum number
            of blocks and will increase if the throughput is less.
            - If [work_delay = 0], all the work that was added to the scan
              state in the previous block is expected to be completed and
              included in the current block if any transactions/coinbase are to
              be included.
            - [work_delay >= 1] means that there's at least two block times for
              completing the proofs.
        *)

      [%%inject "work_delay", scan_state_work_delay]

      [%%inject "block_window_duration_ms", block_window_duration]

      [%%if scan_state_with_tps_goal]

      [%%inject "tps_goal_x10", scan_state_tps_goal_x10]

      let max_coinbases = 2

      (* block_window_duration is in milliseconds, so divide by 1000 divide
         by 10 again because we have tps * 10
      *)
      let max_user_commands_per_block =
        tps_goal_x10 * block_window_duration_ms / (1000 * 10)

      (** Log of the capacity of transactions per transition.
            - 1 will only work if we don't have prover fees.
            - 2 will work with prover fees, but not if we want a transaction
              included in every block.
            - At least 3 ensures a transaction per block and the staged-ledger
              unit tests pass.
        *)
      let transaction_capacity_log_2 =
        1
        + Core_kernel.Int.ceil_log2 (max_user_commands_per_block + max_coinbases)

      [%%else]

      [%%inject
      "transaction_capacity_log_2", scan_state_transaction_capacity_log_2]

      [%%endif]

      [%%inject "supercharged_coinbase_factor", supercharged_coinbase_factor]

      let pending_coinbase_depth =
        Core_kernel.Int.ceil_log2
          (((transaction_capacity_log_2 + 1) * (work_delay + 1)) + 1)

      [%%ifndef fork_previous_length]

      let fork = None

      [%%else]

      [%%inject "fork_previous_length", fork_previous_length]

      [%%inject "fork_previous_state_hash", fork_previous_state_hash]

      [%%inject "fork_previous_global_slot", fork_previous_global_slot]

      let fork =
        Some
          { Fork_constants.previous_length =
              Mina_numbers.Length.of_int fork_previous_length
          ; previous_state_hash =
              Data_hash_lib.State_hash.of_base58_check_exn
                fork_previous_state_hash
          ; previous_global_slot =
              Mina_numbers.Global_slot.of_int fork_previous_global_slot
          }

      [%%endif]

      [%%ifdef network_id]

      [%%inject "network_id", network_id]

      [%%else]

      let network_id = 0

      [%%endif]

      let compiled =
        { sub_windows_per_window
        ; ledger_depth
        ; work_delay
        ; block_window_duration_ms
        ; transaction_capacity_log_2
        ; pending_coinbase_depth
        ; coinbase_amount =
            Currency.Amount.of_formatted_string coinbase_amount_string
        ; supercharged_coinbase_factor
        ; account_creation_fee =
            Currency.Fee.of_formatted_string account_creation_fee_string
        ; fork
        ; network_id
        }
    end :
      sig
        val compiled : t
      end )

  let for_unit_tests = compiled
end

(*Constants that can be specified for generating the base proof (that are not required for key-generation) in runtime_genesis_ledger.exe and that can be configured at runtime.
The types are defined such that this module doesn't depend on any of the coda libraries (except blake2 and module_version) to avoid dependency cycles.
TODO: #4659 move key generation to runtime_genesis_ledger.exe to include scan_state constants, consensus constants (c and  block_window_duration) and ledger depth here*)

let genesis_timestamp_of_string str =
  let default_timezone = Core.Time.Zone.of_utc_offset ~hours:(-8) in
  Core.Time.of_string_gen ~if_no_timezone:(`Use_this_one default_timezone) str

let of_time t = Time.to_span_since_epoch t |> Time.Span.to_ms |> Int64.of_float

let validate_time time_str =
  match
    Result.try_with (fun () ->
        Option.value_map ~default:(Time.now ()) ~f:genesis_timestamp_of_string
          time_str)
  with
  | Ok time ->
      Ok (of_time time)
  | Error _ ->
      Error
        "Invalid timestamp. Please specify timestamp in \"%Y-%m-%d \
         %H:%M:%S%z\". For example, \"2019-01-30 12:00:00-0800\" for UTC-08:00 \
         timezone"

let genesis_timestamp_to_string time =
  Int64.to_float time |> Time.Span.of_ms |> Time.of_span_since_epoch
  |> Core.Time.to_string_iso8601_basic
       ~zone:(Core.Time.Zone.of_utc_offset ~hours:(-8))

(*Protocol constants required for consensus and snarks. Consensus constants is generated using these*)
module Protocol = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type ('length, 'delta, 'genesis_state_timestamp) t =
          { k : 'length
          ; slots_per_epoch : 'length
          ; slots_per_sub_window : 'length
          ; delta : 'delta
          ; genesis_state_timestamp : 'genesis_state_timestamp
          }
        [@@deriving equal, ord, hash, sexp, yojson, hlist, fields]
      end
    end]
  end

  [%%versioned_asserted
  module Stable = struct
    module V1 = struct
      type t = (int, int, Int64.t) Poly.Stable.V1.t
      [@@deriving equal, ord, hash]

      let to_latest = Fn.id

      let to_yojson (t : t) =
        `Assoc
          [ ("k", `Int t.k)
          ; ("slots_per_epoch", `Int t.slots_per_epoch)
          ; ("slots_per_sub_window", `Int t.slots_per_sub_window)
          ; ("delta", `Int t.delta)
          ; ( "genesis_state_timestamp"
            , `String
                (Time.to_string_abs
                   (Time.of_span_since_epoch
                      (Time.Span.of_ms
                         (Int64.to_float t.genesis_state_timestamp)))
                   ~zone:Time.Zone.utc) )
          ]

      let of_yojson = function
        | `Assoc
            [ ("k", `Int k)
            ; ("slots_per_epoch", `Int slots_per_epoch)
            ; ("slots_per_sub_window", `Int slots_per_sub_window)
            ; ("delta", `Int delta)
            ; ("genesis_state_timestamp", `String time_str)
            ] -> (
            match validate_time time_str with
            | Ok genesis_state_timestamp ->
                Ok
                  { Poly.k
                  ; slots_per_epoch
                  ; slots_per_sub_window
                  ; delta
                  ; genesis_state_timestamp
                  }
            | Error e ->
                Error (sprintf !"Genesis_constants.Protocol.of_yojson: %s" e) )
        | _ ->
            Error "Genesis_constants.Protocol.of_yojson: unexpected JSON"

      let t_of_sexp _ = failwith "t_of_sexp: not implemented"

      let sexp_of_t (t : t) =
        let module T = struct
          type t = (int, int, string) Poly.Stable.V1.t [@@deriving sexp]
        end in
        let t' : T.t =
          { k = t.k
          ; delta = t.delta
          ; slots_per_epoch = t.slots_per_epoch
          ; slots_per_sub_window = t.slots_per_sub_window
          ; genesis_state_timestamp =
              Time.to_string_abs
                (Time.of_span_since_epoch
                   (Time.Span.of_ms (Int64.to_float t.genesis_state_timestamp)))
                ~zone:Time.Zone.utc
          }
        in
        T.sexp_of_t t'
    end

    module Tests = struct
      let%test "protocol constants serialization v1" =
        let t : V1.t =
          { k = 1
          ; delta = 100
          ; slots_per_sub_window = 10
          ; slots_per_epoch = 1000
          ; genesis_state_timestamp =
              Time.of_string "2019-10-08 17:51:23.050849Z" |> of_time
          }
        in
        (*from the print statement in Serialization.check_serialization*)
        let known_good_digest = "28b7c3bb5f94351f0afa6ebd83078730" in
        Ppx_version_runtime.Serialization.check_serialization
          (module V1)
          t known_good_digest
    end
  end]

  [%%define_locally Stable.Latest.(to_yojson)]
end

module T = struct
  (* bin_io is for printing chain id inputs *)
  type t =
    { protocol : Protocol.Stable.Latest.t
    ; txpool_max_size : int
    ; num_accounts : int option
    }
  [@@deriving to_yojson, bin_io_unversioned]

  let hash (t : t) =
    let str =
      ( List.map
          (* TODO: *)
          [ t.protocol.k
          ; t.protocol.slots_per_epoch
          ; t.protocol.slots_per_sub_window
          ; t.protocol.delta
          ; t.txpool_max_size
          ]
          ~f:Int.to_string
      |> String.concat ~sep:"" )
      ^ Core.Time.to_string_abs ~zone:Time.Zone.utc
          (Time.of_span_since_epoch
             (Time.Span.of_ms
                (Int64.to_float t.protocol.genesis_state_timestamp)))
    in
    Blake2.digest_string str |> Blake2.to_hex
end

include T

[%%inject "genesis_state_timestamp_string", genesis_state_timestamp]

[%%inject "k", k]

[%%inject "slots_per_epoch", slots_per_epoch]

[%%inject "slots_per_sub_window", slots_per_sub_window]

[%%inject "delta", delta]

[%%inject "pool_max_size", pool_max_size]

let compiled : t =
  { protocol =
      { k
      ; slots_per_epoch
      ; slots_per_sub_window
      ; delta
      ; genesis_state_timestamp =
          genesis_timestamp_of_string genesis_state_timestamp_string |> of_time
      }
  ; txpool_max_size = pool_max_size
  ; num_accounts = None
  }

let for_unit_tests = compiled
