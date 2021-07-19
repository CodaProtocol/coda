open Core_kernel

module Fork_config = struct
  type t =
    { previous_state_hash : string
    ; previous_length : int
    ; previous_global_slot : int
    }
  [@@deriving yojson, dhall_type, bin_io_unversioned]
end

let yojson_strip_fields ~keep_fields = function
  | `Assoc l ->
      `Assoc
        (List.filter l ~f:(fun (fld, _) ->
             Array.mem ~equal:String.equal keep_fields fld))
  | json ->
      json

let yojson_rename_fields ~alternates = function
  | `Assoc l ->
      `Assoc
        (List.map l ~f:(fun (fld, json) ->
             let fld =
               Option.value ~default:fld
                 (Array.find_map alternates ~f:(fun (alt, orig) ->
                      if String.equal fld alt then Some orig else None))
             in
             (fld, json)))
  | json ->
      json

let opt_fallthrough ~default x2 =
  Option.value_map ~default x2 ~f:(fun x -> Some x)

let result_opt ~f x =
  match x with
  | Some x ->
      Result.map ~f:Option.some (f x)
  | None ->
      Result.return None

let dump_on_error yojson x =
  Result.map_error x ~f:(fun str ->
      str ^ "\n\nCould not parse JSON:\n" ^ Yojson.Safe.pretty_to_string yojson)

let of_yojson_generic ~fields of_yojson json =
  dump_on_error json @@ of_yojson
  @@ yojson_strip_fields ~keep_fields:fields json

module Json_layout = struct
  module Accounts = struct
    module Single = struct
      module Timed = struct
        type t =
          { initial_minimum_balance : Currency.Balance.t
          ; cliff_time : Mina_numbers.Global_slot.t
          ; cliff_amount : Currency.Amount.t
          ; vesting_period : Mina_numbers.Global_slot.t
          ; vesting_increment : Currency.Amount.t
          }
        [@@deriving yojson, dhall_type, sexp]

        let fields =
          [| "initial_minimum_balance"
           ; "cliff_time"
           ; "cliff_amount"
           ; "vesting_period"
           ; "vesting_increment"
          |]

        let of_yojson json = of_yojson_generic ~fields of_yojson json
      end

      module Permissions = struct
        module Auth_required = struct
          type t = None | Either | Proof | Signature | Both | Impossible
          [@@deriving dhall_type, sexp, bin_io_unversioned]

          let to_yojson = function
            | None ->
                `String "none"
            | Either ->
                `String "either"
            | Proof ->
                `String "proof"
            | Signature ->
                `String "signature"
            | Both ->
                `String "both"
            | Impossible ->
                `String "impossible"

          let of_yojson = function
            | `String s -> (
                match String.lowercase s with
                | "none" ->
                    Ok None
                | "either" ->
                    Ok Either
                | "proof" ->
                    Ok Proof
                | "signature" ->
                    Ok Signature
                | "both" ->
                    Ok Both
                | "impossible" ->
                    Ok Impossible
                | _ ->
                    Error (sprintf "Invalid Auth_required.t value: %s" s) )
            | _ ->
                Error
                  "Runtime_config.Json_Account.Single.Permissions.Auth_Required.t"
        end

        type t =
          { stake : bool [@default false]
          ; edit_state : Auth_required.t [@default None]
          ; send : Auth_required.t [@default None]
          ; receive : Auth_required.t [@default None]
          ; set_delegate : Auth_required.t [@default None]
          ; set_permissions : Auth_required.t [@default None]
          ; set_verification_key : Auth_required.t [@default None]
          }
        [@@deriving yojson, dhall_type, sexp, bin_io_unversioned]

        let fields =
          [| "stake"
           ; "edit_state"
           ; "send"
           ; "receive"
           ; "set_delegate"
           ; "set_permissions"
           ; "set_verification_key"
          |]

        let of_yojson json = of_yojson_generic ~fields of_yojson json
      end

      module Token_permissions = struct
        type t =
          { token_owned : bool [@default false]
          ; account_disabled : bool [@default false]
          ; disable_new_accounts : bool [@default false]
          }
        [@@deriving yojson, dhall_type, sexp, bin_io_unversioned]

        let fields =
          [| "token_owned"; "account_disabled"; "disable_new_accounts" |]

        let of_yojson json = of_yojson_generic ~fields of_yojson json
      end

      module Snapp_account = struct
        module Field = struct
          type t = Snark_params.Tick.Field.t
          [@@deriving sexp, bin_io_unversioned]

          (* can't be automatically derived *)
          let dhall_type = Ppx_dhall_type.Dhall_type.Text

          let to_yojson t = `String (Snark_params.Tick.Field.to_string t)

          let of_yojson = function
            | `String s ->
                Ok (Snark_params.Tick.Field.of_string s)
            | _ ->
                Error "Invalid Field.t runtime config Snapp_account.state"
        end

        type t = { state : Field.t list; verification_key : string option }
        [@@deriving sexp, dhall_type, yojson, bin_io_unversioned]

        let fields = [| "state"; "verification_key" |]

        let of_yojson json = of_yojson_generic ~fields of_yojson json
      end

      type t =
        { pk : string option [@default None]
        ; sk : string option [@default None]
        ; balance : Currency.Balance.t
        ; delegate : string option [@default None]
        ; timing : Timed.t option [@default None]
        ; token : Unsigned_extended.UInt64.t option [@default None]
        ; token_permissions : Token_permissions.t option [@default None]
        ; nonce : Mina_numbers.Account_nonce.t
              [@default Mina_numbers.Account_nonce.zero]
        ; receipt_chain_hash : string option [@default None]
        ; voting_for : string option [@default None]
        ; snapp : Snapp_account.t option [@default None]
        ; permissions : Permissions.t option [@default None]
        }
      [@@deriving sexp, yojson, dhall_type]

      let fields =
        [| "pk"
         ; "sk"
         ; "balance"
         ; "delegate"
         ; "timing"
         ; "token"
         ; "token_permissions"
         ; "nonce"
         ; "receipt_chain_hash"
         ; "voting_for"
         ; "snapp"
         ; "permissions"
        |]

      let of_yojson json = of_yojson_generic ~fields of_yojson json

      let default : t =
        { pk = None
        ; sk = None
        ; balance = Currency.Balance.zero
        ; delegate = None
        ; timing = None
        ; token = None
        ; token_permissions = None
        ; nonce = Mina_numbers.Account_nonce.zero
        ; receipt_chain_hash = None
        ; voting_for = None
        ; snapp = None
        ; permissions = None
        }
    end

    type t = Single.t list [@@deriving yojson, dhall_type]
  end

  module Ledger = struct
    module Balance_spec = struct
      type t = { number : int; balance : Currency.Balance.t }
      [@@deriving yojson, dhall_type]
    end

    type t =
      { accounts : Accounts.t option [@default None]
      ; num_accounts : int option [@default None]
      ; balances : Balance_spec.t list [@default []]
      ; hash : string option [@default None]
      ; name : string option [@default None]
      ; add_genesis_winner : bool option [@default None]
      }
    [@@deriving yojson, dhall_type]

    let fields =
      [| "accounts"
       ; "num_accounts"
       ; "balances"
       ; "hash"
       ; "name"
       ; "add_genesis_winner"
      |]

    let of_yojson json = of_yojson_generic ~fields of_yojson json
  end

  module Proof_keys = struct
    module Transaction_capacity = struct
      type t =
        { log_2 : int option
              [@default None] [@key "2_to_the"] [@dhall_type.key "two_to_the"]
        ; txns_per_second_x10 : int option [@default None]
        }
      [@@deriving yojson, dhall_type]

      let fields = [| "2_to_the"; "txns_per_second_x10" |]

      let alternates = [| ("two_to_the", "2_to_the"); ("log_2", "2_to_the") |]

      let of_yojson json =
        json
        |> yojson_rename_fields ~alternates
        |> yojson_strip_fields ~keep_fields:fields
        |> of_yojson |> dump_on_error json
    end

    type t =
      { level : string option [@default None]
      ; sub_windows_per_window : int option [@default None]
      ; ledger_depth : int option [@default None]
      ; work_delay : int option [@default None]
      ; block_window_duration_ms : int option [@default None]
      ; transaction_capacity : Transaction_capacity.t option [@default None]
      ; coinbase_amount : Currency.Amount.t option [@default None]
      ; supercharged_coinbase_factor : int option [@default None]
      ; account_creation_fee : Currency.Fee.t option [@default None]
      ; fork : Fork_config.t option [@default None]
      ; network_id : int option [@default None]
      }
    [@@deriving yojson, dhall_type]

    let fields =
      [| "level"
       ; "sub_windows_per_window"
       ; "ledger_depth"
       ; "work_delay"
       ; "block_window_duration_ms"
       ; "transaction_capacity"
       ; "coinbase_amount"
       ; "supercharged_coinbase_factor"
       ; "account_creation_fee"
       ; "network_id"
      |]

    let of_yojson json = of_yojson_generic ~fields of_yojson json
  end

  module Genesis = struct
    type t =
      { k : int option [@default None]
      ; delta : int option [@default None]
      ; slots_per_epoch : int option [@default None]
      ; slots_per_sub_window : int option [@default None]
      ; genesis_state_timestamp : string option [@default None]
      }
    [@@deriving yojson, dhall_type]

    let fields =
      [| "k"
       ; "delta"
       ; "slots_per_epoch"
       ; "slots_per_sub_window"
       ; "sub_window_per_window"
       ; "genesis_state_timestamp"
      |]

    let of_yojson json = of_yojson_generic ~fields of_yojson json
  end

  module Daemon = struct
    type t =
      { txpool_max_size : int option [@default None]
      ; peer_list_url : string option [@default None]
      }
    [@@deriving yojson, dhall_type]

    let fields = [| "txpool_max_size"; "peer_list_url" |]

    let of_yojson json = of_yojson_generic ~fields of_yojson json
  end

  module Epoch_data = struct
    module Data = struct
      type t = { accounts : Accounts.t; seed : string }
      [@@deriving yojson, dhall_type]

      let fields = [| "accounts"; "seed" |]

      let of_yojson json = of_yojson_generic ~fields of_yojson json
    end

    type t =
      { staking : Data.t
      ; next : (Data.t option[@default None]) (*If None then next = staking*)
      }
    [@@deriving yojson, dhall_type]

    let fields = [| "staking"; "next" |]

    let of_yojson json = of_yojson_generic ~fields of_yojson json
  end

  type t =
    { daemon : Daemon.t option [@default None]
    ; genesis : Genesis.t option [@default None]
    ; proof : Proof_keys.t option [@default None]
    ; ledger : Ledger.t option [@default None]
    ; epoch_data : Epoch_data.t option [@default None]
    }
  [@@deriving yojson, dhall_type]

  let fields = [| "daemon"; "ledger"; "genesis"; "proof"; "epoch_data" |]

  let of_yojson json = of_yojson_generic ~fields of_yojson json
end

(** JSON representation:

  { "daemon":
      { "txpool_max_size": 1
      , "peer_list_url": "https://www.example.com/peer-list.txt" }
  , "genesis": { "k": 1, "delta": 1 }
  , "proof":
      { "level": "check"
      , "sub_windows_per_window": 8
      , "ledger_depth": 14
      , "work_delay": 2
      , "block_window_duration_ms": 120000
      , "transaction_capacity": {"txns_per_second_x10": 2}
      , "coinbase_amount": "200"
      , "supercharged_coinbase_factor": 2
      , "account_creation_fee": "0.001"
      , "network_id": 1}
  , "ledger":
      { "name": "release"
      , "accounts":
          [ { "pk": "public_key"
            , "sk": "secret_key"
            , "balance": "0.000600000"
            , "delegate": "public_key" }
          , { "pk": "public_key"
            , "sk": "secret_key"
            , "balance": "0.000000000"
            , "delegate": "public_key" } ]
      , "hash": "root_hash"
      , "num_accounts": 10
      , "genesis_state_timestamp": "2000-00-00 12:00:00+0100" } }

  All fields are optional *except*:
  * each account in [ledger.accounts] must have a [balance] field
  * if [ledger] is present, it must feature one of [name], [accounts] or [hash].

*)

module Accounts = struct
  module Single = struct
    module Timed = struct
      type t = Json_layout.Accounts.Single.Timed.t =
        { initial_minimum_balance : Currency.Balance.Stable.Latest.t
        ; cliff_time : Mina_numbers.Global_slot.Stable.Latest.t
        ; cliff_amount : Currency.Amount.Stable.Latest.t
        ; vesting_period : Mina_numbers.Global_slot.Stable.Latest.t
        ; vesting_increment : Currency.Amount.Stable.Latest.t
        }
      [@@deriving bin_io_unversioned, sexp]
    end

    module Permissions = Json_layout.Accounts.Single.Permissions
    module Token_permissions = Json_layout.Accounts.Single.Token_permissions
    module Snapp_account = Json_layout.Accounts.Single.Snapp_account

    type t = Json_layout.Accounts.Single.t =
      { pk : string option
      ; sk : string option
      ; balance : Currency.Balance.Stable.Latest.t
      ; delegate : string option
      ; timing : Timed.t option
      ; token : Unsigned_extended.UInt64.Stable.Latest.t option
      ; token_permissions : Token_permissions.t option
      ; nonce : Mina_numbers.Account_nonce.Stable.Latest.t
      ; receipt_chain_hash : string option
      ; voting_for : string option
      ; snapp : Snapp_account.t option
      ; permissions : Permissions.t option
      }
    [@@deriving bin_io_unversioned, sexp]

    let to_json_layout : t -> Json_layout.Accounts.Single.t = Fn.id

    let of_json_layout : Json_layout.Accounts.Single.t -> (t, string) Result.t =
      Result.return

    let to_yojson x = Json_layout.Accounts.Single.to_yojson (to_json_layout x)

    let of_yojson json =
      Result.bind ~f:of_json_layout (Json_layout.Accounts.Single.of_yojson json)

    let default = Json_layout.Accounts.Single.default
  end

  type single = Single.t =
    { pk : string option
    ; sk : string option
    ; balance : Currency.Balance.t
    ; delegate : string option
    ; timing : Single.Timed.t option
    ; token : Unsigned_extended.UInt64.t option
    ; token_permissions : Single.Token_permissions.t option
    ; nonce : Mina_numbers.Account_nonce.t
    ; receipt_chain_hash : string option
    ; voting_for : string option
    ; snapp : Single.Snapp_account.t option
    ; permissions : Single.Permissions.t option
    }

  type t = Single.t list [@@deriving bin_io_unversioned]

  let to_json_layout : t -> Json_layout.Accounts.t =
    List.map ~f:Single.to_json_layout

  let of_json_layout (t : Json_layout.Accounts.t) : (t, string) Result.t =
    let exception Stop of string in
    try
      Result.return
      @@ List.map t ~f:(fun x ->
             match Single.of_json_layout x with
             | Ok x ->
                 x
             | Error err ->
                 raise (Stop err))
    with Stop err -> Error err

  let to_yojson x = Json_layout.Accounts.to_yojson (to_json_layout x)

  let of_yojson json =
    Result.bind ~f:of_json_layout (Json_layout.Accounts.of_yojson json)
end

module Ledger = struct
  type base =
    | Named of string  (** One of the named ledgers in [Genesis_ledger] *)
    | Accounts of Accounts.t  (** A ledger generated from the given accounts *)
    | Hash of string  (** The ledger with the given root hash *)
  [@@deriving bin_io_unversioned]

  type t =
    { base : base
    ; num_accounts : int option
    ; balances : (int * Currency.Balance.Stable.Latest.t) list
    ; hash : string option
    ; name : string option
    ; add_genesis_winner : bool option
    }
  [@@deriving bin_io_unversioned]

  let to_json_layout
      { base; num_accounts; balances; hash; name; add_genesis_winner } :
      Json_layout.Ledger.t =
    let balances =
      List.map balances ~f:(fun (number, balance) ->
          { Json_layout.Ledger.Balance_spec.number; balance })
    in
    let without_base : Json_layout.Ledger.t =
      { accounts = None
      ; num_accounts
      ; balances
      ; hash
      ; name
      ; add_genesis_winner
      }
    in
    match base with
    | Named name ->
        { without_base with name = Some name }
    | Accounts accounts ->
        { without_base with accounts = Some (Accounts.to_json_layout accounts) }
    | Hash hash ->
        { without_base with hash = Some hash }

  let of_json_layout
      ({ accounts; num_accounts; balances; hash; name; add_genesis_winner } :
        Json_layout.Ledger.t) : (t, string) Result.t =
    let open Result.Let_syntax in
    let%map base =
      match accounts with
      | Some accounts ->
          let%map accounts = Accounts.of_json_layout accounts in
          Accounts accounts
      | None -> (
          match name with
          | Some name ->
              return (Named name)
          | None -> (
              match hash with
              | Some hash ->
                  return (Hash hash)
              | None ->
                  Error
                    "Runtime_config.Ledger.of_json_layout: Expected a field \
                     'accounts', 'name' or 'hash'" ) )
    in
    let balances =
      List.map balances
        ~f:(fun { Json_layout.Ledger.Balance_spec.number; balance } ->
          (number, balance))
    in
    { base; num_accounts; balances; hash; name; add_genesis_winner }

  let to_yojson x = Json_layout.Ledger.to_yojson (to_json_layout x)

  let of_yojson json =
    Result.bind ~f:of_json_layout (Json_layout.Ledger.of_yojson json)
end

module Proof_keys = struct
  module Level = struct
    type t = Full | Check | None [@@deriving bin_io_unversioned, equal]

    let to_string = function
      | Full ->
          "full"
      | Check ->
          "check"
      | None ->
          "none"

    let of_string str =
      match String.lowercase str with
      | "full" ->
          Ok Full
      | "check" ->
          Ok Check
      | "none" ->
          Ok None
      | _ ->
          Error "Expected one of 'full', 'check', or 'none'"

    let to_json_layout = to_string

    let of_json_layout str =
      Result.map_error (of_string str) ~f:(fun err ->
          "Runtime_config.Proof_keys.Level.of_json_layout: Could not decode \
           field 'level'. " ^ err)

    let to_yojson x = `String (to_json_layout x)

    let of_yojson = function
      | `String str ->
          of_json_layout str
      | _ ->
          Error
            "Runtime_config.Proof_keys.Level.of_json_layout: Expected the \
             field 'level' to contain a string"
  end

  module Transaction_capacity = struct
    type t = Log_2 of int | Txns_per_second_x10 of int
    [@@deriving bin_io_unversioned]

    let to_json_layout : t -> Json_layout.Proof_keys.Transaction_capacity.t =
      function
      | Log_2 i ->
          { log_2 = Some i; txns_per_second_x10 = None }
      | Txns_per_second_x10 i ->
          { log_2 = None; txns_per_second_x10 = Some i }

    let of_json_layout :
        Json_layout.Proof_keys.Transaction_capacity.t -> (t, string) Result.t =
      function
      | { log_2 = Some i; txns_per_second_x10 = None } ->
          Ok (Log_2 i)
      | { txns_per_second_x10 = Some i; log_2 = None } ->
          Ok (Txns_per_second_x10 i)
      | _ ->
          Error
            "Runtime_config.Proof_keys.Transaction_capacity.of_json_layout: \
             Expected exactly one of the fields '2_to_the' or \
             'txns_per_second_x10'"

    let to_yojson x =
      Json_layout.Proof_keys.Transaction_capacity.to_yojson (to_json_layout x)

    let of_yojson json =
      Result.bind ~f:of_json_layout
        (Json_layout.Proof_keys.Transaction_capacity.of_yojson json)
  end

  type t =
    { level : Level.t option
    ; sub_windows_per_window : int option
    ; ledger_depth : int option
    ; work_delay : int option
    ; block_window_duration_ms : int option
    ; transaction_capacity : Transaction_capacity.t option
    ; coinbase_amount : Currency.Amount.Stable.Latest.t option
    ; supercharged_coinbase_factor : int option
    ; account_creation_fee : Currency.Fee.Stable.Latest.t option
    ; fork : Fork_config.t option
    ; network_id : int option
    }
  [@@deriving bin_io_unversioned]

  let to_json_layout
      { level
      ; sub_windows_per_window
      ; ledger_depth
      ; work_delay
      ; block_window_duration_ms
      ; transaction_capacity
      ; coinbase_amount
      ; supercharged_coinbase_factor
      ; account_creation_fee
      ; fork
      ; network_id
      } =
    { Json_layout.Proof_keys.level = Option.map ~f:Level.to_json_layout level
    ; sub_windows_per_window
    ; ledger_depth
    ; work_delay
    ; block_window_duration_ms
    ; transaction_capacity =
        Option.map ~f:Transaction_capacity.to_json_layout transaction_capacity
    ; coinbase_amount
    ; supercharged_coinbase_factor
    ; account_creation_fee
    ; fork
    ; network_id
    }

  let of_json_layout
      { Json_layout.Proof_keys.level
      ; sub_windows_per_window
      ; ledger_depth
      ; work_delay
      ; block_window_duration_ms
      ; transaction_capacity
      ; coinbase_amount
      ; supercharged_coinbase_factor
      ; account_creation_fee
      ; fork
      ; network_id
      } =
    let open Result.Let_syntax in
    let%map level = result_opt ~f:Level.of_json_layout level
    and transaction_capacity =
      result_opt ~f:Transaction_capacity.of_json_layout transaction_capacity
    in
    { level
    ; sub_windows_per_window
    ; ledger_depth
    ; work_delay
    ; block_window_duration_ms
    ; transaction_capacity
    ; coinbase_amount
    ; supercharged_coinbase_factor
    ; account_creation_fee
    ; fork
    ; network_id
    }

  let to_yojson x = Json_layout.Proof_keys.to_yojson (to_json_layout x)

  let of_yojson json =
    Result.bind ~f:of_json_layout (Json_layout.Proof_keys.of_yojson json)

  let combine t1 t2 =
    { level = opt_fallthrough ~default:t1.level t2.level
    ; sub_windows_per_window =
        opt_fallthrough ~default:t1.sub_windows_per_window
          t2.sub_windows_per_window
    ; ledger_depth = opt_fallthrough ~default:t1.ledger_depth t2.ledger_depth
    ; work_delay = opt_fallthrough ~default:t1.work_delay t2.work_delay
    ; block_window_duration_ms =
        opt_fallthrough ~default:t1.block_window_duration_ms
          t2.block_window_duration_ms
    ; transaction_capacity =
        opt_fallthrough ~default:t1.transaction_capacity t2.transaction_capacity
    ; coinbase_amount =
        opt_fallthrough ~default:t1.coinbase_amount t2.coinbase_amount
    ; supercharged_coinbase_factor =
        opt_fallthrough ~default:t1.supercharged_coinbase_factor
          t2.supercharged_coinbase_factor
    ; account_creation_fee =
        opt_fallthrough ~default:t1.account_creation_fee t2.account_creation_fee
    ; fork = opt_fallthrough ~default:t1.fork t2.fork
    ; network_id = opt_fallthrough ~default:t1.network_id t2.network_id
    }
end

module Genesis = struct
  type t = Json_layout.Genesis.t =
    { k : int option
    ; delta : int option
    ; slots_per_epoch : int option
    ; slots_per_sub_window : int option
    ; genesis_state_timestamp : string option
    }
  [@@deriving bin_io_unversioned]

  let to_json_layout : t -> Json_layout.Genesis.t = Fn.id

  let of_json_layout : Json_layout.Genesis.t -> (t, string) Result.t =
    Result.return

  let to_yojson x = Json_layout.Genesis.to_yojson (to_json_layout x)

  let of_yojson json =
    Result.bind ~f:of_json_layout (Json_layout.Genesis.of_yojson json)

  let combine t1 t2 =
    { k = opt_fallthrough ~default:t1.k t2.k
    ; delta = opt_fallthrough ~default:t1.delta t2.delta
    ; slots_per_epoch =
        opt_fallthrough ~default:t1.slots_per_epoch t2.slots_per_epoch
    ; slots_per_sub_window =
        opt_fallthrough ~default:t1.slots_per_sub_window t2.slots_per_sub_window
    ; genesis_state_timestamp =
        opt_fallthrough ~default:t1.genesis_state_timestamp
          t2.genesis_state_timestamp
    }
end

module Daemon = struct
  type t = Json_layout.Daemon.t =
    { txpool_max_size : int option; peer_list_url : string option }
  [@@deriving bin_io_unversioned]

  let to_json_layout : t -> Json_layout.Daemon.t = Fn.id

  let of_json_layout : Json_layout.Daemon.t -> (t, string) Result.t =
    Result.return

  let to_yojson x = Json_layout.Daemon.to_yojson (to_json_layout x)

  let of_yojson json =
    Result.bind ~f:of_json_layout (Json_layout.Daemon.of_yojson json)

  let combine t1 t2 =
    { txpool_max_size =
        opt_fallthrough ~default:t1.txpool_max_size t2.txpool_max_size
    ; peer_list_url = opt_fallthrough ~default:t1.peer_list_url t2.peer_list_url
    }
end

module Epoch_data = struct
  module Data = struct
    type t = { ledger : Ledger.t; seed : string }
    [@@deriving bin_io_unversioned, yojson]
  end

  type t =
    { staking : Data.t; next : Data.t option (*If None, then next = staking*) }
  [@@deriving bin_io_unversioned, yojson]

  let to_json_layout : t -> Json_layout.Epoch_data.t =
   fun { staking; next } ->
    let accounts (ledger : Ledger.t) =
      match ledger.base with Accounts acc -> acc | _ -> assert false
    in
    let staking =
      { Json_layout.Epoch_data.Data.accounts = accounts staking.ledger
      ; seed = staking.seed
      }
    in
    let next =
      Option.map next ~f:(fun n ->
          { Json_layout.Epoch_data.Data.accounts = accounts n.ledger
          ; seed = n.seed
          })
    in
    { Json_layout.Epoch_data.staking; next }

  let of_json_layout : Json_layout.Epoch_data.t -> (t, string) Result.t =
   fun { staking; next } ->
    let data accounts seed =
      let ledger =
        { Ledger.base = Accounts accounts
        ; num_accounts = None
        ; balances = []
        ; hash = None
        ; name = None
        ; add_genesis_winner = Some false
        }
      in
      { Data.ledger; seed }
    in
    let staking = data staking.accounts staking.seed in
    let next = Option.map next ~f:(fun n -> data n.accounts n.seed) in
    Ok { staking; next }

  let to_yojson x = Json_layout.Epoch_data.to_yojson (to_json_layout x)

  let of_yojson json =
    Result.bind ~f:of_json_layout (Json_layout.Epoch_data.of_yojson json)
end

type t =
  { daemon : Daemon.t option
  ; genesis : Genesis.t option
  ; proof : Proof_keys.t option
  ; ledger : Ledger.t option
  ; epoch_data : Epoch_data.t option
  }
[@@deriving bin_io_unversioned]

let to_json_layout { daemon; genesis; proof; ledger; epoch_data } =
  { Json_layout.daemon = Option.map ~f:Daemon.to_json_layout daemon
  ; genesis = Option.map ~f:Genesis.to_json_layout genesis
  ; proof = Option.map ~f:Proof_keys.to_json_layout proof
  ; ledger = Option.map ~f:Ledger.to_json_layout ledger
  ; epoch_data = Option.map ~f:Epoch_data.to_json_layout epoch_data
  }

let of_json_layout { Json_layout.daemon; genesis; proof; ledger; epoch_data } =
  let open Result.Let_syntax in
  let%map daemon = result_opt ~f:Daemon.of_json_layout daemon
  and genesis = result_opt ~f:Genesis.of_json_layout genesis
  and proof = result_opt ~f:Proof_keys.of_json_layout proof
  and ledger = result_opt ~f:Ledger.of_json_layout ledger
  and epoch_data = result_opt ~f:Epoch_data.of_json_layout epoch_data in
  { daemon; genesis; proof; ledger; epoch_data }

let to_yojson x = Json_layout.to_yojson (to_json_layout x)

let of_yojson json = Result.bind ~f:of_json_layout (Json_layout.of_yojson json)

let default =
  { daemon = None
  ; genesis = None
  ; proof = None
  ; ledger = None
  ; epoch_data = None
  }

let combine t1 t2 =
  let merge ~combine t1 t2 =
    match (t1, t2) with
    | Some t1, Some t2 ->
        Some (combine t1 t2)
    | Some t, None | None, Some t ->
        Some t
    | None, None ->
        None
  in
  { daemon = merge ~combine:Daemon.combine t1.daemon t2.daemon
  ; genesis = merge ~combine:Genesis.combine t1.genesis t2.genesis
  ; proof = merge ~combine:Proof_keys.combine t1.proof t2.proof
  ; ledger = opt_fallthrough ~default:t1.ledger t2.ledger
  ; epoch_data = opt_fallthrough ~default:t1.epoch_data t2.epoch_data
  }

module Test_configs = struct
  let bootstrap =
    lazy
      ( (* test_postake_bootstrap *)
        {json|
  { "daemon":
      { "txpool_max_size": 3000 }
  , "genesis":
      { "k": 6
      , "delta": 0
      , "genesis_state_timestamp": "2019-01-30 12:00:00-08:00" }
  , "proof":
      { "level": "none"
      , "sub_windows_per_window": 8
      , "ledger_depth": 6
      , "work_delay": 2
      , "block_window_duration_ms": 1500
      , "transaction_capacity": {"2_to_the": 3}
      , "coinbase_amount": "20"
      , "supercharged_coinbase_factor": 2
      , "account_creation_fee": "1" }
  , "ledger": { "name": "test", "add_genesis_winner": false } }
      |json}
      |> Yojson.Safe.from_string |> of_yojson |> Result.ok_or_failwith )

  let transactions =
    lazy
      ( (* test_postake_txns *)
        {json|
  { "daemon":
      { "txpool_max_size": 3000 }
  , "genesis":
      { "k": 6
      , "delta": 0
      , "genesis_state_timestamp": "2019-01-30 12:00:00-08:00" }
  , "proof":
      { "level": "check"
      , "sub_windows_per_window": 8
      , "ledger_depth": 6
      , "work_delay": 2
      , "block_window_duration_ms": 15000
      , "transaction_capacity": {"2_to_the": 3}
      , "coinbase_amount": "20"
      , "supercharged_coinbase_factor": 2
      , "account_creation_fee": "1" }
  , "ledger":
      { "name": "test_split_two_stakers"
      , "add_genesis_winner": false } }
      |json}
      |> Yojson.Safe.from_string |> of_yojson |> Result.ok_or_failwith )

  let split_snarkless =
    lazy
      ( (* test_postake_split_snarkless *)
        {json|
  { "daemon":
      { "txpool_max_size": 3000 }
  , "genesis":
      { "k": 24
      , "delta": 0
      , "genesis_state_timestamp": "2019-01-30 12:00:00-08:00" }
  , "proof":
      { "level": "check"
      , "sub_windows_per_window": 8
      , "ledger_depth": 30
      , "work_delay": 1
      , "block_window_duration_ms": 10000
      , "transaction_capacity": {"2_to_the": 2}
      , "coinbase_amount": "20"
      , "supercharged_coinbase_factor": 2
      , "account_creation_fee": "1" }
  , "ledger":
      { "name": "test_split_two_stakers"
      , "add_genesis_winner": false } }
      |json}
      |> Yojson.Safe.from_string |> of_yojson |> Result.ok_or_failwith )

  let delegation =
    lazy
      ( (* test_postake_delegation *)
        {json|
  { "daemon":
      { "txpool_max_size": 3000 }
  , "genesis":
      { "k": 4
      , "delta": 0
      , "slots_per_epoch": 72
      , "genesis_state_timestamp": "2019-01-30 12:00:00-08:00" }
  , "proof":
      { "level": "check"
      , "sub_windows_per_window": 4
      , "ledger_depth": 6
      , "work_delay": 1
      , "block_window_duration_ms": 5000
      , "transaction_capacity": {"2_to_the": 2}
      , "coinbase_amount": "20"
      , "supercharged_coinbase_factor": 2
      , "account_creation_fee": "1" }
  , "ledger":
      { "name": "test_delegation"
      , "add_genesis_winner": false } }
      |json}
      |> Yojson.Safe.from_string |> of_yojson |> Result.ok_or_failwith )
end
