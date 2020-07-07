open Core
open Async
open Graphql_async
open Coda_base
open Signature_lib
open Currency

let result_of_exn f v ~error = try Ok (f v) with _ -> Error error

let result_of_or_error ?error v =
  Result.map_error v ~f:(fun internal_error ->
      let str_error = Error.to_string_hum internal_error in
      match error with
      | None ->
          str_error
      | Some error ->
          sprintf "%s (%s)" error str_error )

let result_field ~resolve =
  Schema.io_field ~resolve:(fun resolve_info src inputs ->
      Deferred.return @@ resolve resolve_info src inputs )

let result_field_no_inputs ~resolve =
  Schema.io_field ~resolve:(fun resolve_info src ->
      Deferred.return @@ resolve resolve_info src )

module Doc = struct
  let date ?(extra = "") s =
    sprintf
      !"%s (stringified Unix time - number of milliseconds since January 1, \
        1970)%s"
      s extra

  let bin_prot =
    sprintf !"%s (base58-encoded janestreet/bin_prot serialization)"
end

module Reflection = struct
  let regex = lazy (Re2.create_exn {regex|\_(\w)|regex})

  let underToCamel s =
    Re2.replace_exn (Lazy.force regex) s ~f:(fun m ->
        let s = Re2.Match.get_exn ~sub:(`Index 1) m in
        String.capitalize s )

  (** When Fields.folding, create graphql fields via reflection *)
  let reflect f ~typ acc x =
    let new_name = underToCamel (Field.name x) in
    Schema.(
      field new_name ~typ ~args:Arg.[] ~resolve:(fun _ v -> f (Field.get x v))
      :: acc)

  module Shorthand = struct
    open Schema

    (* Note: Eta expansion is needed here to combat OCaml's weak polymorphism nonsense *)

    let id ~typ a x = reflect Fn.id ~typ a x

    let nn_int a x = id ~typ:(non_null int) a x

    let int a x = id ~typ:int a x

    let nn_bool a x = id ~typ:(non_null bool) a x

    let bool a x = id ~typ:bool a x

    let nn_string a x = id ~typ:(non_null string) a x

    let nn_time a x =
      reflect
        (fun t -> Block_time.to_time t |> Time.to_string)
        ~typ:(non_null string) a x

    let string a x = id ~typ:string a x

    module F = struct
      let int f a x = reflect f ~typ:Schema.int a x

      let nn_int f a x = reflect f ~typ:Schema.(non_null int) a x

      let string f a x = reflect f ~typ:Schema.string a x

      let nn_string f a x = reflect f ~typ:Schema.(non_null string) a x
    end
  end
end

module Types = struct
  open Schema
  open Graphql_lib.Base_types

  let public_key = public_key ()

  let uint64 = uint64 ()

  let uint32 = uint32 ()

  let sync_status : ('context, Sync_status.t option) typ =
    enum "SyncStatus" ~doc:"Sync status of daemon"
      ~values:
        (List.map Sync_status.all ~f:(fun status ->
             enum_value
               (String.map ~f:Char.uppercase @@ Sync_status.to_string status)
               ~value:status ))

  let transaction_status : ('context, Transaction_status.State.t option) typ =
    enum "TransactionStatus" ~doc:"Status of a transaction"
      ~values:
        Transaction_status.State.
          [ enum_value "INCLUDED" ~value:Included
              ~doc:"A transaction that is on the longest chain"
          ; enum_value "PENDING" ~value:Pending
              ~doc:
                "A transaction either in the transition frontier or in \
                 transaction pool but is not on the longest chain"
          ; enum_value "UNKNOWN" ~value:Unknown
              ~doc:
                "The transaction has either been snarked, reached finality \
                 through consensus or has been dropped" ]

  let consensus_time =
    let module C = Consensus.Data.Consensus_time in
    obj "ConsensusTime" ~fields:(fun _ ->
        [ field "epoch" ~typ:(non_null uint32)
            ~args:Arg.[]
            ~resolve:(fun _ global_slot -> C.epoch global_slot)
        ; field "slot" ~typ:(non_null uint32)
            ~args:Arg.[]
            ~resolve:(fun _ global_slot -> C.slot global_slot)
        ; field "globalSlot" ~typ:(non_null uint32)
            ~args:Arg.[]
            ~resolve:(fun _ (global_slot : Consensus.Data.Consensus_time.t) ->
              C.to_uint32 global_slot )
        ; field "startTime" ~typ:(non_null string)
            ~args:Arg.[]
            ~resolve:(fun {ctx= coda; _} global_slot ->
              let constants =
                (Coda_lib.config coda).precomputed_values.consensus_constants
              in
              Block_time.to_string @@ C.start_time ~constants global_slot )
        ; field "endTime" ~typ:(non_null string)
            ~args:Arg.[]
            ~resolve:(fun {ctx= coda; _} global_slot ->
              let constants =
                (Coda_lib.config coda).precomputed_values.consensus_constants
              in
              Block_time.to_string @@ C.end_time ~constants global_slot ) ] )

  let block_producer_timing :
      ( _
      , [`Check_again of Block_time.t | `Produce of Block_time.t | `Produce_now]
        option )
      typ =
    obj "BlockProducerTimings" ~fields:(fun _ ->
        let of_time ~consensus_constants =
          Consensus.Data.Consensus_time.of_time_exn
            ~constants:consensus_constants
        in
        [ field "times"
            ~typ:(non_null @@ list @@ non_null consensus_time)
            ~args:Arg.[]
            ~resolve:(fun {ctx= coda; _} ->
              let consensus_constants =
                (Coda_lib.config coda).precomputed_values.consensus_constants
              in
              function
              | `Check_again _time ->
                  []
              | `Produce time ->
                  [of_time time ~consensus_constants]
              | `Produce_now ->
                  [ of_time ~consensus_constants
                    @@ Block_time.now (Coda_lib.config coda).time_controller ]
              ) ] )

  module DaemonStatus = struct
    type t = Daemon_rpcs.Types.Status.t

    let interval : (_, (Time.Span.t * Time.Span.t) option) typ =
      obj "Interval" ~fields:(fun _ ->
          [ field "start" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ (start, _) ->
                Time.Span.to_ms start |> Int64.of_float |> Int64.to_string )
          ; field "stop" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ (_, end_) ->
                Time.Span.to_ms end_ |> Int64.of_float |> Int64.to_string ) ]
      )

    let histogram : (_, Perf_histograms.Report.t option) typ =
      obj "Histogram" ~fields:(fun _ ->
          let open Reflection.Shorthand in
          List.rev
          @@ Perf_histograms.Report.Fields.fold ~init:[]
               ~values:(id ~typ:Schema.(non_null (list (non_null int))))
               ~intervals:(id ~typ:(non_null (list (non_null interval))))
               ~underflow:nn_int ~overflow:nn_int )

    module Rpc_timings = Daemon_rpcs.Types.Status.Rpc_timings
    module Rpc_pair = Rpc_timings.Rpc_pair

    let rpc_pair : (_, Perf_histograms.Report.t option Rpc_pair.t option) typ =
      let h = Reflection.Shorthand.id ~typ:histogram in
      obj "RpcPair" ~fields:(fun _ ->
          List.rev @@ Rpc_pair.Fields.fold ~init:[] ~dispatch:h ~impl:h )

    let rpc_timings : (_, Rpc_timings.t option) typ =
      let fd = Reflection.Shorthand.id ~typ:(non_null rpc_pair) in
      obj "RpcTimings" ~fields:(fun _ ->
          List.rev
          @@ Rpc_timings.Fields.fold ~init:[] ~get_staged_ledger_aux:fd
               ~answer_sync_ledger_query:fd ~get_ancestry:fd
               ~get_transition_chain_proof:fd ~get_transition_chain:fd )

    module Histograms = Daemon_rpcs.Types.Status.Histograms

    let histograms : (_, Histograms.t option) typ =
      let h = Reflection.Shorthand.id ~typ:histogram in
      obj "Histograms" ~fields:(fun _ ->
          let open Reflection.Shorthand in
          List.rev
          @@ Histograms.Fields.fold ~init:[]
               ~rpc_timings:(id ~typ:(non_null rpc_timings))
               ~external_transition_latency:h
               ~accepted_transition_local_latency:h
               ~accepted_transition_remote_latency:h
               ~snark_worker_transition_time:h ~snark_worker_merge_time:h )

    let consensus_configuration : (_, Consensus.Configuration.t option) typ =
      obj "ConsensusConfiguration" ~fields:(fun _ ->
          let open Reflection.Shorthand in
          List.rev
          @@ Consensus.Configuration.Fields.fold ~init:[] ~delta:nn_int
               ~k:nn_int ~c:nn_int ~c_times_k:nn_int ~slots_per_epoch:nn_int
               ~slot_duration:nn_int ~epoch_duration:nn_int
               ~acceptable_network_delay:nn_int
               ~genesis_state_timestamp:nn_time )

    let peer : (_, Network_peer.Peer.Display.t option) typ =
      obj "Peer" ~fields:(fun _ ->
          let open Reflection.Shorthand in
          List.rev
          @@ Network_peer.Peer.Display.Fields.fold ~init:[] ~host:nn_string
               ~libp2p_port:nn_int ~peer_id:nn_string )

    let addrs_and_ports : (_, Node_addrs_and_ports.Display.t option) typ =
      obj "AddrsAndPorts" ~fields:(fun _ ->
          let open Reflection.Shorthand in
          List.rev
          @@ Node_addrs_and_ports.Display.Fields.fold ~init:[]
               ~external_ip:nn_string ~bind_ip:nn_string ~client_port:nn_int
               ~libp2p_port:nn_int ~peer:(id ~typ:peer) )

    let t : (_, Daemon_rpcs.Types.Status.t option) typ =
      obj "DaemonStatus" ~fields:(fun _ ->
          let open Reflection.Shorthand in
          List.rev
          @@ Daemon_rpcs.Types.Status.Fields.fold ~init:[] ~num_accounts:int
               ~next_block_production:(id ~typ:block_producer_timing)
               ~blockchain_length:int ~uptime_secs:nn_int
               ~ledger_merkle_root:string ~state_hash:string
               ~commit_id:nn_string ~conf_dir:nn_string
               ~peers:(id ~typ:Schema.(non_null @@ list (non_null string)))
               ~user_commands_sent:nn_int ~snark_worker:string
               ~snark_work_fee:nn_int
               ~sync_status:(id ~typ:(non_null sync_status))
               ~block_production_keys:
                 (id ~typ:Schema.(non_null @@ list (non_null string)))
               ~histograms:(id ~typ:histograms)
               ~consensus_time_best_tip:(id ~typ:consensus_time)
               ~consensus_time_now:(id ~typ:Schema.(non_null consensus_time))
               ~consensus_mechanism:nn_string
               ~addrs_and_ports:(id ~typ:(non_null addrs_and_ports))
               ~consensus_configuration:
                 (id ~typ:(non_null consensus_configuration))
               ~highest_block_length_received:nn_int )
  end

  let fee_transfer =
    obj "FeeTransfer" ~fields:(fun _ ->
        [ field "recipient"
            ~args:Arg.[]
            ~doc:"Public key of fee transfer recipient"
            ~typ:(non_null public_key)
            ~resolve:(fun _ {Fee_transfer.receiver_pk= pk; _} -> pk)
        ; field "fee" ~typ:(non_null uint64)
            ~args:Arg.[]
            ~doc:"Amount that the recipient is paid in this fee transfer"
            ~resolve:(fun _ {Fee_transfer.fee; _} -> Currency.Fee.to_uint64 fee)
        ] )

  let completed_work =
    obj "CompletedWork" ~doc:"Completed snark works" ~fields:(fun _ ->
        [ field "prover"
            ~args:Arg.[]
            ~doc:"Public key of the prover" ~typ:(non_null public_key)
            ~resolve:(fun _ {Transaction_snark_work.Info.prover; _} -> prover)
        ; field "fee" ~typ:(non_null uint64)
            ~args:Arg.[]
            ~doc:"Amount the prover is paid for the snark work"
            ~resolve:(fun _ {Transaction_snark_work.Info.fee; _} ->
              Currency.Fee.to_uint64 fee )
        ; field "workIds" ~doc:"Unique identifier for the snark work purchased"
            ~typ:(non_null @@ list @@ non_null int)
            ~args:Arg.[]
            ~resolve:(fun _ {Transaction_snark_work.Info.work_ids; _} ->
              One_or_two.to_list work_ids ) ] )

  let sign =
    enum "sign"
      ~values:
        [enum_value "PLUS" ~value:Sgn.Pos; enum_value "MINUS" ~value:Sgn.Neg]

  let signed_fee =
    obj "SignedFee" ~doc:"Signed fee" ~fields:(fun _ ->
        [ field "sign" ~typ:(non_null sign) ~doc:"+/-"
            ~args:Arg.[]
            ~resolve:(fun _ fee -> Currency.Fee.Signed.sgn fee)
        ; field "feeMagnitude" ~typ:(non_null uint64) ~doc:"Fee"
            ~args:Arg.[]
            ~resolve:(fun _ fee ->
              Currency.Fee.(to_uint64 (Signed.magnitude fee)) ) ] )

  let work_statement =
    obj "WorkDescription"
      ~doc:
        "Transition from a source ledger to a target ledger with some fee \
         excess and increase in supply " ~fields:(fun _ ->
        [ field "sourceLedgerHash" ~typ:(non_null string)
            ~doc:"Base58Check-encoded hash of the source ledger"
            ~args:Arg.[]
            ~resolve:(fun _ {Transaction_snark.Statement.source; _} ->
              Frozen_ledger_hash.to_string source )
        ; field "targetLedgerHash" ~typ:(non_null string)
            ~doc:"Base58Check-encoded hash of the target ledger"
            ~args:Arg.[]
            ~resolve:(fun _ {Transaction_snark.Statement.target; _} ->
              Frozen_ledger_hash.to_string target )
        ; field "feeExcess" ~typ:(non_null signed_fee)
            ~doc:
              "Total transaction fee that is not accounted for in the \
               transition from source ledger to target ledger"
            ~args:Arg.[]
            ~resolve:
              (fun _
                   ({fee_excess= {fee_excess_l; _}; _} :
                     Transaction_snark.Statement.t) ->
              (* TODO: Expose full fee excess data. *)
              fee_excess_l )
        ; field "supplyIncrease" ~typ:(non_null uint64)
            ~doc:"Increase in total coinbase reward "
            ~args:Arg.[]
            ~resolve:(fun _ {Transaction_snark.Statement.supply_increase; _} ->
              Currency.Amount.to_uint64 supply_increase )
        ; field "workId" ~doc:"Unique identifier for a snark work"
            ~typ:(non_null int)
            ~args:Arg.[]
            ~resolve:(fun _ w -> Transaction_snark.Statement.hash w) ] )

  let pending_work =
    obj "PendingSnarkWork"
      ~doc:"Snark work bundles that are not available in the pool yet"
      ~fields:(fun _ ->
        [ field "workBundle"
            ~args:Arg.[]
            ~doc:"Work bundle with one or two snark work"
            ~typ:(non_null @@ list @@ non_null work_statement)
            ~resolve:(fun _ w -> One_or_two.to_list w) ] )

  let blockchain_state =
    obj "BlockchainState" ~fields:(fun _ ->
        [ field "date" ~typ:(non_null string) ~doc:(Doc.date "date")
            ~args:Arg.[]
            ~resolve:(fun _ {Coda_state.Blockchain_state.Poly.timestamp; _} ->
              Block_time.to_string timestamp )
        ; field "utcDate" ~typ:(non_null string)
            ~doc:
              (Doc.date
                 ~extra:
                   ". Time offsets are adjusted to reflect true wall-clock \
                    time instead of genesis time."
                 "utcDate")
            ~args:Arg.[]
            ~resolve:
              (fun {ctx= coda; _}
                   {Coda_state.Blockchain_state.Poly.timestamp; _} ->
              Block_time.to_string_system_time
                (Coda_lib.time_controller coda)
                timestamp )
        ; field "snarkedLedgerHash" ~typ:(non_null string)
            ~doc:"Base58Check-encoded hash of the snarked ledger"
            ~args:Arg.[]
            ~resolve:
              (fun _ {Coda_state.Blockchain_state.Poly.snarked_ledger_hash; _} ->
              Frozen_ledger_hash.to_string snarked_ledger_hash )
        ; field "stagedLedgerHash" ~typ:(non_null string)
            ~doc:"Base58Check-encoded hash of the staged ledger"
            ~args:Arg.[]
            ~resolve:
              (fun _ {Coda_state.Blockchain_state.Poly.staged_ledger_hash; _} ->
              Coda_base.Ledger_hash.to_string
              @@ Staged_ledger_hash.ledger_hash staged_ledger_hash ) ] )

  let protocol_state =
    let open Auxiliary_database.Filtered_external_transition.Protocol_state in
    obj "ProtocolState" ~fields:(fun _ ->
        [ field "previousStateHash" ~typ:(non_null string)
            ~doc:"Base58Check-encoded hash of the previous state"
            ~args:Arg.[]
            ~resolve:(fun _ t ->
              State_hash.to_base58_check t.previous_state_hash )
        ; field "blockchainState"
            ~doc:"State which is agnostic of a particular consensus algorithm"
            ~typ:(non_null blockchain_state)
            ~args:Arg.[]
            ~resolve:(fun _ t -> t.blockchain_state)
        ; field "consensusState"
            ~doc:
              "State specific to the Codaboros Proof of Stake consensus \
               algorithm"
            ~typ:(non_null @@ Consensus.Data.Consensus_state.graphql_type ())
            ~args:Arg.[]
            ~resolve:(fun _ t -> t.consensus_state) ] )

  let chain_reorganization_status : ('context, [`Changed] option) typ =
    enum "ChainReorganizationStatus"
      ~doc:"Status for whenever the blockchain is reorganized"
      ~values:[enum_value "CHANGED" ~value:`Changed]

  module AccountObj = struct
    module AnnotatedBalance = struct
      type t =
        { total: Balance.t
        ; unknown: Balance.t
        ; blockchain_length: Unsigned.uint32 }

      let obj =
        obj "AnnotatedBalance"
          ~doc:
            "A total balance annotated with the amount that is currently \
             unknown with the invariant: unknown <= total" ~fields:(fun _ ->
            [ field "total" ~typ:(non_null uint64)
                ~doc:"The amount of coda owned by the account"
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) -> Balance.to_uint64 b.total)
            ; field "unknown" ~typ:(non_null uint64)
                ~doc:
                  "The amount of coda owned by the account whose origin is \
                   currently unknown"
                ~deprecated:(Deprecated None)
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) -> Balance.to_uint64 b.unknown)
            ; field "blockHeight" ~typ:(non_null uint32)
                ~doc:"Block height at which balance was measured"
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) -> b.blockchain_length) ] )
    end

    module Partial_account = struct
      let to_full_account
          { Account.Poly.public_key
          ; token_id
          ; token_permissions
          ; nonce
          ; balance
          ; receipt_chain_hash
          ; delegate
          ; voting_for
          ; timing } =
        let open Option.Let_syntax in
        let%bind public_key = public_key in
        let%bind token_permissions = token_permissions in
        let%bind nonce = nonce in
        let%bind receipt_chain_hash = receipt_chain_hash in
        let%bind delegate = delegate in
        let%bind voting_for = voting_for in
        let%map timing = timing in
        { Account.Poly.public_key
        ; token_id
        ; token_permissions
        ; nonce
        ; balance
        ; receipt_chain_hash
        ; delegate
        ; voting_for
        ; timing }

      let of_full_account
          { Account.Poly.public_key
          ; token_id
          ; token_permissions
          ; nonce
          ; balance
          ; receipt_chain_hash
          ; delegate
          ; voting_for
          ; timing } blockchain_length =
        { Account.Poly.public_key= Some public_key
        ; token_id
        ; token_permissions= Some token_permissions
        ; nonce= Some nonce
        ; balance=
            { AnnotatedBalance.total= balance
            ; unknown= balance
            ; blockchain_length }
        ; receipt_chain_hash= Some receipt_chain_hash
        ; delegate= Some delegate
        ; voting_for= Some voting_for
        ; timing }

      let of_account_id coda account_id =
        let account =
          coda |> Coda_lib.best_tip |> Participating_state.active
          |> Option.bind ~f:(fun tip ->
                 let ledger =
                   Transition_frontier.Breadcrumb.staged_ledger tip
                   |> Staged_ledger.ledger
                 in
                 Ledger.location_of_account ledger account_id
                 |> Option.bind ~f:(Ledger.get ledger)
                 |> Option.map ~f:(fun account ->
                        ( account
                        , Transition_frontier.Breadcrumb.blockchain_length tip
                        ) ) )
        in
        match account with
        | Some
            ( { Account.Poly.public_key
              ; token_id
              ; token_permissions
              ; nonce
              ; balance
              ; receipt_chain_hash
              ; delegate
              ; voting_for
              ; timing }
            , blockchain_length ) ->
            { Account.Poly.public_key= Some public_key
            ; token_id
            ; token_permissions= Some token_permissions
            ; nonce= Some nonce
            ; delegate= Some delegate
            ; balance=
                { AnnotatedBalance.total= balance
                ; unknown= balance
                ; blockchain_length }
            ; receipt_chain_hash= Some receipt_chain_hash
            ; voting_for= Some voting_for
            ; timing }
        | None ->
            Account.
              { Poly.public_key= Some (Account_id.public_key account_id)
              ; token_id= Account_id.token_id account_id
              ; token_permissions= None
              ; nonce= None
              ; delegate= None
              ; balance=
                  { AnnotatedBalance.total= Balance.zero
                  ; unknown= Balance.zero
                  ; blockchain_length= Unsigned.UInt32.zero }
              ; receipt_chain_hash= None
              ; voting_for= None
              ; timing= Timing.Untimed }

      let of_pk coda pk =
        of_account_id coda (Account_id.create pk Token_id.default)
    end

    (** Hack: Account.Poly.t is only parameterized over 'pk once and so, in
        order for delegate to be optional, we must also make account
        public_key optional even though it's always Some. In an attempt to
        avoid a large refactoring, and also avoid making a new record, we'll
        deal with a value_exn here and be sad. *)
    type t =
      { account:
          ( Public_key.Compressed.t option
          , Token_id.t
          , Token_permissions.t option
          , AnnotatedBalance.t
          , Account.Nonce.t option
          , Receipt.Chain_hash.t option
          , State_hash.t option
          , Account.Timing.t )
          Account.Poly.t
      ; locked: bool option
      ; is_actively_staking: bool
      ; path: string }

    let lift coda pk account =
      let block_production_pubkeys = Coda_lib.block_production_pubkeys coda in
      let accounts = Coda_lib.wallets coda in
      { account
      ; locked= Secrets.Wallets.check_locked accounts ~needle:pk
      ; is_actively_staking=
          Public_key.Compressed.Set.mem block_production_pubkeys pk
      ; path= Secrets.Wallets.get_path accounts pk }

    let get_best_ledger_account coda pk =
      lift coda pk (Partial_account.of_pk coda pk)

    let account_id {Account.Poly.public_key; token_id; _} =
      Account_id.create (Option.value_exn public_key) token_id

    let rec account =
      lazy
        (obj "Account" ~doc:"An account record according to the daemon"
           ~fields:(fun _ ->
             [ field "publicKey" ~typ:(non_null public_key)
                 ~doc:"The public identity of the account"
                 ~args:Arg.[]
                 ~resolve:(fun _ {account; _} ->
                   Option.value_exn account.Account.Poly.public_key )
             ; field "balance"
                 ~typ:(non_null AnnotatedBalance.obj)
                 ~doc:"The amount of coda owned by the account"
                 ~args:Arg.[]
                 ~resolve:(fun _ {account; _} -> account.Account.Poly.balance)
             ; field "nonce" ~typ:string
                 ~doc:
                   "A natural number that increases with each transaction \
                    (stringified uint32)"
                 ~args:Arg.[]
                 ~resolve:(fun _ {account; _} ->
                   Option.map ~f:Account.Nonce.to_string
                     account.Account.Poly.nonce )
             ; field "inferredNonce" ~typ:string
                 ~doc:
                   "Like the `nonce` field, except it includes the scheduled \
                    transactions (transactions not yet included in a block) \
                    (stringified uint32)"
                 ~args:Arg.[]
                 ~resolve:(fun {ctx= coda; _} {account; _} ->
                   let account_id = account_id account in
                   match
                     Coda_lib
                     .get_inferred_nonce_from_transaction_pool_and_ledger coda
                       account_id
                   with
                   | `Active (Some nonce) ->
                       Some (Account.Nonce.to_string nonce)
                   | `Active None | `Bootstrapping ->
                       None )
             ; field "epochDelegateAccount" ~typ:(Lazy.force account)
                 ~doc:
                   "The account that you delegated on the staking ledger of \
                    the current block's epoch"
                 ~args:Arg.[]
                 ~resolve:(fun {ctx= coda; _} {account; _} ->
                   let open Option.Let_syntax in
                   let account_id = account_id account in
                   let%bind staking_ledger = Coda_lib.staking_ledger coda in
                   try
                     let index =
                       Sparse_ledger.find_index_exn staking_ledger account_id
                     in
                     let delegate_account =
                       Sparse_ledger.get_exn staking_ledger index
                     in
                     let delegate_key = delegate_account.public_key in
                     Some (get_best_ledger_account coda delegate_key)
                   with e ->
                     Logger.warn
                       (Coda_lib.top_level_logger coda)
                       ~module_:__MODULE__ ~location:__LOC__
                       ~metadata:[("error", `String (Exn.to_string e))]
                       "Could not retrieve delegate account from sparse \
                        ledger. The account may not be in the ledger: $error" ;
                     None )
             ; field "receiptChainHash" ~typ:string
                 ~doc:"Top hash of the receipt chain merkle-list"
                 ~args:Arg.[]
                 ~resolve:(fun _ {account; _} ->
                   Option.map ~f:Receipt.Chain_hash.to_string
                     account.Account.Poly.receipt_chain_hash )
             ; field "delegate" ~typ:public_key
                 ~doc:
                   "The public key to which you are delegating - if you are \
                    not delegating to anybody, this would return your public \
                    key"
                 ~args:Arg.[]
                 ~deprecated:(Deprecated (Some "use delegateAccount instead"))
                 ~resolve:(fun _ {account; _} -> account.Account.Poly.delegate)
             ; field "delegateAccount" ~typ:(Lazy.force account)
                 ~doc:
                   "The account to which you are delegating - if you are not \
                    delegating to anybody, this would return your public key"
                 ~args:Arg.[]
                 ~resolve:(fun {ctx= coda; _} {account; _} ->
                   Option.map
                     ~f:(get_best_ledger_account coda)
                     account.Account.Poly.delegate )
             ; field "delegators"
                 ~typ:(list @@ non_null @@ Lazy.force account)
                 ~doc:
                   "The list of accounts which are delegating to you (note \
                    that the info is recorded in the last epoch so it might \
                    not be up to date with the current account status)"
                 ~args:Arg.[]
                 ~resolve:(fun {ctx= coda; _} {account; _} ->
                   let open Option.Let_syntax in
                   let%bind pk = account.Account.Poly.public_key in
                   let%map delegators =
                     Coda_lib.current_epoch_delegators coda ~pk
                   in
                   List.map
                     ~f:(fun a ->
                       { account=
                           Partial_account.of_full_account a
                             Unsigned.UInt32.zero
                       ; locked= None
                       ; is_actively_staking= true
                       ; path= "" } )
                     delegators )
             ; field "lastEpochDelegators"
                 ~typ:(list @@ non_null @@ Lazy.force account)
                 ~doc:
                   "The list of accounts which are delegating to you in the \
                    last epoch (note that the info is recorded in the one \
                    before last epoch epoch so it might not be up to date \
                    with the current account status)"
                 ~args:Arg.[]
                 ~resolve:(fun {ctx= coda; _} {account; _} ->
                   let open Option.Let_syntax in
                   let%bind pk = account.Account.Poly.public_key in
                   let%map delegators =
                     Coda_lib.last_epoch_delegators coda ~pk
                   in
                   List.map
                     ~f:(fun a ->
                       { account=
                           Partial_account.of_full_account a
                             Unsigned.UInt32.zero
                       ; locked= None
                       ; is_actively_staking= true
                       ; path= "" } )
                     delegators )
             ; field "votingFor" ~typ:string
                 ~doc:
                   "The previous epoch lock hash of the chain which you are \
                    voting for"
                 ~args:Arg.[]
                 ~resolve:(fun _ {account; _} ->
                   Option.map ~f:Coda_base.State_hash.to_base58_check
                     account.Account.Poly.voting_for )
             ; field "stakingActive" ~typ:(non_null bool)
                 ~doc:
                   "True if you are actively staking with this account on the \
                    current daemon - this may not yet have been updated if \
                    the staking key was changed recently"
                 ~args:Arg.[]
                 ~resolve:(fun _ {is_actively_staking; _} ->
                   is_actively_staking )
             ; field "privateKeyPath" ~typ:(non_null string)
                 ~doc:"Path of the private key file for this account"
                 ~args:Arg.[]
                 ~resolve:(fun _ {path; _} -> path)
             ; field "locked" ~typ:bool
                 ~doc:
                   "True if locked, false if unlocked, null if the account \
                    isn't tracked by the queried daemon"
                 ~args:Arg.[]
                 ~resolve:(fun _ {locked; _} -> locked) ] ))

    let account = Lazy.force account
  end

  let user_command : (Coda_lib.t, User_command.t option) typ =
    obj "UserCommand" ~fields:(fun _ ->
        [ field "id" ~typ:(non_null guid)
            ~args:Arg.[]
            ~resolve:(fun _ user_command ->
              User_command.to_base58_check user_command )
        ; field "isDelegation" ~typ:(non_null bool)
            ~doc:
              "If true, this represents a delegation of stake, otherwise it \
               is a payment"
            ~args:Arg.[]
            ~resolve:(fun _ user_command ->
              match
                User_command.Payload.body @@ User_command.payload user_command
              with
              | Stake_delegation _ ->
                  true
              | _ ->
                  false )
        ; field "nonce" ~typ:(non_null int) ~doc:"Nonce of the transaction"
            ~args:Arg.[]
            ~resolve:(fun _ payment ->
              User_command_payload.nonce @@ User_command.payload payment
              |> Account.Nonce.to_int )
        ; field "from" ~typ:(non_null public_key)
            ~doc:"Public key of the sender"
            ~deprecated:(Deprecated (Some "use fromAccount field instead"))
            ~args:Arg.[]
            ~resolve:(fun _ payment ->
              Account_id.public_key @@ User_command.fee_payer payment )
        ; field "fromAccount"
            ~typ:(non_null AccountObj.account)
            ~doc:"Account of the sender"
            ~args:Arg.[]
            ~resolve:(fun {ctx= coda; _} payment ->
              AccountObj.get_best_ledger_account coda
                (Account_id.public_key @@ User_command.fee_payer payment) )
        ; field "to" ~typ:(non_null public_key)
            ~doc:"Public key of the receiver"
            ~deprecated:(Deprecated (Some "use toAccount field instead"))
            ~args:Arg.[]
            ~resolve:(fun _ payment -> User_command.receiver_pk payment)
        ; field "toAccount"
            ~typ:(non_null AccountObj.account)
            ~doc:"Account of the receiver"
            ~args:Arg.[]
            ~resolve:(fun {ctx= coda; _} payment ->
              let pk = User_command.receiver_pk payment in
              AccountObj.get_best_ledger_account coda pk )
        ; result_field_no_inputs "amount" ~typ:(non_null uint64)
            ~doc:
              "Amount that sender is sending to receiver - this is 0 for \
               delegations"
            ~args:Arg.[]
            ~resolve:(fun _ payment ->
              match
                User_command_payload.body (User_command.payload payment)
              with
              | Payment {Payment_payload.Poly.amount; _} ->
                  Ok (amount |> Currency.Amount.to_uint64)
              | Mint_tokens {amount; _} ->
                  Ok (amount |> Currency.Amount.to_uint64)
              | Stake_delegation _
              | Create_new_token _
              | Create_token_account _
              | Set_token_permissions _
              | Set_account_permissions _ ->
                  (* These commands do not have an amount, so we set it to 0 *)
                  Ok Unsigned.UInt64.zero )
        ; field "fee" ~typ:(non_null uint64)
            ~doc:"Fee that sender is willing to pay for making the transaction"
            ~args:Arg.[]
            ~resolve:(fun _ payment ->
              User_command.fee payment |> Currency.Fee.to_uint64 )
        ; field "memo" ~typ:(non_null string)
            ~doc:"Short arbitrary message provided by the sender"
            ~args:Arg.[]
            ~resolve:(fun _ payment ->
              User_command_payload.memo @@ User_command.payload payment
              |> User_command_memo.to_string ) ] )

  let transactions =
    let open Auxiliary_database.Filtered_external_transition.Transactions in
    obj "Transactions" ~doc:"Different types of transactions in a block"
      ~fields:(fun _ ->
        [ field "userCommands"
            ~doc:
              "List of user commands (payments and stake delegations) \
               included in this block"
            ~typ:(non_null @@ list @@ non_null user_command)
            ~args:Arg.[]
            ~resolve:(fun _ {user_commands; _} -> user_commands)
        ; field "feeTransfer"
            ~doc:"List of fee transfers included in this block"
            ~typ:(non_null @@ list @@ non_null fee_transfer)
            ~args:Arg.[]
            ~resolve:(fun _ {fee_transfers; _} -> fee_transfers)
        ; field "coinbase" ~typ:(non_null uint64)
            ~doc:"Amount of coda granted to the producer of this block"
            ~args:Arg.[]
            ~resolve:(fun _ {coinbase; _} -> Currency.Amount.to_uint64 coinbase)
        ; field "coinbaseReceiverAccount" ~typ:AccountObj.account
            ~doc:"Account to which the coinbase for this block was granted"
            ~args:Arg.[]
            ~resolve:(fun {ctx= coda; _} {coinbase_receiver; _} ->
              Option.map
                ~f:(AccountObj.get_best_ledger_account coda)
                coinbase_receiver ) ] )

  let protocol_state_proof : (Coda_lib.t, Proof.t option) typ =
    let display_g1_elem (g1 : Crypto_params.Tick_backend.Inner_curve.t) =
      let x, y = Crypto_params.Tick_backend.Inner_curve.to_affine_exn g1 in
      List.map [x; y] ~f:Crypto_params.Tick0.Field.to_string
    in
    let display_g2_elem (g2 : Curve_choice.Tock_full.G2.t) =
      let open Curve_choice.Tock_full in
      let x, y = G2.to_affine_exn g2 in
      let to_string (fqe : Fqe.t) =
        let vector = Fqe.to_vector fqe in
        List.init (Fq.Vector.length vector) ~f:(fun i ->
            let fq = Fq.Vector.get vector i in
            Crypto_params.Tick0.Field.to_string fq )
      in
      List.map [x; y] ~f:to_string
    in
    let string_list_field ~resolve =
      field
        ~typ:(non_null @@ list (non_null string))
        ~args:Arg.[]
        ~resolve:(fun _ (proof : Proof.t) -> display_g1_elem (resolve proof))
    in
    let string_list_list_field ~resolve =
      field
        ~typ:(non_null @@ list (non_null @@ list @@ non_null string))
        ~args:Arg.[]
        ~resolve:(fun _ (proof : Proof.t) -> display_g2_elem (resolve proof))
    in
    obj "protocolStateProof" ~fields:(fun _ ->
        [ string_list_field "a" ~resolve:(fun (proof : Proof.t) -> proof.a)
        ; string_list_list_field "b" ~resolve:(fun (proof : Proof.t) -> proof.b)
        ; string_list_field "c" ~resolve:(fun (proof : Proof.t) -> proof.c)
        ; string_list_list_field "delta_prime"
            ~resolve:(fun (proof : Proof.t) -> proof.delta_prime)
        ; string_list_field "z" ~resolve:(fun (proof : Proof.t) -> proof.z) ]
    )

  let block :
      ( Coda_lib.t
      , ( Auxiliary_database.Filtered_external_transition.t
        , State_hash.t )
        With_hash.t
        option )
      typ =
    let open Auxiliary_database.Filtered_external_transition in
    obj "Block" ~fields:(fun _ ->
        [ field "creator" ~typ:(non_null public_key)
            ~doc:"Public key of account that produced this block"
            ~deprecated:(Deprecated (Some "use creatorAccount field instead"))
            ~args:Arg.[]
            ~resolve:(fun _ {With_hash.data; _} -> data.creator)
        ; field "creatorAccount"
            ~typ:(non_null AccountObj.account)
            ~doc:"Account that produced this block"
            ~args:Arg.[]
            ~resolve:(fun {ctx= coda; _} {With_hash.data; _} ->
              AccountObj.get_best_ledger_account coda data.creator )
        ; field "stateHash" ~typ:(non_null string)
            ~doc:"Base58Check-encoded hash of the state after this block"
            ~args:Arg.[]
            ~resolve:(fun _ {With_hash.hash; _} ->
              State_hash.to_base58_check hash )
        ; field "stateHashField" ~typ:(non_null string)
            ~doc:
              "Experimental: Bigint field-element representation of stateHash"
            ~args:Arg.[]
            ~resolve:(fun _ {With_hash.hash; _} ->
              State_hash.to_decimal_string hash )
        ; field "protocolState" ~typ:(non_null protocol_state)
            ~args:Arg.[]
            ~resolve:(fun _ {With_hash.data; _} -> data.protocol_state)
        ; field "protocolStateProof"
            ~typ:(non_null protocol_state_proof)
            ~doc:"Snark proof of blockchain state"
            ~args:Arg.[]
            ~resolve:(fun _ {With_hash.data; _} -> data.proof)
        ; field "transactions" ~typ:(non_null transactions)
            ~args:Arg.[]
            ~resolve:(fun _ {With_hash.data; _} -> data.transactions)
        ; field "snarkJobs"
            ~typ:(non_null @@ list @@ non_null completed_work)
            ~args:Arg.[]
            ~resolve:(fun _ {With_hash.data; _} -> data.snark_jobs) ] )

  let snark_worker =
    obj "SnarkWorker" ~fields:(fun _ ->
        [ field "key" ~typ:(non_null public_key)
            ~doc:"Public key of current snark worker"
            ~deprecated:(Deprecated (Some "use account field instead"))
            ~args:Arg.[]
            ~resolve:(fun (_ : Coda_lib.t resolve_info) (key, _) -> key)
        ; field "account"
            ~typ:(non_null AccountObj.account)
            ~doc:"Account of the current snark worker"
            ~args:Arg.[]
            ~resolve:(fun {ctx= coda; _} (key, _) ->
              AccountObj.get_best_ledger_account coda key )
        ; field "fee" ~typ:(non_null uint64)
            ~doc:"Fee that snark worker is charging to generate a snark proof"
            ~args:Arg.[]
            ~resolve:(fun (_ : Coda_lib.t resolve_info) (_, fee) ->
              Currency.Fee.to_uint64 fee ) ] )

  module Payload = struct
    let create_account : (Coda_lib.t, Account.key option) typ =
      obj "AddAccountPayload" ~fields:(fun _ ->
          [ field "publicKey" ~typ:(non_null public_key)
              ~doc:"Public key of the created account"
              ~deprecated:(Deprecated (Some "use account field instead"))
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id)
          ; field "account"
              ~typ:(non_null AccountObj.account)
              ~doc:"Details of created account"
              ~args:Arg.[]
              ~resolve:(fun {ctx= coda; _} key ->
                AccountObj.get_best_ledger_account coda key ) ] )

    let unlock_account : (Coda_lib.t, Account.key option) typ =
      obj "UnlockPayload" ~fields:(fun _ ->
          [ field "publicKey" ~typ:(non_null public_key)
              ~doc:"Public key of the unlocked account"
              ~deprecated:(Deprecated (Some "use account field instead"))
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id)
          ; field "account"
              ~typ:(non_null AccountObj.account)
              ~doc:"Details of unlocked account"
              ~args:Arg.[]
              ~resolve:(fun {ctx= coda; _} key ->
                AccountObj.get_best_ledger_account coda key ) ] )

    let lock_account : (Coda_lib.t, Account.key option) typ =
      obj "LockPayload" ~fields:(fun _ ->
          [ field "publicKey" ~typ:(non_null public_key)
              ~doc:"Public key of the locked account"
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id)
          ; field "account"
              ~typ:(non_null AccountObj.account)
              ~doc:"Details of locked account"
              ~args:Arg.[]
              ~resolve:(fun {ctx= coda; _} key ->
                AccountObj.get_best_ledger_account coda key ) ] )

    let delete_account =
      obj "DeleteAccountPayload" ~fields:(fun _ ->
          [ field "publicKey" ~typ:(non_null public_key)
              ~doc:"Public key of the deleted account"
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id) ] )

    let reload_accounts =
      obj "ReloadAccountsPayload" ~fields:(fun _ ->
          [ field "success" ~typ:(non_null bool)
              ~doc:"True when the reload was successful"
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id) ] )

    let string_of_banned_status = function
      | Trust_system.Banned_status.Unbanned ->
          None
      | Banned_until tm ->
          Some (Time.to_string tm)

    let trust_status =
      obj "TrustStatusPayload" ~fields:(fun _ ->
          let open Trust_system.Peer_status in
          [ field "ip_addr" ~typ:(non_null string) ~doc:"IP address"
              ~args:Arg.[]
              ~resolve:(fun _ (ip_addr, _) -> Unix.Inet_addr.to_string ip_addr)
          ; field "trust" ~typ:(non_null float) ~doc:"Trust score"
              ~args:Arg.[]
              ~resolve:(fun _ (_, {trust; _}) -> trust)
          ; field "banned_status" ~typ:string ~doc:"Banned status"
              ~args:Arg.[]
              ~resolve:(fun _ (_, {banned; _}) ->
                string_of_banned_status banned ) ] )

    let send_payment =
      obj "SendPaymentPayload" ~fields:(fun _ ->
          [ field "payment" ~typ:(non_null user_command)
              ~doc:"Payment that was sent"
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id) ] )

    let send_delegation =
      obj "SendDelegationPayload" ~fields:(fun _ ->
          [ field "delegation" ~typ:(non_null user_command)
              ~doc:"Delegation change that was sent"
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id) ] )

    let add_payment_receipt =
      obj "AddPaymentReceiptPayload" ~fields:(fun _ ->
          [ field "payment" ~typ:(non_null user_command)
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id) ] )

    let set_staking =
      obj "SetStakingPayload" ~fields:(fun _ ->
          [ field "lastStaking"
              ~doc:"Returns the public keys that were staking funds previously"
              ~typ:(non_null (list (non_null public_key)))
              ~args:Arg.[]
              ~resolve:(fun _ (lastStaking, _, _) -> lastStaking)
          ; field "lockedPublicKeys"
              ~doc:
                "List of public keys that could not be used to stake because \
                 they were locked"
              ~typ:(non_null (list (non_null public_key)))
              ~args:Arg.[]
              ~resolve:(fun _ (_, locked, _) -> locked)
          ; field "currentStakingKeys"
              ~doc:"Returns the public keys that are now staking their funds"
              ~typ:(non_null (list (non_null public_key)))
              ~args:Arg.[]
              ~resolve:(fun _ (_, _, currentStaking) -> currentStaking) ] )

    let set_snark_work_fee =
      obj "SetSnarkWorkFeePayload" ~fields:(fun _ ->
          [ field "lastFee" ~doc:"Returns the last fee set to do snark work"
              ~typ:(non_null uint64)
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id) ] )

    let set_snark_worker =
      obj "SetSnarkWorkerPayload" ~fields:(fun _ ->
          [ field "lastSnarkWorker"
              ~doc:
                "Returns the last public key that was designated for snark work"
              ~typ:public_key
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id) ] )
  end

  module Arguments = struct
    let ip_address ~name ip_addr =
      result_of_exn Unix.Inet_addr.of_string ip_addr
        ~error:(sprintf !"%s is not valid." name)
  end

  module Input = struct
    open Schema.Arg

    let public_key_arg =
      scalar "PublicKey" ~doc:"Base58Check-encoded public key string"
        ~coerce:(fun key ->
          match key with
          | `String s ->
              Public_key.Compressed.of_base58_check s
              |> Result.map_error ~f:(fun _ -> "Could not decode public key.")
          | _ ->
              Error "Invalid format for public key." )

    module type Numeric_type = sig
      type t

      val of_string : string -> t

      val of_int : int -> t
    end

    (** Converts a type into a graphql argument type. Expect name to start with uppercase    *)
    let make_numeric_arg (type t) ~name
        (module Numeric : Numeric_type with type t = t) =
      let lower_name = String.lowercase name in
      scalar name
        ~doc:
          (sprintf
             "String or Integer representation of a %s number. If the input \
              is a string, it must represent the number in base 10"
             lower_name) ~coerce:(fun key ->
          match key with
          | `String s ->
              result_of_exn Numeric.of_string s
                ~error:(sprintf "Could not decode %s." lower_name)
          | `Int n ->
              if n < 0 then
                Error
                  (sprintf "Could not convert negative number to %s."
                     lower_name)
              else Ok (Numeric.of_int n)
          | _ ->
              Error (sprintf "Invalid format for %s type." lower_name) )

    let uint64_arg = make_numeric_arg ~name:"UInt64" (module Unsigned.UInt64)

    let uint32_arg = make_numeric_arg ~name:"UInt32" (module Unsigned.UInt32)

    let signature_arg =
      obj "SignatureInput"
        ~coerce:(fun field scalar ->
          let open Snark_params.Tick in
          (Field.of_string field, Inner_curve.Scalar.of_string scalar) )
        ~doc:"A cryptographic signature"
        ~fields:
          [ arg "field" ~typ:(non_null string)
              ~doc:"Field component of signature"
          ; arg "scalar" ~typ:(non_null string)
              ~doc:"Scalar component of signature" ]

    module Fields = struct
      let from ~doc = arg "from" ~typ:(non_null public_key_arg) ~doc

      let to_ ~doc = arg "to" ~typ:(non_null public_key_arg) ~doc

      let fee ~doc = arg "fee" ~typ:(non_null uint64_arg) ~doc

      let memo =
        arg "memo" ~typ:string
          ~doc:"Short arbitrary message provided by the sender"

      let valid_until =
        arg "validUntil" ~typ:uint32_arg
          ~doc:
            "The global slot number after which this transaction cannot be \
             applied"

      let nonce =
        arg "nonce" ~typ:uint32_arg
          ~doc:
            "Should only be set when cancelling transactions, otherwise a \
             nonce is determined automatically"

      let signature =
        arg "signature" ~typ:signature_arg
          ~doc:
            "If a signature is provided, this transaction is considered \
             signed and will be broadcasted to the network without requiring \
             a private key"
    end

    let send_payment =
      let open Fields in
      obj "SendPaymentInput"
        ~coerce:(fun from to_ amount fee valid_until memo nonce ->
          (from, to_, amount, fee, valid_until, memo, nonce) )
        ~fields:
          [ from ~doc:"Public key of sender of payment"
          ; to_ ~doc:"Public key of recipient of payment"
          ; arg "amount" ~doc:"Amount of coda to send to to receiver"
              ~typ:(non_null uint64_arg)
          ; fee ~doc:"Fee amount in order to send payment"
          ; valid_until
          ; memo
          ; nonce ]

    let send_delegation =
      let open Fields in
      obj "SendDelegationInput"
        ~coerce:(fun from to_ fee valid_until memo nonce ->
          (from, to_, fee, valid_until, memo, nonce) )
        ~fields:
          [ from ~doc:"Public key of sender of a stake delegation"
          ; to_ ~doc:"Public key of the account being delegated to"
          ; fee ~doc:"Fee amount in order to send a stake delegation"
          ; valid_until
          ; memo
          ; nonce ]

    let create_account =
      obj "AddAccountInput" ~coerce:Fn.id
        ~fields:
          [ arg "password" ~doc:"Password used to encrypt the new account"
              ~typ:(non_null string) ]

    let unlock_account =
      obj "UnlockInput"
        ~coerce:(fun password pk -> (password, pk))
        ~fields:
          [ arg "password" ~doc:"Password for the account to be unlocked"
              ~typ:(non_null string)
          ; arg "publicKey"
              ~doc:"Public key specifying which account to unlock"
              ~typ:(non_null public_key_arg) ]

    let create_hd_account =
      obj "CreateHDAccountInput" ~coerce:Fn.id
        ~fields:
          [ arg "index" ~doc:"Index of the account in hardware wallet"
              ~typ:(non_null uint32_arg) ]

    let lock_account =
      obj "LockInput" ~coerce:Fn.id
        ~fields:
          [ arg "publicKey" ~doc:"Public key specifying which account to lock"
              ~typ:(non_null public_key_arg) ]

    let delete_account =
      obj "DeleteAccountInput" ~coerce:Fn.id
        ~fields:
          [ arg "publicKey" ~doc:"Public key of account to be deleted"
              ~typ:(non_null public_key_arg) ]

    let reset_trust_status =
      obj "ResetTrustStatusInput" ~coerce:Fn.id
        ~fields:[arg "ipAddress" ~typ:(non_null string)]

    (* TODO: Treat cases where filter_input has a null argument *)
    let block_filter_input =
      obj "BlockFilterInput" ~coerce:Fn.id
        ~fields:
          [ arg "relatedTo"
              ~doc:
                "A public key of a user who has their\n\
                \        transaction in the block, or produced the block"
              ~typ:(non_null public_key_arg) ]

    let user_command_filter_input =
      obj "UserCommandFilterType" ~coerce:Fn.id
        ~fields:
          [ arg "toOrFrom"
              ~doc:
                "Public key of sender or receiver of transactions you are \
                 looking for"
              ~typ:(non_null public_key_arg) ]

    let set_staking =
      obj "SetStakingInput" ~coerce:Fn.id
        ~fields:
          [ arg "publicKeys"
              ~typ:(non_null (list (non_null public_key_arg)))
              ~doc:
                "Public keys of accounts you wish to stake with - these must \
                 be accounts that are in trackedAccounts" ]

    let set_snark_work_fee =
      obj "SetSnarkWorkFee"
        ~fields:[Fields.fee ~doc:"Fee to get rewarded for producing snark work"]
        ~coerce:Fn.id

    let set_snark_worker =
      obj "SetSnarkWorkerInput" ~coerce:Fn.id
        ~fields:
          [ arg "publicKey" ~typ:public_key_arg
              ~doc:
                "Public key you wish to start snark-working on; null to stop \
                 doing any snark work" ]

    module AddPaymentReceipt = struct
      type t = {payment: string; added_time: string}

      let typ =
        obj "AddPaymentReceiptInput"
          ~coerce:(fun payment added_time -> {payment; added_time})
          ~fields:
            [ arg "payment"
                ~doc:(Doc.bin_prot "Serialized payment")
                ~typ:(non_null string)
            ; (* TODO: create a formal method for verifying that the provided added_time is correct  *)
              arg "added_time" ~typ:(non_null string)
                ~doc:
                  (Doc.date
                     "Time that a payment gets added to another clients \
                      transaction database") ]
    end
  end

  module Pagination = struct
    module User_command = struct
      module Inputs = struct
        module Type = struct
          type t = User_command.t

          let typ = user_command

          let name = "UserCommand"
        end

        module Cursor = struct
          type t = User_command.t

          let serialize = User_command.to_base58_check

          let deserialize ?error serialized_payment =
            result_of_or_error
              (User_command.of_base58_check serialized_payment)
              ~error:(Option.value error ~default:"Invalid cursor")

          let doc = Doc.bin_prot "Opaque pagination cursor for a user command"
        end

        module Pagination_database = Auxiliary_database.Transaction_database

        let get_database = Coda_lib.transaction_database

        let filter_argument = Input.user_command_filter_input

        let query_name = "userCommands"

        let to_cursor = Fn.id
      end

      include Pagination.Make (Inputs)
    end

    module Blocks = struct
      module Inputs = struct
        module Type = struct
          type t =
            ( Auxiliary_database.Filtered_external_transition.t
            , State_hash.t )
            With_hash.t

          let typ = block

          let name = "Block"
        end

        module Cursor = struct
          type t = State_hash.t

          let serialize = State_hash.to_base58_check

          let deserialize ?error data =
            result_of_or_error
              (State_hash.of_base58_check data)
              ~error:(Option.value error ~default:"Invalid state hash data")

          let doc = Doc.bin_prot "Opaque pagination cursor for a block"
        end

        module Pagination_database =
          Auxiliary_database.External_transition_database

        let get_database = Coda_lib.external_transition_database

        let filter_argument = Input.block_filter_input

        let query_name = "blocks"

        let to_cursor {With_hash.hash; _} = hash
      end

      include Pagination.Make (Inputs)
    end
  end
end

module Subscriptions = struct
  open Schema

  let new_sync_update =
    subscription_field "newSyncUpdate"
      ~doc:"Event that triggers when the network sync status changes"
      ~deprecated:NotDeprecated
      ~typ:(non_null Types.sync_status)
      ~args:Arg.[]
      ~resolve:(fun {ctx= coda; _} ->
        Coda_lib.sync_status coda |> Coda_incremental.Status.to_pipe
        |> Deferred.Result.return )

  let new_block =
    subscription_field "newBlock"
      ~doc:
        "Event that triggers when a new block is created that either contains \
         a transaction with the specified public key, or was produced by it. \
         If no public key is provided, then the event will trigger for every \
         new block received"
      ~typ:(non_null Types.block)
      ~args:
        Arg.
          [ arg "publicKey" ~doc:"Public key that is included in the block"
              ~typ:Types.Input.public_key_arg ]
      ~resolve:(fun {ctx= coda; _} public_key ->
        Deferred.Result.return
        @@ Coda_commands.Subscriptions.new_block coda public_key )

  let chain_reorganization =
    subscription_field "chainReorganization"
      ~doc:
        "Event that triggers when the best tip changes in a way that is not a \
         trivial extension of the existing one"
      ~typ:(non_null Types.chain_reorganization_status)
      ~args:Arg.[]
      ~resolve:(fun {ctx= coda; _} ->
        Deferred.Result.return
        @@ Coda_commands.Subscriptions.reorganization coda )

  let commands = [new_sync_update; new_block; chain_reorganization]
end

module Mutations = struct
  open Schema

  let create_account_resolver {ctx= t; _} () password =
    let password = lazy (return (Bytes.of_string password)) in
    let%map pk =
      Coda_lib.wallets t |> Secrets.Wallets.generate_new ~password
    in
    Coda_lib.subscriptions t |> Coda_lib.Subscriptions.add_new_subscription ~pk ;
    Result.return pk

  let add_wallet =
    io_field "addWallet"
      ~doc:
        "Add a wallet - this will create a new keypair and store it in the \
         daemon"
      ~deprecated:(Deprecated (Some "use createAccount instead"))
      ~typ:(non_null Types.Payload.create_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.create_account)]
      ~resolve:create_account_resolver

  let create_account =
    io_field "createAccount"
      ~doc:
        "Create a new account - this will create a new keypair and store it \
         in the daemon"
      ~typ:(non_null Types.Payload.create_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.create_account)]
      ~resolve:create_account_resolver

  let create_hd_account : (Coda_lib.t, unit) field =
    io_field "createHDAccount"
      ~doc:Secrets.Hardware_wallets.create_hd_account_summary
      ~typ:(non_null Types.Payload.create_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.create_hd_account)]
      ~resolve:(fun {ctx= coda; _} () hd_index ->
        Coda_lib.wallets coda |> Secrets.Wallets.create_hd_account ~hd_index )

  let unlock_account_resolver {ctx= t; _} () (password, pk) =
    let password = lazy (return (Bytes.of_string password)) in
    match%map
      Coda_lib.wallets t |> Secrets.Wallets.unlock ~needle:pk ~password
    with
    | Error `Not_found ->
        Error "Could not find owned account associated with provided key"
    | Error `Bad_password ->
        Error "Wrong password provided"
    | Ok () ->
        Ok pk

  let unlock_wallet =
    io_field "unlockWallet"
      ~doc:"Allow transactions to be sent from the unlocked account"
      ~deprecated:(Deprecated (Some "use unlockAccount instead"))
      ~typ:(non_null Types.Payload.unlock_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.unlock_account)]
      ~resolve:unlock_account_resolver

  let unlock_account =
    io_field "unlockAccount"
      ~doc:"Allow transactions to be sent from the unlocked account"
      ~typ:(non_null Types.Payload.unlock_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.unlock_account)]
      ~resolve:unlock_account_resolver

  let lock_account_resolver {ctx= t; _} () pk =
    Coda_lib.wallets t |> Secrets.Wallets.lock ~needle:pk ;
    pk

  let lock_wallet =
    field "lockWallet"
      ~doc:"Lock an unlocked account to prevent transaction being sent from it"
      ~deprecated:(Deprecated (Some "use lockAccount instead"))
      ~typ:(non_null Types.Payload.lock_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.lock_account)]
      ~resolve:lock_account_resolver

  let lock_account =
    field "lockAccount"
      ~doc:"Lock an unlocked account to prevent transaction being sent from it"
      ~typ:(non_null Types.Payload.lock_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.lock_account)]
      ~resolve:lock_account_resolver

  let delete_account_resolver {ctx= coda; _} () public_key =
    let open Deferred.Result.Let_syntax in
    let wallets = Coda_lib.wallets coda in
    let%map () =
      Deferred.Result.map_error
        ~f:(fun `Not_found ->
          "Could not find account with specified public key" )
        (Secrets.Wallets.delete wallets public_key)
    in
    public_key

  let delete_wallet =
    io_field "deleteWallet"
      ~doc:"Delete the private key for an account that you track"
      ~deprecated:(Deprecated (Some "use deleteAccount instead"))
      ~typ:(non_null Types.Payload.delete_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.delete_account)]
      ~resolve:delete_account_resolver

  let delete_account =
    io_field "deleteAccount"
      ~doc:"Delete the private key for an account that you track"
      ~typ:(non_null Types.Payload.delete_account)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.delete_account)]
      ~resolve:delete_account_resolver

  let reload_account_resolver {ctx= coda; _} () =
    let%map _ =
      Secrets.Wallets.reload ~logger:(Logger.create ()) (Coda_lib.wallets coda)
    in
    Ok true

  let reload_wallets =
    io_field "reloadWallets"
      ~doc:"Reload tracked account information from disk"
      ~deprecated:(Deprecated (Some "use reloadAccounts instead"))
      ~typ:(non_null Types.Payload.reload_accounts)
      ~args:Arg.[]
      ~resolve:reload_account_resolver

  let reload_accounts =
    io_field "reloadAccounts"
      ~doc:"Reload tracked account information from disk"
      ~typ:(non_null Types.Payload.reload_accounts)
      ~args:Arg.[]
      ~resolve:reload_account_resolver

  let reset_trust_status =
    io_field "resetTrustStatus"
      ~doc:"Reset trust status for a given IP address"
      ~typ:(non_null Types.Payload.trust_status)
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.reset_trust_status)]
      ~resolve:(fun {ctx= coda; _} () ip_address_input ->
        let open Deferred.Result.Let_syntax in
        let%map ip_address =
          Deferred.return
          @@ Types.Arguments.ip_address ~name:"ip_address" ip_address_input
        in
        (ip_address, Coda_commands.reset_trust_status coda ip_address) )

  let send_user_command coda user_command_input =
    match
      Coda_commands.setup_and_submit_user_command coda user_command_input
    with
    | `Active f -> (
        match%map f with
        | Ok (user_command, _receipt) ->
            Ok user_command
        | Error e ->
            Error ("Couldn't send user_command: " ^ Error.to_string_hum e) )
    | `Bootstrapping ->
        return (Error "Daemon is bootstrapping")

  let find_identity ~public_key coda =
    Result.of_option
      (Secrets.Wallets.find_identity (Coda_lib.wallets coda) ~needle:public_key)
      ~error:
        "Couldn't find an unlocked key for specified `sender`. Did you unlock \
         the account you're making a transaction from?"

  let create_user_command_input ~fee ~fee_token ~fee_payer_pk ~nonce_opt
      ~valid_until ~memo ~signer ~body ~sign_choice :
      (User_command_input.t, string) result =
    let open Result.Let_syntax in
    (* TODO: We should put a more sensible default here. *)
    let valid_until =
      Option.value_map ~default:Coda_numbers.Global_slot.max_value
        ~f:Coda_numbers.Global_slot.of_uint32 valid_until
    in
    let%bind fee =
      result_of_exn Currency.Fee.of_uint64 fee
        ~error:(sprintf "Invalid `fee` provided.")
    in
    let%bind () =
      Result.ok_if_true
        Currency.Fee.(fee >= User_command.minimum_fee)
        ~error:
          (sprintf
             !"Invalid user command. Fee %s is less than the minimum fee, %s."
             (Currency.Fee.to_formatted_string fee)
             (Currency.Fee.to_formatted_string User_command.minimum_fee))
    in
    let%map memo =
      Option.value_map memo ~default:(Ok User_command_memo.empty)
        ~f:(fun memo ->
          result_of_exn User_command_memo.create_from_string_exn memo
            ~error:"Invalid `memo` provided." )
    in
    User_command_input.create ~signer ~fee ~fee_token ~fee_payer_pk
      ?nonce:nonce_opt ~valid_until ~memo ~body ~sign_choice ()

  let send_signed_user_command ~signature ~coda ~nonce_opt ~signer ~memo ~fee
      ~fee_token ~fee_payer_pk ~valid_until ~body =
    let open Deferred.Result.Let_syntax in
    let%bind user_command_input =
      create_user_command_input ~nonce_opt ~signer ~memo ~fee ~fee_token
        ~fee_payer_pk ~valid_until ~body
        ~sign_choice:(User_command_input.Sign_choice.Signature signature)
      |> Deferred.return
    in
    send_user_command coda user_command_input

  let send_unsigned_user_command ~coda ~nonce_opt ~signer ~memo ~fee ~fee_token
      ~fee_payer_pk ~valid_until ~body =
    let open Deferred.Result.Let_syntax in
    let%bind user_command_input =
      (let open Result.Let_syntax in
      let%bind sign_choice =
        match%map find_identity ~public_key:signer coda with
        | `Keypair sender_kp ->
            User_command_input.Sign_choice.Keypair sender_kp
        | `Hd_index hd_index ->
            Hd_index hd_index
      in
      create_user_command_input ~nonce_opt ~signer ~memo ~fee ~fee_token
        ~fee_payer_pk ~valid_until ~body ~sign_choice)
      |> Deferred.return
    in
    send_user_command coda user_command_input

  let send_delegation =
    io_field "sendDelegation"
      ~doc:"Change your delegate by sending a transaction"
      ~typ:(non_null Types.Payload.send_delegation)
      ~args:
        Arg.
          [ arg "input" ~typ:(non_null Types.Input.send_delegation)
          ; Types.Input.Fields.signature ]
      ~resolve:
        (fun {ctx= coda; _} () (from, to_, fee, valid_until, memo, nonce_opt)
             signature ->
        let body =
          User_command_payload.Body.Stake_delegation
            (Set_delegate {delegator= from; new_delegate= to_})
        in
        (* TODO: Multiple tokens. *)
        let fee_token = Token_id.default in
        match signature with
        | None ->
            send_unsigned_user_command ~coda ~nonce_opt ~signer:from ~memo ~fee
              ~fee_token ~fee_payer_pk:from ~valid_until ~body
        | Some signature ->
            send_signed_user_command ~coda ~nonce_opt ~signer:from ~memo ~fee
              ~fee_token ~fee_payer_pk:from ~valid_until ~body ~signature )

  let send_payment =
    io_field "sendPayment" ~doc:"Send a payment"
      ~typ:(non_null Types.Payload.send_payment)
      ~args:
        Arg.
          [ arg "input" ~typ:(non_null Types.Input.send_payment)
          ; Types.Input.Fields.signature ]
      ~resolve:
        (fun {ctx= coda; _} ()
             (from, to_, amount, fee, valid_until, memo, nonce_opt) signature ->
        (* TODO: Multiple tokens. *)
        let body =
          User_command_payload.Body.Payment
            { source_pk= from
            ; receiver_pk= to_
            ; token_id= Token_id.default
            ; amount= Amount.of_uint64 amount }
        in
        let fee_token = Token_id.default in
        match signature with
        | None ->
            send_unsigned_user_command ~coda ~nonce_opt ~signer:from ~memo ~fee
              ~fee_token ~fee_payer_pk:from ~valid_until ~body
        | Some signature ->
            send_signed_user_command ~coda ~nonce_opt ~signer:from ~memo ~fee
              ~fee_token ~fee_payer_pk:from ~valid_until ~body ~signature )

  let add_payment_receipt =
    result_field "addPaymentReceipt"
      ~doc:"Add payment into transaction database"
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.AddPaymentReceipt.typ)]
      ~typ:Types.Payload.add_payment_receipt
      ~resolve:
        (fun {ctx= coda; _} ()
             {Types.Input.AddPaymentReceipt.payment; added_time} ->
        let open Result.Let_syntax in
        let%bind added_time =
          result_of_exn Block_time.Time.of_string_exn added_time
            ~error:"Invalid `time` provided"
        in
        let%map payment =
          Types.Pagination.User_command.Inputs.Cursor.deserialize
            ~error:"Invaid `payment` provided" payment
        in
        let transaction_database = Coda_lib.transaction_database coda in
        Auxiliary_database.Transaction_database.add transaction_database
          payment added_time ;
        Some payment )

  let set_staking =
    field "setStaking" ~doc:"Set keys you wish to stake with"
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.set_staking)]
      ~typ:(non_null Types.Payload.set_staking)
      ~resolve:(fun {ctx= coda; _} () pks ->
        let old_block_production_keys =
          Coda_lib.block_production_pubkeys coda
        in
        let wallet = Coda_lib.wallets coda in
        let unlocked, locked =
          List.partition_map pks ~f:(fun pk ->
              match Secrets.Wallets.find_unlocked ~needle:pk wallet with
              | Some kp ->
                  `Fst (kp, pk)
              | None ->
                  `Snd pk )
        in
        ignore
        @@ Coda_lib.replace_block_production_keypairs coda
             (Keypair.And_compressed_pk.Set.of_list unlocked) ;
        ( Public_key.Compressed.Set.to_list old_block_production_keys
        , locked
        , List.map ~f:Tuple.T2.get2 unlocked ) )

  let set_snark_worker =
    io_field "setSnarkWorker"
      ~doc:"Set key you wish to snark work with or disable snark working"
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.set_snark_worker)]
      ~typ:(non_null Types.Payload.set_snark_worker)
      ~resolve:(fun {ctx= coda; _} () pk ->
        let old_snark_worker_key = Coda_lib.snark_worker_key coda in
        let%map () = Coda_lib.replace_snark_worker_key coda pk in
        Ok old_snark_worker_key )

  let set_snark_work_fee =
    result_field "setSnarkWorkFee"
      ~doc:"Set fee that you will like to receive for doing snark work"
      ~args:Arg.[arg "input" ~typ:(non_null Types.Input.set_snark_work_fee)]
      ~typ:(non_null Types.Payload.set_snark_work_fee)
      ~resolve:(fun {ctx= coda; _} () raw_fee ->
        let open Result.Let_syntax in
        let%map fee =
          result_of_exn Currency.Fee.of_uint64 raw_fee
            ~error:"Invalid snark work `fee` provided."
        in
        let last_fee = Coda_lib.snark_work_fee coda in
        Coda_lib.set_snark_work_fee coda fee ;
        Currency.Fee.to_uint64 last_fee )

  let commands =
    [ add_wallet
    ; create_account
    ; create_hd_account
    ; unlock_account
    ; unlock_wallet
    ; lock_account
    ; lock_wallet
    ; delete_account
    ; delete_wallet
    ; reload_accounts
    ; reload_wallets
    ; send_payment
    ; send_delegation
    ; add_payment_receipt
    ; set_staking
    ; set_snark_worker
    ; set_snark_work_fee ]
end

module Queries = struct
  open Schema

  let pooled_user_commands =
    field "pooledUserCommands"
      ~doc:
        "Retrieve all the scheduled user commands for a specified sender that \
         the current daemon sees in their transaction pool. All scheduled \
         commands are queried if no sender is specified"
      ~typ:(non_null @@ list @@ non_null Types.user_command)
      ~args:
        Arg.
          [ arg "publicKey" ~doc:"Public key of sender of pooled user commands"
              ~typ:Types.Input.public_key_arg ]
      ~resolve:(fun {ctx= coda; _} () opt_pk ->
        let transaction_pool = Coda_lib.transaction_pool coda in
        let resource_pool =
          Network_pool.Transaction_pool.resource_pool transaction_pool
        in
        ( match opt_pk with
        | None ->
            Network_pool.Transaction_pool.Resource_pool.transactions
              ~logger:(Coda_lib.top_level_logger coda)
              resource_pool
            |> Sequence.to_list
        | Some pk ->
            let account_id = Account_id.create pk Token_id.default in
            Network_pool.Transaction_pool.Resource_pool.all_from_account
              resource_pool account_id )
        |> List.map ~f:User_command.forget_check )

  let sync_state =
    result_field_no_inputs "syncStatus" ~doc:"Network sync status" ~args:[]
      ~typ:(non_null Types.sync_status) ~resolve:(fun {ctx= coda; _} () ->
        Result.map_error
          (Coda_incremental.Status.Observer.value @@ Coda_lib.sync_status coda)
          ~f:Error.to_string_hum )

  let daemon_status =
    io_field "daemonStatus" ~doc:"Get running daemon status" ~args:[]
      ~typ:(non_null Types.DaemonStatus.t) ~resolve:(fun {ctx= coda; _} () ->
        Coda_commands.get_status ~flag:`Performance coda >>| Result.return )

  let trust_status =
    field "trustStatus" ~typ:Types.Payload.trust_status
      ~args:Arg.[arg "ipAddress" ~typ:(non_null string)]
      ~doc:"Trust status for an IPv4 or IPv6 address"
      ~resolve:(fun {ctx= coda; _} () (ip_addr_string : string) ->
        match Types.Arguments.ip_address ~name:"ipAddress" ip_addr_string with
        | Ok ip_addr ->
            Some (ip_addr, Coda_commands.get_trust_status coda ip_addr)
        | Error _ ->
            None )

  let trust_status_all =
    field "trustStatusAll"
      ~typ:(non_null @@ list @@ non_null Types.Payload.trust_status)
      ~args:Arg.[]
      ~doc:"IP address and trust status for all peers"
      ~resolve:(fun {ctx= coda; _} () ->
        Coda_commands.get_trust_status_all coda )

  let version =
    field "version" ~typ:string
      ~args:Arg.[]
      ~doc:"The version of the node (git commit hash)"
      ~resolve:(fun _ _ -> Some Coda_version.commit_id)

  let tracked_accounts_resolver {ctx= coda; _} () =
    let wallets = Coda_lib.wallets coda in
    let block_production_pubkeys = Coda_lib.block_production_pubkeys coda in
    wallets |> Secrets.Wallets.pks
    |> List.map ~f:(fun pk ->
           { Types.AccountObj.account=
               Types.AccountObj.Partial_account.of_pk coda pk
           ; locked= Secrets.Wallets.check_locked wallets ~needle:pk
           ; is_actively_staking=
               Public_key.Compressed.Set.mem block_production_pubkeys pk
           ; path= Secrets.Wallets.get_path wallets pk } )

  let owned_wallets =
    field "ownedWallets"
      ~doc:"Wallets for which the daemon knows the private key"
      ~typ:(non_null (list (non_null Types.AccountObj.account)))
      ~deprecated:(Deprecated (Some "use trackedAccounts instead"))
      ~args:Arg.[]
      ~resolve:tracked_accounts_resolver

  let tracked_accounts =
    field "trackedAccounts"
      ~doc:"Accounts for which the daemon tracks the private key"
      ~typ:(non_null (list (non_null Types.AccountObj.account)))
      ~args:Arg.[]
      ~resolve:tracked_accounts_resolver

  let account_resolver {ctx= coda; _} () pk =
    let block_production_pubkeys = Coda_lib.block_production_pubkeys coda in
    let wallets = Coda_lib.wallets coda in
    Some
      { Types.AccountObj.account= Types.AccountObj.Partial_account.of_pk coda pk
      ; locked= Secrets.Wallets.check_locked wallets ~needle:pk
      ; is_actively_staking=
          Public_key.Compressed.Set.mem block_production_pubkeys pk
      ; path= Secrets.Wallets.get_path wallets pk }

  let wallet =
    field "wallet" ~doc:"Find any wallet via a public key"
      ~typ:Types.AccountObj.account
      ~deprecated:(Deprecated (Some "use account instead"))
      ~args:
        Arg.
          [ arg "publicKey" ~doc:"Public key of account being retrieved"
              ~typ:(non_null Types.Input.public_key_arg) ]
      ~resolve:account_resolver

  let account =
    field "account" ~doc:"Find any account via a public key"
      ~typ:Types.AccountObj.account
      ~args:
        Arg.
          [ arg "publicKey" ~doc:"Public key of account being retrieved"
              ~typ:(non_null Types.Input.public_key_arg) ]
      ~resolve:account_resolver

  let transaction_status =
    result_field "transactionStatus" ~doc:"Get the status of a transaction"
      ~typ:(non_null Types.transaction_status)
      ~args:Arg.[arg "payment" ~typ:(non_null guid) ~doc:"Id of a UserCommand"]
      ~resolve:(fun {ctx= coda; _} () serialized_payment ->
        let open Result.Let_syntax in
        let%bind payment =
          Types.Pagination.User_command.Inputs.Cursor.deserialize
            ~error:"Invalid payment provided" serialized_payment
        in
        let frontier_broadcast_pipe = Coda_lib.transition_frontier coda in
        let transaction_pool = Coda_lib.transaction_pool coda in
        Result.map_error
          (Transaction_status.get_status ~frontier_broadcast_pipe
             ~transaction_pool payment)
          ~f:Error.to_string_hum )

  let current_snark_worker =
    field "currentSnarkWorker" ~typ:Types.snark_worker
      ~args:Arg.[]
      ~doc:"Get information about the current snark worker"
      ~resolve:(fun {ctx= coda; _} _ ->
        Option.map (Coda_lib.snark_worker_key coda) ~f:(fun k ->
            (k, Coda_lib.snark_work_fee coda) ) )

  let user_command = Types.Pagination.User_command.query

  let blocks = Types.Pagination.Blocks.query

  let block =
    io_field "block" ~typ:Types.block
      ~args:
        Arg.
          [ arg "stateHash" ~doc:"State hash of the block"
              ~typ:(non_null string) ]
      ~doc:
        "Get information about a single block or null if no block can be found"
      ~resolve:(fun {ctx= coda; _} () state_hash_str ->
        let db = Coda_lib.external_transition_database coda in
        Deferred.return
          (let open Result.Let_syntax in
          let%map state_hash =
            result_of_or_error
              (State_hash.of_base58_check state_hash_str)
              ~error:"Invalid state hash"
          in
          Auxiliary_database.External_transition_database.get_value db
            state_hash) )

  let best_chain =
    field "bestChain"
      ~doc:
        "Retrieve a list of blocks from transition frontier's root to the \
         current best tip. Returns null if the system is bootstrapping."
      ~typ:(list @@ non_null Types.block)
      ~args:Arg.[]
      ~resolve:(fun {ctx= coda; _} () ->
        let open Option.Let_syntax in
        let%map best_chain = Coda_lib.best_chain coda in
        List.map best_chain ~f:(fun breadcrumb ->
            let hash = Transition_frontier.Breadcrumb.state_hash breadcrumb in
            let transition =
              Transition_frontier.Breadcrumb.validated_transition breadcrumb
            in
            let transactions =
              Coda_transition.External_transition.Validated.transactions
                ~constraint_constants:
                  (Coda_lib.config coda).precomputed_values
                    .constraint_constants transition
            in
            With_hash.Stable.Latest.
              { data=
                  Auxiliary_database.Filtered_external_transition.of_transition
                    transition `All transactions
              ; hash } ) )

  let initial_peers =
    field "initialPeers"
      ~doc:"List of peers that the daemon first used to connect to the network"
      ~args:Arg.[]
      ~typ:(non_null @@ list @@ non_null string)
      ~resolve:(fun {ctx= coda; _} () ->
        List.map (Coda_lib.initial_peers coda) ~f:Coda_net2.Multiaddr.to_string
        )

  let snark_pool =
    field "snarkPool"
      ~doc:"List of completed snark works that have the lowest fee so far"
      ~args:Arg.[]
      ~typ:(non_null @@ list @@ non_null Types.completed_work)
      ~resolve:(fun {ctx= coda; _} () ->
        Coda_lib.snark_pool coda |> Network_pool.Snark_pool.resource_pool
        |> Network_pool.Snark_pool.Resource_pool.all_completed_work )

  let pending_snark_work =
    field "pendingSnarkWork" ~doc:"List of snark works that are yet to be done"
      ~args:Arg.[]
      ~typ:(non_null @@ list @@ non_null Types.pending_work)
      ~resolve:(fun {ctx= coda; _} () ->
        match
          Coda_lib.best_staged_ledger coda |> Participating_state.active
        with
        | Some staged_ledger ->
            let snark_pool = Coda_lib.snark_pool coda in
            let fee_opt =
              Coda_lib.(
                Option.map (snark_worker_key coda) ~f:(fun _ ->
                    snark_work_fee coda ))
            in
            let (module S) = Coda_lib.work_selection_method coda in
            S.pending_work_statements ~snark_pool ~fee_opt ~staged_ledger
        | None ->
            [] )

  let commands =
    [ sync_state
    ; daemon_status
    ; version
    ; owned_wallets (* deprecated *)
    ; tracked_accounts
    ; wallet (* deprecated *)
    ; account
    ; current_snark_worker
    ; best_chain
    ; blocks
    ; block
    ; initial_peers
    ; pooled_user_commands
    ; transaction_status
    ; trust_status
    ; trust_status_all
    ; snark_pool
    ; pending_snark_work ]
end

let schema =
  Graphql_async.Schema.(
    schema Queries.commands ~mutations:Mutations.commands
      ~subscriptions:Subscriptions.commands)
