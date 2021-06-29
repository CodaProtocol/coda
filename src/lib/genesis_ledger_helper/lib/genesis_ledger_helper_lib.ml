open Core
open Signature_lib
open Mina_base

module Accounts = struct
  module Single = struct
    let to_account_with_pk :
        Runtime_config.Accounts.Single.t -> Mina_base.Account.t Or_error.t =
     fun t ->
      let open Or_error.Let_syntax in
      let%bind pk =
        match t.pk with
        | Some pk ->
            Ok (Signature_lib.Public_key.Compressed.of_base58_check_exn pk)
        | None ->
            Or_error.errorf
              !"No public key to create the account from runtime config \
                %{sexp: Runtime_config.Accounts.Single.t}"
              t
      in
      let delegate =
        Option.map ~f:Signature_lib.Public_key.Compressed.of_base58_check_exn
          t.delegate
      in
      let token_id =
        Option.value_map t.token ~default:Token_id.default
          ~f:Mina_base.Token_id.of_uint64
      in
      let account_id = Mina_base.Account_id.create pk token_id in
      let account =
        match t.timing with
        | Some
            { initial_minimum_balance
            ; cliff_time
            ; cliff_amount
            ; vesting_period
            ; vesting_increment } ->
            Mina_base.Account.create_timed account_id t.balance
              ~initial_minimum_balance ~cliff_time ~cliff_amount
              ~vesting_period ~vesting_increment
            |> Or_error.ok_exn
        | None ->
            Mina_base.Account.create account_id t.balance
      in
      let permissions =
        match t.permissions with
        | None ->
            account.permissions
        | Some
            { stake
            ; edit_state
            ; send
            ; receive
            ; set_delegate
            ; set_permissions
            ; set_verification_key } ->
            let auth_required a =
              match a with
              | Runtime_config.Accounts.Single.Permissions.Auth_required.None
                ->
                  Mina_base.Permissions.Auth_required.None
              | Either ->
                  Either
              | Proof ->
                  Proof
              | Signature ->
                  Signature
              | Both ->
                  Both
              | Impossible ->
                  Impossible
            in
            { Mina_base.Permissions.Poly.stake
            ; edit_state= auth_required edit_state
            ; send= auth_required send
            ; receive= auth_required receive
            ; set_delegate= auth_required set_delegate
            ; set_permissions= auth_required set_permissions
            ; set_verification_key= auth_required set_verification_key }
      in
      let token_permissions =
        Option.value_map t.token_permissions ~default:account.token_permissions
          ~f:(fun {token_owned; disable_new_accounts; account_disabled} ->
            if token_owned then
              Mina_base.Token_permissions.Token_owned {disable_new_accounts}
            else Not_owned {account_disabled} )
      in
      let%map snapp =
        match t.snapp with
        | None ->
            Ok None
        | Some {state; verification_key} ->
            let%bind app_state =
              if
                Pickles_types.Vector.Nat.to_int Snapp_state.Max_state_size.n
                <> List.length state
              then
                Or_error.errorf
                  !"Snap account state has invalid length %{sexp: \
                    Runtime_config.Accounts.Single.t} length: %d"
                  t (List.length state)
              else Ok (Snapp_state.V.of_list_exn state)
            in
            let%map verification_key =
              (* Use a URI-safe alphabet to make life easier for maintaining json
                   We prefer this to base58-check here because users should not
                   be manually entering verification keys.
                *)
              Option.value_map ~default:(Ok None) verification_key
                ~f:(fun verification_key ->
                  let%map vk =
                    Base64.decode ~alphabet:Base64.uri_safe_alphabet
                      verification_key
                    |> Result.map_error ~f:(function `Msg s ->
                           Error.createf
                             !"Could not parse verification key account \
                               %{sexp:Runtime_config.Accounts.Single.t}: %s"
                             t s )
                    |> Result.map
                         ~f:
                           (Binable.of_string
                              ( module Pickles.Side_loaded.Verification_key
                                       .Stable
                                       .Latest ))
                  in
                  Some
                    (With_hash.of_data ~hash_data:Snapp_account.digest_vk vk)
              )
            in
            Some {Snapp_account.verification_key; app_state}
      in
      { account with
        delegate=
          (if Option.is_some delegate then delegate else account.delegate)
      ; token_id
      ; token_permissions
      ; nonce= Account.Nonce.of_uint32 t.nonce
      ; receipt_chain_hash=
          Option.value_map t.receipt_chain_hash
            ~default:account.receipt_chain_hash
            ~f:Mina_base.Receipt.Chain_hash.of_base58_check_exn
      ; voting_for=
          Option.value_map ~default:account.voting_for
            ~f:Mina_base.State_hash.of_base58_check_exn t.voting_for
      ; snapp
      ; permissions }

    let of_account :
           Mina_base.Account.t
        -> Signature_lib.Private_key.t option
        -> Runtime_config.Accounts.Single.t =
     fun account sk ->
      let timing =
        match account.timing with
        | Account.Timing.Untimed ->
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
      let token_permissions =
        match account.token_permissions with
        | Mina_base.Token_permissions.Token_owned {disable_new_accounts} ->
            Some
              { Runtime_config.Accounts.Single.Token_permissions.token_owned=
                  true
              ; disable_new_accounts
              ; account_disabled= false }
        | Not_owned {account_disabled} ->
            Some
              { token_owned= false
              ; disable_new_accounts= false
              ; account_disabled }
      in
      let permissions =
        let auth_required a =
          match a with
          | Mina_base.Permissions.Auth_required.None ->
              Runtime_config.Accounts.Single.Permissions.Auth_required.None
          | Either ->
              Either
          | Proof ->
              Proof
          | Signature ->
              Signature
          | Both ->
              Both
          | Impossible ->
              Impossible
        in
        let { Mina_base.Permissions.Poly.stake
            ; edit_state
            ; send
            ; receive
            ; set_delegate
            ; set_permissions
            ; set_verification_key } =
          account.permissions
        in
        Some
          { Runtime_config.Accounts.Single.Permissions.stake
          ; edit_state= auth_required edit_state
          ; send= auth_required send
          ; receive= auth_required receive
          ; set_delegate= auth_required set_delegate
          ; set_permissions= auth_required set_permissions
          ; set_verification_key= auth_required set_verification_key }
      in
      let snapp =
        Option.map account.snapp ~f:(fun {app_state; verification_key} ->
            let state = Snapp_state.V.to_list app_state in
            let verification_key =
              Option.map verification_key ~f:(fun vk ->
                  With_hash.data vk
                  |> Binable.to_string
                       ( module Pickles.Side_loaded.Verification_key.Stable
                                .Latest )
                  |> Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet )
            in
            { Runtime_config.Accounts.Single.Snapp_account.state
            ; verification_key } )
      in
      { pk=
          Some
            (Signature_lib.Public_key.Compressed.to_base58_check
               account.public_key)
      ; sk= Option.map ~f:Signature_lib.Private_key.to_base58_check sk
      ; balance= account.balance
      ; delegate=
          Option.map ~f:Signature_lib.Public_key.Compressed.to_base58_check
            account.delegate
      ; timing
      ; token= Some (Mina_base.Token_id.to_uint64 account.token_id)
      ; token_permissions
      ; nonce= account.nonce
      ; receipt_chain_hash=
          Some
            (Mina_base.Receipt.Chain_hash.to_base58_check
               account.receipt_chain_hash)
      ; voting_for=
          Some (Mina_base.State_hash.to_base58_check account.voting_for)
      ; snapp
      ; permissions }
  end

  let to_full :
      Runtime_config.Accounts.t -> (Private_key.t option * Account.t) list =
    List.mapi
      ~f:(fun i ({Runtime_config.Accounts.pk; sk; _} as account_config) ->
        let sk =
          match sk with
          | Some sk -> (
            match Private_key.of_yojson (`String sk) with
            | Ok sk ->
                Some sk
            | Error err ->
                Error.(raise (of_string err)) )
          | None ->
              None
        in
        let pk =
          match pk with
          | Some pk ->
              pk
          | None ->
              Public_key.Compressed.to_base58_check
                (Quickcheck.random_value
                   ~seed:
                     (`Deterministic
                       ("fake pk for genesis ledger " ^ string_of_int i))
                   Public_key.Compressed.gen)
        in
        let account =
          Single.to_account_with_pk {account_config with pk= Some pk}
          |> Or_error.ok_exn
        in
        (sk, account) )

  let gen_with_balance balance :
      (Private_key.t option * Account.t) Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    let%map pk = Signature_lib.Public_key.Compressed.gen in
    (None, Account.create (Account_id.create pk Token_id.default) balance)

  let gen : (Private_key.t option * Account.t) Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    let%bind balance = Int.gen_incl 10 500 >>| Currency.Balance.of_int in
    gen_with_balance balance

  let generate n : (Private_key.t option * Account.t) list =
    let open Quickcheck in
    random_value ~seed:(`Deterministic "fake accounts for genesis ledger")
      (Generator.list_with_length n gen)

  (* This implements a tail-recursive generator using the low-level primitives
     so that we don't blow out the stack.
  *)
  let gen_balances_rev balances :
      (Private_key.t option * Account.t) list Quickcheck.Generator.t =
    match balances with
    | [] ->
        Quickcheck.Generator.return []
    | (n, balance) :: balances_tl ->
        Quickcheck.Generator.create (fun ~size ~random ->
            let rec gen_balances_rev n balance balances_tl accounts =
              if n > 0 then
                let new_random = Splittable_random.State.split random in
                let account =
                  (* Manually generate an account using the [generate] primitive. *)
                  Quickcheck.Generator.generate ~size ~random:new_random
                    (gen_with_balance balance)
                in
                gen_balances_rev (n - 1) balance balances_tl
                  (account :: accounts)
              else
                match balances_tl with
                | [] ->
                    accounts
                | (n, balance) :: balances_tl ->
                    gen_balances_rev n balance balances_tl accounts
            in
            gen_balances_rev n balance balances_tl [] )

  let pad_with_rev_balances balances accounts =
    let balances_accounts =
      Quickcheck.random_value
        ~seed:(`Deterministic "fake accounts with balances for genesis ledger")
        (gen_balances_rev balances)
    in
    List.append accounts balances_accounts

  (* NOTE: When modifying this function, be very careful to ensure that all
     operations are tail-recursive, otherwise a sufficiently large genesis
     ledger will blow the stack.
     In particular, do not use any functions that return values of the form
     [_ :: _], since this construction is NOT tail-recursive.
  *)
  let pad_to n accounts =
    if n <= 0 then accounts
    else
      let exception Stop in
      try
        (* Count accounts and reverse the list while we're doing so to avoid
           re-traversing the list.
        *)
        let rev_accounts, count =
          List.fold ~init:([], 0) accounts ~f:(fun (acc, count) account ->
              let count = count + 1 in
              if count >= n then raise Stop ;
              (account :: acc, count + 1) )
        in
        (* [rev_append] is tail-recursive, and we've already reversed the list,
           so we can avoid calling [append] which may internally reverse the
           list again where it is sufficiently long.
        *)
        List.rev_append rev_accounts (generate (n - count))
      with Stop -> accounts
end

let make_constraint_constants
    ~(default : Genesis_constants.Constraint_constants.t)
    (config : Runtime_config.Proof_keys.t) :
    Genesis_constants.Constraint_constants.t =
  let work_delay =
    Option.value ~default:default.work_delay config.work_delay
  in
  let block_window_duration_ms =
    Option.value ~default:default.block_window_duration_ms
      config.block_window_duration_ms
  in
  let transaction_capacity_log_2 =
    match config.transaction_capacity with
    | Some (Log_2 i) ->
        i
    | Some (Txns_per_second_x10 tps_goal_x10) ->
        let max_coinbases = 2 in
        let max_user_commands_per_block =
          (* block_window_duration is in milliseconds, so divide by 1000 divide
             by 10 again because we have tps * 10
          *)
          tps_goal_x10 * block_window_duration_ms / (1000 * 10)
        in
        (* Log of the capacity of transactions per transition.
            - 1 will only work if we don't have prover fees.
            - 2 will work with prover fees, but not if we want a transaction
              included in every block.
            - At least 3 ensures a transaction per block and the staged-ledger
              unit tests pass.
        *)
        1
        + Core_kernel.Int.ceil_log2
            (max_user_commands_per_block + max_coinbases)
    | None ->
        default.transaction_capacity_log_2
  in
  let pending_coinbase_depth =
    Core_kernel.Int.ceil_log2
      (((transaction_capacity_log_2 + 1) * (work_delay + 1)) + 1)
  in
  { sub_windows_per_window=
      Option.value ~default:default.sub_windows_per_window
        config.sub_windows_per_window
  ; ledger_depth=
      Option.value ~default:default.ledger_depth config.ledger_depth
  ; work_delay
  ; block_window_duration_ms
  ; transaction_capacity_log_2
  ; pending_coinbase_depth
  ; coinbase_amount=
      Option.value ~default:default.coinbase_amount config.coinbase_amount
  ; supercharged_coinbase_factor=
      Option.value ~default:default.supercharged_coinbase_factor
        config.supercharged_coinbase_factor
  ; account_creation_fee=
      Option.value ~default:default.account_creation_fee
        config.account_creation_fee
  ; fork=
      ( match config.fork with
      | None ->
          default.fork
      | Some {previous_state_hash; previous_length; previous_global_slot} ->
          Some
            { previous_state_hash=
                State_hash.of_base58_check_exn previous_state_hash
            ; previous_length= Mina_numbers.Length.of_int previous_length
            ; previous_global_slot=
                Mina_numbers.Global_slot.of_int previous_global_slot } ) }

let make_genesis_constants ~logger ~(default : Genesis_constants.t)
    (config : Runtime_config.t) =
  let open Or_error.Let_syntax in
  let%map genesis_state_timestamp =
    let open Option.Let_syntax in
    match
      let%bind daemon = config.genesis in
      let%map genesis_state_timestamp = daemon.genesis_state_timestamp in
      Genesis_constants.validate_time (Some genesis_state_timestamp)
    with
    | Some (Ok time) ->
        Ok (Some time)
    | Some (Error msg) ->
        [%log error]
          "Could not build genesis constants from the configuration file: \
           $error"
          ~metadata:[("error", `String msg)] ;
        Or_error.errorf
          "Could not build genesis constants from the configuration file: %s"
          msg
    | None ->
        Ok None
  in
  let open Option.Let_syntax in
  { Genesis_constants.protocol=
      { k=
          Option.value ~default:default.protocol.k
            (config.genesis >>= fun cfg -> cfg.k)
      ; delta=
          Option.value ~default:default.protocol.delta
            (config.genesis >>= fun cfg -> cfg.delta)
      ; slots_per_epoch=
          Option.value ~default:default.protocol.slots_per_epoch
            (config.genesis >>= fun cfg -> cfg.slots_per_epoch)
      ; slots_per_sub_window=
          Option.value ~default:default.protocol.slots_per_sub_window
            (config.genesis >>= fun cfg -> cfg.slots_per_sub_window)
      ; genesis_state_timestamp=
          Option.value ~default:default.protocol.genesis_state_timestamp
            genesis_state_timestamp }
  ; txpool_max_size=
      Option.value ~default:default.txpool_max_size
        (config.daemon >>= fun cfg -> cfg.txpool_max_size)
  ; num_accounts=
      Option.value_map ~default:default.num_accounts
        (config.ledger >>= fun cfg -> cfg.num_accounts)
        ~f:(fun num_accounts -> Some num_accounts) }
