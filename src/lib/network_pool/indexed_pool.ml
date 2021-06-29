(* See the .mli for a description of the purpose of this module. *)
open Core
open Mina_base
open Mina_numbers
open Signature_lib

(* Fee increase required to replace a transaction. This represents the cost to
   the network as a whole of checking, gossiping, and storing a transaction
   until it is included in a block. I did some napkin math and came up with
   $0.00007. Ideally we'd fetch an exchange rate and convert that into an amount
   of currency, but a made up number will do for the testnets at least. See
   issue #2385.
*)
let replace_fee : Currency.Fee.t = Currency.Fee.of_int 5_000_000_000

(* Invariants, maintained whenever a t is exposed from this module:
   * Iff a command is in all_by_fee it is also in all_by_sender.
   * Iff a command is in all_by_fee it is also in all_by_hash.
   * Iff a command is at the head of its sender's queue it is also in
     applicable_by_fee.
   * Sequences in all_by_sender are ordered by nonce and "dense".
   * Only commands with an expiration <> Global_slot.max_value is added to transactions_with_expiration.
   * There are no empty sets or sequences.
   * Fee indices are correct.
   * Total currency required is correct.
   * size = the sum of sizes of the sets in all_by_fee = the sum of the lengths
     of the queues in all_by_sender
*)
type t =
  { applicable_by_fee:
      Transaction_hash.User_command_with_valid_signature.Set.t
      Currency.Fee.Map.t
        (** Transactions valid against the current ledger, indexed by fee. *)
  ; all_by_sender:
      ( Transaction_hash.User_command_with_valid_signature.t F_sequence.t
      * Currency.Amount.t )
      Account_id.Map.t
        (** All pending transactions along with the total currency required to
            execute them -- plus any currency spent from this account by
            transactions from other accounts -- indexed by sender account.
            Ordered by nonce inside the accounts. *)
  ; all_by_fee:
      Transaction_hash.User_command_with_valid_signature.Set.t
      Currency.Fee.Map.t
        (** All transactions in the pool indexed by fee. *)
  ; all_by_hash:
      Transaction_hash.User_command_with_valid_signature.t
      Transaction_hash.Map.t
  ; transactions_with_expiration:
      Transaction_hash.User_command_with_valid_signature.Set.t
      Global_slot.Map.t
        (*Only transactions that have an expiry*)
  ; size: int
  ; constraint_constants: Genesis_constants.Constraint_constants.t
  ; consensus_constants: Consensus.Constants.t
  ; time_controller: Block_time.Controller.t }
[@@deriving sexp_of, equal, compare]

module Command_error = struct
  type t =
    | Invalid_nonce of
        [ `Expected of Account.Nonce.t
        | `Between of Account.Nonce.t * Account.Nonce.t ]
        * Account.Nonce.t
    | Insufficient_funds of [`Balance of Currency.Amount.t] * Currency.Amount.t
    | (* NOTE: don't punish for this, attackers can induce nodes to banlist
          each other that way! *)
        Insufficient_replace_fee of
        [`Replace_fee of Currency.Fee.t] * Currency.Fee.t
    | Overflow
    | Bad_token
    | Expired of
        [`Valid_until of Mina_numbers.Global_slot.t]
        * [`Current_global_slot of Mina_numbers.Global_slot.t]
    | Unwanted_fee_token of Token_id.t
    | Invalid_transaction
  [@@deriving sexp_of, to_yojson]
end

(* Compute the total currency required from the sender to execute a command.
   Returns None in case of overflow.
*)
let currency_consumed_unchecked :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> User_command.t
    -> Currency.Amount.t option =
 fun ~constraint_constants cmd ->
  let fee_amt = Currency.Amount.of_fee @@ User_command.fee_exn cmd in
  let open Currency.Amount in
  let amt =
    match cmd with
    | Signed_command c -> (
      match c.payload.body with
      | Payment ({amount; _} as payload) ->
          if
            Token_id.equal c.payload.common.fee_token
              (Payment_payload.token payload)
          then
            (* The fee-payer is also the sender account, include the amount. *)
            amount
          else (* The payment won't affect the balance of this account. *)
            zero
      | Stake_delegation _ ->
          zero
      | Create_new_token _ ->
          Currency.Amount.of_fee constraint_constants.account_creation_fee
      | Create_token_account _ ->
          Currency.Amount.of_fee constraint_constants.account_creation_fee
      | Mint_tokens _ ->
          zero )
    | Snapp_command c -> (
        let open Snapp_command.Party in
        let f (x1 : ((Body.t, _) Predicated.Poly.t, _) Authorized.Poly.t)
            (x2 : ((Body.t, _) Predicated.Poly.t, _) Authorized.Poly.t option)
            token_id (fee_payment : Mina_base.Other_fee_payer.t option) =
          let fee_payer =
            match fee_payment with
            | Some {payload= {pk; token_id; _}; _} ->
                Some (Account_id.create pk token_id)
            | None ->
                None
          in
          let ps =
            x1.data.body
            :: Option.(to_list (map x2 ~f:(fun x2 -> x2.data.body)))
          in
          List.find_map ps ~f:(fun p ->
              match fee_payer with
              | Some fee_payer
                when not
                       (Account_id.equal fee_payer
                          (Account_id.create p.pk token_id)) ->
                  (* Fee payer is distinct from this account. *)
                  None
              | _ -> (
                match p.delta.sgn with
                | Pos ->
                    None
                | Neg ->
                    Some p.delta.magnitude ) )
          |> Option.value ~default:Currency.Amount.zero
        in
        match c with
        | Proved_proved r ->
            f r.one (Some r.two) r.token_id r.fee_payment
        | Proved_signed r ->
            f r.one (Some r.two) r.token_id r.fee_payment
        | Signed_signed r ->
            f r.one (Some r.two) r.token_id r.fee_payment
        | Proved_empty r ->
            f r.one r.two r.token_id r.fee_payment
        | Signed_empty r ->
            f r.one r.two r.token_id r.fee_payment )
  in
  fee_amt + amt

let currency_consumed ~constraint_constants cmd =
  currency_consumed_unchecked ~constraint_constants
    (Transaction_hash.User_command_with_valid_signature.command cmd)

let currency_consumed' :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> User_command.t
    -> (Currency.Amount.t, Command_error.t) Result.t =
 fun ~constraint_constants cmd ->
  cmd
  |> currency_consumed_unchecked ~constraint_constants
  |> Result.of_option ~error:Command_error.Overflow

module For_tests = struct
  (* Check the invariants of the pool structure as listed in the comment above.
  *)
  let assert_invariants : t -> unit =
   fun { applicable_by_fee
       ; all_by_sender
       ; all_by_fee
       ; all_by_hash
       ; size
       ; constraint_constants
       ; _ } ->
    let assert_all_by_fee tx =
      if
        Set.mem
          (Map.find_exn all_by_fee
             ( Transaction_hash.User_command_with_valid_signature.command tx
             |> User_command.fee_exn ))
          tx
      then ()
      else
        failwith
        @@ sprintf
             !"Not found in all_by_fee: %{sexp: \
               Transaction_hash.User_command_with_valid_signature.t }"
             tx
    in
    let assert_all_by_hash tx =
      [%test_eq: Transaction_hash.User_command_with_valid_signature.t] tx
        (Map.find_exn all_by_hash
           (Transaction_hash.User_command_with_valid_signature.hash tx))
    in
    Map.iteri applicable_by_fee ~f:(fun ~key ~data ->
        Set.iter data ~f:(fun tx ->
            let unchecked =
              Transaction_hash.User_command_with_valid_signature.command tx
            in
            [%test_eq: Currency.Fee.t] key (User_command.fee_exn unchecked) ;
            let tx' =
              Map.find_exn all_by_sender (User_command.fee_payer unchecked)
              |> Tuple2.get1 |> F_sequence.head_exn
            in
            [%test_eq: Transaction_hash.User_command_with_valid_signature.t] tx
              tx' ;
            assert_all_by_fee tx ;
            assert_all_by_hash tx ) ) ;
    Map.iteri all_by_sender
      ~f:(fun ~key:fee_payer ~data:(tx_seq, currency_reserved) ->
        assert (F_sequence.length tx_seq > 0) ;
        let check_consistent tx =
          [%test_eq: Account_id.t]
            ( Transaction_hash.User_command_with_valid_signature.command tx
            |> User_command.fee_payer )
            fee_payer ;
          assert_all_by_fee tx ;
          assert_all_by_hash tx
        in
        let applicable, inapplicables =
          Option.value_exn (F_sequence.uncons tx_seq)
        in
        let applicable_unchecked =
          Transaction_hash.User_command_with_valid_signature.command applicable
        in
        check_consistent applicable ;
        assert (
          Set.mem
            (Map.find_exn applicable_by_fee
               (User_command.fee_exn applicable_unchecked))
            applicable ) ;
        let _last_nonce, currency_reserved' =
          F_sequence.foldl
            (fun (prev_nonce, currency_acc) tx ->
              let unchecked =
                Transaction_hash.User_command_with_valid_signature.command tx
              in
              [%test_eq: Account_nonce.t]
                (User_command.nonce_exn unchecked)
                (Account_nonce.succ prev_nonce) ;
              check_consistent tx ;
              ( User_command.nonce_exn unchecked
              , Option.value_exn
                  Currency.Amount.(
                    Option.value_exn
                      (currency_consumed ~constraint_constants tx)
                    + currency_acc) ) )
            ( User_command.nonce_exn applicable_unchecked
            , Option.value_exn
                (currency_consumed ~constraint_constants applicable) )
            inapplicables
        in
        [%test_eq: Currency.Amount.t] currency_reserved currency_reserved' ) ;
    let check_sender_applicable fee tx =
      let unchecked =
        Transaction_hash.User_command_with_valid_signature.command tx
      in
      [%test_eq: Currency.Fee.t] fee (User_command.fee_exn unchecked) ;
      let sender_txs, _currency_reserved =
        Map.find_exn all_by_sender (User_command.fee_payer unchecked)
      in
      let applicable, _inapplicables =
        Option.value_exn (F_sequence.uncons sender_txs)
      in
      assert (
        Set.mem
          (Map.find_exn applicable_by_fee
             ( applicable
             |> Transaction_hash.User_command_with_valid_signature.command
             |> User_command.fee_exn ))
          applicable ) ;
      let first_nonce =
        applicable
        |> Transaction_hash.User_command_with_valid_signature.command
        |> User_command.nonce_exn |> Account_nonce.to_int
      in
      let _split_l, split_r =
        F_sequence.split_at sender_txs
          ( Account_nonce.to_int (User_command.nonce_exn unchecked)
          - first_nonce )
      in
      let tx' = F_sequence.head_exn split_r in
      [%test_eq: Transaction_hash.User_command_with_valid_signature.t] tx tx'
    in
    Map.iteri all_by_fee ~f:(fun ~key:fee ~data:tx_set ->
        Set.iter tx_set ~f:(fun tx ->
            check_sender_applicable fee tx ;
            assert_all_by_hash tx ) ) ;
    Map.iter all_by_hash ~f:(fun tx ->
        check_sender_applicable
          (User_command.fee_exn
             (Transaction_hash.User_command_with_valid_signature.command tx))
          tx ;
        assert_all_by_fee tx ) ;
    [%test_eq: int] (Map.length all_by_hash) size
end

let empty ~constraint_constants ~consensus_constants ~time_controller : t =
  { applicable_by_fee= Currency.Fee.Map.empty
  ; all_by_sender= Account_id.Map.empty
  ; all_by_fee= Currency.Fee.Map.empty
  ; all_by_hash= Transaction_hash.Map.empty
  ; transactions_with_expiration= Global_slot.Map.empty
  ; size= 0
  ; constraint_constants
  ; consensus_constants
  ; time_controller }

let size : t -> int = fun t -> t.size

let min_fee : t -> Currency.Fee.t option =
 fun {all_by_fee; _} -> Option.map ~f:Tuple2.get1 @@ Map.min_elt all_by_fee

let member : t -> Transaction_hash.User_command.t -> bool =
 fun t cmd ->
  Option.is_some
    (Map.find t.all_by_hash (Transaction_hash.User_command.hash cmd))

let all_from_account :
       t
    -> Account_id.t
    -> Transaction_hash.User_command_with_valid_signature.t list =
 fun {all_by_sender; _} account_id ->
  Option.value_map ~default:[] (Map.find all_by_sender account_id)
    ~f:(fun (user_commands, _) -> F_sequence.to_list user_commands)

let get_all {all_by_hash; _} :
    Transaction_hash.User_command_with_valid_signature.t list =
  Map.data all_by_hash

let find_by_hash :
       t
    -> Transaction_hash.t
    -> Transaction_hash.User_command_with_valid_signature.t option =
 fun {all_by_hash; _} hash -> Map.find all_by_hash hash

let current_global_slot t =
  let current_time = Block_time.now t.time_controller in
  let current_slot =
    Consensus.Data.Consensus_time.(
      of_time_exn ~constants:t.consensus_constants current_time
      |> to_global_slot)
  in
  match t.constraint_constants.fork with
  | Some {previous_global_slot; _} ->
      Mina_numbers.Global_slot.(add previous_global_slot current_slot)
  | None ->
      current_slot

let check_expiry t (cmd : User_command.t) =
  let current_global_slot = current_global_slot t in
  let valid_until = User_command.valid_until cmd in
  if Global_slot.(valid_until < current_global_slot) then
    Error
      (Command_error.Expired
         (`Valid_until valid_until, `Current_global_slot current_global_slot))
  else Ok ()

(* a cmd is in the transactions_with_expiration map only if it has an expiry*)
let update_expiration_map expiration_map cmd op =
  let user_cmd =
    Transaction_hash.User_command_with_valid_signature.command cmd
  in
  let expiry = User_command.valid_until user_cmd in
  if Global_slot.(expiry <> max_value) then
    match op with
    | `Add ->
        Map_set.insert
          (module Transaction_hash.User_command_with_valid_signature)
          expiration_map expiry cmd
    | `Del ->
        Map_set.remove_exn expiration_map expiry cmd
  else expiration_map

let remove_from_expiration_exn expiration_map cmd =
  update_expiration_map expiration_map cmd `Del

let add_to_expiration expiration_map cmd =
  update_expiration_map expiration_map cmd `Add

(* Remove a command from the applicable_by_fee field. This may break an
   invariant. *)
let remove_applicable_exn :
    t -> Transaction_hash.User_command_with_valid_signature.t -> t =
 fun t cmd ->
  let fee =
    Transaction_hash.User_command_with_valid_signature.command cmd
    |> User_command.fee_exn
  in
  {t with applicable_by_fee= Map_set.remove_exn t.applicable_by_fee fee cmd}

(* Remove a command from the all_by_fee and all_by_hash fields, and decrement
   size. This may break an invariant. *)
let remove_all_by_fee_and_hash_and_expiration_exn :
    t -> Transaction_hash.User_command_with_valid_signature.t -> t =
 fun t cmd ->
  let fee =
    Transaction_hash.User_command_with_valid_signature.command cmd
    |> User_command.fee_exn
  in
  { t with
    all_by_fee= Map_set.remove_exn t.all_by_fee fee cmd
  ; all_by_hash=
      Map.remove t.all_by_hash
        (Transaction_hash.User_command_with_valid_signature.hash cmd)
  ; transactions_with_expiration=
      remove_from_expiration_exn t.transactions_with_expiration cmd
  ; size= t.size - 1 }

(* Remove a given command from the pool, as well as any commands that depend on
   it. Called from revalidate and remove_lowest_fee, and when replacing
   transactions. *)
let remove_with_dependents_exn :
       t
    -> Transaction_hash.User_command_with_valid_signature.t
    -> Transaction_hash.User_command_with_valid_signature.t Sequence.t * t =
 fun ({constraint_constants; _} as t) cmd ->
  let unchecked =
    Transaction_hash.User_command_with_valid_signature.command cmd
  in
  let sender = User_command.fee_payer unchecked in
  let sender_queue, reserved_currency = Map.find_exn t.all_by_sender sender in
  assert (not @@ F_sequence.is_empty sender_queue) ;
  let first_cmd = F_sequence.head_exn sender_queue in
  let first_nonce =
    Transaction_hash.User_command_with_valid_signature.command first_cmd
    |> User_command.nonce_exn |> Account_nonce.to_int
  in
  let cmd_nonce = User_command.nonce_exn unchecked |> Account_nonce.to_int in
  assert (cmd_nonce >= first_nonce) ;
  let index = cmd_nonce - first_nonce in
  let keep_queue, drop_queue = F_sequence.split_at sender_queue index in
  assert (not (F_sequence.is_empty drop_queue)) ;
  let currency_to_remove =
    F_sequence.foldl
      (fun acc cmd' ->
        Option.value_exn
          (* safe because we check for overflow when we add commands. *)
          (let open Option.Let_syntax in
          let%bind consumed = currency_consumed ~constraint_constants cmd' in
          Currency.Amount.(consumed + acc)) )
      Currency.Amount.zero drop_queue
  in
  let reserved_currency' =
    (* This is safe because the currency in a subset of the commands much be <=
       total currency in all the commands. *)
    Option.value_exn Currency.Amount.(reserved_currency - currency_to_remove)
  in
  let t' =
    F_sequence.foldl
      (fun acc cmd' -> remove_all_by_fee_and_hash_and_expiration_exn acc cmd')
      t drop_queue
  in
  ( F_sequence.to_seq drop_queue
  , { t' with
      all_by_sender=
        ( if not (F_sequence.is_empty keep_queue) then
          Map.set t'.all_by_sender ~key:sender
            ~data:(keep_queue, reserved_currency')
        else (
          assert (Currency.Amount.(equal reserved_currency' zero)) ;
          Map.remove t'.all_by_sender sender ) )
    ; applicable_by_fee=
        ( if
          Transaction_hash.User_command_with_valid_signature.equal first_cmd
            cmd
        then
          Map_set.remove_exn t'.applicable_by_fee
            (User_command.fee_exn unchecked)
            cmd
        else t'.applicable_by_fee ) } )

(** Drop commands from the end of the queue until the total currency consumed is
    <= the current balance. *)
let drop_until_sufficient_balance :
       constraint_constants:Genesis_constants.Constraint_constants.t
    -> Transaction_hash.User_command_with_valid_signature.t F_sequence.t
       * Currency.Amount.t
    -> Currency.Amount.t
    -> Transaction_hash.User_command_with_valid_signature.t F_sequence.t
       * Currency.Amount.t
       * Transaction_hash.User_command_with_valid_signature.t Sequence.t =
 fun ~constraint_constants (queue, currency_reserved) current_balance ->
  let rec go queue' currency_reserved' dropped_so_far =
    if Currency.Amount.(currency_reserved' <= current_balance) then
      (queue', currency_reserved', dropped_so_far)
    else
      let daeh, liat =
        Option.value_exn
          ~message:
            "couldn't drop any more transactions when trying to preserve \
             sufficient balance"
          (F_sequence.unsnoc queue')
      in
      let consumed =
        Option.value_exn (currency_consumed ~constraint_constants liat)
      in
      go daeh
        (Option.value_exn Currency.Amount.(currency_reserved' - consumed))
        (Sequence.append dropped_so_far @@ Sequence.singleton liat)
  in
  go queue currency_reserved Sequence.empty

(* Iterate over all commands in the pool, removing them if they require too much
   currency or have too low of a nonce.
*)
let revalidate :
       t
    -> (Account_id.t -> Account_nonce.t * Currency.Amount.t)
    -> t * Transaction_hash.User_command_with_valid_signature.t Sequence.t =
 fun ({constraint_constants; _} as t) f ->
  Map.fold t.all_by_sender ~init:(t, Sequence.empty)
    ~f:(fun ~key:sender
       ~data:(queue, currency_reserved)
       ((t', dropped_acc) as acc)
       ->
      let current_nonce, current_balance = f sender in
      let first_cmd = F_sequence.head_exn queue in
      let first_nonce =
        first_cmd |> Transaction_hash.User_command_with_valid_signature.command
        |> User_command.nonce_exn
      in
      if Account_nonce.(current_nonce < first_nonce) then
        let dropped, t'' = remove_with_dependents_exn t first_cmd in
        (t'', Sequence.append dropped_acc dropped)
      else
        (* current_nonce >= first_nonce *)
        let idx = Account_nonce.(to_int current_nonce - to_int first_nonce) in
        let drop_queue, keep_queue = F_sequence.split_at queue idx in
        let currency_reserved' =
          F_sequence.foldl
            (fun c cmd ->
              Option.value_exn
                Currency.Amount.(
                  c
                  - Option.value_exn
                      (currency_consumed ~constraint_constants cmd)) )
            currency_reserved drop_queue
        in
        let keep_queue', currency_reserved'', dropped_for_balance =
          drop_until_sufficient_balance ~constraint_constants
            (keep_queue, currency_reserved')
            current_balance
        in
        let to_drop =
          Sequence.append (F_sequence.to_seq drop_queue) dropped_for_balance
        in
        match Sequence.next to_drop with
        | None ->
            acc
        | Some (head, tail) ->
            let t'' =
              Sequence.fold tail
                ~init:
                  (remove_all_by_fee_and_hash_and_expiration_exn
                     (remove_applicable_exn t' head)
                     head)
                ~f:remove_all_by_fee_and_hash_and_expiration_exn
            in
            ( { t'' with
                all_by_sender=
                  Map.set t''.all_by_sender ~key:sender
                    ~data:(keep_queue', currency_reserved'') }
            , Sequence.append dropped_acc to_drop ) )

let remove_expired t :
    Transaction_hash.User_command_with_valid_signature.t Sequence.t * t =
  let current_global_slot = current_global_slot t in
  let expired, _, _ =
    Map.split t.transactions_with_expiration current_global_slot
  in
  Map.fold expired ~init:(Sequence.empty, t) ~f:(fun ~key:_ ~data:cmds acc ->
      Set.fold cmds ~init:acc ~f:(fun acc' cmd ->
          let dropped_acc, t = acc' in
          (*[cmd] would not be in [t] if it depended on an expired transaction already handled*)
          if member t (Transaction_hash.User_command.of_checked cmd) then
            let removed, t' = remove_with_dependents_exn t cmd in
            (Sequence.append dropped_acc removed, t')
          else acc' ) )

let handle_committed_txn :
       t
    -> Transaction_hash.User_command_with_valid_signature.t
    -> fee_payer_balance:Currency.Amount.t
    -> fee_payer_nonce:Mina_base.Account.Nonce.t
    -> ( t * Transaction_hash.User_command_with_valid_signature.t Sequence.t
       , [ `Queued_txns_by_sender of
           string
           * Transaction_hash.User_command_with_valid_signature.t Sequence.t ]
       )
       Result.t =
 fun ({constraint_constants; _} as t) committed ~fee_payer_balance
     ~fee_payer_nonce ->
  let committed' =
    Transaction_hash.User_command_with_valid_signature.command committed
  in
  let fee_payer = User_command.fee_payer committed' in
  let nonce_to_remove = User_command.nonce_exn committed' in
  match Map.find t.all_by_sender fee_payer with
  | None ->
      Ok (t, Sequence.empty)
  | Some (cmds, currency_reserved) ->
      let first_cmd, rest_cmds = Option.value_exn (F_sequence.uncons cmds) in
      let first_nonce =
        first_cmd |> Transaction_hash.User_command_with_valid_signature.command
        |> User_command.nonce_exn
      in
      if Account_nonce.(nonce_to_remove <> first_nonce) then
        Error
          (`Queued_txns_by_sender
            ( "Tried to handle a committed transaction in the pool but its \
               nonce doesn't match the head of the queue for that sender"
            , F_sequence.to_seq cmds ))
      else
        let first_cmd_consumed =
          (* safe since we checked this when we added it to the pool originally *)
          Option.value_exn (currency_consumed ~constraint_constants first_cmd)
        in
        let currency_reserved' =
          (* safe since the sum reserved must be >= reserved by any individual
             command *)
          Option.value_exn
            Currency.Amount.(currency_reserved - first_cmd_consumed)
        in
        let t1 =
          t
          |> Fn.flip remove_applicable_exn first_cmd
          |> Fn.flip remove_all_by_fee_and_hash_and_expiration_exn first_cmd
        in
        let new_queued_cmds, currency_reserved'', dropped_cmds =
          (*removed the first cmd, check if there are anymore committed transactions from the fee payer*)
          if Mina_base.Account.Nonce.(equal (succ first_nonce) fee_payer_nonce)
          then
            (* remove user_commands that consume more currency than what the latest fee_payer_balance is*)
            drop_until_sufficient_balance ~constraint_constants
              (rest_cmds, currency_reserved')
              fee_payer_balance
          else
            (* Don't check if the balance is sufficient, there are other committed user_commands in the pool from the current fee payer that has been accounted for in the fee_payer_balance*)
            (rest_cmds, currency_reserved', Sequence.empty)
        in
        let t2 =
          Sequence.fold dropped_cmds ~init:t1
            ~f:remove_all_by_fee_and_hash_and_expiration_exn
        in
        let set_all_by_sender account_id commands currency_reserved t =
          match F_sequence.uncons commands with
          | None ->
              {t with all_by_sender= Map.remove t.all_by_sender account_id}
          | Some (head_cmd, _) ->
              { t with
                all_by_sender=
                  Map.set t.all_by_sender ~key:account_id
                    ~data:(commands, currency_reserved)
              ; applicable_by_fee=
                  Map_set.insert
                    (module Transaction_hash.User_command_with_valid_signature)
                    t.applicable_by_fee
                    ( head_cmd
                    |> Transaction_hash.User_command_with_valid_signature
                       .command |> User_command.fee_exn )
                    head_cmd }
        in
        let t3 =
          set_all_by_sender fee_payer new_queued_cmds currency_reserved'' t2
        in
        Ok
          ( t3
          , Sequence.append
              ( if
                Transaction_hash.User_command_with_valid_signature.equal
                  committed first_cmd
              then Sequence.empty
              else Sequence.singleton first_cmd )
              dropped_cmds )

let remove_lowest_fee :
    t -> Transaction_hash.User_command_with_valid_signature.t Sequence.t * t =
 fun t ->
  match Map.min_elt t.all_by_fee with
  | None ->
      (Sequence.empty, t)
  | Some (_min_fee, min_fee_set) ->
      remove_with_dependents_exn t @@ Set.min_elt_exn min_fee_set

let get_highest_fee :
    t -> Transaction_hash.User_command_with_valid_signature.t option =
 fun t ->
  Option.map ~f:(Fn.compose Set.min_elt_exn Tuple2.get2)
  @@ Map.max_elt t.applicable_by_fee

(* Add a command that came in from gossip, or return an error. We need to check
   a whole bunch of conditions here and return the appropriate errors.
   Conditions:
   1. Command nonce must be >= account nonce.
   1a. If the sender's queue is empty, command nonce must equal account nonce.
   1b. If the sender's queue is non-empty, command nonce must be <= the nonce of
       the last queued command + 1
   2. The sum of the currency consumed by all queued commands for the sender
      must be <= the sender's balance.
   3. If a command is replaced, the new command must have a fee greater than the
      replaced command by at least replace fee * (number of commands after the
      the replaced command + 1)
   4. No integer overflows are allowed.

   These conditions are referenced in the comments below.
*)
let rec add_from_gossip_exn :
       t
    -> verify:(User_command.t -> User_command.Valid.t option)
    -> [ `Unchecked of Transaction_hash.User_command.t
       | `Checked of Transaction_hash.User_command_with_valid_signature.t ]
    -> Account_nonce.t
    -> Currency.Amount.t
    -> ( Transaction_hash.User_command_with_valid_signature.t
         * t
         * Transaction_hash.User_command_with_valid_signature.t Sequence.t
       , Command_error.t )
       Result.t =
 fun ({constraint_constants; consensus_constants; time_controller; _} as t)
     ~verify cmd0 current_nonce balance ->
  let open Command_error in
  let open Result.Let_syntax in
  let unchecked_cmd =
    match cmd0 with
    | `Unchecked x ->
        x
    | `Checked x ->
        Transaction_hash.User_command.of_checked x
  in
  let unchecked = Transaction_hash.User_command.data unchecked_cmd in
  let verified () =
    match cmd0 with
    | `Checked x ->
        Ok x
    | `Unchecked _ ->
        let%map x =
          Result.of_option (verify unchecked) ~error:Invalid_transaction
        in
        Transaction_hash.(
          User_command_with_valid_signature.make x
            (User_command.hash unchecked_cmd))
  in
  let fee = User_command.fee_exn unchecked in
  let fee_payer = User_command.fee_payer unchecked in
  let nonce = User_command.nonce_exn unchecked in
  (* Result errors indicate problems with the command, while assert failures
     indicate bugs in Coda. *)
  let%bind () = check_expiry t unchecked in
  let%bind consumed = currency_consumed' ~constraint_constants unchecked in
  let%bind () =
    if User_command.check_tokens unchecked then return () else Error Bad_token
  in
  let%bind () =
    (* TODO: Proper exchange rate mechanism. *)
    let fee_token = User_command.fee_token unchecked in
    if Token_id.(equal default) fee_token then return ()
    else Error (Unwanted_fee_token fee_token)
  in
  (* C4 *)
  match Map.find t.all_by_sender fee_payer with
  | None ->
      (* nothing queued for this sender *)
      let%bind () =
        Result.ok_if_true
          (Account_nonce.equal current_nonce nonce)
          ~error:(Invalid_nonce (`Expected current_nonce, nonce))
        (* C1/1a *)
      in
      let%bind () =
        Result.ok_if_true
          Currency.Amount.(consumed <= balance)
          ~error:(Insufficient_funds (`Balance balance, consumed))
        (* C2 *)
      in
      let%map cmd = verified () in
      ( cmd
      , { applicable_by_fee=
            Map_set.insert
              (module Transaction_hash.User_command_with_valid_signature)
              t.applicable_by_fee fee cmd
        ; all_by_sender=
            Map.set t.all_by_sender ~key:fee_payer
              ~data:(F_sequence.singleton cmd, consumed)
        ; all_by_fee=
            Map_set.insert
              (module Transaction_hash.User_command_with_valid_signature)
              t.all_by_fee fee cmd
        ; all_by_hash=
            Map.set t.all_by_hash
              ~key:
                (Transaction_hash.User_command_with_valid_signature.hash cmd)
              ~data:cmd
        ; transactions_with_expiration=
            add_to_expiration t.transactions_with_expiration cmd
        ; size= t.size + 1
        ; constraint_constants
        ; consensus_constants
        ; time_controller }
      , Sequence.empty )
  | Some (queued_cmds, reserved_currency) ->
      (* commands queued for this sender *)
      assert (not @@ F_sequence.is_empty queued_cmds) ;
      let last_queued_nonce =
        F_sequence.last_exn queued_cmds
        |> Transaction_hash.User_command_with_valid_signature.command
        |> User_command.nonce_exn
      in
      if Account_nonce.equal (Account_nonce.succ last_queued_nonce) nonce then
        (* this command goes on the end *)
        let%bind reserved_currency' =
          Currency.Amount.(consumed + reserved_currency)
          |> Result.of_option ~error:Overflow
          (* C4 *)
        in
        let%bind () =
          Result.ok_if_true
            Currency.Amount.(balance >= reserved_currency')
            ~error:(Insufficient_funds (`Balance balance, reserved_currency'))
          (* C2 *)
        in
        let%map cmd = verified () in
        ( cmd
        , { t with
            all_by_sender=
              Map.set t.all_by_sender ~key:fee_payer
                ~data:(F_sequence.snoc queued_cmds cmd, reserved_currency')
          ; all_by_fee=
              Map_set.insert
                (module Transaction_hash.User_command_with_valid_signature)
                t.all_by_fee fee cmd
          ; all_by_hash=
              Map.set t.all_by_hash
                ~key:
                  (Transaction_hash.User_command_with_valid_signature.hash cmd)
                ~data:cmd
          ; transactions_with_expiration=
              add_to_expiration t.transactions_with_expiration cmd
          ; size= t.size + 1 }
        , Sequence.empty )
      else
        (* we're replacing a command *)
        let first_queued_nonce =
          F_sequence.head_exn queued_cmds
          |> Transaction_hash.User_command_with_valid_signature.command
          |> User_command.nonce_exn
        in
        assert (Account_nonce.equal first_queued_nonce current_nonce) ;
        let%bind () =
          Result.ok_if_true
            (Account_nonce.between ~low:first_queued_nonce
               ~high:last_queued_nonce nonce)
            ~error:
              (Invalid_nonce
                 (`Between (first_queued_nonce, last_queued_nonce), nonce))
          (* C1/C1b *)
        in
        assert (
          F_sequence.length queued_cmds
          = Account_nonce.to_int last_queued_nonce
            - Account_nonce.to_int first_queued_nonce
            + 1 ) ;
        let _keep_queue, drop_queue =
          F_sequence.split_at queued_cmds
            ( Account_nonce.to_int nonce
            - Account_nonce.to_int first_queued_nonce )
        in
        let to_drop =
          F_sequence.head_exn drop_queue
          |> Transaction_hash.User_command_with_valid_signature.command
        in
        assert (Account_nonce.equal (User_command.nonce_exn to_drop) nonce) ;
        (* We check the fee increase twice because we need to be sure the
           subtraction is safe. *)
        let%bind () =
          let replace_fee = User_command.fee_exn to_drop in
          Result.ok_if_true
            Currency.Fee.(fee >= replace_fee)
            ~error:(Insufficient_replace_fee (`Replace_fee replace_fee, fee))
          (* C3 *)
        in
        let dropped, t' =
          remove_with_dependents_exn t @@ F_sequence.head_exn drop_queue
        in
        (* check remove_exn dropped the right things *)
        [%test_eq:
          Transaction_hash.User_command_with_valid_signature.t Sequence.t]
          dropped
          (F_sequence.to_seq drop_queue) ;
        let%bind cmd = verified () in
        (* Add the new transaction *)
        let%bind cmd, t'', _ =
          match
            add_from_gossip_exn t' ~verify (`Checked cmd) current_nonce balance
          with
          | Ok (v, t'', dropped') ->
              (* We've already removed them, so this should always be empty. *)
              assert (Sequence.is_empty dropped') ;
              Result.Ok (v, t'', dropped)
          | Error err ->
              Error err
        in
        let drop_head, drop_tail = Option.value_exn (Sequence.next dropped) in
        let increment =
          Option.value_exn Currency.Fee.(fee - User_command.fee_exn to_drop)
        in
        (* Re-add all of the transactions we dropped until there are none left,
           or until the fees from dropped transactions exceed the fee increase
           over the first transaction.
        *)
        let%bind t'', increment, dropped' =
          let rec go t' increment dropped dropped' current_nonce =
            match (Sequence.next dropped, dropped') with
            | None, Some dropped' ->
                Ok (t', increment, dropped')
            | None, None ->
                Ok (t', increment, Sequence.empty)
            | Some (cmd, dropped), Some _ -> (
                let cmd_unchecked =
                  Transaction_hash.User_command_with_valid_signature.command
                    cmd
                in
                let replace_fee = User_command.fee_exn cmd_unchecked in
                match Currency.Fee.(increment - replace_fee) with
                | Some increment ->
                    go t' increment dropped dropped' current_nonce
                | None ->
                    Error
                      (Insufficient_replace_fee
                         (`Replace_fee replace_fee, increment)) )
            | Some (cmd, dropped'), None -> (
                let current_nonce = Account_nonce.succ current_nonce in
                match
                  add_from_gossip_exn t' ~verify (`Checked cmd) current_nonce
                    balance
                with
                | Ok (_v, t', dropped_) ->
                    assert (Sequence.is_empty dropped_) ;
                    go t' increment dropped' None current_nonce
                | Error _err ->
                    (* Re-evaluate with the same [dropped] to calculate the new
                       fee increment.
                    *)
                    go t' increment dropped (Some dropped') current_nonce )
          in
          go t'' increment drop_tail None current_nonce
        in
        let%map () =
          Result.ok_if_true
            Currency.Fee.(increment >= replace_fee)
            ~error:
              (Insufficient_replace_fee (`Replace_fee replace_fee, increment))
          (* C3 *)
        in
        (cmd, t'', Sequence.(append (return drop_head) dropped'))

let add_from_backtrack :
       t
    -> Transaction_hash.User_command_with_valid_signature.t
    -> (t, Command_error.t) Result.t =
 fun ({constraint_constants; consensus_constants; time_controller; _} as t) cmd ->
  let open Result.Let_syntax in
  let unchecked =
    Transaction_hash.User_command_with_valid_signature.command cmd
  in
  let%map () = check_expiry t unchecked in
  let fee_payer = User_command.fee_payer unchecked in
  let fee = User_command.fee_exn unchecked in
  let consumed =
    Option.value_exn (currency_consumed ~constraint_constants cmd)
  in
  match Map.find t.all_by_sender fee_payer with
  | None ->
      { all_by_sender=
          (* If the command comes from backtracking, then we know it doesn't
             cause overflow, so it's OK to throw here.
          *)
          Map.add_exn t.all_by_sender ~key:fee_payer
            ~data:(F_sequence.singleton cmd, consumed)
      ; all_by_fee=
          Map_set.insert
            (module Transaction_hash.User_command_with_valid_signature)
            t.all_by_fee fee cmd
      ; all_by_hash=
          Map.set t.all_by_hash
            ~key:(Transaction_hash.User_command_with_valid_signature.hash cmd)
            ~data:cmd
      ; applicable_by_fee=
          Map_set.insert
            (module Transaction_hash.User_command_with_valid_signature)
            t.applicable_by_fee fee cmd
      ; transactions_with_expiration=
          add_to_expiration t.transactions_with_expiration cmd
      ; size= t.size + 1
      ; constraint_constants
      ; consensus_constants
      ; time_controller }
  | Some (queue, currency_reserved) ->
      let first_queued = F_sequence.head_exn queue in
      if
        not
          (Account_nonce.equal
             (unchecked |> User_command.nonce_exn |> Account_nonce.succ)
             ( first_queued
             |> Transaction_hash.User_command_with_valid_signature.command
             |> User_command.nonce_exn ))
      then
        failwith
        @@ sprintf
             !"indexed pool nonces inconsistent when adding from backtrack. \
               Trying to add \
               %{sexp:Transaction_hash.User_command_with_valid_signature.t} \
               to %{sexp: t}"
             cmd t ;
      let t' = remove_applicable_exn t first_queued in
      { applicable_by_fee=
          Map_set.insert
            (module Transaction_hash.User_command_with_valid_signature)
            t'.applicable_by_fee fee cmd
      ; all_by_fee=
          Map_set.insert
            (module Transaction_hash.User_command_with_valid_signature)
            t'.all_by_fee fee cmd
      ; all_by_hash=
          Map.set t.all_by_hash
            ~key:(Transaction_hash.User_command_with_valid_signature.hash cmd)
            ~data:cmd
      ; all_by_sender=
          Map.set t'.all_by_sender ~key:fee_payer
            ~data:
              ( F_sequence.cons cmd queue
              , Option.value_exn Currency.Amount.(currency_reserved + consumed)
              )
      ; transactions_with_expiration=
          add_to_expiration t.transactions_with_expiration cmd
      ; size= t.size + 1
      ; constraint_constants
      ; consensus_constants
      ; time_controller }

let%test_module _ =
  ( module struct
    open For_tests

    let test_keys = Array.init 10 ~f:(fun _ -> Signature_lib.Keypair.create ())

    let gen_cmd ?sign_type ?nonce ?fee_token ?payment_token () =
      User_command.Valid.Gen.payment_with_random_participants ~keys:test_keys
        ~max_amount:1000 ~fee_range:10 ?sign_type ?nonce ?fee_token
        ?payment_token ()
      |> Quickcheck.Generator.map
           ~f:Transaction_hash.User_command_with_valid_signature.create

    let precomputed_values = Lazy.force Precomputed_values.for_unit_tests

    let constraint_constants = precomputed_values.constraint_constants

    let consensus_constants = precomputed_values.consensus_constants

    let logger = Logger.null ()

    let time_controller = Block_time.Controller.basic ~logger

    let empty =
      empty ~constraint_constants ~consensus_constants ~time_controller

    let%test_unit "empty invariants" = assert_invariants empty

    let don't_verify _ = None

    let%test_unit "singleton properties" =
      Quickcheck.test (gen_cmd ()) ~f:(fun cmd ->
          let pool = empty in
          let add_res =
            add_from_gossip_exn pool (`Checked cmd) Account_nonce.zero
              ~verify:don't_verify
              (Currency.Amount.of_int 500)
          in
          if
            Option.value_exn (currency_consumed ~constraint_constants cmd)
            |> Currency.Amount.to_int > 500
          then
            match add_res with
            | Error (Insufficient_funds _) ->
                ()
            | _ ->
                failwith "should've returned insufficient_funds"
          else
            match add_res with
            | Ok (_, pool', dropped) ->
                assert_invariants pool' ;
                assert (Sequence.is_empty dropped) ;
                [%test_eq: int] (size pool') 1 ;
                [%test_eq:
                  Transaction_hash.User_command_with_valid_signature.t option]
                  (get_highest_fee pool') (Some cmd) ;
                let dropped', pool'' = remove_lowest_fee pool' in
                [%test_eq:
                  Transaction_hash.User_command_with_valid_signature.t
                  Sequence.t] dropped' (Sequence.singleton cmd) ;
                [%test_eq: t] ~equal pool pool''
            | _ ->
                failwith "should've succeeded" )

    let%test_unit "sequential adds (all valid)" =
      let gen :
          ( Ledger.init_state
          * Transaction_hash.User_command_with_valid_signature.t list )
          Quickcheck.Generator.t =
        let open Quickcheck.Generator.Let_syntax in
        let%bind ledger_init = Ledger.gen_initial_ledger_state in
        let%map cmds = User_command.Valid.Gen.sequence ledger_init in
        ( ledger_init
        , List.map ~f:Transaction_hash.User_command_with_valid_signature.create
            cmds )
      in
      let shrinker :
          ( Ledger.init_state
          * Transaction_hash.User_command_with_valid_signature.t list )
          Quickcheck.Shrinker.t =
        Quickcheck.Shrinker.create (fun (init_state, cmds) ->
            Sequence.singleton
              (init_state, List.take cmds (List.length cmds - 1)) )
      in
      Quickcheck.test gen
        ~sexp_of:
          [%sexp_of:
            Ledger.init_state
            * Transaction_hash.User_command_with_valid_signature.t list]
        ~shrinker ~shrink_attempts:`Exhaustive ~seed:(`Deterministic "d")
        ~sizes:(Sequence.repeat 10) ~f:(fun (ledger_init, cmds) ->
          let account_init_states_seq = Array.to_sequence ledger_init in
          let balances = Hashtbl.create (module Public_key.Compressed) in
          let nonces = Hashtbl.create (module Public_key.Compressed) in
          Sequence.iter account_init_states_seq
            ~f:(fun (kp, balance, nonce, _) ->
              let compressed = Public_key.compress kp.public_key in
              Hashtbl.add_exn balances ~key:compressed ~data:balance ;
              Hashtbl.add_exn nonces ~key:compressed ~data:nonce ) ;
          let pool = ref empty in
          let rec go cmds_acc =
            match cmds_acc with
            | [] ->
                ()
            | cmd :: rest -> (
                let unchecked =
                  Transaction_hash.User_command_with_valid_signature.command
                    cmd
                in
                let account_id = User_command.fee_payer unchecked in
                let pk = Account_id.public_key account_id in
                let add_res =
                  add_from_gossip_exn !pool (`Checked cmd) ~verify:don't_verify
                    (Hashtbl.find_exn nonces pk)
                    (Hashtbl.find_exn balances pk)
                in
                match add_res with
                | Ok (_, pool', dropped) ->
                    [%test_eq:
                      Transaction_hash.User_command_with_valid_signature.t
                      Sequence.t] dropped Sequence.empty ;
                    assert_invariants pool' ;
                    pool := pool' ;
                    go rest
                | Error (Invalid_nonce (`Expected want, got)) ->
                    failwithf
                      !"Bad nonce. Expected: %{sexp: Account.Nonce.t}. Got: \
                        %{sexp: Account.Nonce.t}"
                      want got ()
                | Error (Invalid_nonce (`Between (low, high), got)) ->
                    failwithf
                      !"Bad nonce. Expected between %{sexp: Account.Nonce.t} \
                        and %{sexp:Account.Nonce.t}. Got: %{sexp: \
                        Account.Nonce.t}"
                      low high got ()
                | Error (Insufficient_funds (`Balance bal, amt)) ->
                    failwithf
                      !"Insufficient funds. Balance: %{sexp: \
                        Currency.Amount.t}. Amount: %{sexp: Currency.Amount.t}"
                      bal amt ()
                | Error (Insufficient_replace_fee (`Replace_fee rfee, fee)) ->
                    failwithf
                      !"Insufficient fee for replacement. Needed at least \
                        %{sexp: Currency.Fee.t} but got \
                        %{sexp:Currency.Fee.t}."
                      rfee fee ()
                | Error Overflow ->
                    failwith "Overflow."
                | Error Bad_token ->
                    failwith "Token is incompatible with the command."
                | Error Invalid_transaction ->
                    failwith
                      "Transaction had invalid signature or was malformed"
                | Error (Unwanted_fee_token fee_token) ->
                    failwithf
                      !"Bad fee token. The fees are paid in token %{sexp: \
                        Token_id.t}, which we are not accepting fees in."
                      fee_token ()
                | Error
                    (Expired
                      ( `Valid_until valid_until
                      , `Current_global_slot current_global_slot )) ->
                    failwithf
                      !"Expired user command. Current global slot is \
                        %{sexp:Mina_numbers.Global_slot.t} but user command \
                        is only valid until %{sexp:Mina_numbers.Global_slot.t}"
                      current_global_slot valid_until () )
          in
          go cmds )

    let%test_unit "replacement" =
      let modify_payment (c : User_command.t) ~sender ~common:fc ~body:fb =
        let modified_payload : Signed_command.Payload.t =
          match c with
          | Signed_command {payload= {body= Payment payment_payload; common}; _}
            ->
              { common= fc common
              ; body= Signed_command.Payload.Body.Payment (fb payment_payload)
              }
          | _ ->
              failwith "generated user command that wasn't a payment"
        in
        Signed_command
          (Signed_command.For_tests.fake_sign sender modified_payload)
        |> Transaction_hash.User_command_with_valid_signature.create
      in
      let gen :
          ( Account_nonce.t
          * Currency.Amount.t
          * Transaction_hash.User_command_with_valid_signature.t list
          * Transaction_hash.User_command_with_valid_signature.t )
          Quickcheck.Generator.t =
        let open Quickcheck.Generator.Let_syntax in
        let%bind sender_index = Int.gen_incl 0 9 in
        let sender = test_keys.(sender_index) in
        let%bind init_nonce =
          Quickcheck.Generator.map ~f:Account_nonce.of_int
          @@ Int.gen_incl 0 1000
        in
        let init_balance = Currency.Amount.of_int 100_000_000_000_000 in
        let%bind size = Quickcheck.Generator.size in
        let%bind amounts =
          Quickcheck.Generator.map ~f:Array.of_list
          @@ Quickcheck_lib.gen_division_currency init_balance (size + 1)
        in
        let rec go current_nonce current_balance n =
          if n > 0 then
            let%bind cmd =
              let key_gen =
                Quickcheck.Generator.tuple2 (return sender)
                  (Quickcheck_lib.of_array test_keys)
              in
              User_command.Gen.payment ~sign_type:`Fake ~key_gen
                ~nonce:current_nonce ~max_amount:1 ~fee_range:0 ()
            in
            let cmd_currency = amounts.(n - 1) in
            let%bind fee =
              Currency.Amount.(gen_incl zero (min (of_int 10) cmd_currency))
            in
            let amount =
              Option.value_exn Currency.Amount.(cmd_currency - fee)
            in
            let cmd' =
              modify_payment cmd ~sender
                ~common:(fun c -> {c with fee= Currency.Amount.to_fee fee})
                ~body:(fun b -> {b with amount})
            in
            let consumed =
              Option.value_exn (currency_consumed ~constraint_constants cmd')
            in
            let%map rest =
              go
                (Account_nonce.succ current_nonce)
                (Option.value_exn Currency.Amount.(current_balance - consumed))
                (n - 1)
            in
            cmd' :: rest
          else return []
        in
        let%bind setup_cmds = go init_nonce init_balance (size + 1) in
        let init_nonce_int = Account.Nonce.to_int init_nonce in
        let%bind replaced_nonce =
          Int.gen_incl init_nonce_int
            (init_nonce_int + List.length setup_cmds - 1)
        in
        let%map replace_cmd_skeleton =
          let key_gen =
            Quickcheck.Generator.tuple2 (return sender)
              (Quickcheck_lib.of_array test_keys)
          in
          User_command.Gen.payment ~sign_type:`Fake ~key_gen
            ~nonce:(Account_nonce.of_int replaced_nonce)
            ~max_amount:(Currency.Amount.to_int init_balance)
            ~fee_range:0 ()
        in
        let replace_cmd =
          modify_payment replace_cmd_skeleton ~sender ~body:Fn.id
            ~common:(fun c ->
              { c with
                fee=
                  Currency.Fee.of_int ((10 + (5 * (size + 1))) * 1_000_000_000)
              } )
        in
        (init_nonce, init_balance, setup_cmds, replace_cmd)
      in
      Quickcheck.test ~trials:20 gen
        ~sexp_of:
          [%sexp_of:
            Account_nonce.t
            * Currency.Amount.t
            * Transaction_hash.User_command_with_valid_signature.t list
            * Transaction_hash.User_command_with_valid_signature.t]
        ~f:(fun (init_nonce, init_balance, setup_cmds, replace_cmd) ->
          let t =
            List.fold_left setup_cmds ~init:empty ~f:(fun t cmd ->
                match
                  add_from_gossip_exn t (`Checked cmd) init_nonce init_balance
                    ~verify:don't_verify
                with
                | Ok (_, t', removed) ->
                    [%test_eq:
                      Transaction_hash.User_command_with_valid_signature.t
                      Sequence.t] removed Sequence.empty ;
                    t'
                | _ ->
                    failwith
                    @@ sprintf
                         !"adding command %{sexp: \
                           Transaction_hash.User_command_with_valid_signature.t} \
                           failed"
                         cmd )
          in
          let replaced_idx =
            Account_nonce.to_int
              ( replace_cmd
              |> Transaction_hash.User_command_with_valid_signature.command
              |> User_command.nonce_exn )
            - Account_nonce.to_int
                ( List.hd_exn setup_cmds
                |> Transaction_hash.User_command_with_valid_signature.command
                |> User_command.nonce_exn )
          in
          let currency_consumed_pre_replace =
            List.fold_left
              (List.take setup_cmds (replaced_idx + 1))
              ~init:Currency.Amount.zero
              ~f:(fun consumed_so_far cmd ->
                Option.value_exn
                  Option.(
                    currency_consumed ~constraint_constants cmd
                    >>= fun consumed ->
                    Currency.Amount.(consumed + consumed_so_far)) )
          in
          assert (
            Currency.Amount.(currency_consumed_pre_replace <= init_balance) ) ;
          let currency_consumed_post_replace =
            Option.value_exn
              (let open Option.Let_syntax in
              let%bind replaced_currency_consumed =
                currency_consumed ~constraint_constants
                @@ List.nth_exn setup_cmds replaced_idx
              in
              let%bind replacer_currency_consumed =
                currency_consumed ~constraint_constants replace_cmd
              in
              let%bind a =
                Currency.Amount.(
                  currency_consumed_pre_replace - replaced_currency_consumed)
              in
              Currency.Amount.(a + replacer_currency_consumed))
          in
          let add_res =
            add_from_gossip_exn t (`Checked replace_cmd) init_nonce
              init_balance ~verify:don't_verify
          in
          if Currency.Amount.(currency_consumed_post_replace <= init_balance)
          then
            match add_res with
            | Ok (_, t', dropped) ->
                assert (not (Sequence.is_empty dropped)) ;
                assert_invariants t'
            | Error _ ->
                failwith "adding command failed"
          else
            match add_res with
            | Error (Insufficient_funds _) ->
                ()
            | _ ->
                failwith "should've returned insufficient_funds" )
  end )
