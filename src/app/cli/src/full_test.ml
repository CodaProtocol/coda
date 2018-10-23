open Core
open Async_kernel
open Coda_base
open Coda_main
open Signature_lib

let pk_of_sk sk = Public_key.of_private_key_exn sk |> Public_key.compress

let run_test (type ledger_proof) (with_snark_worker: bool) (module Kernel
    : Kernel_intf with type Ledger_proof.t = ledger_proof) (module Coda
    : Coda_intf.S with type ledger_proof = ledger_proof) : unit Deferred.t =
  Parallel.init_master () ;
  let log = Logger.create () in
  let%bind temp_conf_dir =
    Async.Unix.mkdtemp (Filename.temp_dir_name ^/ "full_test_config")
  in
  let keypair = Keypair.of_private_key_exn Genesis_ledger.high_balance_sk in
  let module Config = struct
    let logger = log

    let conf_dir = temp_conf_dir

    let lbc_tree_max_depth = `Finite 50

    let transition_interval = Time.Span.of_ms 1000.0

    let keypair = keypair

    let genesis_proof = Precomputed_values.base_proof

    let transaction_capacity_log_2 =
      if Insecure.with_snark then 1
        (*this works because we don't have prover fees. Once we have that, the transaction_capacity_log_2 has to be at least 2 for transactions to be included*)
      else 3

    let commit_id = None

    let work_selection = Protocols.Coda_pow.Work_selection.Seq
  end in
  let should_propose_bool = true in
  let%bind (module Init) =
    make_init ~should_propose:should_propose_bool
      (module Config)
      (module Kernel)
  in
  let module Main = Coda.Make (Init) () in
  let module Run = Run (Config) (Main) in
  let open Main in
  let banlist_dir_name = temp_conf_dir ^/ "banlist" in
  let%bind () = Async.Unix.mkdir banlist_dir_name in
  let%bind suspicious_dir =
    Async.Unix.mkdtemp (banlist_dir_name ^/ "suspicious")
  in
  let%bind punished_dir = Async.Unix.mkdtemp (banlist_dir_name ^/ "banned") in
  let banlist = Coda_base.Banlist.create ~suspicious_dir ~punished_dir in
  let net_config =
    { Inputs.Net.Config.parent_log= log
    ; gossip_net_params=
        { Inputs.Net.Gossip_net.Config.timeout= Time.Span.of_sec 1.
        ; parent_log= log
        ; target_peer_count= 8
        ; initial_peers= []
        ; conf_dir= temp_conf_dir
        ; me= (Host_and_port.of_string "127.0.0.1:8001", 8000)
        ; banlist } }
  in
  let%bind coda =
    Main.create
      (Main.Config.make ~log ~net_config ~should_propose:should_propose_bool
         ~run_snark_worker:with_snark_worker
         ~ledger_builder_persistant_location:(temp_conf_dir ^/ "ledger_builder")
         ~transaction_pool_disk_location:(temp_conf_dir ^/ "transaction_pool")
         ~snark_pool_disk_location:(temp_conf_dir ^/ "snark_pool")
         ~time_controller:(Inputs.Time.Controller.create ())
         ~keypair () ~banlist)
  in
  don't_wait_for (Linear_pipe.drain (Main.strongest_ledgers coda)) ;
  let balance_change_or_timeout ~initial_receiver_balance receiver_pk =
    let rec go () =
      match Run.get_balance coda receiver_pk with
      | Some b when not (Currency.Balance.equal b initial_receiver_balance) ->
          return ()
      | _ ->
          let%bind () = after (Time_ns.Span.of_sec 10.) in
          go ()
    in
    Deferred.any [after (Time_ns.Span.of_min 3.); go ()]
  in
  let open Genesis_ledger in
  let assert_balance pk amount =
    match Run.get_balance coda pk with
    | Some balance ->
        if not (Currency.Balance.equal balance amount) then
          failwithf
            !"Balance in account %{sexp: Currency.Balance.t} is not asserted \
              balance %{sexp: Currency.Balance.t}"
            balance amount ()
    | None ->
        failwith
          (sprintf !"Invalid Account: %{sexp: Public_key.Compressed.t}" pk)
  in
  let client_port = 8123 in
  let run_snark_worker = `With_public_key Genesis_ledger.high_balance_pk in
  Run.setup_local_server ~client_port ~coda ~log () ;
  Run.run_snark_worker ~log ~client_port run_snark_worker ;
  (* Let the system settle *)
  let%bind () = Async.after (Time.Span.of_ms 100.) in
  assert (Option.is_none @@ Run.For_tests.ledger_proof coda) ;
  (* Note: This is much less than half of the high balance account so we can test
   *       transaction replays being prohibited
   *)
  let send_amount = Currency.Amount.of_int 10 in
  (* Send money to someone *)
  let build_txn amount sender_sk receiver_pk fee =
    let nonce = Run.get_nonce coda (pk_of_sk sender_sk) |> Option.value_exn in
    let payload : Transaction.Payload.t =
      {receiver= receiver_pk; amount; fee; nonce}
    in
    Transaction.sign (Keypair.of_private_key_exn sender_sk) payload
  in
  let test_sending_transaction sender_sk receiver_pk =
    let transaction =
      build_txn send_amount sender_sk receiver_pk (Currency.Fee.of_int 0)
    in
    let prev_sender_balance =
      Run.get_balance coda (pk_of_sk sender_sk) |> Option.value_exn
    in
    let prev_receiver_balance =
      Run.get_balance coda receiver_pk
      |> Option.value ~default:Currency.Balance.zero
    in
    let%bind () = Run.send_txn log coda (transaction :> Transaction.t) in
    (* Send a similar the transaction twice on purpose; this second one
    * will be rejected because the nonce is wrong *)
    let transaction' =
      build_txn send_amount sender_sk receiver_pk (Currency.Fee.of_int 0)
    in
    let%bind () = Run.send_txn log coda (transaction' :> Transaction.t) in
    (* Let the system settle, mine some blocks *)
    let%map () =
      balance_change_or_timeout ~initial_receiver_balance:prev_receiver_balance
        receiver_pk
    in
    assert_balance receiver_pk
      ( Currency.Balance.( + ) prev_receiver_balance send_amount
      |> Option.value_exn ) ;
    assert_balance (pk_of_sk sender_sk)
      ( Currency.Balance.( - ) prev_sender_balance send_amount
      |> Option.value_exn )
  in
  let send_txn_update_balance_sheet sender_sk sender_pk receiver_pk amount
      balance_sheet fee =
    let transaction = build_txn amount sender_sk receiver_pk fee in
    let new_balance_sheet =
      Map.update balance_sheet sender_pk (fun v ->
          Option.value_exn
            (Currency.Balance.sub_amount (Option.value_exn v)
               (Option.value_exn (Currency.Amount.add_fee amount fee))) )
    in
    let new_balance_sheet' =
      Map.update new_balance_sheet receiver_pk (fun v ->
          Option.value_exn
            (Currency.Balance.add_amount (Option.value_exn v) amount) )
    in
    let%map () = Run.send_txn log coda (transaction :> Transaction.t) in
    new_balance_sheet'
  in
  let send_txns key_pairs pks balance_sheet f_amount =
    Deferred.List.foldi key_pairs ~init:balance_sheet ~f:(fun i acc key_pair ->
        let sender_pk = fst key_pair in
        let receiver =
          List.random_element_exn
            (List.filter pks ~f:(fun pk -> not (pk = sender_pk)))
        in
        send_txn_update_balance_sheet (snd key_pair) sender_pk receiver
          (f_amount i) acc (Currency.Fee.of_int 0) )
  in
  let wait_for_proof_or_timeout timeout () =
    let rec go () =
      if Option.is_some @@ Run.For_tests.ledger_proof coda then (
        Core.printf "Ledger_proof emitted\n %!" ;
        return true )
      else
        let%bind () = after (Time_ns.Span.of_sec 10.) in
        go ()
    in
    let timeout =
      let%map () = after (Time_ns.Span.of_min timeout) in
      false
    in
    Deferred.any [timeout; go ()]
  in
  let test_multiple_txns key_pairs pks timeout =
    let balance_sheet =
      Public_key.Compressed.Map.of_alist_exn
        (List.map pks ~f:(fun pk -> (pk, Currency.Balance.of_int init_balance)))
    in
    let%bind updated_balance_sheet =
      send_txns key_pairs pks balance_sheet (fun i ->
          Currency.Amount.of_int ((i + 1) * 10) )
    in
    (*After mining a few blocks and emitting a ledger_proof (by the parallel scan), check if the balances match *)
    let%map emitted = wait_for_proof_or_timeout timeout () in
    assert emitted ;
    Map.fold updated_balance_sheet ~init:() ~f:(fun ~key ~data () ->
        assert_balance key data )
  in
  let test_duplicate_txns new_sender_sk =
    let%bind () =
      test_sending_transaction new_sender_sk Genesis_ledger.low_balance_pk
    in
    test_sending_transaction new_sender_sk Genesis_ledger.low_balance_pk
  in
  if Insecure.with_snark then (
    let accounts = List.take extra_accounts 2 in
    let pks = fst @@ List.unzip accounts in
    test_multiple_txns accounts pks 7. )
  else
    let new_sender, rest_accounts = List.split_n extra_accounts 1 in
    let new_sender_pk = fst (List.hd_exn new_sender) in
    let new_sender_sk = snd (List.hd_exn new_sender) in
    let rest_pks = fst @@ List.unzip rest_accounts in
    assert_balance new_sender_pk (Currency.Balance.of_int init_balance) ;
    assert_balance low_balance_pk initial_low_balance ;
    let%bind () = test_multiple_txns rest_accounts rest_pks 3. in
    test_duplicate_txns new_sender_sk

let command (type ledger_proof) (module Kernel
    : Kernel_intf with type Ledger_proof.t = ledger_proof) (module Coda
    : Coda_intf.S with type ledger_proof = ledger_proof) =
  let open Core in
  let open Async in
  Command.async ~summary:"Full coda end-to-end test"
    (let open Command.Let_syntax in
    let%map_open with_snark_worker =
      flag "with-snark-worker" no_arg ~doc:"Produce snarks"
    in
    fun () -> run_test with_snark_worker (module Kernel) (module Coda))
