open Core
open Async
open Mina_base
open Signature_lib

let name = "coda-delegation-test"

include Heartbeat.Make ()

let runtime_config = Runtime_config.Test_configs.delegation

let main () =
  let logger = Logger.create () in
  let%bind precomputed_values, _runtime_config =
    Genesis_ledger_helper.inputs_from_config_file ~logger ~proof_level:None
      (Lazy.force runtime_config)
    >>| Or_error.ok_exn
  in
  let num_block_producers = 3 in
  let accounts =
    Lazy.force (Genesis_proof.Inputs.accounts precomputed_values)
  in
  let snark_work_public_keys ndx =
    List.nth_exn accounts ndx
    |> fun (_, acct) -> Some (Account.public_key acct)
  in
  let%bind testnet =
    Coda_worker_testnet.test ~name logger num_block_producers Option.some
      snark_work_public_keys Cli_lib.Arg_type.Work_selection_method.Sequence
      ~max_concurrent_connections:None ~precomputed_values
  in
  [%log info] "Started test net" ;
  (* keep CI alive *)
  Deferred.don't_wait_for (print_heartbeat logger) ;
  (* dump account info to log *)
  List.iteri accounts ~f:(fun ndx (sk, acct) ->
      let sk =
        match sk with
        | Some sk ->
            `String (Private_key.to_base58_check sk)
        | None ->
            `Null
      in
      [%log info] "Account: $account_number"
        ~metadata:
          [ ("account_number", `Int ndx)
          ; ("private_key", sk)
          ; ( "public_key"
            , `String
                ( Public_key.Compressed.to_base58_check
                @@ Account.public_key acct ) )
          ; ("balance", `Int (Currency.Balance.to_int acct.balance)) ] ) ;
  (* second account is delegator; see genesis_ledger/test_delegation_ledger.ml *)
  let ((_, delegator_account) as delegator) = List.nth_exn accounts 2 in
  let delegator_pubkey = Account.public_key delegator_account in
  let delegator_keypair =
    Genesis_proof.Inputs.keypair_of_account_record_exn delegator
  in
  (* zeroth account is delegatee *)
  let _, delegatee_account = List.nth_exn accounts 0 in
  let delegatee_pubkey = Account.public_key delegatee_account in
  let worker = testnet.workers.(0) in
  (* setup readers for produced blocks by delegator, delegatee *)
  let%bind delegator_transition_reader =
    Coda_process.new_block_exn worker delegator_pubkey
  in
  (* delegator's transition reader will fill this ivar when it has seen a few blocks *)
  let delegator_ivar : unit Ivar.t = Ivar.create () in
  (* once the delegatee starts producing blocks, the delegator should
     no longer be producing
  *)
  let delegatee_has_produced = ref false in
  let delegator_production_count = ref 0 in
  let delegator_production_goal = 20 in
  Deferred.don't_wait_for
    (Pipe_lib.Linear_pipe.iter delegator_transition_reader
       ~f:(fun {With_hash.data= transition; _} ->
         if Public_key.Compressed.equal transition.creator delegator_pubkey
         then (
           [%log info] "Observed block produced by delegator $delegator"
             ~metadata:
               [ ( "delegator"
                 , `String
                     (Public_key.Compressed.to_base58_check delegator_pubkey)
                 ) ] ;
           assert (not !delegatee_has_produced) ;
           incr delegator_production_count ;
           if Int.equal !delegator_production_count delegator_production_goal
           then Ivar.fill delegator_ivar () ) ;
         return () )) ;
  [%log info] "Started delegator transition reader" ;
  let%bind delegatee_transition_reader =
    Coda_process.new_block_exn worker delegatee_pubkey
  in
  let delegatee_production_count = ref 0 in
  (* delegatee's transition reader will fill this ivar when it has seen a few blocks *)
  let delegatee_ivar : unit Ivar.t = Ivar.create () in
  (* how many blocks we should wait for from the delegatee *)
  let delegatee_production_goal = 5 in
  Deferred.don't_wait_for
    (Pipe_lib.Linear_pipe.iter delegatee_transition_reader
       ~f:(fun {With_hash.data= transition; _} ->
         if Public_key.Compressed.equal transition.creator delegatee_pubkey
         then (
           [%log info] "Observed block produced by delegatee $delegatee"
             ~metadata:
               [ ( "delegatee"
                 , `String
                     (Public_key.Compressed.to_base58_check delegatee_pubkey)
                 ) ] ;
           delegatee_has_produced := true ;
           incr delegatee_production_count ;
           if Int.equal !delegatee_production_count delegatee_production_goal
           then Ivar.fill delegatee_ivar () ) ;
         return () )) ;
  [%log info] "Started delegatee transition reader" ;
  (* wait for delegator to produce some blocks *)
  let%bind () = Ivar.read delegator_ivar in
  assert (Int.equal !delegatee_production_count 0) ;
  [%log info]
    "Before delegation, got $delegator_production_count blocks from delegator \
     (and none from delegatee)"
    ~metadata:[("delegator_production_count", `Int !delegator_production_count)] ;
  let%bind () =
    Coda_worker_testnet.Delegation.delegate_stake testnet ~node:0
      ~delegator:delegator_keypair.private_key ~delegatee:delegatee_pubkey
  in
  [%log info] "Ran delegation command" ;
  (* wait for delegatee to produce a few blocks *)
  let%bind () = Ivar.read delegatee_ivar in
  [%log info] "Saw $delegatee_production_count blocks produced by delegatee"
    ~metadata:[("delegatee_production_count", `Int !delegatee_production_count)] ;
  heartbeat_flag := false ;
  Coda_worker_testnet.Api.teardown testnet ~logger

let command =
  Command.async
    ~summary:
      "Test whether stake delegation from a high-balance account to a \
       low-balance account works"
    (Command.Param.return main)
