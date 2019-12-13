[%%import
"../../config.mlh"]

open Core
open Async
open Coda_base

[%%inject
"ledger_depth", ledger_depth]

[%%if
proof_level = "full"]

let use_dummy_values = false

[%%else]

let use_dummy_values = true

[%%endif]

type t = Ledger.t

let generate_base_proof ~ledger =
  let%map (module Keys) = Keys_lib.Keys.create () in
  let genesis_ledger = lazy ledger in
  let genesis_state = Coda_state.Genesis_protocol_state.t ~genesis_ledger in
  let base_hash = Keys.Step.instance_hash genesis_state.data in
  let wrap hash proof =
    let open Snark_params in
    let module Wrap = Keys.Wrap in
    let input = Wrap_input.of_tick_field hash in
    let proof =
      Tock.prove
        (Tock.Keypair.pk Wrap.keys)
        Wrap.input {Wrap.Prover_state.proof} Wrap.main input
    in
    assert (Tock.verify proof (Tock.Keypair.vk Wrap.keys) Wrap.input input) ;
    proof
  in
  let base_proof =
    let open Snark_params in
    let prover_state =
      { Keys.Step.Prover_state.prev_proof= Tock.Proof.dummy
      ; wrap_vk= Tock.Keypair.vk Keys.Wrap.keys
      ; prev_state= Coda_state.Protocol_state.negative_one ~genesis_ledger
      ; genesis_state_hash= genesis_state.hash
      ; expected_next_state= None
      ; update= Coda_state.Snark_transition.genesis ~genesis_ledger }
    in
    let main x =
      Tick.handle (Keys.Step.main x)
        (Consensus.Data.Prover_state.precomputed_handler ~genesis_ledger)
    in
    let tick =
      Tick.prove
        (Tick.Keypair.pk Keys.Step.keys)
        (Keys.Step.input ()) prover_state main base_hash
    in
    assert (
      Tick.verify tick
        (Tick.Keypair.vk Keys.Step.keys)
        (Keys.Step.input ()) base_hash ) ;
    wrap base_hash tick
  in
  (base_hash, base_proof)

let compiled_accounts_json () : Account_config.t =
  List.map Test_genesis_ledger.accounts ~f:(fun (sk_opt, acc) ->
      { Account_config.pk= acc.public_key
      ; sk= sk_opt
      ; balance= acc.balance
      ; delegate= Some acc.delegate } )

let create : directory_name:string -> Account_config.t -> t =
 fun ~directory_name accounts ->
  let ledger = Ledger.create ~directory_name () in
  List.iter accounts ~f:(fun {pk; balance; delegate; _} ->
      let account =
        let base_acct = Account.create pk balance in
        {base_acct with delegate= Option.value ~default:pk delegate}
      in
      Ledger.create_new_account_exn ledger account.public_key account ) ;
  ledger

let commit ledger = Ledger.commit ledger

let get_accounts accounts_json_file genesis_dir n =
  let open Deferred.Or_error.Let_syntax in
  let%map accounts =
    match accounts_json_file with
    | Some file -> (
        let open Deferred.Let_syntax in
        match%map
          Deferred.Or_error.try_with_join (fun () ->
              let%map accounts_str = Reader.file_contents file in
              let res = Yojson.Safe.from_string accounts_str in
              match Account_config.of_yojson res with
              | Ok res ->
                  Ok res
              | Error s ->
                  Error
                    (Error.of_string
                       (sprintf "Account_config.of_yojson failed: %s" s)) )
        with
        | Ok res ->
            Ok res
        | Error e ->
            Or_error.errorf "Could not read accounts from file:%s\n%s" file
              (Error.to_string_hum e) )
    | None ->
        Deferred.return (Ok (compiled_accounts_json ()))
  in
  let real_accounts =
    let genesis_winner_account : Account_config.account_data =
      let pk, _ = Coda_state.Consensus_state_hooks.genesis_winner in
      {pk; sk= None; balance= Currency.Balance.of_int 1000; delegate= None}
    in
    if
      List.exists accounts (fun acc ->
          Signature_lib.Public_key.Compressed.equal acc.pk
            genesis_winner_account.pk )
    then accounts
    else genesis_winner_account :: accounts
  in
  let all_accounts =
    let fake_accounts =
      Account_config.Fake_accounts.generate
        (max (n - List.length real_accounts) 0)
    in
    real_accounts @ fake_accounts
  in
  (*the accounts file that can be edited later*)
  Out_channel.with_file (genesis_dir ^/ "accounts.json") ~f:(fun json_file ->
      Yojson.Safe.pretty_to_channel json_file
        (Account_config.to_yojson all_accounts) ) ;
  all_accounts

let main accounts_json_file genesis_dir n =
  let open Deferred.Let_syntax in
  let%bind genesis_dir =
    let dir =
      Option.value ~default:Cache_dir.autogen_path genesis_dir
      |> Cache_dir.genesis_state_path
    in
    let%map () = File_system.create_dir dir ~clear_if_exists:true in
    dir
  in
  let%bind accounts = get_accounts accounts_json_file genesis_dir n in
  match
    Or_error.try_with_join (fun () ->
        let open Or_error.Let_syntax in
        let%map accounts = accounts in
        let ledger =
          create ~directory_name:(genesis_dir ^/ "ledger") accounts
        in
        let () = commit ledger in
        ledger )
  with
  | Ok ledger ->
      let%bind _base_hash, base_proof =
        if use_dummy_values then
          return
            ( Snark_params.Tick.Field.zero
            , Dummy_values.Tock.Bowe_gabizon18.proof )
        else generate_base_proof ~ledger
      in
      let%map wr = Writer.open_file (genesis_dir ^/ "base_proof") in
      Writer.write wr (Proof.Stable.V1.sexp_of_t base_proof |> Sexp.to_string)
  | Error e ->
      failwithf "Failed to create genesis ledger\n%s" (Error.to_string_hum e)
        ()

let () =
  Command.run
    (Command.async
       ~summary:
         "Create the genesis ledger with configurable accounts, balances, and \
          delegates "
       Command.(
         let open Let_syntax in
         let open Command.Param in
         let%map accounts_json =
           flag "account-file"
             ~doc:
               "Filepath of the json file that has all the account data in \
                the format: [{\"pk\":public-key-string, \
                \"sk\":optional-secret-key-string, \"balance\":int, \
                \"delegate\":optional-public-key-string}]"
             (optional string)
         and genesis_dir =
           flag "genesis-dir"
             ~doc:
               "Dir where the genesis ledger and genesis proof is to be saved"
             (optional string)
         and n =
           flag "n"
             ~doc:
               (sprintf
                  "Int Total number of accounts in the ledger (Maximum: %d). \
                   If the number of accounts in the account file, say x, is \
                   less than n then the tool will generate (n-x) fake \
                   accounts (default: x)."
                  (Int.pow 2 ledger_depth))
             (optional int)
         in
         fun () ->
           let max = Int.pow 2 ledger_depth in
           if Option.value ~default:0 n >= max then
             failwith (sprintf "Invalid value for n (0 <= n <= %d)" max)
           else main accounts_json genesis_dir (Option.value ~default:0 n)))
