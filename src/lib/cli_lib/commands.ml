open Signature_lib
open Core_kernel
open Async

let generate_keypair =
  Command.async ~summary:"Generate a new public, private keypair"
    (let open Command.Let_syntax in
    let env = Secrets.Keypair.env in
    if Option.is_some (Sys.getenv env) then
      printf "Using password from environment variable %s\n" env ;
    let%map_open privkey_path = Flag.privkey_write_path in
    Exceptions.handle_nicely
    @@ fun () ->
    let open Deferred.Let_syntax in
    let kp = Keypair.create () in
    let%bind () = Secrets.Keypair.Terminal_stdin.write_exn kp ~privkey_path in
    printf "Keypair generated\nPublic key: %s\nRaw public key: %s\n"
      ( kp.public_key |> Public_key.compress
      |> Public_key.Compressed.to_base58_check )
      (Rosetta_coding.Coding.of_public_key kp.public_key) ;
    exit 0)

let validate_keypair =
  Command.async ~summary:"Validate a public, private keypair"
    (let open Command.Let_syntax in
    let open Core_kernel in
    let%map_open privkey_path = Flag.privkey_write_path in
    Exceptions.handle_nicely
    @@ fun () ->
    let read_pk () =
      let pubkey_path = privkey_path ^ ".pub" in
      try
        In_channel.with_file pubkey_path ~f:(fun in_channel ->
            match In_channel.input_line in_channel with
            | Some line -> (
              try Public_key.Compressed.of_base58_check_exn line
              with _exn ->
                eprintf
                  "Could not create public key in file %s from text: %s\n"
                  pubkey_path line ;
                exit 1 )
            | None ->
                eprintf "No public key found in file %s\n" pubkey_path ;
                exit 1 )
      with exn ->
        eprintf "Could not read public key file %s, error: %s\n" pubkey_path
          (Exn.to_string exn) ;
        exit 1
    in
    let compare_public_keys ~pk_from_disk ~pk_from_keypair =
      if Public_key.Compressed.equal pk_from_disk pk_from_keypair then
        printf "Public key on-disk is derivable from private key\n"
      else (
        eprintf
          "Public key read from disk %s different than public key %s derived \
           from private key\n"
          (Public_key.Compressed.to_base58_check pk_from_disk)
          (Public_key.Compressed.to_base58_check pk_from_keypair) ;
        exit 1 )
    in
    let validate_transaction keypair =
      let dummy_payload = Mina_base.Signed_command_payload.dummy in
      let signature =
        Mina_base.Signed_command.sign_payload keypair.Keypair.private_key
          dummy_payload
      in
      let message = Mina_base.Signed_command.to_input dummy_payload in
      let verified =
        Schnorr.verify signature
          (Snark_params.Tick.Inner_curve.of_affine keypair.public_key)
          message
      in
      if verified then
        printf "Verified a transaction using specified keypair\n"
      else (
        eprintf "Failed to verify a transaction using the specific keypair\n" ;
        exit 1 )
    in
    let open Deferred.Let_syntax in
    let%bind () =
      let password =
        lazy
          (Secrets.Keypair.Terminal_stdin.prompt_password "Enter password: ")
      in
      match%map Secrets.Keypair.read ~privkey_path ~password with
      | Ok keypair ->
          let pk_from_disk = read_pk () in
          compare_public_keys ~pk_from_disk
            ~pk_from_keypair:(keypair.public_key |> Public_key.compress) ;
          validate_transaction keypair
      | Error err ->
          eprintf "Could not read the specified keypair: %s\n"
            (Secrets.Privkey_error.to_string err) ;
          exit 1
    in
    exit 0)

let validate_transaction =
  Command.async
    ~summary:
      "Validate the signature on one or more transactions, provided to stdin \
       in rosetta format"
    ( Command.Param.return
    @@ fun () ->
    let num_fails = ref 0 in
    let jsons = Yojson.Safe.stream_from_channel In_channel.stdin in
    ( match
        Or_error.try_with (fun () ->
            Caml.Stream.iter
              (fun transaction_json ->
                match
                  Rosetta_lib.Transaction.to_mina_signed transaction_json
                with
                | Ok cmd ->
                    if Mina_base.Signed_command.check_signature cmd then
                      Format.eprintf "Transaction was valid@."
                    else (
                      incr num_fails ;
                      Format.eprintf "Transaction was invalid@." )
                | Error err ->
                    incr num_fails ;
                    Format.eprintf
                      "Failed to validate transaction:@.%s@.Failed with \
                       error:%s@."
                      (Yojson.Safe.pretty_to_string transaction_json)
                      (Yojson.Safe.pretty_to_string
                         (Error_json.error_to_yojson err)) )
              jsons )
      with
    | Ok () ->
        ()
    | Error err ->
        Format.eprintf "Error:@.%s@.@."
          (Yojson.Safe.pretty_to_string (Error_json.error_to_yojson err)) ;
        Format.printf "Invalid transaction.@." ;
        Core_kernel.exit 1 ) ;
    if !num_fails > 0 then (
      Format.printf "Some transactions failed to verify@." ;
      exit 1 )
    else (
      Format.printf "All transactions were valid@." ;
      exit 0 ) )

module Vrf = struct
  let generate_witness =
    Command.async
      ~summary:
        "Generate a vrf evaluation witness. This may be used to calculate \
         whether a given private key will win a given slot (by checking \
         threshold_met = true in the JSON output), or to generate a witness \
         that a 3rd party can use to verify a vrf evaluation."
      (let open Command.Let_syntax in
      let env = Secrets.Keypair.env in
      if Option.is_some (Sys.getenv env) then
        printf "Using password from environment variable %s\n" env ;
      let%map_open privkey_path = Flag.privkey_write_path
      and global_slot =
        flag "--global-slot" ~doc:"NUM Global slot to evaluate the VRF for"
          (required int)
      and epoch_seed =
        flag "--epoch-seed" ~doc:"SEED Epoch seed to evaluate the VRF with"
          (required string)
      and delegator_index =
        flag "--delegator-index"
          ~doc:"NUM The index of the delegating account in the epoch ledger"
          (required int)
      and generate_outputs =
        flag "--generate-outputs"
          ~doc:
            "true|false Whether to generate the vrf in addition to the \
             witness (default: false)"
          (optional_with_default false bool)
      and delegated_stake =
        flag "--delegated-stake"
          ~doc:
            "AMOUNT The balance of the delegating account in the epoch ledger"
          (optional int)
      and total_stake =
        flag "--total-stake"
          ~doc:"AMOUNT The total balance of all accounts in the epoch ledger"
          (optional int)
      in
      Exceptions.handle_nicely
      @@ fun () ->
      let open Deferred.Let_syntax in
      (* TODO-someday: constraint constants from config file. *)
      let constraint_constants =
        Genesis_constants.Constraint_constants.compiled
      in
      let%bind () =
        let password =
          lazy
            (Secrets.Keypair.Terminal_stdin.prompt_password "Enter password: ")
        in
        match%bind Secrets.Keypair.read ~privkey_path ~password with
        | Ok keypair ->
            let open Consensus_vrf.Layout in
            let evaluation =
              Evaluation.of_message_and_sk ~constraint_constants
                { global_slot= Mina_numbers.Global_slot.of_int global_slot
                ; epoch_seed=
                    Mina_base.Epoch_seed.of_base58_check_exn epoch_seed
                ; delegator_index }
                keypair.private_key
            in
            let evaluation =
              match (delegated_stake, total_stake) with
              | Some delegated_stake, Some total_stake ->
                  { evaluation with
                    vrf_threshold=
                      Some
                        { delegated_stake=
                            Currency.Balance.of_int delegated_stake
                        ; total_stake= Currency.Amount.of_int total_stake } }
              | _ ->
                  evaluation
            in
            let evaluation =
              if generate_outputs then
                Evaluation.compute_vrf ~constraint_constants evaluation
              else evaluation
            in
            Format.printf "%a@."
              (Yojson.Safe.pretty_print ?std:None)
              (Evaluation.to_yojson evaluation) ;
            Deferred.return ()
        | Error err ->
            eprintf "Could not read the specified keypair: %s\n"
              (Secrets.Privkey_error.to_string err) ;
            exit 1
      in
      exit 0)

  let batch_generate_witness =
    Command.async
      ~summary:
        "Generate a batch of vrf evaluation witnesses from {\"globalSlot\": \
         _, \"epochSeed\": _, \"delegatorIndex\": _} JSON message objects \
         read on stdin"
      (let open Command.Let_syntax in
      let env = Secrets.Keypair.env in
      if Option.is_some (Sys.getenv env) then
        printf "Using password from environment variable %s\n" env ;
      let%map_open privkey_path = Flag.privkey_write_path in
      Exceptions.handle_nicely
      @@ fun () ->
      let open Deferred.Let_syntax in
      (* TODO-someday: constraint constants from config file. *)
      let constraint_constants =
        Genesis_constants.Constraint_constants.compiled
      in
      let%bind () =
        let password =
          lazy
            (Secrets.Keypair.Terminal_stdin.prompt_password "Enter password: ")
        in
        match%bind Secrets.Keypair.read ~privkey_path ~password with
        | Ok keypair ->
            let lexbuf = Lexing.from_channel In_channel.stdin in
            let lexer = Yojson.init_lexer () in
            Deferred.repeat_until_finished () (fun () ->
                Deferred.Or_error.try_with (fun () ->
                    try
                      let message_json =
                        Yojson.Safe.from_lexbuf ~stream:true lexer lexbuf
                      in
                      let open Consensus_vrf.Layout in
                      let message =
                        Result.ok_or_failwith (Message.of_yojson message_json)
                      in
                      let evaluation =
                        Evaluation.of_message_and_sk ~constraint_constants
                          message keypair.private_key
                      in
                      Format.printf "%a@."
                        (Yojson.Safe.pretty_print ?std:None)
                        (Evaluation.to_yojson evaluation) ;
                      Deferred.return (`Repeat ())
                    with Yojson.End_of_input -> return (`Finished ()) )
                >>| function
                | Ok x ->
                    x
                | Error err ->
                    Format.eprintf "Error:@.%s@.@."
                      (Yojson.Safe.pretty_to_string
                         (Error_json.error_to_yojson err)) ;
                    `Repeat () )
        | Error err ->
            eprintf "Could not read the specified keypair: %s\n"
              (Secrets.Privkey_error.to_string err) ;
            exit 1
      in
      exit 0)

  let batch_check_witness =
    Command.async
      ~summary:
        "Check a batch of vrf evaluation witnesses read on stdin. Outputs the \
         verified vrf evaluations (or no vrf output if the witness is \
         invalid), and whether the vrf output satisfies the threshold values \
         if given. The threshold should be included in the JSON for each vrf \
         as the 'vrfThreshold' field, of format {delegatedStake: 1000, \
         totalStake: 1000000000}. The threshold is not checked against a \
         ledger; this should be done manually to confirm whether \
         threshold_met in the output corresponds to an actual won block."
      ( Command.Param.return @@ Exceptions.handle_nicely
      @@ fun () ->
      let open Deferred.Let_syntax in
      (* TODO-someday: constraint constants from config file. *)
      let constraint_constants =
        Genesis_constants.Constraint_constants.compiled
      in
      let lexbuf = Lexing.from_channel In_channel.stdin in
      let lexer = Yojson.init_lexer () in
      let%bind () =
        Deferred.repeat_until_finished () (fun () ->
            Deferred.Or_error.try_with (fun () ->
                try
                  let evaluation_json =
                    Yojson.Safe.from_lexbuf ~stream:true lexer lexbuf
                  in
                  let open Consensus_vrf.Layout in
                  let evaluation =
                    Result.ok_or_failwith
                      (Evaluation.of_yojson evaluation_json)
                  in
                  let evaluation =
                    Evaluation.compute_vrf ~constraint_constants evaluation
                  in
                  Format.printf "%a@."
                    (Yojson.Safe.pretty_print ?std:None)
                    (Evaluation.to_yojson evaluation) ;
                  Deferred.return (`Repeat ())
                with Yojson.End_of_input -> return (`Finished ()) )
            >>| function
            | Ok x ->
                x
            | Error err ->
                Format.eprintf "Error:@.%s@.@."
                  (Yojson.Safe.pretty_to_string
                     (Error_json.error_to_yojson err)) ;
                `Repeat () )
      in
      exit 0 )

  let command_group =
    Command.group ~summary:"Commands for vrf evaluations"
      [ ("generate-witness", generate_witness)
      ; ("batch-generate-witness", batch_generate_witness)
      ; ("batch-check-witness", batch_check_witness) ]
end
