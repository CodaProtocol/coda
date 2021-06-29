(** An agent that pokes at Coda and peeks at Rosetta to see if things look alright *)

open Core_kernel
open Async
open Rosetta_lib
open Signature_lib
open Lib

let sign_command =
  let open Command.Let_syntax in
  let%map_open unsigned_transaction =
    flag "--unsigned-transaction" ~aliases:["unsigned-transaction"]
      ~doc:"Unsigned transaction string returned from Rosetta"
      (required string)
  and private_key =
    flag "--private-key" ~aliases:["private-key"] ~doc:"Private key hex bytes"
      (required string)
  in
  let open Deferred.Let_syntax in
  fun () ->
    let keys = Signer.Keys.of_private_key_bytes private_key in
    match
      Signer.sign ~keys ~unsigned_transaction_string:unsigned_transaction
    with
    | Ok signature ->
        printf "%s\n" signature ; return ()
    | Error e ->
        eprintf "Failed to sign transaction %s" (Errors.show e) ;
        exit 1

let verify_command =
  let open Command.Let_syntax in
  let%map_open signed_transaction =
    flag "--signed-transaction" ~aliases:["signed-transaction"]
      ~doc:"Signed transaction string returned from Rosetta" (required string)
  and public_key =
    flag "--public-key" ~aliases:["public-key"] ~doc:"Public key hex bytes"
      (required string)
  in
  let open Deferred.Let_syntax in
  fun () ->
    match
      Signer.verify ~public_key_hex_bytes:public_key
        ~signed_transaction_string:signed_transaction
    with
    | Ok b when b ->
        return ()
    | Ok _b (* when not _b *) ->
        eprintf "Signature does not verify against this public key" ;
        exit 1
    | Error e ->
        eprintf "Failed to verify signature %s" (Errors.show e) ;
        exit 1

let derive_command =
  let open Command.Let_syntax in
  let%map_open private_key =
    flag "--private-key" ~aliases:["private-key"] ~doc:"Private key hex bytes"
      (required string)
  in
  let open Deferred.Let_syntax in
  fun () ->
    printf "%s\n"
      Signer.Keys.(of_private_key_bytes private_key).public_key_hex_bytes ;
    return ()

let generate_command =
  let open Deferred.Let_syntax in
  Command.Param.return
  @@ fun () ->
  let keypair = Keypair.create () in
  printf "%s\n" Signer.Keys.(of_keypair keypair |> to_private_key_bytes) ;
  return ()

let commands =
  [ ("sign", Command.async ~summary:"Sign an unsigned transaction" sign_command)
  ; ( "verify"
    , Command.async
        ~summary:
          "Verify the signature of a signed transaction. Exits 0 if the \
           signature verifies."
        verify_command )
  ; ( "derive-public-key"
    , Command.async ~summary:"Import a private key, returns a public-key"
        derive_command )
  ; ( "generate-private-key"
    , Command.async ~summary:"Generate a new private key" generate_command ) ]
