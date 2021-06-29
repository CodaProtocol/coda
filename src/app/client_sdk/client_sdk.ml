(* client_sdk.ml *)

[%%import
"/src/config.mlh"]

[%%ifdef
consensus_mechanism]

[%%error
"Client SDK cannot be built if \"consensus_mechanism\" is defined"]

[%%endif]

open Js_of_ocaml
open Snark_params_nonconsensus
open Signature_lib_nonconsensus
open Mina_base_nonconsensus
open Rosetta_lib_nonconsensus
open Rosetta_coding_nonconsensus
open Js_util

let _ =
  Js.export "minaSDK"
    (object%js (_self)
       (** public key corresponding to a private key *)
       method publicKeyOfPrivateKey (sk_base58_check_js : string_js) =
         let sk =
           Js.to_string sk_base58_check_js |> Private_key.of_base58_check_exn
         in
         Public_key.(
           of_private_key_exn sk |> compress |> Compressed.to_base58_check
           |> Js.string)

       (** generate a private key, public key pair *)
       method genKeys =
         let sk = Private_key.create () in
         let sk_str_js = sk |> Private_key.to_base58_check |> Js.string in
         let pk_str_js = _self##publicKeyOfPrivateKey sk_str_js in
         object%js
           val privateKey = sk_str_js

           val publicKey = pk_str_js
         end

       (** return public key associated with private key in raw hex format for Rosetta *)
       method rawPublicKeyOfPrivateKey (sk_base58_check_js : string_js) =
         let sk =
           Js.to_string sk_base58_check_js |> Private_key.of_base58_check_exn
         in
         Public_key.of_private_key_exn sk |> Coding.of_public_key |> Js.string

       (** return public key in raw hex format for Rosetta *)
       method rawPublicKeyOfPublicKey (pk_base58_check_js : string_js) =
         let pk =
           Js.to_string pk_base58_check_js
           |> Public_key.of_base58_check_decompress_exn
         in
         Coding.of_public_key_compressed pk |> Js.string

       (** return public key, given that key in raw hex format for Rosetta *)
       method publicKeyOfRawPublicKey (pk_raw_js : string_js) =
         let pk_raw_str = Js.to_string pk_raw_js in
         Coding.to_public_key_compressed pk_raw_str
         |> Public_key.Compressed.to_base58_check |> Js.string

       (** is the public key valid and derivable from private key; can
           the private key be used to sign a transaction?
       *)
       method validKeypair (keypair_js : keypair_js) =
         let sk_base58_check = Js.to_string keypair_js##.privateKey in
         let pk_base58_check = Js.to_string keypair_js##.publicKey in
         let derived_pk =
           Js.to_string @@ _self##publicKeyOfPrivateKey keypair_js##.privateKey
         in
         if not (String.equal pk_base58_check derived_pk) then
           raise_js_error "Public key not derivable from private key"
         else
           let sk = Private_key.of_base58_check_exn sk_base58_check in
           let pk =
             Public_key.Compressed.of_base58_check_exn pk_base58_check
             |> Public_key.decompress_exn
           in
           let dummy_payload =
             Mina_base_nonconsensus.Signed_command_payload.dummy
           in
           let signature =
             Mina_base_nonconsensus.Signed_command.sign_payload sk
               dummy_payload
           in
           let message =
             Mina_base_nonconsensus.Signed_command.to_input dummy_payload
           in
           let verified =
             Schnorr.verify signature
               (Snark_params_nonconsensus.Inner_curve.of_affine pk)
               message
           in
           if verified then Js._true
           else raise_js_error "Could not sign a transaction with private key"

       (** sign arbitrary string with private key *)
       method signString (sk_base58_check_js : string_js) (str_js : string_js)
           =
         let sk_base58_check = Js.to_string sk_base58_check_js in
         let sk = Private_key.of_base58_check_exn sk_base58_check in
         let str = Js.to_string str_js in
         String_sign.Schnorr.sign sk str |> signature_to_js_object

       (** verify signature of arbitrary string signed with signString *)
       method verifyStringSignature (signature_js : signature_js)
           (public_key_js : string_js) (str_js : string_js) : bool Js.t =
         let field = Js.to_string signature_js##.field |> Field.of_string in
         let scalar =
           Js.to_string signature_js##.scalar |> Inner_curve.Scalar.of_string
         in
         let signature = (field, scalar) in
         let pk =
           Js.to_string public_key_js
           |> Public_key.Compressed.of_base58_check_exn
           |> Public_key.decompress_exn
         in
         let inner_curve =
           Snark_params_nonconsensus.Inner_curve.of_affine pk
         in
         let str = Js.to_string str_js in
         if String_sign.Schnorr.verify signature inner_curve str then Js._true
         else Js._false

       (** sign payment transaction payload with private key *)
       method signPayment (sk_base58_check_js : string_js)
           (payment_js : payment_js) : signed_payment =
         let sk_base58_check = Js.to_string sk_base58_check_js in
         let sk = Private_key.of_base58_check_exn sk_base58_check in
         let payload = payload_of_payment_js payment_js in
         let signature =
           Signed_command.sign_payload sk payload |> signature_to_js_object
         in
         let publicKey = _self##publicKeyOfPrivateKey sk_base58_check_js in
         object%js
           val payment = payment_js

           val sender = publicKey

           val signature = signature
         end

       (** verify signed payments *)
       method verifyPaymentSignature (signed_payment : signed_payment)
           : bool Js.t =
         let payload : Signed_command_payload.t =
           payload_of_payment_js signed_payment##.payment
         in
         let signer =
           signed_payment##.sender |> Js.to_string
           |> Public_key.Compressed.of_base58_check_exn
           |> Public_key.decompress_exn
         in
         let signature = signature_of_js_object signed_payment##.signature in
         let signed = Signed_command.Poly.{payload; signer; signature} in
         if Signed_command.check_signature signed then Js._true else Js._false

       (** sign payment transaction payload with private key *)
       method signStakeDelegation (sk_base58_check_js : string_js)
           (stake_delegation_js : stake_delegation_js)
           : signed_stake_delegation =
         let sk_base58_check = Js.to_string sk_base58_check_js in
         let sk = Private_key.of_base58_check_exn sk_base58_check in
         let payload = payload_of_stake_delegation_js stake_delegation_js in
         let signature =
           Signed_command.sign_payload sk payload |> signature_to_js_object
         in
         let publicKey = _self##publicKeyOfPrivateKey sk_base58_check_js in
         object%js
           val stakeDelegation = stake_delegation_js

           val sender = publicKey

           val signature = signature
         end

       (** verify signed delegations *)
       method verifyStakeDelegationSignature
             (signed_stake_delegation : signed_stake_delegation) : bool Js.t =
         let payload : Signed_command_payload.t =
           payload_of_stake_delegation_js
             signed_stake_delegation##.stakeDelegation
         in
         let signer =
           signed_stake_delegation##.sender
           |> Js.to_string |> Public_key.Compressed.of_base58_check_exn
           |> Public_key.decompress_exn
         in
         let signature =
           signature_of_js_object signed_stake_delegation##.signature
         in
         let signed = Signed_command.Poly.{payload; signer; signature} in
         if Signed_command.check_signature signed then Js._true else Js._false

       (** sign a transaction in Rosetta rendered format *)
       method signRosettaTransaction (sk_base58_check_js : string_js)
           (unsignedRosettaTxn : string_js) =
         let sk_base58_check = Js.to_string sk_base58_check_js in
         let sk = Private_key.of_base58_check_exn sk_base58_check in
         let unsigned_txn_json =
           Js.to_string unsignedRosettaTxn |> Yojson.Safe.from_string
         in
         let make_error err =
           let json = `Assoc [("error", `String err)] in
           Js.string (Yojson.Safe.to_string json)
         in
         let make_signed_transaction command nonce =
           let payload_or_err =
             command
             |> Rosetta_lib_nonconsensus.User_command_info.Partial
                .to_user_command_payload ~nonce
           in
           match payload_or_err with
           | Ok payload -> (
               let signature =
                 Signed_command.sign_payload sk payload |> Signature.Raw.encode
               in
               let signed_txn =
                 Transaction.Signed.{command; nonce; signature}
               in
               match Transaction.Signed.render signed_txn with
               | Ok signed ->
                   let json = Transaction.Signed.Rendered.to_yojson signed in
                   let json' = `Assoc [("data", json)] in
                   Js.string (Yojson.Safe.to_string json')
               | Error errs ->
                   make_error (Rosetta_lib_nonconsensus.Errors.show errs) )
           | Error errs ->
               make_error (Rosetta_lib_nonconsensus.Errors.show errs)
         in
         match Transaction.Unsigned.Rendered.of_yojson unsigned_txn_json with
         | Ok
             { random_oracle_input= _
             ; payment= Some payment
             ; stake_delegation= None
             ; create_token= None
             ; create_token_account= None
             ; mint_tokens= None } ->
             let command = Transaction.Unsigned.of_rendered_payment payment in
             make_signed_transaction command payment.nonce
         | Ok
             { random_oracle_input= _
             ; payment= None
             ; stake_delegation= Some delegation
             ; create_token= None
             ; create_token_account= None
             ; mint_tokens= None } ->
             let command =
               Transaction.Unsigned.of_rendered_delegation delegation
             in
             make_signed_transaction command delegation.nonce
         | Ok _ ->
             make_error
               "Unsigned transaction must contain a payment or a delegation, \
                exclusively"
         | Error msg ->
             make_error msg

       method signedRosettaTransactionToSignedCommand
             (signedRosettaTxn : string_js) =
         let signed_txn_json =
           Js.to_string signedRosettaTxn |> Yojson.Safe.from_string
         in
         let result_json =
           match Transaction.to_mina_signed signed_txn_json with
           | Ok signed_cmd ->
               let cmd_json = Signed_command.to_yojson signed_cmd in
               `Assoc [("data", cmd_json)]
           | Error err ->
               let open Core_kernel in
               let err_msg =
                 sprintf
                   "Could not parse JSON for signed Rosetta transaction: %s"
                   (Error.to_string_hum err)
               in
               `Assoc [("error", `String err_msg)]
         in
         Js.string (Yojson.Safe.to_string result_json)

       method runUnitTests () : bool Js.t = Coding.run_unit_tests () ; Js._true
    end)
