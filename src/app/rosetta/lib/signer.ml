(* A to-spec signer library that uses internal coda libs *)

open Core_kernel
open Signature_lib
open Rosetta_lib
open Rosetta_coding
module Signature = Mina_base.Signature
module User_command = Mina_base.User_command
module Signed_command = Mina_base.Signed_command

module Keys = struct
  type t = {keypair: Keypair.t; public_key_hex_bytes: string}

  let of_keypair keypair =
    {keypair; public_key_hex_bytes= Coding.of_public_key keypair.public_key}

  let to_private_key_bytes t = Coding.of_scalar t.keypair.private_key

  let of_private_key_bytes s =
    Coding.to_scalar s |> Keypair.of_private_key_exn |> of_keypair

  let of_private_key_box secret_box_string =
    let json = Yojson.Safe.from_string secret_box_string in
    let sb : Secrets.Secret_box.t =
      Secrets.Secret_box.of_yojson json
      |> Result.ok
      |> Option.value_exn ~here:[%here] ?error:None ?message:None
    in
    let output : Bytes.t =
      Secrets.Secret_box.decrypt ~password:(Bytes.of_string "") sb
      |> Result.ok
      |> Option.value_exn ~here:[%here] ?error:None ?message:None
    in
    let sk = output |> Bigstring.of_bytes |> Private_key.of_bigstring_exn in
    (*printf !"Secret key hex bytes is: %s\n" (Coding.of_scalar sk) ;*)
    of_keypair (Keypair.of_private_key_exn sk)
end

(* Returns signed_transaction_string *)
let sign ~(keys : Keys.t) ~unsigned_transaction_string =
  let open Result.Let_syntax in
  let%bind json =
    try return (Yojson.Safe.from_string unsigned_transaction_string)
    with _ -> Result.fail (Errors.create (`Json_parse None))
  in
  let%map unsigned_transaction =
    Transaction.Unsigned.Rendered.of_yojson json
    |> Result.map_error ~f:(fun e -> Errors.create (`Json_parse (Some e)))
    |> Result.bind ~f:Transaction.Unsigned.of_rendered
  in
  let user_command_payload =
    User_command_info.Partial.to_user_command_payload
      ~nonce:unsigned_transaction.Transaction.Unsigned.nonce
      unsigned_transaction.command
    |> Result.ok
    |> Option.value_exn ~here:[%here] ?error:None ?message:None
  in
  let signature =
    Schnorr.sign keys.keypair.private_key
      unsigned_transaction.random_oracle_input
  in
  let signature' =
    Signed_command.sign_payload keys.keypair.private_key user_command_payload
  in
  [%test_eq: Signature.t] signature signature' ;
  signature |> Signature.Raw.encode

let verify ~public_key_hex_bytes ~signed_transaction_string =
  let open Result.Let_syntax in
  let%bind json =
    try return (Yojson.Safe.from_string signed_transaction_string)
    with _ -> Result.fail (Errors.create (`Json_parse None))
  in
  let%bind signed_transaction =
    Transaction.Signed.Rendered.of_yojson json
    |> Result.map_error ~f:(fun e -> Errors.create (`Json_parse (Some e)))
    |> Result.bind ~f:Transaction.Signed.of_rendered
  in
  let public_key : Public_key.t = Coding.to_public_key public_key_hex_bytes in
  let%map signature =
    Signature.Raw.decode signed_transaction.signature
    |> Result.of_option
         ~error:
           (Errors.create ~context:"Signature malformed" (`Json_parse None))
  in
  let user_command_payload =
    User_command_info.Partial.to_user_command_payload
      ~nonce:signed_transaction.nonce
      signed_transaction.Transaction.Signed.command
    |> Result.ok
    |> Option.value_exn ~here:[%here] ?error:None ?message:None
  in
  let message = Signed_command.to_input user_command_payload in
  Schnorr.verify signature
    (Snark_params.Tick.Inner_curve.of_affine public_key)
    message
