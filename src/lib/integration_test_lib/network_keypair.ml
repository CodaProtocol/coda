open Signature_lib
open Core_kernel

type t =
  { keypair: Keypair.t
  ; secret_name: string
  ; public_key_file: string
  ; private_key_file: string }
[@@deriving to_yojson]

let create_network_keypair ~keypair ~secret_name =
  let open Keypair in
  let public_key_file =
    Public_key.Compressed.to_base58_check
      (Public_key.compress keypair.public_key)
    ^ "\n"
  in
  let private_key_file =
    let plaintext =
      Bigstring.to_bytes (Private_key.to_bigstring keypair.private_key)
    in
    let password = Bytes.of_string "naughty blue worm" in
    Secrets.Secret_box.encrypt ~plaintext ~password
    |> Secrets.Secret_box.to_yojson |> Yojson.Safe.to_string
  in
  {keypair; secret_name; public_key_file; private_key_file}
