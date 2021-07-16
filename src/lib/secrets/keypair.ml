open Signature_lib
open Core
open Async
open Async.Deferred.Let_syntax
open Keypair_common

module T = struct
  type t = Keypair.t

  let env = "MINA_PRIVKEY_PASS"

  let which = "Mina keypair"

  (** Writes a keypair to [privkey_path] and [privkey_path ^ ".pub"] using [Secret_file] *)
  let write_exn { Keypair.private_key; public_key } ~(privkey_path : string)
      ~(password : Secret_file.password) : unit Deferred.t =
    let privkey_bytes =
      Private_key.to_bigstring private_key |> Bigstring.to_bytes
    in
    let pubkey_string =
      Public_key.Compressed.to_base58_check (Public_key.compress public_key)
    in
    match%bind
      Secret_file.write ~path:privkey_path ~mkdir:true ~plaintext:privkey_bytes
        ~password
    with
    | Ok () ->
        (* The hope is that if [Secret_file.write] succeeded then this ought to
           as well, letting [handle_open] stay inside [Secret_file]. It might not
           if the environment changes underneath us, and we won't have nice errors
           in that case. *)
        let%bind pubkey_f = Writer.open_file (privkey_path ^ ".pub") in
        Writer.write_line pubkey_f pubkey_string ;
        Writer.close pubkey_f
    | Error e ->
        Privkey_error.raise ~which e

  (** Reads a private key from [privkey_path] using [Secret_file] *)
  let read ~(privkey_path : string) ~(password : Secret_file.password) :
      (Keypair.t, Privkey_error.t) Deferred.Result.t =
    let open Deferred.Result.Let_syntax in
    let%bind pk_bytes = Secret_file.read ~path:privkey_path ~password in
    let open Result.Let_syntax in
    Deferred.return
    @@ let%bind sk =
         try
           return
             (pk_bytes |> Bigstring.of_bytes |> Private_key.of_bigstring_exn)
         with exn ->
           Privkey_error.corrupted_privkey
             (Error.createf "Error parsing decrypted private key file: %s"
                (Exn.to_string exn))
       in
       try return (Keypair.of_private_key_exn sk)
       with exn ->
         Privkey_error.corrupted_privkey
           (Error.createf
              "Error computing public key from private, is your keyfile \
               corrupt? %s"
              (Exn.to_string exn))

  (** Reads a private key from [privkey_path] using [Secret_file], throws on failure *)
  let read_exn ~(privkey_path : string) ~(password : Secret_file.password) :
      Keypair.t Deferred.t =
    match%map read ~privkey_path ~password with
    | Ok keypair ->
        keypair
    | Error priv_key_error ->
        Privkey_error.raise ~which priv_key_error

  let read_exn' path =
    let password =
      let env_value = Sys.getenv env in
      let env_deprecated_value = Sys.getenv env_deprecated in
      match (env_value, env_deprecated_value) with
      | Some v, _ | None, Some v ->
          lazy (return @@ Bytes.of_string v)
      | None, None ->
          let error_help_message =
            sprintf "Set the %s environment variable to the password" env
          in
          lazy
            (Password.read_hidden_line ~error_help_message
               "Secret key password: ")
    in
    read_exn ~privkey_path:path ~password
end

include T
module Terminal_stdin = Make_terminal_stdin (T)
