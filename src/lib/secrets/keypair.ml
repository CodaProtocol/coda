open Signature_lib
open Core
open Async
open Async.Deferred.Let_syntax

let rec prompt_password prompt =
  let open Deferred.Let_syntax in
  let%bind pw1 = Password.read prompt in
  let%bind pw2 = Password.read "Again to confirm: " in
  if not (Bytes.equal pw1 pw2) then (
    eprintf "Error: passwords don't match, try again\n" ;
    prompt_password prompt )
  else return pw2

let error_raise e ~error_ctx =
  raise
    Error.(
      to_exn (of_string (sprintf !"%s\n%s" error_ctx (Error.to_string_hum e))))

(** Writes a keypair to [privkey_path] and [privkey_path ^ ".pub"] using [Secret_file] *)
let write_exn {Keypair.private_key; public_key} ~(privkey_path : string)
    ~(password : Secret_file.password) : unit Deferred.t =
  let privkey_bytes =
    Private_key.to_bigstring private_key |> Bigstring.to_bytes
  in
  let pubkey_string = Public_key.to_base58_check public_key in
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
      Privkey_error.raise e

(** Reads a private key from [privkey_path] using [Secret_file] *)
let read ~(privkey_path : string) ~(password : Secret_file.password) :
    (Keypair.t, Privkey_error.t) Deferred.Result.t =
  let open Deferred.Result.Let_syntax in
  let%bind pk_bytes = Secret_file.read ~path:privkey_path ~password in
  let open Result.Let_syntax in
  Deferred.return
  @@ let%bind sk =
       try
         return (pk_bytes |> Bigstring.of_bytes |> Private_key.of_bigstring_exn)
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
      Privkey_error.raise priv_key_error

let read_exn' path =
  read_exn ~privkey_path:path
    ~password:(lazy (Password.read "Secret key password: "))

module Terminal_stdin = struct
  let read_exn ?(should_reask = true) path =
    let read_privkey password = read ~privkey_path:path ~password in
    let%bind result =
      match Sys.getenv Password.default_password_env with
      | Some password ->
          read_privkey (lazy (Deferred.return @@ Bytes.of_string password))
      | None ->
          let read_file () =
            read_privkey
              (lazy (Password.read_hidden_line "Secret key password: "))
          in
          let rec read_until_correct () =
            match%bind read_file () with
            | Ok result ->
                Deferred.Result.return result
            | Error `Incorrect_password_or_corrupted_privkey ->
                eprintf "Wrong password! Please try again\n" ;
                read_until_correct ()
            | Error exn ->
                Privkey_error.raise exn
          in
          if should_reask then read_until_correct () else read_file ()
    in
    match result with
    | Ok result ->
        return result
    | Error e ->
        Privkey_error.raise e

  let write_exn kp ~privkey_path =
    write_exn kp ~privkey_path
      ~password:(lazy (prompt_password "Password for new private key file: "))
end
