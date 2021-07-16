open Core
open Async
open Async.Deferred.Let_syntax
open Keypair_common

module T = struct
  type t = Mina_net2.Keypair.t

  let env = "MINA_LIBP2P_PASS"

  let which = "libp2p keypair"

  (** Writes a keypair to [privkey_path] and [privkey_path ^ ".pub"] using [Secret_file] *)
  let write_exn kp ~(privkey_path : string) ~(password : Secret_file.password) :
      unit Deferred.t =
    let str = Mina_net2.Keypair.to_string kp in
    match%bind
      Secret_file.write ~path:privkey_path ~mkdir:true
        ~plaintext:(Bytes.of_string str) ~password
    with
    | Ok () ->
        (* The hope is that if [Secret_file.write] succeeded then this ought to
           as well, letting [handle_open] stay inside [Secret_file]. It might not
           if the environment changes underneath us, and we won't have nice errors
           in that case. *)
        let%bind pubkey_f = Writer.open_file (privkey_path ^ ".peerid") in
        Writer.write_line pubkey_f (Mina_net2.Keypair.to_peer_id kp) ;
        Writer.close pubkey_f
    | Error e ->
        Privkey_error.raise ~which e

  (** Reads a private key from [privkey_path] using [Secret_file] *)
  let read ~(privkey_path : string) ~(password : Secret_file.password) :
      (t, Privkey_error.t) Deferred.Result.t =
    let open Deferred.Result.Let_syntax in
    let%bind bytes = Secret_file.read ~path:privkey_path ~password in
    Deferred.return
    @@
    match Mina_net2.Keypair.of_string (Bytes.to_string bytes) with
    | Ok kp ->
        Ok kp
    | Error e ->
        Privkey_error.corrupted_privkey e

  (** Reads a private key from [privkey_path] using [Secret_file], throws on failure *)
  let read_exn ~(privkey_path : string) ~(password : Secret_file.password) :
      t Deferred.t =
    match%map read ~privkey_path ~password with
    | Ok keypair ->
        keypair
    | Error priv_key_error ->
        Privkey_error.raise ~which priv_key_error

  let read_exn' path =
    read_exn ~privkey_path:path
      ~password:
        (lazy (Password.hidden_line_or_env "Libp2p secret key password: " ~env))
end

include T
module Terminal_stdin = Make_terminal_stdin (T)
