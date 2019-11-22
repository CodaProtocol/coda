open Async
open Signature_lib

let main privkey_path =
  let%map {public_key; _} =
    Secrets.Keypair.Terminal_stdin.read_exn privkey_path
  in
  printf "%s\n%!" (Public_key.to_base58_check public_key)

let cmd =
  Command.async
    ~summary:
      "Display the latest-verison public key corresponding to the given \
       private key."
    Command.(
      let open Let_syntax in
      let%map path = Cli_lib.Flag.privkey_read_path in
      fun () -> main path)

let () = Command.run cmd
