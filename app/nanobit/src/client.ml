open Core
open Async
open Cli_lib
open Signature_lib
open Nanobit_base

let of_local_port port = Host_and_port.create ~host:"127.0.0.1" ~port

let dispatch rpc query port =
  Tcp.with_connection
    (Tcp.Where_to_connect.of_host_and_port (of_local_port port))
    ~timeout:(Time.Span.of_sec 1.)
    (fun _ r w ->
      let open Deferred.Let_syntax in
      match%bind Rpc.Connection.create r w ~connection_state:(fun _ -> ()) with
      | Error exn -> return (Or_error.of_exn exn)
      | Ok conn -> Rpc.Rpc.dispatch rpc conn query )

let daemon_port_flag =
  Command.Param.flag "daemon-port"
    ~doc:
      (Printf.sprintf "PORT Client to daemon local communication (default: %d)"
         default_client_port)
    (Command.Param.optional int16)

let json_flag =
  Command.Param.(flag "json" no_arg ~doc:"Use json output (default: plaintext)")

module Daemon_cli = struct
  type state =
    | Start
    | Show_menu
    | Select_action
    | Run_daemon
    | Run_client
    | Abort

  let reader = Reader.stdin

  let does_daemon_exist port =
    let open Deferred.Let_syntax in
    let%map result =
      Rpc.Connection.client
        (Tcp.Where_to_connect.of_host_and_port (of_local_port port))
    in
    Result.is_ok result

  let kill p =
    Process.run_exn () ~prog:"kill" ~args:["-9"; Pid.to_string @@ Process.pid p]

  let print_menu () =
    printf
      "%!Before starting a client command, you will need to start the Coda \
       daemon.\n\
       Would you like to run the daemon?\n\
       -----------------------------------\n\n\
       %!"

  let timeout = Time.Span.of_sec 10.0

  let heartbeat = Time.Span.of_sec 0.5

  let invoke_daemon port =
    let rec check_daemon () =
      let%bind result = does_daemon_exist port in
      if result then Deferred.unit
      else
        let%bind () = Async.after heartbeat in
        check_daemon ()
    in
    let our_binary = Sys.executable_name in
    let args = ["daemon"; "-background"; "-client-port"; sprintf "%d" port] in
    let%bind p = Process.create_exn () ~prog:our_binary ~args in
    (* Wait for process to start the client server *)
    match%bind Async.Clock.with_timeout timeout (check_daemon ()) with
    | `Result _ -> Deferred.unit
    | `Timeout ->
        let%bind _ = kill p in
        failwith "Cannot connect to daemon"

  let run ~f port arg =
    let port = Option.value port ~default:default_client_port in
    let rec go = function
      | Start ->
          let%bind has_daemon = does_daemon_exist port in
          if has_daemon then go Run_client else go Show_menu
      | Show_menu -> print_menu () ; go Select_action
      | Select_action -> (
          printf "[Y/n]: " ;
          match%bind Reader.read_line (Lazy.force reader) with
          | `Eof -> go Select_action
          | `Ok input ->
            match String.capitalize input with
            | "Y" | "YES" | "" -> go Run_daemon
            | "N" | "NO" -> go Abort
            | _ -> go Select_action )
      | Run_daemon ->
          let%bind () = invoke_daemon port in
          go Run_client
      | Run_client -> f port arg
      | Abort -> Deferred.unit
    in
    go Start

  let init ~f arg_flag =
    let open Command.Param.Applicative_infix in
    Command.Param.return (fun port arg () -> run ~f port arg)
    <*> daemon_port_flag <*> arg_flag
end

let get_balance =
  let open Command.Param in
  let open Deferred.Let_syntax in
  let address_flag =
    flag "address"
      ~doc:
        "PUBLICKEY Public-key address of which you want to check the balance"
      (required public_key)
  in
  Command.async ~summary:"Get balance associated with an address"
    (Daemon_cli.init address_flag ~f:(fun port address ->
         match%map
           dispatch Client_lib.Get_balance.rpc
             (Public_key.compress address)
             port
         with
         | Ok (Some b) -> printf "%s\n" (Currency.Balance.to_string b)
         | Ok None ->
             printf "No account found at that public_key (zero balance)\n"
         | Error e -> printf "Failed to send txn %s\n" (Error.to_string_hum e)
     ))

let get_public_keys =
  let open Deferred.Let_syntax in
  let open Client_lib in
  Command.async ~summary:"Get public keys"
    (Daemon_cli.init json_flag ~f:(fun port json ->
         dispatch Get_public_keys.rpc () port
         >>| print (module Public_key) json ))

let get_nonce addr port =
  let open Deferred.Let_syntax in
  match%map
    dispatch Client_lib.Get_nonce.rpc (Public_key.compress addr) port
  with
  | Ok (Some n) -> Ok n
  | Ok None -> Error "No account found at that public_key"
  | Error e -> Error (Error.to_string_hum e)

let status =
  let open Deferred.Let_syntax in
  let open Client_lib in
  Command.async ~summary:"Get running daemon status"
    (Daemon_cli.init json_flag ~f:(fun port json ->
         dispatch Get_status.rpc () port >>| print (module Status) json ))

let send_txn =
  let open Command.Param in
  let address_flag =
    flag "receiver"
      ~doc:"PUBLICKEY Public-key address to which you want to send money"
      (required public_key)
  in
  let from_account_flag =
    flag "from"
      ~doc:
        "PRIVATEKEY Private-key address from which you would like to send money"
      (required private_key)
  in
  let fee_flag =
    flag "fee" ~doc:"VALUE  Transaction fee you're willing to pay (default: 1)"
      (optional txn_fee)
  in
  let amount_flag =
    flag "amount" ~doc:"VALUE Transaction amount you want to send"
      (required txn_amount)
  in
  let flag =
    let open Command.Param in
    return (fun a b c d -> (a, b, c, d))
    <*> address_flag <*> from_account_flag <*> fee_flag <*> amount_flag
  in
  Command.async ~summary:"Send transaction to an address"
    (Daemon_cli.init flag ~f:(fun port (address, from_account, fee, amount) ->
         let open Deferred.Let_syntax in
         let receiver_compressed = Public_key.compress address in
         let sender_kp = Keypair.of_private_key_exn from_account in
         match%bind get_nonce sender_kp.public_key port with
         | Error e ->
             printf "Failed to get nonce %s\n" e ;
             return ()
         | Ok nonce ->
             let fee = Option.value ~default:(Currency.Fee.of_int 1) fee in
             let payload : Transaction.Payload.t =
               {receiver= receiver_compressed; amount; fee; nonce}
             in
             let txn = Transaction.sign sender_kp payload in
             match%map
               dispatch Client_lib.Send_transaction.rpc
                 (txn :> Transaction.t)
                 port
             with
             | Ok () -> printf "Successfully enqueued transaction in pool\n"
             | Error e ->
                 printf "Failed to send transaction %s\n"
                   (Error.to_string_hum e) ))

let generate_keypair =
  Command.async ~summary:"Generate a new public-key/private-key pair"
    (Command.Param.return (fun () ->
         let keypair = Keypair.create () in
         printf "public-key: %s\nprivate-key: %s\n"
           (Public_key.Compressed.to_base64
              (Public_key.compress keypair.public_key))
           (Private_key.to_base64 keypair.private_key) ;
         exit 0 ))

let command =
  Command.group ~summary:"Lightweight client process"
    [ ("get-balance", get_balance)
    ; ("get-public-keys", get_public_keys)
    ; ("send-txn", send_txn)
    ; ("status", status)
    ; ("generate-keypair", generate_keypair) ]
