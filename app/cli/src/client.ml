open Core
open Async
open Cli_lib
open Signature_lib
open Coda_base

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
    Process.run_exn () ~prog:"kill" ~args:[Pid.to_string @@ Process.pid p]

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

let get_nonce_cmd =
  let open Command.Param in
  let address_flag =
    flag "address" ~doc:"PUBLICKEY Public-key address you want the nonce for"
      (required public_key)
  in
  Command.async ~summary:"Get the current nonce for an account"
    (Daemon_cli.init address_flag ~f:(fun port address ->
         match%bind get_nonce address port with
         | Error e ->
             eprintf "Failed to get nonce: %s\n" e ;
             exit 1
         | Ok nonce ->
             printf "%s\n" (Account.Nonce.to_string nonce) ;
             exit 0 ))

let status =
  let open Deferred.Let_syntax in
  let open Client_lib in
  Command.async ~summary:"Get running daemon status"
    (Daemon_cli.init json_flag ~f:(fun port json ->
         dispatch Get_status.rpc () port >>| print (module Status) json ))

let handle_open ~mkdir ~(f: string -> 'a Deferred.t) path : 'a Deferred.t =
  let open Unix.Error in
  let dn = Filename.dirname path in
  let%bind parent_exists =
    match%bind Sys.is_directory ~follow_symlinks:true dn with
    | `No ->
        eprintf "Error: %s is not a directory\n" dn ;
        exit 1
    | `Unknown -> return false
    | `Yes -> return true
  in
  let%bind () =
    match%bind
      Monitor.try_with ~extract_exn:true (fun () ->
          if not parent_exists && mkdir then
            let%bind () = Unix.mkdir ~p:() dn in
            Unix.chmod dn 700
          else if not parent_exists then (
            eprintf
              "Error: %s does not exist\nHint: mkdir -p %s; chmod 700 %s\n" dn
              dn dn ;
            exit 1 )
          else Deferred.unit )
    with
    | Ok x -> return x
    | Error (Unix.Unix_error ((EACCES as e), _, _)) ->
        eprintf "Error: could not mkdir -p %s: %s\n" dn (message e) ;
        exit 1
    | Error e -> raise e
  in
  match%bind Monitor.try_with ~extract_exn:true (fun () -> f path) with
  | Ok x -> return x
  | Error (Unix.Unix_error (e, _, _)) ->
      eprintf "Error: could not open %s: %s\n" path (message e) ;
      exit 1
  | Error e -> raise e

let write_keypair {Keypair.private_key; public_key; _} privkey_path
    ~(password: unit -> Bytes.t Deferred.t) =
  let%bind privkey_f =
    handle_open ~mkdir:true ~f:Writer.open_file privkey_path
  in
  let%bind pubkey_f =
    handle_open ~mkdir:false ~f:Writer.open_file (privkey_path ^ ".pub")
  in
  let%bind password = password () in
  let sb =
    Secret_box.encrypt
      ~plaintext:(Private_key.to_bigstring private_key |> Bigstring.to_bytes)
      ~password
  in
  let sb =
    Secret_box.to_yojson sb |> Yojson.Safe.to_string |> Bytes.of_string
  in
  Writer.write_bytes privkey_f sb ;
  let%bind () = Writer.close privkey_f in
  let%bind () = Unix.chmod privkey_path ~perm:0o600 in
  let pubkey_bytes =
    Public_key.Compressed.to_base64 (Public_key.compress public_key)
    |> Bytes.of_string
  in
  Writer.write_bytes pubkey_f pubkey_bytes ;
  let%bind () = Writer.close pubkey_f in
  Deferred.unit

let read_keypair_exn privkey_path ~(password: unit -> Bytes.t Deferred.t) =
  let open Deferred.Let_syntax in
  let read_all r =
    Pipe.to_list (Reader.lines r) >>| fun ss -> String.concat ~sep:"\n" ss
  in
  let%bind privkey_file =
    handle_open ~mkdir:false ~f:Reader.open_file privkey_path
  in
  let%bind file_contents = read_all privkey_file in
  let%bind sb =
    match Secret_box.of_yojson (Yojson.Safe.from_string file_contents) with
    | Ok sb -> return sb
    | Error e ->
        eprintf "Error parsing %s, is your keyfile corrupt?: %s\n" privkey_path
          e ;
        exit 1
  in
  let%bind password = password () in
  let%bind pk =
    match Secret_box.decrypt ~password sb with
    | Ok pk_bytes -> (
      try
        pk_bytes |> Bigstring.of_bytes |> Private_key.of_bigstring_exn
        |> return
      with exn ->
        eprintf
          "Error parsing decrypted private key, is your keyfile corrupt?: %s\n"
          (Exn.to_string exn) ;
        exit 1 )
    | Error e ->
        eprintf "Error decrypting %s: %s\n" privkey_path
          (Error.to_string_hum e) ;
        exit 1
  in
  try return @@ Keypair.of_private_key_exn pk with exn ->
    eprintf
      "Error computing public key from private, is your keyfile corrupt?: %s\n"
      (Exn.to_string exn) ;
    exit 1

let rec prompt_password prompt =
  let%bind pw1 = read_password_exn prompt in
  let%bind pw2 = read_password_exn "Again to confirm: " in
  if not (Bytes.equal pw1 pw2) then (
    eprintf "Error: passwords don't match, try again\n" ;
    prompt_password prompt )
  else return pw2

let privkey_path_flag =
  let open Command.Param in
  flag "privkey-path"
    ~doc:"FILE File to write private key into (public key will be FILE.pub)"
    (required file)

let send_txn =
  let open Command.Param in
  let address_flag =
    flag "receiver"
      ~doc:"PUBLICKEY Public-key address to which you want to send money"
      (required public_key)
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
    <*> address_flag <*> privkey_path_flag <*> fee_flag <*> amount_flag
  in
  Command.async ~summary:"Send transaction to an address"
    (Daemon_cli.init flag ~f:(fun port (address, from_account, fee, amount) ->
         let open Deferred.Let_syntax in
         let receiver_compressed = Public_key.compress address in
         let perm_error = ref false in
         let%bind st = Unix.stat from_account in
         if st.perm land 0o777 <> 0o600 then (
           eprintf
             "Error: insecure permissions on `%s`. They should be 0600, they \
              are %o\n"
             from_account (st.perm land 0o777) ;
           perm_error := true ) ;
         let%bind st = Unix.stat (Filename.dirname from_account) in
         if st.perm land 0o777 <> 0o700 then (
           eprintf
             "Error: insecure permissions on `%s`. They should be 0700, they \
              are %o\n"
             (Filename.dirname from_account)
             (st.perm land 0o777) ;
           perm_error := true ) ;
         let%bind () = if !perm_error then exit 1 else Deferred.unit in
         let%bind sender_kp =
           read_keypair_exn from_account ~password:(fun () ->
               prompt_password "Private key password: " )
         in
         match%bind get_nonce sender_kp.public_key port with
         | Error e ->
             eprintf "Failed to get nonce %s\n" e ;
             exit 1
         | Ok nonce ->
             let fee = Option.value ~default:(Currency.Fee.of_int 1) fee in
             let payload : Transaction.Payload.t =
               {receiver= receiver_compressed; amount; fee; nonce}
             in
             let txn = Transaction.sign sender_kp payload in
             match%bind
               dispatch Client_lib.Send_transaction.rpc
                 (txn :> Transaction.t)
                 port
             with
             | Ok () ->
                 printf "Successfully enqueued transaction in pool\n" ;
                 Deferred.unit
             | Error e ->
                 eprintf "Failed to send transaction %s\n"
                   (Error.to_string_hum e) ;
                 exit 1 ))

let wrap_key =
  Command.async ~summary:"Wrap a private key into a private key file"
    (let open Command.Let_syntax in
    let%map_open privkey_path = privkey_path_flag in
    fun () ->
      let open Deferred.Let_syntax in
      let%bind privkey =
        hidden_line_or_env "Private key: " ~env:"CODA_PRIVKEY"
      in
      let pk = Private_key.of_base64_exn (Bytes.to_string privkey) in
      let kp = Keypair.of_private_key_exn pk in
      write_keypair kp privkey_path ~password:(fun () ->
          prompt_password "Password for new private key file: " ))

let dump_keypair =
  Command.async ~summary:"Print out a keypair from a private key file"
    (let open Command.Let_syntax in
    let%map_open privkey_path = privkey_path_flag in
    fun () ->
      let open Deferred.Let_syntax in
      let%map kp =
        read_keypair_exn privkey_path ~password:(fun () ->
            prompt_password "Password for private key file: " )
      in
      printf "Public key: %s\nPrivate key: %s\n"
        ( kp.public_key |> Public_key.compress
        |> Public_key.Compressed.to_base64 )
        (kp.private_key |> Private_key.to_base64))

let generate_keypair =
  Command.async ~summary:"Generate a new public-key/private-key pair"
    (let open Command.Let_syntax in
    let%map_open privkey_path = privkey_path_flag in
    fun () ->
      let open Deferred.Let_syntax in
      let kp = Keypair.create () in
      let%bind () =
        write_keypair kp privkey_path ~password:(fun () ->
            prompt_password "Password for new private key file: " )
      in
      printf "Public key: %s\n"
        ( kp.public_key |> Public_key.compress
        |> Public_key.Compressed.to_base64 ) ;
      exit 0)

let command =
  Command.group ~summary:"Lightweight client process"
    [ ("get-balance", get_balance)
    ; ("get-public-keys", get_public_keys)
    ; ("get-nonce", get_nonce_cmd)
    ; ("send-txn", send_txn)
    ; ("status", status)
    ; ("wrap-key", wrap_key)
    ; ("dump-keypair", dump_keypair)
    ; ("generate-keypair", generate_keypair) ]
