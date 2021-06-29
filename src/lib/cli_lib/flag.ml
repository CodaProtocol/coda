open Core

let json =
  Command.Param.(
    flag "--json" ~aliases:["json"] no_arg
      ~doc:"Use JSON output (default: plaintext)")

let plaintext =
  Command.Param.(
    flag "--plaintext" ~aliases:["plaintext"] no_arg
      ~doc:"Use plaintext input or output (default: JSON)")

let performance =
  Command.Param.(
    flag "--performance" ~aliases:["performance"] no_arg
      ~doc:
        "Include performance histograms in status output (default: don't \
         include)")

let privkey_write_path =
  let open Command.Param in
  flag "--privkey-path" ~aliases:["privkey-path"]
    ~doc:"FILE File to write private key into (public key will be FILE.pub)"
    (required string)

let privkey_read_path =
  let open Command.Param in
  flag "--privkey-path" ~aliases:["privkey-path"]
    ~doc:"FILE File to read private key from" (required string)

let conf_dir =
  let open Command.Param in
  flag "--config-directory" ~aliases:["config-directory"]
    ~doc:"DIR Configuration directory" (optional string)

module Doc_builder = struct
  type 'value t =
    { type_name: string
    ; description: string
    ; examples: 'value list
    ; display: 'value -> string }

  let create ~display ?(examples = []) type_name description =
    {type_name; description; examples; display}

  let display ~default {type_name; description; examples; display} =
    let open Printf in
    let example_text =
      if List.is_empty examples then ""
      else
        sprintf "(examples: %s)"
          (String.concat ~sep:", " @@ List.map examples ~f:display)
    in
    let default_text =
      Option.value_map ~default:"" default
        ~f:(Fn.compose (sprintf !" (default: %s)") display)
    in
    sprintf !"%s %s %s%s" type_name description example_text default_text
end

module Types = struct
  type 'a with_name = {name: string; value: 'a}

  type 'a with_name_and_displayed_default =
    {name: string; value: 'a option; default: 'a}

  (*Difference between Optional and Optional_value is that the name is still accessible if the value is None*)
  type ('value, 'output) t =
    | Optional : ('value, 'value with_name option) t
    | Optional_value : ('value, 'value option with_name) t
    | Optional_with_displayed_default :
        'value
        -> ('value, 'value with_name_and_displayed_default) t
    | Resolve_with_default : 'value -> ('value, 'value with_name) t
end

let setup_flag ~arg_type ~name ?aliases doc =
  let open Command.Let_syntax in
  Command.Param.flag name ?aliases ~doc (Command.Param.optional arg_type)
  >>| Option.map ~f:(fun value -> {Types.name; value})

let create (type value output) :
       name:string
    -> ?aliases:string list
    -> arg_type:value Command.Arg_type.t
    -> value Doc_builder.t
    -> (value, output) Types.t
    -> output Command.Param.t =
  let open Command.Let_syntax in
  fun ~name ?aliases ~arg_type doc_builder -> function
    | Optional ->
        setup_flag ~arg_type ~name ?aliases
          (Doc_builder.display ~default:None doc_builder)
    | Optional_value -> (
        setup_flag ~arg_type ~name ?aliases
          (Doc_builder.display ~default:None doc_builder)
        >>| function
        | Some {name; value} ->
            {Types.name; value= Some value}
        | None ->
            {name; value= None} )
    | Optional_with_displayed_default default -> (
        setup_flag ~arg_type ~name ?aliases
          (Doc_builder.display ~default:(Some default) doc_builder)
        >>| function
        | Some {name; value} ->
            {Types.name; value= Some value; default}
        | None ->
            {name; value= None; default} )
    | Resolve_with_default default ->
        setup_flag ~arg_type ~name ?aliases
          (Doc_builder.display ~default:(Some default) doc_builder)
        >>| Option.value ~default:{Types.name; value= default}

module Port = struct
  let to_string = Int.to_string

  let doc_builder description =
    Doc_builder.create ~display:to_string "PORT" description

  let create_optional ~name ?aliases description =
    create ~name ?aliases (doc_builder description) Optional_value
      ~arg_type:Arg_type.int16

  let create ~name ?aliases ~default description =
    create ~name ?aliases (doc_builder description)
      (Optional_with_displayed_default default) ~arg_type:Arg_type.int16

  let default_client = 8301

  let default_rest = 0xc0d

  let default_archive = default_rest + 1

  let default_libp2p = 8302

  let of_raw raw =
    let open Or_error.Let_syntax in
    let%bind () =
      Result.ok_if_true
        (String.for_all raw ~f:Char.is_digit)
        ~error:(Error.of_string "Not a number")
    in
    Arg_type.validate_int16 (Int.of_string raw)

  let to_host_and_port port = Host_and_port.create ~host:"127.0.0.1" ~port

  let to_uri ~path port =
    Uri.of_string ("http://localhost:" ^ string_of_int port ^/ path)

  module Daemon = struct
    let external_ =
      create ~name:"--external-port" ~aliases:["external-port"]
        ~default:default_libp2p
        "Port to use for all libp2p communications (gossip and RPC)"

    let client =
      create ~name:"--client-port" ~aliases:["client-port"]
        ~default:default_client
        "local RPC-server for clients to interact with the daemon"

    let rest_server =
      create ~name:"--rest-port" ~aliases:["rest-port"] ~default:default_rest
        "local REST-server for daemon interaction"

    let limited_graphql_server =
      create_optional ~name:"--limited-graphql-port"
        ~aliases:["limited-graphql-port"]
        "GraphQL-server for limited daemon interaction"
  end

  module Archive = struct
    let server =
      create ~name:"--server-port" ~aliases:["server-port"]
        ~default:default_archive "port to launch the archive server"
  end
end

module Host = struct
  let localhost = Core.Unix.Host.getbyname_exn "localhost"

  let is_localhost host =
    Option.value_map ~default:false (Unix.Host.getbyname host) ~f:(fun host ->
        Core.Unix.Host.have_address_in_common host localhost )
end

let example_host = "154.97.53.97"

module Host_and_port = struct
  let parse_host_and_port raw =
    match Port.of_raw raw with
    | Ok port ->
        Port.to_host_and_port port
    | Error _ ->
        Host_and_port.of_string raw

  let arg_type : Host_and_port.t Command.Arg_type.t =
    Command.Arg_type.map Command.Param.string ~f:parse_host_and_port

  let is_localhost (host_and_port : Host_and_port.t) =
    Host.is_localhost (Host_and_port.host host_and_port)

  let to_string host_and_port =
    if is_localhost host_and_port then
      Int.to_string @@ Host_and_port.port host_and_port
    else Host_and_port.to_string host_and_port

  let create_examples port =
    [Port.to_host_and_port port; Host_and_port.create ~host:example_host ~port]

  let make_doc_builder description example_port =
    Doc_builder.create ~display:to_string
      ~examples:(create_examples example_port)
      "HOST:PORT/LOCALHOST-PORT"
      (sprintf "%s. If HOST is omitted, then localhost is assumed to be HOST."
         description)

  module Client = struct
    let daemon =
      create ~name:"--daemon-port" ~aliases:["daemon-port"] ~arg_type
        (make_doc_builder "Client to local daemon communication"
           Port.default_client)
        (Resolve_with_default (Port.to_host_and_port Port.default_client))
  end

  module Daemon = struct
    let archive =
      create ~name:"--archive-address" ~aliases:["archive-address"] ~arg_type
        (make_doc_builder "Daemon to archive process communication"
           Port.default_archive)
        Optional
  end
end

module Uri = struct
  let parse_uri ~path raw =
    match Port.of_raw raw with
    | Ok port ->
        Port.to_uri ~path port
    | Error _ ->
        Uri.of_string raw

  let arg_type ~path =
    Command.Arg_type.map Command.Param.string ~f:(parse_uri ~path)

  let is_localhost (host_and_port : Uri.t) =
    Option.value_map ~default:false (Uri.host host_and_port)
      ~f:Host.is_localhost

  let to_string uri =
    if is_localhost uri then
      sprintf "%i or %s" (Option.value_exn (Uri.port uri)) (Uri.to_string uri)
    else Uri.to_string uri

  module Client = struct
    let doc_builder =
      Doc_builder.create ~display:to_string
        ~examples:
          [ Port.to_uri ~path:"graphql" Port.default_rest
          ; Uri.of_string
              ( "/dns4/peer1-rising-phoenix.o1test.net" ^ ":"
              ^ Int.to_string Port.default_rest
              ^/ "graphql" ) ]
        "URI/LOCALHOST-PORT" "graphql rest server for daemon interaction"

    let name = "rest-server"

    let default = Port.to_uri ~path:"graphql" Port.default_rest

    let rest_graphql =
      create ~name:"--rest-server" ~aliases:["rest-server"]
        ~arg_type:(arg_type ~path:"graphql") doc_builder
        (Resolve_with_default default)

    let rest_graphql_opt =
      create ~name:"--rest-server" ~aliases:["rest-server"]
        ~arg_type:(arg_type ~path:"graphql") doc_builder Optional
  end

  module Archive = struct
    let postgres =
      let doc_builder =
        Doc_builder.create ~display:to_string
          ~examples:
            [Uri.of_string "postgres://admin:codarules@postgres:5432/archiver"]
          "URI" "URI for postgresql database"
      in
      create ~name:"--postgres-uri" ~aliases:["postgres-uri"]
        ~arg_type:(Command.Arg_type.map Command.Param.string ~f:Uri.of_string)
        doc_builder
        (Resolve_with_default
           (Uri.of_string "postgres://admin:codarules@postgres:5432/archiver"))
  end
end

module Log = struct
  let json =
    let open Command.Param in
    flag "--log-json" ~aliases:["log-json"] no_arg
      ~doc:"Print log output as JSON (default: plain text)"

  let all_levels =
    String.concat ~sep:"|" (List.map ~f:Logger.Level.show Logger.Level.all)

  let level =
    let log_level = Arg_type.log_level in
    let open Command.Param in
    let doc = sprintf "LEVEL Set log level (%s, default: Info)" all_levels in
    flag "--log-level" ~aliases:["log-level"] ~doc
      (optional_with_default Logger.Level.Info log_level)

  let file_log_level =
    let log_level = Arg_type.log_level in
    let open Command.Param in
    let doc =
      sprintf "LEVEL Set log level for the log file (%s, default: Trace)"
        all_levels
    in
    flag "--file-log-level" ~aliases:["file-log-level"] ~doc
      (optional_with_default Logger.Level.Trace log_level)
end

type signed_command_common =
  { sender: Signature_lib.Public_key.Compressed.t
  ; fee: Currency.Fee.t
  ; nonce: Mina_base.Account.Nonce.t option
  ; memo: string option }

let signed_command_common : signed_command_common Command.Param.t =
  let open Command.Let_syntax in
  let open Arg_type in
  let%map_open sender =
    flag "--sender" ~aliases:["sender"]
      (required public_key_compressed)
      ~doc:"PUBLICKEY Public key from which you want to send the transaction"
  and fee =
    flag "--fee" ~aliases:["fee"]
      ~doc:
        (Printf.sprintf
           "FEE Amount you are willing to pay to process the transaction \
            (default: %s) (minimum: %s)"
           (Currency.Fee.to_formatted_string
              Mina_compile_config.default_transaction_fee)
           (Currency.Fee.to_formatted_string
              Mina_base.Signed_command.minimum_fee))
      (optional txn_fee)
  and nonce =
    flag "--nonce" ~aliases:["nonce"]
      ~doc:
        "NONCE Nonce that you would like to set for your transaction \
         (default: nonce of your account on the best ledger or the successor \
         of highest value nonce of your sent transactions from the \
         transaction pool )"
      (optional txn_nonce)
  and memo =
    flag "--memo" ~aliases:["memo"]
      ~doc:"STRING Memo accompanying the transaction" (optional string)
  in
  { sender
  ; fee= Option.value fee ~default:Mina_compile_config.default_transaction_fee
  ; nonce
  ; memo }

module Signed_command = struct
  open Arg_type

  let hd_index =
    let open Command.Param in
    flag "--hd-index" ~aliases:["HD-index"]
      ~doc:"HD-INDEX Index used by hardware wallet" (required hd_index)

  let receiver_pk =
    let open Command.Param in
    flag "--receiver" ~aliases:["receiver"]
      ~doc:"PUBLICKEY Public key to which you want to send money"
      (required public_key_compressed)

  let amount =
    let open Command.Param in
    flag "--amount" ~aliases:["amount"]
      ~doc:"VALUE Payment amount you want to send" (required txn_amount)

  let fee =
    let open Command.Param in
    flag "--fee" ~aliases:["fee"]
      ~doc:
        (Printf.sprintf
           "FEE Amount you are willing to pay to process the transaction \
            (default: %s) (minimum: %s)"
           (Currency.Fee.to_formatted_string
              Mina_compile_config.default_transaction_fee)
           (Currency.Fee.to_formatted_string
              Mina_base.Signed_command.minimum_fee))
      (optional txn_fee)

  let valid_until =
    let open Command.Param in
    flag "--valid-until" ~aliases:["valid-until"]
      ~doc:
        "GLOBAL-SLOT The last global-slot at which this transaction will be \
         considered valid. This makes it possible to have transactions which \
         expire if they are not applied before this time. If omitted, the \
         transaction will never expire."
      (optional global_slot)

  let nonce =
    let open Command.Param in
    flag "--nonce" ~aliases:["nonce"]
      ~doc:
        "NONCE Nonce that you would like to set for your transaction \
         (default: nonce of your account on the best ledger or the successor \
         of highest value nonce of your sent transactions from the \
         transaction pool )"
      (optional txn_nonce)

  let memo =
    let open Command.Param in
    flag "--memo" ~aliases:["memo"]
      ~doc:
        (sprintf
           "STRING Memo accompanying the transaction (up to %d characters)"
           Mina_base.Signed_command_memo.max_input_length)
      (optional string)
end
