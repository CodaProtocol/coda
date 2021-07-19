open Core
open Async
open Integration_test_lib

(* exclude from bisect_ppx to avoid type error on GraphQL modules *)
[@@@coverage exclude_file]

module Node = struct
  type t =
    { swarm_name : string
    ; service_id : string
    ; ports : (string * string) list
    ; graphql_enabled : bool
    ; network_keypair : Network_keypair.t option
    }

  let id { service_id; _ } = service_id

  let network_keypair { network_keypair; _ } = network_keypair

  let get_container_cmd t =
    Printf.sprintf "$(docker ps -f name=%s --quiet)" t.service_id

  let run_in_postgresql_container _ _ = failwith "run_in_postgresql_container"

  let get_logs_in_container _ = failwith "get_logs_in_container"

  let run_in_container node cmd =
    let base_docker_cmd = "docker exec" in
    let docker_cmd =
      Printf.sprintf "%s %s %s" base_docker_cmd (get_container_cmd node) cmd
    in
    let%bind.Deferred cwd = Unix.getcwd () in
    Malleable_error.return (Util.run_cmd_exn cwd "sh" [ "-c"; docker_cmd ])

  let start ~fresh_state node : unit Malleable_error.t =
    let open Malleable_error.Let_syntax in
    let%bind _ =
      Deferred.bind ~f:Malleable_error.return (run_in_container node "ps aux")
    in
    let%bind () =
      if fresh_state then
        let%bind _ = run_in_container node "rm -rf .mina-config/*" in
        Malleable_error.return ()
      else Malleable_error.return ()
    in
    let cmd =
      match String.substr_index node.service_id ~pattern:"snark-worker" with
      | Some _ ->
          (* Snark-workers should wait for work to be generated so they don't error a 'get_work' RPC call*)
          "/bin/bash -c 'sleep 120 && ./start.sh'"
      | None ->
          "./start.sh"
    in
    let%bind _ = run_in_container node cmd in
    Malleable_error.return ()

  let stop node =
    let open Malleable_error.Let_syntax in
    let%bind _ = run_in_container node "ps aux" in
    let%bind _ = run_in_container node "./stop.sh" in
    let%bind _ = run_in_container node "ps aux" in
    return ()

  module Decoders = Graphql_lib.Decoders

  module Graphql = struct
    let ingress_uri node =
      let host = Printf.sprintf "0.0.0.0" in
      let path = "/graphql" in
      let rest_port =
        List.find
          ~f:(fun ports -> String.equal (fst ports) "rest-port")
          node.ports
        |> Option.value_exn |> snd
      in
      Uri.make ~scheme:"http" ~host ~path ~port:(int_of_string rest_port) ()

    module Client = Graphql_lib.Client.Make (struct
      let preprocess_variables_string = Fn.id

      let headers = String.Map.empty
    end)

    module Unlock_account =
    [%graphql
    {|
      mutation ($password: String!, $public_key: PublicKey!) {
        unlockAccount(input: {password: $password, publicKey: $public_key }) {
          public_key: publicKey @bsDecoder(fn: "Decoders.public_key")
        }
      }
    |}]

    module Send_payment =
    [%graphql
    {|
      mutation ($sender: PublicKey!,
      $receiver: PublicKey!,
      $amount: UInt64!,
      $token: UInt64,
      $fee: UInt64!,
      $nonce: UInt32,
      $memo: String) {
        sendPayment(input:
          {from: $sender, to: $receiver, amount: $amount, token: $token, fee: $fee, nonce: $nonce, memo: $memo}) {
            payment {
              id
            }
          }
      }
    |}]

    module Get_balance =
    [%graphql
    {|
      query ($public_key: PublicKey, $token: UInt64) {
        account(publicKey: $public_key, token: $token) {
          balance {
            total @bsDecoder(fn: "Decoders.balance")
          }
        }
      }
    |}]

    module Query_peer_id =
    [%graphql
    {|
      query {
        daemonStatus {
          addrsAndPorts {
            peer {
              peerId
            }
          }
          peers {  peerId }

        }
      }
    |}]

    module Best_chain =
    [%graphql
    {|
      query {
        bestChain {
          stateHash
        }
      }
    |}]
  end

  let exec_graphql_request ?(num_tries = 10) ?(retry_delay_sec = 30.0)
      ?(initial_delay_sec = 30.0) ~logger ~node ~query_name query_obj =
    let open Deferred.Let_syntax in
    if not node.graphql_enabled then
      Deferred.Or_error.error_string
        "graphql is not enabled (hint: set `requires_graphql= true` in the \
         test config)"
    else
      let uri = Graphql.ingress_uri node in
      let metadata =
        [ ("query", `String query_name); ("uri", `String (Uri.to_string uri)) ]
      in
      [%log info] "Attempting to send GraphQL request \"$query\" to \"$uri\""
        ~metadata ;
      let rec retry n =
        if n <= 0 then (
          [%log error]
            "GraphQL request \"$query\" to \"$uri\" failed too many times"
            ~metadata ;
          Deferred.Or_error.errorf
            "GraphQL \"%s\" to \"%s\" request failed too many times" query_name
            (Uri.to_string uri) )
        else
          match%bind Graphql.Client.query query_obj uri with
          | Ok result ->
              [%log info] "GraphQL request \"$query\" to \"$uri\" succeeded"
                ~metadata ;
              Deferred.Or_error.return result
          | Error (`Failed_request err_string) ->
              [%log warn]
                "GraphQL request \"$query\" to \"$uri\" failed: \"$error\" \
                 ($num_tries attempts left)"
                ~metadata:
                  ( metadata
                  @ [ ("error", `String err_string)
                    ; ("num_tries", `Int (n - 1))
                    ] ) ;
              let%bind () = after (Time.Span.of_sec retry_delay_sec) in
              retry (n - 1)
          | Error (`Graphql_error err_string) ->
              [%log error]
                "GraphQL request \"$query\" to \"$uri\" returned an error: \
                 \"$error\" (this is a graphql error so not retrying)"
                ~metadata:(metadata @ [ ("error", `String err_string) ]) ;
              Deferred.Or_error.error_string err_string
      in
      let%bind () = after (Time.Span.of_sec initial_delay_sec) in
      retry num_tries

  let get_peer_id ~logger t =
    let open Deferred.Or_error.Let_syntax in
    [%log info] "Getting node's peer_id, and the peer_ids of node's peers"
      ~metadata:[ ("service_id", `String t.service_id) ] ;
    let query_obj = Graphql.Query_peer_id.make () in
    let%bind query_result_obj =
      exec_graphql_request ~logger ~node:t ~query_name:"query_peer_id" query_obj
    in
    [%log info] "get_peer_id, finished exec_graphql_request" ;
    let self_id_obj = query_result_obj#daemonStatus#addrsAndPorts#peer in
    let%bind self_id =
      match self_id_obj with
      | None ->
          Deferred.Or_error.error_string "Peer not found"
      | Some peer ->
          return peer#peerId
    in
    let peers = query_result_obj#daemonStatus#peers |> Array.to_list in
    let peer_ids = List.map peers ~f:(fun peer -> peer#peerId) in
    [%log info] "get_peer_id, result of graphql query (self_id,[peers]) (%s,%s)"
      self_id
      (String.concat ~sep:" " peer_ids) ;
    return (self_id, peer_ids)

  let must_get_peer_id ~logger t =
    get_peer_id ~logger t |> Deferred.bind ~f:Malleable_error.or_hard_error

  let get_best_chain ~logger t =
    let open Deferred.Or_error.Let_syntax in
    let query = Graphql.Best_chain.make () in
    let%bind result =
      exec_graphql_request ~logger ~node:t ~query_name:"best_chain" query
    in
    match result#bestChain with
    | None | Some [||] ->
        Deferred.Or_error.error_string "failed to get best chains"
    | Some chain ->
        return
        @@ List.map ~f:(fun block -> block#stateHash) (Array.to_list chain)

  let must_get_best_chain ~logger t =
    get_best_chain ~logger t |> Deferred.bind ~f:Malleable_error.or_hard_error

  let get_balance ~logger t ~account_id =
    let open Deferred.Or_error.Let_syntax in
    [%log info] "Getting account balance"
      ~metadata:
        [ ("service_id", `String t.service_id)
        ; ("account_id", Mina_base.Account_id.to_yojson account_id)
        ] ;
    let pk = Mina_base.Account_id.public_key account_id in
    let token = Mina_base.Account_id.token_id account_id in
    let get_balance_obj =
      Graphql.Get_balance.make
        ~public_key:(Graphql_lib.Encoders.public_key pk)
        ~token:(Graphql_lib.Encoders.token token)
        ()
    in
    let%bind balance_obj =
      exec_graphql_request ~logger ~node:t ~query_name:"get_balance_graphql"
        get_balance_obj
    in
    match balance_obj#account with
    | None ->
        Deferred.Or_error.errorf
          !"Account with %{sexp:Mina_base.Account_id.t} not found"
          account_id
    | Some acc ->
        return acc#balance#total

  let must_get_balance ~logger t ~account_id =
    get_balance ~logger t ~account_id
    |> Deferred.bind ~f:Malleable_error.or_hard_error

  (* if we expect failure, might want retry_on_graphql_error to be false *)
  let send_payment ~logger t ~sender_pub_key ~receiver_pub_key ~amount ~fee =
    [%log info] "Sending a payment"
      ~metadata:[ ("service_id", `String t.service_id) ] ;
    let open Deferred.Or_error.Let_syntax in
    let sender_pk_str =
      Signature_lib.Public_key.Compressed.to_string sender_pub_key
    in
    [%log info] "send_payment: unlocking account"
      ~metadata:[ ("sender_pk", `String sender_pk_str) ] ;
    let unlock_sender_account_graphql () =
      let unlock_account_obj =
        Graphql.Unlock_account.make ~password:"naughty blue worm"
          ~public_key:(Graphql_lib.Encoders.public_key sender_pub_key)
          ()
      in
      exec_graphql_request ~logger ~node:t
        ~query_name:"unlock_sender_account_graphql" unlock_account_obj
    in
    let%bind _ = unlock_sender_account_graphql () in
    let send_payment_graphql () =
      let send_payment_obj =
        Graphql.Send_payment.make
          ~sender:(Graphql_lib.Encoders.public_key sender_pub_key)
          ~receiver:(Graphql_lib.Encoders.public_key receiver_pub_key)
          ~amount:(Graphql_lib.Encoders.amount amount)
          ~fee:(Graphql_lib.Encoders.fee fee)
          ()
      in
      exec_graphql_request ~logger ~node:t ~query_name:"send_payment_graphql"
        send_payment_obj
    in
    let%map sent_payment_obj = send_payment_graphql () in
    let (`UserCommand id_obj) = sent_payment_obj#sendPayment#payment in
    let user_cmd_id = id_obj#id in
    [%log info] "Sent payment"
      ~metadata:[ ("user_command_id", `String user_cmd_id) ] ;
    ()

  let must_send_payment ~logger t ~sender_pub_key ~receiver_pub_key ~amount ~fee
      =
    send_payment ~logger t ~sender_pub_key ~receiver_pub_key ~amount ~fee
    |> Deferred.bind ~f:Malleable_error.or_hard_error

  let dump_archive_data ~logger:_ (_ : t) ~data_file:_ =
    failwith "dump_archive_data"

  let dump_container_logs ~logger:_ (_ : t) ~log_file:_ =
    Malleable_error.return ()

  let dump_precomputed_blocks ~logger:_ (_ : t) = Malleable_error.return ()
end

type t =
  { namespace : string
  ; constants : Test_config.constants
  ; seeds : Node.t list
  ; block_producers : Node.t list
  ; snark_coordinators : Node.t list
  ; archive_nodes : Node.t list
  ; testnet_log_filter : string
  ; keypairs : Signature_lib.Keypair.t list
  ; nodes_by_app_id : Node.t String.Map.t
  }

let constants { constants; _ } = constants

let constraint_constants { constants; _ } = constants.constraints

let genesis_constants { constants; _ } = constants.genesis

let seeds { seeds; _ } = seeds

let block_producers { block_producers; _ } = block_producers

let snark_coordinators { snark_coordinators; _ } = snark_coordinators

let archive_nodes { archive_nodes; _ } = archive_nodes

let keypairs { keypairs; _ } = keypairs

let all_nodes { seeds; block_producers; snark_coordinators; archive_nodes; _ } =
  List.concat [ seeds; block_producers; snark_coordinators; archive_nodes ]

let lookup_node_by_app_id t = Map.find t.nodes_by_app_id

let initialize ~logger network =
  Print.print_endline "initialize" ;
  let open Malleable_error.Let_syntax in
  let poll_interval = Time.Span.of_sec 15.0 in
  let max_polls = 60 (* 15 mins *) in
  let all_services =
    all_nodes network
    |> List.map ~f:(fun { service_id; _ } -> service_id)
    |> String.Set.of_list
  in
  let get_service_statuses () =
    let%map output =
      Deferred.bind ~f:Malleable_error.return
        (Util.run_cmd_exn "/" "docker"
           [ "service"; "ls"; "--format"; "{{.Name}}: {{.Replicas}}" ])
    in
    output |> String.split_lines
    |> List.map ~f:(fun line ->
           let parts = String.split line ~on:':' in
           assert (List.length parts = 2) ;
           (List.nth_exn parts 0, List.nth_exn parts 1))
    |> List.filter ~f:(fun (service_name, _) ->
           String.Set.mem all_services service_name)
  in
  let rec poll n =
    let%bind pod_statuses = get_service_statuses () in
    (* TODO: detect "bad statuses" (eg CrashLoopBackoff) and terminate early *)
    let bad_service_statuses =
      List.filter pod_statuses ~f:(fun (_, status) ->
          let parts = String.split status ~on:'/' in
          assert (List.length parts = 2) ;
          let num, denom =
            ( String.strip (List.nth_exn parts 0)
            , String.strip (List.nth_exn parts 1) )
          in
          not (String.equal num denom))
    in
    if List.is_empty bad_service_statuses then return ()
    else if n < max_polls then
      let%bind () =
        after poll_interval |> Deferred.bind ~f:Malleable_error.return
      in
      poll (n + 1)
    else
      let bad_service_statuses_json =
        `List
          (List.map bad_service_statuses ~f:(fun (service_name, status) ->
               `Assoc
                 [ ("service_name", `String service_name)
                 ; ("status", `String status)
                 ]))
      in
      [%log fatal]
        "Not all services could be deployed in time: $bad_service_statuses"
        ~metadata:[ ("bad_service_statuses", bad_service_statuses_json) ] ;
      Malleable_error.hard_error_format
        "Some services either were not deployed properly (errors: %s)"
        (Yojson.Safe.to_string bad_service_statuses_json)
  in
  [%log info] "Waiting for pods to be assigned nodes and become ready" ;
  Deferred.bind (poll 0) ~f:(fun res ->
      if Malleable_error.is_ok res then
        let seed_nodes = seeds network in
        let seed_service_ids =
          seed_nodes
          |> List.map ~f:(fun { Node.service_id; _ } -> service_id)
          |> String.Set.of_list
        in
        let archive_nodes = archive_nodes network in
        let archive_service_ids =
          archive_nodes
          |> List.map ~f:(fun { Node.service_id; _ } -> service_id)
          |> String.Set.of_list
        in
        let non_seed_archive_nodes =
          network |> all_nodes
          |> List.filter ~f:(fun { Node.service_id; _ } ->
                 (not (String.Set.mem seed_service_ids service_id))
                 && not (String.Set.mem archive_service_ids service_id))
        in
        (* TODO: parallelize (requires accumlative hard errors) *)
        let%bind () =
          Malleable_error.List.iter seed_nodes
            ~f:(Node.start ~fresh_state:false)
        in
        (* put a short delay before starting other nodes, to help avoid artifact generation races *)
        let%bind () =
          after (Time.Span.of_sec 30.0)
          |> Deferred.bind ~f:Malleable_error.return
        in
        Malleable_error.List.iter non_seed_archive_nodes
          ~f:(Node.start ~fresh_state:false)
      else Deferred.return res)
