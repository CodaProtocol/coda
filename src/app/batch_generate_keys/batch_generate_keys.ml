open Core
open Signature_lib

let gen_secret_keys count =
  Quickcheck.random_value ~seed:`Nondeterministic
    (Quickcheck.Generator.list_with_length count Private_key.gen)

let gen_keys count =
  Quickcheck.random_value ~seed:`Nondeterministic
    (Quickcheck.Generator.list_with_length count Public_key.Compressed.gen)

let output_keys =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate the given number of public keys on stdout"
    (let%map_open count =
       flag "--count" ~aliases:[ "count" ] ~doc:"NUM Number of keys to generate"
         (required int)
     in
     fun () ->
       List.iter (gen_keys count) ~f:(fun pk ->
           Format.printf "%s@." (Public_key.Compressed.to_base58_check pk)))

let output_cmds =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate the given number of public keys on stdout"
    (let%map_open count =
       flag "--count" ~aliases:[ "count" ] ~doc:"NUM Number of keys to generate"
         (required int)
     and txns_per_block =
       flag "--txn-capacity-per-block"
         ~aliases:[ "txn-capacity-per-block" ]
         ~doc:
           "NUM Transaction capacity per block. Used for rate limiting. \
            (default: 128)"
         (optional_with_default 128 int)
     and slot_time =
       flag "--slot-time" ~aliases:[ "slot-time" ]
         ~doc:
           "NUM_MILLISECONDS Slot duration in milliseconds. (default: 180000)"
         (optional_with_default 180000 int)
     and fill_rate =
       flag "--fill-rate" ~aliases:[ "fill-rate" ]
         ~doc:"FILL_RATE Fill rate (default: 0.75)"
         (optional_with_default 0.75 float)
     and rate_limit =
       flag "--apply-rate-limit" ~aliases:[ "apply-rate-limit" ]
         ~doc:
           "TRUE/FALSE Whether to emit sleep commands between commands to \
            enforce sleeps (default: true)"
         (optional_with_default true bool)
     and rate_limit_level =
       flag "--rate-limit-level" ~aliases:[ "rate-limit-level" ]
         ~doc:
           "NUM Number of transactions that can be sent in a time interval \
            before hitting the rate limit (default: 200)"
         (optional_with_default 200 int)
     and rate_limit_interval =
       flag "--rate-limit-interval" ~aliases:[ "rate-limit-interval" ]
         ~doc:
           "NUM_MILLISECONDS Interval that the rate-limiter is applied over \
            (default: 300000)"
         (optional_with_default 300000 int)
     and sender_key =
       flag "--sender-pk" ~aliases:[ "sender-pk" ]
         ~doc:"PUBLIC_KEY Public key to send the transactions from"
         (required string)
     in
     fun () ->
       let rate_limit =
         if rate_limit then
           let slot_limit =
             Float.(
               of_int txns_per_block /. of_int slot_time *. fill_rate
               *. of_int rate_limit_interval)
           in
           let limit = min (Float.to_int slot_limit) rate_limit_level in
           Some limit
         else None
       in
       let batch_count = ref 0 in
       List.iter (gen_keys count) ~f:(fun pk ->
           Option.iter rate_limit ~f:(fun rate_limit ->
               if !batch_count >= rate_limit then (
                 Format.printf "sleep %f@."
                   Float.(of_int rate_limit_interval /. 1000.) ;
                 batch_count := 0 )
               else incr batch_count) ;
           Format.printf
             "mina client send-payment --amount 1 --receiver %s --sender %s@."
             (Public_key.Compressed.to_base58_check pk)
             sender_key))

(* Shamelessly copied from src/app/cli/src/init/graphql_queries.ml and tweaked*)

module Send_payment =
[%graphql
{|
mutation ($sender: PublicKey!,
          $receiver: PublicKey!,
          $amount: UInt64!,
          $token: UInt64,
          $fee: UInt64!,
          $nonce: UInt32,
          $memo: String,
          $field: String,
          $scalar: String) {
  sendPayment(input:
    {from: $sender, to: $receiver, amount: $amount, token: $token, fee: $fee, nonce: $nonce, memo: $memo},
    signature: {field: $field, scalar: $scalar}) {
    payment {
      id
    }
  }
}
|}]

let make_graphql_signed_transaction ~sender_priv_key ~receiver ~amount ~fee =
  let sender_pub_key =
    Public_key.of_private_key_exn sender_priv_key |> Public_key.compress
  in
  let receiver_pub_key = Public_key.Compressed.of_base58_check_exn receiver in
  let field, scalar =
    Mina_base.Signed_command.sign_payload sender_priv_key
      { common =
          { fee
          ; fee_token = Mina_base.Token_id.default
          ; fee_payer_pk = sender_pub_key
          ; nonce = Mina_numbers.Account_nonce.zero
          ; valid_until = Mina_numbers.Global_slot.max_value
          ; memo = Mina_base.Signed_command_memo.empty
          }
      ; body =
          Payment
            { source_pk = sender_pub_key
            ; receiver_pk = receiver_pub_key
            ; token_id = Mina_base.Token_id.default
            ; amount
            }
      }
  in
  let graphql_query =
    Send_payment.make
      ~receiver:(Graphql_lib.Encoders.public_key receiver_pub_key)
      ~sender:(Graphql_lib.Encoders.public_key sender_pub_key)
      ~amount:(Graphql_lib.Encoders.amount amount)
      ~fee:(Graphql_lib.Encoders.fee fee)
      ~field:(Snark_params.Tick.Field.to_string field)
      ~scalar:(Snark_params.Tick.Inner_curve.Scalar.to_string scalar)
      ()
  in
  let graphql_query_json =
    `Assoc
      [ ("query", `String graphql_query#query)
      ; ("variables", graphql_query#variables)
      ]
  in
  Format.sprintf
    "curl 'http://127.0.0.1:3085/graphql' -X POST -H 'content-type: \
     application/json' --data '%s'@."
    (Yojson.Basic.to_string graphql_query_json)

let output_there_and_back_cmds =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Generate commands to send funds from a single account to many accounts, \
       then transfer them back again. The 'back again' commands are expressed \
       as GraphQL commands, so that we can pass a signature, rather than \
       having to load the secret key for each account"
    (let%map_open count =
       flag "--count" ~aliases:[ "count" ] ~doc:"NUM Number of keys to generate"
         (required int)
     and txns_per_block =
       flag "--txn-capacity-per-block"
         ~aliases:[ "txn-capacity-per-block" ]
         ~doc:
           "NUM Transaction capacity per block. Used for rate limiting. \
            (default: 128)"
         (optional_with_default 128 int)
     and slot_time =
       flag "--slot-time" ~aliases:[ "slot-time" ]
         ~doc:
           "NUM_MILLISECONDS Slot duration in milliseconds. (default: 180000)"
         (optional_with_default 180000 int)
     and fill_rate =
       flag "--fill-rate" ~aliases:[ "fill-rate" ]
         ~doc:"FILL_RATE Fill rate (default: 0.75)"
         (optional_with_default 0.75 float)
     and rate_limit =
       flag "--apply-rate-limit" ~aliases:[ "apply-rate-limit" ]
         ~doc:
           "TRUE/FALSE Whether to emit sleep commands between commands to \
            enforce sleeps (default: true)"
         (optional_with_default true bool)
     and rate_limit_level =
       flag "--rate-limit-level" ~aliases:[ "rate-limit-level" ]
         ~doc:
           "NUM Number of transactions that can be sent in a time interval \
            before hitting the rate limit (default: 200)"
         (optional_with_default 200 int)
     and rate_limit_interval =
       flag "--rate-limit-interval" ~aliases:[ "rate-limit-interval" ]
         ~doc:
           "NUM_MILLISECONDS Interval that the rate-limiter is applied over \
            (default: 300000)"
         (optional_with_default 300000 int)
     and origin_sender_public_key =
       flag "--origin-sender-pk" ~aliases:[ "origin-sender-pk" ]
         ~doc:"PUBLIC_KEY Public key to send the transactions from"
         (optional string)
     and origin_sender_secret_key =
       (* TODO: this probably needs to be read in from a file instead of just stuck in plaintext in an argument*)
       flag "--origin-sender-sk" ~aliases:[ "origin-sender-sk" ]
         ~doc:"PRIVATE_KEY Private key to send the transactions from"
         (optional string)
     in
     fun () ->
       let rate_limit =
         if rate_limit then
           let slot_limit =
             Float.(
               of_int txns_per_block /. of_int slot_time *. fill_rate
               *. of_int rate_limit_interval)
           in
           let limit = min (Float.to_int slot_limit) rate_limit_level in
           Some limit
         else None
       in
       let batch_count = ref 0 in
       let base_send_amount = Currency.Amount.of_formatted_string "2" in
       let initial_send_amount =
         Option.value_exn
           (Currency.Amount.add base_send_amount
              (Currency.Amount.of_fee Mina_base.Signed_command.minimum_fee))
       in
       let fee_amount = Mina_base.Signed_command.minimum_fee in
       let generated_secrets = gen_secret_keys count in

       (* there... *)
       if Option.is_some origin_sender_public_key then
         List.iter generated_secrets ~f:(fun sk ->
             Option.iter rate_limit ~f:(fun rate_limit ->
                 if !batch_count >= rate_limit then (
                   Format.printf "sleep %f@."
                     Float.(of_int rate_limit_interval /. 1000.) ;
                   batch_count := 0 )
                 else incr batch_count) ;
             let acct_pk = Public_key.of_private_key_exn sk in
             let transaction_command =
               Format.sprintf
                 "mina client send-payment --amount %s --fee %s --receiver %s \
                  --sender %s@."
                 (Currency.Amount.to_formatted_string initial_send_amount)
                 ( fee_amount |> Currency.Amount.of_fee
                 |> Currency.Amount.to_formatted_string )
                 Public_key.(Compressed.to_base58_check (compress acct_pk))
                 (Option.value_exn origin_sender_public_key)
             in
             Format.print_string transaction_command)
       else if Option.is_some origin_sender_secret_key then
         List.iter generated_secrets ~f:(fun sk ->
             Option.iter rate_limit ~f:(fun rate_limit ->
                 if !batch_count >= rate_limit then (
                   Format.printf "sleep %f@."
                     Float.(of_int rate_limit_interval /. 1000.) ;
                   batch_count := 0 )
                 else incr batch_count) ;
             let acct_pk = Public_key.of_private_key_exn sk in
             let origin_sender_sk =
               Private_key.of_base58_check_exn
                 (Option.value_exn origin_sender_secret_key)
             in
             let transaction_command =
               make_graphql_signed_transaction ~sender_priv_key:origin_sender_sk
                 ~receiver:
                   Public_key.(Compressed.to_base58_check (compress acct_pk))
                 ~amount:initial_send_amount ~fee:fee_amount
             in
             Format.print_string transaction_command)
       else exit 1 ;

       (* and back again... *)
       let origin_pk : string =
         if Option.is_some origin_sender_public_key then
           Option.value_exn origin_sender_public_key
         else if Option.is_some origin_sender_secret_key then
           let origin_sender_sk =
             Private_key.of_base58_check_exn
               (Option.value_exn origin_sender_secret_key)
           in
           origin_sender_sk |> Public_key.of_private_key_exn
           |> Public_key.compress |> Public_key.Compressed.to_base58_check
         else exit 1
       in
       List.iter generated_secrets ~f:(fun sk ->
           Option.iter rate_limit ~f:(fun rate_limit ->
               if !batch_count >= rate_limit then (
                 Format.printf "sleep %f@."
                   Float.(of_int rate_limit_interval /. 1000.) ;
                 batch_count := 0 )
               else incr batch_count) ;
           let transaction_command =
             make_graphql_signed_transaction ~sender_priv_key:sk
               ~receiver:origin_pk ~amount:base_send_amount ~fee:fee_amount
           in
           Format.print_string transaction_command))

let () =
  Command.run
    (Command.group
       ~summary:"Generate public keys for sending batches of transactions"
       [ ("gen-keys", output_keys)
       ; ("gen-txns", output_cmds)
       ; ("gen-there-and-back-txns", output_there_and_back_cmds)
       ])
