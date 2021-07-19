open Core
open Signature_lib

let gen_keys count =
  Quickcheck.random_value ~seed:`Nondeterministic
    (Quickcheck.Generator.list_with_length count Public_key.Compressed.gen)

let output_keys =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate the given number of public keys on stdout"
    (let%map_open count =
       flag "--count" ~aliases:["count"] ~doc:"NUM Number of keys to generate"
         (required int)
     in
     fun () ->
       List.iter (gen_keys count) ~f:(fun pk ->
           Format.printf "%s@." (Public_key.Compressed.to_base58_check pk) ))

let output_cmds =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate the given number of public keys on stdout"
    (let%map_open count =
       flag "--count" ~aliases:["count"] ~doc:"NUM Number of keys to generate"
         (required int)
     and txns_per_block =
       flag "--txn-capacity-per-block" ~aliases:["txn-capacity-per-block"]
         ~doc:
           "NUM Transaction capacity per block. Used for rate limiting. \
            (default: 128)"
         (optional_with_default 128 int)
     and slot_time =
       flag "--slot-time" ~aliases:["slot-time"]
         ~doc:
           "NUM_MILLISECONDS Slot duration in milliseconds. (default: 180000)"
         (optional_with_default 180000 int)
     and fill_rate =
       flag "--fill-rate" ~aliases:["fill-rate"]
         ~doc:"FILL_RATE Fill rate (default: 0.75)"
         (optional_with_default 0.75 float)
     and rate_limit =
       flag "--apply-rate-limit" ~aliases:["apply-rate-limit"]
         ~doc:
           "TRUE/FALSE Whether to emit sleep commands between commands to \
            enforce sleeps (default: true)"
         (optional_with_default true bool)
     and rate_limit_level =
       flag "--rate-limit-level" ~aliases:["rate-limit-level"]
         ~doc:
           "NUM Number of transactions that can be sent in a time interval \
            before hitting the rate limit (default: 200)"
         (optional_with_default 200 int)
     and rate_limit_interval =
       flag "--rate-limit-interval" ~aliases:["rate-limit-interval"]
         ~doc:
           "NUM_MILLISECONDS Interval that the rate-limiter is applied over \
            (default: 300000)"
         (optional_with_default 300000 int)
     and sender_key =
       flag "--sender-pk" ~aliases:["sender-pk"]
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
               else incr batch_count ) ;
           Format.printf
             "mina client send-payment --amount 1 --receiver %s --sender %s@."
             (Public_key.Compressed.to_base58_check pk)
             sender_key ))

let () =
  Command.run
    (Command.group
       ~summary:"Generate public keys for sending batches of transactions"
       [("gen-keys", output_keys); ("gen-txns", output_cmds)])
