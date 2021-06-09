open Core
open Async
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs
  open Engine
  open Dsl

  (* TODO: find a way to avoid this type alias (first class module signatures restrictions make this tricky) *)
  type network = Network.t

  type node = Network.Node.t

  type dsl = Dsl.t

  (* DONE BUT UNTESTED: porting send-payments *)
  (* CURRENTLY: port timed accounts test *)
  (* TODO: refactor all currency values to decimal represenation *)
  (* TODO: test account creation fee *)
  (* TODO: test snark work *)
  let config =
    let open Test_config in
    let open Test_config.Account_config in
    let make_timing ~min_balance ~cliff_time ~cliff_amount ~vesting_period
        ~vesting_increment : Mina_base.Account_timing.t =
      let open Currency in
      Timed
        { initial_minimum_balance= Balance.of_int min_balance
        ; cliff_time= Mina_numbers.Global_slot.of_int cliff_time
        ; cliff_amount= Amount.of_int cliff_amount
        ; vesting_period= Mina_numbers.Global_slot.of_int vesting_period
        ; vesting_increment= Amount.of_int vesting_increment }
    in
    { default with
      requires_graphql= true
    ; block_producers=
        [ {balance= "4000"; timing= Untimed; delegate= None}
        ; {balance= "3000"; timing= Untimed; delegate= None}
        ; { balance= "1000"
          ; timing=
              make_timing ~min_balance:100_000_000_000 ~cliff_time:4
                ~cliff_amount:0 ~vesting_period:2
                ~vesting_increment:50_000_000_000
          ; delegate= None } ]
    ; num_snark_workers= 0 }

  let run network t =
    let open Network in
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    (* fee for user commands *)
    let fee = Currency.Fee.of_int 10_000_000 in
    let block_producer_nodes = Network.block_producers network in
    let%bind () =
      Malleable_error.List.iter block_producer_nodes
        ~f:(Fn.compose (wait_for t) Wait_condition.node_to_initialize)
    in
    let[@warning "-8"] [untimed_node_a; untimed_node_b; timed_node_a] =
      block_producer_nodes
    in
    let%bind () =
      section "send a single payment between 2 untimed accounts"
        (let amount = Currency.Amount.of_int 200_000_000 in
         let fee = Currency.Fee.of_int 10_000_000 in
         let receiver = untimed_node_a in
         let%bind receiver_pub_key = Util.pub_key_of_node receiver in
         let sender = untimed_node_b in
         let%bind sender_pub_key = Util.pub_key_of_node sender in
         let%bind () =
           Network.Node.must_send_payment ~logger sender ~sender_pub_key
             ~receiver_pub_key ~amount ~fee
         in
         wait_for t
           (Wait_condition.payment_to_be_included_in_frontier ~sender_pub_key
              ~receiver_pub_key ~amount))
    in
    let%bind () =
      section "send a single payment from timed account using available liquid"
        (let amount = Currency.Amount.of_int 300_000_000_000 in
         let receiver = untimed_node_a in
         let%bind receiver_pub_key = Util.pub_key_of_node receiver in
         let sender = timed_node_a in
         let%bind sender_pub_key = Util.pub_key_of_node sender in
         let%bind () =
           Network.Node.must_send_payment ~logger sender ~sender_pub_key
             ~receiver_pub_key ~amount ~fee
         in
         wait_for t
           (Wait_condition.payment_to_be_included_in_frontier ~sender_pub_key
              ~receiver_pub_key ~amount))
    in
    section "unable to send payment from timed account using illiquid tokens"
      (let amount = Currency.Amount.of_int 600_000_000_000 in
       let receiver = untimed_node_b in
       let%bind receiver_pub_key = Util.pub_key_of_node receiver in
       let sender = timed_node_a in
       let%bind sender_pub_key = Util.pub_key_of_node sender in
       (* TODO: refactor this using new [expect] dsl when it's available *)
       let open Deferred.Let_syntax in
       match%bind
         Node.send_payment ~logger sender ~sender_pub_key ~receiver_pub_key
           ~amount ~fee
       with
       | Ok () ->
           Malleable_error.soft_error_string ~value:()
             "Payment succeeded, but expected it to fail because of a minimum \
              balance violation"
       | Error error ->
           (* expect GraphQL error due to insufficient funds *)
           let err_str = Error.to_string_mach error in
           let err_str_lowercase = String.lowercase err_str in
           if
             String.is_substring ~substring:"insufficient_funds"
               err_str_lowercase
           then (
             [%log info] "Got expected insufficient funds error from GraphQL" ;
             Malleable_error.return () )
           else (
             [%log error]
               "Payment failed in GraphQL, but for unexpected reason: %s"
               err_str ;
             Malleable_error.soft_error_format ~value:()
               "Payment failed for unexpected reason: %s" err_str ))
end
