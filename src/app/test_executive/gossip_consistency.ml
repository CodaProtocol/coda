open Core
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs
  open Engine
  open Dsl

  (* TODO: find a way to avoid this type alias (first class module signatures restrictions make this tricky) *)
  type network = Network.t

  type node = Network.Node.t

  type dsl = Dsl.t

  let block_producer_balance = "1000" (* 1_000_000_000_000 *)

  let config =
    let n = 3 in
    let open Test_config in
    { default with
      requires_graphql= true
    ; block_producers=
        List.init n ~f:(fun _ ->
            {Block_producer.balance= block_producer_balance; timing= Untimed}
        ) }

  let wait_for_all_to_initialize ~logger network t =
    let open Malleable_error.Let_syntax in
    let producers = Network.block_producers network in
    let n = List.length producers in
    List.mapi producers ~f:(fun i node ->
        let%map () = wait_for t (Wait_condition.node_to_initialize node) in
        [%log info]
          "gossip_consistency test: Block producer %d (of %d) initialized"
          (i + 1) n ;
        () )
    |> Malleable_error.all_unit

  let send_payments ~logger ~sender_pub_key ~receiver_pub_key ~amount ~fee
      ~node n =
    let open Malleable_error.Let_syntax in
    let rec go n =
      if n = 0 then return ()
      else
        let%bind () =
          let%map () =
            Network.Node.must_send_payment ~logger ~sender_pub_key
              ~receiver_pub_key ~amount ~fee node
          in
          [%log info] "gossip_consistency test: payment #%d sent." n ;
          ()
        in
        go (n - 1)
    in
    go n

  let wait_for_payments ~logger ~dsl ~sender_pub_key ~receiver_pub_key ~amount
      n =
    let open Malleable_error.Let_syntax in
    let rec go n =
      if n = 0 then return ()
      else
        (* confirm payment *)
        let%bind () =
          let%map () =
            wait_for dsl
              (Wait_condition.payment_to_be_included_in_frontier
                 ~sender_pub_key ~receiver_pub_key ~amount)
          in
          [%log info]
            "gossip_consistency test: payment #%d successfully included in \
             frontier."
            n ;
          ()
        in
        go (n - 1)
    in
    go n

  let run network t =
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    [%log info] "gossip_consistency test: starting..." ;
    let%bind () = wait_for_all_to_initialize ~logger network t in
    [%log info] "gossip_consistency test: done waiting for initializations" ;
    let receiver_bp = Caml.List.nth (Network.block_producers network) 0 in
    let%bind receiver_pub_key = Util.pub_key_of_node receiver_bp in
    let sender_bp =
      Core_kernel.List.nth_exn (Network.block_producers network) 1
    in
    let%bind sender_pub_key = Util.pub_key_of_node sender_bp in
    let num_payments = 3 in
    let fee = Currency.Fee.of_int 10_000_000 in
    let amount = Currency.Amount.of_int 10_000_000 in
    [%log info] "gossip_consistency test: will now send %d payments"
      num_payments ;
    let%bind () =
      send_payments ~logger ~sender_pub_key ~receiver_pub_key ~node:sender_bp
        ~fee ~amount num_payments
    in
    [%log info]
      "gossip_consistency test: sending payments done. will now wait for \
       payments" ;
    let%bind () =
      wait_for_payments ~logger ~dsl:t ~sender_pub_key ~receiver_pub_key
        ~amount num_payments
    in
    [%log info] "gossip_consistency test: finished waiting for payments" ;
    let gossip_states = (network_state t).gossip_received in
    let num_transactions_seen =
      let open Gossip_state in
      let ss =
        Map.data gossip_states
        |> List.map ~f:(Fn.compose By_direction.received transactions)
      in
      Set.(size (union ss))
    in
    [%log info] "gossip_consistency test: num_transactions_seen = %d"
      num_transactions_seen ;
    let%bind () =
      if num_transactions_seen < num_payments - 1 then (
        let result =
          Malleable_error.soft_error_string ~value:()
            (Printf.sprintf
               "transactions seen = %d, which is less than (numpayments = %d) \
                - 1"
               num_transactions_seen num_payments)
        in
        [%log error]
          "gossip_consistency test: TEST FAILURE.  transactions seen = %d, \
           which is less than (numpayments = %d) - 1"
          num_transactions_seen num_payments ;
        result )
      else
        let result = Malleable_error.ok_unit in
        [%log info] "gossip_consistency test: num_transactions_seen OK" ;
        result
    in
    let `Seen_by_all inter, `Seen_by_some union =
      Gossip_state.stats Transactions_gossip
        (Map.data (network_state t).gossip_received)
        ~exclusion_list:[Network.Node.id sender_bp]
    in
    [%log info] "gossip_consistency test: inter = %d; union = %d " inter union ;
    let ratio =
      if union = 0 then 1. else Float.of_int inter /. Float.of_int union
      (* Gossip_state.consistency_ratio Transactions_gossip
        (Map.data (network_state t).gossip_received) *)
    in
    [%log info] "gossip_consistency test: consistency ratio = %f" ratio ;
    let threshold = 0.95 in
    let%map () =
      if Float.(ratio < threshold) then (
        let result =
          Malleable_error.soft_error_string ~value:()
            (Printf.sprintf
               "consistency ratio = %f, which is less than threshold = %f"
               ratio threshold)
        in
        [%log error]
          "gossip_consistency test: TEST FAILURE. consistency ratio = %f, \
           which is less than threshold = %f"
          ratio threshold ;
        result )
      else
        let result = Malleable_error.ok_unit in
        [%log info] "gossip_consistency test: consistency ratio OK" ;
        result
    in
    [%log info] "gossip_consistency test: test finished!!"
end
