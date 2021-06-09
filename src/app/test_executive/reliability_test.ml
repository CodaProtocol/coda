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

  let config =
    let open Test_config in
    let open Test_config.Account_config in
    { default with
      requires_graphql= true
    ; block_producers=
        [ {balance= "1000"; timing= Untimed; delegate= None}
        ; {balance= "1000"; timing= Untimed; delegate= None}
        ; {balance= "1000"; timing= Untimed; delegate= None} ]
    ; num_snark_workers= 0 }

  let check_common_prefixes ~number_of_blocks:n ~logger chains =
    let recent_chains =
      List.map chains ~f:(fun chain ->
          List.take (List.rev chain) n |> Hash_set.of_list (module String) )
    in
    let common_prefixes =
      List.fold ~f:Hash_set.inter
        ~init:(List.hd_exn recent_chains)
        (List.tl_exn recent_chains)
    in
    let length = Hash_set.length common_prefixes in
    if length = 0 then (
      let result =
        Malleable_error.soft_error ~value:()
          (Error.of_string
             (sprintf
                "Chains don't have any common prefixes among their most \
                 recent %d blocks"
                n))
      in
      [%log error]
        "common_prefix test: TEST FAILURE, Chains don't have any common \
         prefixes among their most recent %d blocks"
        n ;
      result )
    else if length < n then (
      let result =
        Malleable_error.soft_error ~value:()
          (Error.of_string
             (sprintf
                !"Chains only have %d common prefixes, expected %d common \
                  prefixes"
                length n))
      in
      [%log error]
        "common_prefix test: TEST FAILURE, Chains only have %d common \
         prefixes, expected %d common prefixes"
        length n ;
      result )
    else Malleable_error.return ()

  let check_peer_connectivity ~nodes_by_peer_id ~peer_id ~connected_peers =
    let get_node_id p =
      p |> String.Map.find_exn nodes_by_peer_id |> Network.Node.id
    in
    let expected_peers =
      nodes_by_peer_id |> String.Map.keys
      |> List.filter ~f:(fun p -> not (String.equal p peer_id))
    in
    Malleable_error.List.iter expected_peers ~f:(fun p ->
        let error =
          Printf.sprintf "node %s (id=%s) is not connected to node %s (id=%s)"
            (get_node_id peer_id) peer_id p (get_node_id p)
          |> Error.of_string
        in
        Malleable_error.ok_if_true
          (List.mem connected_peers p ~equal:String.equal)
          ~error_type:`Hard ~error )

  let check_peers ~logger nodes =
    let open Malleable_error.Let_syntax in
    let%bind nodes_and_responses =
      Malleable_error.List.map nodes ~f:(fun node ->
          let%map response = Network.Node.must_get_peer_id ~logger node in
          (node, response) )
    in
    let nodes_by_peer_id =
      nodes_and_responses
      |> List.map ~f:(fun (node, (peer_id, _)) -> (peer_id, node))
      |> String.Map.of_alist_exn
    in
    Malleable_error.List.iter nodes_and_responses
      ~f:(fun (_, (peer_id, connected_peers)) ->
        check_peer_connectivity ~nodes_by_peer_id ~peer_id ~connected_peers )

  let run network t =
    let open Network in
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    (* TEMP: until we fix the seed graphql port, we will only check peers for block producers *)
    (* let all_nodes = Network.all_nodes network in *)
    let all_nodes = Network.block_producers network in
    let[@warning "-8"] [node_a; node_b; node_c] =
      Network.block_producers network
    in
    (* TODO: let%bind () = wait_for t (Wait_condition.nodes_to_initialize [node_a; node_b; node_c]) in *)
    let%bind () =
      Malleable_error.List.iter [node_a; node_b; node_c]
        ~f:(Fn.compose (wait_for t) Wait_condition.node_to_initialize)
    in
    let%bind () =
      section "network is fully connected upon initialization"
        (check_peers ~logger all_nodes)
    in
    let%bind _ =
      section "blocks are produced"
        (wait_for t (Wait_condition.blocks_to_be_produced 2))
    in
    let%bind () =
      section "short bootstrap"
        (let%bind () = Node.stop node_c in
         let%bind _ = wait_for t (Wait_condition.blocks_to_be_produced 1) in
         let%bind () = Node.start ~fresh_state:true node_c in
         let%bind () = wait_for t (Wait_condition.node_to_initialize node_c) in
         wait_for t
           ( Wait_condition.nodes_to_synchronize [node_a; node_b; node_c]
           |> Wait_condition.with_timeouts
                ~hard_timeout:
                  (Network_time_span.Literal
                     (Time.Span.of_ms (15. *. 60. *. 1000.))) ))
    in
    let%bind () =
      section "network is fully connected after one node is restarted"
        (let%bind () = Malleable_error.lift (after (Time.Span.of_sec 180.0)) in
         check_peers ~logger all_nodes)
    in
    section "nodes share common prefix no greater than 2 block back"
      (let%bind _ = wait_for t (Wait_condition.blocks_to_be_produced 1) in
       (* the common prefix test relies on 4 blocks having been produced.  previous sections altogether have already produced 3.  if previous sections change, then this may need to be re-adjusted*)
       let%bind chains =
         Malleable_error.List.map all_nodes
           ~f:(Network.Node.must_get_best_chain ~logger)
       in
       check_common_prefixes chains ~number_of_blocks:2 ~logger)
end
