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
    let open Test_config.Block_producer in
    { default with
      requires_graphql = true
    ; block_producers =
        [ { balance = "1000"; timing = Untimed }
        ; { balance = "1000"; timing = Untimed }
        ; { balance = "1000"; timing = Untimed }
        ]
    ; num_snark_workers = 0
    }

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
          ~error_type:`Hard ~error)

  let check_peers ~logger nodes =
    let open Malleable_error.Let_syntax in
    let%bind nodes_and_responses =
      Malleable_error.List.map nodes ~f:(fun node ->
          let%map response = Network.Node.must_get_peer_id ~logger node in
          (node, response))
    in
    let nodes_by_peer_id =
      nodes_and_responses
      |> List.map ~f:(fun (node, (peer_id, _)) -> (peer_id, node))
      |> String.Map.of_alist_exn
    in
    Malleable_error.List.iter nodes_and_responses
      ~f:(fun (_, (peer_id, connected_peers)) ->
        check_peer_connectivity ~nodes_by_peer_id ~peer_id ~connected_peers)

  let run network t =
    let open Network in
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    let all_nodes = Network.block_producers network in
    let[@warning "-8"] [ node_a; node_b; node_c ] =
      Network.block_producers network
    in
    let%bind () =
      Malleable_error.List.iter [ node_a; node_b; node_c ]
        ~f:(Fn.compose (wait_for t) Wait_condition.node_to_initialize)
    in
    let%bind () =
      section "short bootstrap"
        (let%bind () = Node.stop node_c in
         let%bind _ = wait_for t (Wait_condition.blocks_to_be_produced 1) in
         (* let%bind () = Node.start ~fresh_state:true node_c in *)
         let%bind () = wait_for t (Wait_condition.node_to_initialize node_c) in
         (* above line SHOULD FAIL, bc i've commented out the re-start *)
         wait_for t
           ( Wait_condition.nodes_to_synchronize [ node_a; node_b; node_c ]
           |> Wait_condition.with_timeouts
                ~hard_timeout:
                  (Network_time_span.Literal
                     (Time.Span.of_ms (15. *. 60. *. 1000.))) ))
    in
    section "network is fully connected after one node is restarted"
      (let%bind () = Malleable_error.lift (after (Time.Span.of_sec 180.0)) in
       check_peers ~logger all_nodes)
end
