open Core
open Integration_test_lib
open Currency

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
    let timing : Mina_base.Account_timing.t =
      Timed
        { initial_minimum_balance= Balance.of_int 1000
        ; cliff_time= Mina_numbers.Global_slot.of_int 4
        ; cliff_amount= Amount.zero
        ; vesting_period= Mina_numbers.Global_slot.of_int 2
        ; vesting_increment= Amount.of_int 50_000_000_000 }
    in
    { default with
      requires_graphql= true
    ; block_producers=
        [ {balance= "1000"; timing}
        ; {balance= "1000"; timing}
        ; {balance= "1000"; timing} ]
    ; num_snark_workers= 0 }

  let expected_error_event_reprs = []

  let rec to_string_query_results query_results str =
    match query_results with
    | element :: tail ->
        let node_id, peer_list = element in
        to_string_query_results tail
          ( str
          ^ Printf.sprintf "( %s, [%s]) " node_id
              (String.concat ~sep:", " peer_list) )
    | [] ->
        str

  open Graph_algorithms

  module X = struct
    include String

    type display = string [@@deriving yojson]

    let display = Fn.id

    let name = Fn.id
  end

  module G = Visualization.Make_ocamlgraph (X)

  let graph_of_adjacency_list adj =
    List.fold adj ~init:G.empty ~f:(fun acc (x, xs) ->
        let acc = G.add_vertex acc x in
        List.fold xs ~init:acc ~f:(fun acc y ->
            let acc = G.add_vertex acc y in
            G.add_edge acc x y ) )

  let run network t =
    let open Network in
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    [%log info] "mina_peers_test: started" ;
    let peer_list = Network.seeds network @ Network.block_producers network in
    let%bind () =
      Malleable_error.List.iter peer_list ~f:(fun node ->
          wait_for t (Wait_condition.node_to_initialize node) )
    in
    [%log info] "mina_peers_test: done waiting for initialization" ;
    [%log info] "peers_list"
      ~metadata:
        [ ( "peers"
          , `List (List.map peer_list ~f:(fun n -> `String (Node.id n))) ) ] ;
    let get_peer_id_partial = Node.get_peer_id ~logger in
    (* each element in query_results represents the data of a single node relevant to this test. ( peer_id of node * [list of peer_ids of node's peers] ) *)
    let%bind (query_results : (string * string list) list) =
      Malleable_error.List.map peer_list ~f:get_peer_id_partial
    in
    [%log info]
      "mina_peers_test: successfully made graphql query.  query_results: %s"
      (to_string_query_results query_results "") ;
    let () =
      Out_channel.with_file "/tmp/network-graph.dot" ~f:(fun c ->
          G.output_graph c (graph_of_adjacency_list query_results) )
    in
    let expected_peers, _ = List.unzip query_results in
    let test_compare_func (node_peer_id, visible_peers_of_node) =
      let expected_peers_of_node : string list =
        List.filter
          ~f:(fun p -> not (String.equal p node_peer_id))
          expected_peers
        (* expected_peers_of_node is just expected_peers but with the peer_id of the given node removed from the list *)
      in
      [%log info] "node_peer_id: %s" node_peer_id ;
      [%log info] "expected_peers_of_node: %s"
        (String.concat ~sep:" " expected_peers_of_node) ;
      [%log info] "visible_peers_of_node: %s"
        (String.concat ~sep:" " visible_peers_of_node) ;
      List.iter expected_peers_of_node ~f:(fun p ->
          assert (List.exists visible_peers_of_node ~f:(String.equal p)) )
      (* loop through expected_peers_of_node and make sure everything in that list is also in visible_peers_of_node  *)
    in
    [%log info] "mina_peers_test: making assertions" ;
    let result = return (List.iter query_results ~f:test_compare_func) in
    ( (* Check that the network cannot be disconnected by removing 0 or 1 nodes. *)
    match Nat.take (connectivity (module String) query_results) 2 with
    | `Failed_after n ->
        failwithf "The network could be disconnected by removing %d node(s)" n
          ()
    | `Ok ->
        () ) ;
    [%log info]
      "mina_peers_test: assertions passed, peers test successfully ran!!!" ;
    result
end
