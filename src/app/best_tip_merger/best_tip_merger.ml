(* Accumulates the best tip history from mina-best-tip.log files and consolidates it into a rose tree representation*)

open Core
open Async
open Mina_base

module Node = struct
  module T = struct
    type t =
      { state: Transition_frontier.Extensions.Best_tip_diff.Log_event.t
      ; peer_ids: String.Set.t }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module Input = struct
  (*all_states: hash table of a parent block and a map of its successors
   * init_states: blocks for which there are no previous blocks in the log
   * peers: set of peers whose logs were processed
   * seen_state_hashes: map of states that were obtained from the logs. Used to keep the roots updated*)
  type t =
    { all_states: (State_hash.t, Node.t State_hash.Map.t) Hashtbl.t
    ; init_states: (State_hash.t, Node.t) Hashtbl.t
          (*generate from seen state hashes later on*)
    ; peers: String.Set.t
    ; seen_state_hashes: State_hash.Set.t }

  type added_transitions =
    Transition_frontier.Extensions.Best_tip_diff.Log_event.t list
  [@@deriving yojson]

  let of_logs ~logger ~log_file t =
    [%log info] "Processing log file: %s" log_file ;
    let log_lines = In_channel.read_lines log_file in
    let res =
      List.fold ~init:t log_lines ~f:(fun acc line ->
          match Logger.Message.of_yojson (Yojson.Safe.from_string line) with
          | Ok msg -> (
              let tf_event_id =
                Option.map msg.event_id ~f:(fun e ->
                    Structured_log_events.equal_id e
                      Transition_frontier.Extensions.Best_tip_diff.Log_event
                      .new_best_tip_event_structured_events_id )
              in
              match tf_event_id with
              | Some true ->
                  (*This is a Best_tip_diff log*)
                  let peer_id =
                    match Map.find_exn msg.metadata "peer_id" with
                    | `String p ->
                        p
                    | _ ->
                        failwith "Expected `String for peer_id"
                  in
                  let peers = Set.add acc.peers peer_id in
                  let added_transitions =
                    Map.find_exn msg.metadata "added_transitions"
                    |> added_transitions_of_yojson |> Result.ok_or_failwith
                  in
                  let acc' =
                    List.fold ~init:acc added_transitions ~f:(fun acc'' tr ->
                        let new_node =
                          { Node.state= tr
                          ; peer_ids= String.Set.singleton peer_id }
                        in
                        let parent_hash =
                          Mina_state.Protocol_state.previous_state_hash
                            tr.protocol_state
                        in
                        let new_state_hash = tr.state_hash in
                        let seen_state_hashes =
                          let seen_state_hashes =
                            Set.add acc''.seen_state_hashes new_state_hash
                          in
                          if Set.mem acc''.seen_state_hashes parent_hash |> not
                          then
                            (*Assuming the logs are in order, if the parent hash was not already seen then it is the root*)
                            Hashtbl.update t.init_states new_state_hash
                              ~f:(function
                              | None ->
                                  new_node
                              | Some node ->
                                  { state= node.state
                                  ; peer_ids= Set.add node.peer_ids peer_id } ) ;
                          seen_state_hashes
                        in
                        Hashtbl.update t.all_states parent_hash ~f:(function
                          | None ->
                              State_hash.Map.singleton new_state_hash new_node
                          | Some map -> (
                            match Map.find map new_state_hash with
                            | None ->
                                Map.add_exn map ~key:new_state_hash
                                  ~data:new_node
                            | Some {state; peer_ids} ->
                                Map.set map ~key:new_state_hash
                                  ~data:
                                    {state; peer_ids= Set.add peer_ids peer_id}
                            ) ) ;
                        {acc'' with seen_state_hashes} )
                  in
                  (* remove any previous roots for which there are ancestors now*)
                  List.iter (Hashtbl.keys acc'.init_states) ~f:(fun root ->
                      let state = Hashtbl.find_exn acc'.init_states root in
                      let parent =
                        state.state.protocol_state.previous_state_hash
                      in
                      if State_hash.Set.mem acc'.seen_state_hashes parent then
                        (* no longer a root because a node for its parent was seen*)
                        Hashtbl.remove acc'.init_states root ) ;
                  {acc' with peers}
              | None | Some false ->
                  [%log error]
                    "Skipping log line $line because it is not a \
                     best-tip-change log"
                    ~metadata:[("line", `String line)] ;
                  (*skipping any other logs*) acc )
          | Error err ->
              [%log error] "Could not process log line $line: $error"
                ~metadata:[("line", `String line); ("error", `String err)] ;
              acc )
    in
    [%log info] "Finished processing log file: %s" log_file ;
    res
end

(*Output is a rose tree and consists of all the forks seen from an initial state; Multiple rose trees is there are logs with different initial states*)
module Output = struct
  type node =
    | Root of {state: State_hash.t; peer_ids: String.Set.t}
    | Node of Node.t

  type t = node Rose_tree.t list

  let of_input (input : Input.t) ~min_peers : t =
    let roots =
      List.fold (Hashtbl.data input.init_states) ~init:State_hash.Map.empty
        ~f:(fun map root_state ->
          Map.update map
            (Mina_state.Protocol_state.previous_state_hash
               root_state.state.protocol_state) ~f:(function
            | Some peer_ids ->
                Set.union peer_ids root_state.peer_ids
            | None ->
                root_state.peer_ids ) )
    in
    List.fold ~init:[] (Map.to_alist roots)
      ~f:(fun acc_trees (root, peer_ids) ->
        let rec go parent_hash =
          let successors =
            Option.value ~default:State_hash.Map.empty
              (Hashtbl.find input.all_states parent_hash)
            |> Map.data
          in
          let successors_with_min_peers =
            if min_peers > 1 then
              List.filter successors ~f:(fun s ->
                  Set.length s.peer_ids >= min_peers )
            else successors
          in
          List.map successors_with_min_peers ~f:(fun s ->
              Rose_tree.T
                ( Node {state= s.state; peer_ids= s.peer_ids}
                , go s.state.state_hash ) )
        in
        let root_node = Rose_tree.T (Root {state= root; peer_ids}, go root) in
        root_node :: acc_trees )
end

module type Graph_node_intf = sig
  type t

  type display [@@deriving yojson]

  val display : t -> display

  val equal : t -> t -> bool

  val hash : t -> int

  val compare : t -> t -> int

  val name : t -> string
end

module Display = struct
  type state =
    | Root of State_hash.t
    | Node of Transition_frontier.Extensions.Best_tip_diff.Log_event.t
  [@@deriving yojson]

  type node = {state: state; peers: int} [@@deriving yojson]

  type t = node Rose_tree.t list [@@deriving yojson]

  let of_output : Output.t -> t =
   fun t ->
    List.map t ~f:(fun tree ->
        Rose_tree.map tree ~f:(fun (t : Output.node) ->
            match t with
            | Root s ->
                {state= Root s.state; peers= Set.length s.peer_ids}
            | Node s ->
                {state= Node s.state; peers= Set.length s.peer_ids} ) )
end

module Compact_display = struct
  type state =
    | Root of State_hash.t
    | Node of
        { current: State_hash.t
        ; parent: State_hash.t
        ; blockchain_length: Mina_numbers.Length.t
        ; global_slot: Mina_numbers.Global_slot.t }
  [@@deriving yojson]

  type node = {state: state; peers: int} [@@deriving yojson]

  type t = node Rose_tree.t list [@@deriving yojson]

  let of_output t =
    List.map t ~f:(fun tree ->
        Rose_tree.map tree ~f:(fun (t : Output.node) ->
            match t with
            | Root s ->
                {state= Root s.state; peers= Set.length s.peer_ids}
            | Node t ->
                let state : state =
                  Node
                    { current= t.state.state_hash
                    ; parent= t.state.protocol_state.previous_state_hash
                    ; blockchain_length=
                        Mina_state.Protocol_state.consensus_state
                          t.state.protocol_state
                        |> Consensus.Data.Consensus_state.blockchain_length
                    ; global_slot=
                        Mina_state.Protocol_state.consensus_state
                          t.state.protocol_state
                        |> Consensus.Data.Consensus_state.curr_global_slot }
                in
                {state; peers= Set.length t.peer_ids} ) )
end

module Graph_node = struct
  type state =
    | Root of State_hash.t
    | Node of
        { current: State_hash.t
        ; length: Mina_numbers.Length.t
        ; slot: Mina_numbers.Global_slot.t }
  [@@deriving yojson, equal, hash]

  type t = {state: state; peers: int} [@@deriving yojson, equal, hash]

  type display = {state: string; length: string; slot: string; peers: int}
  [@@deriving yojson]

  let name (t : t) =
    match t.state with
    | Root s ->
        State_hash.to_string s |> Fn.flip String.suffix 7
    | Node s ->
        State_hash.to_string s.current |> Fn.flip String.suffix 7

  let display (t : t) =
    let state = name t in
    let length, slot =
      match t.state with
      | Root _ ->
          ("NA", "NA")
      | Node s ->
          ( Mina_numbers.Length.to_string s.length
          , Mina_numbers.Global_slot.to_string s.slot )
    in
    {state; slot; length; peers= t.peers}

  let compare (t : t) (t' : t) =
    let state_hash = function Root s -> s | Node s -> s.current in
    State_hash.compare (state_hash t.state) (state_hash t'.state)
end

module Visualization = struct
  include Visualization.Make_ocamlgraph (Graph_node)

  let to_graph (t : Compact_display.node Rose_tree.t) =
    let to_graph_node (node : Compact_display.node) =
      let state =
        match node.state with
        | Root s ->
            Graph_node.Root s
        | Node s ->
            Node
              { current= s.current
              ; length= s.blockchain_length
              ; slot= s.global_slot }
      in
      {Graph_node.state; peers= node.peers}
    in
    let rec go (Rose_tree.T (node, subtrees)) graph =
      let node = to_graph_node node in
      let graph_with_node = add_vertex graph node in
      List.fold ~init:graph_with_node subtrees
        ~f:(fun gr (T (child_node, _) as child_tree) ->
          let gr' = add_edge gr node (to_graph_node child_node) in
          go child_tree gr' )
    in
    go t empty

  let visualize (t : Compact_display.t) ~output_dir =
    List.iteri t ~f:(fun i tree ->
        let filename = output_dir ^/ "tree_" ^ Int.to_string i ^ ".dot" in
        Out_channel.with_file filename ~f:(fun output_channel ->
            let graph = to_graph tree in
            output_graph output_channel graph ) )
end

let main ~input_dir ~output_dir ~output_format ~min_peers () =
  let%map files =
    Sys.ls_dir input_dir
    >>| List.filter_map ~f:(fun n ->
            if Filename.check_suffix n ".log" then Some (input_dir ^/ n)
            else None )
  in
  let t : Input.t =
    { Input.all_states= Hashtbl.create (module State_hash)
    ; peers= String.Set.empty
    ; init_states= Hashtbl.create (module State_hash)
    ; seen_state_hashes= State_hash.Set.empty }
  in
  let logrotate_max_size = 1024 * 1024 * 1 in
  let logrotate_num_rotate = 1 in
  Logger.Consumer_registry.register ~id:"default"
    ~processor:(Logger.Processor.raw ())
    ~transport:
      (Logger.Transport.File_system.dumb_logrotate ~directory:output_dir
         ~log_filename:"mina-best-tip-merger.log" ~max_size:logrotate_max_size
         ~num_rotate) ;
  let logger = Logger.create () in
  let t' =
    List.fold ~init:t files ~f:(fun t log_file ->
        Input.of_logs ~logger ~log_file t )
  in
  [%log info] "Consolidating best-tip history.." ;
  let output = Output.of_input t' ~min_peers in
  [%log info] "Generated the resulting rose tree" ;
  let output_json, fmt_str =
    match output_format with
    | `Full ->
        (Display.(to_yojson @@ of_output output), "Full")
    | `Compact ->
        (Compact_display.(to_yojson @@ of_output output), "Compact")
  in
  let result_file = output_dir ^/ "Result.txt" in
  [%log info] "Writing the result (format: %s) to %s" fmt_str result_file ;
  Yojson.Safe.to_file result_file output_json ;
  (*Visualization*)
  [%log info] "Writing visualization files" ;
  Visualization.visualize (Compact_display.of_output output) ~output_dir ;
  ()

let () =
  Command.(
    run
      (let open Let_syntax in
      Command.async
        ~summary:
          "Consolidates best tip history from multiple log files into a rose \
           tree representation"
        (let%map input_dir =
           Param.flag "--input-dir" ~aliases:["-input-dir"]
             ~doc:
               "PATH Directory containing one or more mina-best-tip.log files"
             Param.(required string)
         and output_dir =
           Param.flag "--output-dir" ~aliases:["-output-dir"]
             ~doc:"PATH Directory to save the output"
             Param.(required string)
         and output_format =
           Param.flag "--output-format" ~aliases:["-output-format"]
             ~doc:
               "Full|Compact Information shown for each block. Full= Protocol \
                state and Compact= Current state hash, previous state hash, \
                blockchain length, and global slot. Default: Compact"
             Param.(optional string)
         and log_json = Cli_lib.Flag.Log.json
         and log_level = Cli_lib.Flag.Log.level
         and min_peers =
           Param.flag "--min-peers" ~aliases:["-min-peers"]
             ~doc:
               "Int(>0) Keep blocks that were accepted by at least min-peers \
                number of peers and prune the rest (Default=1)"
             Param.(optional int)
         in
         let output_format =
           match output_format with
           | Some "Full" | Some "full" ->
               `Full
           | Some "Compact" | Some "compact" | None ->
               `Compact
           | Some x ->
               failwith
                 (sprintf
                    "Invalid value %s for output-format. Currently supported \
                     formats are Full or Compact"
                    x)
         in
         let min_peers =
           match min_peers with
           | Some x when x > 0 ->
               x
           | None ->
               1
           | _ ->
               failwith "Invalid value for min-peers"
         in
         Cli_lib.Stdout_log.setup log_json log_level ;
         main ~input_dir ~output_dir ~output_format ~min_peers)))
