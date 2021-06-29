open Core_kernel
open Mina_base
open Currency
open Signature_lib

module Make
    (Engine : Intf.Engine.S)
    (Event_router : Intf.Dsl.Event_router_intf with module Engine := Engine)
    (Network_state : Intf.Dsl.Network_state_intf
                     with module Engine := Engine
                      and module Event_router := Event_router) =
struct
  open Network_state
  module Node = Engine.Network.Node

  type 'a predicate_result =
    | Predicate_passed
    | Predicate_continuation of 'a
    | Predicate_failure of Error.t

  (* NEED TO LIFT THIS UP OR FUNCTOR IT *)
  type predicate =
    | Network_state_predicate :
        (Network_state.t -> 'a predicate_result)
        * ('a -> Network_state.t -> 'a predicate_result)
        -> predicate
    | Event_predicate :
        'b Event_type.t * 'a * ('a -> Node.t -> 'b -> 'a predicate_result)
        -> predicate

  type t =
    { description: string
    ; predicate: predicate
    ; soft_timeout: Network_time_span.t
    ; hard_timeout: Network_time_span.t }

  let with_timeouts ?soft_timeout ?hard_timeout t =
    { t with
      soft_timeout= Option.value soft_timeout ~default:t.soft_timeout
    ; hard_timeout= Option.value hard_timeout ~default:t.hard_timeout }

  (* TODO: does this actually work if it's run twice? I think not *)
  (*
   * options:
   *   - assume nodes have not yet initialized by the time we get here
   *   - associate additional state to see when initialization was last checked
   *)
  let nodes_to_initialize nodes =
    let open Network_state in
    let check () (state : Network_state.t) =
      if
        List.for_all nodes ~f:(fun node ->
            String.Map.find state.node_initialization (Node.id node)
            |> Option.value ~default:false )
      then Predicate_passed
      else Predicate_continuation ()
    in
    let description =
      nodes |> List.map ~f:Node.id |> String.concat ~sep:", "
      |> Printf.sprintf "[%s] to initialize"
    in
    { description
    ; predicate= Network_state_predicate (check (), check)
    ; soft_timeout= Literal (Time.Span.of_min 10.0)
    ; hard_timeout= Literal (Time.Span.of_min 15.0) }

  let node_to_initialize node = nodes_to_initialize [node]

  (* let blocks_produced ?(active_stake_percentage = 1.0) n = *)
  let blocks_to_be_produced n =
    let init state = Predicate_continuation state.blocks_generated in
    let check init_blocks_generated state =
      if state.blocks_generated - init_blocks_generated >= n then
        Predicate_passed
      else Predicate_continuation init_blocks_generated
    in
    let soft_timeout_in_slots = 2 * n in
    { description= Printf.sprintf "%d blocks to be produced" n
    ; predicate= Network_state_predicate (init, check)
    ; soft_timeout= Slots soft_timeout_in_slots
    ; hard_timeout= Slots (soft_timeout_in_slots * 2) }

  let nodes_to_synchronize (nodes : Node.t list) =
    let all_equal ls =
      Option.value_map (List.hd ls) ~default:true ~f:(fun h ->
          [%equal: State_hash.t list] [h]
            (List.find_all_dups ~compare:State_hash.compare ls) )
    in
    let check () state =
      let best_tips =
        List.map nodes ~f:(fun node ->
            String.Map.find_exn state.best_tips_by_node (Node.id node) )
      in
      if all_equal best_tips then Predicate_passed
      else Predicate_continuation ()
    in
    let soft_timeout_in_slots = 8 * 3 in
    let formatted_nodes =
      nodes
      |> List.map ~f:(fun node -> "\"" ^ Node.id node ^ "\"")
      |> String.concat ~sep:", "
    in
    { description= Printf.sprintf "%s to synchronize" formatted_nodes
    ; predicate= Network_state_predicate (check (), check)
    ; soft_timeout= Slots soft_timeout_in_slots
    ; hard_timeout= Slots (soft_timeout_in_slots * 2) }

  let payment_to_be_included_in_frontier ~sender_pub_key ~receiver_pub_key
      ~amount =
    let command_matches_payment cmd =
      let open User_command in
      match cmd with
      | Signed_command signed_cmd -> (
          let open Signature_lib in
          let body =
            Signed_command.payload signed_cmd |> Signed_command_payload.body
          in
          match body with
          | Payment {source_pk; receiver_pk; amount= paid_amt; token_id= _}
            when Public_key.Compressed.equal source_pk sender_pub_key
                 && Public_key.Compressed.equal receiver_pk receiver_pub_key
                 && Currency.Amount.equal paid_amt amount ->
              true
          | _ ->
              false )
      | Snapp_command _ ->
          false
    in
    let check () _node (breadcrumb_added : Event_type.Breadcrumb_added.t) =
      let payment_opt =
        List.find breadcrumb_added.user_commands ~f:(fun cmd_with_status ->
            cmd_with_status.With_status.data |> User_command.forget_check
            |> command_matches_payment )
      in
      match payment_opt with
      | Some cmd_with_status ->
          let actual_status = cmd_with_status.With_status.status in
          let was_applied =
            match actual_status with
            | Transaction_status.Applied _ ->
                true
            | _ ->
                false
          in
          if was_applied then Predicate_passed
          else
            Predicate_failure
              (Error.createf "Unexpected status in matching payment: %s"
                 ( Transaction_status.to_yojson actual_status
                 |> Yojson.Safe.to_string ))
      | None ->
          Predicate_continuation ()
    in
    let soft_timeout_in_slots = 8 in
    { description=
        Printf.sprintf "payment from %s to %s of amount %s"
          (Public_key.Compressed.to_string sender_pub_key)
          (Public_key.Compressed.to_string receiver_pub_key)
          (Amount.to_string amount)
    ; predicate= Event_predicate (Event_type.Breadcrumb_added, (), check)
    ; soft_timeout= Slots soft_timeout_in_slots
    ; hard_timeout= Slots (soft_timeout_in_slots * 2) }
end
