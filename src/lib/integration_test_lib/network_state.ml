open Async_kernel
open Core_kernel
open Pipe_lib
open Mina_base

module Make
    (Engine : Intf.Engine.S)
    (Event_router : Intf.Dsl.Event_router_intf with module Engine := Engine) :
  Intf.Dsl.Network_state_intf
  with module Engine := Engine
   and module Event_router := Event_router = struct
  module Node = Engine.Network.Node

  let map_to_yojson m ~f = `Assoc String.Map.(m |> map ~f |> to_alist)

  (* TODO: Just replace the first 3 fields here with Protocol_state *)
  type t =
    { block_height: int
    ; epoch: int
    ; global_slot: int
    ; snarked_ledgers_generated: int
    ; blocks_generated: int
    ; node_initialization: bool String.Map.t
          [@to_yojson map_to_yojson ~f:(fun b -> `Bool b)]
    ; gossip_received: Gossip_state.t String.Map.t
          [@to_yojson map_to_yojson ~f:Gossip_state.to_yojson]
    ; best_tips_by_node: State_hash.t String.Map.t
          [@to_yojson map_to_yojson ~f:State_hash.to_yojson] }
  [@@deriving to_yojson]

  let empty =
    { block_height= 0
    ; epoch= 0
    ; global_slot= 0
    ; snarked_ledgers_generated= 0
    ; blocks_generated= 0
    ; node_initialization= String.Map.empty
    ; gossip_received= String.Map.empty
    ; best_tips_by_node= String.Map.empty }

  let listen ~logger event_router =
    let r, w = Broadcast_pipe.create empty in
    let update ~f =
      (* should be safe to ignore the write here, so long as `f` is synchronous *)
      let state = f (Broadcast_pipe.Reader.peek r) in
      [%log debug] "updated network state to: $state"
        ~metadata:[("state", to_yojson state)] ;
      ignore (Broadcast_pipe.Writer.write w state) ;
      Deferred.return `Continue
    in
    ignore
      (Event_router.on event_router Event_type.Block_produced
         ~f:(fun node block_produced ->
           [%log debug] "Updating network state with block produced event" ;
           update ~f:(fun state ->
               [%log debug] "handling block production from $node"
                 ~metadata:[("node", `String (Node.id node))] ;
               if block_produced.block_height > state.block_height then
                 let snarked_ledgers_generated =
                   if block_produced.snarked_ledger_generated then 1 else 0
                 in
                 { state with
                   epoch= block_produced.global_slot
                 ; global_slot= block_produced.global_slot
                 ; block_height= block_produced.block_height
                 ; blocks_generated= state.blocks_generated + 1
                 ; snarked_ledgers_generated=
                     state.snarked_ledgers_generated
                     + snarked_ledgers_generated }
               else state ) )) ;
    ignore
      (Event_router.on event_router
         Event_type.Transition_frontier_diff_application
         ~f:(fun node diff_application ->
           [%log debug]
             "Updating network state with transition frontier diff \
              application event" ;
           update ~f:(fun state ->
               [%log debug] "handling frontier diff application of $node"
                 ~metadata:[("node", `String (Node.id node))] ;
               Option.value_map diff_application.best_tip_changed
                 ~default:state ~f:(fun new_best_tip ->
                   let best_tips_by_node' =
                     String.Map.set state.best_tips_by_node ~key:(Node.id node)
                       ~data:new_best_tip
                   in
                   {state with best_tips_by_node= best_tips_by_node'} ) ) )) ;
    let handle_gossip_received event_type =
      ignore
        (Event_router.on event_router event_type
           ~f:(fun node gossip_with_direction ->
             update ~f:(fun state ->
                 { state with
                   gossip_received=
                     Map.update state.gossip_received (Node.id node)
                       ~f:(fun gossip_state_opt ->
                         let gossip_state =
                           match gossip_state_opt with
                           | None ->
                               Gossip_state.create (Node.id node)
                           | Some state ->
                               state
                         in
                         [%log debug] "GOSSIP RECEIVED by $node"
                           ~metadata:[("node", `String (Node.id node))] ;
                         [%log debug] "GOSSIP RECEIVED recevied event: $event"
                           ~metadata:
                             [ ( "event"
                               , Event_type.event_to_yojson
                                   (Event_type.Event
                                      (event_type, gossip_with_direction)) ) ] ;
                         Gossip_state.add gossip_state event_type
                           gossip_with_direction ;
                         gossip_state ) } ) ))
    in
    handle_gossip_received Block_gossip ;
    handle_gossip_received Snark_work_gossip ;
    handle_gossip_received Transactions_gossip ;
    ignore
      (Event_router.on event_router Event_type.Node_initialization
         ~f:(fun node () ->
           update ~f:(fun state ->
               [%log debug]
                 "Updating network state with initialization event of $node"
                 ~metadata:[("node", `String (Node.id node))] ;
               let node_initialization' =
                 String.Map.set state.node_initialization ~key:(Node.id node)
                   ~data:true
               in
               {state with node_initialization= node_initialization'} ) )) ;
    (r, w)
end
