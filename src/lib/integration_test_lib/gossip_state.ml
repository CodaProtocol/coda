open Core_kernel

module By_direction = struct
  type 'a t = {sent: 'a; received: 'a} [@@deriving to_yojson, fields]
end

module type Set = sig
  type 'a t [@@deriving to_yojson]

  val add :
    'a t -> 'a Event_type.Gossip.With_direction.t Event_type.t -> 'a -> unit

  val size : _ t -> int

  val inter : 'a t list -> 'a t

  val union : 'a t list -> 'a t

  val create : unit -> 'a t
end

module Set : Set = struct
  type _ t = Int.Hash_set.t

  let size = Hash_set.length

  let inter = List.reduce_exn ~f:Hash_set.inter

  let union xs =
    let res = Int.Hash_set.create () in
    List.iter xs ~f:(fun s -> Hash_set.iter s ~f:(Hash_set.add res)) ;
    res

  let to_yojson _f t = `Int (Hash_set.length t)

  let create () = Int.Hash_set.create ()

  let add (type a) (t : a t)
      (type_ : a Event_type.Gossip.With_direction.t Event_type.t) (x : a) =
    let h =
      let open Event_type.Gossip in
      match type_ with
      | Block_gossip ->
          Block.hash_r x
      | Transactions_gossip ->
          Transactions.hash_r x
      | Snark_work_gossip ->
          Snark_work.hash_r x
    in
    Hash_set.add t h
end

open Event_type.Gossip

type t =
  { node_id: string
  ; blocks: Block.r Set.t By_direction.t
  ; transactions: Transactions.r Set.t By_direction.t
  ; snark_work: Snark_work.r Set.t By_direction.t }
[@@deriving to_yojson, fields]

let create node_id : t =
  let f create =
    {By_direction.sent= create (); By_direction.received= create ()}
  in
  { node_id
  ; blocks= f Set.create
  ; transactions= f Set.create
  ; snark_work= f Set.create }

let add (gossip_state : t) (type a)
    (event_type : a Event_type.Gossip.With_direction.t Event_type.t)
    ((gossip_message : a), (dir : Event_type.Gossip.Direction.t)) : unit =
  let set : a Set.t By_direction.t =
    match event_type with
    | Block_gossip ->
        gossip_state.blocks
    | Transactions_gossip ->
        gossip_state.transactions
    | Snark_work_gossip ->
        gossip_state.snark_work
  in
  let directional_set =
    match dir with
    | Sent ->
        By_direction.sent set
    | Received ->
        By_direction.received set
  in
  Set.add directional_set event_type gossip_message

(* this function returns a tuple: (number of messages seen by all nodes, number of messages seen by at least one node ) *)
(* each gossip_state in the input list gossip_states corresponds to a node *)
let stats (type a)
    (event_type : a Event_type.Gossip.With_direction.t Event_type.t)
    (gossip_states : t list) ~(exclusion_list : string list) =
  match gossip_states with
  | [] ->
      (`Seen_by_all 0, `Seen_by_some 0)
  | _ ->
      let getter_func : t -> a Set.t By_direction.t =
        match event_type with
        | Block_gossip ->
            blocks
        | Transactions_gossip ->
            transactions
        | Snark_work_gossip ->
            snark_work
      in
      let gossip_states_filtered =
        List.filter_map gossip_states ~f:(fun gos_state ->
            if
              List.exists exclusion_list ~f:(fun id ->
                  String.equal id gos_state.node_id )
            then None
            else Some gos_state )
      in
      let event_type_gossip_states =
        List.map gossip_states_filtered ~f:(fun gos_state ->
            let event_type_gossip_state_by_direction = getter_func gos_state in
            Set.union
              [ event_type_gossip_state_by_direction.sent
              ; event_type_gossip_state_by_direction.received ] )
      in
      ( `Seen_by_all (Set.size (Set.inter event_type_gossip_states))
      , `Seen_by_some (Set.size (Set.union event_type_gossip_states)) )

let consistency_ratio event_type gossip_states ~exclusion_list =
  let `Seen_by_all inter, `Seen_by_some union =
    stats event_type gossip_states ~exclusion_list
  in
  if union = 0 then 1. else Float.of_int inter /. Float.of_int union
