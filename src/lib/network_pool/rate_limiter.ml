open Core
open Network_peer

(*
   We maintain, for each "attribute" (IP, peer ID), how many operations have we
   performed for a sender with that attribute in the last X minutes.
*)

(* The interval over which we limit the max number of actions performed. *)
let interval = Time.Span.of_min 5.

(* An abelian group with a subset of non_negative elements. This is here in
   case we want to generalize to more nuanced kinds of score than numerical
   values.
*)
module type Coned_abelian_group = sig
  type t [@@deriving sexp, to_yojson]

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val is_non_negative : t -> bool
end

module type Score_intf = Coned_abelian_group

module Score : Score_intf with type t = int = struct
  open Int

  type t = int [@@deriving to_yojson, sexp]

  let ( + ) = ( + )

  let ( - ) = ( - )

  let is_non_negative = is_non_negative
end

module Record = struct
  (* For a given peer, all of the actions within [interval] that peer has performed,
     along with the remaining capacity for actions. *)
  type t =
    {mutable remaining_capacity: Score.t; elts: (Score.t * Time.t) Queue.t}
  [@@deriving sexp]

  let clear_old_entries r ~now =
    let rec go () =
      match Queue.peek r.elts with
      | None ->
          ()
      | Some (n, t) ->
          let is_old =
            let age = Time.diff now t in
            Time.Span.(age > interval)
          in
          if is_old then (
            r.remaining_capacity <- Score.(r.remaining_capacity + n) ;
            ignore (Queue.dequeue_exn r.elts : int * Time.t) ;
            go () )
    in
    go ()

  let add (r : t) ~(now : Time.t) ~(score : Score.t) =
    let new_score = Score.(r.remaining_capacity - score) in
    if Score.is_non_negative new_score then (
      Queue.enqueue r.elts (score, now) ;
      r.remaining_capacity <- new_score ;
      clear_old_entries r ~now ;
      `Ok )
    else `No_space
end

module Lru_table (Q : Hash_queue.S) = struct
  let max_size = 2048

  type t = {table: Record.t Q.t; initial_capacity: Score.t}
  [@@deriving sexp_of]

  let add ({table; initial_capacity} : t) (k : Q.Key.t) ~now ~score =
    match Q.lookup_and_move_to_back table k with
    | None ->
        if Int.(Q.length table >= max_size) then
          ignore (Q.dequeue_front table : Record.t option) ;
        Q.enqueue_back_exn table k
          {Record.remaining_capacity= initial_capacity; elts= Queue.create ()} ;
        `Ok
    | Some r ->
        Record.add r ~now ~score

  let has_capacity t k ~now ~score =
    match Q.lookup_and_move_to_back t.table k with
    | None ->
        true
    | Some r ->
        Record.clear_old_entries r ~now ;
        Score.(is_non_negative (r.remaining_capacity - score))

  let create ~initial_capacity = {initial_capacity; table= Q.create ()}

  let next_expires ({table; _} : t) (k : Q.Key.t) =
    match Q.lookup table k with
    | None ->
        Time.now ()
    | Some {elts; _} -> (
      match Queue.peek elts with Some (_, time) -> time | None -> Time.now () )
end

module Ip = struct
  module Hash_queue = Hash_queue.Make (Unix.Inet_addr)
  module Lru = Lru_table (Hash_queue)
end

module Peer_id = struct
  module Hash_queue = Hash_queue.Make (Peer.Id)
  module Lru = Lru_table (Hash_queue)
end

type t = {by_ip: Ip.Lru.t; by_peer_id: Peer_id.Lru.t} [@@deriving sexp_of]

let create ~capacity:(capacity, `Per t) =
  let initial_capacity =
    let max_per_second = Float.of_int capacity /. Time.Span.to_sec t in
    Float.round_up (max_per_second *. Time.Span.to_sec interval)
    |> Float.to_int
  in
  { by_ip= Ip.Lru.create ~initial_capacity
  ; by_peer_id= Peer_id.Lru.create ~initial_capacity }

let add {by_ip; by_peer_id} (sender : Envelope.Sender.t) ~now ~score =
  match sender with
  | Local ->
      `Within_capacity
  | Remote peer ->
      let ip = Peer.ip peer in
      let id = peer.peer_id in
      if
        Ip.Lru.has_capacity by_ip ip ~now ~score
        && Peer_id.Lru.has_capacity by_peer_id id ~now ~score
      then (
        ignore (Ip.Lru.add by_ip ip ~now ~score : [`No_space | `Ok]) ;
        ignore (Peer_id.Lru.add by_peer_id id ~now ~score : [`No_space | `Ok]) ;
        `Within_capacity )
      else `Capacity_exceeded

let next_expires {by_peer_id; _} (sender : Envelope.Sender.t) =
  match sender with
  | Local ->
      Time.now ()
  | Remote {peer_id; _} ->
      Peer_id.Lru.next_expires by_peer_id peer_id

module Summary = struct
  type r = {capacity_used: Score.t} [@@deriving to_yojson]

  type t = {by_ip: (string * r) list; by_peer_id: (string * r) list}
  [@@deriving to_yojson]
end

let summary ({by_ip; by_peer_id} : t) =
  let open Summary in
  to_yojson
    { by_ip=
        Ip.Hash_queue.foldi by_ip.table ~init:[] ~f:(fun acc ~key ~data ->
            ( Unix.Inet_addr.to_string key
            , {capacity_used= by_ip.initial_capacity - data.remaining_capacity}
            )
            :: acc )
    ; by_peer_id=
        Peer_id.Hash_queue.foldi by_peer_id.table ~init:[]
          ~f:(fun acc ~key ~data ->
            ( Peer.Id.to_string key
            , {capacity_used= by_ip.initial_capacity - data.remaining_capacity}
            )
            :: acc ) }
