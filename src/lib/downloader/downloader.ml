open Async
open Core
open Pipe_lib
open Network_peer

module Job = struct
  type ('key, 'attempt, 'a) t =
    { key: 'key
    ; attempts: 'attempt Peer.Map.t
    ; res:
        ('a Envelope.Incoming.t * 'attempt Peer.Map.t, [`Finished]) Result.t
        Ivar.t }

  let result t = Ivar.read t.res
end

type 'a pred = 'a -> bool

let pred_to_yojson _f _x = `String "<opaque>"

let sexp_opaque_to_yojson _f _x = `String "<opaque>"

module Claimed_knowledge = struct
  type 'key t = [`All | `Some of 'key list | `Call of 'key pred sexp_opaque]
  [@@deriving sexp, to_yojson]

  let to_yojson f t =
    match t with
    | `Some ks ->
        let n = List.length ks in
        if n > 5 then to_yojson (fun x -> `Int x) (`Some [n])
        else to_yojson f t
    | _ ->
        to_yojson f t

  let check ~equal t k =
    match t with
    | `All ->
        true
    | `Some ks ->
        List.mem ~equal ks k
    | `Call f ->
        f k
end

module Make (Key : sig
  type t [@@deriving to_yojson, hash, sexp, compare]

  include Hashable.S with type t := t

  include Comparable.S with type t := t
end) (Attempt : sig
  type t [@@deriving to_yojson]

  val download : t

  val worth_retrying : t -> bool
end) (Result : sig
  type t

  val key : t -> Key.t
end) (Knowledge_context : sig
  type t
end) : sig
  type t [@@deriving to_yojson]

  module Job : sig
    type t = (Key.t, Attempt.t, Result.t) Job.t

    val result :
         t
      -> ( Result.t Envelope.Incoming.t * Attempt.t Peer.Map.t
         , [`Finished] )
         Base.Result.t
         Deferred.t
  end

  val cancel : t -> Key.t -> unit

  val create :
       max_batch_size:int
    -> stop:unit Deferred.t
    -> trust_system:Trust_system.t
    -> get:(Peer.t -> Key.t list -> Result.t list Deferred.Or_error.t)
    -> knowledge_context:Knowledge_context.t Broadcast_pipe.Reader.t
    -> knowledge:(   Knowledge_context.t
                  -> Peer.t
                  -> Key.t Claimed_knowledge.t Deferred.t)
    -> peers:(unit -> Peer.t list Deferred.t)
    -> preferred:Peer.t list
    -> t Deferred.t

  val download : t -> key:Key.t -> attempts:Attempt.t Peer.Map.t -> Job.t

  val mark_preferred : t -> Peer.t -> now:Time.t -> unit

  val add_knowledge : t -> Peer.t -> Key.t list -> unit

  val update_knowledge : t -> Peer.t -> Key.t Claimed_knowledge.t -> unit

  val total_jobs : t -> int

  val check_invariant : t -> unit

  val set_check_invariant : (t -> unit) -> unit
end = struct
  let max_wait = Time.Span.of_ms 100.

  module J = Job

  module Job = struct
    type t = (Key.t, Attempt.t, Result.t) Job.t

    let to_yojson ({key; attempts; _} : t) : Yojson.Safe.t =
      `Assoc
        [ ("key", Key.to_yojson key)
        ; ( "attempts"
          , `Assoc
              (List.map (Map.to_alist attempts) ~f:(fun (p, a) ->
                   (Peer.to_multiaddr_string p, Attempt.to_yojson a) )) ) ]

    let result = Job.result
  end

  module Make_hash_queue (Key : Hashable.S) = struct
    module Key_value = struct
      type 'a t = {key: Key.t; mutable value: 'a} [@@deriving fields]
    end

    (* Hash_queue would be perfect, but it doesn't expose enough for
         us to make sure the underlying queue is sorted by blockchain_length. *)
    type 'a t =
      { queue: 'a Key_value.t Doubly_linked.t
      ; table: 'a Key_value.t Doubly_linked.Elt.t Key.Table.t }

    let dequeue t =
      Option.map (Doubly_linked.remove_first t.queue) ~f:(fun {key; value} ->
          Hashtbl.remove t.table key ; value )

    let enqueue t (e : _ J.t) =
      if Hashtbl.mem t.table e.key then `Key_already_present
      else
        let kv = {Key_value.key= e.key; value= e} in
        let elt =
          match
            Doubly_linked.find_elt t.queue ~f:(fun {value; _} ->
                Key.compare e.key value.J.key < 0 )
          with
          | None ->
              (* e is >= everything. Put it at the back. *)
              Doubly_linked.insert_last t.queue kv
          | Some pred ->
              Doubly_linked.insert_before t.queue pred kv
        in
        Hashtbl.set t.table ~key:e.key ~data:elt ;
        `Ok

    let lookup t k =
      Option.map (Hashtbl.find t.table k) ~f:(fun x ->
          (Doubly_linked.Elt.value x).value )

    let remove t k =
      match Hashtbl.find_and_remove t.table k with
      | None ->
          ()
      | Some elt ->
          Doubly_linked.remove t.queue elt

    let length t = Doubly_linked.length t.queue

    let to_list t = List.map (Doubly_linked.to_list t.queue) ~f:Key_value.value

    let create () = {table= Key.Table.create (); queue= Doubly_linked.create ()}
  end

  module Q = Make_hash_queue (Key)

  module Knowledge = struct
    module Key_set = struct
      type t = Key.Hash_set.t [@@deriving sexp]

      let to_yojson t = `List (List.map (Hash_set.to_list t) ~f:Key.to_yojson)
    end

    type t =
      {claimed: Key.t Claimed_knowledge.t option; tried_and_failed: Key_set.t}
    [@@deriving sexp, to_yojson]

    let clear t = Hash_set.clear t.tried_and_failed

    let create () = {claimed= None; tried_and_failed= Key.Hash_set.create ()}

    let knows t k =
      if Hash_set.mem t.tried_and_failed k then `No
      else
        match t.claimed with
        | None ->
            `No_information
        | Some claimed ->
            if Claimed_knowledge.check ~equal:Key.equal claimed k then
              `Claims_to
            else `No
  end

  module Useful_peers = struct
    module Preferred_heap = struct
      (* The preferred peers, sorted by the last time that they were useful to us. *)
      type t =
        { heap: (Peer.t * Time.t) Heap.t
        ; table: (Peer.t * Time.t) Heap.Elt.t Peer.Table.t }

      let cmp (p1, t1) (p2, t2) =
        (* Later is smaller *)
        match Int.neg (Time.compare t1 t2) with
        | 0 ->
            Peer.compare p1 p2
        | c ->
            c

      let clear t =
        let rec go t = match Heap.pop t with None -> () | Some _ -> go t in
        go t.heap ; Hashtbl.clear t.table

      let create () = {heap= Heap.create ~cmp (); table= Peer.Table.create ()}

      let add t (p, time) =
        Option.iter (Hashtbl.find t.table p) ~f:(fun elt ->
            Heap.remove t.heap elt ) ;
        Hashtbl.set t.table ~key:p ~data:(Heap.add_removable t.heap (p, time))

      let sexp_of_t (t : t) =
        List.sexp_of_t [%sexp_of: Peer.t * Time.t] (Heap.to_list t.heap)

      let t_of_sexp s =
        let elts = [%of_sexp: (Peer.t * Time.t) list] s in
        let t = create () in
        List.iter elts ~f:(add t) ;
        t

      let of_list xs =
        let now = Time.now () in
        let t = create () in
        List.iter xs ~f:(fun p -> add t (p, now)) ;
        t

      let mem t p = Hashtbl.mem t.table p

      let fold t ~init ~f =
        Heap.fold t.heap ~init ~f:(fun acc (p, _) -> f acc p)

      let to_list (t : t) = List.map ~f:fst (Heap.to_list t.heap)
    end

    type t =
      { downloading_peers: Peer.Hash_set.t
      ; knowledge_requesting_peers: Peer.Hash_set.t
      ; temporary_ignores: (unit, unit) Clock.Event.t sexp_opaque Peer.Table.t
      ; mutable all_preferred: Preferred_heap.t
      ; knowledge: Knowledge.t Peer.Table.t
            (* Written to when something changes. *)
      ; r: unit Strict_pipe.Reader.t sexp_opaque
      ; w:
          ( unit
          , Strict_pipe.drop_head Strict_pipe.buffered
          , unit )
          Strict_pipe.Writer.t
          sexp_opaque }
    [@@deriving sexp]

    let reset_knowledge t ~all_peers =
      (* Reset preferred *)
      Preferred_heap.clear t.all_preferred ;
      Hashtbl.filter_mapi_inplace t.knowledge ~f:(fun ~key:p ~data:k ->
          Hash_set.clear k.tried_and_failed ;
          if Set.mem all_peers p then Some {k with claimed= None} else None ) ;
      Set.iter all_peers ~f:(fun p ->
          if not (Hashtbl.mem t.knowledge p) then
            Hashtbl.add_exn t.knowledge ~key:p ~data:(Knowledge.create ()) ) ;
      Strict_pipe.Writer.write t.w ()

    let to_yojson
        { knowledge
        ; all_preferred
        ; knowledge_requesting_peers
        ; temporary_ignores
        ; downloading_peers
        ; r= _
        ; w= _ } =
      let list xs =
        `Assoc [("length", `Int (List.length xs)); ("elts", `List xs)]
      in
      let f q = Knowledge.to_yojson q in
      `Assoc
        [ ( "all"
          , `Assoc
              (List.map (Hashtbl.to_alist knowledge) ~f:(fun (p, s) ->
                   (Peer.to_multiaddr_string p, f s) )) )
        ; ( "preferred"
          , `List
              (List.map (Preferred_heap.to_list all_preferred) ~f:(fun p ->
                   `String (Peer.to_multiaddr_string p) )) )
        ; ( "temporary_ignores"
          , list (List.map ~f:Peer.to_yojson (Hashtbl.keys temporary_ignores))
          )
        ; ( "downloading_peers"
          , list
              (List.map ~f:Peer.to_yojson (Hash_set.to_list downloading_peers))
          )
        ; ( "knowledge_requesting_peers"
          , list
              (List.map ~f:Peer.to_yojson
                 (Hash_set.to_list knowledge_requesting_peers)) ) ]

    let create ~preferred ~all_peers =
      let knowledge =
        Peer.Table.of_alist_exn
          (List.map (List.dedup_and_sort ~compare:Peer.compare all_peers)
             ~f:(fun p -> (p, Knowledge.create ())))
      in
      let r, w =
        Strict_pipe.create ~name:"useful_peers-available" ~warn_on_drop:false
          (Buffered (`Capacity 0, `Overflow Drop_head))
      in
      { downloading_peers= Peer.Hash_set.create ()
      ; knowledge_requesting_peers= Peer.Hash_set.create ()
      ; temporary_ignores= Peer.Table.create ()
      ; knowledge
      ; r
      ; w
      ; all_preferred= Preferred_heap.of_list preferred }

    let tear_down
        { downloading_peers
        ; temporary_ignores
        ; knowledge_requesting_peers
        ; knowledge
        ; r= _
        ; w
        ; all_preferred } =
      Hashtbl.iter temporary_ignores ~f:(fun e ->
          Clock.Event.abort_if_possible e () ) ;
      Hashtbl.clear temporary_ignores ;
      Hash_set.clear downloading_peers ;
      Hash_set.clear knowledge_requesting_peers ;
      Hashtbl.iter knowledge ~f:Knowledge.clear ;
      Hashtbl.clear knowledge ;
      Preferred_heap.clear all_preferred ;
      Strict_pipe.Writer.close w

    module Knowledge_summary = struct
      type t = {no_information: int; no: int; claims_to: int}
      [@@deriving fields]

      (* Score needs revising -- should be more lexicographic *)
      let score {no_information; no= _; claims_to} =
        Float.of_int claims_to +. (0.1 *. Float.of_int no_information)
    end

    let maxes ~compare xs =
      Sequence.fold xs ~init:[] ~f:(fun acc x ->
          match acc with
          | [] ->
              [x]
          | best :: _ ->
              let c = compare best x in
              if c = 0 then x :: acc else if c < 0 then [x] else acc )
      |> List.rev

    let useful_peer t ~pending_jobs =
      let ts =
        List.rev
          (Preferred_heap.fold t.all_preferred ~init:[] ~f:(fun acc p ->
               match Hashtbl.find t.knowledge p with
               | None ->
                   acc
               | Some k ->
                   (p, k) :: acc ))
        @ Hashtbl.fold t.knowledge ~init:[] ~f:(fun ~key:p ~data:k acc ->
              if not (Preferred_heap.mem t.all_preferred p) then (p, k) :: acc
              else acc )
      in
      (*
         Algorithm:
         - Look at the pending jobs
         - Find all peers who have the best claim to knowing the first job in the queue.
         - If there are any, pick the one of those who has the best total knowledge score.
         - If there are none, pick the one of all the peers who has the best total knowledge score.
      *)
      let with_best_claim_to_knowing_first_job =
        match List.hd pending_jobs with
        | None ->
            []
        | Some j ->
            Sequence.filter_map (Sequence.of_list ts) ~f:(fun (p, k) ->
                match Knowledge.knows k j.J.key with
                | `No ->
                    None
                | `Claims_to ->
                    Some ((p, k), `Claims_to)
                | `No_information ->
                    Some ((p, k), `No_information) )
            |> maxes ~compare:(fun (_, c1) (_, c2) ->
                   match (c1, c2) with
                   | `Claims_to, `Claims_to | `No_information, `No_information
                     ->
                       0
                   | `Claims_to, `No_information ->
                       1
                   | `No_information, `Claims_to ->
                       -1 )
            |> List.map ~f:fst
      in
      let ts =
        match with_best_claim_to_knowing_first_job with
        | [] ->
            ts
        | _ :: _ ->
            with_best_claim_to_knowing_first_job
      in
      let knowledge =
        List.map ts ~f:(fun (p, k) ->
            let summary, js =
              List.fold
                ~init:
                  ( {Knowledge_summary.no_information= 0; no= 0; claims_to= 0}
                  , [] )
                pending_jobs
                ~f:(fun (acc, js) j ->
                  let field, js =
                    let open Knowledge_summary.Fields in
                    match Knowledge.knows k j.J.key with
                    | `Claims_to ->
                        (claims_to, j :: js)
                    | `No ->
                        (no, js)
                    | `No_information ->
                        (no_information, j :: js)
                  in
                  (Field.map field acc ~f:(( + ) 1), js) )
            in
            ((p, List.rev js), Knowledge_summary.score summary) )
      in
      let useful_exists =
        List.exists knowledge ~f:(fun (_, s) -> Float.(s > 0.))
      in
      let best =
        List.max_elt
          (List.filter knowledge ~f:(fun ((p, _), _) ->
               (not (Hashtbl.mem t.temporary_ignores p))
               && (not (Hash_set.mem t.downloading_peers p))
               && not (Hash_set.mem t.knowledge_requesting_peers p) ))
          ~compare:(fun (_, s1) (_, s2) -> Float.compare s1 s2)
      in
      match best with
      | None ->
          if useful_exists then `Useful_but_busy else `No_peers
      | Some ((p, k), score) ->
          if Float.(score <= 0.) then `Stalled else `Useful (p, k)

    type update =
      | Refreshed_peers of {all_peers: Peer.Set.t}
      | Download_finished of
          Peer.t * [`Successful of Key.t list] * [`Unsuccessful of Key.t list]
      | Download_starting of Peer.t
      | Job_cancelled of Key.t
      | Add_knowledge of {peer: Peer.t; claimed: Key.t list; out_of_band: bool}
      | Knowledge_request_starting of Peer.t
      | Knowledge of
          { peer: Peer.t
          ; claimed: Key.t Claimed_knowledge.t
          ; active_jobs: Job.t list
          ; out_of_band: bool }

    let jobs_no_longer_needed t ks =
      Hashtbl.iter t.knowledge ~f:(fun s ->
          List.iter ks ~f:(Hash_set.remove s.tried_and_failed) )

    let ignore_period = Time.Span.of_min 2.

    let update t u =
      match u with
      | Add_knowledge {peer; claimed; out_of_band} ->
          if not out_of_band then
            Hash_set.remove t.knowledge_requesting_peers peer ;
          Hashtbl.update t.knowledge peer ~f:(function
            | None ->
                { Knowledge.claimed= Some (`Some claimed)
                ; tried_and_failed= Key.Hash_set.create () }
            | Some k ->
                let claimed =
                  match k.claimed with
                  | None ->
                      `Some claimed
                  | Some (`Some claimed') ->
                      `Some
                        (List.dedup_and_sort ~compare:Key.compare
                           (claimed' @ claimed))
                  | Some `All ->
                      `All
                  | Some (`Call f) ->
                      let s = Key.Hash_set.of_list claimed in
                      `Call (fun key -> f key || Hash_set.mem s key)
                in
                {k with claimed= Some claimed} )
      | Knowledge_request_starting peer ->
          Hash_set.add t.knowledge_requesting_peers peer
      | Knowledge {peer; claimed; active_jobs; out_of_band} ->
          if not out_of_band then
            Hash_set.remove t.knowledge_requesting_peers peer ;
          let tried_and_failed =
            let s =
              match Hashtbl.find t.knowledge peer with
              | None ->
                  Key.Hash_set.create ()
              | Some {tried_and_failed; _} ->
                  tried_and_failed
            in
            List.iter active_jobs ~f:(fun j ->
                match Map.find j.J.attempts peer with
                | None ->
                    ()
                | Some a ->
                    if not (Attempt.worth_retrying a) then Hash_set.add s j.key
            ) ;
            s
          in
          Hashtbl.set t.knowledge ~key:peer
            ~data:{Knowledge.claimed= Some claimed; tried_and_failed}
      | Job_cancelled h ->
          jobs_no_longer_needed t [h] ;
          Hashtbl.iter t.knowledge ~f:(fun s ->
              Hash_set.remove s.tried_and_failed h )
      | Download_starting peer ->
          Hash_set.add t.downloading_peers peer
      | Download_finished (peer0, `Successful succs, `Unsuccessful unsuccs)
        -> (
          (let cancel =
             Option.iter ~f:(fun e -> Clock.Event.abort_if_possible e ())
           in
           if List.is_empty succs then
             Hashtbl.update t.temporary_ignores peer0 ~f:(fun x ->
                 cancel x ;
                 Clock.Event.run_after ignore_period
                   (fun () ->
                     Hashtbl.remove t.temporary_ignores peer0 ;
                     if not (Strict_pipe.Writer.is_closed t.w) then
                       Strict_pipe.Writer.write t.w () )
                   () )
           else (
             Hashtbl.find_and_remove t.temporary_ignores peer0 |> cancel ;
             Preferred_heap.add t.all_preferred (peer0, Time.now ()) )) ;
          Hash_set.remove t.downloading_peers peer0 ;
          jobs_no_longer_needed t succs ;
          match Hashtbl.find t.knowledge peer0 with
          | None ->
              ()
          | Some {tried_and_failed; claimed= _} ->
              List.iter unsuccs ~f:(Hash_set.add tried_and_failed) )
      | Refreshed_peers {all_peers} ->
          Hashtbl.filter_keys_inplace t.knowledge ~f:(Set.mem all_peers) ;
          Set.iter all_peers ~f:(fun p ->
              if not (Hashtbl.mem t.knowledge p) then
                Hashtbl.add_exn t.knowledge ~key:p
                  ~data:
                    { Knowledge.claimed= None
                    ; tried_and_failed= Key.Hash_set.create () } )

    let update t u : unit =
      update t u ;
      if not (Strict_pipe.Writer.is_closed t.w) then
        Strict_pipe.Writer.write t.w ()
  end

  type t =
    { mutable next_flush: (unit, unit) Clock.Event.t option
    ; mutable all_peers: Peer.Set.t
    ; pending: Job.t Q.t
    ; downloading: (Peer.t * Job.t * Time.t) Key.Table.t
    ; useful_peers: Useful_peers.t
    ; flush_r: unit Strict_pipe.Reader.t (* Single reader *)
    ; flush_w:
        ( unit
        , Strict_pipe.drop_head Strict_pipe.buffered
        , unit )
        Strict_pipe.Writer.t
          (* buffer of length 0 *)
    ; get: Peer.t -> Key.t list -> Result.t list Deferred.Or_error.t
    ; max_batch_size: int
          (* A peer is useful if there is a job in the pending queue which has not
   been attempted with that peer. *)
    ; got_new_peers_w:
        ( unit
        , Strict_pipe.drop_head Strict_pipe.buffered
        , unit )
        Strict_pipe.Writer.t
          (* buffer of length 0 *)
    ; got_new_peers_r: unit Strict_pipe.Reader.t
    ; logger: Logger.t
    ; trust_system: Trust_system.t
    ; stop: unit Deferred.t }

  let total_jobs (t : t) = Q.length t.pending + Hashtbl.length t.downloading

  (* Checks disjointness *)
  let check_invariant (t : t) =
    Set.length
      (Key.Set.union_list
         [ Q.to_list t.pending
           |> List.map ~f:(fun j -> j.key)
           |> Key.Set.of_list
         ; Key.Set.of_hashtbl_keys t.downloading ])
    |> [%test_eq: int] (total_jobs t)

  let check_invariant_r = ref check_invariant

  let set_check_invariant f = check_invariant_r := f

  let job_finished t j x =
    Hashtbl.remove t.downloading j.J.key ;
    Ivar.fill_if_empty j.res x ;
    try !check_invariant_r t
    with e ->
      [%log' debug t.logger]
        ~metadata:[("exn", `String (Exn.to_string e))]
        "job_finished $exn"

  let kill_job _t j = Ivar.fill_if_empty j.J.res (Error `Finished)

  let flush_soon t =
    Option.iter t.next_flush ~f:(fun e -> Clock.Event.abort_if_possible e ()) ;
    t.next_flush
    <- Some
         (Clock.Event.run_after max_wait
            (fun () ->
              if not (Strict_pipe.Writer.is_closed t.flush_w) then
                Strict_pipe.Writer.write t.flush_w () )
            ())

  let cancel t h =
    let job =
      List.find_map ~f:Lazy.force
        [ lazy (Q.lookup t.pending h)
        ; lazy
            (Option.map ~f:(fun (_, j, _) -> j) (Hashtbl.find t.downloading h))
        ]
    in
    Q.remove t.pending h ;
    Hashtbl.remove t.downloading h ;
    match job with
    | None ->
        ()
    | Some j ->
        kill_job t j ;
        Useful_peers.update t.useful_peers (Job_cancelled h)

  let enqueue t e = Q.enqueue t.pending e

  let enqueue_exn t e =
    assert ([%equal: [`Ok | `Key_already_present]] (enqueue t e) `Ok)

  let active_jobs t =
    Q.to_list t.pending
    @ List.map (Hashtbl.data t.downloading) ~f:(fun (_, j, _) -> j)

  let refresh_peers t peers =
    Broadcast_pipe.Reader.iter peers ~f:(fun peers ->
        let peers' = Peer.Set.of_list peers in
        let new_peers = Set.diff peers' t.all_peers in
        Useful_peers.update t.useful_peers
          (Refreshed_peers {all_peers= peers'}) ;
        if
          (not (Set.is_empty new_peers))
          && not (Strict_pipe.Writer.is_closed t.got_new_peers_w)
        then Strict_pipe.Writer.write t.got_new_peers_w () ;
        t.all_peers <- Peer.Set.of_list peers ;
        Deferred.unit )
    |> don't_wait_for

  let tear_down
      ( { next_flush
        ; all_peers= _
        ; flush_w
        ; get= _
        ; got_new_peers_w
        ; flush_r= _
        ; useful_peers
        ; got_new_peers_r= _
        ; pending
        ; downloading
        ; max_batch_size= _
        ; logger= _
        ; trust_system= _
        ; stop= _ } as t ) =
    let rec clear_queue q =
      match Q.dequeue q with
      | None ->
          ()
      | Some j ->
          kill_job t j ; clear_queue q
    in
    Option.iter next_flush ~f:(fun e -> Clock.Event.abort_if_possible e ()) ;
    Strict_pipe.Writer.close flush_w ;
    Useful_peers.tear_down useful_peers ;
    Strict_pipe.Writer.close got_new_peers_w ;
    Hashtbl.iter downloading ~f:(fun (_, j, _) -> kill_job t j) ;
    Hashtbl.clear downloading ;
    clear_queue pending

  let download t peer xs =
    let f xs =
      let n = List.length xs in
      `Assoc
        ( ("length", `Int n)
        ::
        ( if n > 8 then []
        else [("elts", `List (List.map xs ~f:(fun j -> Key.to_yojson j.J.key)))]
        ) )
    in
    let keys = List.map xs ~f:(fun x -> x.J.key) in
    let fail ?punish (e : Error.t) =
      let e = Error.to_string_hum e in
      [%log' debug t.logger] "Downloading from $peer failed ($error) on $keys"
        ~metadata:
          [("peer", Peer.to_yojson peer); ("error", `String e); ("keys", f xs)] ;
      if Option.is_some punish then
        (* TODO: Make this an insta ban *)
        Trust_system.(
          record t.trust_system t.logger peer
            Actions.(Violated_protocol, Some (e, [])))
        |> don't_wait_for ;
      List.iter xs ~f:(fun x ->
          enqueue_exn t
            { x with
              attempts= Map.set x.attempts ~key:peer ~data:Attempt.download }
      ) ;
      flush_soon t
    in
    List.iter xs ~f:(fun x ->
        Hashtbl.set t.downloading ~key:x.key ~data:(peer, x, Time.now ()) ) ;
    Useful_peers.update t.useful_peers (Download_starting peer) ;
    let download_deferred = t.get peer keys in
    upon download_deferred (fun res ->
        let succs, unsuccs =
          match res with
          | Error _ ->
              ([], keys)
          | Ok rs ->
              let all = Key.Hash_set.of_list keys in
              let succ =
                List.filter_map rs ~f:(fun r ->
                    let k = Result.key r in
                    if Hash_set.mem all k then Some k else None )
              in
              List.iter succ ~f:(Hash_set.remove all) ;
              (succ, Hash_set.to_list all)
        in
        Useful_peers.update t.useful_peers
          (Download_finished (peer, `Successful succs, `Unsuccessful unsuccs))
    ) ;
    let%map res =
      Deferred.choose
        [ Deferred.choice download_deferred (fun x -> `Not_stopped x)
        ; Deferred.choice t.stop (fun () -> `Stopped)
        ; Deferred.choice
            (* This happens if all the jobs are cancelled. *)
            (Deferred.List.map xs ~f:(fun x -> Ivar.read x.res))
            (fun _ -> `Stopped) ]
    in
    List.iter xs ~f:(fun j -> Hashtbl.remove t.downloading j.key) ;
    match res with
    | `Stopped ->
        List.iter xs ~f:(kill_job t)
    | `Not_stopped r -> (
      match r with
      | Error e ->
          fail e
      | Ok rs ->
          [%log' debug t.logger] "result is $result"
            ~metadata:
              [ ("result", f xs)
              ; ("peer", `String (Peer.to_multiaddr_string peer)) ] ;
          let received_at = Time.now () in
          let jobs =
            Key.Table.of_alist_exn (List.map xs ~f:(fun x -> (x.key, x)))
          in
          List.iter rs ~f:(fun r ->
              match Hashtbl.find jobs (Result.key r) with
              | None ->
                  (* Got something we didn't ask for. *)
                  Trust_system.(
                    record t.trust_system t.logger peer
                      Actions.(Violated_protocol, None))
                  |> don't_wait_for
              | Some j ->
                  Hashtbl.remove jobs j.key ;
                  job_finished t j
                    (Ok
                       ( { Envelope.Incoming.data= r
                         ; received_at
                         ; sender= Remote peer }
                       , j.attempts )) ) ;
          (* Anything left in jobs, we did not get results for :( *)
          Hashtbl.iter jobs ~f:(fun x ->
              Hashtbl.remove t.downloading x.J.key ;
              enqueue_exn t
                { x with
                  attempts= Map.set x.attempts ~key:peer ~data:Attempt.download
                } ) ;
          flush_soon t )

  let to_yojson t : Yojson.Safe.t =
    check_invariant t ;
    let list xs =
      `Assoc [("length", `Int (List.length xs)); ("elts", `List xs)]
    in
    let now = Time.now () in
    let f q = list (List.map ~f:Job.to_yojson (Q.to_list q)) in
    `Assoc
      [ ("total_jobs", `Int (total_jobs t))
      ; ("useful_peers", Useful_peers.to_yojson t.useful_peers)
      ; ("pending", f t.pending)
      ; ( "downloading"
        , list
            (List.map (Hashtbl.to_alist t.downloading)
               ~f:(fun (h, (p, _, start)) ->
                 `Assoc
                   [ ("hash", Key.to_yojson h)
                   ; ("start", `String (Time.to_string start))
                   ; ( "time_since_start"
                     , `String (Time.Span.to_string_hum (Time.diff now start))
                     )
                   ; ("peer", `String (Peer.to_multiaddr_string p)) ] )) ) ]

  let post_stall_retry_delay = Time.Span.of_min 1.

  let rec step t =
    if Q.length t.pending = 0 then (
      [%log' debug t.logger] "Downloader: no jobs. waiting" ;
      match%bind Strict_pipe.Reader.read t.flush_r with
      | `Eof ->
          [%log' debug t.logger] "Downloader: flush eof" ;
          Deferred.unit
      | `Ok () ->
          step t )
    else
      match
        Useful_peers.useful_peer t.useful_peers
          ~pending_jobs:(Q.to_list t.pending)
      with
      | `No_peers -> (
          match%bind Strict_pipe.Reader.read t.got_new_peers_r with
          | `Eof ->
              [%log' debug t.logger] "Downloader: new peers eof" ;
              Deferred.unit
          | `Ok () ->
              step t )
      | `Useful_but_busy -> (
          [%log' debug t.logger] "Downloader: Waiting. All useful peers busy" ;
          let read p =
            Pipe.read_choice_single_consumer_exn
              (Strict_pipe.Reader.to_linear_pipe p).pipe [%here]
          in
          match%bind
            Deferred.choose [read t.flush_r; read t.useful_peers.r]
          with
          | `Eof ->
              [%log' debug t.logger] "Downloader: flush eof" ;
              Deferred.unit
          | `Ok () ->
              (* Try again, something might have changed *)
              step t )
      | `Stalled ->
          [%log' debug t.logger]
            "Downloader: all stalled. Resetting knowledge, waiting %s and \
             then retrying."
            (Time.Span.to_string_hum post_stall_retry_delay) ;
          Useful_peers.reset_knowledge t.useful_peers ~all_peers:t.all_peers ;
          let%bind () = after post_stall_retry_delay in
          [%log' debug t.logger] "Downloader: continuing after reset" ;
          step t
      | `Useful (peer, might_know) -> (
          let to_download = List.take might_know t.max_batch_size in
          [%log' debug t.logger] "Downloader: downloading $n from $peer"
            ~metadata:
              [ ("n", `Int (List.length to_download))
              ; ("peer", Peer.to_yojson peer) ] ;
          List.iter to_download ~f:(fun j -> Q.remove t.pending j.key) ;
          match to_download with
          | [] ->
              step t
          | _ :: _ ->
              don't_wait_for (download t peer to_download) ;
              step t )

  let add_knowledge t peer claimed =
    Useful_peers.update t.useful_peers
      (Add_knowledge {peer; claimed; out_of_band= true})

  let update_knowledge t peer claimed =
    Useful_peers.update t.useful_peers
      (Knowledge {peer; claimed; active_jobs= active_jobs t; out_of_band= true})

  let mark_preferred t peer ~now =
    Useful_peers.Preferred_heap.add t.useful_peers.all_preferred (peer, now)

  let create ~max_batch_size ~stop ~trust_system ~get ~knowledge_context
      ~knowledge ~peers ~preferred =
    let%map all_peers = peers () in
    let pipe ~name c =
      Strict_pipe.create ~warn_on_drop:false ~name
        (Buffered (`Capacity c, `Overflow Drop_head))
    in
    let flush_r, flush_w = pipe ~name:"flush" 0 in
    let got_new_peers_r, got_new_peers_w = pipe ~name:"got_new_peers" 0 in
    let t =
      { all_peers= Peer.Set.of_list all_peers
      ; pending= Q.create ()
      ; next_flush= None
      ; flush_r
      ; flush_w
      ; got_new_peers_r
      ; got_new_peers_w
      ; useful_peers= Useful_peers.create ~all_peers ~preferred
      ; get
      ; max_batch_size
      ; logger= Logger.create ()
      ; trust_system
      ; downloading= Key.Table.create ()
      ; stop }
    in
    let peers =
      let r, w = Broadcast_pipe.create [] in
      upon stop (fun () -> Broadcast_pipe.Writer.close w) ;
      Clock.every' ~stop (Time.Span.of_min 1.) (fun () ->
          peers ()
          >>= fun ps ->
          try Broadcast_pipe.Writer.write w ps
          with Broadcast_pipe.Already_closed _ -> Deferred.unit ) ;
      r
    in
    let deferred_ok = Deferred.return `Ok in
    let deferred_finished = Deferred.return `Finished in
    let const_eof _ = `Eof in
    let rec jobs_to_download stop =
      if total_jobs t <> 0 then deferred_ok
      else
        match%bind
          Deferred.choose
            [ choice stop const_eof
            ; choice
                (Pipe.values_available
                   (Strict_pipe.Reader.to_linear_pipe t.flush_r).pipe)
                Fn.id
            ; choice
                (Pipe.values_available
                   (Strict_pipe.Reader.to_linear_pipe t.useful_peers.r).pipe)
                Fn.id ]
        with
        | `Eof ->
            deferred_finished
        | `Ok ->
            jobs_to_download stop
    in
    let () =
      let request_r, request_w =
        Strict_pipe.create ~name:"knowledge-requests" Strict_pipe.Synchronous
      in
      upon stop (fun () -> Strict_pipe.Writer.close request_w) ;
      let refresh_knowledge stop peer =
        Clock.every' (Time.Span.of_min 7.) ~stop (fun () ->
            match%bind jobs_to_download stop with
            | `Finished ->
                Deferred.unit
            | `Ok ->
                if not (Strict_pipe.Writer.is_closed request_w) then
                  Strict_pipe.Writer.write request_w peer
                else Deferred.unit )
      in
      let ps : unit Ivar.t Peer.Table.t = Peer.Table.create () in
      Broadcast_pipe.Reader.iter peers ~f:(fun peers ->
          let peers = Peer.Hash_set.of_list peers in
          Hashtbl.filteri_inplace ps ~f:(fun ~key:p ~data:finished ->
              let keep = Hash_set.mem peers p in
              if not keep then Ivar.fill_if_empty finished () ;
              keep ) ;
          Hash_set.iter peers ~f:(fun p ->
              if not (Hashtbl.mem ps p) then (
                let finished = Ivar.create () in
                refresh_knowledge (Ivar.read finished) p ;
                Hashtbl.add_exn ps ~key:p ~data:finished ) ) ;
          Deferred.unit )
      |> don't_wait_for ;
      let throttle =
        Throttle.create ~continue_on_error:true ~max_concurrent_jobs:8
      in
      let get_knowledge ctx peer =
        Throttle.enqueue throttle (fun () -> knowledge ctx peer)
      in
      Broadcast_pipe.Reader.iter knowledge_context ~f:(fun _ ->
          Hashtbl.mapi_inplace ps ~f:(fun ~key:p ~data:finished ->
              Ivar.fill_if_empty finished () ;
              let finished = Ivar.create () in
              refresh_knowledge (Ivar.read finished) p ;
              finished ) ;
          Deferred.unit )
      |> don't_wait_for ;
      Strict_pipe.Reader.iter request_r ~f:(fun peer ->
          (* TODO: The pipe/clock logic is not quite right, but it is good enough. *)
          if Deferred.is_determined stop then Deferred.unit
          else
            let%map () = Throttle.capacity_available throttle in
            don't_wait_for
              (let ctx = Broadcast_pipe.Reader.peek knowledge_context in
               (* TODO: Check if already downloading a job from them here. *)
               Useful_peers.update t.useful_peers
                 (Knowledge_request_starting peer) ;
               let%map k = get_knowledge ctx peer in
               Useful_peers.update t.useful_peers
                 (Knowledge
                    { out_of_band= false
                    ; peer
                    ; claimed= k
                    ; active_jobs= active_jobs t })) )
      |> don't_wait_for
    in
    don't_wait_for (step t) ;
    upon stop (fun () -> tear_down t) ;
    every ~stop (Time.Span.of_sec 30.) (fun () ->
        [%log' debug t.logger]
          ~metadata:[("jobs", to_yojson t)]
          "Downloader jobs" ) ;
    refresh_peers t peers ;
    t

  (* After calling download, if no one else has called within time [max_wait],
       we flush our queue. *)
  let download t ~key ~attempts : Job.t =
    match (Q.lookup t.pending key, Hashtbl.find t.downloading key) with
    | Some _, Some _ ->
        assert false
    | Some x, None | None, Some (_, x, _) ->
        x
    | None, None ->
        flush_soon t ;
        let e = {J.key; attempts; res= Ivar.create ()} in
        enqueue_exn t e ; e
end
