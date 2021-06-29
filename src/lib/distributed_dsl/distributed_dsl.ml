open Core_kernel
open Async_kernel
open Pipe_lib

module Ident = struct
  let state = ref 0

  let next () =
    let old = !state in
    state := old + 1 ;
    old
end

module type Message_delay_intf = sig
  type message

  val delay : message -> Time.Span.t
end

module Time_queue = struct
  type 'action t =
    { mutable curr_time: Time.Span.t
    ; pending_actions: ('action * Time.Span.t) Heap.t
    ; mutable on_new_action: unit Ivar.t option }

  let handle_in_future t ~after action =
    Option.iter t.on_new_action ~f:(fun ivar ->
        Ivar.fill_if_empty ivar () ;
        t.on_new_action <- None ) ;
    Heap.add t.pending_actions (action, Time.Span.(after + t.curr_time))

  let create ~now =
    { curr_time= now
    ; pending_actions=
        Heap.create ~cmp:(fun (_, ts) (_, ts') -> Time.Span.compare ts ts') ()
    ; on_new_action= None }

  let actions_ready t =
    match (Heap.top t.pending_actions, t.on_new_action) with
    | Some _, _ ->
        return ()
    | None, Some ivar ->
        Ivar.read ivar
    | None, None ->
        let ivar = Ivar.create () in
        t.on_new_action <- Some ivar ;
        Ivar.read ivar

  let tick_forwards t ~f =
    let rec go () =
      let do_next_action () =
        let action, _ = Heap.pop_exn t.pending_actions in
        f action
      in
      match Heap.top t.pending_actions with
      | None ->
          return ()
      | Some (_, at) ->
          let%bind () = do_next_action () in
          if Time.Span.(t.curr_time >= at) then go ()
          else (
            t.curr_time <- at ;
            let rec loop () =
              match Heap.top t.pending_actions with
              | None ->
                  return ()
              | Some (_, at) when Time.Span.(t.curr_time >= at) ->
                  let%bind () = do_next_action () in
                  loop ()
              | Some _ ->
                  return ()
            in
            loop () )
    in
    go ()

  let%test_unit "time_queue_empty_returns" =
    Async.Thread_safe.block_on_async_exn (fun () ->
        let t = create ~now:Time.Span.zero in
        tick_forwards t ~f:(fun _ -> return (assert false)) )

  let%test_unit "time_queue_handles_in_order" =
    Async.Thread_safe.block_on_async_exn (fun () ->
        let tick_assert_sees t actions =
          let table = Char.Table.create () in
          let%map () =
            tick_forwards t ~f:(fun next ->
                Char.Table.add_exn table ~key:next ~data:() ;
                return () )
          in
          if Char.Table.length table <> List.length actions then
            failwithf
              !"Char length %d and action length %d ; actions %{sexp:char \
                List.t} ; table %{sexp:char List.t}\n\
                %!"
              (Char.Table.length table) (List.length actions) actions
              (Char.Table.keys table) () ;
          List.iter actions ~f:(fun a -> Char.Table.find_exn table a)
        in
        let t : char t = create ~now:Time.Span.zero in
        handle_in_future t ~after:(Time.Span.of_int_sec 100) 'a' ;
        handle_in_future t ~after:(Time.Span.of_int_sec 10) 'b' ;
        let%bind () = tick_assert_sees t ['b'] in
        handle_in_future t ~after:(Time.Span.of_int_sec 10) 'c' ;
        handle_in_future t ~after:(Time.Span.of_int_sec 10) 'd' ;
        let%bind () = tick_assert_sees t ['c'; 'd'] in
        tick_assert_sees t ['a'] )
end

module type Temporal_intf = sig
  type t

  val create : now:Time.Span.t -> t

  val tick_forwards : t -> unit Deferred.t
end

module type Fake_timer_transport_intf = sig
  include Node.Transport_intf

  include Node.Timer_intf with type t := t

  include Temporal_intf with type t := t

  val stop_listening : t -> me:peer -> unit
end

module type Fake_timer_transport_s = functor
  (Message :sig
            
            type t
          end)
  (Message_delay : Message_delay_intf with type message := Message.t)
  (Peer : Node.Peer_intf)
  -> Fake_timer_transport_intf
     with type message := Message.t
      and type peer := Peer.t

module Fake_timer_transport (Message : sig
  type t
end)
(Message_delay : Message_delay_intf with type message := Message.t)
(Peer : Node.Peer_intf) =
struct
  module Token = Int

  type tok = Token.t [@@deriving equal, sexp]

  type message = Message.t

  type peer = Peer.t

  type action =
    | Timeout of [`Cancelled | `Finished] Ivar.t
    | Msg of message * peer

  type t =
    { network:
        (message Linear_pipe.Reader.t * message Linear_pipe.Writer.t)
        Peer.Table.t
    ; q: action Time_queue.t
    ; timer_stoppers: [`Cancelled | `Finished] Ivar.t Token.Table.t }

  let create ~now =
    { network= Peer.Table.create ()
    ; q= Time_queue.create ~now
    ; timer_stoppers= Token.Table.create () }

  let actions_ready t = Time_queue.actions_ready t.q

  let tick_forwards t =
    Time_queue.tick_forwards t.q ~f:(function
      | Timeout ivar ->
          Ivar.fill_if_empty ivar `Finished ;
          Ivar.read ivar >>| Fn.const ()
      | Msg (m, p) -> (
        match Peer.Table.find t.network p with
        | None ->
            failwithf "Unknown recipient %s"
              (Peer.sexp_of_t p |> Sexp.to_string_hum)
              ()
        | Some (r, w) ->
            Linear_pipe.write_or_exn ~capacity:1024 w r m ;
            Linear_pipe.values_available r >>| Fn.const () ) )

  let wait t ts =
    let tok = Ident.next () in
    let ivar = Ivar.create () in
    Time_queue.handle_in_future t.q ~after:ts (Timeout ivar) ;
    Token.Table.add_exn t.timer_stoppers ~key:tok ~data:ivar ;
    (tok, Ivar.read ivar)

  let cancel t tok =
    match Token.Table.find t.timer_stoppers tok with
    | Some ivar ->
        if Ivar.is_full ivar then
          [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
        Ivar.fill ivar `Cancelled
    | None ->
        ()

  let send t ~recipient message : unit Deferred.Or_error.t =
    match Peer.Table.find t.network recipient with
    | None ->
        return
          (Or_error.error_string
             (Printf.sprintf "Unknown recipient %s"
                (Peer.sexp_of_t recipient |> Sexp.to_string_hum)))
    | Some (_r, _w) ->
        Time_queue.handle_in_future t.q
          ~after:(Message_delay.delay message)
          (Msg (message, recipient)) ;
        Deferred.Or_error.return ()

  let listen t ~me =
    let r, w = Linear_pipe.create () in
    Peer.Table.add_exn t.network ~key:me ~data:(r, w) ;
    r

  let stop_listening t ~me = Peer.Table.remove t.network me
end

module type Trivial_peer_intf = sig
  type t = int [@@deriving equal, hash, compare, sexp, yojson]

  include Hashable.S with type t := t
end

module Trivial_peer : Trivial_peer_intf = struct
  module T = struct
    type t = int [@@deriving equal, hash, compare, sexp, yojson]
  end

  include Hashable.Make (T)
  include T
end

module type S = functor
  (State :sig
          
          type t [@@deriving equal, sexp, yojson]
        end)
  (Message :sig
            
            type t
          end)
  (Message_delay : Message_delay_intf with type message := Message.t)
  (Message_label :sig
                  
                  type label [@@deriving enum, sexp]

                  include Hashable.S with type t = label
                end)
  (Timer_label :sig
                
                type label [@@deriving enum, sexp]

                include Hashable.S with type t = label
              end)
  (Condition_label :sig
                    
                    type label [@@deriving enum, sexp, yojson]

                    include Hashable.S with type t = label
                  end)
  -> sig
  type t

  module Timer_transport :
    Fake_timer_transport_intf
    with type message := Message.t
     and type peer := Trivial_peer.t

  module MyNode :
    Node.S
    with type message := Message.t
     and type state := State.t
     and type transport := Timer_transport.t
     and type peer := Trivial_peer.t
     and module Message_label := Message_label
     and module Timer_label := Timer_label
     and module Condition_label := Condition_label
     and module Timer := Timer_transport

  module Identifier : sig
    type t = Trivial_peer.t
  end

  type change = Delete of Identifier.t | Add of MyNode.t

  val loop :
    t -> stop:unit Deferred.t -> max_iters:int option -> unit Deferred.t

  val change : t -> change list -> unit

  val create :
       count:int
    -> initial_state:State.t
    -> (int -> MyNode.message_command list * MyNode.handle_command list)
    -> stop:unit Deferred.t
    -> t
end

module Make (State : sig
  type t [@@deriving equal, sexp, yojson]
end) (Message : sig
  type t
end)
(Message_delay : Message_delay_intf with type message := Message.t)
                                                                  (Message_label : sig
    type label [@@deriving enum, sexp]

    include Hashable.S with type t = label
end) (Timer_label : sig
  type label [@@deriving enum, sexp]

  include Hashable.S with type t = label
end) (Condition_label : sig
  type label [@@deriving enum, sexp, yojson]

  include Hashable.S with type t = label
end) =
struct
  module Timer_transport =
    Fake_timer_transport (Message) (Message_delay) (Trivial_peer)
  module MyNode =
    Node.Make (State) (Message) (Trivial_peer) (Timer_transport)
      (Message_label)
      (Timer_label)
      (Condition_label)
      (Timer_transport)

  module Identifier = struct
    type t = Trivial_peer.t

    include MyNode.Identifier
  end

  type t = {nodes: MyNode.t Identifier.Table.t; timer: Timer_transport.t}

  type change = Delete of Identifier.t | Add of MyNode.t

  let change t changes =
    List.iter changes ~f:(function
      | Delete ident ->
          Identifier.Table.remove t.nodes ident
      | Add n ->
          Identifier.Table.add_exn t.nodes ~key:(MyNode.ident n) ~data:n )

  let rec loop t ~stop ~max_iters =
    match max_iters with
    | Some iters when iters <= 0 ->
        return ()
    | _ -> (
        let merge : 'a. 'a option -> 'a option -> 'a option =
         fun a b ->
          match (a, b) with
          | None, None ->
              None
          | None, Some b ->
              Some b
          | Some a, None ->
              Some a
          | Some a, Some _ ->
              Some a
        in
        let choose3 (a : 'a Deferred.t) (a_imm : 'a option) (b : 'b Deferred.t)
            (b_imm : 'b option) (c : 'c Deferred.t) (c_imm : 'c option) :
            ('a option * 'b option * 'c option) Deferred.t =
          Deferred.any
            [ ( a
              >>| fun a ->
              ( Some a
              , merge (Deferred.peek b) b_imm
              , merge (Deferred.peek c) c_imm ) )
            ; ( b
              >>| fun b ->
              ( merge (Deferred.peek a) a_imm
              , Some b
              , merge (Deferred.peek c) c_imm ) )
            ; ( c
              >>| fun () ->
              ( merge (Deferred.peek a) a_imm
              , merge (Deferred.peek b) b_imm
              , Some () ) ) ]
        in
        let node_ready : MyNode.t Deferred.t =
          let any_ready : MyNode.t Deferred.t =
            Deferred.any
              (List.map (Identifier.Table.data t.nodes) ~f:(fun n ->
                   MyNode.next_ready n >>| Fn.const n ))
          in
          any_ready
          >>| fun n ->
          let maybe_real =
            List.fold (Identifier.Table.data t.nodes) ~init:None
              ~f:(fun acc x ->
                match (acc, x) with
                | Some _, _ ->
                    acc
                | None, x when MyNode.is_ready x ->
                    Some x
                | None, _ ->
                    acc )
          in
          Option.value maybe_real ~default:n
        in
        let node_ready_imm =
          List.find (Identifier.Table.data t.nodes) ~f:MyNode.is_ready
        in
        let ticks_available = Timer_transport.actions_ready t.timer in
        let chosen =
          choose3 stop None node_ready node_ready_imm ticks_available None
        in
        match%bind chosen with
        | Some (), _, _ ->
            return ()
        | None, Some n, _ ->
            (*printf "There's a transition at peer %d\n%!" (MyNode.ident n);*)
            let%bind n' = MyNode.step n in
            let () =
              Identifier.Table.set t.nodes ~key:(MyNode.ident n) ~data:n'
            in
            loop t ~stop ~max_iters:(Option.map max_iters ~f:(fun i -> i - 1))
        | None, None, Some () ->
            (*printf "There's an event since no stuff for peers\n%!";*)
            let%bind () = Timer_transport.tick_forwards t.timer in
            loop t ~stop ~max_iters:(Option.map max_iters ~f:(fun i -> i - 1))
        | None, None, None ->
            failwith "Something is ready" )

  let create ~count ~initial_state cmds_per_node ~stop =
    let table = Identifier.Table.create () in
    let now = Time.Span.zero in
    let timer = Timer_transport.create ~now in
    let nodes =
      List.init count ~f:(fun i ->
          let messages = Timer_transport.listen timer ~me:i in
          let msg_commands, handle_commands = cmds_per_node i in
          MyNode.make_node ~logger:(Logger.create ()) ~transport:timer ~me:i
            ~messages ~initial_state ~timer msg_commands handle_commands )
    in
    (* Schedule cleanup *)
    don't_wait_for
      (let%map () = stop in
       List.iter nodes ~f:(fun n ->
           Timer_transport.stop_listening timer ~me:(MyNode.ident n) )) ;
    (* Fill table *)
    List.iter nodes ~f:(fun n ->
        Identifier.Table.add_exn table ~key:(MyNode.ident n) ~data:n ) ;
    {nodes= table; timer}
end

let%test_module "Distributed_dsl" =
  ( module struct
    let expect f =
      Async.Thread_safe.block_on_async_exn (fun () ->
          match%map Deferred.create f with
          | `Success ->
              ()
          | `Failure s ->
              failwith s )

    module State = struct
      type t = Start | Wait_msg | Sent_msg | Got_msg of int | Timeout
      [@@deriving equal, sexp, yojson]
    end

    module Message = struct
      type t = Msg of int
    end

    module Message_delay = struct
      let delay _ = Time.Span.of_ms 500.
    end

    module Message_label = struct
      type label = Send_msg [@@deriving enum, sexp, compare, hash]

      module T = struct
        type t = label [@@deriving compare, hash, sexp]
      end

      include T
      include Hashable.Make (T)
    end

    module Timer_label = struct
      type label = Timeout_message | Spawn_msg
      [@@deriving enum, sexp, compare, hash]

      module T = struct
        type t = label [@@deriving compare, hash, sexp]
      end

      include T
      include Hashable.Make (T)
    end

    module Condition_label = struct
      type label = Init | Wait_timeout | Bigger_than_five | Failure_case
      [@@deriving enum, sexp, compare, hash, yojson]

      module T = struct
        type t = label [@@deriving compare, hash, sexp]
      end

      include T
      include Hashable.Make (T)
    end

    module Machine =
      Make (State) (Message) (Message_delay) (Message_label) (Timer_label)
        (Condition_label)

    let%test_unit "run_machine" =
      expect (fun finish_ivar ->
          let open State in
          let open Message_label in
          let open Condition_label in
          let open Timer_label in
          let count = 10 in
          let spec0 =
            let open Machine.MyNode in
            ( []
            , (* no message handlers *)
              [ on Init
                  (function Start -> true | _ -> false)
                  ~f:(fun t _state ->
                    timeout' t Spawn_msg (Time.Span.of_sec 10.)
                      ~f:(fun t _state ->
                        let%map () =
                          send_multi_exn t
                            ~recipients:
                              (List.init (count - 1) ~f:(fun i -> i + 1))
                            (Msg 10)
                        in
                        Sent_msg ) ;
                    return Wait_msg ) ] )
          in
          let specRest =
            let open Machine.MyNode in
            ( [ msg Send_msg
                  (Fn.const (Fn.const true))
                  ~f:(fun _t (Msg i) -> function Wait_msg -> return (Got_msg i)
                    | m -> return m ) ]
            , [ on Init
                  (function Start -> true | _ -> false)
                  ~f:(fun _ _ -> return Wait_msg)
              ; on Wait_timeout
                  (function Wait_msg -> true | _ -> false)
                  ~f:(fun t state ->
                    timeout' t Timeout_message (Time.Span.of_sec 20.)
                      ~f:(fun _t -> function
                      | Got_msg _ as m -> return m | _ -> return Timeout ) ;
                    return state )
              ; on Failure_case
                  (function
                    | Timeout ->
                        true
                    | Got_msg i when i <= 5 ->
                        true
                    | _ ->
                        false )
                  ~f:(fun _ _ ->
                    failwith
                      "All nodes should have received a message containing a \
                       number more than five" )
              ; on Bigger_than_five
                  (function Got_msg i -> i > 5 | _ -> false)
                  ~f:(fun t state ->
                    cancel t Timeout_message ;
                    Ivar.fill_if_empty finish_ivar `Success ;
                    return state ) ] )
          in
          let machine =
            Machine.create ~count ~initial_state:Start
              ~stop:(Deferred.never ()) (fun i ->
                if i = 0 then spec0 else specRest )
          in
          don't_wait_for
            (let%map () =
               Machine.loop machine ~stop:(Deferred.never ())
                 ~max_iters:(Some 10000)
             in
             Ivar.fill_if_empty finish_ivar
               (`Failure "Stopped looping without getting to success state"))
      )
  end )
