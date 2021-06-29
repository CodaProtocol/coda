open Core_kernel
open Async_kernel

type t = Int64.t [@@deriving compare]

let add = Int64.( + )

let sub = Int64.( - )

let diff = Int64.( - )

let modulus = Int64.( % )

let ( < ) = Int64.( < )

module Action = struct
  type nonrec t = {at: t; perform: t Ivar.t; afterwards: unit Deferred.t}

  let compare {at; _} {at= at'; _} = compare at at'
end

(* Seconds in floating point *)
module Span = struct
  include Int64

  let of_ms = Fn.id

  let of_time_span s = Int64.of_float (Time.Span.to_ms s)
end

module Controller = struct
  type nonrec t =
    { mutable last_time: t
    ; mutable last_snapshot: Time.t
    ; actions: Action.t Heap.t }

  let create () =
    { last_time= Int64.zero
    ; last_snapshot= Time.now ()
    ; actions= Heap.create ~cmp:Action.compare () }

  let fast_forward t time =
    if time < t.last_time then ()
    else
      let sys_now = Time.now () in
      let diff = diff time t.last_time in
      let diff_since_checked =
        Span.of_time_span (Time.diff sys_now t.last_snapshot)
      in
      let true_diff = Int64.max diff diff_since_checked in
      t.last_time <- add t.last_time true_diff ;
      t.last_snapshot <- sys_now

  let now t =
    let sys_now = Time.now () in
    let diff_since_checked =
      Span.of_time_span (Time.diff sys_now t.last_snapshot)
    in
    t.last_time <- add t.last_time diff_since_checked ;
    t.last_snapshot <- sys_now ;
    t.last_time

  (* Semantics: Tick to the next event immediately. The internal clock
   * fast-forwards to at-least the time of the next event. If real sys time
   * elapsed since the last tick/now is greater we prefer that time. Thus, we
   * could end up needing to process more than one event. And we do so.
   *
   * Why? This behavior ensures that if our real logic is too slow, we'll still
   * simulate that same behavior.
   *
   * If we process at least one event, we wait until that event is handled
   * before running the next one or yielding to the caller of tick.
   *)
  let tick t =
    let exec t =
      let {Action.perform; at; afterwards} = Heap.pop_exn t.actions in
      fast_forward t at ;
      Ivar.fill_if_empty perform t.last_time ;
      afterwards
    in
    let rec go once =
      match (Heap.top t.actions, once) with
      | Some _, `First ->
          let%bind () = exec t in
          go `No
      | Some {Action.at; _}, `No when at < t.last_time ->
          let%bind () = exec t in
          go `No
      | Some _, `No ->
          return ()
      | None, _ ->
          return ()
    in
    go `First
end

let now = Controller.now

module Timeout = struct
  type 'a t = {d: 'a Deferred.t; elt: Action.t Heap.Elt.t; cancel: 'a Ivar.t}

  let create (ctrl : Controller.t) span ~f =
    let ivar = Ivar.create () in
    let cancel = Ivar.create () in
    let d = Deferred.any [Ivar.read ivar >>| f; Ivar.read cancel] in
    let elt =
      Heap.add_removable ctrl.actions
        { Action.at= add (now ctrl) span
        ; perform= ivar
        ; afterwards= d >>| ignore }
    in
    {d; elt; cancel}

  let to_deferred {d; _} = d

  let peek {d; _} = Deferred.peek d

  let cancel (ctrl : Controller.t) {elt; cancel; _} a =
    Heap.remove ctrl.actions elt ;
    Ivar.fill_if_empty cancel a
end

let%test_unit "tick triggers timeouts and fast-forwards to event time" =
  let ctrl = Controller.create () in
  let start = now ctrl in
  let fired = ref false in
  let _timeout =
    Timeout.create ctrl
      (Span.of_ms (Int64.of_int 5000))
      ~f:(fun _t -> fired := true)
  in
  Async.Thread_safe.block_on_async_exn (fun () ->
      [%test_result: Bool.t]
        ~message:"Time in simulator land doesn't progress until with tick"
        ~expect:false !fired ;
      let%map () = Controller.tick ctrl in
      [%test_result: Bool.t] ~message:"We ticked" ~expect:true !fired ;
      [%test_result: Bool.t]
        ~message:"Time fast-forwads to at least event time" ~expect:true
        Int64.(diff (now ctrl) start >= Int64.of_int 5000) )

let%test_unit "tick triggers timeouts and adjusts to system time" =
  let ctrl = Controller.create () in
  let start = now ctrl in
  let fired = ref false in
  let _timeout =
    Timeout.create ctrl
      (Span.of_ms (Int64.of_int 2))
      ~f:(fun _t -> fired := true)
  in
  Async.Thread_safe.block_on_async_exn (fun () ->
      let%bind () = Async.after (Time.Span.of_ms 5.) in
      [%test_result: Bool.t]
        ~message:"Time in simulator land doesn't progress until with tick"
        ~expect:false !fired ;
      let%map () = Controller.tick ctrl in
      [%test_result: Bool.t] ~message:"We ticked" ~expect:true !fired ;
      [%test_result: Bool.t]
        ~message:
          "Since 10ms of real time passed, we need to jump more than the 5ms \
           of the event"
        ~expect:true
        (Int64.( >= ) (diff (now ctrl) start) (Int64.of_int 5)) )

let%test_unit "tick handles multiple timeouts if necessary" =
  let ctrl = Controller.create () in
  let start = now ctrl in
  let count = ref 0 in
  let timeout x =
    ignore
      ( Timeout.create ctrl
          (Span.of_ms (Int64.of_int x))
          ~f:(fun _t -> count := !count + 1)
        : _ Timeout.t )
  in
  List.iter [2; 3; 5; 500] ~f:timeout ;
  Async.Thread_safe.block_on_async_exn (fun () ->
      let%bind () = Async.after (Time.Span.of_ms 7.) in
      [%test_result: Int.t]
        ~message:"Time in simulator land doesn't progress until with tick"
        ~expect:0 !count ;
      let%map () = Controller.tick ctrl in
      [%test_result: Int.t]
        ~message:"Sys time elapsed so we triggered more than one event"
        ~expect:3 !count ;
      [%test_result: Bool.t]
        ~message:
          "Since 10ms of real time passed, we need to jump more than the 5ms \
           of the event"
        ~expect:true
        Int64.(diff (now ctrl) start >= Int64.of_int 7) )

let%test_unit "cancelling a timeout means it won't fire" =
  let ctrl = Controller.create () in
  let message = ref "" in
  let timeout (x, s) =
    Timeout.create ctrl
      (Span.of_ms (Int64.of_int x))
      ~f:(fun _t -> message := !message ^ s)
  in
  let tokens = List.map [(2, "a"); (3, "b"); (5, "c")] ~f:timeout in
  (* Cancel "b" *)
  Timeout.cancel ctrl (List.nth_exn tokens 1) () ;
  Async.Thread_safe.block_on_async_exn (fun () ->
      let%bind () = Async.after (Time.Span.of_ms 7.) in
      let%map () = Controller.tick ctrl in
      [%test_result: String.t]
        ~message:"We only triggered the events that we didn't cancel"
        ~expect:"ac" !message )
