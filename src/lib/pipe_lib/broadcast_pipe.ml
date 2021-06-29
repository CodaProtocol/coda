open Core_kernel
open Async_kernel

type 'a t =
  { root_pipe: 'a Pipe.Writer.t
  ; mutable cache: 'a
  ; mutable reader_id: int
  ; pipes: 'a Pipe.Writer.t Int.Table.t }

let create a =
  let root_r, root_w = Pipe.create () in
  let t =
    {root_pipe= root_w; cache= a; reader_id= 0; pipes= Int.Table.create ()}
  in
  let downstream_flushed_v : unit Ivar.t ref = ref @@ Ivar.create () in
  let consumer =
    Pipe.add_consumer root_r ~downstream_flushed:(fun () ->
        let%map () = Ivar.read !downstream_flushed_v in
        (* Sub-pipes are never closed without closing the master pipe. *)
        `Ok )
  in
  don't_wait_for
    (Pipe.iter ~flushed:(Consumer consumer) root_r ~f:(fun v ->
         downstream_flushed_v := Ivar.create () ;
         let inner_pipes = Int.Table.data t.pipes in
         let%bind () =
           Deferred.List.iter ~how:`Parallel inner_pipes ~f:(fun p ->
               Pipe.write p v )
         in
         Pipe.Consumer.values_sent_downstream consumer ;
         let%bind () =
           Deferred.List.iter ~how:`Parallel inner_pipes ~f:(fun p ->
               Deferred.ignore_m @@ Pipe.downstream_flushed p )
         in
         if Ivar.is_full !downstream_flushed_v then
           [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
         Ivar.fill !downstream_flushed_v () ;
         Deferred.unit )) ;
  (t, t)

exception Already_closed of string

let if_closed t ~then_ ~else_ =
  if Pipe.is_closed t.root_pipe then then_ () else else_ ()

let guard_already_closed ~context t f =
  if_closed t ~then_:(fun () -> raise (Already_closed context)) ~else_:f

module Reader = struct
  type nonrec 'a t = 'a t

  let peek t =
    guard_already_closed ~context:"Reader.peek" t (fun () -> t.cache)

  let fresh_reader_id t =
    t.reader_id <- t.reader_id + 1 ;
    t.reader_id

  let prepare_pipe t ~default_value ~f =
    if_closed t
      ~then_:(Fn.const (Deferred.return default_value))
      ~else_:(fun () ->
        let r, w = Pipe.create () in
        Pipe.write_without_pushback w (peek t) ;
        let reader_id = fresh_reader_id t in
        Int.Table.add_exn t.pipes ~key:reader_id ~data:w ;
        let d =
          let%map b = f r in
          Int.Table.remove t.pipes reader_id ;
          b
        in
        d )

  (* The sub-pipes have no downstream consumer, so the downstream flushed should
     always be determined and return `Ok. *)
  let add_trivial_consumer p =
    Pipe.add_consumer p ~downstream_flushed:(fun () -> Deferred.return `Ok)

  let fold t ~init ~f =
    prepare_pipe t ~default_value:init ~f:(fun r ->
        let consumer = add_trivial_consumer r in
        Pipe.fold r ~init ~f:(fun acc v ->
            let%map res = f acc v in
            Pipe.Consumer.values_sent_downstream consumer ;
            res ) )

  let iter t ~f =
    prepare_pipe t ~default_value:() ~f:(fun r ->
        let consumer = add_trivial_consumer r in
        Pipe.iter ~flushed:(Consumer consumer) r ~f:(fun v ->
            let%map () = f v in
            Pipe.Consumer.values_sent_downstream consumer ) )

  let iter_until t ~f =
    let rec loop ~consumer reader =
      match%bind Pipe.read ~consumer reader with
      | `Eof ->
          return ()
      | `Ok v ->
          let%bind b = f v in
          Pipe.Consumer.values_sent_downstream consumer ;
          if b then return () else loop ~consumer reader
    in
    prepare_pipe t ~default_value:() ~f:(fun reader ->
        let consumer = add_trivial_consumer reader in
        loop ~consumer reader )
end

module Writer = struct
  type nonrec 'a t = 'a t

  let write t x =
    guard_already_closed ~context:"Writer.write" t (fun () ->
        t.cache <- x ;
        let%bind () = Pipe.write t.root_pipe x in
        let%bind _ = Pipe.downstream_flushed t.root_pipe in
        Deferred.unit )

  let close t =
    guard_already_closed ~context:"Writer.close" t (fun () ->
        Pipe.close t.root_pipe ;
        Int.Table.iter t.pipes ~f:(fun w -> Pipe.close w) ;
        Int.Table.clear t.pipes )
end

let map t ~f =
  let r, w = create (f (Reader.peek t)) in
  don't_wait_for (Reader.iter t ~f:(fun x -> Writer.write w (f x))) ;
  r

(*
 * 1. Cached value is keeping peek working
 * 2. Multiple listeners receive the first mvar value
 * 3. Multiple listeners receive updates after changes
 * 4. Peek sees the latest value
 * 5. If we close the broadcast pipe, all listeners stop
*)
let%test_unit "listeners properly receive updates" =
  let expect_pipe t expected =
    let got_rev =
      Reader.fold t ~init:[] ~f:(fun acc a1 -> return @@ (a1 :: acc))
    in
    let%map got = got_rev >>| List.rev in
    [%test_result: int list]
      ~message:"Expected the following values from the pipe" ~expect:expected
      got
  in
  Async.Thread_safe.block_on_async_exn (fun () ->
      let initial = 0 in
      let r, w = create initial in
      (*1*)
      [%test_result: int] ~message:"Initial value not observed when peeking"
        ~expect:initial (Reader.peek r) ;
      (* 2-3 *)
      let d1 = expect_pipe r [0; 1; 2] in
      let d2 = expect_pipe r [0; 1; 2] in
      don't_wait_for d1 ;
      don't_wait_for d2 ;
      let next_value = 1 in
      (*3*)
      let%bind () = Writer.write w next_value in
      (*4*)
      let next_value = 2 in
      let%bind () = Writer.write w next_value in
      (*5*)
      [%test_result: int] ~message:"Latest value is observed when peeking"
        ~expect:next_value (Reader.peek r) ;
      (*6*)
      Writer.close w ;
      Deferred.both d1 d2 >>| Fn.ignore )

let%test_module _ =
  ( module struct
    type iter_counts =
      {mutable immediate_iterations: int; mutable deferred_iterations: int}

    let zero_counts () = {immediate_iterations= 0; deferred_iterations= 0}

    let assert_immediate counts expected =
      [%test_eq: int] counts.immediate_iterations expected

    let assert_deferred counts expected =
      [%test_eq: int] counts.deferred_iterations expected

    let assert_both counts expected =
      assert_immediate counts expected ;
      assert_deferred counts expected

    let%test "Writing is synchronous" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          Core.Backtrace.elide := false ;
          Async.Scheduler.set_record_backtraces true ;
          let pipe_r, pipe_w = create () in
          let counts1, counts2 = (zero_counts (), zero_counts ()) in
          let setup_reader counts =
            don't_wait_for
            @@ Reader.iter pipe_r ~f:(fun () ->
                   counts.immediate_iterations
                   <- counts.immediate_iterations + 1 ;
                   let%map () = Async.after @@ Time.Span.of_sec 1. in
                   counts.deferred_iterations <- counts.deferred_iterations + 1
               )
          in
          setup_reader counts1 ;
          (* The reader doesn't run until we yield. *)
          assert_both counts1 0 ;
          (* Once we yield, the reader has run, but has returned to the
             scheduler before setting deferred_iterations. *)
          let%bind () = Async.after @@ Time.Span.of_sec 0.1 in
          assert_immediate counts1 1 ;
          assert_deferred counts1 0 ;
          (* After we yield for long enough, deferred_iterations has been
             set. *)
          let%bind () = Async.after @@ Time.Span.of_sec 1.1 in
          assert_both counts1 1 ;
          (* Writing to the pipe blocks until the reader is finished. *)
          let%bind () = Writer.write pipe_w () in
          assert_both counts1 2 ;
          (* A second reader gets the current value, and all values written
             after its creation. *)
          setup_reader counts2 ;
          assert_both counts2 0 ;
          let%bind () = Async.after @@ Time.Span.of_sec 0.1 in
          assert_immediate counts2 1 ;
          assert_deferred counts2 0 ;
          let%bind () = Writer.write pipe_w () in
          assert_both counts1 3 ; assert_both counts2 2 ; Deferred.return true
      )
  end )
