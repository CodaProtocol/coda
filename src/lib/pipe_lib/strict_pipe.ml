open Async_kernel
open Core_kernel

exception Overflow of string

exception Multiple_reads_attempted of string

type crash = Overflow_behavior_crash

type drop_head = Overflow_behavior_drop_head

type call = Overflow_behavior_call

type (_, _, _) overflow_behavior =
  | Crash : ('a, crash, unit) overflow_behavior
  | Drop_head : ('a, drop_head, unit) overflow_behavior
  | Call : ('a -> 'r) -> ('a, call, 'r option) overflow_behavior

type synchronous = Type_synchronous

type _ buffered = Type_buffered

type (_, _, _) type_ =
  | Synchronous : ('a, synchronous, unit Deferred.t) type_
  | Buffered :
      [`Capacity of int] * [`Overflow of ('a, 'b, 'r) overflow_behavior]
      -> ('a, 'b buffered, 'r) type_

let value_or_empty = Option.value ~default:"<unnamed>"

module Reader0 = struct
  type 't t =
    { reader: 't Pipe.Reader.t
    ; mutable has_reader: bool
    ; mutable downstreams: downstreams
    ; name: string option }

  and downstreams =
    | [] : downstreams
    | ( :: ) : 'a t * downstreams -> downstreams

  let rec downstreams_from_list : 'a t list -> downstreams = function
    | [] ->
        []
    | r :: rs ->
        r :: downstreams_from_list rs

  (* TODO: See #1281 *)
  let to_linear_pipe {reader= pipe; has_reader; _} =
    {Linear_pipe.Reader.pipe; has_reader}

  let of_linear_pipe ?name {Linear_pipe.Reader.pipe= reader; has_reader} =
    {reader; has_reader; downstreams= []; name}

  let assert_not_read reader =
    if reader.has_reader then
      raise (Multiple_reads_attempted (value_or_empty reader.name))

  let wrap_reader ?name reader =
    {reader; has_reader= false; downstreams= []; name}

  let enforce_single_reader reader deferred =
    assert_not_read reader ;
    reader.has_reader <- true ;
    let%map result = deferred in
    reader.has_reader <- false ;
    result

  let read t = enforce_single_reader t (Pipe.read t.reader)

  let read' t = enforce_single_reader t (Pipe.read' t.reader)

  let fold reader ~init ~f =
    enforce_single_reader reader
      (let rec go b =
         match%bind Pipe.read reader.reader with
         | `Eof ->
             return b
         | `Ok a ->
             (* The async scheduler could yield here *)
             let%bind b' = f b a in
             go b'
       in
       go init)

  let fold_until reader ~init ~f =
    enforce_single_reader reader
      (let rec go b =
         match%bind Pipe.read reader.reader with
         | `Eof ->
             return (`Eof b)
         | `Ok a -> (
             (* The async scheduler could yield here *)
             match%bind f b a with
             | `Stop x ->
                 return (`Terminated x)
             | `Continue b' ->
                 go b' )
       in
       go init)

  let fold_without_pushback ?consumer reader ~init ~f =
    Pipe.fold_without_pushback ?consumer reader.reader ~init ~f

  let iter reader ~f = fold reader ~init:() ~f:(fun () -> f)

  let iter_without_pushback ?consumer ?continue_on_error reader ~f =
    Pipe.iter_without_pushback reader.reader ?consumer ?continue_on_error ~f

  let iter' reader ~f = Pipe.iter' reader.reader ~f

  let map reader ~f =
    assert_not_read reader ;
    reader.has_reader <- true ;
    let strict_reader =
      wrap_reader ?name:reader.name (Pipe.map reader.reader ~f)
    in
    reader.downstreams <- [strict_reader] ;
    strict_reader

  let filter_map reader ~f =
    assert_not_read reader ;
    reader.has_reader <- true ;
    let strict_reader =
      wrap_reader ?name:reader.name (Pipe.filter_map reader.reader ~f)
    in
    reader.downstreams <- [strict_reader] ;
    strict_reader

  let clear t = Pipe.clear t.reader

  let is_closed reader = Pipe.is_closed reader.reader

  module Merge = struct
    let iter readers ~f =
      let not_empty r = not @@ Pipe.is_empty r.reader in
      let rec read_deferred readers =
        let%bind ready_reader =
          match List.find readers ~f:not_empty with
          | Some reader ->
              Deferred.return (Some reader)
          | None ->
              let%map () =
                Deferred.choose
                  (List.map readers ~f:(fun r ->
                       Deferred.choice (Pipe.values_available r.reader)
                         (fun _ -> ()) ))
              in
              List.find readers ~f:not_empty
        in
        match ready_reader with
        | Some reader -> (
          match Pipe.read_now reader.reader with
          | `Nothing_available ->
              failwith "impossible"
          | `Eof ->
              Deferred.unit
          | `Ok value ->
              Deferred.bind (f value) ~f:(fun () -> read_deferred readers) )
        | None -> (
          match List.filter readers ~f:(fun r -> not @@ is_closed r) with
          | [] ->
              Deferred.unit
          | open_readers ->
              read_deferred open_readers )
      in
      List.iter readers ~f:assert_not_read ;
      read_deferred readers

    let iter_sync readers ~f = iter readers ~f:(fun x -> f x ; Deferred.unit)
  end

  module Fork = struct
    let n reader count =
      let pipes = List.init count ~f:(fun _ -> Pipe.create ()) in
      let readers, writers = List.unzip pipes in
      (* This one place we _do_ want iter with pushback which we want to trigger
       * when all reads have pushed back downstream
       *
       * Since future reads will resolve via the iter_without_pushback, we
       * should still get the behavior we want. *)
      don't_wait_for
        (Pipe.iter reader.reader ~f:(fun x ->
             Deferred.List.iter writers ~f:(fun writer ->
                 if not (Pipe.is_closed writer) then Pipe.write writer x
                 else return () ) )) ;
      don't_wait_for
        (let%map () = Deferred.List.iter readers ~f:Pipe.closed in
         Pipe.close_read reader.reader) ;
      let strict_readers =
        List.map readers ~f:(wrap_reader ?name:reader.name)
      in
      reader.downstreams <- downstreams_from_list strict_readers ;
      strict_readers

    let two reader =
      match n reader 2 with [a; b] -> (a, b) | _ -> failwith "unexpected"

    let three reader =
      match n reader 3 with
      | [a; b; c] ->
          (a, b, c)
      | _ ->
          failwith "unexpected"
  end

  let rec close_downstreams = function
    | [] ->
        ()
    (* The use of close_read is justified, because close_read would do
     * everything close does, and in addition:
     * 1. all pending flushes become determined with `Reader_closed.
     * 2. the pipe buffer is cleared.
     * 3. all subsequent reads will get `Eof. *)
    | r :: rs ->
        Pipe.close_read r.reader ; close_downstreams rs
end

module Writer = struct
  type ('t, 'type_, 'write_return) t =
    { type_: ('t, 'type_, 'write_return) type_
    ; strict_reader: 't Reader0.t
    ; writer: 't Pipe.Writer.t
    ; warn_on_drop: bool
    ; name: string option }

  (* TODO: See #1281 *)
  let to_linear_pipe {writer= pipe; _} = pipe

  let handle_buffered_write : type type_ return.
         ('t, type_, return) t
      -> 't
      -> capacity:int
      -> on_overflow:(unit -> return)
      -> normal_return:return
      -> return =
   fun t data ~capacity ~on_overflow ~normal_return ->
    if Pipe.length t.strict_reader.reader > capacity then on_overflow ()
    else (
      Pipe.write_without_pushback t.writer data ;
      normal_return )

  let write : type type_ return. ('t, type_, return) t -> 't -> return =
   fun writer data ->
    ( if Pipe.is_closed writer.writer then
      let logger = Logger.create () in
      [%log warn] "writing to closed pipe $name"
        ~metadata:
          [ ( "name"
            , `String
                (Sexplib.Sexp.to_string ([%sexp_of: string option] writer.name))
            ) ] ) ;
    match writer.type_ with
    | Synchronous ->
        Pipe.write writer.writer data
    | Buffered (`Capacity capacity, `Overflow Crash) ->
        handle_buffered_write writer data ~capacity
          ~on_overflow:(fun () -> raise (Overflow (value_or_empty writer.name)))
          ~normal_return:()
    | Buffered (`Capacity capacity, `Overflow Drop_head) ->
        handle_buffered_write writer data ~capacity
          ~on_overflow:(fun () ->
            let logger = Logger.create () in
            let my_name = Option.value writer.name ~default:"<unnamed>" in
            if writer.warn_on_drop then
              [%log warn]
                ~metadata:[("pipe_name", `String my_name)]
                "Dropping message on pipe $pipe_name" ;
            ignore
              ( Pipe.read_now writer.strict_reader.reader
                : [`Eof | `Nothing_available | `Ok of 'a] ) ;
            Pipe.write_without_pushback writer.writer data )
          ~normal_return:()
    | Buffered (`Capacity capacity, `Overflow (Call f)) ->
        handle_buffered_write writer data ~capacity
          ~on_overflow:(fun () -> Some (f data))
          ~normal_return:None

  let close {strict_reader; writer; _} =
    Pipe.close writer ;
    Reader0.close_downstreams strict_reader.downstreams

  let kill {strict_reader; writer; _} =
    Pipe.clear strict_reader.reader ;
    Pipe.close writer ;
    Reader0.close_downstreams strict_reader.downstreams

  let is_closed {writer; _} = Pipe.is_closed writer
end

let create ?name ?(warn_on_drop = true) type_ =
  let reader, writer = Pipe.create () in
  let strict_reader =
    Reader0.{reader; has_reader= false; downstreams= []; name}
  in
  let strict_writer =
    Writer.{type_; strict_reader; warn_on_drop; writer; name}
  in
  (strict_reader, strict_writer)

let transfer reader Writer.{strict_reader; writer; _} ~f =
  Reader0.(reader.downstreams <- [strict_reader]) ;
  Reader0.enforce_single_reader reader (Pipe.transfer reader.reader writer ~f)

let rec transfer_while_writer_alive reader writer ~f =
  if Pipe.is_closed writer.Writer.writer then Deferred.unit
  else
    match%bind Pipe.read reader.Reader0.reader with
    | `Ok x ->
        let%bind () = Pipe.write_if_open writer.Writer.writer (f x) in
        transfer_while_writer_alive reader writer ~f
    | `Eof ->
        Pipe.close_read writer.Writer.strict_reader.reader ;
        Deferred.unit

module Reader = struct
  include Reader0

  let partition_map3 reader ~f =
    let (reader_a, writer_a), (reader_b, writer_b), (reader_c, writer_c) =
      (create Synchronous, create Synchronous, create Synchronous)
    in
    don't_wait_for
      (Reader0.iter reader ~f:(fun x ->
           match f x with
           | `Fst x ->
               Writer.write writer_a x
           | `Snd x ->
               Writer.write writer_b x
           | `Trd x ->
               Writer.write writer_c x )) ;
    don't_wait_for
      (let%map () = Pipe.closed reader_a.reader
       and () = Pipe.closed reader_b.reader
       and () = Pipe.closed reader_c.reader in
       Pipe.close_read reader.reader) ;
    reader.downstreams <- [reader_a; reader_b; reader_c] ;
    (reader_a, reader_b, reader_c)
end

let%test_module "Strict_pipe.Reader.Merge" =
  ( module struct
    let%test_unit "'iter' would filter out the closed pipes" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          let reader1, writer1 =
            create (Buffered (`Capacity 10, `Overflow Drop_head))
          in
          let reader2, writer2 =
            create (Buffered (`Capacity 10, `Overflow Drop_head))
          in
          Reader.Merge.iter [reader1; reader2] ~f:(fun _ -> Deferred.unit)
          |> don't_wait_for ;
          Writer.write writer1 1 ;
          Writer.write writer2 2 ;
          Writer.close writer1 ;
          let%map () = Async.after (Time.Span.of_ms 5.) in
          Writer.write writer2 3 ; () )
  end )

let%test_module "Strict_pipe.close" =
  ( module struct
    let%test_unit "'close' would close a writer" =
      let _, writer = create Synchronous in
      assert (not (Writer.is_closed writer)) ;
      Writer.close writer ;
      assert (Writer.is_closed writer)

    let%test_unit "'close' would close a writer" =
      let _, writer = create (Buffered (`Capacity 64, `Overflow Crash)) in
      assert (not (Writer.is_closed writer)) ;
      Writer.close writer ;
      assert (Writer.is_closed writer)

    let%test_unit "'close' would close the downstream pipes linked by 'map'" =
      let input_reader, input_writer = create Synchronous in
      assert (not (Writer.is_closed input_writer)) ;
      let output_reader = Reader.map ~f:Fn.id input_reader in
      assert (not (Reader.is_closed output_reader)) ;
      Writer.close input_writer ;
      assert (Writer.is_closed input_writer) ;
      assert (Reader.is_closed output_reader)

    let%test_unit "'close' would close the downstream pipes linked by \
                   'filter_map'" =
      let input_reader, input_writer = create Synchronous in
      assert (not (Writer.is_closed input_writer)) ;
      let output_reader =
        Reader.filter_map ~f:(Fn.const (Some 1)) input_reader
      in
      assert (not (Reader.is_closed output_reader)) ;
      Writer.close input_writer ;
      assert (Writer.is_closed input_writer) ;
      assert (Reader.is_closed output_reader)

    let%test_unit "'close' would close the downstream pipes linked by 'Fork'" =
      let input_reader, input_writer = create Synchronous in
      assert (not (Writer.is_closed input_writer)) ;
      let output_reader1, output_reader2 = Reader.Fork.two input_reader in
      assert (not (Reader.is_closed output_reader1)) ;
      assert (not (Reader.is_closed output_reader2)) ;
      Writer.close input_writer ;
      assert (Writer.is_closed input_writer) ;
      assert (Reader.is_closed output_reader1) ;
      assert (Reader.is_closed output_reader2)

    let%test_unit "'close' would close the downstream pipes linked by \
                   'partition_map3'" =
      let input_reader, input_writer = create Synchronous in
      assert (not (Writer.is_closed input_writer)) ;
      let output_reader1, output_reader2, output_reader3 =
        Reader.partition_map3 input_reader ~f:(fun _ -> `Fst 1)
      in
      assert (not (Reader.is_closed output_reader1)) ;
      assert (not (Reader.is_closed output_reader2)) ;
      assert (not (Reader.is_closed output_reader3)) ;
      Writer.close input_writer ;
      assert (Writer.is_closed input_writer) ;
      assert (Reader.is_closed output_reader1) ;
      assert (Reader.is_closed output_reader2) ;
      assert (Reader.is_closed output_reader3)

    let%test_unit "'close' would close the downstream pipes linked by \
                   'transfer'" =
      let input_reader, input_writer = create Synchronous
      and _, output_writer = create Synchronous in
      assert (not (Writer.is_closed input_writer)) ;
      assert (not (Writer.is_closed output_writer)) ;
      let (_ : unit Deferred.t) =
        transfer input_reader output_writer ~f:Fn.id
      in
      Writer.close input_writer ;
      assert (Writer.is_closed input_writer) ;
      assert (Writer.is_closed output_writer)
  end )
