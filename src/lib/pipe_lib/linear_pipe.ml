open Core_kernel
open Async_kernel
module Writer = Pipe.Writer

module Reader = struct
  type 'a t = {pipe: 'a Pipe.Reader.t; mutable has_reader: bool}
end

let create () =
  let r, w = Pipe.create () in
  ({Reader.pipe= r; has_reader= false}, w)

let wrap_reader reader = {Reader.pipe= reader; has_reader= false}

let force_write_maybe_drop_head ~capacity writer reader x =
  if Pipe.length reader.Reader.pipe > capacity then
    ignore
      ( Pipe.read_now reader.Reader.pipe
        : [`Eof | `Nothing_available | `Ok of 'a] ) ;
  Pipe.write_without_pushback writer x

let create_reader ~close_on_exception f =
  let r = Pipe.create_reader ~close_on_exception f in
  {Reader.pipe= r; has_reader= false}

let write w x =
  ( if Pipe.is_closed w then
    let logger = Logger.create () in
    [%log warn] "writing to closed linear pipe" ~metadata:[] ) ;
  Pipe.write w x

let write_if_open = Pipe.write_if_open

let write_without_pushback = Pipe.write_without_pushback

let write_without_pushback_if_open = Pipe.write_without_pushback_if_open

exception Overflow

let write_or_exn ~capacity writer reader x =
  if Pipe.length reader.Reader.pipe > capacity then raise Overflow
  else Pipe.write_without_pushback writer x

let close_read (reader : 'a Reader.t) = Pipe.close_read reader.pipe

let close = Pipe.close

let closed (reader : 'a Reader.t) = Pipe.closed reader.pipe

let multiple_reads_error () =
  failwith
    "Linear_pipe.bracket: the same reader has been used multiple times. If \
     you want to rebroadcast the reader, use fork"

let bracket (reader : 'a Reader.t) dx =
  if reader.has_reader then multiple_reads_error ()
  else (
    reader.has_reader <- true ;
    let%map x = dx in
    reader.has_reader <- false ;
    x )

let set_has_reader (reader : 'a Reader.t) =
  if reader.has_reader then multiple_reads_error ()
  else reader.has_reader <- true

let iter ?flushed ?continue_on_error reader ~f =
  bracket reader (Pipe.iter reader.Reader.pipe ?flushed ?continue_on_error ~f)

let iter_unordered ?consumer ~max_concurrency reader ~f =
  bracket reader
    (let rec run_reader () =
       match%bind Pipe.read ?consumer reader.Reader.pipe with
       | `Eof ->
           return ()
       | `Ok v ->
           let%bind () = f v in
           run_reader ()
     in
     Deferred.all_unit (List.init max_concurrency ~f:(fun _ -> run_reader ())))

let drain r = iter r ~f:(fun _ -> Deferred.unit)

let length reader = Pipe.length reader.Reader.pipe

let of_list xs =
  let reader = wrap_reader (Pipe.of_list xs) in
  reader

let to_list reader = Pipe.to_list reader.Reader.pipe

let fold reader ~init ~f =
  bracket reader (Pipe.fold reader.Reader.pipe ~init ~f)

(* Adapted from Async_kernel's fold impl *)
let scan reader ~init ~f =
  set_has_reader reader ;
  let r, w = Pipe.create () in
  let rec loop b =
    match Pipe.read_now reader.Reader.pipe with
    | `Eof ->
        return (Pipe.close w)
    | `Ok v ->
        let%bind next = f b v in
        let%bind () = Pipe.write w next in
        loop next
    | `Nothing_available ->
        let%bind _ = Pipe.values_available reader.Reader.pipe in
        loop b
  in
  don't_wait_for
    ( (* Force async ala https://github.com/janestreet/async_kernel/blob/master/src/pipe.ml#L703 *)
      return ()
    >>= fun () -> loop init ) ;
  wrap_reader r

let map (reader : 'a Reader.t) ~f =
  set_has_reader reader ;
  wrap_reader (Pipe.map reader.Reader.pipe ~f)

let filter_map (reader : 'a Reader.t) ~f =
  set_has_reader reader ;
  wrap_reader (Pipe.filter_map reader.Reader.pipe ~f)

let transfer reader writer ~f =
  bracket reader (Pipe.transfer reader.Reader.pipe writer ~f)

let transfer_id reader writer =
  bracket reader (Pipe.transfer_id reader.Reader.pipe writer)

let merge_unordered rs =
  let merged_reader, merged_writer = create () in
  List.iter rs ~f:(fun reader ->
      don't_wait_for (iter reader ~f:(fun x -> Pipe.write merged_writer x)) ) ;
  don't_wait_for
    (let%map () = Deferred.List.iter rs ~f:closed in
     Pipe.close merged_writer) ;
  merged_reader

(* TODO following are all more efficient with iter',
 * but I get write' doesn't exist on my version of ocaml *)

let fork reader n =
  let pipes = List.init n ~f:(fun _ -> create ()) in
  let writers = List.map pipes ~f:(fun (_, w) -> w) in
  let readers = List.map pipes ~f:(fun (r, _) -> r) in
  don't_wait_for
    (iter reader ~f:(fun x ->
         Deferred.List.iter writers ~f:(fun writer ->
             if not (Pipe.is_closed writer) then Pipe.write writer x
             else return () ) )) ;
  don't_wait_for
    (let%map () = Deferred.List.iter readers ~f:closed in
     close_read reader) ;
  readers

let fork2 reader =
  match fork reader 2 with [x; y] -> (x, y) | _ -> assert false

let fork3 reader =
  match fork reader 3 with [x; y; z] -> (x, y, z) | _ -> assert false

let fork4 reader =
  match fork reader 4 with [x; y; z; w] -> (x, y, z, w) | _ -> assert false

let fork5 reader =
  match fork reader 5 with
  | [x; y; z; w; v] ->
      (x, y, z, w, v)
  | _ ->
      assert false

let fork6 reader =
  match fork reader 6 with
  | [x; y; z; w; v; u] ->
      (x, y, z, w, v, u)
  | _ ->
      assert false

let partition_map2 reader ~f =
  let (reader_a, writer_a), (reader_b, writer_b) = (create (), create ()) in
  don't_wait_for
    (iter reader ~f:(fun x ->
         match f x with
         | `Fst x ->
             Pipe.write writer_a x
         | `Snd x ->
             Pipe.write writer_b x )) ;
  don't_wait_for
    (let%map () = closed reader_a and () = closed reader_b in
     close_read reader) ;
  (reader_a, reader_b)

let partition_map3 reader ~f =
  let (reader_a, writer_a), (reader_b, writer_b), (reader_c, writer_c) =
    (create (), create (), create ())
  in
  don't_wait_for
    (iter reader ~f:(fun x ->
         match f x with
         | `Fst x ->
             Pipe.write writer_a x
         | `Snd x ->
             Pipe.write writer_b x
         | `Trd x ->
             Pipe.write writer_c x )) ;
  don't_wait_for
    (let%map () = closed reader_a
     and () = closed reader_b
     and () = closed reader_c in
     close_read reader) ;
  (reader_a, reader_b, reader_c)

let filter_map_unordered ~max_concurrency t ~f =
  let reader, writer = create () in
  don't_wait_for
    (iter_unordered ~max_concurrency t ~f:(fun x ->
         match%bind f x with
         | Some y ->
             Pipe.write writer y
         | None ->
             return () )) ;
  don't_wait_for
    (let%map () = closed reader in
     close_read t) ;
  reader

let latest_ref t ~initial =
  let cell = ref initial in
  don't_wait_for (iter t ~f:(fun a -> return (cell := a))) ;
  cell

let values_available ({pipe; _} : 'a Reader.t) = Pipe.values_available pipe

let peek ({pipe; _} : 'a Reader.t) = Pipe.peek pipe

let release_has_reader (reader : 'a Reader.t) =
  if not reader.has_reader then
    failwith "Linear_pipe.bracket: did not have reader"
  else reader.has_reader <- false

let read_now reader =
  set_has_reader reader ;
  let res = Pipe.read_now reader.pipe in
  release_has_reader reader ; res

let read' ?max_queue_length ({pipe; _} : 'a Reader.t) =
  Pipe.read' ?max_queue_length pipe

let read ({pipe; _} : 'a Reader.t) = Pipe.read pipe

let read_exn reader =
  match%map read reader with
  | `Eof ->
      failwith "Expecting a value from reader"
  | `Ok value ->
      value
