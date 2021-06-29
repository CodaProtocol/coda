open Core
open Async

type password = Bytes.t Async.Deferred.t Lazy.t

let handle_open ~mkdir ~(f : string -> 'a Deferred.t) path =
  let corrupted_privkey = Privkey_error.corrupted_privkey in
  let open Unix.Error in
  let open Deferred.Result.Let_syntax in
  let dn = Filename.dirname path in
  let%bind parent_exists =
    let open Deferred.Let_syntax in
    match%bind
      Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
          let%bind stat = Unix.stat dn in
          Deferred.return
          @@
          if not (Unix.File_kind.equal stat.kind `Directory) then
            corrupted_privkey
              (Error.createf
                 "%s exists and it is not a directory, can't store files there"
                 dn)
          else Ok true )
    with
    | Ok x ->
        return x
    | Error (Unix.Unix_error (ENOENT, _, _)) ->
        Deferred.Result.return false
    | Error (Unix.Unix_error (e, _, _)) ->
        Deferred.return @@ corrupted_privkey
        @@ Error.createf
             !"could not stat %s: %s, not making keys\n"
             dn (message e)
    | Error e ->
        Deferred.return @@ corrupted_privkey (Error.of_exn e)
  in
  let%bind () =
    let open Deferred.Let_syntax in
    match%bind
      Monitor.try_with ~here:[%here] ~extract_exn:true (fun () ->
          if (not parent_exists) && mkdir then
            let%bind () = Unix.mkdir ~p:() dn in
            let%bind () = Unix.chmod dn ~perm:0o700 in
            Deferred.Result.return ()
          else if not parent_exists then
            Deferred.return (Error (`Parent_directory_does_not_exist dn))
          else Deferred.Result.return () )
    with
    | Ok x ->
        Deferred.return x
    | Error (Unix.Unix_error ((EACCES as e), _, _)) ->
        Deferred.return @@ corrupted_privkey
        @@ Error.createf "could not mkdir -p %s: %s\n" dn (message e)
    | Error e ->
        raise e
  in
  let open Deferred.Let_syntax in
  match%bind
    Deferred.Or_error.try_with ~here:[%here] ~extract_exn:true (fun () ->
        f path )
  with
  | Ok x ->
      Deferred.Result.return x
  | Error e -> (
    match Error.to_exn e with
    | Unix.Unix_error (_, _, _) ->
        Deferred.return (Error (`Cannot_open_file path))
    | e ->
        Deferred.return @@ corrupted_privkey (Error.of_exn e) )

let lift (t : 'a Deferred.t) : ('a, 'b) Deferred.Result.t = t >>| fun x -> Ok x

let write ~path ~mkdir ~(password : Bytes.t Deferred.t Lazy.t) ~plaintext =
  let open Deferred.Result.Let_syntax in
  let%bind privkey_f =
    handle_open ~mkdir ~f:(fun path -> Writer.open_file path) path
  in
  let%bind password = lift @@ Lazy.force password in
  let sb = Secret_box.encrypt ~plaintext ~password in
  let sb =
    Secret_box.to_yojson sb |> Yojson.Safe.to_string |> Bytes.of_string
  in
  Writer.write_bytes privkey_f sb ;
  let%bind () = lift (Unix.chmod path ~perm:0o600) in
  lift (Writer.close privkey_f)

let read ~path ~(password : Bytes.t Deferred.t Lazy.t) =
  let to_corrupt_privkey =
    Deferred.Result.map_error ~f:(fun e -> `Corrupted_privkey e)
  in
  let handle_open ~mkdir ~f p = handle_open ~mkdir ~f p in
  let open Deferred.Result.Let_syntax in
  let read_all r =
    lift (Pipe.to_list (Reader.lines r))
    >>| fun ss -> String.concat ~sep:"\n" ss
  in
  let%bind privkey_file = handle_open ~mkdir:false ~f:Reader.open_file path in
  let%bind st = handle_open ~mkdir:false ~f:Unix.stat path in
  let file_error =
    if st.perm land 0o077 <> 0 then
      Some
        (sprintf
           "insecure permissions on `%s`. They should be 0600, they are %o\n\
            Hint: chmod 600 %s\n"
           path (st.perm land 0o777) path)
    else None
  in
  let dn = Filename.dirname path in
  let%bind st = handle_open ~mkdir:false ~f:Unix.stat dn in
  let dir_error =
    if st.perm land 0o777 <> 0o700 then
      Some
        (sprintf
           "insecure permissions on `%s`. They should be 0700, they are %o\n\
            Hint: chmod 700 %s\n"
           dn (st.perm land 0o777) dn)
    else None
  in
  let%bind () =
    match (file_error, dir_error) with
    | Some e1, Some e2 ->
        Deferred.Or_error.error_string (e1 ^ e2) |> to_corrupt_privkey
    | Some e1, None | None, Some e1 ->
        Deferred.Or_error.error_string e1 |> to_corrupt_privkey
    | None, None ->
        Deferred.Result.return ()
  in
  let%bind file_contents = read_all privkey_file |> to_corrupt_privkey in
  let%bind sb =
    match Secret_box.of_yojson (Yojson.Safe.from_string file_contents) with
    | Ok sb ->
        return sb
    | Error e ->
        Deferred.return
          (Privkey_error.corrupted_privkey
             (Error.createf "couldn't parse %s: %s" path e))
  in
  let%bind password = lift (Lazy.force password) in
  Deferred.return (Secret_box.decrypt ~password sb)
