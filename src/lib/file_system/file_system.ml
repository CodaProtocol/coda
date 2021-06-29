open Core
open Async

let dir_exists dir =
  let%bind access_res = Unix.access dir [`Exists] in
  if Result.is_ok access_res then
    let%map stat = Unix.stat dir in
    Unix.File_kind.equal (Unix.Stats.kind stat) `Directory
  else return false

let remove_dir dir =
  let%bind _ = Process.run_exn ~prog:"rm" ~args:["-rf"; dir] () in
  Deferred.unit

let rec rmrf path =
  match Core.Sys.is_directory path with
  | `Yes ->
      Core.Sys.readdir path
      |> Array.iter ~f:(fun name -> rmrf (Filename.concat path name)) ;
      Core.Unix.rmdir path
  | _ ->
      if [%equal: [`Yes | `No | `Unknown]] (Core.Sys.file_exists path) `Yes
      then Core.Sys.remove path

let try_finally ~(f : unit -> 'a Deferred.t)
    ~(finally : unit -> unit Deferred.t) =
  try_with f
  >>= function
  | Ok x ->
      Deferred.map (finally ()) ~f:(Fn.const x)
  | Error exn ->
      finally () >>= fun () -> raise exn

let with_temp_dir ~f dir =
  let%bind temp_dir = Async.Unix.mkdtemp dir in
  try_finally ~f:(fun () -> f temp_dir) ~finally:(fun () -> remove_dir temp_dir)

let dup_stdout ?(f = Core.Fn.id) (process : Process.t) =
  Pipe.transfer ~f
    (Reader.pipe @@ Process.stdout process)
    (Writer.pipe @@ Lazy.force Writer.stdout)
  |> don't_wait_for

let dup_stderr ?(f = Core.Fn.id) (process : Process.t) =
  Pipe.transfer ~f
    (Reader.pipe @@ Process.stderr process)
    (Writer.pipe @@ Lazy.force Writer.stderr)
  |> don't_wait_for

let clear_dir toplevel_dir =
  let rec all_files dirname basename =
    let fullname = Filename.concat dirname basename in
    match%bind Sys.is_directory fullname with
    | `Yes ->
        let%map dirs, files =
          Sys.ls_dir fullname
          >>= Deferred.List.map ~f:(all_files fullname)
          >>| List.unzip
        in
        let dirs =
          if String.equal dirname toplevel_dir then List.concat dirs
          else List.append (List.concat dirs) [fullname]
        in
        (dirs, List.concat files)
    | _ ->
        Deferred.return ([], [fullname])
  in
  let%bind dirs, files = all_files toplevel_dir "" in
  let%bind () = Deferred.List.iter files ~f:(fun file -> Sys.remove file) in
  Deferred.List.iter dirs ~f:(fun file -> Unix.rmdir file)

let create_dir ?(clear_if_exists = false) dir =
  match Core.Sys.file_exists dir with
  | `Yes ->
      if clear_if_exists then clear_dir dir else return ()
  | _ ->
      return (Core.Unix.mkdir_p dir)
