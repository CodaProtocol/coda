open Core
open Async

(* If OCamlformat ever breaks on any files add their paths here *)
let trustlist = []

let dirs_trustlist =
  [ (* `.git` is NOT 'ignored' by git - it is 'un-addable', and therefore must be excluded explicitly *)
    ".git"
  ; "stationary"
  ; ".un~"
  ; "frontend"
  ; "external"
  ; "ocamlformat"
  ; "tablecloth"
  ; "zexe"
  ; "marlin"
  ; "snarky" ]

let git_ignored (filepath : Filename.t) : bool Deferred.t =
  match%map Sys.command @@ "git check-ignore -q " ^ Sys.quote filepath with
  | 0 ->
      true
  | 1 ->
      false
  | _ ->
      failwith @@ "Unexpected error occurred @ " ^ __LOC__

let rec fold_over_files ~path ~process_path ~init ~f =
  let%bind all = Sys.ls_dir path in
  Deferred.List.fold all ~init ~f:(fun acc x ->
      match%bind Sys.is_directory (path ^/ x) with
      | `Yes ->
          if%bind process_path `Dir (path ^/ x) then
            fold_over_files ~path:(path ^/ x) ~process_path ~init:acc ~f
          else return acc
      | _ ->
          if%bind process_path `File (path ^/ x) then f acc (path ^/ x)
          else return acc )

(* checks if the provided string ends with any of the provided suffixes. *)
let exists_suffix suffixes path =
  List.exists suffixes ~f:(fun s -> String.is_suffix ~suffix:s path)

let is_trusted_filepath : [< `Dir | `File] -> Filename.t -> bool = function
  | `Dir ->
      exists_suffix dirs_trustlist
  | `File ->
      exists_suffix trustlist

let has_ocaml_file_ext = exists_suffix [".ml"; ".mli"]

let process_path kind path =
  let%map ignored = git_ignored path in
  if ignored || is_trusted_filepath kind path then false
  else match kind with `Dir -> true | `File -> has_ocaml_file_ext path

let main dry_run check path =
  let%bind _all =
    fold_over_files ~path ~init:() ~process_path ~f:(fun () file ->
        let dump prog args =
          printf !"%s %{sexp: string List.t}\n" prog args ;
          return ()
        in
        if check then
          let prog, args = ("ocamlformat", ["--doc-comments=before"; file]) in
          let%bind formatted = Process.run_exn ~prog ~args () in
          let%bind raw = Reader.file_contents file in
          if not (String.equal formatted raw) then (
            eprintf "File: %s has needs to be ocamlformat-ed\n" file ;
            exit 1 )
          else return ()
        else
          let prog, args =
            ("ocamlformat", ["--doc-comments=before"; "-i"; file])
          in
          if dry_run then dump prog args
          else
            let%map _stdout = Process.run_exn ~prog ~args () in
            () )
  in
  exit 0

let cli =
  let open Command.Let_syntax in
  Command.async ~summary:"Format all ml and mli files"
    (let%map_open path =
       flag "--path" ~aliases:["path"] ~doc:"Path to traverse"
         (required string)
     and dry_run = flag "--dry-run" ~aliases:["dry-run"] no_arg ~doc:"Dry run"
     and check =
       flag "--check" ~aliases:["check"] no_arg
         ~doc:
           "Return with an error code if there exists an ml file that was \
            formatted improperly"
     in
     fun () -> main dry_run check path)
  |> Command.run

let () = never_returns (Scheduler.go ())
