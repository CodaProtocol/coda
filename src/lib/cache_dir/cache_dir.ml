open Core
open Async

let autogen_path = Filename.temp_dir_name ^/ "coda_cache_dir"

let s3_install_path = "/tmp/s3_cache_dir"

let manual_install_path = "/var/lib/coda"

let genesis_state_path dir = dir ^/ "genesis"

let brew_install_path =
  match
    let p = Core.Unix.open_process_in "brew --prefix 2>/dev/null" in
    let r = In_channel.input_lines p in
    (r, Core.Unix.close_process_in p)
  with
  | brew :: _, Ok () ->
      brew ^ "/var/coda"
  | _ ->
      "/usr/local/var/coda"

let possible_paths base =
  List.map
    [manual_install_path; brew_install_path; s3_install_path; autogen_path]
    ~f:(fun d -> d ^/ base)

let load_from_s3 s3_bucket_prefix s3_install_path =
  Deferred.map ~f:Result.join
  @@ Monitor.try_with (fun () ->
         let each_uri (uri_string, file_path) =
           let open Deferred.Let_syntax in
           let%map result =
             Process.run_exn ~prog:"curl"
               ~args:["-o"; file_path; uri_string]
               ()
           in
           Core_kernel.printf !"Curl finished: %s\n" result ;
           Result.return ()
         in
         Deferred.List.map ~f:each_uri
           (List.zip_exn s3_bucket_prefix s3_install_path)
         |> Deferred.map ~f:Result.all_unit )
  |> Deferred.Result.map_error ~f:Error.of_exn
