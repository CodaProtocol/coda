open Core
open Async

let autogen_path = Filename.temp_dir_name ^/ "coda_cache_dir"

let s3_install_path = "/tmp/s3_cache_dir"

let s3_keys_bucket_prefix =
  "https://s3-us-west-2.amazonaws.com/snark-keys.o1test.net"

let manual_install_path = "/var/lib/coda"

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

let cache =
  let dir d w = Key_cache.Spec.On_disk { directory = d; should_write = w } in
  [ dir manual_install_path false
  ; dir brew_install_path false
  ; dir s3_install_path false
  ; dir autogen_path true
  ; Key_cache.Spec.S3
      { bucket_prefix = s3_keys_bucket_prefix; install_path = s3_install_path }
  ]

let env_path =
  match Sys.getenv "MINA_KEYS_PATH" with
  | Some path ->
      path
  | None ->
      manual_install_path

let possible_paths base =
  List.map
    [ env_path
    ; brew_install_path
    ; s3_install_path
    ; autogen_path
    ; manual_install_path
    ] ~f:(fun d -> d ^/ base)

let load_from_s3 s3_bucket_prefix s3_install_path ~logger =
  Deferred.map ~f:Result.join
  @@ Monitor.try_with ~here:[%here] (fun () ->
         let each_uri (uri_string, file_path) =
           let open Deferred.Let_syntax in
           [%log trace] "Downloading file from S3"
             ~metadata:
               [ ("url", `String uri_string)
               ; ("local_file_path", `String file_path)
               ] ;
           let%map _result =
             Process.run_exn ~prog:"curl"
               ~args:
                 [ "--fail"
                 ; "--silent"
                 ; "--show-error"
                 ; "-o"
                 ; file_path
                 ; uri_string
                 ]
               ()
           in
           [%log trace] "Download finished"
             ~metadata:
               [ ("url", `String uri_string)
               ; ("local_file_path", `String file_path)
               ] ;
           Result.return ()
         in
         Deferred.List.map ~f:each_uri
           (List.zip_exn s3_bucket_prefix s3_install_path)
         |> Deferred.map ~f:Result.all_unit)
  |> Deferred.Result.map_error ~f:Error.of_exn
