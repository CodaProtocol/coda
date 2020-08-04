open Core

module Spec = struct
  type t =
    | On_disk of {directory: string; should_write: bool}
    | S3 of {bucket_prefix: string; install_path: string}
end

module T (M : sig
  type _ t
end) =
struct
  type ('a, 'b) t = {write: 'a -> 'b -> unit M.t; read: 'a -> 'b M.t}
end

module Disk_storable (M : sig
  type _ t
end) =
struct
  type ('k, 'v) t =
    { to_string: 'k -> string
    ; read: 'k -> path:string -> 'v M.t
    ; write: 'v -> string -> unit M.t }
end

module type S = sig
  module M : sig
    type _ t
  end

  type ('a, 'b) t = ('a, 'b) T(M).t =
    {write: 'a -> 'b -> unit M.t; read: 'a -> 'b M.t}

  module Disk_storable : sig
    type ('k, 'v) t = ('k, 'v) Disk_storable(M).t =
      { to_string: 'k -> string
      ; read: 'k -> path:string -> 'v M.t
      ; write: 'v -> string -> unit M.t }

    val of_binable :
      ('k -> string) -> (module Binable.S with type t = 'v) -> ('k, 'v) t

    val simple :
         ('k -> string)
      -> ('k -> path:string -> 'v)
      -> ('v -> string -> unit)
      -> ('k, 'v) t
  end

  val read :
       Spec.t list
    -> ('k, 'v) Disk_storable.t
    -> 'k
    -> ('v * [> `Cache_hit | `Locally_generated]) M.t

  val write : Spec.t list -> ('k, 'v) Disk_storable.t -> 'k -> 'v -> unit M.t
end

module Sync : S with module M := Or_error = struct
  include T (Or_error)

  let on_disk to_string read write prefix =
    let path k = prefix ^/ to_string k in
    let read k =
      let p = path k in
      match Sys.file_exists p with
      | `No | `Unknown ->
          Or_error.errorf "file %s does not exist or cannot be read" p
      | `Yes ->
          read k ~path:p
    in
    let write key v =
      match Sys.is_directory prefix with
      | `No | `Unknown ->
          Or_error.errorf "directory %s does not exist or cannot be read"
            prefix
      | `Yes ->
          write v (path key)
    in
    {read; write}

  let s3 to_string read ~bucket_prefix ~install_path =
    let read k =
      let label = to_string k in
      let uri_string = bucket_prefix ^/ label in
      let file_path = install_path ^/ label in
      let open Or_error.Let_syntax in
      let%bind () =
        Result.map_error
          (ksprintf Unix.system "curl --fail -o \"%s\" \"%s\"" file_path
             uri_string) ~f:(function
          | `Exit_non_zero _ as e ->
              Error.of_string (Unix.Exit.to_string_hum (Error e))
          | `Signal s ->
              Error.createf "died after receiving %s (signal number %d)"
                (Signal.to_string s) (Signal.to_system_int s) )
      in
      let logger = Logger.create () in
      [%log debug] "Curl finished"
        ~metadata:
          [("url", `String uri_string); ("local_file_path", `String file_path)] ;
      read k ~path:file_path
    in
    let write _ _ = Or_error.return () in
    {read; write}

  module Disk_storable = struct
    include Disk_storable (Or_error)

    let of_binable to_string m =
      (* TODO: Make more efficient *)
      let read _ ~path =
        Or_error.try_with (fun () ->
            Binable.of_string m (In_channel.read_all path) )
      in
      let write t path =
        Or_error.try_with (fun () ->
            Out_channel.write_all path ~data:(Binable.to_string m t) )
      in
      {to_string; read; write}

    let simple to_string read write =
      { to_string
      ; read= (fun k ~path -> Or_error.return (read k ~path))
      ; write= (fun v s -> Or_error.return (write v s)) }
  end

  let read spec {Disk_storable.to_string; read= r; write= w} k =
    let errs = ref [] in
    match
      List.find_mapi spec ~f:(fun i s ->
          let res =
            match s with
            | Spec.On_disk {directory; _} ->
                (on_disk to_string r w directory).read k
            | S3 {bucket_prefix; install_path} ->
                Unix.mkdir_p install_path ;
                (s3 to_string r ~bucket_prefix ~install_path).read k
          in
          match res with
          | Error e ->
              errs := e :: !errs ;
              None
          | Ok x ->
              Some (i, x) )
    with
    | Some (i, x) ->
        Ok (x, if i = 0 then `Cache_hit else `Locally_generated)
    | None ->
        Error (Error.of_list !errs)

  let write spec {Disk_storable.to_string; read= r; write= w} k v =
    let errs =
      List.filter_map spec ~f:(fun s ->
          let res =
            match s with
            | Spec.On_disk {directory; should_write} ->
                if should_write then (
                  Unix.mkdir_p directory ;
                  (on_disk to_string r w directory).write k v )
                else Or_error.return ()
            | S3 {bucket_prefix= _; install_path= _} ->
                Or_error.return ()
          in
          match res with Error e -> Some e | Ok () -> None )
    in
    match errs with [] -> Ok () | errs -> Error (Error.of_list errs)
end

module Async : S with module M := Async.Deferred.Or_error = struct
  open Async
  include T (Deferred.Or_error)

  let on_disk to_string read write prefix =
    let path k = prefix ^/ to_string k in
    let read k =
      let p = path k in
      match%bind Sys.file_exists p with
      | `No | `Unknown ->
          return (Or_error.errorf "file %s does not exist or cannot be read" p)
      | `Yes ->
          read k ~path:p
    in
    let write key v =
      match%bind Sys.is_directory prefix with
      | `No | `Unknown ->
          return
            (Or_error.errorf "directory %s does not exist or cannot be read"
               prefix)
      | `Yes ->
          write v (path key)
    in
    {read; write}

  let s3 to_string read ~bucket_prefix ~install_path =
    let read k =
      let label = to_string k in
      let uri_string = bucket_prefix ^/ label in
      let file_path = install_path ^/ label in
      let open Deferred.Or_error.Let_syntax in
      let logger = Logger.create () in
      [%log debug] "Running curl"
        ~metadata:
          [("url", `String uri_string); ("local_file_path", `String file_path)] ;
      let%bind result =
        Process.run ~prog:"curl"
          ~args:["--fail"; "-o"; file_path; uri_string]
          ()
      in
      [%log debug] "Curl finished"
        ~metadata:
          [ ("url", `String uri_string)
          ; ("local_file_path", `String file_path)
          ; ("result", `String result) ] ;
      read k ~path:file_path
    in
    let write _ _ = Deferred.Or_error.return () in
    {read; write}

  module Disk_storable = struct
    include Disk_storable (Deferred.Or_error)

    let of_binable (type t) to_string (module B : Binable.S with type t = t) =
      let read _ ~path = Reader.load_bin_prot path B.bin_reader_t in
      let write t path =
        Deferred.map
          (Writer.save_bin_prot path B.bin_writer_t t)
          ~f:Or_error.return
      in
      {to_string; read; write}

    let simple to_string read write =
      { to_string
      ; read= (fun k ~path -> Deferred.Or_error.return (read k ~path))
      ; write= (fun v s -> Deferred.Or_error.return (write v s)) }
  end

  let read spec {Disk_storable.to_string; read= r; write= w} k =
    let errs = ref [] in
    match%map
      Deferred.List.find_mapi spec ~f:(fun i s ->
          let res =
            match s with
            | Spec.On_disk {directory; _} ->
                (on_disk to_string r w directory).read k
            | S3 {bucket_prefix; install_path} ->
                (s3 to_string r ~bucket_prefix ~install_path).read k
          in
          match%map res with
          | Error e ->
              errs := e :: !errs ;
              None
          | Ok x ->
              Some (i, x) )
    with
    | Some (i, x) ->
        Ok (x, if i = 0 then `Cache_hit else `Locally_generated)
    | None ->
        Error (Error.of_list !errs)

  let write spec {Disk_storable.to_string; read= r; write= w} k v =
    let%map errs =
      Deferred.List.filter_map spec ~f:(fun s ->
          let res =
            match s with
            | Spec.On_disk {directory; should_write} ->
                if should_write then
                  let%bind () = Unix.mkdir ~p:() directory in
                  (on_disk to_string r w directory).write k v
                else Deferred.Or_error.return ()
            | S3 {bucket_prefix= _; install_path= _} ->
                Deferred.Or_error.return ()
          in
          match%map res with Error e -> Some e | Ok () -> None )
    in
    match errs with [] -> Ok () | errs -> Error (Error.of_list errs)
end
