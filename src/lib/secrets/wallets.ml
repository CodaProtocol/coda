open Core
open Async
module Secret_keypair = Keypair
open Signature_lib

(** The string is the filename of the secret key file *)
type locked_key =
  | Locked of string
  | Unlocked of (string * Keypair.t)
  | Hd_account of Mina_numbers.Hd_index.t

(* A simple cache on top of the fs *)
type t = {cache: locked_key Public_key.Compressed.Table.t; path: string}

let get_privkey_filename public_key =
  Public_key.Compressed.to_base58_check public_key

let get_path {path; cache} public_key =
  (* TODO: Do we need to version this? *)
  let filename =
    Public_key.Compressed.Table.find cache public_key
    |> Option.bind ~f:(function
         | Locked file | Unlocked (file, _) ->
             Option.return file
         | Hd_account _ ->
             Option.return
               (Public_key.Compressed.to_base58_check public_key ^ ".index") )
    |> Option.value ~default:(get_privkey_filename public_key)
  in
  path ^/ filename

let decode_public_key key file path logger =
  match
    Or_error.try_with (fun () -> Public_key.of_base58_check_decompress_exn key)
  with
  | Ok pk ->
      Some pk
  | Error e ->
      [%log error] "Error decoding public key at $path/$file: $error"
        ~metadata:
          [ ("file", `String file)
          ; ("path", `String path)
          ; ("error", Error_json.error_to_yojson e) ] ;
      None

let reload ~logger {cache; path} : unit Deferred.t =
  let logger =
    Logger.extend logger [("wallets_context", `String "Wallets.get")]
  in
  Public_key.Compressed.Table.clear cache ;
  let%bind () = File_system.create_dir path in
  let%bind files = Sys.readdir path >>| Array.to_list in
  let%bind () =
    Deferred.List.iter files ~f:(fun file ->
        match String.chop_suffix file ~suffix:".pub" with
        | Some sk_filename -> (
            let%map lines = Reader.file_lines (path ^/ file) in
            match lines with
            | public_key :: _ ->
                decode_public_key public_key file path logger
                |> Option.iter ~f:(fun pk ->
                       ignore
                       @@ Public_key.Compressed.Table.add cache ~key:pk
                            ~data:(Locked sk_filename) )
            | _ ->
                () )
        | None -> (
          match String.chop_suffix file ~suffix:".index" with
          | Some public_key -> (
              let%map lines = Reader.file_lines (path ^/ file) in
              match lines with
              | hd_index :: _ ->
                  decode_public_key public_key file path logger
                  |> Option.iter ~f:(fun pk ->
                         ignore
                         @@ Public_key.Compressed.Table.add cache ~key:pk
                              ~data:
                                (Hd_account
                                   (Mina_numbers.Hd_index.of_string hd_index))
                     )
              | _ ->
                  () )
          | None ->
              return () ) )
  in
  Unix.chmod path ~perm:0o700

let load ~logger ~disk_location =
  let t =
    { cache= Public_key.Compressed.Table.create ()
    ; path= disk_location ^/ "store" }
  in
  let%map () = reload ~logger t in
  t

let import_keypair_helper t keypair write_keypair =
  let compressed_pk = Public_key.compress keypair.Keypair.public_key in
  let privkey_path = get_path t compressed_pk in
  let%bind () = write_keypair privkey_path in
  let%map () = Unix.chmod privkey_path ~perm:0o600 in
  Public_key.Compressed.Table.add t.cache ~key:compressed_pk
    ~data:(Unlocked (get_privkey_filename compressed_pk, keypair))
  |> ignore ;
  compressed_pk

let import_keypair t keypair ~password =
  import_keypair_helper t keypair (fun privkey_path ->
      Secret_keypair.write_exn keypair ~privkey_path ~password )

let import_keypair_terminal_stdin t keypair =
  import_keypair_helper t keypair (fun privkey_path ->
      Secret_keypair.Terminal_stdin.write_exn keypair ~privkey_path )

(** Generates a new private key file and a keypair *)
let generate_new t ~password : Public_key.Compressed.t Deferred.t =
  let keypair = Keypair.create () in
  import_keypair t keypair ~password

let create_hd_account t ~hd_index :
    (Public_key.Compressed.t, string) Deferred.Result.t =
  let open Deferred.Result.Let_syntax in
  let%bind public_key = Hardware_wallets.compute_public_key ~hd_index in
  let compressed_pk = Public_key.compress public_key in
  let index_path =
    t.path ^/ Public_key.Compressed.to_base58_check compressed_pk ^ ".index"
  in
  let%bind () =
    Hardware_wallets.write_exn ~hd_index ~index_path
    |> Deferred.map ~f:Result.return
  in
  let%map () =
    Unix.chmod index_path ~perm:0o600 |> Deferred.map ~f:Result.return
  in
  Public_key.Compressed.Table.add t.cache ~key:compressed_pk
    ~data:(Hd_account hd_index)
  |> ignore ;
  compressed_pk

let delete ({cache; _} as t : t) (pk : Public_key.Compressed.t) :
    (unit, [`Not_found]) Deferred.Result.t =
  Hashtbl.remove cache pk ;
  Deferred.Or_error.try_with ~here:[%here] (fun () ->
      Unix.remove (get_path t pk) )
  |> Deferred.Result.map_error ~f:(fun _ -> `Not_found)

let pks ({cache; _} : t) = Public_key.Compressed.Table.keys cache

let find_unlocked ({cache; _} : t) ~needle =
  Public_key.Compressed.Table.find cache needle
  |> Option.bind ~f:(function
       | Locked _ ->
           None
       | Unlocked (_, kp) ->
           Some kp
       | Hd_account _ ->
           None )

let find_identity ({cache; _} : t) ~needle =
  Public_key.Compressed.Table.find cache needle
  |> Option.bind ~f:(function
       | Locked _ ->
           None
       | Unlocked (_, kp) ->
           Some (`Keypair kp)
       | Hd_account index ->
           Some (`Hd_index index) )

let check_locked {cache; _} ~needle =
  Public_key.Compressed.Table.find cache needle
  |> Option.map ~f:(function
       | Locked _ ->
           true
       | Unlocked _ ->
           false
       | Hd_account _ ->
           true )

let unlock {cache; path} ~needle ~password =
  let unlock_keypair = function
    | Locked file ->
        Secret_keypair.read ~privkey_path:(path ^/ file) ~password
        |> Deferred.Result.map_error ~f:(fun _ -> `Bad_password)
        |> Deferred.Result.map ~f:(fun kp ->
               Public_key.Compressed.Table.set cache ~key:needle
                 ~data:(Unlocked (file, kp)) )
        |> Deferred.Result.ignore
    | Unlocked _ ->
        Deferred.Result.return ()
    | Hd_account _ ->
        Deferred.Result.return ()
  in
  Public_key.Compressed.Table.find cache needle
  |> Result.of_option ~error:`Not_found
  |> Deferred.return
  |> Deferred.Result.bind ~f:unlock_keypair

let lock {cache; _} ~needle =
  Public_key.Compressed.Table.change cache needle ~f:(function
    | Some (Unlocked (file, _)) ->
        Some (Locked file)
    | k ->
        k )

let get_tracked_keypair ~logger ~which ~read_from_env_exn ~conf_dir pk =
  let%bind wallets = load ~logger ~disk_location:(conf_dir ^/ "wallets") in
  let sk_file = get_path wallets pk in
  read_from_env_exn ~logger ~which sk_file

let%test_module "wallets" =
  ( module struct
    let logger = Logger.create ()

    let password = lazy (Deferred.return (Bytes.of_string ""))

    module Set = Public_key.Compressed.Set

    let%test_unit "get from scratch" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          File_system.with_temp_dir "/tmp/coda-wallets-test" ~f:(fun path ->
              let%bind wallets = load ~logger ~disk_location:path in
              let%map pk = generate_new wallets ~password in
              let keys = Set.of_list (pks wallets) in
              assert (Set.mem keys pk) ;
              assert (find_unlocked wallets ~needle:pk |> Option.is_some) ) )

    let%test_unit "get from existing file system not-scratch" =
      Backtrace.elide := false ;
      Async.Thread_safe.block_on_async_exn (fun () ->
          File_system.with_temp_dir "/tmp/coda-wallets-test" ~f:(fun path ->
              let%bind wallets = load ~logger ~disk_location:path in
              let%bind pk1 = generate_new wallets ~password in
              let%bind pk2 = generate_new wallets ~password in
              let keys = Set.of_list (pks wallets) in
              assert (Set.mem keys pk1 && Set.mem keys pk2) ;
              (* Get wallets again from scratch *)
              let%map wallets = load ~logger ~disk_location:path in
              let keys = Set.of_list (pks wallets) in
              assert (Set.mem keys pk1 && Set.mem keys pk2) ) )

    let%test_unit "create wallet then delete it" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          File_system.with_temp_dir "/tmp/coda-wallets-test" ~f:(fun path ->
              let%bind wallets = load ~logger ~disk_location:path in
              let%bind pk = generate_new wallets ~password in
              let keys = Set.of_list (pks wallets) in
              assert (Set.mem keys pk) ;
              match%map delete wallets pk with
              | Ok () ->
                  assert (
                    Option.is_none
                    @@ Public_key.Compressed.Table.find wallets.cache pk )
              | Error _ ->
                  failwith "unexpected" ) )

    let%test_unit "Unable to find wallet" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          File_system.with_temp_dir "/tmp/coda-wallets-test" ~f:(fun path ->
              let%bind wallets = load ~logger ~disk_location:path in
              let keypair = Keypair.create () in
              let%map result =
                delete wallets (Public_key.compress @@ keypair.public_key)
              in
              assert (Result.is_error result) ) )
  end )
