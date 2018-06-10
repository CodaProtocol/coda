open Core
open Async

type 'a t =
  { path     : string
  ; value    : 'a
  ; checksum : Md5.t
  }

let create ~directory ~digest_input ~bin_t f input =
  let%bind () = Unix.mkdir ~p:() directory in
  let hash = digest_input input in
  let path = directory ^/ hash in
  let open Storage.Disk in
  let controller = Controller.create ~parent_log:(Logger.create ()) bin_t in
  match%bind load_with_checksum controller path with
  | Error `Checksum_no_match ->
    failwith "Checksum failure"
  | Error ((`IO_error _ | `No_exist) as err) ->
    begin match err with
    | `IO_error e -> Core.printf "Cached error: %s\n%!" (Error.to_string_hum e)
    | `No_exist -> Core.printf "Not found\n%!"
    end;
    let value = f input in
    let%map checksum = store_with_checksum controller path value in
    { path; value; checksum }
  | Ok { data; checksum } ->
    Core.printf "All ok!\n%!";
    return { path; value=data; checksum }
