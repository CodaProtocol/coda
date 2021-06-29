open Core
open Async
module Location = String

type location = Location.t [@@deriving sexp]

type 'a t = 'a Checked_data.t

module Controller = struct
  type nonrec 'a t = {logger: Logger.t; tc: 'a Binable.m}

  let create ~logger tc = {logger; tc}
end

let load_with_checksum (type a) (c : a Controller.t) location =
  match%bind Sys.file_exists location with
  | `Yes -> (
      match%map
        Reader.load_bin_prot
          ~max_len:(5 * 512 * 1024 * 1024 (* 2.5 GB *))
          location
          (Checked_data.Stable.Latest.bin_reader_t String.bin_reader_t)
      with
      | Ok t ->
          if Checked_data.valid t then
            Ok
              Checked_data.
                {checksum= t.checksum; data= Binable.of_string c.tc t.data}
          else Error `Checksum_no_match
      | Error e ->
          Error (`IO_error e) )
  | `No | `Unknown ->
      return (Error `No_exist)

let load c location =
  Deferred.Result.map (load_with_checksum c location) ~f:(fun t -> t.data)

let atomic_write (c : 'a Controller.t) location data =
  let temp_location = location ^ ".temp" in
  let t = Checked_data.wrap c.tc data in
  let%bind () =
    Writer.save_bin_prot temp_location
      (Checked_data.Stable.Latest.bin_writer_t String.bin_writer_t)
      t
  in
  let%map () = Sys.rename temp_location location in
  t

let store_with_checksum (type a) (c : a Controller.t) location (data : a) =
  atomic_write c location data >>| fun t -> t.Checked_data.checksum

let store c location data = atomic_write c location data |> Deferred.ignore_m
