open Core_kernel
open Async

let time ~label f =
  let start = Time.now () in
  let%map x = f () in
  let stop = Time.now () in
  [%log' info (Logger.create ())]
    "%s took %s" label
    (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

let default_missing_blocks_width = 2000

module Max_block_height = struct
  let query =
    Caqti_request.find Caqti_type.unit Caqti_type.int
      "SELECT GREATEST(0, MAX(height)) FROM blocks"

  let update (module Conn : Caqti_async.CONNECTION) metric_server =
    time ~label:"max_block_height" (fun () ->
        let open Deferred.Result.Let_syntax in
        let%map max_height = Conn.find query () in
        Mina_metrics.(
          Gauge.set
            (Archive.max_block_height metric_server)
            (Float.of_int max_height)))
end

module Missing_blocks = struct
  (*A block is missing if there is no entry for a specific height. However, if there is an entry then it doesn't necessarily mean that it is part of the main chain. Unparented_blocks will show value > 1 in that case. Look for the last 2000 blocks*)
  let query missing_blocks_width =
    Caqti_request.find Caqti_type.unit Caqti_type.int
      (Core_kernel.sprintf
         {sql| 
        SELECT COUNT( * )
        FROM (SELECT h::int FROM generate_series(GREATEST(1, (SELECT MAX(height) FROM blocks) - %d) , (SELECT MAX(height) FROM blocks)) h
        LEFT JOIN blocks b 
        ON h = b.height WHERE b.height IS NULL) as v
      |sql}
         missing_blocks_width)

  let update ~missing_blocks_width (module Conn : Caqti_async.CONNECTION)
      metric_server =
    let open Deferred.Result.Let_syntax in
    time ~label:"missing_blocks" (fun () ->
        let%map missing_blocks = Conn.find (query missing_blocks_width) () in
        Mina_metrics.(
          Gauge.set
            (Archive.missing_blocks metric_server)
            (Float.of_int missing_blocks)))
end

module Unparented_blocks = struct
  (* parent_hashes represent ends of chains leading to an orphan block *)

  let query =
    Caqti_request.find Caqti_type.unit Caqti_type.int
      {sql|
           SELECT COUNT( * ) FROM blocks
           WHERE parent_id IS NULL
      |sql}

  let update (module Conn : Caqti_async.CONNECTION) metric_server =
    let open Deferred.Result.Let_syntax in
    time ~label:"unparented_blocks" (fun () ->
        let%map unparented_block_count = Conn.find query () in
        Mina_metrics.(
          Gauge.set
            (Archive.unparented_blocks metric_server)
            (Float.of_int unparented_block_count)))
end

let log_error ~logger pool metric_server
    (f :
         (module Caqti_async.CONNECTION)
      -> Mina_metrics.Archive.t
      -> (unit, [> Caqti_error.call_or_retrieve ]) Deferred.Result.t) =
  let open Deferred.Let_syntax in
  match%map
    Caqti_async.Pool.use
      (fun (module Conn : Caqti_async.CONNECTION) ->
        f (module Conn) metric_server)
      pool
  with
  | Ok () ->
      ()
  | Error e ->
      [%log warn] "Error updating archive metrics: $error"
        ~metadata:[ ("error", `String (Caqti_error.show e)) ]

let update ~logger ~missing_blocks_width pool metric_server =
  Deferred.all_unit
    (List.map
       ~f:(log_error ~logger pool metric_server)
       [ Max_block_height.update
       ; Unparented_blocks.update
       ; Missing_blocks.update ~missing_blocks_width
       ])
