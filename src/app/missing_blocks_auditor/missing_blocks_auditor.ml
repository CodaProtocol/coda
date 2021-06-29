(* missing_blocks_auditor.ml -- report missing blocks from an archive db *)

open Core_kernel
open Async

let main ~archive_uri () =
  let logger = Logger.create () in
  let archive_uri = Uri.of_string archive_uri in
  match Caqti_async.connect_pool ~max_size:128 archive_uri with
  | Error e ->
      [%log fatal]
        ~metadata:[("error", `String (Caqti_error.show e))]
        "Failed to create a Caqti pool for Postgresql" ;
      exit 1
  | Ok pool ->
      [%log info] "Successfully created Caqti pool for Postgresql" ;
      [%log info] "Querying missing blocks" ;
      let%map missing_blocks =
        match%bind
          Caqti_async.Pool.use (fun db -> Sql.Unparented_blocks.run db ()) pool
        with
        | Ok blocks ->
            return blocks
        | Error msg ->
            [%log error] "Error getting missing blocks"
              ~metadata:[("error", `String (Caqti_error.show msg))] ;
            exit 1
      in
      List.iter missing_blocks ~f:(fun (block_id, state_hash, parent_hash) ->
          [%log info] "Block has no parent in archive db"
            ~metadata:
              [ ("block_id", `Int block_id)
              ; ("state_hash", `String state_hash)
              ; ("parent_hash", `String parent_hash) ] ) ;
      ()

let () =
  Command.(
    run
      (let open Let_syntax in
      Command.async
        ~summary:"Report state hashes of blocks missing from archive database"
        (let%map archive_uri =
           Param.flag "--archive-uri"
             ~doc:
               "URI URI for connecting to the archive database (e.g., \
                postgres://$USER@localhost:5432/archiver)"
             Param.(required string)
         in
         main ~archive_uri)))
