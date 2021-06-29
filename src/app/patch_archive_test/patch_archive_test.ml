(* patch_archive_test.ml *)

(* test patching of archive databases

   test structure:
    - copy archive database
    - remove some blocks from the copy
    - patch the copy
    - compare original and copy
*)

open Core_kernel
open Async

let db_from_uri uri =
  let path = Uri.path uri in
  String.sub path ~pos:1 ~len:(String.length path - 1)

let make_archive_copy_uri archive_uri =
  let db = db_from_uri archive_uri in
  Uri.with_path archive_uri ("/copy_of_" ^ db)

let query_db pool ~f ~item =
  match%bind Caqti_async.Pool.use f pool with
  | Ok v ->
      return v
  | Error msg ->
      failwithf "Error running query for %s from db, error: %s" item
        (Caqti_error.show msg) ()

let complete_prog_path prog_path =
  let open Core in
  (* make prog path absolute, so we can run it from a different working dir *)
  if Filename.is_absolute prog_path then prog_path
  else Sys.getcwd () ^/ prog_path

let extract_blocks ~uri ~working_dir ~extract_blocks_path =
  let args = ["--archive-uri"; Uri.to_string uri; "--all-blocks"] in
  let prog = complete_prog_path extract_blocks_path in
  Process.run_lines_exn ~working_dir ~prog ~args ()

let archive_blocks ~uri ~archive_blocks_path ~block_kind files =
  let args = ["--archive-uri"; Uri.to_string uri; block_kind] @ files in
  Process.run_lines_exn ~prog:archive_blocks_path ~args ()

let compare_blocks ~logger ~original_blocks_dir ~copy_blocks_dir =
  let blocks_in_dir dir =
    let%map blocks_array = Async.Sys.readdir dir in
    String.Set.of_array blocks_array
  in
  let diff_list s1 s2 = String.Set.diff s1 s2 |> String.Set.to_list in
  let get_block fn =
    In_channel.with_file fn ~f:(fun in_channel ->
        let json = Yojson.Safe.from_channel in_channel in
        match Archive_lib.Extensional.Block.of_yojson json with
        | Ok block ->
            block
        | Error err ->
            failwithf "Could not parse extensional block in file %s, error: %s"
              fn err () )
  in
  let%bind original_blocks = blocks_in_dir original_blocks_dir in
  let%bind copy_blocks = blocks_in_dir copy_blocks_dir in
  if not (String.Set.equal original_blocks copy_blocks) then (
    [%log error]
      "After patching, original and copied databases contain different blocks" ;
    let original_diff = diff_list original_blocks copy_blocks in
    [%log error] "Original database contains these blocks not in the copy"
      ~metadata:
        [("blocks", `List (List.map original_diff ~f:(fun s -> `String s)))] ;
    let copy_diff = diff_list copy_blocks original_blocks in
    [%log error] "Copied database contains these blocks not in the original"
      ~metadata:[("blocks", `List (List.map copy_diff ~f:(fun s -> `String s)))] ;
    Core_kernel.exit 1 ) ;
  [%log info]
    "After patching, original and copied databases contain the same set of \
     blocks" ;
  (* same set of blocks, see if the blocks are equal *)
  let found_difference =
    let open Core in
    String.Set.fold original_blocks ~init:false ~f:(fun acc block_file ->
        let original_block = get_block (original_blocks_dir ^/ block_file) in
        let copied_block = get_block (copy_blocks_dir ^/ block_file) in
        if
          not (Archive_lib.Extensional.Block.equal original_block copied_block)
        then (
          [%log error] "Original, copied blocks differ in file %s" block_file ;
          true )
        else acc )
  in
  if found_difference then (
    [%log fatal]
      "The contents of one or more blocks differs between the original and \
       copied databases" ;
    Core.exit 1 ) ;
  Deferred.unit

let rec rm_file_or_dir path =
  match%bind Sys.is_directory path with
  | `Yes ->
      let%bind contents = Sys.readdir path in
      let%bind () =
        Deferred.Array.iter
          ~f:(fun name -> rm_file_or_dir (Filename.concat path name))
          contents
      in
      Unix.rmdir path
  | `No ->
      Sys.remove path
  | `Unknown ->
      failwithf "Could not determine whether path %s is a directory" path ()

let main ~archive_uri ~num_blocks_to_patch ~archive_blocks_path
    ~extract_blocks_path ~precomputed ~extensional ~files () =
  let () =
    match (precomputed, extensional) with
    | true, false | false, true ->
        ()
    | _ ->
        failwith "Exactly one of -precomputed and -extensional must be true"
  in
  let logger = Logger.create () in
  let archive_uri = Uri.of_string archive_uri in
  let copy_uri = make_archive_copy_uri archive_uri in
  [%log info] "Connecting to original database" ;
  let%bind () =
    match Caqti_async.connect_pool ~max_size:128 archive_uri with
    | Error e ->
        [%log fatal]
          ~metadata:[("error", `String (Caqti_error.show e))]
          "Failed to create a Caqti pool for Postgresql" ;
        exit 1
    | Ok pool ->
        [%log info] "Successfully created Caqti pool for Postgresql" ;
        let original_db = db_from_uri archive_uri in
        let copy_db = db_from_uri copy_uri in
        [%log info] "Dropping copied database, in case it already exists" ;
        let%bind () =
          match%bind
            Caqti_async.Pool.use
              (fun db -> Sql.Copy_database.run_drop_db db ~copy_db)
              pool
          with
          | Ok () ->
              return ()
          | Error msg ->
              [%log info]
                "Dropping copied database resulted in error (probably it \
                 didn't exist): %s"
                (Caqti_error.show msg) ;
              return ()
        in
        [%log info] "Copying database" ;
        let%map () =
          query_db pool
            ~f:(fun db -> Sql.Copy_database.run db ~original_db ~copy_db)
            ~item:"database copy"
        in
        ()
  in
  [%log info] "Connecting to copied database" ;
  match Caqti_async.connect_pool ~max_size:128 copy_uri with
  | Error e ->
      [%log fatal]
        ~metadata:[("error", `String (Caqti_error.show e))]
        "Failed to create a Caqti pool for Postgresql" ;
      exit 1
  | Ok pool ->
      [%log info] "Successfully created Caqti pool for Postgresql" ;
      let%bind state_hashes =
        query_db pool
          ~f:(fun db -> Sql.Block.run_state_hash db)
          ~item:"state hashes"
      in
      let state_hash_array = Array.of_list state_hashes in
      (* indexes of block state_hashes to delete from copied database *)
      let indexes_to_delete =
        Quickcheck.random_value
          (Quickcheck.Generator.list_with_length num_blocks_to_patch
             (Int.gen_uniform_incl 0 (Array.length state_hash_array - 1)))
      in
      let%bind () =
        Deferred.List.iter indexes_to_delete ~f:(fun ndx ->
            let state_hash = state_hash_array.(ndx) in
            (* before removing block, remove any parent id references to that block
               otherwise, we get a foreign key constraint violation
            *)
            [%log info]
              "Removing parent references to block with state hash \
               $state_hash in copied database"
              ~metadata:[("state_hash", `String state_hash)] ;
            let%bind id =
              query_db pool
                ~f:(fun db -> Sql.Block.run db ~state_hash)
                ~item:"id of block to delete"
            in
            let%bind () =
              query_db pool
                ~f:(fun db -> Sql.Block.run_unset_parent db id)
                ~item:"id of parent block to be NULLed"
            in
            [%log info]
              "Deleting block with state hash $state_hash from copied database"
              ~metadata:[("state_hash", `String state_hash)] ;
            query_db pool
              ~f:(fun db -> Sql.Block.run_delete db ~state_hash)
              ~item:"state hash of block to delete" )
      in
      (* patch the copy with precomputed or extensional blocks, using the archive_blocks tool *)
      [%log info] "Patching the copy with supplied blocks" ;
      let block_kind =
        if precomputed then "-precomputed" else "-extensional"
      in
      let%bind _lines =
        archive_blocks ~uri:copy_uri ~archive_blocks_path ~block_kind files
      in
      (* extract extensional blocks from original and copy *)
      [%log info]
        "Extract extensional blocks from the original and copied databases" ;
      let tmp_prefix = "mina_archive_blocks" in
      let original_suffix = ".original" in
      let copy_suffix = ".copy" in
      let original_blocks_dir =
        Core.Filename.temp_dir ~in_dir:Filename.temp_dir_name tmp_prefix
          original_suffix
      in
      let copy_blocks_dir =
        Core.Filename.temp_dir ~in_dir:Filename.temp_dir_name tmp_prefix
          copy_suffix
      in
      [%log info] "Extracting blocks from original database to directory %s"
        original_blocks_dir ;
      let%bind _lines =
        extract_blocks ~uri:archive_uri ~working_dir:original_blocks_dir
          ~extract_blocks_path
      in
      [%log info]
        "Extracting blocks from copied and patched database to directory %s"
        copy_blocks_dir ;
      let%bind _lines =
        extract_blocks ~uri:copy_uri ~working_dir:copy_blocks_dir
          ~extract_blocks_path
      in
      let%bind () =
        compare_blocks ~logger ~original_blocks_dir ~copy_blocks_dir
      in
      [%log info] "Original archive db and patched copy are equivalent" ;
      (* no error, delete temp directories *)
      let%bind () = rm_file_or_dir original_blocks_dir in
      rm_file_or_dir copy_blocks_dir

let () =
  Command.(
    run
      (let open Let_syntax in
      async ~summary:"Test patching of blocks in an archive database"
        (let%map archive_uri =
           Param.flag "--archive-uri"
             ~doc:
               "URI URI for connecting to the archive database (e.g., \
                postgres://$USER@localhost:5432/archiver)"
             Param.(required string)
         and num_blocks_to_patch =
           Param.(
             flag "--num-blocks-to-patch" ~aliases:["num-blocks-to-patch"]
               Param.(required int))
             ~doc:"NN Number of blocks to remove and patch"
         and archive_blocks_path =
           Param.(
             flag "--archive-blocks-path" ~aliases:["archive-blocks-path"]
               Param.(required string))
             ~doc:"PATH Path to archive_blocks executable"
         and extract_blocks_path =
           Param.(
             flag "--extract-blocks-path" ~aliases:["extract-blocks-path"]
               Param.(required string))
             ~doc:"PATH Path to extract_blocks executable"
         and precomputed =
           Param.(flag "--precomputed" ~aliases:["precomputed"] no_arg)
             ~doc:"Blocks are in precomputed format"
         and extensional =
           Param.(flag "--extensional" ~aliases:["extensional"] no_arg)
             ~doc:"Blocks are in extensional format"
         and files = Param.anon Anons.(sequence ("FILES" %: Param.string)) in
         main ~archive_uri ~num_blocks_to_patch ~archive_blocks_path
           ~extract_blocks_path ~precomputed ~extensional ~files)))
