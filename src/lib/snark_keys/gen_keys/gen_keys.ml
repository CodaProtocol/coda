open Ppxlib
open Asttypes
open Parsetree
open Longident
open Core

let hashes ~constraint_constants ~loc =
  let module E = Ppxlib.Ast_builder.Make (struct
    let loc = loc
  end) in
  let open E in
  let f (_, x) = estring (Core.Md5.to_hex x) in
  let proof_level = Genesis_constants.Proof_level.compiled in
  let ts =
    Transaction_snark.constraint_system_digests ~constraint_constants ()
  in
  let bs =
    Blockchain_snark.Blockchain_snark_state.constraint_system_digests
      ~proof_level ~constraint_constants ()
  in
  elist (List.map ts ~f @ List.map bs ~f)

let from_disk_expr ~loc id =
  let module E = Ppxlib.Ast_builder.Make (struct
    let loc = loc
  end) in
  let open E in
  [%expr
    (* TODO: Not sure what to do with cache hit/generated something *)
    let open Async in
    let%map t, _ =
      Pickles.Verification_key.load ~cache:Cache_dir.cache
        (Sexp.of_string_conv_exn
           [%e
             estring
               (Pickles.Verification_key.Id.sexp_of_t id |> Sexp.to_string)]
           Pickles.Verification_key.Id.t_of_sexp)
      >>| Or_error.ok_exn
    in
    t]

let str ~loc ~constraint_constants ~blockchain_verification_key_id
    ~transaction_snark ~blockchain_snark =
  let module E = Ppxlib.Ast_builder.Make (struct
    let loc = loc
  end) in
  let open E in
  let hashes = hashes ~constraint_constants ~loc in
  [%str
    open! Core_kernel

    let blockchain_verification_key_id = [%e blockchain_verification_key_id]

    let transaction_verification () = [%e transaction_snark]

    let blockchain_verification () = [%e blockchain_snark]

    type key_hashes = string list [@@deriving to_yojson]

    let key_hashes : key_hashes = [%e hashes]]

let ok_or_fail_expr ~loc =
  [%expr function Ok x -> x | Error _ -> failwith "Gen_keys error"]

open Async

let loc = Ppxlib.Location.none

let handle_dirty dirty =
  if Array.mem ~equal:String.equal Sys.argv "--generate-keys-only" then
    Stdlib.exit 0 ;
  match dirty with
  | `Generated_something | `Locally_generated -> (
    (* If we generated any keys, then we need to make sure to upload these keys
     * to some central store to keep our builds compatible with one-another.
     *
     * We used to have a process where we manually upload keys whenever we want
     * to persist a new change. This is an attempt to make that process
     * automatic.
     *
     * We don't want to force an upload on every change as during development
     * you could churn on changes. Instead, we force uploads on CI jobs that
     * build testnet artifacts (as these are the binaries we want to make sure
     * we can retrieve keys for).
     * Uploads occur out-of-process in CI.
     *
     * See the background section of https://bkase.dev/posts/ocaml-writer
     * for more info on how this system works.
     *
     * NOTE: This behaviour is overriden or external contributors, because they
     * cannot upload new keys if they have modified the snark. See branch
     * referencing "CIRCLE_PR_USERNAME" below.
     *)
    match (Sys.getenv "CI", Sys.getenv "DUNE_PROFILE") with
    | Some _, Some _ when Option.is_some (Sys.getenv "CIRCLE_PR_USERNAME") ->
        (* External contributors cannot upload new keys to AWS, but we would
           still like to run CI for their pull requests if they have modified the
           snark.
        *)
        ( match dirty with
        | `Generated_something ->
            Format.eprintf
              "No keys were found in the cache, but this pull-request is from \
               an external contributor.@ Generated fresh keys for this \
               build.@."
        | `Locally_generated ->
            Format.eprintf
              "Only locally-generated keys were found in the cache, but this \
               pull-request is from an external contributor.@ Using the local \
               keys@."
        | `Cache_hit ->
            (* Excluded above. *)
            assert false ) ;
        Deferred.unit
    | Some _, Some profile
      when String.is_substring ~substring:"testnet" profile ->
        (* We are intentionally aborting the build here with a special error code
        * because we do not want builds to succeed if keys are not uploaded.
        *
        * Exit code is 0xc1 for "CI" *)
        exit 0xc1
    | Some _, Some _ | _, None | None, _ ->
        Deferred.unit )
  | `Cache_hit ->
      Deferred.unit

let str ~proof_level ~constraint_constants ~loc =
  if Genesis_constants.Proof_level.equal Full proof_level then
    let module T = Transaction_snark.Make (struct
      let constraint_constants = constraint_constants

      let proof_level = proof_level
    end) in
    let module B = Blockchain_snark.Blockchain_snark_state.Make (struct
      let tag = T.tag

      let constraint_constants = constraint_constants

      let proof_level = proof_level
    end) in
    let%map () =
      handle_dirty
        Pickles.(
          List.map
            [T.cache_handle; B.cache_handle]
            ~f:Cache_handle.generate_or_load
          |> List.reduce_exn ~f:Dirty.( + ))
    in
    let module E = Ppxlib.Ast_builder.Make (struct
      let loc = loc
    end) in
    let open E in
    str ~loc ~constraint_constants
      ~blockchain_verification_key_id:
        [%expr
          let t =
            lazy
              (Sexp.of_string_conv_exn
                 [%e
                   estring
                     ( Pickles.Verification_key.Id.sexp_of_t
                         (Lazy.force B.Proof.id)
                     |> Sexp.to_string )]
                 Pickles.Verification_key.Id.t_of_sexp)
          in
          fun () -> Lazy.force t]
      ~transaction_snark:(from_disk_expr ~loc (Lazy.force T.id))
      ~blockchain_snark:(from_disk_expr ~loc (Lazy.force B.Proof.id))
  else
    let e =
      [%expr Async.Deferred.return (Lazy.force Pickles.Verification_key.dummy)]
    in
    return
      (str ~loc ~constraint_constants
         ~blockchain_verification_key_id:
           [%expr Pickles.Verification_key.Id.dummy] ~transaction_snark:e
         ~blockchain_snark:e)

let main () =
  (* Wrap any junk we print to stdout in a comment.. *)
  Format.printf "(*@." ;
  let config_file =
    (* TODO-someday: Use a proper argument parser. *)
    match Array.findi Sys.argv (fun _ -> String.equal "--config-file") with
    | Some (i, _) ->
        let filename =
          match Sys.argv.(i + 1) with
          | filename ->
              filename
          | exception _ ->
              failwith "Expected an argument FILENAME for --config-file"
        in
        let json = Yojson.Safe.from_file ~fname:filename filename in
        Runtime_config.Json_layout.of_yojson json
        |> Result.ok_or_failwith |> Runtime_config.of_json_layout
        |> Result.ok_or_failwith
    | None ->
        Runtime_config.default
  in
  let proof_level =
    match
      let open Option.Let_syntax in
      let%bind proof = config_file.proof in
      proof.level
    with
    | None ->
        Genesis_constants.Proof_level.compiled
    | Some Full ->
        Full
    | Some Check ->
        Check
    | Some None ->
        None
  in
  let constraint_constants =
    let default = Genesis_constants.Constraint_constants.compiled in
    Option.value_map ~default
      ~f:(Genesis_ledger_helper_lib.make_constraint_constants ~default)
      config_file.proof
  in
  if Array.mem ~equal:String.equal Sys.argv "--download-keys" then
    Key_cache.set_downloads_enabled true ;
  let%bind str = str ~proof_level ~constraint_constants ~loc:Location.none in
  (* End comment started at the top of this function *)
  Format.printf "*)@." ;
  Pprintast.top_phrase Format.std_formatter (Ptop_def str) ;
  exit 0

let () =
  ignore (main ()) ;
  never_returns (Scheduler.go ())
