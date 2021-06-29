open Core

module Step = struct
  module Key = struct
    module Proving = struct
      type t =
        Type_equal.Id.Uid.t
        * Snark_keys_header.t
        * int
        * Backend.Tick.R1CS_constraint_system.t

      let to_string : t -> _ = function
        | _id, header, n, h ->
            sprintf !"step-%s-%s-%d-%s" header.kind.type_
              header.kind.identifier n header.identifying_hash
    end

    module Verification = struct
      type t = Type_equal.Id.Uid.t * Snark_keys_header.t * int * Md5.t
      [@@deriving sexp]

      let to_string : t -> _ = function
        | _id, header, n, h ->
            sprintf !"vk-step-%s-%s-%d-%s" header.kind.type_
              header.kind.identifier n header.identifying_hash
    end
  end

  let storable =
    Key_cache.Sync.Disk_storable.simple Key.Proving.to_string
      (fun (_, header, _, cs) ~path ->
        Or_error.try_with_join (fun () ->
            let open Or_error.Let_syntax in
            let%map header_read, index =
              Snark_keys_header.read_with_header
                ~read_data:(fun ~offset ->
                  Marlin_plonk_bindings.Pasta_fp_index.read ~offset
                    (Backend.Tick.Keypair.load_urs ()) )
                path
            in
            [%test_eq: int] header.header_version header_read.header_version ;
            [%test_eq: Snark_keys_header.Kind.t] header.kind header_read.kind ;
            [%test_eq: Snark_keys_header.Constraint_constants.t]
              header.constraint_constants header_read.constraint_constants ;
            [%test_eq: string] header.constraint_system_hash
              header_read.constraint_system_hash ;
            {Backend.Tick.Keypair.index; cs} ) )
      (fun (_, header, _, _) t path ->
        Or_error.try_with (fun () ->
            Snark_keys_header.write_with_header
              ~expected_max_size_log2:33 (* 8 GB should be enough *)
              ~append_data:
                (Marlin_plonk_bindings.Pasta_fp_index.write ~append:true
                   t.Backend.Tick.Keypair.index)
              header path ) )

  let vk_storable =
    Key_cache.Sync.Disk_storable.simple Key.Verification.to_string
      (fun (_, header, _, _) ~path ->
        Or_error.try_with_join (fun () ->
            let open Or_error.Let_syntax in
            let%map header_read, index =
              Snark_keys_header.read_with_header
                ~read_data:(fun ~offset path ->
                  Marlin_plonk_bindings.Pasta_fp_verifier_index.read ~offset
                    (Backend.Tick.Keypair.load_urs ())
                    path )
                path
            in
            [%test_eq: int] header.header_version header_read.header_version ;
            [%test_eq: Snark_keys_header.Kind.t] header.kind header_read.kind ;
            [%test_eq: Snark_keys_header.Constraint_constants.t]
              header.constraint_constants header_read.constraint_constants ;
            [%test_eq: string] header.constraint_system_hash
              header_read.constraint_system_hash ;
            index ) )
      (fun (_, header, _, _) x path ->
        Or_error.try_with (fun () ->
            Snark_keys_header.write_with_header
              ~expected_max_size_log2:33 (* 8 GB should be enough *)
              ~append_data:
                (Marlin_plonk_bindings.Pasta_fp_verifier_index.write
                   ~append:true x)
              header path ) )

  let read_or_generate cache k_p k_v typ main =
    let s_p = storable in
    let s_v = vk_storable in
    let open Impls.Step in
    let pk =
      lazy
        ( match
            Common.time "step keypair read" (fun () ->
                Key_cache.Sync.read cache s_p (Lazy.force k_p) )
          with
        | Ok (pk, dirty) ->
            Common.time "step keypair create" (fun () ->
                (Keypair.create ~pk ~vk:(Backend.Tick.Keypair.vk pk), dirty) )
        | Error _e ->
            let r =
              Common.time "stepkeygen" (fun () ->
                  generate_keypair ~exposing:[typ] main )
            in
            Timer.clock __LOC__ ;
            ignore
              ( Key_cache.Sync.write cache s_p (Lazy.force k_p) (Keypair.pk r)
                : unit Or_error.t ) ;
            (r, `Generated_something) )
    in
    let vk =
      lazy
        (let k_v = Lazy.force k_v in
         match
           Common.time "step vk read" (fun () ->
               Key_cache.Sync.read cache s_v k_v )
         with
         | Ok (vk, _) ->
             (vk, `Cache_hit)
         | Error _e ->
             let pk, c = Lazy.force pk in
             let vk = Keypair.vk pk in
             ignore (Key_cache.Sync.write cache s_v k_v vk : unit Or_error.t) ;
             (vk, c))
    in
    (pk, vk)
end

module Wrap = struct
  module Key = struct
    module Verification = struct
      type t = Type_equal.Id.Uid.t * Snark_keys_header.t * Md5.t
      [@@deriving sexp]

      let equal ((_, x1, y1) : t) ((_, x2, y2) : t) =
        [%equal: unit * Md5.t] ((* TODO: *) ignore x1, y1) (ignore x2, y2)

      let to_string : t -> _ = function
        | _id, header, h ->
            sprintf !"vk-wrap-%s-%s-%s" header.kind.type_
              header.kind.identifier header.identifying_hash
    end

    module Proving = struct
      type t =
        Type_equal.Id.Uid.t
        * Snark_keys_header.t
        * Backend.Tock.R1CS_constraint_system.t

      let to_string : t -> _ = function
        | _id, header, h ->
            sprintf !"wrap-%s-%s-%s" header.kind.type_ header.kind.identifier
              header.identifying_hash
    end
  end

  let storable =
    Key_cache.Sync.Disk_storable.simple Key.Proving.to_string
      (fun (_, header, cs) ~path ->
        Or_error.try_with_join (fun () ->
            let open Or_error.Let_syntax in
            let%map header_read, index =
              Snark_keys_header.read_with_header
                ~read_data:(fun ~offset ->
                  Marlin_plonk_bindings.Pasta_fq_index.read ~offset
                    (Backend.Tock.Keypair.load_urs ()) )
                path
            in
            [%test_eq: int] header.header_version header_read.header_version ;
            [%test_eq: Snark_keys_header.Kind.t] header.kind header_read.kind ;
            [%test_eq: Snark_keys_header.Constraint_constants.t]
              header.constraint_constants header_read.constraint_constants ;
            [%test_eq: string] header.constraint_system_hash
              header_read.constraint_system_hash ;
            {Backend.Tock.Keypair.index; cs} ) )
      (fun (_, header, _) t path ->
        Or_error.try_with (fun () ->
            Snark_keys_header.write_with_header
              ~expected_max_size_log2:33 (* 8 GB should be enough *)
              ~append_data:
                (Marlin_plonk_bindings.Pasta_fq_index.write ~append:true
                   t.index)
              header path ) )

  let read_or_generate step_domains cache k_p k_v typ main =
    let module Vk = Verification_key in
    let open Impls.Wrap in
    let s_p = storable in
    let pk =
      lazy
        (let k = Lazy.force k_p in
         match
           Common.time "wrap key read" (fun () ->
               Key_cache.Sync.read cache s_p k )
         with
         | Ok (pk, d) ->
             (Keypair.create ~pk ~vk:(Backend.Tock.Keypair.vk pk), d)
         | Error _e ->
             let r =
               Common.time "wrapkeygen" (fun () ->
                   generate_keypair ~exposing:[typ] main )
             in
             ignore
               ( Key_cache.Sync.write cache s_p k (Keypair.pk r)
                 : unit Or_error.t ) ;
             (r, `Generated_something))
    in
    let vk =
      lazy
        (let k_v = Lazy.force k_v in
         let s_v =
           Key_cache.Sync.Disk_storable.simple Key.Verification.to_string
             (fun (_, header, cs) ~path ->
               Or_error.try_with_join (fun () ->
                   let open Or_error.Let_syntax in
                   let%map header_read, index =
                     Snark_keys_header.read_with_header
                       ~read_data:(fun ~offset path ->
                         Binable.of_string
                           (module Vk.Stable.Latest)
                           (In_channel.read_all path) )
                       path
                   in
                   [%test_eq: int] header.header_version
                     header_read.header_version ;
                   [%test_eq: Snark_keys_header.Kind.t] header.kind
                     header_read.kind ;
                   [%test_eq: Snark_keys_header.Constraint_constants.t]
                     header.constraint_constants
                     header_read.constraint_constants ;
                   [%test_eq: string] header.constraint_system_hash
                     header_read.constraint_system_hash ;
                   index ) )
             (fun (_, header, _) t path ->
               Or_error.try_with (fun () ->
                   Snark_keys_header.write_with_header
                     ~expected_max_size_log2:33 (* 8 GB should be enough *)
                     ~append_data:(fun path ->
                       Out_channel.with_file ~append:true path ~f:(fun file ->
                           Out_channel.output_string file
                             (Binable.to_string (module Vk.Stable.Latest) t) )
                       )
                     header path ) )
         in
         match Key_cache.Sync.read cache s_v k_v with
         | Ok (vk, d) ->
             (vk, d)
         | Error e ->
             let kp, _dirty = Lazy.force pk in
             let vk = Keypair.vk kp in
             let pk = Keypair.pk kp in
             let vk : Vk.t =
               { index= vk
               ; commitments=
                   Pickles_types.Plonk_verification_key_evals.map vk.evals
                     ~f:(fun x ->
                       Array.map x.unshifted ~f:(function
                         | Infinity ->
                             failwith "Unexpected zero curve point"
                         | Finite x ->
                             x ) )
               ; step_domains
               ; data=
                   (let open Marlin_plonk_bindings.Pasta_fq_index in
                   {constraints= domain_d1_size pk.index}) }
             in
             ignore (Key_cache.Sync.write cache s_v k_v vk : unit Or_error.t) ;
             let _vk = Key_cache.Sync.read cache s_v k_v in
             (vk, `Generated_something))
    in
    (pk, vk)
end
