module Poly_comm0 = Poly_comm
open Marlin_plonk_bindings.Types
open Unsigned.Size_t

module type Stable_v1 = sig
  module Stable : sig
    module V1 : sig
      type t [@@deriving version, bin_io, sexp, compare, yojson, hash, eq]
    end

    module Latest = V1
  end

  type t = Stable.V1.t [@@deriving sexp, compare, yojson]
end

module type Inputs_intf = sig
  open Intf

  val name : string

  module Rounds : Pickles_types.Nat.Intf

  module Gate_vector : sig
    open Unsigned

    type t

    val wrap :
      t -> Plonk_plookup_gate.Wire.t -> Plonk_plookup_gate.Wire.t -> unit
  end

  module Urs : sig
    type t

    val read : ?offset:int -> string -> t option

    val write : ?append:bool -> t -> string -> unit

    val create : int -> t
  end

  module Scalar_field : sig
    include Stable_v1

    val one : t
  end

  module Constraint_system : sig
    type t = (Gate_vector.t, Scalar_field.t) Plonk_plookup_constraint_system.t

    val finalize_and_get_gates : t -> Gate_vector.t
  end

  module Index : sig
    type t

    val create : Gate_vector.t -> int -> Urs.t -> t
  end

  module Curve : sig
    module Affine : sig
      type t
    end
  end

  module Poly_comm : sig
    module Backend : sig
      type t
    end

    type t = Curve.Affine.t Poly_comm0.t

    val of_backend_without_degree_bound : Backend.t -> t
  end

  module Verifier_index : sig
    type t =
      ( Scalar_field.t
      , Urs.t
      , Poly_comm.Backend.t )
      Plonk_plookup_verifier_index.t

    val create : Index.t -> t
  end
end

module Make (Inputs : Inputs_intf) = struct
  open Core_kernel
  open Inputs

  type t =
    { index: Index.t
    ; cs: (Gate_vector.t, Scalar_field.t) Plonk_plookup_constraint_system.t }

  let name = sprintf "%s_%d_v3" name (Pickles_types.Nat.to_int Rounds.n)

  let set_urs_info, load_urs =
    let urs_info = Set_once.create () in
    let urs = ref None in
    let degree = 1 lsl Pickles_types.Nat.to_int Rounds.n in
    let set_urs_info specs =
      Set_once.set_exn urs_info Lexing.dummy_pos specs
    in
    let load () =
      match !urs with
      | Some urs ->
          urs
      | None ->
          let specs =
            match Set_once.get urs_info with
            | None ->
                failwith "Dlog_based.urs: Info not set"
            | Some t ->
                t
          in
          let store =
            Key_cache.Sync.Disk_storable.simple
              (fun () -> name)
              (fun () ~path ->
                Or_error.try_with_join (fun () ->
                    match Urs.read path with
                    | Some urs ->
                        Ok urs
                    | None ->
                        Or_error.errorf
                          "Could not read the URS from disk; its format did \
                           not match the expected format" ) )
              (fun _ urs path ->
                Or_error.try_with (fun () -> Urs.write urs path) )
          in
          let u =
            match Key_cache.Sync.read specs store () with
            | Ok (u, _) ->
                u
            | Error _e ->
                let urs = Urs.create degree in
                let _ =
                  Key_cache.Sync.write
                    (List.filter specs ~f:(function
                      | On_disk _ ->
                          true
                      | S3 _ ->
                          false ))
                    store () urs
                in
                urs
          in
          urs := Some u ;
          u
    in
    (set_urs_info, load)

  let create cs =
    let gates = Constraint_system.finalize_and_get_gates cs in
    let conv =
      Plonk_plookup_constraint_system.Row.to_absolute
        ~public_input_size:(Set_once.get_exn cs.public_input_size [%here])
    in
    Plonk_plookup_constraint_system.V.Table.iter cs.equivalence_classes
      ~f:(fun x ->
        if List.length x > 1 then
          let h = List.hd_exn x in
          let t = List.last_exn x in
          let col_to_index = Plonk_plookup_constraint_system.col_to_index in
          Gate_vector.wrap gates
            {row= conv t.row; col= col_to_index t.col}
            {row= conv h.row; col= col_to_index h.col} ) ;
    let index =
      Index.create gates
        (Set_once.get_exn cs.public_input_size [%here])
        (load_urs ())
    in
    {index; cs}

  let vk t = Verifier_index.create t.index

  let pk t = t

  open Pickles_types

  let vk_commitments (t : Verifier_index.t) :
      Curve.Affine.t Dlog_plonk_types.Poly_comm.Without_degree_bound.t
      Plonk_plookup_verification_key_evals.t =
    let f (t : Poly_comm.Backend.t) =
      match Poly_comm.of_backend_without_degree_bound t with
      | `Without_degree_bound a ->
          a
      | _ ->
          assert false
    in
    Plonk_plookup_verification_key_evals.map ~f t.evals
end
