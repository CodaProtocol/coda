module Bigint = struct
  module R = struct
    include Bigint.R

    let to_field x =
      let r = Snarky_bn382.Fq.of_bigint x in
      Gc.finalise Snarky_bn382.Fq.delete r ;
      r

    let of_field x =
      let r = Snarky_bn382.Fq.to_bigint x in
      Gc.finalise Snarky_bn382.Bigint.delete r ;
      r
  end
end

let field_size : Bigint.R.t = Snarky_bn382.Fp.size ()

module Field = Fq
module R1CS_constraint_system =
  R1cs_constraint_system.Make (Fq) (Snarky_bn382.Fq.Constraint_matrix)
module Var = Var
open Core_kernel
module Proof = Dlog_based_proof

module Verification_key = struct
  open Snarky_bn382

  type t = Fq_verifier_index.t

  let to_string _ = failwith "TODO"

  let of_string _ = failwith "TODO"
end

module Oracles = struct
  open Snarky_bn382

  let field f t =
    let x = f t in
    Caml.Gc.finalise Fq.delete x ;
    x

  let fieldvec f t =
    let x = f t in
    Caml.Gc.finalise Fq.Vector.delete x ;
    x

  open Fq_oracles

  let alpha = field alpha

  let eta_a = field eta_a

  let eta_b = field eta_b

  let eta_c = field eta_c

  let beta1 = field beta1

  let beta2 = field beta2

  let beta3 = field beta3

  let polys = field polys

  let evals = field evals

  (* TODO: Leaky *)
  let opening_prechallenges t =
    let open Snarky_bn382 in
    let t = opening_prechallenges t in
    Array.init (Fq.Vector.length t) ~f:(Fq.Vector.get t)

  let x_hat t =
    let t = x_hat_nocopy t in
    let fqv f = fieldvec f t in
    Snarky_bn382.Fq_vector_triple.(fqv f0, fqv f1, fqv f2)

  let digest_before_evaluations = field digest_before_evaluations
end

module Proving_key = struct
  type t = Snarky_bn382.Fq_index.t

  include Binable.Of_binable
            (Unit)
            (struct
              type nonrec t = t

              let to_binable _ = ()

              let of_binable () = failwith "TODO"
            end)

  let is_initialized _ = `Yes

  let set_constraint_system _ _ = ()

  let to_string _ = failwith "TODO"

  let of_string _ = failwith "TODO"
end

module Keypair = struct
  module Fq_index = Snarky_bn382.Fq_index
  module Fq_verifier_index = Snarky_bn382.Fq_verifier_index

  type t = Fq_index.t

  let set_urs_info, load_urs =
    let urs_info = Set_once.create () in
    let urs = ref None in
    let set_urs_info ?(degree = 20000) path =
      Set_once.set_exn urs_info Lexing.dummy_pos (degree, path)
    in
    let load () =
      match !urs with
      | Some urs ->
          urs
      | None ->
          let degree, path =
            match Set_once.get urs_info with
            | None ->
                failwith "Dlog_based.urs: Info not set"
            | Some t ->
                t
          in
          let u =
            if Sys.file_exists path then Snarky_bn382.Fq_urs.read path
            else
              let urs =
                Snarky_bn382.Fq_urs.create (Unsigned.Size_t.of_int degree)
              in
              Snarky_bn382.Fq_urs.write urs path ;
              urs
          in
          urs := Some u ;
          u
    in
    (set_urs_info, load)

  let () = set_urs_info "/tmp/dlog-urs"

  let create
      { R1cs_constraint_system.public_input_size
      ; auxiliary_input_size
      ; m= {a; b; c}
      ; weight } =
    let vars = 1 + public_input_size + auxiliary_input_size in
    Fq_index.create a b c
      (Unsigned.Size_t.of_int vars)
      (Unsigned.Size_t.of_int (public_input_size + 1))
      (load_urs ())

  let vk t = Fq_verifier_index.create t

  let pk = Fn.id

  open Pickles_types

  let vk_commitments t : Snarky_bn382.Fq_poly_comm.t Abc.t Matrix_evals.t =
    let f t = t in
    { row=
        { Abc.a= Fq_index.a_row_comm t
        ; b= Fq_index.b_row_comm t
        ; c= Fq_index.c_row_comm t }
    ; col=
        { a= Fq_index.a_col_comm t
        ; b= Fq_index.b_col_comm t
        ; c= Fq_index.c_col_comm t }
    ; value=
        { a= Fq_index.a_val_comm t
        ; b= Fq_index.b_val_comm t
        ; c= Fq_index.c_val_comm t }
    ; rc=
        { a= Fq_index.a_rc_comm t
        ; b= Fq_index.b_rc_comm t
        ; c= Fq_index.c_rc_comm t } }
    |> Matrix_evals.map ~f:(Abc.map ~f)
end
