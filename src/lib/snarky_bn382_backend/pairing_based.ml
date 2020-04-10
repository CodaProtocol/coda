module Bigint = struct
  module R = struct
    include Bigint.R

    let to_field x =
      let r = Snarky_bn382.Fp.of_bigint x in
      Gc.finalise Snarky_bn382.Fp.delete r ;
      r

    let of_field x =
      let r = Snarky_bn382.Fp.to_bigint x in
      Gc.finalise Snarky_bn382.Bigint.delete r ;
      r
  end
end

open Core_kernel

let field_size : Bigint.R.t = Snarky_bn382.Fp.size ()

module Field = Fp

module Proving_key = struct
  open Core
  open Snarky_bn382

  type t = Fp_index.t

  include Binable.Of_binable
            (Unit)
            (struct
              type t = Fp_index.t

              let to_binable _ = ()

              let of_binable () = failwith "TODO"
            end)

  let is_initialized _ = `Yes

  let set_constraint_system _ _ = ()

  let to_string _ = failwith "TODO"

  let of_string _ = failwith "TODO"
end

module Verification_key = struct
  open Snarky_bn382

  type t = Fp_verifier_index.t

  let to_string _ = failwith "TODO"

  let of_string _ = failwith "TODO"
end

module R1CS_constraint_system =
  R1cs_constraint_system.Make (Fp) (Snarky_bn382.Fp.Constraint_matrix)
module Var = Var

module Oracles = struct
  open Snarky_bn382

  let field f t =
    let x = f t in
    Caml.Gc.finalise Field.delete x ;
    x

  open Fp_oracles

  let alpha = field alpha

  let eta_a = field eta_a

  let eta_b = field eta_b

  let eta_c = field eta_c

  let beta1 = field beta1

  let beta2 = field beta2

  let beta3 = field beta3

  let batch = field batch

  let r_k = field r_k

  let r = field r

  let x_hat_beta1 = field x_hat_beta1

  let digest_before_evaluations = field digest_before_evaluations
end

module Keypair = struct
  module Fp_index = Snarky_bn382.Fp_index
  module Fp_verifier_index = Snarky_bn382.Fp_verifier_index

  type t = Fp_index.t

  let set_urs_info, load_urs =
    let urs_info = Set_once.create () in
    let urs = ref None in
    let set_urs_info ?(degree = 3_11) path =
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
                failwith "Pairing_based.urs: Info not set"
            | Some t ->
                t
          in
          let u =
            if Sys.file_exists path then Snarky_bn382.Fp_urs.read path
            else
              let urs =
                Snarky_bn382.Fp_urs.create (Unsigned.Size_t.of_int degree)
              in
              Snarky_bn382.Fp_urs.write urs path ;
              urs
          in
          urs := Some u ;
          u
    in
    (set_urs_info, load)

  let () = set_urs_info "/tmp/pairing-urs"

  let create
      { R1cs_constraint_system.public_input_size
      ; auxiliary_input_size
      ; m= {a; b; c}
      ; weight } =
    let vars = 1 + public_input_size + auxiliary_input_size in
    Fp_index.create a b c
      (Unsigned.Size_t.of_int vars)
      (Unsigned.Size_t.of_int (public_input_size + 1))
      (load_urs ())

  let vk t = Fp_verifier_index.create t

  let pk = Fn.id

  open Pickles_types

  let vk_commitments t : G1.Affine.t Abc.t Matrix_evals.t =
    { row=
        { Abc.a= Fp_index.a_row_comm t
        ; b= Fp_index.b_row_comm t
        ; c= Fp_index.c_row_comm t }
    ; col=
        { a= Fp_index.a_col_comm t
        ; b= Fp_index.b_col_comm t
        ; c= Fp_index.c_col_comm t }
    ; value=
        { a= Fp_index.a_val_comm t
        ; b= Fp_index.b_val_comm t
        ; c= Fp_index.c_val_comm t }
    ; rc=
        { a= Fp_index.a_rc_comm t
        ; b= Fp_index.b_rc_comm t
        ; c= Fp_index.c_rc_comm t } }
    |> Matrix_evals.map ~f:(Abc.map ~f:G1.Affine.of_backend)
end

module Proof = Proof
