open Pickles_types
open Core_kernel
open Hlist
open Import
open Backend
module Wrap_impl = Snarky_backendless.Snark.Run.Make (Tock) (Unit)

let test_bit x i = B.(shift_right x i land one = one)

let forbidden_shifted_values ~modulus:r ~size_in_bits ~f =
  let two_to_n = B.(pow (of_int 2) (of_int size_in_bits)) in
  let neg_two_to_n = B.(neg two_to_n) in
  let representatives x =
    let open Sequence in
    (* All values equivalent to x mod r that fit in [size_in_bits]
       many bits. *)
    let fits_in_n_bits x = B.(x < two_to_n) in
    unfold ~init:B.(x % r) ~f:(fun x -> Some (x, B.(x + r)))
    |> take_while ~f:fits_in_n_bits
    |> to_list
  in
  List.concat_map [neg_two_to_n; B.(neg_two_to_n - one)] ~f:representatives
  |> List.dedup_and_sort ~compare:B.compare
  |> List.map ~f

module Step = struct
  module Impl = Snarky_backendless.Snark.Run.Make (Tick) (Unit)
  include Impl

  module Other_field = struct
    (* Tick.Field.t = p < q = Tock.Field.t *)
    let size_in_bits = Tock.Field.size_in_bits

    module Constant = Tock.Field

    type t = (* Low bits, high bit *)
      Field.t * Boolean.var

    let forbidden_shifted_values =
      forbidden_shifted_values ~size_in_bits:Constant.size_in_bits
        ~modulus:(Wrap_impl.Bigint.to_bignum_bigint Constant.size) ~f:(fun x ->
          let hi = test_bit x (Field.size_in_bits - 1) in
          let lo = B.shift_right x 1 in
          (Impl.Bigint.(to_field (of_bignum_bigint lo)), hi) )

    let (typ_unchecked : (t, Constant.t) Typ.t), check =
      let t0 =
        Typ.transport
          (Typ.tuple2 Field.typ Boolean.typ)
          ~there:(fun x ->
            let low, high = Util.split_last (Tock.Field.to_bits x) in
            (Field.Constant.project low, high) )
          ~back:(fun (low, high) ->
            let low, _ = Util.split_last (Field.Constant.unpack low) in
            Tock.Field.of_bits (low @ [high]) )
      in
      let check t =
        let open Internal_Basic in
        let open Let_syntax in
        let equal (x1, b1) (x2, b2) =
          let%bind x_eq = Field.Checked.equal x1 (Field.Var.constant x2) in
          let b_eq = match b2 with true -> b1 | false -> Boolean.not b1 in
          Boolean.( && ) x_eq b_eq
        in
        let%bind () = t0.check t in
        Checked.List.map forbidden_shifted_values ~f:(equal t)
        >>= Boolean.any >>| Boolean.not >>= Boolean.Assert.is_true
      in
      (t0, check)

    let typ = {typ_unchecked with check}

    let to_bits (x, b) = Field.unpack x ~length:(Field.size_in_bits - 1) @ [b]
  end

  module Digest = Digest.Make (Impl)
  module Challenge = Challenge.Make (Impl)

  let input ~num_input_proofs ~wrap_rounds =
    let open Types.Pairing_based.Statement in
    let spec = spec num_input_proofs wrap_rounds in
    let (T (typ, f)) =
      Spec.packed_typ
        (module Impl)
        (T
           ( Shifted_value.typ Other_field.typ_unchecked
           , fun (Shifted_value x as t) ->
               Impl.run_checked (Other_field.check x) ;
               t ))
        spec
    in
    let typ = Typ.transport typ ~there:to_data ~back:of_data in
    Spec.ETyp.T (typ, fun x -> of_data (f x))

  let input_of_hlist (type num_input_proofss vars values)
      ~(num_input_proofss : num_input_proofss H1.T(Nat).t)
      ~(per_proof_specs : (values, vars, _) H2_1.T(Spec).t) :
      ( ( (vars, num_input_proofss) H2.T(Vector).t
        , Field.t
        , num_input_proofss H1.T(Vector.Carrying(Digest)).t )
        Types.Pairing_based.Statement.t
      , ( (values, num_input_proofss) H2.T(Vector).t
        , Digest.Constant.t
        , num_input_proofss H1.T(Vector.Carrying(Digest.Constant)).t )
        Types.Pairing_based.Statement.t
      , _ )
      Spec.ETyp.t =
    let open Types.Pairing_based.Statement in
    let packed_typ spec =
      Spec.packed_typ
        (module Impl)
        (T
           ( Shifted_value.typ Other_field.typ_unchecked
           , fun (Shifted_value x as t) ->
               Impl.run_checked (Other_field.check x) ;
               t ))
        spec
    in
    let rec build_typs : type num_input_proofss vars values.
           num_input_proofss H1.T(Nat).t
        -> (values, vars, _) H2_1.T(Spec).t
        -> ( (vars, num_input_proofss) H2.T(Vector).t
           , (values, num_input_proofss) H2.T(Vector).t
           , _ )
           Spec.ETyp.t
           * ( num_input_proofss H1.T(Vector.Carrying(Digest)).t
             , num_input_proofss H1.T(Vector.Carrying(Digest.Constant)).t
             , _ )
             Spec.ETyp.t =
     fun num_input_proofss specs ->
      match (num_input_proofss, specs) with
      | [], [] ->
          let per_proof_typ =
            let open Snarky_backendless.Typ in
            let module M = H2.T (Vector) in
            let open M in
            transport (unit ()) ~there:(fun [] -> ()) ~back:(fun () -> [])
            |> transport_var ~there:(fun [] -> ()) ~back:(fun () -> [])
          in
          let digest_typ =
            let open Snarky_backendless.Typ in
            let module M = H1.T (Vector.Carrying (Digest.Constant)) in
            let module N = H1.T (Vector.Carrying (Digest)) in
            let open M in
            transport (unit ()) ~there:(fun [] -> ()) ~back:(fun () -> [])
            |> transport_var ~there:N.(fun [] -> ()) ~back:N.(fun () -> [])
          in
          (Spec.ETyp.T (per_proof_typ, Fn.id), Spec.ETyp.T (digest_typ, Fn.id))
      | [], _ ->
          failwith "Pickles.Impls.Step.input_of_hlist: too many specs"
      | _, [] ->
          failwith
            "Pickles.Impls.Step.input_of_hlist: too many num_input_proofss"
      | num_input_proofs :: num_input_proofss, spec :: specs ->
          let T (per_proofs_typ, per_proofs_fn), T (digests_typ, digests_fn) =
            build_typs num_input_proofss specs
          in
          let (T (per_proof_typ, per_proof_fn)) =
            packed_typ (Vector (spec, num_input_proofs))
          in
          let (T (digest_typ, digest_fn)) =
            packed_typ (Vector (B Spec.Digest, num_input_proofs))
          in
          let per_proof_typ =
            let open Snarky_backendless.Typ in
            let module M = H2.T (Vector) in
            let open M in
            let typ =
              transport
                (tuple2 per_proof_typ per_proofs_typ)
                ~there:(fun (x :: y) -> (x, y))
                ~back:(fun (x, y) -> x :: y)
            in
            Spec.ETyp.T (typ, fun (x, y) -> per_proof_fn x :: per_proofs_fn y)
          in
          let digest_typ =
            let open Snarky_backendless.Typ in
            let module M = H1.T (Vector.Carrying (Digest.Constant)) in
            let module N = H1.T (Vector.Carrying (Digest)) in
            let open N in
            let typ =
              transport
                (tuple2 digest_typ digests_typ)
                ~there:M.(fun (x :: y) -> (x, y))
                ~back:(fun (x, y) -> x :: y)
            in
            Spec.ETyp.T (typ, fun (x, y) -> digest_fn x :: digests_fn y)
          in
          (per_proof_typ, digest_typ)
    in
    let T (per_proof_typ, per_proof_fn), T (digests_typ, digests_fn) =
      build_typs num_input_proofss per_proof_specs
    in
    let (T (me_only_typ, me_only_fn)) = packed_typ (B Spec.Digest) in
    let proof_state_typ =
      let open Types.Pairing_based.Proof_state in
      Snarky_backendless.Typ.of_hlistable
        [per_proof_typ; me_only_typ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
    in
    let typ =
      Snarky_backendless.Typ.of_hlistable
        [proof_state_typ; digests_typ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
    in
    Spec.ETyp.T
      ( typ
      , fun {proof_state= {unfinalized_proofs; me_only}; pass_through} ->
          { proof_state=
              { unfinalized_proofs= per_proof_fn unfinalized_proofs
              ; me_only= me_only_fn me_only }
          ; pass_through= digests_fn pass_through } )
end

module Wrap = struct
  module Impl = Wrap_impl
  include Impl
  module Challenge = Challenge.Make (Impl)
  module Digest = Digest.Make (Impl)
  module Wrap_field = Tock.Field
  module Step_field = Tick.Field

  module Other_field = struct
    module Constant = Tick.Field
    open Impl

    type t = Field.t

    let forbidden_shifted_values =
      forbidden_shifted_values ~size_in_bits:Constant.size_in_bits
        ~modulus:(Step.Impl.Bigint.to_bignum_bigint Constant.size) ~f:(fun x ->
          Impl.Bigint.(to_field (of_bignum_bigint x)) )

    let typ_unchecked, check =
      let t0 =
        Typ.transport Field.typ
          ~there:(Fn.compose Tock.Field.of_bits Tick.Field.to_bits)
          ~back:(Fn.compose Tick.Field.of_bits Tock.Field.to_bits)
      in
      let check t =
        let open Internal_Basic in
        let open Let_syntax in
        let equal x1 x2 = Field.Checked.equal x1 (Field.Var.constant x2) in
        let%bind () = t0.check t in
        Checked.List.map forbidden_shifted_values ~f:(equal t)
        >>= Boolean.any >>| Boolean.not >>= Boolean.Assert.is_true
      in
      (t0, check)

    let typ = {typ_unchecked with check}

    let to_bits x = Field.unpack x ~length:Field.size_in_bits
  end

  let input () =
    let fp : ('a, Other_field.Constant.t) Typ.t = Other_field.typ_unchecked in
    let open Types.Dlog_based.Statement in
    let (T (typ, f)) =
      Spec.packed_typ
        (module Impl)
        (T
           ( Shifted_value.typ fp
           , fun (Shifted_value x as t) ->
               Impl.run_checked (Other_field.check x) ;
               t ))
        In_circuit.spec
    in
    let typ =
      Typ.transport typ ~there:In_circuit.to_data ~back:In_circuit.of_data
    in
    Spec.ETyp.T (typ, fun x -> In_circuit.of_data (f x))
end
