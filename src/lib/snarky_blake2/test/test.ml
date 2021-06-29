open Core_kernel
open Snarky_blake2

let%test_module "blake2-equality test" =
  ( module struct
    (* Delete once the snarky pr lands *)
    module Impl = Snark_params.Tick
    include Make (Impl)

    let checked_to_unchecked typ1 typ2 checked input =
      let open Impl in
      let (), checked_result =
        run_and_check
          (let%bind input = exists typ1 ~compute:(As_prover.return input) in
           let%map result = checked input in
           As_prover.read typ2 result)
          ()
        |> Or_error.ok_exn
      in
      checked_result

    let test_equal (type a) ?(sexp_of_t = sexp_of_opaque) ?(equal = ( = )) typ1
        typ2 checked unchecked input =
      let checked_result = checked_to_unchecked typ1 typ2 checked input in
      let sexp_of_a = sexp_of_t in
      let compare_a x y = if equal x y then 0 else 1 in
      [%test_eq: a] checked_result (unchecked input)

    let blake2_unchecked s =
      Blake2.string_to_bits
        Digestif.BLAKE2S.(
          digest_string (Blake2.bits_to_string s) |> to_raw_string)

    let to_bitstring bits =
      String.init (Array.length bits) ~f:(fun i ->
          if bits.(i) then '1' else '0' )

    let%test_unit "constraint count" =
      assert (
        Impl.constraint_count
          (let open Impl in
          let%bind bits =
            exists
              (Typ.array ~length:512 Boolean.typ_unchecked)
              ~compute:(As_prover.return (Array.create ~len:512 true))
          in
          blake2s bits)
        <= 21278 )

    let%test_unit "blake2 equality" =
      let input =
        let open Quickcheck.Let_syntax in
        let%bind n = Int.gen_incl 0 (1024 / 8) in
        let%map x = String.gen_with_length n Char.quickcheck_generator in
        (n, Blake2.string_to_bits x)
      in
      let output_typ =
        Impl.Typ.array ~length:digest_length_in_bits Impl.Boolean.typ
      in
      Quickcheck.test ~trials:20 input ~f:(fun (n, input) ->
          let input_typ = Impl.Typ.array ~length:(8 * n) Impl.Boolean.typ in
          test_equal
            ~sexp_of_t:(Fn.compose [%sexp_of: string] to_bitstring)
            input_typ output_typ
            (blake2s ?personalization:None)
            blake2_unchecked input )
  end )
