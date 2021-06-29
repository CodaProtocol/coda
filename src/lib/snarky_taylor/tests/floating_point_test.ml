open Core_kernel
open Snarky
open Snark
open Snarky_integer
open Util
open Snarky_taylor.Floating_point

let%test_unit "of-quotient" =
  let module M = Snark_params.Tick.Run in
  let m : M.field m = (module M) in
  let gen =
    let open Quickcheck in
    let open Generator.Let_syntax in
    let m = B.((one lsl 32) - one) in
    let%bind a = B.(gen_incl zero (m - one)) in
    let%map b = B.(gen_incl (a + one) m) in
    (a, b)
  in
  Quickcheck.test ~trials:5 gen ~f:(fun (a, b) ->
      let precision = 32 in
      let (), res =
        assert (B.(a < b)) ;
        M.run_and_check
          (fun () ->
            let t =
              of_quotient ~m ~precision ~top:(Integer.constant ~m a)
                ~bottom:(Integer.constant ~m b) ~top_is_less_than_bottom:()
            in
            to_bignum ~m t )
          ()
        |> Or_error.ok_exn
      in
      let actual = Bignum.(of_bigint a / of_bigint b) in
      let good =
        Bignum.(abs (res - actual) < one / of_bigint B.(one lsl precision))
      in
      if not good then
        failwithf "got %s, expected %s\n" (Bignum.to_string_hum res)
          (Bignum.to_string_hum actual)
          () )
