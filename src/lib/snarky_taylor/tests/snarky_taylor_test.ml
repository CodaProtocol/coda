open Core_kernel
open Bitstring_lib
open Snarky
open Snark
open Snarky_integer
open Snarky_taylor

let%test_unit "instantiate" =
  let module M = Snark_params.Tick.Run in
  let m : M.field m = (module M) in
  let open M in
  let params =
    Exp.params ~field_size_in_bits:Field.Constant.size_in_bits
      ~base:Bignum.(one / of_int 2)
  in
  let c () =
    let arg =
      Floating_point.of_quotient ~m
        ~top:(Integer.of_bits ~m (Bitstring.Lsb_first.of_list Boolean.[true_]))
        ~bottom:
          (Integer.of_bits ~m
             (Bitstring.Lsb_first.of_list Boolean.[false_; true_]))
        ~top_is_less_than_bottom:() ~precision:2
    in
    Floating_point.to_bignum ~m (Exp.one_minus_exp ~m params arg)
  in
  let (), res = M.run_and_check c () |> Or_error.ok_exn in
  assert (
    Bignum.(equal res (Exp.Unchecked.one_minus_exp params (one / of_int 2))) )
