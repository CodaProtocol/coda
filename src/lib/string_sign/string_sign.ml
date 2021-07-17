(* string_sign.ml -- signatures for strings *)

[%%import
"/src/config.mlh"]

[%%ifdef
consensus_mechanism]

open Snark_params
open Signature_lib
open Mina_base
module Field = Snark_params.Tick.Field
module Boolean = Snark_params.Tick.Boolean
module Inner_curve = Snark_params.Tick.Inner_curve

[%%else]

open Snark_params_nonconsensus
open Signature_lib_nonconsensus
open Mina_base_nonconsensus
module Snark_params = Snark_params_nonconsensus
module Tick = Snark_params
module Signature_lib = Signature_lib_nonconsensus
module Random_oracle = Random_oracle_nonconsensus.Random_oracle
module Inner_curve = Snark_params.Inner_curve

[%%endif]

module Message = struct
  type t = string

  let nybble_bits = function
    | 0x0 ->
        [false; false; false; false]
    | 0x1 ->
        [false; false; false; true]
    | 0x2 ->
        [false; false; true; false]
    | 0x3 ->
        [false; false; true; true]
    | 0x4 ->
        [false; true; false; false]
    | 0x5 ->
        [false; true; false; true]
    | 0x6 ->
        [false; true; true; false]
    | 0x7 ->
        [false; true; true; true]
    | 0x8 ->
        [true; false; false; false]
    | 0x9 ->
        [true; false; false; true]
    | 0xA ->
        [true; false; true; false]
    | 0xB ->
        [true; false; true; true]
    | 0xC ->
        [true; true; false; false]
    | 0xD ->
        [true; true; false; true]
    | 0xE ->
        [true; true; true; false]
    | 0xF ->
        [true; true; true; true]
    | _ ->
        failwith "nybble_bits: expected value from 0 to 0xF"

  let char_bits c =
    let open Core_kernel in
    let n = Char.to_int c in
    let hi = Int.(shift_right (bit_and n 0xF0) 4) in
    let lo = Int.bit_and n 0x0F in
    List.concat_map [hi; lo] ~f:nybble_bits

  let string_bits s =
    let open Core_kernel in
    List.(concat_map (String.to_list s) ~f:char_bits)

  let derive t ~private_key ~public_key:pk =
    let pk_bits {Public_key.Compressed.Poly.x; is_odd} =
      is_odd :: Field.unpack x
    in
    List.concat
      [ Tock.Field.unpack private_key
      ; pk_bits (Public_key.compress (Inner_curve.to_affine_exn pk))
      ; string_bits t ]
    |> Array.of_list |> Blake2.bits_to_string |> Blake2.digest_string
    |> Blake2.to_raw_string |> Blake2.string_to_bits |> Array.to_list
    |> Base.(Fn.flip List.take (Int.min 256 (Tock.Field.size_in_bits - 1)))
    |> Tock.Field.project

  let hash t ~public_key ~r =
    let string_to_input s =
      Random_oracle.Input.
        { field_elements= [||]
        ; bitstrings=
            Stdlib.(Array.of_seq (Seq.map char_bits (String.to_seq s))) }
    in
    let input =
      let px, py = Inner_curve.to_affine_exn public_key in
      Random_oracle.Input.append (string_to_input t)
        {field_elements= [|px; py; r|]; bitstrings= [||]}
    in
    let open Random_oracle in
    hash ~init:Hash_prefix.signature (pack_input input)
    |> Digest.to_bits |> Inner_curve.Scalar.of_bits

  [%%ifdef
  consensus_mechanism]

  (* TODO: factor out this common code from Signature_lib.Schnorr.Message *)
  type var = (Field.Var.t, Boolean.var) Random_oracle.Input.t

  let%snarkydef hash_checked t ~public_key ~r =
    let input =
      let px, py = public_key in
      Random_oracle.Input.append t
        {field_elements= [|px; py; r|]; bitstrings= [||]}
    in
    Tick.make_checked (fun () ->
        let open Random_oracle.Checked in
        hash ~init:Hash_prefix_states.signature (pack_input input)
        |> Digest.to_bits ~length:Field.size_in_bits
        |> Bitstring_lib.Bitstring.Lsb_first.of_list )

  [%%endif]
end

module Schnorr = Signature_lib.Schnorr.Make (Tick) (Inner_curve) (Message)
