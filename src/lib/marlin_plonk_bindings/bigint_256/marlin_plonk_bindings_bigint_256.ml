type t

external num_limbs : unit -> int = "caml_bigint_256_num_limbs"

external bytes_per_limb : unit -> int = "caml_bigint_256_bytes_per_limb"

external compare : t -> t -> int = "caml_bigint_256_compare"

external div : t -> t -> t = "caml_bigint_256_div"

external test_bit : t -> int -> bool = "caml_bigint_256_test_bit"

external print : t -> unit = "caml_bigint_256_print"

external to_string : t -> string = "caml_bigint_256_to_string"

external of_numeral : string -> int -> int -> t = "caml_bigint_256_of_numeral"

external of_decimal_string : string -> t = "caml_bigint_256_of_decimal_string"

external to_bytes : t -> Bytes.t = "caml_bigint_256_to_bytes"

external of_bytes : Bytes.t -> t = "caml_bigint_256_of_bytes"

external deep_copy : t -> t = "caml_bigint_256_deep_copy"

let%test "deep_copy" =
  let x = of_decimal_string "12345" in
  deep_copy x = x

let%test "serialization and deserialization" =
  let x = of_decimal_string "3" in
  let bytes = to_bytes x in
  let y = of_bytes bytes in
  x = y

let%test "div and compare" =
  let x = of_decimal_string "3" in
  let y = of_numeral "3" 1 10 in
  compare x y = 0

let%test "test_bit" =
  let x = of_decimal_string "6" in
  let y = of_decimal_string "2" in
  (* z = 0b0000_0011 *)
  let z = div x y in
  test_bit z 0 && test_bit z 1 && not (test_bit z 2)
