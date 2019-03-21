open Core
open Import

(* TODO : version *)
type single = Public_key.Compressed.Stable.V1.t * Currency.Fee.Stable.V1.t
[@@deriving bin_io, sexp, compare, eq, yojson]

(* TODO : version *)
type t = One of single | Two of single * single
[@@deriving bin_io, sexp, compare, eq, yojson]

let to_list = function One x -> [x] | Two (x, y) -> [x; y]

let of_single s = One s

let of_single_list xs =
  let rec go acc = function
    | x1 :: x2 :: xs -> go (Two (x1, x2) :: acc) xs
    | [] -> acc
    | [x] -> One x :: acc
  in
  go [] xs

let fee_excess = function
  | One (_, fee) ->
      Ok (Currency.Fee.Signed.negate @@ Currency.Fee.Signed.of_unsigned fee)
  | Two ((_, fee1), (_, fee2)) -> (
    match Currency.Fee.add fee1 fee2 with
    | None -> Or_error.error_string "Fee_transfer.fee_excess: overflow"
    | Some res ->
        Ok (Currency.Fee.Signed.negate @@ Currency.Fee.Signed.of_unsigned res)
    )

let receivers t = List.map (to_list t) ~f:(fun (pk, _) -> pk)
