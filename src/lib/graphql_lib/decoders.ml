(* decoders.ml -- decode Yojson to internal types *)

open Core_kernel

let optional ~f = function `Null -> None | json -> Some (f json)

let public_key json =
  Yojson.Basic.Util.to_string json
  |> Signature_lib.Public_key.of_base58_check_decompress_exn

let public_key_array = Array.map ~f:public_key

let optional_public_key = Option.map ~f:public_key

let uint64 json = Yojson.Basic.Util.to_string json |> Unsigned.UInt64.of_string

let optional_uint64 json = Option.map json ~f:uint64

let uint32 json = Yojson.Basic.Util.to_string json |> Unsigned.UInt32.of_string

let balance json =
  Yojson.Basic.Util.to_string json |> Currency.Balance.of_string

let amount json = Yojson.Basic.Util.to_string json |> Currency.Amount.of_string

let fee json = Yojson.Basic.Util.to_string json |> Currency.Fee.of_string

let nonce json =
  Yojson.Basic.Util.to_string json |> Mina_base.Account.Nonce.of_string

let token json =
  Yojson.Basic.Util.to_string json |> Mina_base.Token_id.of_string
