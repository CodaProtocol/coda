open Core_kernel
open Snark_params

type t = Tick.Field.t * Tick.Field.t
[@@deriving bin_io]
let equal (x,y) (x',y') = Tick.Field.equal x y && Tick.Field.equal x' y'

type var = Tick.Field.var * Tick.Field.var
let typ : (var, t) Tick.Typ.t = Tick.Typ.(field * field)

(* TODO: We can move it onto the subgroup during account creation. No need to check with
  every transaction *)

module Compressed = struct
  open Tick
  type t = Field.t [@@deriving bin_io]
  type var = Field.var
  let typ : (var, t) Typ.t = Typ.field
  let assert_equal (x : var) (y : var) = assert_equal x y

(* TODO: Right now everyone could switch to using the other unpacking...
   Either decide this is ok or assert bitstring lt field size *)
  let var_to_bits (pk : var) =
    Checked.choose_preimage pk ~length:Field.size_in_bits
end

let compress_var : var -> Compressed.var = fun _ -> failwith "TODO"
let decompress_var : Compressed.var -> var = fun _ -> failwith "TODO"
let assert_equal : var -> var -> (unit, _) Tick.Checked.t = fun _ _ -> failwith "TODO"

let of_bigstring bs =
  let open Or_error.Let_syntax in
  let%map elem, _ = Bigstring.read_bin_prot bs bin_reader_t in
  elem

let to_bigstring elem =
  let bs = Bigstring.create (bin_size_t elem) in
  let _ = Bigstring.write_bin_prot bs bin_writer_t elem in
  bs

