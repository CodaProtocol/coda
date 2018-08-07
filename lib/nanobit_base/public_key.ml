open Core_kernel
open Snark_params

module Stable = struct
  module V1 = struct
    type t = Tick.Field.t * Tick.Field.t
    [@@deriving bin_io, sexp, eq, compare, hash]
  end
end

include Stable.V1

let ( = ) = equal

type var = Tick.Field.var * Tick.Field.var

let typ : (var, t) Tick.Typ.t = Tick.Typ.(field * field)

let to_bits ((x, y) : t) = Tick.Field.Bits.to_bits x @ Tick.Field.Bits.to_bits y

let var_to_bits ((x, y) : var) =
  let open Tick.Let_syntax in
  let%map x_bits = Tick.Field.Checked.choose_preimage_var x ~length:Tick.Field.size_in_bits
  and y_bits = Tick.Field.Checked.choose_preimage_var y ~length:Tick.Field.size_in_bits in
  x_bits @ y_bits

let fold ((x, y) : t) ~init ~f =
  let open Util in
  (Tick.Field.Bits.fold x +> Tick.Field.Bits.fold y) ~init ~f

let to_constant_var ((x, y) : t) : var =
  (Tick.Field.Checked.constant x, Tick.Field.Checked.constant y)

(* TODO: We can move it onto the subgroup during account creation. No need to check with
  every transaction *)

let of_private_key pk = Tick.Hash_curve.scale Tick.Hash_curve.generator pk

module Compressed = struct
  open Tick

  type ('field, 'boolean) t_ = {x: 'field; is_odd: 'boolean}
  [@@deriving bin_io, sexp, compare, eq, hash]

  module Stable = struct
    module V1 = struct
      type t = (Field.t, bool) t_ [@@deriving bin_io, sexp, eq, compare, hash]
    end
  end

  include Stable.V1
  include Comparable.Make_binable (Stable.V1)
  include Hashable.Make_binable (Stable.V1)

  let empty = {x= Field.zero; is_odd= false}

  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map x = Field.gen and is_odd = Bool.gen in
    {x; is_odd}

  let length_in_bits = 1 + Field.size_in_bits

  type var = (Field.var, Boolean.var) t_

  let to_hlist {x; is_odd} = H_list.[x; is_odd]

  let of_hlist : (unit, 'a -> 'b -> unit) H_list.t -> ('a, 'b) t_ =
    H_list.(fun [x; is_odd] -> {x; is_odd})

  let typ : (var, t) Typ.t =
    Typ.of_hlistable
      Data_spec.[Field.typ; Boolean.typ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  let var_of_t ({x; is_odd}: t) : var =
    {x= Field.Checked.constant x; is_odd= Boolean.var_of_value is_odd}

  let assert_equal (t1: var) (t2: var) =
    let open Let_syntax in
    let%map () = Field.Checked.Assert.equal t1.x t2.x
    and () = Boolean.Assert.(t1.is_odd = t2.is_odd) in
    ()

  let fold {is_odd; x} ~init ~f = Field.Bits.fold x ~init:(f init is_odd) ~f

  let to_bits {is_odd; x} = is_odd :: Field.Bits.to_bits x

  (* TODO: Right now everyone could switch to using the other unpacking...
   Either decide this is ok or assert bitstring lt field size *)
  let var_to_bits ({x; is_odd}: var) =
    let open Let_syntax in
    let%map x_bits =
      Field.Checked.choose_preimage_var x ~length:Field.size_in_bits
    in
    is_odd :: x_bits
end

open Tick
open Let_syntax

let parity y = Bigint.(test_bit (of_field y) 0)

let decompress ({x; is_odd}: Compressed.t) =
  Option.map (Signature_curve.find_y x) ~f:(fun y ->
      let y_parity = parity y in
      let y = if Bool.(is_odd = y_parity) then y else Field.negate y in
      (x, y) )

let decompress_exn t = Option.value_exn (decompress t)

let parity_var y =
  let%map bs = Util.unpack_field_var y in
  List.hd_exn (bs :> Boolean.var list)

let decompress_var ({x; is_odd} as c: Compressed.var) =
  let%bind y =
    provide_witness Typ.field
      As_prover.(
        map (read Compressed.typ c) ~f:(fun c -> snd (decompress_exn c)))
  in
  let%map () = Signature_curve.Checked.Assert.on_curve (x, y)
  and () = parity_var y >>= Boolean.Assert.(( = ) is_odd) in
  (x, y)

let compress ((x, y): t) : Compressed.t = {x; is_odd= parity y}

let compress_var ((x, y): var) : (Compressed.var, _) Checked.t =
  with_label __LOC__
    (let%map is_odd = parity_var y in
     {Compressed.x; is_odd})

let assert_equal ((x1, y1): var) ((x2, y2): var) : (unit, _) Checked.t =
  let%map () = Field.Checked.Assert.equal x1 x2
  and () = Field.Checked.Assert.equal y1 y2 in
  ()

let of_bigstring bs =
  let open Or_error.Let_syntax in
  let%map elem, _ = Bigstring.read_bin_prot bs bin_reader_t in
  elem

let to_bigstring elem =
  let bs =
    Bigstring.create (bin_size_t elem + Bin_prot.Utils.size_header_length)
  in
  let _ = Bigstring.write_bin_prot bs bin_writer_t elem in
  bs

let%test_unit "point-compression: decompress . compress = id" =
  Test_util.with_randomness 123456789 (fun () ->
      let test () =
        let pk = of_private_key (Private_key.create ()) in
        assert (decompress_exn (compress pk) = pk)
      in
      for i = 0 to 100 do test () done )
