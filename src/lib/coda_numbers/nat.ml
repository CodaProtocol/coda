open Core_kernel
open Snark_bits
open Fold_lib
include Intf
module Intf = Intf

let zero_checked =
  Snarky_integer.Integer.constant ~m:Snark_params.Tick.m Bigint.zero

module Make (N : sig
  type t [@@deriving sexp, compare, hash]

  include Unsigned_extended.S with type t := t

  val random : unit -> t
end)
(Bits : Bits_intf.Convertible_bits with type t := N.t) =
struct
  type t = N.t [@@deriving sexp, compare, hash, yojson]

  include Comparable.Make (N)

  include (N : module type of N with type t := t)

  module Checked = struct
    open Bitstring_lib
    open Snark_params.Tick
    open Snarky_integer

    type var = field Integer.t

    let () = assert (Int.(N.length_in_bits < Field.size_in_bits))

    let to_field = Integer.to_field

    let of_bits bs = Integer.of_bits ~m bs

    let to_bits t =
      with_label
        (sprintf "to_bits: %s" __LOC__)
        (make_checked (fun () -> Integer.to_bits ~length:N.length_in_bits ~m t))

    let to_triples t =
      Checked.map (to_bits t)
        ~f:
          Bitstring.(
            Fn.compose
              (pad_to_triple_list ~default:Boolean.false_)
              Lsb_first.to_list)

    let constant n = Integer.constant ~m (Bignum_bigint.of_int (N.to_int n))

    let typ : (field Integer.t, t) Typ.t =
      let typ = Typ.list ~length:N.length_in_bits Boolean.typ in
      let of_bits bs = of_bits (Bitstring.Lsb_first.of_list bs) in
      let alloc = Typ.Alloc.map typ.alloc ~f:of_bits in
      let store t =
        Typ.Store.map (typ.store (Fold.to_list (Bits.fold t))) ~f:of_bits
      in
      let check v =
        typ.check (Bitstring.Lsb_first.to_list (Integer.to_bits_exn v))
      in
      let read v =
        let of_field_elt x =
          let bs = List.take (Field.unpack x) N.length_in_bits in
          (* TODO: Make this efficient *)
          List.foldi bs ~init:N.zero ~f:(fun i acc b ->
              if b then N.(logor (shift_left one i) acc) else acc )
        in
        Typ.Read.map (Field.typ.read (Integer.to_field v)) ~f:of_field_elt
      in
      {alloc; store; check; read}

    type t = var

    let is_succ ~pred ~succ =
      let open Snark_params.Tick in
      let open Field in
      Checked.(equal (to_field pred + Var.constant one) (to_field succ))

    let min a b = make_checked (fun () -> Integer.min ~m a b)

    let if_ c ~then_ ~else_ =
      make_checked (fun () -> Integer.if_ ~m c ~then_ ~else_)

    let succ_if t c =
      make_checked (fun () ->
          let t = Integer.succ_if ~m t c in
          t )

    let succ t =
      make_checked (fun () ->
          let t = Integer.succ ~m t in
          t )

    let op op a b = make_checked (fun () -> op ~m a b)

    let equal a b = op Integer.equal a b

    let ( < ) a b = op Integer.lt a b

    let ( <= ) a b = op Integer.lte a b

    let ( > ) a b = op Integer.gt a b

    let ( >= ) a b = op Integer.gte a b

    let ( = ) = equal

    let to_integer = Fn.id

    module Unsafe = struct
      let of_integer = Fn.id
    end

    let zero = zero_checked
  end

  let typ = Checked.typ

  module Bits = Bits

  let to_bits = Bits.to_bits

  let of_bits = Bits.of_bits

  let fold t = Fold.group3 ~default:false (Bits.fold t)

  let length_in_triples = (length_in_bits + 2) / 3

  let gen =
    Quickcheck.Generator.map
      ~f:(fun n -> N.of_string (Bignum_bigint.to_string n))
      (Bignum_bigint.gen_incl Bignum_bigint.zero
         (Bignum_bigint.of_string N.(to_string max_int)))
end

module Make32 () : UInt32 = struct
  open Unsigned_extended

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = UInt32.t [@@deriving sexp, eq, compare, hash, yojson]

      let to_latest = Fn.id
    end
  end]

  include Make (struct
              include UInt32

              let random () =
                let mask = if Random.bool () then one else zero in
                let open UInt32.Infix in
                logor (mask lsl 31)
                  ( Int32.max_value |> Random.int32 |> Int64.of_int32
                  |> UInt32.of_int64 )
            end)
            (Bits.UInt32)

  let to_uint32 = Unsigned_extended.UInt32.to_uint32

  let of_uint32 = Unsigned_extended.UInt32.of_uint32
end

module Make64 () : UInt64 = struct
  open Unsigned_extended

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = UInt64.t [@@deriving sexp, eq, compare, hash, yojson]

      let to_latest = Fn.id
    end
  end]

  include Make (struct
              include UInt64

              let random () =
                let mask = if Random.bool () then one else zero in
                let open UInt64.Infix in
                logor (mask lsl 63)
                  (Int64.max_value |> Random.int64 |> UInt64.of_int64)
            end)
            (Bits.UInt64)

  let to_uint64 = Unsigned_extended.UInt64.to_uint64

  let of_uint64 = Unsigned_extended.UInt64.of_uint64
end
