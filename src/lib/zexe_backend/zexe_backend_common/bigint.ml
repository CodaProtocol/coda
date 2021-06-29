open Core

module type Bindings = sig
  type t

  val num_limbs : unit -> int

  val bytes_per_limb : unit -> int

  val compare : t -> t -> int

  val div : t -> t -> t

  val test_bit : t -> int -> bool

  val print : t -> unit

  val to_string : t -> string

  val of_numeral : string -> int -> int -> t

  val of_decimal_string : string -> t

  val to_bytes : t -> Bytes.t

  val of_bytes : Bytes.t -> t
end

module type Intf = sig
  type t [@@deriving bin_io, sexp, compare]

  include Bindings with type t := t

  val num_limbs : int

  val bytes_per_limb : int

  val length_in_bytes : int

  val to_hex_string : t -> string

  val of_hex_string : string -> t

  val of_numeral : string -> base:int -> t
end

module Make
    (B : Bindings) (M : sig
        val length_in_bytes : int
    end) : Intf with type t = B.t = struct
  include B

  let num_limbs = num_limbs ()

  let bytes_per_limb = bytes_per_limb ()

  let length_in_bytes = num_limbs * bytes_per_limb

  let to_hex_string t =
    let data = to_bytes t in
    "0x" ^ Hex.encode (Bytes.to_string data)

  let sexp_of_t t = to_hex_string t |> Sexp.of_string

  let of_hex_string s =
    assert (Char.equal s.[0] '0' && Char.equal s.[1] 'x') ;
    String.drop_prefix s 2 |> Hex.Safe.of_hex
    |> Option.value_exn ~here:[%here]
    |> Bytes.of_string |> of_bytes

  let%test_unit "hex test" =
    let bytes =
      String.init length_in_bytes ~f:(fun _ -> Char.of_int_exn (Random.int 255))
    in
    let h = "0x" ^ Hex.encode bytes in
    [%test_eq: string] h (to_hex_string (of_hex_string h))

  let t_of_sexp s = of_hex_string (String.t_of_sexp s)

  include Bin_prot.Utils.Of_minimal (struct
    type nonrec t = t

    let bin_shape_t =
      Bin_prot.Shape.basetype
        (Bin_prot.Shape.Uuid.of_string
           (sprintf "zexe_backend_bigint_%d" M.length_in_bytes))
        []

    let __bin_read_t__ _buf ~pos_ref _vint =
      Bin_prot.Common.raise_variant_wrong_type "Bigint.t" !pos_ref

    let bin_size_t _ = length_in_bytes

    let bin_write_t buf ~pos t =
      let bytes = to_bytes t in
      let len = length_in_bytes in
      Bigstring.From_bytes.blit ~src:bytes ~src_pos:0 ~len:length_in_bytes
        ~dst:buf ~dst_pos:pos ;
      pos + len

    let bin_read_t buf ~pos_ref =
      let remaining_bytes = Bigstring.length buf - !pos_ref in
      let len = length_in_bytes in
      if remaining_bytes < len then
        failwithf "Bigint.bin_read_t: Expected %d bytes, got %d"
          M.length_in_bytes remaining_bytes () ;
      let bytes = Bigstring.To_bytes.sub ~pos:!pos_ref ~len buf in
      pos_ref := len + !pos_ref ;
      of_bytes bytes
  end)

  let of_numeral s ~base = of_numeral s (String.length s) base
end
