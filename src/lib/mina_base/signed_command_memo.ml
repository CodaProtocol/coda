(* user_command_memo.ml *)

[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Crypto_params

[%%endif]

[%%versioned
module Stable = struct
  module V1 = struct
    type t = string [@@deriving sexp, equal, compare, hash]

    let to_latest = Fn.id

    module Base58_check = Base58_check.Make (struct
      let description = "User command memo"

      let version_byte = Base58_check.Version_bytes.user_command_memo
    end)

    let to_string (memo : t) : string = Base58_check.encode memo

    let of_string (s : string) : t = Base58_check.decode_exn s

    module T = struct
      type nonrec t = t

      let to_string = to_string

      let of_string = of_string
    end

    include Codable.Make_of_string (T)
  end
end]

[%%define_locally
Stable.Latest.(to_yojson, of_yojson, to_string, of_string)]

exception Too_long_user_memo_input

exception Too_long_digestible_string

let max_digestible_string_length = 1000

(* 0th byte is a tag to distinguish digests from other data
   1st byte is length, always 32 for digests
   bytes 2 to 33 are data, 0-right-padded if length is less than 32
 *)

let digest_tag = '\x00'

let bytes_tag = '\x01'

let tag_index = 0

let length_index = 1

let digest_length = Blake2.digest_size_in_bytes

let digest_length_byte = Char.of_int_exn digest_length

(* +2 for tag and length bytes *)
let memo_length = digest_length + 2

let max_input_length = digest_length

let tag (memo : t) = memo.[tag_index]

let length memo = Char.to_int memo.[length_index]

let is_digest memo = Char.equal (tag memo) digest_tag

let is_valid memo =
  Int.(String.length memo = memo_length)
  &&
  let length = length memo in
  if is_digest memo then Int.(length = digest_length)
  else
    Char.equal (tag memo) bytes_tag
    && Int.(length <= digest_length)
    &&
    let padded =
      String.sub memo ~pos:(length + 2) ~len:(digest_length - length)
    in
    String.for_all padded ~f:(Char.equal '\x00')

let create_by_digesting_string_exn s =
  if Int.(String.length s > max_digestible_string_length) then
    raise Too_long_digestible_string ;
  let digest = Blake2.(to_raw_string (digest_string s)) in
  String.init memo_length ~f:(fun ndx ->
      if Int.(ndx = tag_index) then digest_tag
      else if Int.(ndx = length_index) then digest_length_byte
      else digest.[ndx - 2] )

let create_by_digesting_string (s : string) =
  try Ok (create_by_digesting_string_exn s)
  with Too_long_digestible_string ->
    Or_error.error_string "create_by_digesting_string: string too long"

module type Memoable = sig
  type t

  val length : t -> int

  val get : t -> int -> char
end

let create_from_value_exn (type t) (module M : Memoable with type t = t)
    (value : t) =
  let len = M.length value in
  if Int.(len > max_input_length) then raise Too_long_user_memo_input ;
  String.init memo_length ~f:(fun ndx ->
      if Int.(ndx = tag_index) then bytes_tag
      else if Int.(ndx = length_index) then Char.of_int_exn len
      else if Int.(ndx < len + 2) then M.get value (ndx - 2)
      else '\x00' )

let create_from_bytes_exn bytes = create_from_value_exn (module Bytes) bytes

let create_from_bytes bytes =
  try Ok (create_from_bytes_exn bytes)
  with Too_long_user_memo_input ->
    Or_error.error_string
      (sprintf "create_from_bytes: length exceeds %d" max_input_length)

let create_from_string_exn s = create_from_value_exn (module String) s

let create_from_string s =
  try Ok (create_from_string_exn s)
  with Too_long_user_memo_input ->
    Or_error.error_string
      (sprintf "create_from_string: length exceeds %d" max_input_length)

let dummy = (create_by_digesting_string_exn "" :> t)

let empty = create_from_string_exn ""

let fold_bits t =
  { Fold_lib.Fold.fold=
      (fun ~init ~f ->
        let n = 8 * String.length t in
        let rec go acc i =
          if i = n then acc
          else
            let b = (Char.to_int t.[i / 8] lsr (i mod 8)) land 1 = 1 in
            go (f acc b) (i + 1)
        in
        go init 0 ) }

let to_bits t = Fold_lib.Fold.to_list (fold_bits t)

[%%ifdef
consensus_mechanism]

module Boolean = Tick.Boolean
module Typ = Tick.Typ

(* the code below is much the same as in Random_oracle.Digest; tag and length bytes
   make it a little different
 *)

module Checked = struct
  type unchecked = t

  type t = Boolean.var array

  let constant unchecked =
    assert (Int.(String.length (unchecked :> string) = memo_length)) ;
    Array.map
      (Blake2.string_to_bits (unchecked :> string))
      ~f:Boolean.var_of_value
end

let length_in_bits = 8 * memo_length

let typ : (Checked.t, t) Typ.t =
  Typ.transport
    (Typ.array ~length:length_in_bits Boolean.typ)
    ~there:(fun (t : t) -> Blake2.string_to_bits (t :> string))
    ~back:(fun bs -> (Blake2.bits_to_string bs :> t))

[%%endif]

let%test_module "user_command_memo" =
  ( module struct
    let data memo = String.sub memo ~pos:(length_index + 1) ~len:(length memo)

    let%test "digest string" =
      let s = "this is a string" in
      let memo = create_by_digesting_string_exn s in
      is_valid memo

    let%test "digest too-long string" =
      let s =
        String.init (max_digestible_string_length + 1) ~f:(fun _ -> '\xFF')
      in
      try
        let (_ : t) = create_by_digesting_string_exn s in
        false
      with Too_long_digestible_string -> true

    let%test "memo from string" =
      let s = "time and tide wait for no one" in
      let memo = create_from_string_exn s in
      is_valid memo && String.equal s (data memo)

    let%test "memo from too-long string" =
      let s = String.init (max_input_length + 1) ~f:(fun _ -> '\xFF') in
      try
        let (_ : t) = create_from_string_exn s in
        false
      with Too_long_user_memo_input -> true

    [%%ifdef
    consensus_mechanism]

    let%test_unit "typ is identity" =
      let s = "this is a string" in
      let memo = create_by_digesting_string_exn s in
      let read_constant = function
        | Snarky_backendless.Cvar.Constant x ->
            x
        | _ ->
            assert false
      in
      let memo_var =
        Snarky_backendless.Typ_monads.Store.run (typ.store memo) (fun x ->
            Snarky_backendless.Cvar.Constant x )
      in
      let memo_read =
        Snarky_backendless.Typ_monads.Read.run (typ.read memo_var)
          read_constant
      in
      [%test_eq: string] memo memo_read

    [%%endif]
  end )
