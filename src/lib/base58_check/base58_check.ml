(* base58_check.ml : implement Base58Check algorithm
   see: https://www.oreilly.com/library/view/mastering-bitcoin-2nd/9781491954379/ch04.html#base58
*)

open Core_kernel

exception Invalid_base58_checksum of string

exception Invalid_base58_version_byte of (char * string)

exception Invalid_base58_check_length of string

exception Invalid_base58_character of string

(* same as Bitcoin alphabet *)
let mina_alphabet =
  B58.make_alphabet
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

let version_len = 1

let checksum_len = 4

module Make (M : sig
  val description : string

  val version_byte : char
end) =
struct
  let version_byte = M.version_byte

  let version_string = String.make 1 version_byte

  (* the Base58 library's "convert" routine, used for both
      encoding and decoding, runs in time O(n^2); limit
      length of input when encoding to avoid bottlenecks
  *)
  let max_encodable_length = 8192

  let compute_checksum payload =
    (* double-hash using SHA256 *)
    let open Digestif.SHA256 in
    let ctx0 = init () in
    let ctx1 = feed_string ctx0 version_string in
    let ctx2 = feed_string ctx1 payload in
    let first_hash = get ctx2 |> to_raw_string in
    let ctx3 = feed_string ctx0 first_hash in
    let second_hash = get ctx3 |> to_raw_string in
    second_hash |> String.sub ~pos:0 ~len:checksum_len

  let encode payload =
    let len = String.length payload in
    if len > max_encodable_length then
      failwith (sprintf "Base58_check.encode: input too long (%d bytes)" len) ;
    let checksum = compute_checksum payload in
    let bytes = version_string ^ payload ^ checksum |> Bytes.of_string in
    B58.encode mina_alphabet bytes |> Bytes.to_string

  let decode_exn s =
    let bytes = Bytes.of_string s in
    let decoded =
      try B58.decode mina_alphabet bytes |> Bytes.to_string
      with B58.Invalid_base58_character ->
        raise (Invalid_base58_character M.description)
    in
    let len = String.length decoded in
    (* input must be at least as long as the version byte and checksum *)
    if len < version_len + checksum_len then
      raise (Invalid_base58_check_length M.description) ;
    let checksum =
      String.sub decoded
        ~pos:(String.length decoded - checksum_len)
        ~len:checksum_len
    in
    let payload =
      String.sub decoded ~pos:1 ~len:(len - version_len - checksum_len)
    in
    if not (String.equal checksum (compute_checksum payload)) then
      raise (Invalid_base58_checksum M.description) ;
    if not (Char.equal decoded.[0] version_byte) then
      raise (Invalid_base58_version_byte (decoded.[0], M.description)) ;
    payload

  let decode s =
    let error_str e desc =
      sprintf "Error decoding %s\nInvalid base58 %s in %s" s e desc
    in
    try Ok (decode_exn s) with
    | Invalid_base58_character str ->
        Or_error.error_string (error_str "character" str)
    | Invalid_base58_check_length str ->
        Or_error.error_string (error_str "check length" str)
    | Invalid_base58_checksum str ->
        Or_error.error_string (error_str "checksum" str)
    | Invalid_base58_version_byte (ch, str) ->
        Or_error.error_string
          (error_str
             (sprintf "version byte \\x%02X, expected \\x%02X" (Char.to_int ch)
                (Char.to_int version_byte))
             str)
end

module Version_bytes = Version_bytes

let%test_module "base58check tests" =
  ( module struct
    module Base58_check = Make (struct
      let description = "Base58check tests"

      let version_byte = '\x53'
    end)

    open Base58_check

    let test_roundtrip payload =
      let encoded = encode payload in
      let payload' = decode_exn encoded in
      String.equal payload payload'

    let%test "empty_string" = test_roundtrip ""

    let%test "nonempty_string" =
      test_roundtrip "Somewhere, over the rainbow, way up high"

    let%test "longer_string" =
      test_roundtrip
        "Someday, I wish upon a star, wake up where the clouds are far behind \
         me, where trouble melts like lemon drops, High above the chimney \
         top, that's where you'll find me"

    let%test "invalid checksum" =
      try
        let encoded = encode "Bluer than velvet were her eyes" in
        let bytes = Bytes.of_string encoded in
        let len = Bytes.length bytes in
        let last_ch = Bytes.get bytes (len - 1) in
        (* change last byte to invalidate checksum *)
        let new_last_ch =
          if Char.equal last_ch '\xFF' then '\x00'
          else Char.of_int_exn (Char.to_int last_ch + 1)
        in
        Bytes.set bytes (len - 1) new_last_ch ;
        let encoded_bad_checksum = Bytes.to_string bytes in
        let _payload = decode_exn encoded_bad_checksum in
        false
      with Invalid_base58_checksum _ -> true

    let%test "invalid length" =
      try
        let _payload = decode_exn "abcd" in
        false
      with Invalid_base58_check_length _ -> true
  end )
