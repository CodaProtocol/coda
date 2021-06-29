(* base58_check.mli -- implementation of Base58Check algorithm *)

open Core_kernel

exception Invalid_base58_checksum of string

exception Invalid_base58_version_byte of (char * string)

exception Invalid_base58_check_length of string

exception Invalid_base58_character of string

(** the Mina base 58 alphabet *)
val mina_alphabet : B58.alphabet

module Make (M : sig
  val description : string

  val version_byte : char
end) : sig
  (** apply Base58Check algorithm to a payload *)
  val encode : string -> string

  (** decode Base58Check result into payload; can raise the above
      exceptions
  *)
  val decode_exn : string -> string

  (** decode Base58Check result into payload *)
  val decode : string -> string Or_error.t
end

module Version_bytes : module type of Version_bytes
