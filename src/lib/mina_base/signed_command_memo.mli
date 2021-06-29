(* user_command_memo.ml *)

[%%import "/src/config.mlh"]

open Core_kernel

[%%ifdef consensus_mechanism]

open Snark_params.Tick

[%%endif]

exception Too_long_user_memo_input

exception Too_long_digestible_string

type t [@@deriving sexp, equal, compare, hash, yojson]

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving bin_io, sexp, equal, compare, hash, yojson, version]
  end

  module Latest = V1
end

[%%ifdef consensus_mechanism]

module Checked : sig
  type unchecked = t

  type t = private Boolean.var array

  val constant : unchecked -> t
end

(** typ representation *)
val typ : (Checked.t, t) Typ.t

[%%endif]

val dummy : t

val empty : t

val to_string : t -> string

val of_string : string -> t

(** is the memo a digest *)
val is_digest : t -> bool

(** is the memo well-formed *)
val is_valid : t -> bool

(** bound on length of strings to digest *)
val max_digestible_string_length : int

(** bound on length of strings or bytes in memo *)
val max_input_length : int

(** create a memo by digesting a string; raises [Too_long_digestible_string] if
    length exceeds [max_digestible_string_length]
 *)
val create_by_digesting_string_exn : string -> t

(** create a memo by digesting a string; returns error if
    length exceeds [max_digestible_string_length]
 *)
val create_by_digesting_string : string -> t Or_error.t

(** create a memo from bytes of length up to max_input_length;
    raise [Too_long_user_memo_input] if length is greater
 *)
val create_from_bytes_exn : bytes -> t

(** create a memo from bytes of length up to max_input_length; returns
    error is length is greater
 *)
val create_from_bytes : bytes -> t Or_error.t

(** create a memo from a string of length up to max_input_length;
    raise [Too_long_user_memo_input] if length is greater
 *)
val create_from_string_exn : string -> t

(** create a memo from a string of length up to max_input_length;
    returns error if length is greater
 *)
val create_from_string : string -> t Or_error.t

(** convert a memo to a list of bools
 *)
val to_bits : t -> bool list
