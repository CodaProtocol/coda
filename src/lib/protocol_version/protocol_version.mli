(* protocol_version.mli *)

[%%versioned:
module Stable : sig
  module V1 : sig
    type t [@@deriving compare, sexp]
  end
end]

val create_exn : major:int -> minor:int -> patch:int -> t

val create_opt : major:int -> minor:int -> patch:int -> t option

val get_current : unit -> t

val set_current : t -> unit

val get_proposed_opt : unit -> t option

val set_proposed_opt : t option -> unit

val zero : t

(** a daemon can accept blocks or RPC responses with compatible protocol versions *)
val compatible_with_daemon : t -> bool

val to_string : t -> string

val of_string_exn : string -> t

val of_string_opt : string -> t option

(** useful when deserializing, could contain negative integers *)
val is_valid : t -> bool
