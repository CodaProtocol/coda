open Core_kernel
open Async_kernel

module Ring_buffer : sig
  type 'a t [@@deriving sexp, bin_io]

  val read_all : 'a t -> 'a list

  val read_k : 'a t -> int -> 'a list
end

module State : sig
  module Job : sig
    type ('a, 'd) t =
      | Merge_up of 'a option
      | Merge of 'a option * 'a option
      | Base of 'd option
    [@@deriving bin_io, sexp]
  end

  module Completed_job : sig
    type 'a t = Lifted of 'a | Merged of 'a [@@deriving bin_io, sexp]
  end

  type ('a, 'd) t [@@deriving sexp, bin_io]

  val copy : ('a, 'd) t -> ('a, 'd) t

  module Hash : sig
    type t = Cryptokit.hash
  end

  val hash : ('a, 'd) t -> ('a -> string) -> ('d -> string) -> Hash.t
end

module type Spec_intf = sig
  type data [@@deriving sexp_of]

  type accum [@@deriving sexp_of]

  type output [@@deriving sexp_of]
end

module Available_job : sig
  type ('a, 'd) t = Base of 'd | Merge of 'a * 'a [@@deriving sexp]
end

val start : parallelism_log_2:int -> ('a, 'd) State.t

val next_k_jobs :
  state:('a, 'd) State.t -> k:int -> ('a, 'd) Available_job.t list Or_error.t

val next_jobs : state:('a, 'd) State.t -> ('a, 'd) Available_job.t list

val enqueue_data : state:('a, 'd) State.t -> data:'d list -> unit Or_error.t

val free_space : state:('a, 'd) State.t -> int

val fill_in_completed_jobs :
     state:('a, 'd) State.t
  -> jobs:'a State.Completed_job.t list
  -> 'a option Or_error.t
