open Core_kernel
open Pipe_lib
open Mina_base

module State : sig
  module Stable : sig
    module V1 : sig
      type t = Pending | Included | Unknown
      [@@deriving equal, sexp, compare, bin_io, version]
    end

    module Latest = V1
  end

  type t = Stable.Latest.t = Pending | Included | Unknown
  [@@deriving equal, sexp, compare]

  val to_string : t -> string
end

val get_status :
     frontier_broadcast_pipe:Transition_frontier.t Option.t
                             Broadcast_pipe.Reader.t
  -> transaction_pool:Network_pool.Transaction_pool.t
  -> Signed_command.t
  -> State.t Or_error.t

val get_snapp_status :
     frontier_broadcast_pipe:Transition_frontier.t Option.t
                             Broadcast_pipe.Reader.t
  -> transaction_pool:Network_pool.Transaction_pool.t
  -> Snapp_command.t
  -> State.t Or_error.t
