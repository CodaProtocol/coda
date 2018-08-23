open Async_kernel

module type S = sig
  type t

  val connect :
       initial_peers:Peer.t list
    -> me:Peer.t
    -> external_rpc_port:int
    -> parent_log:Logger.t
    -> conf_dir:string
    -> t Deferred.Or_error.t

  val peers : t -> Peer.t list

  val first_peers : t -> Peer.t list Deferred.t

  val changes : t -> Peer.Event.t Linear_pipe.Reader.t

  val stop : t -> unit Deferred.t
end

module Haskell : S
