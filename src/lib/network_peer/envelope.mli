open Core

module Sender : sig
  type t = Local | Remote of Peer.t [@@deriving sexp, equal, yojson, compare]

  val remote_exn : t -> Peer.t
end

module Incoming : sig
  type 'a t = {data: 'a; sender: Sender.t; received_at: Time.t}
  [@@deriving equal, sexp, yojson, compare]

  val sender : 'a t -> Sender.t

  val data : 'a t -> 'a

  val received_at : 'a t -> Time.t

  val wrap : data:'a -> sender:Sender.t -> 'a t

  val wrap_peer : data:'a -> sender:Peer.t -> 'a t

  val map : f:('a -> 'b) -> 'a t -> 'b t

  val local : 'a -> 'a t

  val remote_sender_exn : 'a t -> Peer.t

  val gen : 'a Quickcheck.Generator.t -> 'a t Quickcheck.Generator.t
end
