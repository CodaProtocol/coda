open Core_kernel
open Async_kernel

module type Message_delay_intf = sig
  type message

  val delay : message -> Time.Span.t
end

module type Temporal_intf = sig
  type t

  val create : now:Time.Span.t -> t

  val tick_forwards : t -> unit Deferred.t
end

module type Fake_timer_transport_intf = sig
  include Node.Transport_intf

  include Node.Timer_intf with type t := t

  include Temporal_intf with type t := t

  val stop_listening : t -> me:peer -> unit
end

module type Fake_timer_transport_s = functor
  (Message :sig
            
            type t
          end)
  (Message_delay : Message_delay_intf with type message := Message.t)
  (Peer : Node.Peer_intf)
  -> Fake_timer_transport_intf
     with type message := Message.t
      and type peer := Peer.t

module type Trivial_peer_intf = sig
  type t = int [@@deriving equal, hash, compare, sexp, yojson]

  include Hashable.S with type t := t
end

module Trivial_peer : Trivial_peer_intf

module type S = functor
  (State :sig
          
          type t [@@deriving equal, sexp, yojson]
        end)
  (Message :sig
            
            type t
          end)
  (Message_delay : Message_delay_intf with type message := Message.t)
  (Message_label :sig
                  
                  type label [@@deriving enum, sexp]

                  include Hashable.S with type t = label
                end)
  (Timer_label :sig
                
                type label [@@deriving enum, sexp]

                include Hashable.S with type t = label
              end)
  (Condition_label :sig
                    
                    type label [@@deriving enum, sexp, yojson]

                    include Hashable.S with type t = label
                  end)
  -> sig
  type t

  module Timer_transport :
    Fake_timer_transport_intf
    with type message := Message.t
     and type peer := Trivial_peer.t

  module MyNode :
    Node.S
    with type message := Message.t
     and type state := State.t
     and type transport := Timer_transport.t
     and type peer := Trivial_peer.t
     and module Message_label := Message_label
     and module Timer_label := Timer_label
     and module Condition_label := Condition_label
     and module Timer := Timer_transport

  module Identifier : sig
    type t = Trivial_peer.t
  end

  type change = Delete of Identifier.t | Add of MyNode.t

  val loop :
    t -> stop:unit Deferred.t -> max_iters:int option -> unit Deferred.t

  val change : t -> change list -> unit

  val create :
       count:int
    -> initial_state:State.t
    -> (int -> MyNode.message_command list * MyNode.handle_command list)
    -> stop:unit Deferred.t
    -> t
end

module Make : S

module Fake_timer_transport : Fake_timer_transport_s
