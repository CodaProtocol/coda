open Core_kernel
open Async_kernel
open Pipe_lib

module type Peer_intf = sig
  type t [@@deriving equal, hash, compare, sexp, yojson]

  include Hashable.S with type t := t
end

module type Transport_intf = sig
  type t

  type message

  type peer

  val send : t -> recipient:peer -> message -> unit Or_error.t Deferred.t

  val listen : t -> me:peer -> message Linear_pipe.Reader.t
end

module type Timer_intf = sig
  type t

  type tok [@@deriving equal]

  val wait : t -> Time.Span.t -> tok * [`Cancelled | `Finished] Deferred.t

  val cancel : t -> tok -> unit
end

module type S = sig
  type message

  type state

  type transport

  type peer

  module Message_label : Hashable.S

  module Timer_label : Hashable.S

  module Condition_label : Hashable.S

  module Timer : Timer_intf

  module Identifier : Hashable.S with type t := peer

  type condition = state -> bool

  type message_condition = message -> condition

  type transition = t -> state -> state Deferred.t

  and message_transition = t -> message -> state -> state Deferred.t

  and t

  type handle_command = Condition_label.t * condition * transition

  type message_command =
    Message_label.t * message_condition * message_transition

  val on : Condition_label.t -> condition -> f:transition -> handle_command

  val msg :
       Message_label.t
    -> message_condition
    -> f:message_transition
    -> message_command

  val cancel : t -> ?tok:Timer.tok option -> Timer_label.t -> unit

  val timeout : t -> Timer_label.t -> Time.Span.t -> f:transition -> Timer.tok

  val timeout' : t -> Timer_label.t -> Time.Span.t -> f:transition -> unit

  val next_ready : t -> unit Deferred.t

  val is_ready : t -> bool

  val make_node :
       transport:transport
    -> logger:Logger.t
    -> me:peer
    -> messages:message Linear_pipe.Reader.t
    -> ?parent:t
    -> initial_state:state
    -> timer:Timer.t
    -> message_command list
    -> handle_command list
    -> t

  val step : t -> t Deferred.t

  val ident : t -> peer

  val state : t -> state

  val send : t -> recipient:peer -> message -> unit Or_error.t Deferred.t

  val send_exn : t -> recipient:peer -> message -> unit Deferred.t

  val send_multi :
    t -> recipients:peer list -> message -> unit Or_error.t list Deferred.t

  val send_multi_exn : t -> recipients:peer list -> message -> unit Deferred.t
end

module type F = functor
  (State :sig
          
          type t [@@deriving equal, sexp, yojson]
        end)
  (Message :sig
            
            type t
          end)
  (Peer : Peer_intf)
  (Timer : Timer_intf)
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
  (Transport :
     Transport_intf with type message := Message.t and type peer := Peer.t)
  -> S
     with type message := Message.t
      and type state := State.t
      and type transport := Transport.t
      and type peer := Peer.t
      and module Message_label := Message_label
      and module Timer_label := Timer_label
      and module Condition_label := Condition_label
      and module Timer := Timer

module Make : F
