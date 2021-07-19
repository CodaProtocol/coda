open Async
open Core
open Integration_test_lib
module Timeout = Timeout_lib.Core_time
module Node = Swarm_network.Node

(* TODO: Implement local engine logging *)

type t =
  { logger : Logger.t
  ; event_writer : (Node.t * Event_type.event) Pipe.Writer.t
  ; event_reader : (Node.t * Event_type.event) Pipe.Reader.t
  }

let event_reader { event_reader; _ } = event_reader

let create ~logger ~(network : Swarm_network.t) =
  [%log info] "docker_pipe_log_engine: create %s" network.namespace ;
  let event_reader, event_writer = Pipe.create () in
  Deferred.Or_error.return { logger; event_reader; event_writer }

let destroy t : unit Deferred.Or_error.t =
  let { logger; event_reader = _; event_writer } = t in
  Pipe.close event_writer ;
  [%log debug] "subscription deleted" ;
  Deferred.Or_error.error_string "subscription deleted"
