(** This module provides a functor for instantiating async supervisors which is
 *  capable of monitoring and dispatching work to a single worker. The module
 *  allows synchronous checking of the worker's state (is it processing work
 *  or not), and prevents parallel dispatching of work to that worker.
 *)

open Async_kernel
open Core_kernel

(** The interface shared by both the worker and the supervisor which wraps it. *)
module type Base_intf = sig
  type t

  type create_args

  type input

  type output

  val create : create_args -> t

  val close : t -> unit Deferred.t
end

(** The interface of the worker. Extends the base interface with a [perform]
 *  function.
 *)
module type Worker_intf = sig
  include Base_intf

  val perform : t -> input -> output Deferred.t
end

(** The interface of the supervisor constructed by the [Make] functor. *)
module type S = sig
  include Base_intf

  val is_working : t -> bool

  val dispatch : t -> input -> output Deferred.t
end

(** [Make (Worker)] creates a supervisor which wraps dispatches to [Worker]. *)
module Make (Worker : Worker_intf) :
  S
    with type create_args := Worker.create_args
     and type input := Worker.input
     and type output := Worker.output = struct
  type t =
    { mutable thread : Worker.output Deferred.t option; worker : Worker.t }

  let create args = { thread = None; worker = Worker.create args }

  let is_working t =
    Option.value_map t.thread ~default:false ~f:Deferred.is_determined

  let assert_not_working t =
    if is_working t then failwith "cannot dispatch to busy worker"

  let dispatch t work =
    assert_not_working t ;
    let thread = Worker.perform t.worker work in
    t.thread <- Some thread ;
    let%map x = thread in
    t.thread <- None ;
    x

  let close t = assert_not_working t ; Worker.close t.worker
end
