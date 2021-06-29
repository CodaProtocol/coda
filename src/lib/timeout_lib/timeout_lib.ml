open Async_kernel
open Core_kernel

module type Time_intf = sig
  type t

  module Span : sig
    type t

    val to_time_ns_span : t -> Core.Time_ns.Span.t

    val ( - ) : t -> t -> t
  end

  module Controller : sig
    type t
  end

  val now : Controller.t -> t

  val diff : t -> t -> Span.t
end

module Timeout_intf (Time : Time_intf) = struct
  module type S = sig
    type 'a t

    val create : Time.Controller.t -> Time.Span.t -> f:(Time.t -> 'a) -> 'a t

    val to_deferred : 'a t -> 'a Async_kernel.Deferred.t

    val peek : 'a t -> 'a option

    val cancel : Time.Controller.t -> 'a t -> 'a -> unit

    val remaining_time : 'a t -> Time.Span.t

    val await :
         timeout_duration:Time.Span.t
      -> Time.Controller.t
      -> 'a Deferred.t
      -> [`Ok of 'a | `Timeout] Deferred.t

    val await_exn :
         timeout_duration:Time.Span.t
      -> Time.Controller.t
      -> 'a Deferred.t
      -> 'a Deferred.t
  end
end

module Make (Time : Time_intf) : Timeout_intf(Time).S = struct
  type 'a t =
    { deferred: 'a Deferred.t
    ; cancel: 'a -> unit
    ; start_time: Time.t
    ; span: Time.Span.t
    ; ctrl: Time.Controller.t }

  let create ctrl span ~f:action =
    let open Deferred.Let_syntax in
    let cancel_ivar = Ivar.create () in
    let timeout = after (Time.Span.to_time_ns_span span) >>| fun () -> None in
    let deferred =
      Deferred.any [Ivar.read cancel_ivar; timeout]
      >>| function None -> action (Time.now ctrl) | Some x -> x
    in
    let cancel value = Ivar.fill_if_empty cancel_ivar (Some value) in
    {ctrl; deferred; cancel; start_time= Time.now ctrl; span}

  let to_deferred {deferred; _} = deferred

  let peek {deferred; _} = Deferred.peek deferred

  let cancel _ {cancel; _} value = cancel value

  let remaining_time {ctrl: _; start_time; span; _} =
    let current_time = Time.now ctrl in
    let time_elapsed = Time.diff current_time start_time in
    Time.Span.(span - time_elapsed)

  let await ~timeout_duration time_controller deferred =
    let timeout =
      Deferred.create (fun ivar ->
          ignore
            ( create time_controller timeout_duration ~f:(fun x ->
                  if Ivar.is_full ivar then
                    [%log' error (Logger.create ())] "Ivar.fill bug is here!" ;
                  Ivar.fill_if_empty ivar x )
              : unit t ) )
    in
    Deferred.(
      choose
        [choice deferred (fun x -> `Ok x); choice timeout (Fn.const `Timeout)])

  let await_exn ~timeout_duration time_controller deferred =
    match%map await ~timeout_duration time_controller deferred with
    | `Timeout ->
        failwith "timeout"
    | `Ok x ->
        x
end

module Core_time = Make (struct
  include (
    Core.Time :
      module type of Core.Time
      with module Span := Core.Time.Span
       and type underlying = float )

  module Controller = struct
    type t = unit
  end

  module Span = struct
    include Core.Time.Span

    let to_time_ns_span = Fn.compose Core.Time_ns.Span.of_ns to_ns
  end

  let diff x y =
    let x_ns = Span.to_ns @@ to_span_since_epoch x in
    let y_ns = Span.to_ms @@ to_span_since_epoch y in
    Span.of_ns (x_ns -. y_ns)
end)

module Core_time_ns = Make (struct
  include (
    Core.Time_ns :
      module type of Core.Time_ns with module Span := Core.Time_ns.Span )

  module Controller = struct
    type t = unit
  end

  module Span = struct
    include Core.Time_ns.Span

    let to_time_ns_span = Fn.id
  end

  let diff x y =
    let x_ns = Span.to_ns @@ to_span_since_epoch x in
    let y_ns = Span.to_ms @@ to_span_since_epoch y in
    Span.of_ns (x_ns -. y_ns)
end)
