open Core_kernel
open Async_kernel
open Prometheus
open Namespace

module type Metric_spec_intf = sig
  val subsystem : string

  val name : string

  val help : string
end

module type Bucketed_average_spec_intf = sig
  include Metric_spec_intf

  val bucket_interval : Core.Time.Span.t

  val num_buckets : int

  val render_average : (float * int) list -> float
end

module type Time_average_spec_intf = sig
  include Metric_spec_intf

  val tick_interval : Core.Time.Span.t

  val rolling_interval : Core.Time.Span.t
end

module type Average_metric_intf = sig
  type datum

  val v : Gauge.t

  val update : datum -> unit

  val clear : unit -> unit
end

module Rolling_average (Spec : Metric_spec_intf) () :
  Average_metric_intf with type datum := float = struct
  open Spec

  let v = Gauge.v name ~subsystem ~namespace ~help

  let count = ref 0

  let moving_average = ref None

  let clear () =
    count := 0 ;
    moving_average := None

  let update_avg ~avg ~count ~value =
    ((avg /. count) +. value) *. (count +. 1.0)

  let update value =
    (moving_average :=
       match !moving_average with
       | None ->
           Some value
       | Some avg ->
           Some (update_avg ~avg ~value ~count:(Float.of_int !count))) ;
    incr count
end

module Moving_bucketed_average (Spec : Bucketed_average_spec_intf) () :
  Average_metric_intf with type datum := float = struct
  open Spec

  let v = Gauge.v name ~subsystem ~namespace ~help

  let empty_bucket_entry = (0.0, 0)

  let empty_buckets () = List.init num_buckets ~f:(Fn.const empty_bucket_entry)

  let buckets = ref None

  let clear () = buckets := Some (empty_buckets ())

  let update datum =
    if Option.is_none !buckets then buckets := Some (empty_buckets ()) ;
    match Option.value_exn !buckets with
    | [] ->
        failwith "Moving_bucketed_average buckets are malformed"
    | (value, num_entries) :: t ->
        buckets := Some ((value +. datum, num_entries + 1) :: t)

  let () =
    let rec tick () =
      upon
        (after (Time_ns.Span.of_ns @@ Time.Span.to_ns bucket_interval))
        (fun () ->
          if Option.is_none !buckets then buckets := Some (empty_buckets ()) ;
          let buckets_val = Option.value_exn !buckets in
          Gauge.set v (render_average buckets_val) ;
          buckets :=
            Some (empty_bucket_entry :: List.take buckets_val (num_buckets - 1)) ;
          tick () )
    in
    tick ()
end

module Moving_time_average (Spec : Time_average_spec_intf) () :
  Average_metric_intf with type datum := Core.Time.Span.t = struct
  open Spec

  let () =
    let open Time.Span in
    let ( = ) = Float.equal in
    let ( mod ) = Float.mod_float in
    if not (to_ns rolling_interval mod to_ns tick_interval = 0.0) then
      failwith
        "invalid intervals provided to Moving_time_average -- the \
         tick_interval does not evenly divide the rolling_interval"

  include Moving_bucketed_average (struct
              include Spec

              let bucket_interval = tick_interval

              let num_buckets =
                Float.to_int
                  ( Time.Span.to_ns rolling_interval
                  /. Time.Span.to_ns tick_interval )

              let render_average buckets =
                let total_sum, count_sum =
                  List.fold buckets ~init:(0.0, 0)
                    ~f:(fun (total_sum, count_sum) (total, count) ->
                      (total_sum +. total, count_sum + count) )
                in
                total_sum /. Float.of_int count_sum
            end)
            ()

  let update span = update (Core.Time.Span.to_sec span)
end
