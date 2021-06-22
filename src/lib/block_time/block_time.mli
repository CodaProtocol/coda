open Async_kernel
open Core_kernel
open Snark_params
open Snark_bits

module Time : sig
  type t [@@deriving sexp, compare, yojson]

  val zero : t

  val max_value : t

  include Comparable.S with type t := t

  include Hashable.S with type t := t

  module Controller : sig
    type t [@@deriving sexp, equal, compare]

    val create : t -> t

    val basic : logger:Logger.t -> t

    (** Override the time offset set by the [MINA_TIME_OFFSET] environment
        variable for all block time controllers.
        [enable_setting_offset] must have been called first, and
        [disable_setting_offset] must not have been called, otherwise this
        raises a [Failure].
    *)
    val set_time_offset : Time.Span.t -> unit

    (** Get the current time offset, either from the [MINA_TIME_OFFSET]
        environment variable, or as last set by [set_time_offset].
    *)
    val get_time_offset : logger:Logger.t -> Time.Span.t

    (** Disallow setting the time offset. This should be run at every
        entrypoint which does not explicitly need to update the time offset.
    *)
    val disable_setting_offset : unit -> unit

    (** Allow setting the time offset. This may only be run if
        [disable_setting_offset] has not already been called, otherwise it will
        raise a [Failure].
    *)
    val enable_setting_offset : unit -> unit
  end

  [%%versioned:
  module Stable : sig
    [@@@no_toplevel_latest_type]

    module V1 : sig
      type nonrec t = t [@@deriving sexp, compare, equal, hash, yojson]

      include Hashable.S with type t := t
    end
  end]

  module Bits : Bits_intf.Convertible_bits with type t := t

  include
    Tick.Snarkable.Bits.Faithful
      with type Unpacked.value = t
       and type Packed.value = t
       and type Packed.var = private Tick.Field.Var.t

  module Checked : sig
    open Snark_params.Tick

    type t = Unpacked.var

    val ( = ) : t -> t -> (Boolean.var, _) Checked.t

    val ( < ) : t -> t -> (Boolean.var, _) Checked.t

    val ( > ) : t -> t -> (Boolean.var, _) Checked.t

    val ( <= ) : t -> t -> (Boolean.var, _) Checked.t

    val ( >= ) : t -> t -> (Boolean.var, _) Checked.t
  end

  module Span : sig
    type t [@@deriving sexp, compare, equal, yojson]

    module Stable : sig
      module V1 : sig
        type nonrec t = t
        [@@deriving bin_io, equal, sexp, compare, hash, yojson, version]
      end
    end

    val of_time_span : Time.Span.t -> t

    val to_time_span : t -> Time.Span.t

    module Bits : Bits_intf.Convertible_bits with type t := t

    include
      Tick.Snarkable.Bits.Faithful
        with type Unpacked.value = t
         and type Packed.value = t

    val to_time_ns_span : t -> Core.Time_ns.Span.t

    val to_string_hum : t -> string

    val to_ms : t -> Int64.t

    val of_ms : Int64.t -> t

    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( * ) : t -> t -> t

    val ( < ) : t -> t -> bool

    val ( > ) : t -> t -> bool

    val ( = ) : t -> t -> bool

    val ( <= ) : t -> t -> bool

    val ( >= ) : t -> t -> bool

    val min : t -> t -> t

    val zero : t
  end

  val field_var_to_unpacked :
    Tick.Field.Var.t -> (Unpacked.var, _) Tick.Checked.t

  val diff_checked :
    Unpacked.var -> Unpacked.var -> (Span.Unpacked.var, _) Tick.Checked.t

  val unpacked_to_number : Span.Unpacked.var -> Tick.Number.t

  val add : t -> Span.t -> t

  val diff : t -> t -> Span.t

  val sub : t -> Span.t -> t

  val to_span_since_epoch : t -> Span.t

  val of_span_since_epoch : Span.t -> t

  val modulus : t -> Span.t -> Span.t

  val of_time : Time.t -> t

  val to_time : t -> Time.t

  val now : Controller.t -> t

  val to_int64 : t -> Int64.t

  val of_int64 : Int64.t -> t

  val to_string : t -> string

  (** Strip time offset *)
  val to_string_system_time : Controller.t -> t -> string

  val of_string_exn : string -> t

  val gen_incl : t -> t -> t Quickcheck.Generator.t

  val gen : t Quickcheck.Generator.t
end

include module type of Time with type t = Time.t

module Timeout : sig
  type 'a t

  type time

  val create : Controller.t -> Span.t -> f:(time -> 'a) -> 'a t

  val to_deferred : 'a t -> 'a Async_kernel.Deferred.t

  val peek : 'a t -> 'a option

  val cancel : Controller.t -> 'a t -> 'a -> unit

  val remaining_time : 'a t -> Span.t

  val await :
       timeout_duration:Span.t
    -> Controller.t
    -> 'a Deferred.t
    -> [ `Ok of 'a | `Timeout ] Deferred.t

  val await_exn :
    timeout_duration:Span.t -> Controller.t -> 'a Deferred.t -> 'a Deferred.t
end
with type time := t
