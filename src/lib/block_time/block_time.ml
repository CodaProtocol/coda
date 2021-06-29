[%%import
"/src/config.mlh"]

open Core_kernel
open Snark_params
open Tick
open Unsigned_extended
open Snark_bits

module Time = struct
  (* Milliseconds since epoch *)
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = UInt64.Stable.V1.t
      [@@deriving sexp, compare, equal, hash, yojson]

      let to_latest = Fn.id

      module T = struct
        type typ = t [@@deriving sexp, compare, hash]

        type t = typ [@@deriving sexp, compare, hash]
      end

      include Hashable.Make (T)
    end
  end]

  let max_value = UInt64.max_int

  let zero = UInt64.zero

  module Controller = struct
    [%%if
    time_offsets]

    type t = unit -> Time.Span.t [@@deriving sexp]

    (* NB: All instances are identical by construction (see basic below). *)
    let equal _ _ = true

    (* NB: All instances are identical by construction (see basic below). *)
    let compare _ _ = 0

    let time_offset = ref None

    let setting_enabled = ref None

    let disable_setting_offset () = setting_enabled := Some false

    let enable_setting_offset () =
      match !setting_enabled with
      | None ->
          setting_enabled := Some true
      | Some true ->
          ()
      | Some false ->
          failwith
            "Cannot enable time offset mutations; it has been explicitly \
             disabled"

    let set_time_offset offset =
      match !setting_enabled with
      | Some true ->
          time_offset := Some offset
      | None | Some false ->
          failwith "Cannot mutate the time offset"

    let create offset = offset

    let basic ~logger () =
      match !time_offset with
      | Some offset ->
          offset
      | None ->
          let offset =
            let env_offset =
              match Core.Sys.getenv "CODA_TIME_OFFSET" with
              | Some tm ->
                  Int.of_string tm
              | None ->
                  [%log debug]
                    "Environment variable CODA_TIME_OFFSET not found, using \
                     default of 0" ;
                  0
            in
            Core_kernel.Time.Span.of_int_sec env_offset
          in
          time_offset := Some offset ;
          offset

    let get_time_offset ~logger = basic ~logger ()

    [%%else]

    type t = unit [@@deriving sexp, equal, compare]

    let create () = ()

    let basic ~logger:_ = ()

    let disable_setting_offset () = ()

    let enable_setting_offset () = ()

    let set_time_offset _ = failwith "Cannot mutate the time offset"

    let get_time_offset _ = Core_kernel.Time.Span.of_int_sec 0

    [%%endif]
  end

  module B = Bits
  module Bits = Bits.UInt64
  include B.Snarkable.UInt64 (Tick)

  module Checked = struct
    type t = Unpacked.var

    module N = Mina_numbers.Nat.Make_checked (UInt64) (Bits)

    let op f (x : t) (y : t) : (Boolean.var, 'a) Checked.t =
      let g = Fn.compose N.of_bits Unpacked.var_to_bits in
      f (g x) (g y)

    let ( = ) x = op N.( = ) x

    let ( <= ) x = op N.( <= ) x

    let ( >= ) x = op N.( >= ) x

    let ( < ) x = op N.( < ) x

    let ( > ) x = op N.( > ) x
  end

  module Span = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = UInt64.Stable.V1.t
        [@@deriving sexp, compare, equal, hash, yojson]

        let to_latest = Fn.id
      end
    end]

    module Bits = B.UInt64
    include B.Snarkable.UInt64 (Tick)

    let of_time_span s = UInt64.of_int64 (Int64.of_float (Time.Span.to_ms s))

    let to_time_span s = Time.Span.of_ms (Int64.to_float (UInt64.to_int64 s))

    let to_time_ns_span s =
      Time_ns.Span.of_ms (Int64.to_float (UInt64.to_int64 s))

    let to_string_hum s = to_time_ns_span s |> Time_ns.Span.to_string_hum

    let to_ms = UInt64.to_int64

    let of_ms = UInt64.of_int64

    let ( + ) = UInt64.Infix.( + )

    let ( - ) = UInt64.Infix.( - )

    let ( * ) = UInt64.Infix.( * )

    let ( < ) = UInt64.( < )

    let ( > ) = UInt64.( > )

    let ( = ) = UInt64.( = )

    let ( <= ) = UInt64.( <= )

    let ( >= ) = UInt64.( >= )

    let min = UInt64.min

    let zero = UInt64.zero
  end

  include Comparable.Make (Stable.Latest)
  include Hashable.Make (Stable.Latest)

  let of_time t =
    UInt64.of_int64
      (Int64.of_float (Time.Span.to_ms (Time.to_span_since_epoch t)))

  let to_time t =
    Time.of_span_since_epoch
      (Time.Span.of_ms (Int64.to_float (UInt64.to_int64 t)))

  [%%if
  time_offsets]

  let now offset = of_time (Time.sub (Time.now ()) (offset ()))

  [%%else]

  let now _ = of_time (Time.now ())

  [%%endif]

  let field_var_to_unpacked (x : Tick.Field.Var.t) =
    Tick.Field.Checked.unpack ~length:64 x

  let epoch = of_time Time.epoch

  let add x y = UInt64.add x y

  let diff x y = UInt64.sub x y

  let sub x y = UInt64.sub x y

  let to_span_since_epoch t = diff t epoch

  let of_span_since_epoch s = UInt64.add s epoch

  let diff_checked x y =
    let pack = Tick.Field.Var.project in
    Span.unpack_var Tick.Field.Checked.(pack x - pack y)

  let modulus t span = UInt64.rem t span

  let unpacked_to_number var =
    let bits = Span.Unpacked.var_to_bits var in
    Number.of_bits (bits :> Boolean.var list)

  let to_int64 = Fn.compose Span.to_ms to_span_since_epoch

  let of_int64 = Fn.compose of_span_since_epoch Span.of_ms

  let to_string = Fn.compose Int64.to_string to_int64

  [%%if
  time_offsets]

  let to_string_system_time (offset : Controller.t) (t : t) : string =
    let t2 : t =
      of_span_since_epoch
        Span.(to_span_since_epoch t + of_time_span (offset ()))
    in
    Int64.to_string (to_int64 t2)

  [%%else]

  let to_string_system_time _ = Fn.compose Int64.to_string to_int64

  [%%endif]

  let of_string_exn string =
    Int64.of_string string |> Span.of_ms |> of_span_since_epoch

  let gen_incl time_beginning time_end =
    let open Quickcheck.Let_syntax in
    let time_beginning_int64 = to_int64 time_beginning in
    let time_end_int64 = to_int64 time_end in
    let%map int64_time_span =
      Int64.(gen_incl time_beginning_int64 time_end_int64)
    in
    of_span_since_epoch @@ Span.of_ms int64_time_span

  let gen =
    let open Quickcheck.Let_syntax in
    let%map int64_time_span = Int64.(gen_incl zero max_value) in
    of_span_since_epoch @@ Span.of_ms int64_time_span
end

include Time
module Timeout = Timeout_lib.Make (Time)
