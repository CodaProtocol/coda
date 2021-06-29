open Core
open Signed
open Unsigned
open Num_util

include Mina_numbers.Nat.Make32 ()

module Time = Block_time

let of_time_exn ~(constants : Constants.t) t : t =
  if Time.(t < constants.genesis_state_timestamp) then
    raise
      (Invalid_argument
         "Epoch.of_time: time is earlier than genesis block timestamp") ;
  let time_since_genesis = Time.diff t constants.genesis_state_timestamp in
  uint32_of_int64
    Int64.Infix.(
      Time.Span.to_ms time_since_genesis
      / Time.Span.(to_ms constants.epoch_duration))

let start_time ~(constants : Constants.t) (epoch : t) =
  let ms =
    let open Int64.Infix in
    Block_time.Span.to_ms
      Block_time.(to_span_since_epoch constants.genesis_state_timestamp)
    + (int64_of_uint32 epoch * Block_time.Span.(to_ms constants.epoch_duration))
  in
  Block_time.of_span_since_epoch (Block_time.Span.of_ms ms)

let end_time ~(constants : Constants.t) (t : t) =
  Time.add (start_time t ~constants) constants.epoch_duration

let slot_start_time ~(constants : Constants.t) (epoch : t) (slot : Slot.t) =
  Block_time.add
    (start_time epoch ~constants)
    (Block_time.Span.of_ms
       Int64.Infix.(
         int64_of_uint32 slot * Time.Span.to_ms constants.slot_duration_ms))

let slot_end_time ~(constants : Constants.t) (epoch : t) (slot : Slot.t) =
  Time.add (slot_start_time epoch slot ~constants) constants.slot_duration_ms

let epoch_and_slot_of_time_exn ~(constants : Constants.t) tm : t * Slot.t =
  let epoch = of_time_exn tm ~constants in
  let time_since_epoch = Block_time.diff tm (start_time epoch ~constants) in
  let slot =
    uint32_of_int64
    @@ Int64.Infix.(
         Time.Span.to_ms time_since_epoch
         / Time.Span.to_ms constants.slot_duration_ms)
  in
  (epoch, slot)

let diff_in_slots ~(constants : Constants.t) ((epoch, slot) : t * Slot.t)
    ((epoch', slot') : t * Slot.t) : int64 =
  let ( < ) x y =
    let open Core_kernel in
    Int64.compare x y < 0
  in
  let ( > ) x y =
    let open Core_kernel in
    Int64.compare x y > 0
  in
  let open Int64.Infix in
  let of_uint32 = UInt32.to_int64 in
  let epoch, slot = (of_uint32 epoch, of_uint32 slot) in
  let epoch', slot' = (of_uint32 epoch', of_uint32 slot') in
  let epoch_size = UInt32.to_int64 constants.epoch_size in
  let epoch_diff = epoch - epoch' in
  if epoch_diff > 0L then
    ((epoch_diff - 1L) * epoch_size) + slot + (epoch_size - slot')
  else if epoch_diff < 0L then
    ((epoch_diff + 1L) * epoch_size) - (epoch_size - slot) - slot'
  else slot - slot'

let%test_unit "test diff_in_slots" =
  let constants = Lazy.force Constants.for_unit_tests in
  let open Int64.Infix in
  let ( !^ ) = UInt32.of_int in
  let ( !@ ) = Fn.compose ( !^ ) Int64.to_int in
  let epoch_size_int64 = UInt32.to_int64 constants.epoch_size in
  [%test_eq: int64] (diff_in_slots (!^0, !^5) (!^0, !^0) ~constants) 5L ;
  [%test_eq: int64] (diff_in_slots (!^3, !^23) (!^3, !^20) ~constants) 3L ;
  [%test_eq: int64]
    (diff_in_slots (!^4, !^4) (!^3, !^0) ~constants)
    (epoch_size_int64 + 4L) ;
  [%test_eq: int64]
    (diff_in_slots (!^5, !^2) (!^4, !@(epoch_size_int64 - 3L)) ~constants)
    5L ;
  [%test_eq: int64]
    (diff_in_slots (!^6, !^42) (!^2, !^16) ~constants)
    ((epoch_size_int64 * 3L) + 42L + (epoch_size_int64 - 16L)) ;
  [%test_eq: int64]
    (diff_in_slots (!^2, !@(epoch_size_int64 - 1L)) (!^3, !^4) ~constants)
    (0L - 5L) ;
  [%test_eq: int64]
    (diff_in_slots (!^1, !^3) (!^7, !^27) ~constants)
    (0L - ((epoch_size_int64 * 5L) + (epoch_size_int64 - 3L) + 27L))

let incr ~(constants : Constants.t) ((epoch, slot) : t * Slot.t) =
  let open UInt32 in
  if
    Slot.equal slot
      (sub (Mina_numbers.Length.to_uint32 constants.epoch_size) one)
  then (add epoch one, zero)
  else (epoch, add slot one)
