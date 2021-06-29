open Unsigned
open Core
open Snark_params.Tick
module T = Mina_numbers.Global_slot
module Length = Mina_numbers.Length

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('slot_number, 'slots_per_epoch) t =
        {slot_number: 'slot_number; slots_per_epoch: 'slots_per_epoch}
      [@@deriving sexp, equal, compare, hash, yojson, hlist]
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t = (T.Stable.V1.t, Length.Stable.V1.t) Poly.Stable.V1.t
    [@@deriving sexp, equal, compare, hash, yojson]

    let to_latest = Fn.id
  end
end]

type value = t [@@deriving sexp, compare, hash, yojson]

type var = (T.Checked.t, Length.Checked.t) Poly.t

let data_spec = Data_spec.[T.Checked.typ; Length.Checked.typ]

let typ =
  Typ.of_hlistable data_spec ~var_to_hlist:Poly.to_hlist
    ~var_of_hlist:Poly.of_hlist ~value_to_hlist:Poly.to_hlist
    ~value_of_hlist:Poly.of_hlist

let to_input (t : value) =
  Random_oracle.Input.bitstrings
    [|T.to_bits t.slot_number; Length.to_bits t.slots_per_epoch|]

let gen ~(constants : Constants.t) =
  let open Quickcheck.Let_syntax in
  let slots_per_epoch = constants.slots_per_epoch in
  let%map slot_number = T.gen in
  {Poly.slot_number; slots_per_epoch}

let create ~(constants : Constants.t) ~(epoch : Epoch.t) ~(slot : Slot.t) : t =
  { slot_number= UInt32.Infix.(slot + (constants.slots_per_epoch * epoch))
  ; slots_per_epoch= constants.slots_per_epoch }

let of_epoch_and_slot ~(constants : Constants.t) (epoch, slot) =
  create ~epoch ~slot ~constants

let zero ~(constants : Constants.t) : t =
  {slot_number= T.zero; slots_per_epoch= constants.slots_per_epoch}

let slot_number {Poly.slot_number; _} = slot_number

let slots_per_epoch {Poly.slots_per_epoch; _} = slots_per_epoch

let to_bits (t : t) =
  List.concat_map ~f:T.to_bits [t.slot_number; t.slots_per_epoch]

let epoch (t : t) = UInt32.Infix.(t.slot_number / t.slots_per_epoch)

let slot (t : t) = UInt32.Infix.(t.slot_number mod t.slots_per_epoch)

let to_epoch_and_slot t = (epoch t, slot t)

let ( + ) (x : t) n : t = {x with slot_number= T.add x.slot_number (T.of_int n)}

let ( < ) (t : t) (t' : t) = UInt32.compare t.slot_number t'.slot_number < 0

let ( - ) (t : t) (t' : t) = T.sub t.slot_number t'.slot_number

let max (t1 : t) (t2 : t) = if t1 < t2 then t2 else t1

let succ (t : t) = {t with slot_number= T.succ t.slot_number}

let of_slot_number ~(constants : Constants.t) slot_number =
  {Poly.slot_number; slots_per_epoch= constants.slots_per_epoch}

let start_time ~(constants : Constants.t) t =
  let epoch, slot = to_epoch_and_slot t in
  Epoch.slot_start_time epoch slot ~constants

let end_time ~(constants : Constants.t) t =
  let epoch, slot = to_epoch_and_slot t in
  Epoch.slot_end_time epoch slot ~constants

let time_hum t =
  let epoch, slot = to_epoch_and_slot t in
  sprintf "epoch=%d, slot=%d" (Epoch.to_int epoch) (Slot.to_int slot)

let of_time_exn ~(constants : Constants.t) time =
  of_epoch_and_slot
    (Epoch.epoch_and_slot_of_time_exn time ~constants)
    ~constants

let diff ~(constants : Constants.t) (t : t) (other_epoch, other_slot) =
  let open UInt32.Infix in
  let epoch, slot = to_epoch_and_slot t in
  let old_epoch =
    epoch - other_epoch
    - UInt32.(of_int @@ if compare other_slot slot > 0 then 1 else 0)
  in
  let old_slot =
    (slot - other_slot) mod Length.to_uint32 constants.epoch_size
  in
  of_epoch_and_slot (old_epoch, old_slot) ~constants

module Checked = struct
  type t = var

  let ( < ) (t : t) (t' : t) = T.Checked.(t.slot_number < t'.slot_number)

  let of_slot_number ~(constants : Constants.var) slot_number : t =
    {slot_number; slots_per_epoch= constants.slots_per_epoch}

  let to_bits (t : t) =
    let open Bitstring_lib.Bitstring.Lsb_first in
    let%map slot_number = T.Checked.to_bits t.slot_number
    and slots_per_epoch = Length.Checked.to_bits t.slots_per_epoch in
    List.concat_map ~f:to_list [slot_number; slots_per_epoch] |> of_list

  let to_input (var : t) =
    let s = Bitstring_lib.Bitstring.Lsb_first.to_list in
    let%map slot_number = T.Checked.to_bits var.slot_number
    and slots_per_epoch = Length.Checked.to_bits var.slots_per_epoch in
    Random_oracle.Input.bitstrings
      (Array.map ~f:s [|slot_number; slots_per_epoch|])

  let to_epoch_and_slot (t : t) :
      (Epoch.Checked.t * Slot.Checked.t, _) Checked.t =
    make_checked (fun () ->
        let open Snarky_integer in
        let epoch, slot =
          Integer.div_mod ~m
            (T.Checked.to_integer t.slot_number)
            (Length.Checked.to_integer t.slots_per_epoch)
        in
        ( Epoch.Checked.Unsafe.of_integer epoch
        , Slot.Checked.Unsafe.of_integer slot ) )

  let sub (t : t) (t' : t) = T.Checked.sub t.slot_number t'.slot_number
end

module For_tests = struct
  let of_global_slot (t : t) slot_number : t = {t with slot_number}
end
