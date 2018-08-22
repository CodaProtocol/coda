open Core_kernel
open Signed
open Unsigned
open Coda_numbers
open Currency

module type Inputs_intf = sig
  module Proof : sig
    type t [@@deriving bin_io, sexp]
  end

  module Ledger_builder_diff : sig
    type t [@@deriving bin_io, sexp]
  end

  module Time : sig
    type t

    module Span : sig
      type t

      val to_ms : t -> Int64.t

      val of_ms : Int64.t -> t

      val ( + ) : t -> t -> t

      val ( * ) : t -> t -> t
    end

    val ( < ) : t -> t -> bool

    val ( >= ) : t -> t -> bool

    val diff : t -> t -> Span.t

    val to_span_since_epoch : t -> Span.t

    val of_span_since_epoch : Span.t -> t

    val add : t -> Span.t -> t
  end

  val genesis_state_timestamp : Time.t

  val genesis_ledger_total_currency : Amount.t

  val coinbase : Amount.t

  val slot_interval : Time.Span.t

  val epoch_size : UInt32.t
end

module Segment_id = Nat.Make32 ()

let uint32_of_int64 x = x |> Int64.to_int64 |> UInt32.of_int64

let int64_of_uint32 x = x |> UInt32.to_int64 |> Int64.of_int64

module Make (Inputs : Inputs_intf) : Mechanism.S = struct
  module Proof = Inputs.Proof
  module Ledger_builder_diff = Inputs.Ledger_builder_diff
  module Time = Inputs.Time

  module Epoch = struct
    include Segment_id

    let size = Inputs.epoch_size

    let interval =
      Time.Span.of_ms
        Int64.Infix.(
          Time.Span.to_ms Inputs.slot_interval * int64_of_uint32 size)

    let of_time_exn t : t =
      if Time.(t < Inputs.genesis_state_timestamp) then
        raise
          (Invalid_argument
             "Epoch.of_time: time is less than genesis block timestamp") ;
      let time_since_genesis = Time.diff t Inputs.genesis_state_timestamp in
      uint32_of_int64
        Int64.Infix.(
          Time.Span.to_ms time_since_genesis / Time.Span.to_ms interval)

    let start_time (epoch: t) =
      let ms =
        let open Int64.Infix in
        Time.Span.to_ms
          (Time.to_span_since_epoch Inputs.genesis_state_timestamp)
        + (int64_of_uint32 epoch * Time.Span.to_ms interval)
      in
      Time.of_span_since_epoch (Time.Span.of_ms ms)

    let end_time (epoch: t) = Time.add (start_time epoch) interval

    module Slot = struct
      include Segment_id

      let interval = Inputs.slot_interval
    end

    let slot_start_time (epoch: t) (slot: Slot.t) =
      Time.add (start_time epoch)
        (Time.Span.of_ms
           Int64.Infix.(int64_of_uint32 slot * Time.Span.to_ms Slot.interval))

    let slot_end_time (epoch: t) (slot: Slot.t) =
      Time.add (slot_start_time epoch slot) Slot.interval

    let epoch_and_slot_of_time_exn t : t * Slot.t =
      let epoch = of_time_exn t in
      let time_since_epoch = Time.diff t (start_time epoch) in
      let slot =
        uint32_of_int64
        @@
        Int64.Infix.(
          Time.Span.to_ms time_since_epoch / Time.Span.to_ms Slot.interval)
      in
      (epoch, slot)
  end

  module Consensus_transition_data = struct
    type ('epoch, 'slot, 'amount) t =
      {epoch: 'epoch; slot: 'slot; total_currency_diff: 'amount}
    [@@deriving sexp, bin_io, eq, compare]

    type value = (Epoch.t, Epoch.Slot.t, Amount.t) t
    [@@deriving sexp, bin_io, eq, compare]

    type var = (Epoch.Unpacked.var, Epoch.Slot.Unpacked.var, Amount.var) t

    let genesis =
      { epoch= Epoch.zero
      ; slot= Epoch.Slot.zero
      ; total_currency_diff= Amount.zero }

    let to_hlist {epoch; slot; total_currency_diff} =
      Nanobit_base.H_list.[epoch; slot; total_currency_diff]

    let of_hlist :
           (unit, 'epoch -> 'slot -> 'amount -> unit) Nanobit_base.H_list.t
        -> ('epoch, 'slot, 'amount) t =
     fun Nanobit_base.H_list.([epoch; slot; total_currency_diff]) ->
      {epoch; slot; total_currency_diff}

    let data_spec =
      let open Snark_params.Tick.Data_spec in
      [Epoch.Unpacked.typ; Epoch.Slot.Unpacked.typ; Amount.typ]

    let typ =
      Snark_params.Tick.Typ.of_hlistable data_spec ~var_to_hlist:to_hlist
        ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    let fold {epoch; slot; total_currency_diff} =
      let open Nanobit_base.Util in
      Epoch.Bits.fold epoch +> Epoch.Slot.Bits.fold slot
      +> Amount.fold total_currency_diff

    let var_to_bits {epoch; slot; total_currency_diff} =
      Epoch.Unpacked.var_to_bits epoch
      @ Epoch.Slot.Unpacked.var_to_bits slot
      @ ( total_currency_diff |> Amount.var_to_bits
        |> Bitstring_lib.Bitstring.Lsb_first.to_list )

    let bit_length =
      Epoch.length_in_bits + Epoch.Slot.length_in_bits + Amount.length
  end

  module Consensus_state = struct
    type ('length, 'epoch, 'slot, 'amount) t =
      { length: 'length
      ; current_epoch: 'epoch
      ; current_slot: 'slot
      ; total_currency: 'amount }
    [@@deriving sexp, bin_io, eq, compare, hash]

    type value = (Length.t, Epoch.t, Epoch.Slot.t, Amount.t) t
    [@@deriving sexp, bin_io, eq, compare, hash]

    type var =
      ( Length.Unpacked.var
      , Epoch.Unpacked.var
      , Epoch.Slot.Unpacked.var
      , Amount.var )
      t

    let genesis =
      { length= Length.zero
      ; current_epoch= Epoch.zero
      ; current_slot= Epoch.Slot.zero
      ; total_currency= Inputs.genesis_ledger_total_currency }

    let to_hlist {length; current_epoch; current_slot; total_currency} =
      Nanobit_base.H_list.[length; current_epoch; current_slot; total_currency]

    let of_hlist :
           ( unit
           , 'length -> 'epoch -> 'slot -> 'amount -> unit )
           Nanobit_base.H_list.t
        -> ('length, 'epoch, 'slot, 'amount) t =
     fun Nanobit_base.H_list.([ length
                              ; current_epoch
                              ; current_slot
                              ; total_currency ]) ->
      {length; current_epoch; current_slot; total_currency}

    let data_spec =
      let open Snark_params.Tick.Data_spec in
      [ Length.Unpacked.typ
      ; Epoch.Unpacked.typ
      ; Epoch.Slot.Unpacked.typ
      ; Amount.typ ]

    let typ =
      Snark_params.Tick.Typ.of_hlistable data_spec ~var_to_hlist:to_hlist
        ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    let var_to_bits {length; current_epoch; current_slot; total_currency} =
      Snark_params.Tick.Let_syntax.return
        ( Length.Unpacked.var_to_bits length
        @ Epoch.Unpacked.var_to_bits current_epoch
        @ Epoch.Slot.Unpacked.var_to_bits current_slot
        @ ( total_currency |> Amount.var_to_bits
          |> Bitstring_lib.Bitstring.Lsb_first.to_list ) )

    let fold {length; current_epoch; current_slot; total_currency} =
      let open Nanobit_base.Util in
      Length.Bits.fold length
      +> Epoch.Bits.fold current_epoch
      +> Epoch.Slot.Bits.fold current_slot
      +> Amount.fold total_currency

    let bit_length =
      Length.length_in_bits + Epoch.length_in_bits + Epoch.Slot.length_in_bits
      + Amount.length
  end

  module Protocol_state = Nanobit_base.Protocol_state.Make (Consensus_state)
  module Snark_transition =
    Nanobit_base.Snark_transition.Make (Consensus_transition_data) (Proof)
  module Internal_transition =
    Nanobit_base.Internal_transition.Make (Ledger_builder_diff)
      (Snark_transition)
  module External_transition =
    Nanobit_base.External_transition.Make (Ledger_builder_diff)
      (Protocol_state)

  let verify _transition = Snark_params.Tick.(Let_syntax.return Boolean.true_)

  let update_var state _transition = Snark_params.Tick.Let_syntax.return state

  let update (state: Consensus_state.value)
      (transition: Snark_transition.value) =
    let open Or_error.Let_syntax in
    let open Consensus_state in
    let Consensus_transition_data.({epoch; slot; total_currency_diff}) =
      Snark_transition.consensus_data transition
    in
    let%map total_currency =
      Amount.add state.total_currency total_currency_diff
      |> Option.map ~f:Or_error.return
      |> Option.value
           ~default:(Or_error.error_string "failed to add total_currency")
    in
    { length= Length.succ state.length
    ; current_epoch= epoch
    ; current_slot= slot
    ; total_currency }

  let step = Async_kernel.Deferred.Or_error.return

  let select _curr _cand = `Keep

  (*
  let select curr cand =
    let cand_fork_before_checkpoint =
      not (List.exists curr.checkpoints ~f:(fun c ->
        List.exists cand.checkpoints ~f:(checkpoint_equal c)))
    in
    let cand_is_valid =
      (* shouldn't the proof have already been checked before this point? *)
      verify cand.proof?
      && Time.less_than (Epoch.Slot.start_time (cand.epoch, cand.slot)) time_of_reciept
      && Time.greater_than_equal (Epoch.Slot.end_time (cand.epoch, cand.slot)) time_of_reciept
      && check cand.state?
    in
    if not cand_fork_before_checkpoint || not cand_is_valid then
      `Keep
    else if curr.current_epoch.post_lock_hash = cand.current_epoch.post_lock_hash then
      argmax_(chain in [cand, curr])(len(chain))?
    else if curr.current_epoch.last_start_hash = cand.current_epoch.last_start_hash then
      argmax_(chain in [cand, curr])(len(chain.last_epoch_length))?
    else
      argmax_(chain in [cand, curr])(len(chain.last_epoch_participation))?
    *)
  (* TODO: only track total currency from accounts > 1% of the currency using transactions *)
  let generate_transition ~previous_protocol_state ~blockchain_state ~time
      ~transactions:_ =
    let previous_consensus_state =
      Protocol_state.consensus_state previous_protocol_state
    in
    let time = Time.of_span_since_epoch (Time.Span.of_ms time) in
    let epoch, slot = Epoch.epoch_and_slot_of_time_exn time in
    let consensus_transition_data =
      let open Consensus_transition_data in
      {epoch; slot; total_currency_diff= Inputs.coinbase}
    in
    let consensus_state =
      let open Consensus_state in
      { length= Length.succ previous_consensus_state.length
      ; current_epoch= epoch
      ; current_slot= slot
      ; total_currency=
          Option.value_exn ~message:"failed to add total currency"
            (Amount.add previous_consensus_state.total_currency Inputs.coinbase)
      }
    in
    let protocol_state =
      Protocol_state.create_value
        ~previous_state_hash:(Protocol_state.hash previous_protocol_state)
        ~blockchain_state ~consensus_state
    in
    (protocol_state, consensus_transition_data)

  let genesis_protocol_state =
    Protocol_state.create_value
      ~previous_state_hash:(Protocol_state.hash Protocol_state.negative_one)
      ~blockchain_state:
        (Snark_transition.genesis |> Snark_transition.blockchain_state)
      ~consensus_state:
        ( Or_error.ok_exn
        @@ update
             (Protocol_state.consensus_state Protocol_state.negative_one)
             Snark_transition.genesis )
end
