open Async_kernel
open Core_kernel
open Signed
open Unsigned
open Coda_numbers
open Currency
open Fold_lib
open Signature_lib
open Module_version
module Time = Coda_base.Block_time
module Run = Snark_params.Tick.Run

let m = Snark_params.Tick.m

let make_checked t =
  let open Snark_params.Tick in
  with_state (As_prover.return ()) (Run.make_checked t)

let name = "proof_of_stake"

let uint32_of_int64 x = x |> Int64.to_int64 |> UInt32.of_int64

let int64_of_uint32 x = x |> UInt32.to_int64 |> Int64.of_int64

let genesis_ledger_total_currency =
  lazy
    ( Coda_base.Ledger.to_list (Lazy.force Genesis_ledger.t)
    |> List.fold_left ~init:Balance.zero ~f:(fun sum account ->
           Balance.add_amount sum
             (Balance.to_amount @@ account.Coda_base.Account.Poly.balance)
           |> Option.value_exn ?here:None ?error:None
                ~message:"failed to calculate total currency in genesis ledger"
       )
    |> Balance.to_amount )

let genesis_ledger_hash =
  lazy
    ( Coda_base.Ledger.merkle_root (Lazy.force Genesis_ledger.t)
    |> Coda_base.Frozen_ledger_hash.of_ledger_hash )

let compute_delegatee_table keys ~iter_accounts =
  let open Coda_base in
  let outer_table = Public_key.Compressed.Table.create () in
  iter_accounts (fun i (acct : Account.t) ->
      if Public_key.Compressed.Set.mem keys acct.delegate then
        Public_key.Compressed.Table.change outer_table acct.delegate
          ~f:(fun maybe_table ->
            let table =
              Option.value maybe_table ~default:(Account.Index.Table.create ())
            in
            Account.Index.Table.add_exn table ~key:i ~data:acct.balance ;
            Some table ) ) ;
  (* TODO: this metric tracking currently assumes that the
   * result of compute_delegatee_table is called with the
   * full set of proposer keypairs every time the set
   * changes, which is true right now, but this should be
   * control flow should be refactored to make this clearer *)
  let num_delegators =
    Public_key.Compressed.Table.fold outer_table ~init:0
      ~f:(fun ~key:_ ~data sum -> sum + Account.Index.Table.length data)
  in
  Coda_metrics.Gauge.set Coda_metrics.Consensus.staking_keypairs
    (Float.of_int @@ Public_key.Compressed.Set.length keys) ;
  Coda_metrics.Gauge.set Coda_metrics.Consensus.stake_delegators
    (Float.of_int num_delegators) ;
  outer_table

let compute_delegatee_table_sparse_ledger keys ledger =
  compute_delegatee_table keys ~iter_accounts:(fun f ->
      Coda_base.Sparse_ledger.iteri ledger ~f:(fun i acct -> f i acct) )

module Segment_id = Nat.Make32 ()

module Typ = Crypto_params.Tick0.Typ

module Constants = struct
  include Constants

  module Slot = struct
    (* let duration = Constants.block_window_duration *)

    let duration_ms = Int64.of_int block_window_duration_ms
  end

  module Epoch = struct
    let size = slots_per_epoch

    (** Amount of time in total for an epoch *)
    let duration =
      Time.Span.of_ms Int64.Infix.(Slot.duration_ms * int64_of_uint32 size)
  end

  module Checkpoint_window = struct
    let per_year = 12

    let slots_per_year =
      let one_year_ms =
        Core.Time.Span.(to_ms (of_day 365.)) |> Float.to_int |> Int.to_int64
      in
      Int64.Infix.(one_year_ms / Slot.duration_ms) |> Int64.to_int

    let size_in_slots =
      assert (slots_per_year mod per_year = 0) ;
      slots_per_year / per_year

    (* Number of bits required to represent a number
       < size_in_slots *)
    (* let per_window_index_size_in_bits = Core.Int.ceil_log2 size_in_slots *)
  end

  (** The duration of delta *)
  let delta_duration =
    Time.Span.of_ms (Int64.of_int (Int64.to_int Slot.duration_ms * delta))
end

let epoch_size = UInt32.to_int Constants.Epoch.size

module Configuration = struct
  type t =
    { delta: int
    ; k: int
    ; c: int
    ; c_times_k: int
    ; slots_per_epoch: int
    ; slot_duration: int
    ; epoch_duration: int
    ; acceptable_network_delay: int }
  [@@deriving yojson, bin_io, fields]

  let t =
    let open Constants in
    { delta
    ; k
    ; c
    ; c_times_k= c * k
    ; slots_per_epoch= UInt32.to_int Epoch.size
    ; slot_duration= Int64.to_int Slot.duration_ms
    ; epoch_duration= Int64.to_int (Time.Span.to_ms Epoch.duration)
    ; acceptable_network_delay= Int64.to_int (Time.Span.to_ms delta_duration)
    }
end

module Data = struct
  module Epoch_seed = struct
    include Coda_base.Data_hash.Make_full_size ()

    let initial : t = of_hash Snark_params.Tick.Pedersen.zero_hash

    let fold_vrf_result seed vrf_result =
      Fold.(fold seed +> Random_oracle.Digest.fold vrf_result)

    let update seed vrf_result =
      let open Snark_params.Tick in
      of_hash
        (Pedersen.digest_fold Coda_base.Hash_prefix.epoch_seed
           (fold_vrf_result seed vrf_result))

    let update_var (seed : var) (vrf_result : Random_oracle.Digest.Checked.t) :
        (var, _) Snark_params.Tick.Checked.t =
      let open Snark_params.Tick in
      let%bind seed_triples = var_to_triples seed in
      let%map hash =
        Pedersen.Checked.digest_triples ~init:Coda_base.Hash_prefix.epoch_seed
          (seed_triples @ Random_oracle.Digest.Checked.to_triples vrf_result)
      in
      var_of_hash_packed hash
  end

  module Epoch = struct
    include Epoch

    let of_time_exn t : t =
      if Time.(t < Constants.genesis_state_timestamp) then
        raise
          (Invalid_argument
             "Epoch.of_time: time is earlier than genesis block timestamp") ;
      let time_since_genesis = Time.diff t Constants.genesis_state_timestamp in
      uint32_of_int64
        Int64.Infix.(
          Time.Span.to_ms time_since_genesis
          / Time.Span.to_ms Constants.Epoch.duration)

    let start_time (epoch : t) =
      let ms =
        let open Int64.Infix in
        Time.Span.to_ms
          (Time.to_span_since_epoch Constants.genesis_state_timestamp)
        + (int64_of_uint32 epoch * Time.Span.to_ms Constants.Epoch.duration)
      in
      Time.of_span_since_epoch (Time.Span.of_ms ms)

    let end_time (epoch : t) =
      Time.add (start_time epoch) Constants.Epoch.duration

    module Slot = struct
      include Slot

      (*
      let after_lock_checkpoint (slot : t) =
        let ck = Constants.(c * k |> UInt32.of_int) in
        let open UInt32.Infix in
        ck * UInt32.of_int 2 < slot
      *)

      let in_seed_update_range (slot : t) =
        let ck = Constants.(c * k |> UInt32.of_int) in
        let open UInt32.Infix in
        ck <= slot && slot < ck * UInt32.of_int 2

      let in_seed_update_range_var (slot : Unpacked.var) =
        let open Snark_params.Tick in
        let open Snark_params.Tick.Let_syntax in
        let uint32_msb x =
          List.init 32 ~f:(fun i ->
              UInt32.Infix.((x lsr Int.sub 31 i) land UInt32.one = UInt32.one)
          )
          |> Bitstring_lib.Bitstring.Msb_first.of_list
        in
        let ( < ) = Bitstring_checked.lt_value in
        let ck = Constants.(c * k) |> UInt32.of_int in
        let ck_bitstring = uint32_msb ck
        and ck_times_2 = uint32_msb UInt32.(Infix.(of_int 2 * ck)) in
        let slot_msb =
          Bitstring_lib.Bitstring.Msb_first.of_lsb_first
            (Unpacked.var_to_bits slot)
        in
        let%bind slot_gte_ck = slot_msb < ck_bitstring >>| Boolean.not
        and slot_lt_ck_times_2 = slot_msb < ck_times_2 in
        Boolean.(slot_gte_ck && slot_lt_ck_times_2)

      let gen =
        let open Quickcheck.Let_syntax in
        Core.Int.gen_incl 0 (Constants.(c * k) * 3) >>| UInt32.of_int

      let%test_unit "in_seed_update_range unchecked vs. checked equality" =
        let test =
          Test_util.test_equal Unpacked.typ Snark_params.Tick.Boolean.typ
            in_seed_update_range_var in_seed_update_range
        in
        let x = Constants.(c * k) in
        let examples =
          List.map ~f:UInt32.of_int
            [x; x - 1; x + 1; x * 2; (x * 2) - 1; (x * 2) + 1]
        in
        Quickcheck.test ~trials:100 ~examples gen ~f:test
    end

    let slot_start_time (epoch : t) (slot : Slot.t) =
      Coda_base.Block_time.add (start_time epoch)
        (Coda_base.Block_time.Span.of_ms
           Int64.Infix.(int64_of_uint32 slot * Constants.Slot.duration_ms))

    (*
    let slot_end_time (epoch : t) (slot : Slot.t) =
      Time.add (slot_start_time epoch slot) Constants.Slot.duration
    *)

    let epoch_and_slot_of_time_exn tm : t * Slot.t =
      let epoch = of_time_exn tm in
      let time_since_epoch = Coda_base.Block_time.diff tm (start_time epoch) in
      let slot =
        uint32_of_int64
        @@ Int64.Infix.(
             Time.Span.to_ms time_since_epoch / Constants.Slot.duration_ms)
      in
      (epoch, slot)

    let diff_in_slots ((epoch, slot) : t * Slot.t)
        ((epoch', slot') : t * Slot.t) : int64 =
      let ( < ) x y = Pervasives.(Int64.compare x y < 0) in
      let ( > ) x y = Pervasives.(Int64.compare x y > 0) in
      let open Int64.Infix in
      let of_uint32 = UInt32.to_int64 in
      let epoch, slot = (of_uint32 epoch, of_uint32 slot) in
      let epoch', slot' = (of_uint32 epoch', of_uint32 slot') in
      let epoch_size = of_uint32 Constants.Epoch.size in
      let epoch_diff = epoch - epoch' in
      if epoch_diff > 0L then
        ((epoch_diff - 1L) * epoch_size) + slot + (epoch_size - slot')
      else if epoch_diff < 0L then
        ((epoch_diff + 1L) * epoch_size) - (epoch_size - slot) - slot'
      else slot - slot'

    let%test_unit "test diff_in_slots" =
      let open Int64.Infix in
      let ( !^ ) = UInt32.of_int in
      let ( !@ ) = Fn.compose ( !^ ) Int64.to_int in
      let epoch_size = UInt32.to_int64 Constants.Epoch.size in
      [%test_eq: int64] (diff_in_slots (!^0, !^5) (!^0, !^0)) 5L ;
      [%test_eq: int64] (diff_in_slots (!^3, !^23) (!^3, !^20)) 3L ;
      [%test_eq: int64] (diff_in_slots (!^4, !^4) (!^3, !^0)) (epoch_size + 4L) ;
      [%test_eq: int64]
        (diff_in_slots (!^5, !^2) (!^4, !@(epoch_size - 3L)))
        5L ;
      [%test_eq: int64]
        (diff_in_slots (!^6, !^42) (!^2, !^16))
        ((epoch_size * 3L) + 42L + (epoch_size - 16L)) ;
      [%test_eq: int64]
        (diff_in_slots (!^2, !@(epoch_size - 1L)) (!^3, !^4))
        (0L - 5L) ;
      [%test_eq: int64]
        (diff_in_slots (!^1, !^3) (!^7, !^27))
        (0L - ((epoch_size * 5L) + (epoch_size - 3L) + 27L))

    let incr ((epoch, slot) : t * Slot.t) =
      let open UInt32 in
      if Slot.equal slot (sub Constants.Epoch.size one) then
        (add epoch one, zero)
      else (epoch, add slot one)
  end

  module Epoch_and_slot = struct
    type t = Epoch.t * Epoch.Slot.t [@@deriving eq, sexp]

    let of_time_exn tm : t =
      let epoch = Epoch.of_time_exn tm in
      let time_since_epoch = Time.diff tm (Epoch.start_time epoch) in
      let slot =
        uint32_of_int64
        @@ Int64.Infix.(
             Time.Span.to_ms time_since_epoch / Constants.Slot.duration_ms)
      in
      (epoch, slot)
  end

  module Proposal_data = struct
    type t =
      { stake_proof: Stake_proof.t
      ; epoch_and_slot: Epoch_and_slot.t
      ; vrf_result: Random_oracle.Digest.t }

    let prover_state {stake_proof; _} = stake_proof
  end

  module Local_state = struct
    module Snapshot = struct
      type t =
        { ledger: Coda_base.Sparse_ledger.t
        ; delegatee_table:
            Currency.Balance.t Coda_base.Account.Index.Table.t
            Public_key.Compressed.Table.t }
      [@@deriving sexp]

      let delegators t key =
        Public_key.Compressed.Table.find t.delegatee_table key

      let to_yojson {ledger; delegatee_table} =
        `Assoc
          [ ( "ledger_hash"
            , Coda_base.(
                Sparse_ledger.merkle_root ledger |> Ledger_hash.to_yojson) )
          ; ( "delegators"
            , `Assoc
                ( Hashtbl.to_alist delegatee_table
                |> List.map ~f:(fun (key, delegators) ->
                       ( Public_key.Compressed.to_string key
                       , `Assoc
                           ( Hashtbl.to_alist delegators
                           |> List.map ~f:(fun (account, balance) ->
                                  ( Int.to_string account
                                  , `Int (Currency.Balance.to_int balance) ) )
                           ) ) ) ) ) ]
    end

    module Data = struct
      (* Invariant: Snapshot's delegators are taken from accounts in proposer_public_keys *)
      type t =
        { mutable staking_epoch_snapshot: Snapshot.t
        ; mutable next_epoch_snapshot: Snapshot.t
        ; last_checked_slot_and_epoch:
            (Epoch.t * Epoch.Slot.t) Public_key.Compressed.Table.t
        ; genesis_epoch_snapshot: Snapshot.t }
      [@@deriving sexp]

      let to_yojson t =
        `Assoc
          [ ( "staking_epoch_snapshot"
            , [%to_yojson: Snapshot.t] t.staking_epoch_snapshot )
          ; ( "next_epoch_snapshot"
            , [%to_yojson: Snapshot.t] t.next_epoch_snapshot )
          ; ( "last_checked_slot_and_epoch"
            , `Assoc
                ( Public_key.Compressed.Table.to_alist
                    t.last_checked_slot_and_epoch
                |> List.map ~f:(fun (key, epoch_and_slot) ->
                       ( Public_key.Compressed.to_string key
                       , [%to_yojson: Epoch.t * Epoch.Slot.t] epoch_and_slot )
                   ) ) )
          ; ( "genesis_epoch_snapshot"
            , [%to_yojson: Snapshot.t] t.genesis_epoch_snapshot ) ]
    end

    (* The outer ref changes whenever we swap in new staker set; all the snapshots are recomputed *)
    type t = Data.t ref [@@deriving sexp, to_yojson]

    let current_proposers t =
      Public_key.Compressed.Table.keys !t.Data.last_checked_slot_and_epoch
      |> Public_key.Compressed.Set.of_list

    let make_last_checked_slot_and_epoch_table old_table new_keys ~default =
      let module Set = Public_key.Compressed.Set in
      let module Table = Public_key.Compressed.Table in
      let last_checked_slot_and_epoch = Table.create () in
      Set.iter new_keys ~f:(fun pk ->
          let data = Option.value (Table.find old_table pk) ~default in
          Table.add_exn last_checked_slot_and_epoch ~key:pk ~data ) ;
      last_checked_slot_and_epoch

    let create proposer_public_keys =
      (* TODO: remove this duplicate of the genesis ledger *)
      let ledger =
        Coda_base.Sparse_ledger.of_any_ledger
          (Coda_base.Ledger.Any_ledger.cast
             (module Coda_base.Ledger)
             (Lazy.force Genesis_ledger.t))
      in
      let delegatee_table =
        compute_delegatee_table_sparse_ledger proposer_public_keys ledger
      in
      let genesis_epoch_snapshot = Snapshot.{delegatee_table; ledger} in
      ref
        { Data.staking_epoch_snapshot= genesis_epoch_snapshot
        ; next_epoch_snapshot= genesis_epoch_snapshot
        ; genesis_epoch_snapshot
        ; last_checked_slot_and_epoch=
            make_last_checked_slot_and_epoch_table
              (Public_key.Compressed.Table.create ())
              proposer_public_keys
              ~default:(Epoch.zero, Epoch.Slot.zero) }

    let proposer_swap t proposer_public_keys now =
      let old : Data.t = !t in
      let s {Snapshot.ledger; delegatee_table= _} =
        { Snapshot.ledger
        ; delegatee_table=
            compute_delegatee_table_sparse_ledger proposer_public_keys ledger
        }
      in
      t :=
        { Data.staking_epoch_snapshot= s old.staking_epoch_snapshot
        ; next_epoch_snapshot= s old.next_epoch_snapshot
        ; genesis_epoch_snapshot=
            s old.genesis_epoch_snapshot
            (* assume these keys are different and therefore we haven't checked any
         * slots or epochs *)
        ; last_checked_slot_and_epoch=
            make_last_checked_slot_and_epoch_table
              !t.Data.last_checked_slot_and_epoch proposer_public_keys
              ~default:
                ((* TODO: Be smarter so that we don't have to look at the slot before again *)
                 let epoch, slot = Epoch_and_slot.of_time_exn now in
                 (epoch, UInt32.(if slot > zero then sub slot one else slot)))
        }

    type snapshot_identifier = Staking_epoch_snapshot | Next_epoch_snapshot
    [@@deriving to_yojson]

    let get_snapshot (t : t) id =
      match id with
      | Staking_epoch_snapshot ->
          !t.staking_epoch_snapshot
      | Next_epoch_snapshot ->
          !t.next_epoch_snapshot

    let set_snapshot (t : t) id v =
      match id with
      | Staking_epoch_snapshot ->
          !t.staking_epoch_snapshot <- v
      | Next_epoch_snapshot ->
          !t.next_epoch_snapshot <- v

    let seen_slot (t : t) epoch slot =
      let module Table = Public_key.Compressed.Table in
      let unseens =
        Table.to_alist !t.last_checked_slot_and_epoch
        |> List.filter_map ~f:(fun (pk, last_checked_epoch_and_slot) ->
               let i =
                 Tuple2.compare ~cmp1:Epoch.compare ~cmp2:Epoch.Slot.compare
                   last_checked_epoch_and_slot (epoch, slot)
               in
               if i >= 0 then None
               else (
                 Table.set !t.last_checked_slot_and_epoch ~key:pk
                   ~data:(epoch, slot) ;
                 Some pk ) )
      in
      match unseens with
      | [] ->
          `All_seen
      | nel ->
          `Unseen (Public_key.Compressed.Set.of_list nel)
  end

  module Epoch_ledger = struct
    module Poly = struct
      module Stable = struct
        module V1 = struct
          module T = struct
            type ('ledger_hash, 'amount) t =
              {hash: 'ledger_hash; total_currency: 'amount}
            [@@deriving sexp, bin_io, eq, compare, hash, to_yojson, version]
          end

          include T
        end

        module Latest = V1
      end

      type ('ledger_hash, 'amount) t =
            ('ledger_hash, 'amount) Stable.Latest.t =
        {hash: 'ledger_hash; total_currency: 'amount}
      [@@deriving sexp, compare, hash, to_yojson]
    end

    module Value = struct
      module Stable = struct
        module V1 = struct
          module T = struct
            type t =
              ( Coda_base.Frozen_ledger_hash.Stable.V1.t
              , Amount.Stable.V1.t )
              Poly.Stable.V1.t
            [@@deriving sexp, bin_io, eq, compare, hash, to_yojson, version]
          end

          include T
          include Module_version.Registration.Make_latest_version (T)
        end

        module Latest = V1

        module Module_decl = struct
          let name = "epoch_ledger_data_proof_of_stake"

          type latest = Latest.t
        end

        module Registrar = Module_version.Registration.Make (Module_decl)
        module Registered_V1 = Registrar.Register (V1)
      end

      type t = Stable.Latest.t [@@deriving sexp, compare, hash, to_yojson]
    end

    type var = (Coda_base.Frozen_ledger_hash.var, Amount.var) Poly.t

    let to_hlist {Poly.hash; total_currency} =
      Coda_base.H_list.[hash; total_currency]

    let of_hlist :
           (unit, 'ledger_hash -> 'total_currency -> unit) Coda_base.H_list.t
        -> ('ledger_hash, 'total_currency) Poly.t =
     fun Coda_base.H_list.[hash; total_currency] -> {hash; total_currency}

    let data_spec =
      Snark_params.Tick.Data_spec.
        [Coda_base.Frozen_ledger_hash.typ; Amount.typ]

    let typ : (var, Value.t) Typ.t =
      Snark_params.Tick.Typ.of_hlistable data_spec ~var_to_hlist:to_hlist
        ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    let var_to_triples {Poly.hash; total_currency} =
      let open Snark_params.Tick.Checked.Let_syntax in
      let%map hash_triples =
        Coda_base.Frozen_ledger_hash.var_to_triples hash
      in
      hash_triples @ Amount.var_to_triples total_currency

    let fold {Poly.hash; total_currency} =
      let open Fold in
      Coda_base.Frozen_ledger_hash.fold hash +> Amount.fold total_currency

    let length_in_triples =
      Coda_base.Frozen_ledger_hash.length_in_triples + Amount.length_in_triples

    let if_ cond
        ~(then_ : (Coda_base.Frozen_ledger_hash.var, Amount.var) Poly.t)
        ~(else_ : (Coda_base.Frozen_ledger_hash.var, Amount.var) Poly.t) =
      let open Snark_params.Tick.Checked.Let_syntax in
      let%map hash =
        Coda_base.Frozen_ledger_hash.if_ cond ~then_:then_.hash
          ~else_:else_.hash
      and total_currency =
        Amount.Checked.if_ cond ~then_:then_.total_currency
          ~else_:else_.total_currency
      in
      {Poly.hash; total_currency}

    let genesis =
      lazy
        { Poly.hash= Lazy.force genesis_ledger_hash
        ; total_currency= Lazy.force genesis_ledger_total_currency }
  end

  module Vrf = struct
    module Scalar = struct
      type value = Snark_params.Tick.Inner_curve.Scalar.t

      type var = Snark_params.Tick.Inner_curve.Scalar.var

      let typ : (var, value) Typ.t = Snark_params.Tick.Inner_curve.Scalar.typ
    end

    module Group = struct
      open Snark_params.Tick

      type value = Inner_curve.t

      type var = Inner_curve.var

      let scale = Inner_curve.scale

      module Checked = struct
        include Inner_curve.Checked

        let scale_generator shifted s ~init =
          scale_known shifted Inner_curve.one s ~init
      end
    end

    module Message = struct
      type ('epoch, 'slot, 'epoch_seed, 'delegator) t =
        {epoch: 'epoch; slot: 'slot; seed: 'epoch_seed; delegator: 'delegator}

      type value =
        (Epoch.t, Epoch.Slot.t, Epoch_seed.t, Coda_base.Account.Index.t) t

      type var =
        ( Epoch.Unpacked.var
        , Epoch.Slot.Unpacked.var
        , Epoch_seed.var
        , Coda_base.Account.Index.Unpacked.var )
        t

      let to_hlist {epoch; slot; seed; delegator} =
        Coda_base.H_list.[epoch; slot; seed; delegator]

      let of_hlist :
             ( unit
             , 'epoch -> 'slot -> 'epoch_seed -> 'del -> unit )
             Coda_base.H_list.t
          -> ('epoch, 'slot, 'epoch_seed, 'del) t =
       fun Coda_base.H_list.[epoch; slot; seed; delegator] ->
        {epoch; slot; seed; delegator}

      let data_spec =
        let open Snark_params.Tick.Data_spec in
        [ Epoch.Unpacked.typ
        ; Epoch.Slot.Unpacked.typ
        ; Epoch_seed.typ
        ; Coda_base.Account.Index.Unpacked.typ ]

      let typ : (var, value) Typ.t =
        Snark_params.Tick.Typ.of_hlistable data_spec ~var_to_hlist:to_hlist
          ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
          ~value_of_hlist:of_hlist

      let fold {epoch; slot; seed; delegator} =
        let open Fold in
        Epoch.fold epoch +> Epoch.Slot.fold slot +> Epoch_seed.fold seed
        +> Coda_base.Account.Index.fold delegator

      let hash_to_group msg =
        let msg_hash_state =
          Snark_params.Tick.Pedersen.hash_fold
            Coda_base.Hash_prefix.vrf_message (fold msg)
        in
        msg_hash_state.acc

      module Checked = struct
        let var_to_triples {epoch; slot; seed; delegator} =
          let open Snark_params.Tick.Checked.Let_syntax in
          let%map seed_triples = Epoch_seed.var_to_triples seed in
          Epoch.Unpacked.var_to_triples epoch
          @ Epoch.Slot.Unpacked.var_to_triples slot
          @ seed_triples
          @ Coda_base.Account.Index.Unpacked.var_to_triples delegator

        let hash_to_group msg =
          let open Snark_params.Tick in
          let open Snark_params.Tick.Checked.Let_syntax in
          let%bind msg_triples = var_to_triples msg in
          Pedersen.Checked.hash_triples ~init:Coda_base.Hash_prefix.vrf_message
            msg_triples
      end

      let gen =
        let open Quickcheck.Let_syntax in
        let%map epoch = Epoch.gen
        and slot = Epoch.Slot.gen
        and seed = Epoch_seed.gen
        and delegator = Coda_base.Account.Index.gen in
        {epoch; slot; seed; delegator}
    end

    module Output = struct
      module Stable = struct
        module V1 = struct
          module T = struct
            type t = Random_oracle.Digest.Stable.V1.t
            [@@deriving sexp, bin_io, eq, compare, hash, yojson, version]
          end

          include T
          include Registration.Make_latest_version (T)
        end

        module Latest = V1

        module Module_decl = struct
          let name = "random_oracle_digest"

          type latest = Latest.t
        end

        module Registrar = Registration.Make (Module_decl)
        module Registered_V1 = Registrar.Register (V1)
      end

      type t = Stable.Latest.t [@@deriving sexp, compare, hash, yojson]

      type var = Random_oracle.Digest.Checked.t

      let typ : (var, t) Typ.t = Random_oracle.Digest.typ

      let fold = Random_oracle.Digest.fold

      let length_in_triples = Random_oracle.Digest.length_in_triples

      let gen = Random_oracle.Digest.gen

      let to_string (t : t) = (t :> string)

      type value = t [@@deriving sexp]

      let dummy = Random_oracle.digest_string ""

      let hash msg g =
        let open Fold in
        let compressed_g =
          Non_zero_curve_point.(g |> of_inner_curve_exn |> compress)
        in
        let digest =
          Snark_params.Tick.Pedersen.digest_fold
            Coda_base.Hash_prefix.vrf_output
            ( Message.fold msg
            +> Non_zero_curve_point.Compressed.fold compressed_g )
        in
        Random_oracle.digest_field digest

      module Checked = struct
        include Random_oracle.Digest.Checked

        let hash msg g =
          let open Snark_params.Tick.Checked.Let_syntax in
          let%bind msg_triples = Message.Checked.var_to_triples msg in
          let%bind g_triples =
            Non_zero_curve_point.(compress_var g >>= Compressed.var_to_triples)
          in
          let%bind pedersen_digest =
            Snark_params.Tick.Pedersen.Checked.digest_triples
              ~init:Coda_base.Hash_prefix.vrf_output (msg_triples @ g_triples)
          in
          Random_oracle.Checked.digest_field pedersen_digest
      end

      let%test_unit "hash unchecked vs. checked equality" =
        let gen_inner_curve_point =
          let open Quickcheck.Generator.Let_syntax in
          let%map compressed = Non_zero_curve_point.gen in
          Non_zero_curve_point.to_inner_curve compressed
        in
        let gen_message_and_curve_point =
          let open Quickcheck.Generator.Let_syntax in
          let%map msg = Message.gen and g = gen_inner_curve_point in
          (msg, g)
        in
        Quickcheck.test ~trials:10 gen_message_and_curve_point
          ~f:
            (Test_util.test_equal ~equal:Random_oracle.Digest.equal
               Snark_params.Tick.Typ.(
                 Message.typ * Snark_params.Tick.Inner_curve.typ)
               Random_oracle.Digest.typ
               (fun (msg, g) -> Checked.hash msg g)
               (fun (msg, g) -> hash msg g))
    end

    module Threshold = struct
      open Bignum_bigint

      (* TEMPORARY HACK FOR TESTNETS: c should be 1 (or possibly 2) otherwise *)
      let c = `Two_to_the 2

      let base = Bignum.(one / of_int 2)

      let c_bias =
        let (`Two_to_the i) = c in
        fun xs -> List.drop xs i

      let params =
        Snarky_taylor.Exp.params ~base
          ~field_size_in_bits:Snark_params.Tick.Field.size_in_bits

      let bigint_of_uint64 = Fn.compose Bigint.of_string UInt64.to_string

      (*  Check if
          vrf_output / 2^256 <= c * my_stake / total_currency

          So that we don't have to do division we check

          vrf_output * total_currency <= c * my_stake * 2^256
      *)
      let is_satisfied ~my_stake ~total_stake vrf_output =
        let input =
          (* get first params.per_term_precision bits of top / bottom.

            This is equal to

            floor(2^params.per_term_precision * top / bottom) / 2^params.per_term_precision
          *)
          let k = params.per_term_precision in
          let top = bigint_of_uint64 (Balance.to_uint64 my_stake) in
          let bottom = bigint_of_uint64 (Amount.to_uint64 total_stake) in
          Bignum.(
            of_bigint Bignum_bigint.(shift_left top k / bottom)
            / of_bigint Bignum_bigint.(shift_left one k))
        in
        let rhs = Snarky_taylor.Exp.Unchecked.one_minus_exp params input in
        let lhs =
          let bs = Random_oracle.Digest.to_bits vrf_output in
          let n = of_bits_lsb (c_bias (Array.to_list bs)) in
          Bignum.(
            of_bigint n
            / of_bigint
                Bignum_bigint.(
                  shift_left one Random_oracle.Digest.length_in_bits))
        in
        Bignum.(lhs <= rhs)

      module Checked = struct
        let is_satisfied ~my_stake ~total_stake (vrf_output : Output.var) =
          let open Snarky_taylor in
          make_checked (fun () ->
              let open Run in
              let rhs =
                Exp.one_minus_exp ~m params
                  (Floating_point.of_quotient ~m
                     ~precision:params.per_term_precision
                     ~top:(Integer.of_bits ~m (Balance.var_to_bits my_stake))
                     ~bottom:
                       (Integer.of_bits ~m (Amount.var_to_bits total_stake))
                     ~top_is_less_than_bottom:())
              in
              let vrf_output =
                Array.to_list (vrf_output :> Boolean.var array)
              in
              let lhs = c_bias vrf_output in
              Floating_point.(
                le ~m
                  (of_bits ~m lhs
                     ~precision:Random_oracle.Digest.length_in_bits)
                  rhs) )
      end
    end

    module T =
      Vrf_lib.Integrated.Make (Snark_params.Tick) (Scalar) (Group) (Message)
        (Output)

    type _ Snarky.Request.t +=
      | Winner_address : Coda_base.Account.Index.t Snarky.Request.t
      | Private_key : Scalar.value Snarky.Request.t
      | Public_key : Public_key.t Snarky.Request.t

    let%snarkydef get_vrf_evaluation shifted ~ledger ~message =
      let open Coda_base in
      let open Snark_params.Tick in
      let%bind private_key =
        request_witness Scalar.typ (As_prover.return Private_key)
      in
      let%bind public_key =
        request_witness Public_key.typ (As_prover.return Public_key)
      in
      let staker_addr = message.Message.delegator in
      let%bind account =
        with_label __LOC__ (Frozen_ledger_hash.get ledger staker_addr)
      in
      let%bind delegate =
        with_label __LOC__ (Public_key.decompress_var account.delegate)
      in
      let%bind () =
        with_label __LOC__ (Public_key.assert_equal public_key delegate)
      in
      let%map evaluation =
        with_label __LOC__
          (T.Checked.eval_and_check_public_key shifted ~private_key
             ~public_key:delegate message)
      in
      (evaluation, account.balance)

    module Checked = struct
      let%snarkydef check shifted ~(epoch_ledger : Epoch_ledger.var) ~epoch
          ~slot ~seed =
        let open Snark_params.Tick in
        let%bind winner_addr =
          request_witness Coda_base.Account.Index.Unpacked.typ
            (As_prover.return Winner_address)
        in
        let%bind result, my_stake =
          get_vrf_evaluation shifted ~ledger:epoch_ledger.hash
            ~message:{Message.epoch; slot; seed; delegator= winner_addr}
        in
        let%map satisifed =
          Threshold.Checked.is_satisfied ~my_stake
            ~total_stake:epoch_ledger.total_currency result
        in
        (satisifed, result)
    end

    let eval = T.eval

    module Precomputed = struct
      let keypairs = Lazy.force Coda_base.Sample_keypairs.keypairs

      let handler : Snark_params.Tick.Handler.t Lazy.t =
        lazy
          (let pk, sk = keypairs.(0) in
           let dummy_sparse_ledger =
             Coda_base.Sparse_ledger.of_ledger_subset_exn
               (Lazy.force Genesis_ledger.t)
               [pk]
           in
           let empty_pending_coinbase =
             Coda_base.Pending_coinbase.create () |> Or_error.ok_exn
           in
           let ledger_handler =
             unstage (Coda_base.Sparse_ledger.handler dummy_sparse_ledger)
           in
           let pending_coinbase_handler =
             unstage
               (Coda_base.Pending_coinbase.handler empty_pending_coinbase
                  ~is_new_stack:false)
           in
           let handlers =
             Snarky.Request.Handler.(
               push
                 (push fail (create_single pending_coinbase_handler))
                 (create_single ledger_handler))
           in
           fun (With {request; respond}) ->
             match request with
             | Winner_address ->
                 respond (Provide 0)
             | Private_key ->
                 respond (Provide sk)
             | Public_key ->
                 respond (Provide (Public_key.decompress_exn pk))
             | _ ->
                 respond
                   (Provide
                      (Snarky.Request.Handler.run handlers
                         ["Ledger Handler"; "Pending Coinbase Handler"]
                         request)))

      let vrf_output =
        let _, sk = keypairs.(0) in
        eval ~private_key:sk
          { Message.epoch= Epoch.zero
          ; slot= Epoch.Slot.zero
          ; seed= Epoch_seed.initial
          ; delegator= 0 }
    end

    let check ~epoch ~slot ~seed ~private_key ~public_key
        ~public_key_compressed ~total_stake ~logger ~epoch_snapshot =
      let open Message in
      let open Local_state in
      let open Snapshot in
      Logger.info logger ~module_:__MODULE__ ~location:__LOC__
        "Checking VRF evaluations at epoch: $epoch, slot: $slot"
        ~metadata:
          [ ("epoch", `Int (Epoch.to_int epoch))
          ; ("slot", `Int (Epoch.Slot.to_int slot)) ] ;
      with_return (fun {return} ->
          Hashtbl.iteri
            ( Snapshot.delegators epoch_snapshot public_key_compressed
            |> Option.value ~default:(Core_kernel.Int.Table.create ()) )
            ~f:(fun ~key:delegator ~data:balance ->
              let vrf_result =
                T.eval ~private_key {epoch; slot; seed; delegator}
              in
              Logger.debug logger ~module_:__MODULE__ ~location:__LOC__
                "VRF result for delegator: $delegator, balance: $balance, \
                 amount: $amount, result: $result"
                ~metadata:
                  [ ( "delegator"
                    , `Int (Coda_base.Account.Index.to_int delegator) )
                  ; ("balance", `Int (Balance.to_int balance))
                  ; ("amount", `Int (Amount.to_int total_stake))
                  ; ( "result"
                    , `String
                        (* use sexp representation; int might be too small *)
                        ( Random_oracle.Digest.fold_bits vrf_result
                        |> Bignum_bigint.of_bit_fold_lsb
                        |> Bignum_bigint.sexp_of_t |> Sexp.to_string ) ) ] ;
              Coda_metrics.Counter.inc_one
                Coda_metrics.Consensus.vrf_evaluations ;
              if
                Threshold.is_satisfied ~my_stake:balance ~total_stake
                  vrf_result
              then
                return
                  (Some
                     { Proposal_data.stake_proof=
                         { private_key
                         ; public_key
                         ; delegator
                         ; ledger= epoch_snapshot.ledger }
                     ; epoch_and_slot= (epoch, slot)
                     ; vrf_result }) ) ;
          None )
  end

  module Optional_state_hash = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type t = Coda_base.State_hash.Stable.V1.t option
          [@@deriving
            sexp, bin_io, eq, compare, hash, to_yojson, version {unnumbered}]
        end

        include T
      end

      module Latest = V1
    end

    type t = Stable.Latest.t [@@deriving sexp, compare, hash, to_yojson]

    type var = Coda_base.State_hash.var

    type value = t

    open Snark_params.Tick

    let typ : (var, value) Typ.t =
      let there = function
        | None ->
            Coda_base.State_hash.(of_hash zero)
        | Some h ->
            h
      in
      let back h =
        if Coda_base.State_hash.(equal h (of_hash zero)) then None else Some h
      in
      Typ.transport Coda_base.State_hash.typ ~there ~back

    let fold =
      Fn.compose Coda_base.State_hash.fold
        (Option.value ~default:Coda_base.State_hash.(of_hash zero))
  end

  module Epoch_data = struct
    module Poly = struct
      module Stable = struct
        module V1 = struct
          module T = struct
            type ( 'epoch_ledger
                 , 'epoch_seed
                 , 'start_checkpoint
                 , 'lock_checkpoint
                 , 'length )
                 t =
              { ledger: 'epoch_ledger
              ; seed: 'epoch_seed
              ; start_checkpoint: 'start_checkpoint
              ; lock_checkpoint: 'lock_checkpoint
              ; epoch_length: 'length }
            [@@deriving sexp, bin_io, eq, compare, hash, to_yojson, version]
          end

          include T
        end

        module Latest = V1
      end

      type ( 'epoch_ledger
           , 'epoch_seed
           , 'start_checkpoint
           , 'lock_checkpoint
           , 'length )
           t =
            ( 'epoch_ledger
            , 'epoch_seed
            , 'start_checkpoint
            , 'lock_checkpoint
            , 'length )
            Stable.Latest.t =
        { ledger: 'epoch_ledger
        ; seed: 'epoch_seed
        ; start_checkpoint: 'start_checkpoint
        ; lock_checkpoint: 'lock_checkpoint
        ; epoch_length: 'length }
      [@@deriving sexp, compare, hash, to_yojson]
    end

    type var =
      ( Epoch_ledger.var
      , Epoch_seed.var
      , Coda_base.State_hash.var
      , Coda_base.State_hash.var
      , Length.Unpacked.var )
      Poly.t

    let var_to_triples
        {Poly.ledger; seed; start_checkpoint; lock_checkpoint; epoch_length} =
      let open Snark_params.Tick.Checked.Let_syntax in
      let%map ledger_triples = Epoch_ledger.var_to_triples ledger
      and seed_triples = Epoch_seed.var_to_triples seed
      and start_checkpoint_triples =
        Coda_base.State_hash.var_to_triples start_checkpoint
      and lock_checkpoint_triples =
        Coda_base.State_hash.var_to_triples lock_checkpoint
      in
      ledger_triples @ seed_triples @ start_checkpoint_triples
      @ lock_checkpoint_triples
      @ Length.Unpacked.var_to_triples epoch_length

    let length_in_triples =
      Epoch_ledger.length_in_triples + Epoch_seed.length_in_triples
      + Coda_base.State_hash.length_in_triples
      + Coda_base.State_hash.length_in_triples + Length.length_in_triples

    let if_ cond
        ~(then_ :
           ( Epoch_ledger.var
           , Epoch_seed.var
           , Coda_base.State_hash.var
           , Coda_base.State_hash.var
           , Length.Unpacked.var )
           Poly.t)
        ~(else_ :
           ( Epoch_ledger.var
           , Epoch_seed.var
           , Coda_base.State_hash.var
           , Coda_base.State_hash.var
           , Length.Unpacked.var )
           Poly.t) =
      let open Snark_params.Tick.Checked.Let_syntax in
      let%map ledger =
        Epoch_ledger.if_ cond ~then_:then_.ledger ~else_:else_.ledger
      and seed = Epoch_seed.if_ cond ~then_:then_.seed ~else_:else_.seed
      and start_checkpoint =
        Coda_base.State_hash.if_ cond ~then_:then_.start_checkpoint
          ~else_:else_.start_checkpoint
      and lock_checkpoint =
        Coda_base.State_hash.if_ cond ~then_:then_.lock_checkpoint
          ~else_:else_.lock_checkpoint
      and epoch_length =
        Length.if_ cond ~then_:then_.epoch_length ~else_:else_.epoch_length
      in
      {Poly.ledger; seed; start_checkpoint; lock_checkpoint; epoch_length}

    let to_hlist
        {Poly.ledger; seed; start_checkpoint; lock_checkpoint; epoch_length} =
      Coda_base.H_list.
        [ledger; seed; start_checkpoint; lock_checkpoint; epoch_length]

    let of_hlist :
           ( unit
           ,    'ledger
             -> 'seed
             -> 'start_checkpoint
             -> 'lock_checkpoint
             -> 'length
             -> unit )
           Coda_base.H_list.t
        -> ( 'ledger
           , 'seed
           , 'start_checkpoint
           , 'lock_checkpoint
           , 'length )
           Poly.t =
     fun Coda_base.H_list.
           [ledger; seed; start_checkpoint; lock_checkpoint; epoch_length] ->
      {ledger; seed; start_checkpoint; lock_checkpoint; epoch_length}

    module Make (Lock_checkpoint : sig
      module Stable : sig
        module V1 : sig
          type t
          [@@deriving sexp, bin_io, eq, compare, hash, to_yojson, version]
        end

        module Latest : sig
          type t [@@deriving sexp, bin_io, compare, hash, to_yojson, version]
        end
      end

      type t = Stable.Latest.t

      val typ : (Coda_base.State_hash.var, t) Typ.t

      val fold : t -> bool Tuple_lib.Triple.t Fold.t

      val null : t
    end) =
    struct
      module Value = struct
        module Stable = struct
          module V1 = struct
            module T = struct
              type t =
                ( Epoch_ledger.Value.Stable.V1.t
                , Epoch_seed.Stable.V1.t
                , Coda_base.State_hash.Stable.V1.t
                , Lock_checkpoint.Stable.V1.t
                , Length.Stable.V1.t )
                Poly.Stable.V1.t
              [@@deriving sexp, bin_io, eq, compare, hash, to_yojson, version]
            end

            include T
            include Module_version.Registration.Make_latest_version (T)
          end

          module Latest = V1

          module Module_decl = struct
            let name = "epoch_data_proof_of_stake"

            type latest = Latest.t
          end

          module Registrar = Module_version.Registration.Make (Module_decl)
          module Registered_V1 = Registrar.Register (V1)
        end

        type t =
          ( Epoch_ledger.Value.Stable.Latest.t
          , Epoch_seed.Stable.Latest.t
          , Coda_base.State_hash.Stable.Latest.t
          , Lock_checkpoint.Stable.Latest.t
          , Length.Stable.Latest.t )
          Poly.t
        [@@deriving sexp, compare, hash, to_yojson]
      end

      let data_spec =
        let open Snark_params.Tick.Data_spec in
        [ Epoch_ledger.typ
        ; Epoch_seed.typ
        ; Coda_base.State_hash.typ
        ; Lock_checkpoint.typ
        ; Length.Unpacked.typ ]

      let typ : (var, Value.t) Typ.t =
        Snark_params.Tick.Typ.of_hlistable data_spec ~var_to_hlist:to_hlist
          ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
          ~value_of_hlist:of_hlist

      let fold
          {Poly.ledger; seed; start_checkpoint; lock_checkpoint; epoch_length}
          =
        let open Fold in
        Epoch_ledger.fold ledger +> Epoch_seed.fold seed
        +> Coda_base.State_hash.fold start_checkpoint
        +> Lock_checkpoint.fold lock_checkpoint
        +> Length.fold epoch_length

      let genesis =
        lazy
          { Poly.ledger=
              Lazy.force Epoch_ledger.genesis
              (* TODO: epoch_seed needs to be non-determinable by o1-labs before mainnet launch *)
          ; seed= Epoch_seed.initial
          ; start_checkpoint= Coda_base.State_hash.(of_hash zero)
          ; lock_checkpoint= Lock_checkpoint.null
          ; epoch_length= Length.of_int 1 }
    end

    module Staking = Make (struct
      include Coda_base.State_hash

      let null = Coda_base.State_hash.(of_hash zero)
    end)

    module Next = Make (struct
      include Optional_state_hash

      let null = None
    end)

    let next_to_staking next =
      Poly.
        { next with
          lock_checkpoint=
            (* TODO: This is just a hack to make code compatible with old
                   implementation. We should change it once Issue #2328
                   is properly addressed. *)
            Option.value next.lock_checkpoint
              ~default:Coda_base.State_hash.(of_hash zero) }

    let update_pair
        ((staking_data, next_data) : Staking.Value.t * Next.Value.t)
        epoch_count ~prev_epoch ~next_epoch ~prev_slot
        ~prev_protocol_state_hash ~proposer_vrf_result ~snarked_ledger_hash
        ~total_currency =
      let staking_data', next_data', epoch_count' =
        if next_epoch > prev_epoch then
          ( next_to_staking next_data
          , { Poly.seed= Epoch_seed.initial
            ; ledger=
                {Epoch_ledger.Poly.hash= snarked_ledger_hash; total_currency}
            ; start_checkpoint= prev_protocol_state_hash
            ; lock_checkpoint= None
            ; epoch_length= Length.of_int 1 }
          , Length.succ epoch_count )
        else (
          assert (Epoch.equal next_epoch prev_epoch) ;
          ( staking_data
          , Poly.
              {next_data with epoch_length= Length.succ next_data.epoch_length}
          , epoch_count ) )
      in
      let curr_seed, curr_lock_checkpoint =
        if Epoch.Slot.in_seed_update_range prev_slot then
          ( Epoch_seed.update next_data'.seed proposer_vrf_result
          , Some prev_protocol_state_hash )
        else (next_data'.seed, next_data'.lock_checkpoint)
      in
      let next_data'' =
        Poly.
          { next_data' with
            seed= curr_seed
          ; lock_checkpoint= curr_lock_checkpoint }
      in
      (staking_data', next_data'', epoch_count')
  end

  module Consensus_transition = struct
    include Global_slot
    module Value = Global_slot

    let typ = Global_slot.Unpacked.typ

    type var = Unpacked.var

    let genesis = zero
  end

  module Checkpoints = struct
    module Hash = Coda_base.Data_hash.Make_full_size ()

    let merge (s : Coda_base.State_hash.t) (h : Hash.t) =
      Snark_params.Tick.Pedersen.digest_fold
        Coda_base.Hash_prefix.checkpoint_list
        Fold.(Coda_base.State_hash.fold s +> Hash.fold h)
      |> Hash.of_hash

    let length = Constants.Checkpoint_window.per_year

    module Repr = struct
      module Stable = struct
        module V1 = struct
          module T = struct
            type t =
              { (* TODO: Make a nice way to force this to have bounded (or fixed) size for
                   bin_io reasons *)
                prefix:
                  Coda_base.State_hash.Stable.V1.t Core.Fqueue.Stable.V1.t
              ; tail: Hash.Stable.V1.t }
            [@@deriving sexp, bin_io, compare, version {unnumbered}]

            let digest ({prefix; tail} : t) =
              let rec go acc p =
                match Fqueue.dequeue p with
                | None ->
                    acc
                | Some (h, p) ->
                    go (merge h acc) p
              in
              go tail prefix
          end

          include T

          module Yojson = struct
            type t =
              { prefix: Coda_base.State_hash.Stable.V1.t list
              ; tail: Hash.Stable.V1.t }
            [@@deriving to_yojson]
          end

          let to_yojson ({prefix; tail} : t) =
            Yojson.to_yojson {prefix= Fqueue.to_list prefix; tail}
        end

        module Latest = V1
      end

      type t = Stable.Latest.t =
        {prefix: Coda_base.State_hash.t Fqueue.t; tail: Hash.t}
      [@@deriving sexp, hash, compare]

      let to_yojson = Stable.V1.to_yojson
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type t = (Repr.Stable.V1.t, Hash.Stable.V1.t) With_hash.Stable.V1.t
          [@@deriving sexp, version, to_yojson]

          let compare (t1 : t) (t2 : t) = Hash.compare t1.hash t2.hash

          let equal (t1 : t) (t2 : t) = Hash.equal t1.hash t2.hash

          let hash_fold_t s (t : t) = Hash.hash_fold_t s t.hash

          let to_repr (t : t) = t.data

          let of_repr r =
            {With_hash.Stable.V1.data= r; hash= Repr.Stable.V1.digest r}

          include Binable.Of_binable
                    (Repr.Stable.V1)
                    (struct
                      type nonrec t = t

                      let to_binable = to_repr

                      let of_binable = of_repr
                    end)
        end

        include T
        include Registration.Make_latest_version (T)
      end

      module Latest = V1

      module Module_decl = struct
        let name = "checkpoints_proof_of_stake"

        type latest = Latest.t
      end

      module Registrar = Registration.Make (Module_decl)
      module Registered_V1 = Registrar.Register (V1)
    end

    type t = (Repr.t, Hash.t) With_hash.t [@@deriving sexp, to_yojson]

    let empty : t =
      let dummy = Hash.of_hash Snark_params.Tick.Field.zero in
      {hash= dummy; data= {prefix= Fqueue.empty; tail= dummy}}

    let cons sh (t : t) : t =
      (* This kind of defeats the purpose of having a queue, but oh well. *)
      let n = Fqueue.length t.data.prefix in
      let hash = merge sh t.hash in
      let {Repr.prefix; tail} = t.data in
      if n < length then {hash; data= {prefix= Fqueue.enqueue prefix sh; tail}}
      else
        let sh0, prefix = Fqueue.dequeue_exn prefix in
        {hash; data= {prefix= Fqueue.enqueue prefix sh; tail= merge sh0 tail}}

    type var = Hash.var

    let typ =
      Typ.transport Hash.typ
        ~there:(fun (t : t) -> t.hash)
        ~back:(fun hash -> {hash; data= {prefix= Fqueue.empty; tail= hash}})

    module Checked = struct
      let if_ = Hash.if_

      let cons sh t =
        let open Snark_params.Tick in
        let open Checked in
        let%bind sh = Coda_base.State_hash.var_to_triples sh
        and t = Hash.var_to_triples t in
        Pedersen.Checked.digest_triples
          ~init:Coda_base.Hash_prefix.checkpoint_list (sh @ t)
        >>| Hash.var_of_hash_packed
    end
  end

  (* We have a list of state hashes. When we extend the blockchain,
     we see if the **previous** state should be saved as a checkpoint.
     This is because we have convenient access to the entire previous
     protocol state hash.

     We divide the slots of an epoch into "checkpoint windows": chunks of
     size [checkpoint_window_size]. The goal is to record the first block
     in a given window as a check-point if there are any blocks in that
     window, and zero checkpoints if the window was empty.

     To that end, we store in each state a bit [checkpoint_window_filled] which
     is true iff there has already been a state in the history of the given state
     which is in the same checkpoint window as the given state.
  *)
  module Consensus_state = struct
    module Poly = struct
      module Stable = struct
        module V1 = struct
          module T = struct
            type ( 'length
                 , 'vrf_output
                 , 'amount
                 , 'global_slot
                 , 'staking_epoch_data
                 , 'next_epoch_data
                 , 'bool
                 , 'checkpoints )
                 t =
              { block_height: 'length
              ; epoch_count: 'length
              ; min_epoch_length: 'length
              ; last_vrf_output: 'vrf_output
              ; total_currency: 'amount
              ; curr_global_slot: 'global_slot
              ; staking_epoch_data: 'staking_epoch_data
              ; next_epoch_data: 'next_epoch_data
              ; has_ancestor_in_same_checkpoint_window: 'bool
              ; checkpoints: 'checkpoints }
            [@@deriving sexp, bin_io, eq, compare, hash, to_yojson, version]
          end

          include T
        end

        module Latest = V1
      end

      type ( 'length
           , 'vrf_output
           , 'amount
           , 'global_slot
           , 'staking_epoch_data
           , 'next_epoch_data
           , 'bool
           , 'checkpoints )
           t =
            ( 'length
            , 'vrf_output
            , 'amount
            , 'global_slot
            , 'staking_epoch_data
            , 'next_epoch_data
            , 'bool
            , 'checkpoints )
            Stable.Latest.t =
        { block_height: 'length
        ; epoch_count: 'length
        ; min_epoch_length: 'length
        ; last_vrf_output: 'vrf_output
        ; total_currency: 'amount
        ; curr_global_slot: 'global_slot
        ; staking_epoch_data: 'staking_epoch_data
        ; next_epoch_data: 'next_epoch_data
        ; has_ancestor_in_same_checkpoint_window: 'bool
        ; checkpoints: 'checkpoints }
      [@@deriving sexp, compare, hash, to_yojson]
    end

    module Value = struct
      module Stable = struct
        module V1 = struct
          module T = struct
            type t =
              ( Length.Stable.V1.t
              , Vrf.Output.Stable.V1.t
              , Amount.Stable.V1.t
              , Global_slot.Stable.V1.t
              , Epoch_data.Staking.Value.Stable.V1.t
              , Epoch_data.Next.Value.Stable.V1.t
              , bool
              , Checkpoints.Stable.V1.t )
              Poly.Stable.V1.t
            [@@deriving sexp, bin_io, eq, compare, hash, version, to_yojson]
          end

          include T
          include Registration.Make_latest_version (T)
        end

        module Latest = V1

        module Module_decl = struct
          let name = "consensus_state_proof_of_stake"

          type latest = Latest.t
        end

        module Registrar = Registration.Make (Module_decl)
        module Registered_V1 = Registrar.Register (V1)
      end

      type t = Stable.Latest.t (* bin_io omitted intentionally *)
      [@@deriving sexp, eq, compare, hash, to_yojson]
    end

    open Snark_params.Tick

    type var =
      ( Length.Unpacked.var
      , Vrf.Output.var
      , Amount.var
      , Global_slot.Unpacked.var
      , Epoch_data.var
      , Epoch_data.var
      , Boolean.var
      , Checkpoints.var )
      Poly.t

    let to_hlist
        { Poly.block_height
        ; epoch_count
        ; min_epoch_length
        ; last_vrf_output
        ; total_currency
        ; curr_global_slot
        ; staking_epoch_data
        ; next_epoch_data
        ; has_ancestor_in_same_checkpoint_window
        ; checkpoints } =
      let open Coda_base.H_list in
      [ block_height
      ; epoch_count
      ; min_epoch_length
      ; last_vrf_output
      ; total_currency
      ; curr_global_slot
      ; staking_epoch_data
      ; next_epoch_data
      ; has_ancestor_in_same_checkpoint_window
      ; checkpoints ]

    let of_hlist :
           ( unit
           ,    'length
             -> 'length
             -> 'length
             -> 'vrf_output
             -> 'amount
             -> 'global_slot
             -> 'staking_epoch_data
             -> 'next_epoch_data
             -> 'bool
             -> 'checkpoints
             -> unit )
           Coda_base.H_list.t
        -> ( 'length
           , 'vrf_output
           , 'amount
           , 'global_slot
           , 'staking_epoch_data
           , 'next_epoch_data
           , 'bool
           , 'checkpoints )
           Poly.t =
     fun Coda_base.H_list.
           [ block_height
           ; epoch_count
           ; min_epoch_length
           ; last_vrf_output
           ; total_currency
           ; curr_global_slot
           ; staking_epoch_data
           ; next_epoch_data
           ; has_ancestor_in_same_checkpoint_window
           ; checkpoints ] ->
      { block_height
      ; epoch_count
      ; min_epoch_length
      ; last_vrf_output
      ; total_currency
      ; curr_global_slot
      ; staking_epoch_data
      ; next_epoch_data
      ; has_ancestor_in_same_checkpoint_window
      ; checkpoints }

    let data_spec =
      let open Snark_params.Tick.Data_spec in
      [ Length.Unpacked.typ
      ; Length.Unpacked.typ
      ; Length.Unpacked.typ
      ; Vrf.Output.typ
      ; Amount.typ
      ; Global_slot.Unpacked.typ
      ; Epoch_data.Staking.typ
      ; Epoch_data.Next.typ
      ; Boolean.typ
      ; Checkpoints.typ ]

    let typ : (var, Value.t) Typ.t =
      Snark_params.Tick.Typ.of_hlistable data_spec ~var_to_hlist:to_hlist
        ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    let var_to_triples
        { Poly.block_height
        ; epoch_count
        ; min_epoch_length
        ; last_vrf_output
        ; total_currency
        ; curr_global_slot
        ; staking_epoch_data
        ; next_epoch_data
        ; has_ancestor_in_same_checkpoint_window
        ; checkpoints } =
      let open Snark_params.Tick.Checked.Let_syntax in
      let%map staking_epoch_data_triples =
        Epoch_data.var_to_triples staking_epoch_data
      and next_epoch_data_triples = Epoch_data.var_to_triples next_epoch_data
      and checkpoints_triples = Checkpoints.Hash.var_to_triples checkpoints in
      Length.Unpacked.var_to_triples block_height
      @ Length.Unpacked.var_to_triples epoch_count
      @ Length.Unpacked.var_to_triples min_epoch_length
      @ Vrf.Output.Checked.to_triples last_vrf_output
      @ Global_slot.Unpacked.var_to_triples curr_global_slot
      @ Amount.var_to_triples total_currency
      @ staking_epoch_data_triples @ next_epoch_data_triples
      @ [Boolean.(has_ancestor_in_same_checkpoint_window, false_, false_)]
      @ checkpoints_triples

    let fold
        ({ Poly.block_height
         ; epoch_count
         ; min_epoch_length
         ; last_vrf_output
         ; total_currency
         ; curr_global_slot
         ; staking_epoch_data
         ; next_epoch_data
         ; has_ancestor_in_same_checkpoint_window
         ; checkpoints } :
          Value.t) =
      let open Fold in
      Length.fold block_height
      +> Length.fold epoch_count
      +> Length.fold min_epoch_length
      +> Vrf.Output.fold last_vrf_output
      +> Global_slot.fold curr_global_slot
      +> Amount.fold total_currency
      +> Epoch_data.Staking.fold staking_epoch_data
      +> Epoch_data.Next.fold next_epoch_data
      +> return (has_ancestor_in_same_checkpoint_window, false, false)
      +> Checkpoints.Hash.fold checkpoints.hash

    let length_in_triples =
      Length.length_in_triples + Length.length_in_triples
      + Vrf.Output.length_in_triples + Epoch.length_in_triples
      + Epoch.Slot.length_in_triples + Amount.length_in_triples
      + Epoch_data.length_in_triples + Epoch_data.length_in_triples

    let checkpoint_window slot =
      Global_slot.to_int slot / Constants.Checkpoint_window.size_in_slots

    let same_checkpoint_window_unchecked slot1 slot2 =
      Core.Int.(checkpoint_window slot1 = checkpoint_window slot2)

    let time_hum (t : Value.t) =
      let epoch, slot = Global_slot.to_epoch_and_slot t.curr_global_slot in
      sprintf "epoch=%d, slot=%d" (Epoch.to_int epoch) (Epoch.Slot.to_int slot)

    let update ~(previous_consensus_state : Value.t)
        ~(consensus_transition : Consensus_transition.t)
        ~(previous_protocol_state_hash : Coda_base.State_hash.t)
        ~(supply_increase : Currency.Amount.t)
        ~(snarked_ledger_hash : Coda_base.Frozen_ledger_hash.t)
        ~(proposer_vrf_result : Random_oracle.Digest.t) : Value.t Or_error.t =
      let open Or_error.Let_syntax in
      let prev_epoch, prev_slot =
        Global_slot.to_epoch_and_slot previous_consensus_state.curr_global_slot
      in
      let next_epoch, next_slot =
        Global_slot.to_epoch_and_slot consensus_transition
      in
      let%map total_currency =
        Amount.add previous_consensus_state.total_currency supply_increase
        |> Option.map ~f:Or_error.return
        |> Option.value
             ~default:(Or_error.error_string "failed to add total_currency")
      and () =
        if
          Global_slot.(equal consensus_transition Consensus_transition.genesis)
          || Global_slot.(
               previous_consensus_state.curr_global_slot < consensus_transition)
        then Ok ()
        else
          Or_error.errorf
            !"(epoch, slot) did not increase. prev=%{sexp:Epoch.t * \
              Epoch.Slot.t}, next=%{sexp:Epoch.t * Epoch.Slot.t}"
            (prev_epoch, prev_slot) (next_epoch, next_slot)
      in
      let staking_epoch_data, next_epoch_data, epoch_count =
        Epoch_data.update_pair
          ( previous_consensus_state.staking_epoch_data
          , previous_consensus_state.next_epoch_data )
          previous_consensus_state.epoch_count ~prev_epoch ~next_epoch
          ~prev_slot ~prev_protocol_state_hash:previous_protocol_state_hash
          ~proposer_vrf_result ~snarked_ledger_hash ~total_currency
      in
      let checkpoints =
        if previous_consensus_state.has_ancestor_in_same_checkpoint_window then
          previous_consensus_state.checkpoints
        else
          Checkpoints.cons previous_protocol_state_hash
            previous_consensus_state.checkpoints
      in
      { Poly.block_height=
          Length.succ previous_consensus_state.block_height
      ; epoch_count
      ; min_epoch_length=
          ( if Epoch.equal prev_epoch next_epoch then
            previous_consensus_state.min_epoch_length
          else if Epoch.(equal next_epoch (succ prev_epoch)) then
            Length.min previous_consensus_state.min_epoch_length
              previous_consensus_state.next_epoch_data.epoch_length
          else Length.zero )
      ; last_vrf_output= proposer_vrf_result
      ; total_currency
      ; curr_global_slot= consensus_transition
      ; staking_epoch_data
      ; next_epoch_data
      ; has_ancestor_in_same_checkpoint_window=
          same_checkpoint_window_unchecked
            (Global_slot.create ~epoch:prev_epoch ~slot:prev_slot)
            (Global_slot.create ~epoch:next_epoch ~slot:next_slot)
      ; checkpoints }

    let same_checkpoint_window ~prev:(slot1 : Global_slot.Packed.var)
        ~next:(slot2 : Global_slot.Packed.var) =
      let open Snarky_taylor in
      let open Run in
      let _q1, r1 =
        Integer.div_mod ~m
          (Global_slot.Checked.to_integer slot1)
          (Integer.constant ~m
             (Bignum_bigint.of_int Constants.Checkpoint_window.size_in_slots))
      in
      let next_window_start =
        Field.(
          (slot1 :> Field.t)
          - Integer.to_field r1
          + of_int Constants.Checkpoint_window.size_in_slots)
      in
      (Field.compare ~bit_length:Global_slot.length_in_bits
         (slot2 :> Field.t)
         next_window_start)
        .less

    let same_checkpoint_window ~prev ~next =
      make_checked (fun () -> same_checkpoint_window ~prev ~next)

    let negative_one : Value.t Lazy.t =
      lazy
        { Poly.block_height= Length.zero
        ; epoch_count= Length.zero
        ; min_epoch_length= Length.of_int (UInt32.to_int Constants.Epoch.size)
        ; last_vrf_output= Vrf.Output.dummy
        ; total_currency= Lazy.force genesis_ledger_total_currency
        ; curr_global_slot= Global_slot.zero
        ; staking_epoch_data= Lazy.force Epoch_data.Staking.genesis
        ; next_epoch_data= Lazy.force Epoch_data.Next.genesis
        ; has_ancestor_in_same_checkpoint_window= false
        ; checkpoints= Checkpoints.empty }

    let create_genesis_from_transition ~negative_one_protocol_state_hash
        ~consensus_transition : Value.t =
      Or_error.ok_exn
        (update ~proposer_vrf_result:Vrf.Precomputed.vrf_output
           ~previous_consensus_state:(Lazy.force negative_one)
           ~previous_protocol_state_hash:negative_one_protocol_state_hash
           ~consensus_transition ~supply_increase:Currency.Amount.zero
           ~snarked_ledger_hash:(Lazy.force genesis_ledger_hash))

    let create_genesis ~negative_one_protocol_state_hash : Value.t =
      create_genesis_from_transition ~negative_one_protocol_state_hash
        ~consensus_transition:Consensus_transition.genesis

    (* Check that both epoch and slot are zero.
    *)
    let is_genesis (global_slot : Global_slot.Unpacked.var) =
      let open Field in
      Checked.equal
        (Global_slot.pack_var global_slot :> Var.t)
        (Var.constant zero)

    let%snarkydef update_var (previous_state : var)
        (transition_data : Consensus_transition.var)
        (previous_protocol_state_hash : Coda_base.State_hash.var)
        ~(supply_increase : Currency.Amount.var)
        ~(previous_blockchain_state_ledger_hash :
           Coda_base.Frozen_ledger_hash.var) =
      let open Snark_params.Tick in
      let {Poly.curr_global_slot= prev_global_slot; _} = previous_state in
      let next_global_slot = transition_data in
      let%bind () =
        let%bind global_slot_increased =
          let%map c =
            Global_slot.compare_var prev_global_slot next_global_slot
          in
          c.less
        in
        let%bind is_genesis = is_genesis next_global_slot in
        Boolean.Assert.any [global_slot_increased; is_genesis]
      in
      let%bind next_epoch, next_slot =
        make_checked (fun () ->
            Global_slot.Checked.to_epoch_and_slot next_global_slot )
      and prev_epoch, prev_slot =
        make_checked (fun () ->
            Global_slot.Checked.to_epoch_and_slot prev_global_slot )
      in
      let%bind epoch_increased =
        let%bind c = Epoch.compare_var prev_epoch next_epoch in
        let%map () = Boolean.Assert.is_true c.less_or_equal in
        c.less
      in
      let%bind staking_epoch_data =
        Epoch_data.if_ epoch_increased ~then_:previous_state.next_epoch_data
          ~else_:previous_state.staking_epoch_data
      in
      let%bind threshold_satisfied, vrf_result =
        let%bind (module M) = Inner_curve.Checked.Shifted.create () in
        Vrf.Checked.check
          (module M)
          ~epoch_ledger:staking_epoch_data.ledger ~epoch:next_epoch
          ~slot:next_slot ~seed:staking_epoch_data.seed
      in
      let%bind new_total_currency =
        Currency.Amount.Checked.add previous_state.total_currency
          supply_increase
      in
      let%bind checkpoints =
        let%bind consed =
          Checkpoints.Checked.cons previous_protocol_state_hash
            previous_state.checkpoints
        in
        Checkpoints.Checked.if_
          previous_state.has_ancestor_in_same_checkpoint_window
          ~then_:previous_state.checkpoints ~else_:consed
      in
      let%bind has_ancestor_in_same_checkpoint_window =
        same_checkpoint_window
          ~prev:(Global_slot.Checked.create ~epoch:prev_epoch ~slot:prev_slot)
          ~next:(Global_slot.Checked.create ~epoch:next_epoch ~slot:next_slot)
      in
      let%bind next_epoch_data =
        let%map seed =
          let%bind in_seed_update_range =
            Epoch.Slot.in_seed_update_range_var prev_slot
          in
          let%bind base =
            Epoch_seed.if_ epoch_increased
              ~then_:Epoch_seed.(var_of_t initial)
              ~else_:previous_state.next_epoch_data.seed
          in
          let%bind updated = Epoch_seed.update_var base vrf_result in
          Epoch_seed.if_ in_seed_update_range ~then_:updated ~else_:base
        and epoch_length =
          let%bind base =
            Field.Checked.if_ epoch_increased
              ~then_:Field.(Var.constant zero)
              ~else_:
                ( Length.pack_var previous_state.next_epoch_data.epoch_length
                  :> Field.Var.t )
          in
          Length.var_of_field Field.(Var.(add (constant one) base))
        and ledger =
          Epoch_ledger.if_ epoch_increased
            ~then_:
              { total_currency= new_total_currency
              ; hash= previous_blockchain_state_ledger_hash }
            ~else_:previous_state.next_epoch_data.ledger
        and start_checkpoint =
          Coda_base.State_hash.if_ epoch_increased
            ~then_:previous_protocol_state_hash
            ~else_:previous_state.next_epoch_data.start_checkpoint
        (* Want this to be the protocol state hash once we leave the seed
           update range. *)
        and lock_checkpoint =
          let%bind base =
            (* TODO: Should this be zero or some other sentinel value? *)
            Coda_base.State_hash.if_ epoch_increased
              ~then_:Coda_base.State_hash.(var_of_t (of_hash zero))
              ~else_:previous_state.next_epoch_data.lock_checkpoint
          in
          let%bind in_seed_update_range =
            Epoch.Slot.in_seed_update_range_var prev_slot
          in
          Coda_base.State_hash.if_ in_seed_update_range
            ~then_:previous_protocol_state_hash ~else_:base
        in
        { Epoch_data.Poly.seed
        ; epoch_length
        ; ledger
        ; start_checkpoint
        ; lock_checkpoint }
      and block_height =
        Length.increment_var previous_state.block_height
      (* TODO: keep track of total_currency in transaction snark. The current_slot
       * implementation would allow an adversary to make then total_currency incorrect by
       * not adding the coinbase to their account. *)
      and new_total_currency =
        Amount.Checked.add previous_state.total_currency supply_increase
      and epoch_count =
        Length.increment_if_var previous_state.epoch_count epoch_increased
      and min_epoch_length =
        let if_ b ~then_ ~else_ =
          let%bind b = b and then_ = then_ and else_ = else_ in
          Length.if_ b ~then_ ~else_
        in
        let return = Checked.return in
        if_
          (return Boolean.(not epoch_increased))
          ~then_:(return previous_state.min_epoch_length)
          ~else_:
            (if_
               (Epoch.is_succ_var ~pred:prev_epoch ~succ:next_epoch)
               ~then_:
                 (Length.min_var previous_state.min_epoch_length
                    previous_state.next_epoch_data.epoch_length)
               ~else_:(return (Length.Unpacked.var_of_value Length.zero)))
      in
      Checked.return
        ( `Success threshold_satisfied
        , { Poly.block_height
          ; epoch_count
          ; min_epoch_length
          ; last_vrf_output= vrf_result
          ; curr_global_slot= next_global_slot
          ; total_currency= new_total_currency
          ; staking_epoch_data
          ; next_epoch_data
          ; has_ancestor_in_same_checkpoint_window
          ; checkpoints } )

    let block_height (t : Value.t) = t.block_height

    let to_lite = None

    type display =
      { block_height: int
      ; epoch_count: int
      ; curr_epoch: int
      ; curr_slot: int
      ; total_currency: int }
    [@@deriving yojson]

    let display (t : Value.t) =
      let epoch, slot = Global_slot.to_epoch_and_slot t.curr_global_slot in
      { block_height= Length.to_int t.block_height
      ; epoch_count= Length.to_int t.epoch_count
      ; curr_epoch= Segment_id.to_int epoch
      ; curr_slot= Segment_id.to_int slot
      ; total_currency= Amount.to_int t.total_currency }

    let network_delay (config : Configuration.t) =
      config.acceptable_network_delay

    let curr_global_slot (t : Value.t) = t.curr_global_slot

    let curr_ f = Fn.compose f curr_global_slot

    let curr_epoch_and_slot = curr_ Global_slot.to_epoch_and_slot

    let curr_epoch = curr_ Global_slot.epoch

    let curr_slot = curr_ Global_slot.slot
  end

  module Prover_state = struct
    include Stake_proof

    let precomputed_handler = Vrf.Precomputed.handler

    let handler {delegator; ledger; private_key; public_key}
        ~pending_coinbase:{ Coda_base.Pending_coinbase_witness.pending_coinbases
                          ; is_new_stack } : Snark_params.Tick.Handler.t =
      let ledger_handler = unstage (Coda_base.Sparse_ledger.handler ledger) in
      let pending_coinbase_handler =
        unstage
          (Coda_base.Pending_coinbase.handler pending_coinbases ~is_new_stack)
      in
      let handlers =
        Snarky.Request.Handler.(
          push
            (push fail (create_single pending_coinbase_handler))
            (create_single ledger_handler))
      in
      fun (With {request; respond}) ->
        match request with
        | Vrf.Winner_address ->
            respond (Provide delegator)
        | Vrf.Private_key ->
            respond (Provide private_key)
        | Vrf.Public_key ->
            respond (Provide public_key)
        | _ ->
            respond
              (Provide
                 (Snarky.Request.Handler.run handlers
                    ["Ledger Handler"; "Pending Coinbase Handler"]
                    request))
  end
end

module Hooks = struct
  open Data

  module Rpcs = struct
    open Async

    module Get_epoch_ledger = struct
      module T = struct
        let name = "get_epoch_ledger"

        module T = struct
          type query = Coda_base.Ledger_hash.Stable.V1.t

          type response = (Coda_base.Sparse_ledger.t, string) Result.t
        end

        module Caller = T
        module Callee = T
      end

      include T.T
      module M = Versioned_rpc.Both_convert.Plain.Make (T)
      include M

      include Perf_histograms.Rpc.Plain.Extend (struct
        include M
        include T
      end)

      module V1 = struct
        module T = struct
          type query = Coda_base.Ledger_hash.Stable.V1.t [@@deriving bin_io]

          type response =
            ( Coda_base.Sparse_ledger.Stable.V1.t
            , string )
            Core_kernel.Result.Stable.V1.t
          [@@deriving bin_io, version {rpc}]

          let query_of_caller_model = Fn.id

          let callee_model_of_query = Fn.id

          let response_of_callee_model = Fn.id

          let caller_model_of_response = Fn.id
        end

        include T
        include Register (T)
      end

      let implementation ~logger ~local_state conn ~version:_ ledger_hash =
        let open Coda_base in
        let open Local_state in
        let open Snapshot in
        Deferred.create (fun ivar ->
            Logger.info logger ~module_:__MODULE__ ~location:__LOC__
              ~metadata:
                [ ("peer", `String (Host_and_port.to_string conn))
                ; ("ledger_hash", Coda_base.Ledger_hash.to_yojson ledger_hash)
                ]
              "Serving epoch ledger query with hash $ledger_hash from $peer" ;
            let response =
              if
                Ledger_hash.equal ledger_hash
                  (Frozen_ledger_hash.to_ledger_hash
                     (Lazy.force genesis_ledger_hash))
              then Error "refusing to serve genesis epoch ledger"
              else
                let candidate_snapshots =
                  [ !local_state.Data.staking_epoch_snapshot
                  ; !local_state.Data.next_epoch_snapshot ]
                in
                List.find_map candidate_snapshots ~f:(fun snapshot ->
                    if
                      Ledger_hash.equal ledger_hash
                        (Sparse_ledger.merkle_root snapshot.ledger)
                    then Some snapshot.ledger
                    else None )
                |> Result.of_option ~error:"epoch ledger not found"
            in
            Result.iter_error response ~f:(fun err ->
                Logger.info logger ~module_:__MODULE__ ~location:__LOC__
                  ~metadata:
                    [ ("peer", `String (Host_and_port.to_string conn))
                    ; ("error", `String err)
                    ; ( "ledger_hash"
                      , Coda_base.Ledger_hash.to_yojson ledger_hash ) ]
                  "Failed to serve epoch ledger query with hash $ledger_hash \
                   from $peer: $error" ) ;
            Ivar.fill ivar response )
    end

    let implementations ~logger ~local_state =
      Get_epoch_ledger.(implement_multi (implementation ~logger ~local_state))
  end

  let is_genesis time = Epoch.(equal (of_time_exn time) zero)

  (* Select the correct epoch data to use from a consensus state for a given epoch.
   * The rule for selecting the correct epoch data changes based on whether or not
   * the consensus state we are selecting from is in the epoch we want to select.
   * There is also a special case for when the consensus state we are selecting
   * from is in the genesis epoch.
  *)
  let select_epoch_data ~(consensus_state : Consensus_state.Value.t) ~epoch =
    let curr_epoch = Consensus_state.curr_epoch consensus_state in
    (* are we in the same epoch as the consensus state? *)
    let in_same_epoch = Epoch.equal epoch curr_epoch in
    (* are we in the next epoch after the consensus state? *)
    let in_next_epoch = Epoch.equal epoch (Epoch.succ curr_epoch) in
    (* is the consensus state from the genesis epoch? *)
    let from_genesis_epoch =
      Length.equal consensus_state.epoch_count Length.zero
    in
    if in_next_epoch then
      Ok (Epoch_data.next_to_staking consensus_state.next_epoch_data)
    else if in_same_epoch || from_genesis_epoch then
      Ok consensus_state.staking_epoch_data
    else Error ()

  let epoch_snapshot_name = function
    | `Genesis ->
        "genesis"
    | `Curr ->
        "curr"
    | `Last ->
        "last"

  (* Select the correct epoch snapshot to use from local state for an epoch.
   * The rule for selecting the correct epoch snapshot is predicated off of
   * whether or not the first transition in the epoch in question has been
   * finalized yet, as the local state epoch snapshot pointers are not
   * updated until the consensus state reaches the root of the transition frontier.
   * This function does not guarantee that the selected epoch snapshot is valid
   * (i.e. it does not check that the epoch snapshot's ledger hash is the same
   * as the ledger hash specified by the epoch data).
  *)
  let select_epoch_snapshot ~(consensus_state : Consensus_state.Value.t)
      ~local_state ~epoch =
    let open Local_state in
    let open Epoch_data.Poly in
    (* are we in the next epoch after the consensus state? *)
    let in_next_epoch =
      Epoch.equal epoch
        (Epoch.succ (Consensus_state.curr_epoch consensus_state))
    in
    (* has the first transition in the epoch reached finalization? *)
    let epoch_is_finalized =
      consensus_state.next_epoch_data.epoch_length > Length.of_int Constants.k
    in
    if in_next_epoch || not epoch_is_finalized then
      (`Curr, !local_state.Data.next_epoch_snapshot)
    else (`Last, !local_state.staking_epoch_snapshot)

  type local_state_sync =
    { snapshot_id: Local_state.snapshot_identifier
    ; expected_root: Coda_base.Frozen_ledger_hash.t }
  [@@deriving to_yojson]

  let required_local_state_sync ~(consensus_state : Consensus_state.Value.t)
      ~local_state =
    let open Coda_base in
    let epoch = Consensus_state.curr_epoch consensus_state in
    let source, _snapshot =
      select_epoch_snapshot ~consensus_state ~local_state ~epoch
    in
    let required_snapshot_sync snapshot_id expected_root =
      Option.some_if
        (not
           (Ledger_hash.equal
              (Frozen_ledger_hash.to_ledger_hash expected_root)
              (Sparse_ledger.merkle_root
                 (Local_state.get_snapshot local_state snapshot_id).ledger)))
        {snapshot_id; expected_root}
    in
    match source with
    | `Curr ->
        Option.map
          (required_snapshot_sync Next_epoch_snapshot
             consensus_state.staking_epoch_data.ledger.hash)
          ~f:Non_empty_list.singleton
    | `Last -> (
      match
        Core.List.filter_map
          [ required_snapshot_sync Next_epoch_snapshot
              consensus_state.next_epoch_data.ledger.hash
          ; required_snapshot_sync Staking_epoch_snapshot
              consensus_state.staking_epoch_data.ledger.hash ]
          ~f:Fn.id
      with
      | [] ->
          None
      | ls ->
          Non_empty_list.of_list_opt ls )

  let sync_local_state ~logger ~trust_system ~local_state ~random_peers
      ~(query_peer : Network_peer.query_peer) requested_syncs =
    let open Local_state in
    let open Snapshot in
    let open Deferred.Let_syntax in
    let requested_syncs = Non_empty_list.to_list requested_syncs in
    Logger.info logger
      "Syncing local state; requesting $num_requested snapshots from peers"
      ~location:__LOC__ ~module_:__MODULE__
      ~metadata:
        [ ("num_requested", `Int (List.length requested_syncs))
        ; ( "requested_syncs"
          , `List (List.map requested_syncs ~f:local_state_sync_to_yojson) )
        ; ("local_state", Local_state.to_yojson local_state) ] ;
    let sync {snapshot_id; expected_root= target_ledger_hash} =
      (* if requested last epoch ledger is equal to the current epoch ledger
         then we don't need make a rpc call to the peers. *)
      if
        snapshot_id = Staking_epoch_snapshot
        && Coda_base.(
             Ledger_hash.equal
               (Frozen_ledger_hash.to_ledger_hash target_ledger_hash)
               (Sparse_ledger.merkle_root
                  !local_state.next_epoch_snapshot.ledger))
      then (
        set_snapshot local_state Staking_epoch_snapshot
          { ledger= !local_state.next_epoch_snapshot.ledger
          ; delegatee_table= !local_state.next_epoch_snapshot.delegatee_table
          } ;
        return true )
      else
        Deferred.List.exists (random_peers 3) ~f:(fun peer ->
            match%bind
              query_peer.query peer Rpcs.Get_epoch_ledger.dispatch_multi
                (Coda_base.Frozen_ledger_hash.to_ledger_hash target_ledger_hash)
            with
            | Ok (Ok snapshot_ledger) ->
                let%bind () =
                  Trust_system.(
                    record trust_system logger peer.host
                      Actions.(Epoch_ledger_provided, None))
                in
                let delegatee_table =
                  compute_delegatee_table_sparse_ledger
                    (Local_state.current_proposers local_state)
                    snapshot_ledger
                in
                set_snapshot local_state snapshot_id
                  {ledger= snapshot_ledger; delegatee_table} ;
                return true
            | Ok (Error err) ->
                Logger.faulty_peer_without_punishment logger
                  ~module_:__MODULE__ ~location:__LOC__
                  ~metadata:
                    [ ("peer", Network_peer.Peer.to_yojson peer)
                    ; ("error", `String err) ]
                  "Peer $peer failed to serve requested epoch ledger: $error" ;
                return false
            | Error err ->
                Logger.faulty_peer_without_punishment logger
                  ~module_:__MODULE__ ~location:__LOC__
                  ~metadata:
                    [ ("peer", Network_peer.Peer.to_yojson peer)
                    ; ("error", `String (Error.to_string_hum err)) ]
                  "Error when querying peer $peer for epoch ledger: $error" ;
                return false )
    in
    if%map Deferred.List.for_all requested_syncs ~f:sync then Ok ()
    else Error (Error.of_string "failed to synchronize epoch ledger")

  let received_within_window (epoch, slot) ~time_received =
    let open Time in
    let open Int64 in
    let ( < ) x y = Pervasives.(compare x y < 0) in
    let ( >= ) x y = Pervasives.(compare x y >= 0) in
    let time_received =
      of_span_since_epoch (Span.of_ms (Unix_timestamp.to_int64 time_received))
    in
    let slot_diff =
      Epoch.diff_in_slots
        (Epoch_and_slot.of_time_exn time_received)
        (epoch, slot)
    in
    if slot_diff < 0L then Error `Too_early
    else if slot_diff >= of_int Constants.delta then
      Error (`Too_late slot_diff)
    else Ok ()

  let received_at_valid_time (consensus_state : Consensus_state.Value.t)
      ~time_received =
    received_within_window
      (Consensus_state.curr_epoch_and_slot consensus_state)
      ~time_received

  let select ~existing ~candidate ~logger =
    let string_of_choice = function `Take -> "Take" | `Keep -> "Keep" in
    let log_result choice msg =
      Logger.debug logger ~module_:__MODULE__ ~location:__LOC__
        "Select result: $choice -- $message"
        ~metadata:
          [ ("choice", `String (string_of_choice choice))
          ; ("message", `String msg) ]
    in
    let log_choice ~precondition_msg ~choice_msg choice =
      let choice_msg =
        match choice with
        | `Take ->
            choice_msg
        | `Keep ->
            Printf.sprintf "not (%s)" choice_msg
      in
      let msg = Printf.sprintf "(%s) && (%s)" precondition_msg choice_msg in
      log_result choice msg
    in
    Logger.debug logger ~module_:__MODULE__ ~location:__LOC__
      "Selecting best consensus state"
      ~metadata:
        [ ("existing", Consensus_state.Value.to_yojson existing)
        ; ("candidate", Consensus_state.Value.to_yojson candidate) ] ;
    (* TODO: add fork_before_checkpoint check *)
    (* Each branch contains a precondition predicate and a choice predicate,
     * which takes the new state when true. Each predicate is also decorated
     * with a string description, used for debugging messages *)
    let candidate_vrf_is_bigger =
      let d = Fn.compose Random_oracle.digest_string Vrf.Output.to_string in
      Random_oracle.Digest.( > )
        (d candidate.last_vrf_output)
        (d existing.last_vrf_output)
    in
    let ( << ) a b =
      let c = Length.compare a b in
      c < 0 || (c = 0 && candidate_vrf_is_bigger)
    in
    let ( = ) = Coda_base.State_hash.equal in
    let branches =
      [ ( ( lazy
              ( existing.staking_epoch_data.lock_checkpoint
              = candidate.staking_epoch_data.lock_checkpoint )
          , "last epoch lock checkpoints are equal" )
        , ( lazy (existing.block_height << candidate.block_height)
          , "candidate is longer than existing" ) )
      ; ( ( lazy
              ( existing.staking_epoch_data.start_checkpoint
              = candidate.staking_epoch_data.start_checkpoint )
          , "last epoch start checkpoints are equal" )
        , ( lazy
              ( existing.staking_epoch_data.epoch_length
              << candidate.staking_epoch_data.epoch_length )
          , "candidate last epoch is longer than existing last epoch" ) )
        (* these two could be condensed into one entry *)
      ; ( ( lazy
              (Option.fold existing.next_epoch_data.lock_checkpoint ~init:false
                 ~f:(fun _ existing_curr_lock_checkpoint ->
                   existing_curr_lock_checkpoint
                   = candidate.staking_epoch_data.lock_checkpoint ))
          , "candidate last epoch lock checkpoint is equal to existing \
             current epoch lock checkpoint" )
        , ( lazy (existing.block_height << candidate.block_height)
          , "candidate is longer than existing" ) )
      ; ( ( lazy
              (Option.fold candidate.next_epoch_data.lock_checkpoint
                 ~init:false ~f:(fun _ candidate_curr_lock_checkpoint ->
                   existing.staking_epoch_data.lock_checkpoint
                   = candidate_curr_lock_checkpoint ))
          , "candidate current epoch lock checkpoint is equal to existing \
             last epoch lock checkpoint" )
        , ( lazy (existing.block_height << candidate.block_height)
          , "candidate is longer than existing" ) )
      ; ( ( lazy
              ( existing.next_epoch_data.start_checkpoint
              = candidate.staking_epoch_data.start_checkpoint )
          , "candidate last epoch start checkpoint is equal to existing \
             current epoch start checkpoint" )
        , ( lazy
              ( existing.next_epoch_data.epoch_length
              << candidate.staking_epoch_data.epoch_length )
          , "candidate last epoch is longer than existing current epoch" ) )
      ; ( ( lazy
              ( existing.staking_epoch_data.start_checkpoint
              = candidate.next_epoch_data.start_checkpoint )
          , "candidate current epoch start checkpoint is equal to existing \
             last epoch start checkpoint" )
        , ( lazy
              ( existing.staking_epoch_data.epoch_length
              << candidate.next_epoch_data.epoch_length )
          , "candidate current epoch is longer than existing last epoch" ) ) ]
    in
    let precondition_msg, choice_msg, should_take =
      List.find_map branches
        ~f:(fun ((precondition, precondition_msg), (choice, choice_msg)) ->
          Option.some_if (Lazy.force precondition)
            (precondition_msg, choice_msg, choice) )
      |> Option.value
           ~default:
             ( "default case"
             , "candidate virtual min-length is longer than existing virtual \
                min-length"
             , lazy
                 (let newest_epoch =
                    Epoch.max
                      (Consensus_state.curr_epoch existing)
                      (Consensus_state.curr_epoch candidate)
                  in
                  let virtual_min_length (s : Consensus_state.Value.t) =
                    let curr_epoch = Consensus_state.curr_epoch s in
                    if Epoch.(succ curr_epoch < newest_epoch) then Length.zero
                      (* There is a gap of an entire epoch *)
                    else if Epoch.(succ curr_epoch = newest_epoch) then
                      Length.(
                        min s.min_epoch_length s.next_epoch_data.epoch_length)
                      (* Imagine the latest epoch was padded out with zeros to reach the newest_epoch *)
                    else s.min_epoch_length
                  in
                  Length.(
                    virtual_min_length existing < virtual_min_length candidate))
             )
    in
    let choice = if Lazy.force should_take then `Take else `Keep in
    log_choice ~precondition_msg ~choice_msg choice ;
    choice

  let next_proposal now (state : Consensus_state.Value.t) ~local_state
      ~keypairs ~logger =
    let info_if_proposing =
      if Keypair.And_compressed_pk.Set.is_empty keypairs then Logger.debug
      else Logger.info
    in
    info_if_proposing logger ~module_:__MODULE__ ~location:__LOC__
      "Determining next slot to produce block" ;
    let curr_epoch, curr_slot =
      Epoch.epoch_and_slot_of_time_exn
        (Coda_base.Block_time.of_span_since_epoch
           (Coda_base.Block_time.Span.of_ms now))
    in
    let epoch, slot =
      if
        Epoch.equal curr_epoch (Consensus_state.curr_epoch state)
        && Epoch.Slot.equal curr_slot (Consensus_state.curr_slot state)
      then Epoch.incr (curr_epoch, curr_slot)
      else (curr_epoch, curr_slot)
    in
    Logger.debug logger ~module_:__MODULE__ ~location:__LOC__
      "Systime: %d, epoch-slot@systime: %08d-%04d, starttime@epoch@systime: %d"
      (Int64.to_int now) (Epoch.to_int epoch) (Epoch.Slot.to_int slot)
      ( Int64.to_int @@ Time.Span.to_ms @@ Time.to_span_since_epoch
      @@ Epoch.start_time epoch ) ;
    let next_slot =
      Logger.debug logger ~module_:__MODULE__ ~location:__LOC__
        !"Selecting correct epoch data from state -- epoch by time: %d, state \
          epoch: %d, state epoch count: %d"
        (Epoch.to_int epoch)
        (Epoch.to_int (Consensus_state.curr_epoch state))
        (Length.to_int state.epoch_count) ;
      let epoch_data =
        match select_epoch_data ~consensus_state:state ~epoch with
        | Ok epoch_data ->
            epoch_data
        | Error () ->
            Logger.error logger ~module_:__MODULE__ ~location:__LOC__
              "System time is out of sync with protocol state time" ;
            failwith
              "System time is out of sync with protocol state time; please \
               setup NTP if you haven't)"
      in
      let total_stake = epoch_data.ledger.total_currency in
      let epoch_snapshot =
        let source, snapshot =
          select_epoch_snapshot ~consensus_state:state ~local_state ~epoch
        in
        Logger.debug logger ~module_:__MODULE__ ~location:__LOC__
          !"Using %s_epoch_snapshot root hash %{sexp:Coda_base.Ledger_hash.t}"
          (epoch_snapshot_name source)
          (Coda_base.Sparse_ledger.merkle_root snapshot.ledger) ;
        snapshot
      in
      let proposal_data unseen_pks slot =
        (* Try vrfs for all keypairs that are unseen within this slot until one wins or all lose *)
        (* TODO: Don't do this, and instead pick the one that has the highest
         * chance of winning. See #2573 *)
        Keypair.And_compressed_pk.Set.fold_until keypairs ~init:()
          ~f:(fun () (keypair, public_key_compressed) ->
            if
              not
              @@ Public_key.Compressed.Set.mem unseen_pks public_key_compressed
            then Continue_or_stop.Continue ()
            else
              match
                Vrf.check ~epoch ~slot ~seed:epoch_data.seed ~epoch_snapshot
                  ~private_key:keypair.private_key
                  ~public_key:keypair.public_key ~public_key_compressed
                  ~total_stake ~logger
              with
              | None ->
                  Continue_or_stop.Continue ()
              | Some data ->
                  Continue_or_stop.Stop (Some (keypair, data)) )
          ~finish:(fun () -> None)
      in
      let rec find_winning_slot (slot : Epoch.Slot.t) =
        if UInt32.of_int (Epoch.Slot.to_int slot) >= Constants.Epoch.size then
          None
        else
          match Local_state.seen_slot local_state epoch slot with
          | `All_seen ->
              find_winning_slot (Epoch.Slot.succ slot)
          | `Unseen pks -> (
            match proposal_data pks slot with
            | None ->
                find_winning_slot (Epoch.Slot.succ slot)
            | Some (keypair, data) ->
                Some (slot, keypair, data) )
      in
      find_winning_slot slot
    in
    let ms_since_epoch = Fn.compose Time.Span.to_ms Time.to_span_since_epoch in
    match next_slot with
    | Some (next_slot, keypair, data) ->
        info_if_proposing logger ~module_:__MODULE__ ~location:__LOC__
          "Producing block in %d slots"
          (Epoch.Slot.to_int next_slot - Epoch.Slot.to_int slot) ;
        if Epoch.Slot.equal curr_slot next_slot then
          `Propose_now (keypair, data)
        else
          `Propose
            ( Epoch.slot_start_time epoch next_slot
              |> Time.to_span_since_epoch |> Time.Span.to_ms
            , keypair
            , data )
    | None ->
        let epoch_end_time = Epoch.end_time epoch |> ms_since_epoch in
        info_if_proposing logger ~module_:__MODULE__ ~location:__LOC__
          "No slots won in this epoch. Waiting for next epoch to check again, \
           @%d"
          (Int64.to_int epoch_end_time) ;
        `Check_again epoch_end_time

  let frontier_root_transition (prev : Consensus_state.Value.t)
      (next : Consensus_state.Value.t) ~local_state ~snarked_ledger =
    if
      not
        (Epoch.equal
           (Consensus_state.curr_epoch prev)
           (Consensus_state.curr_epoch next))
    then (
      let delegatee_table =
        compute_delegatee_table (Local_state.current_proposers local_state)
          ~iter_accounts:(fun f ->
            Coda_base.Ledger.Any_ledger.M.iteri snarked_ledger ~f )
      in
      let ledger = Coda_base.Sparse_ledger.of_any_ledger snarked_ledger in
      let epoch_snapshot = {Local_state.Snapshot.delegatee_table; ledger} in
      !local_state.staking_epoch_snapshot <- !local_state.next_epoch_snapshot ;
      !local_state.next_epoch_snapshot <- epoch_snapshot )

  let should_bootstrap_len ~existing ~candidate =
    let length = Length.to_int in
    length candidate - length existing > (2 * Constants.k) + Constants.delta

  let should_bootstrap ~existing ~candidate ~logger =
    match select ~existing ~candidate ~logger with
    | `Keep ->
        false
    | `Take ->
        should_bootstrap_len
          ~existing:(Consensus_state.block_height existing)
          ~candidate:(Consensus_state.block_height candidate)

  let%test "should_bootstrap is sane" =
    (* Even when consensus constants are of prod sizes, candidate should still trigger a bootstrap *)
    should_bootstrap_len ~existing:Length.zero
      ~candidate:(Length.of_int 100_000_000)

  let to_unix_timestamp recieved_time =
    recieved_time |> Time.to_span_since_epoch |> Time.Span.to_ms
    |> Unix_timestamp.of_int64

  let%test "Receive a valid consensus_state with a bit of delay" =
    let curr_epoch, curr_slot =
      Consensus_state.curr_epoch_and_slot
        (Lazy.force Consensus_state.negative_one)
    in
    let delay = Constants.delta / 2 |> UInt32.of_int in
    let new_slot = UInt32.Infix.(curr_slot + delay) in
    let time_received = Epoch.slot_start_time curr_epoch new_slot in
    received_at_valid_time
      (Lazy.force Consensus_state.negative_one)
      ~time_received:(to_unix_timestamp time_received)
    |> Result.is_ok

  let%test "Receive an invalid consensus_state" =
    let epoch = Epoch.of_int 5 in
    let start_time = Epoch.start_time epoch in
    let ((curr_epoch, curr_slot) as curr) =
      Epoch_and_slot.of_time_exn start_time
    in
    let consensus_state =
      { (Lazy.force Consensus_state.negative_one) with
        curr_global_slot= Global_slot.of_epoch_and_slot curr }
    in
    let too_early =
      (* TODO: Does this make sense? *)
      Epoch.start_time
        (Consensus_state.curr_slot (Lazy.force Consensus_state.negative_one))
    in
    let too_late =
      let delay = Constants.delta * 2 |> UInt32.of_int in
      let delayed_slot = UInt32.Infix.(curr_slot + delay) in
      Epoch.slot_start_time curr_epoch delayed_slot
    in
    let times = [too_late; too_early] in
    List.for_all times ~f:(fun time ->
        not
          ( received_at_valid_time consensus_state
              ~time_received:(to_unix_timestamp time)
          |> Result.is_ok ) )

  module type State_hooks_intf =
    Intf.State_hooks_intf
    with type consensus_state := Consensus_state.Value.t
     and type consensus_state_var := Consensus_state.var
     and type consensus_transition := Consensus_transition.t
     and type proposal_data := Proposal_data.t

  module Make_state_hooks
      (Blockchain_state : Intf.Blockchain_state_intf)
      (Protocol_state : Intf.Protocol_state_intf
                        with type blockchain_state := Blockchain_state.Value.t
                         and type blockchain_state_var := Blockchain_state.var
                         and type consensus_state := Consensus_state.Value.t
                         and type consensus_state_var := Consensus_state.var)
      (Snark_transition : Intf.Snark_transition_intf
                          with type blockchain_state_var :=
                                      Blockchain_state.var
                           and type consensus_transition_var :=
                                      Consensus_transition.var) :
    State_hooks_intf
    with type blockchain_state := Blockchain_state.Value.t
     and type protocol_state := Protocol_state.Value.t
     and type protocol_state_var := Protocol_state.var
     and type snark_transition_var := Snark_transition.var = struct
    (* TODO: only track total currency from accounts > 1% of the currency using transactions *)

    let check_proposal_data ~logger (proposal_data : Proposal_data.t)
        epoch_and_slot =
      if not (Epoch_and_slot.equal epoch_and_slot proposal_data.epoch_and_slot)
      then
        Logger.error ~module_:__MODULE__ ~location:__LOC__ logger
          !"VRF was evaluated at (epoch, slot) %{sexp:Epoch_and_slot.t} but \
            the corresponding proposal happened at a time corresponding to \
            %{sexp:Epoch_and_slot.t}. This means that generating the proposal \
            took more time than expected."
          proposal_data.epoch_and_slot epoch_and_slot

    let generate_transition ~(previous_protocol_state : Protocol_state.Value.t)
        ~blockchain_state ~current_time ~(proposal_data : Proposal_data.t)
        ~transactions:_ ~snarked_ledger_hash ~supply_increase ~logger =
      let previous_consensus_state =
        Protocol_state.consensus_state previous_protocol_state
      in
      (let actual_epoch_and_slot =
         let time = Time.of_span_since_epoch (Time.Span.of_ms current_time) in
         Epoch_and_slot.of_time_exn time
       in
       check_proposal_data ~logger proposal_data actual_epoch_and_slot) ;
      let consensus_transition =
        Global_slot.of_epoch_and_slot proposal_data.epoch_and_slot
      in
      let consensus_state =
        Or_error.ok_exn
          (Consensus_state.update ~previous_consensus_state
             ~consensus_transition
             ~proposer_vrf_result:proposal_data.Proposal_data.vrf_result
             ~previous_protocol_state_hash:
               (Protocol_state.hash previous_protocol_state)
             ~supply_increase ~snarked_ledger_hash)
      in
      let protocol_state =
        Protocol_state.create_value
          ~previous_state_hash:(Protocol_state.hash previous_protocol_state)
          ~blockchain_state ~consensus_state
      in
      (protocol_state, consensus_transition)

    include struct
      let%snarkydef next_state_checked ~(prev_state : Protocol_state.var)
          ~(prev_state_hash : Coda_base.State_hash.var) transition
          supply_increase =
        Consensus_state.update_var
          (Protocol_state.consensus_state prev_state)
          (Snark_transition.consensus_transition transition)
          prev_state_hash ~supply_increase
          ~previous_blockchain_state_ledger_hash:
            ( Protocol_state.blockchain_state prev_state
            |> Blockchain_state.snarked_ledger_hash )
    end

    module For_tests = struct
      let gen_consensus_state
          ~(gen_slot_advancement : int Quickcheck.Generator.t) :
          (   previous_protocol_state:( Protocol_state.Value.t
                                      , Coda_base.State_hash.t )
                                      With_hash.t
           -> snarked_ledger_hash:Coda_base.Frozen_ledger_hash.t
           -> Consensus_state.Value.t)
          Quickcheck.Generator.t =
        let open Consensus_state in
        let open Quickcheck.Let_syntax in
        let%bind slot_advancement = gen_slot_advancement in
        let%map proposer_vrf_result = Vrf.Output.gen in
        fun ~(previous_protocol_state :
               (Protocol_state.Value.t, Coda_base.State_hash.t) With_hash.t)
            ~(snarked_ledger_hash : Coda_base.Frozen_ledger_hash.t) ->
          let prev =
            Protocol_state.consensus_state
              (With_hash.data previous_protocol_state)
          in
          let block_height = Length.succ prev.block_height in
          let curr_global_slot =
            Global_slot.(prev.curr_global_slot + slot_advancement)
          in
          let curr_epoch, curr_slot =
            Global_slot.to_epoch_and_slot curr_global_slot
          in
          let total_currency =
            Option.value_exn
              (Amount.add prev.total_currency Constants.coinbase)
          in
          let prev_epoch, prev_slot =
            Consensus_state.curr_epoch_and_slot prev
          in
          let staking_epoch_data, next_epoch_data, epoch_count =
            Epoch_data.update_pair
              (prev.staking_epoch_data, prev.next_epoch_data)
              prev.epoch_count ~prev_epoch ~next_epoch:curr_epoch ~prev_slot
              ~prev_protocol_state_hash:
                (With_hash.hash previous_protocol_state)
              ~proposer_vrf_result ~snarked_ledger_hash ~total_currency
          in
          let checkpoints =
            if prev.has_ancestor_in_same_checkpoint_window then
              prev.checkpoints
            else Checkpoints.cons previous_protocol_state.hash prev.checkpoints
          in
          { Poly.block_height
          ; epoch_count
          ; min_epoch_length=
              ( if Epoch.equal prev_epoch curr_epoch then prev.min_epoch_length
              else if Epoch.(equal curr_epoch (succ prev_epoch)) then
                Length.min prev.min_epoch_length
                  prev.next_epoch_data.epoch_length
              else Length.zero )
          ; last_vrf_output= proposer_vrf_result
          ; total_currency
          ; curr_global_slot
          ; staking_epoch_data
          ; next_epoch_data
          ; has_ancestor_in_same_checkpoint_window=
              same_checkpoint_window_unchecked
                (Global_slot.create ~epoch:prev_epoch ~slot:prev_slot)
                (Global_slot.create ~epoch:curr_epoch ~slot:curr_slot)
          ; checkpoints }
    end
  end
end

let time_hum (now : Coda_base.Block_time.t) =
  let epoch, slot = Data.Epoch.epoch_and_slot_of_time_exn now in
  Printf.sprintf "epoch=%d, slot=%d" (Data.Epoch.to_int epoch)
    (Data.Epoch.Slot.to_int slot)

let%test_module "Proof of stake tests" =
  ( module struct
    open Coda_base
    open Data
    open Consensus_state

    let%test_unit "update, update_var agree starting from same genesis state" =
      (* build pieces needed to apply "update" *)
      let snarked_ledger_hash =
        Frozen_ledger_hash.of_ledger_hash
          (Ledger.merkle_root (Lazy.force Genesis_ledger.t))
      in
      let previous_protocol_state_hash = State_hash.(of_hash zero) in
      let previous_consensus_state =
        Consensus_state.create_genesis
          ~negative_one_protocol_state_hash:previous_protocol_state_hash
      in
      let global_slot =
        Core_kernel.Time.now () |> Time.of_time |> Epoch_and_slot.of_time_exn
        |> Global_slot.of_epoch_and_slot
      in
      let consensus_transition : Consensus_transition.t = global_slot in
      let supply_increase = Currency.Amount.of_int 42 in
      (* setup ledger, needed to compute proposer_vrf_result here and handler below *)
      let open Coda_base in
      (* choose largest account as most likely to propose *)
      let ledger_data = Lazy.force Genesis_ledger.t in
      let ledger = Ledger.Any_ledger.cast (module Ledger) ledger_data in
      let pending_coinbases = Pending_coinbase.create () |> Or_error.ok_exn in
      let maybe_sk, account = Genesis_ledger.largest_account_exn () in
      let private_key = Option.value_exn maybe_sk in
      let public_key_compressed = Account.public_key account in
      let location =
        Ledger.Any_ledger.M.location_of_key ledger public_key_compressed
      in
      let delegator =
        Option.value_exn location |> Ledger.Any_ledger.M.Location.to_path_exn
        |> Ledger.Addr.to_int
      in
      let proposer_vrf_result =
        let epoch, slot = Global_slot.to_epoch_and_slot global_slot in
        Vrf.eval ~private_key {epoch; slot; seed= Epoch_seed.initial; delegator}
      in
      let next_consensus_state =
        update ~previous_consensus_state ~consensus_transition
          ~previous_protocol_state_hash ~supply_increase ~snarked_ledger_hash
          ~proposer_vrf_result
        |> Or_error.ok_exn
      in
      (* build pieces needed to apply "update_var" *)
      let checked_computation =
        let open Snark_params.Tick in
        (* work in Checked monad *)
        let%bind previous_state =
          exists typ ~compute:(As_prover.return previous_consensus_state)
        in
        let%bind transition_data =
          exists Consensus_transition.typ
            ~compute:(As_prover.return consensus_transition)
        in
        let%bind previous_protocol_state_hash =
          exists State_hash.typ
            ~compute:(As_prover.return previous_protocol_state_hash)
        in
        let%bind supply_increase =
          exists Amount.typ ~compute:(As_prover.return supply_increase)
        in
        let%bind previous_blockchain_state_ledger_hash =
          exists Coda_base.Frozen_ledger_hash.typ
            ~compute:(As_prover.return snarked_ledger_hash)
        in
        let result =
          update_var previous_state transition_data
            previous_protocol_state_hash ~supply_increase
            ~previous_blockchain_state_ledger_hash
        in
        (* setup handler *)
        let indices =
          Ledger.Any_ledger.M.foldi ~init:[] ledger ~f:(fun i accum _acct ->
              Ledger.Any_ledger.M.Addr.to_int i :: accum )
        in
        let sparse_ledger =
          Sparse_ledger.of_ledger_index_subset_exn ledger indices
        in
        let public_key = Public_key.decompress_exn public_key_compressed in
        let handler =
          Prover_state.handler
            {delegator; ledger= sparse_ledger; private_key; public_key}
            ~pending_coinbase:
              {Pending_coinbase_witness.pending_coinbases; is_new_stack= true}
        in
        let%map `Success _, var = Snark_params.Tick.handle result handler in
        As_prover.read typ var
      in
      let (), checked_value =
        Or_error.ok_exn
        @@ Snark_params.Tick.run_and_check checked_computation ()
      in
      assert (Value.equal checked_value next_consensus_state) ;
      ()
  end )
