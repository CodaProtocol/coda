open Core_kernel
open Async_kernel

module type S = sig
  include Minibit.Ledger_fetcher_intf
end

module type Inputs_intf = sig
  include Protocols.Minibit_pow.Inputs_intf
  module Net : Minibit.Network_intf with type ledger := Ledger.t
                                     and type ledger_hash := Ledger_hash.t
                                     and type state := State.t
  module Store : Storage.With_checksum_intf
end

module Make
  (Inputs : Inputs_intf)
= struct
  open Inputs

  module Config = struct
    type t =
      { keep_count : int [@default 50]
      ; parent_log : Logger.t
      ; net_deferred : Net.t Deferred.t
      ; ledger_transitions : (Ledger_hash.t * Transaction.With_valid_signature.t list * State.t) Linear_pipe.Reader.t
      ; disk_location : Store.location
      }
    [@@deriving make]
  end

  let heap_cmp (_, s) (_, s') = Strength.compare s s'

  module State = struct
    module T = struct
      type t =
        { strongest_ledgers : (Ledger_hash.t * Strength.t) Heap.t
        ; hash_to_ledger : (Ledger.t * State.t) Ledger_hash.Table.t
        }
    end

    include T
    include Bin_prot.Utils.Make_binable(struct
      module Binable = struct
        type t =
          { strongest_ledgers : (Ledger_hash.t * Strength.t) list
          ; hash_to_ledger : (Ledger.t * State.t) Ledger_hash.Table.t
          }
        [@@deriving bin_io]
      end

      type t = T.t
      let to_binable ({strongest_ledgers ; hash_to_ledger} : t) : Binable.t =
        { strongest_ledgers = Heap.to_list strongest_ledgers
        ; hash_to_ledger
        }

      let of_binable ({Binable.strongest_ledgers ; hash_to_ledger} : Binable.t) : t =
        { strongest_ledgers = Heap.of_list strongest_ledgers ~cmp:heap_cmp
        ; hash_to_ledger
        }
    end)

    let create () : t =
      { strongest_ledgers = Heap.create ~cmp:heap_cmp ()
      ; hash_to_ledger = Ledger_hash.Table.create ()
      }
  end

  type t =
    { state : State.t
    ; net : Net.t Deferred.t
    ; log : Logger.t
    ; keep_count : int
    ; storage_controller : State.t Store.Controller.t
    ; disk_location : Store.location
    }

  (* For now: Keep the top 50 ledgers (by strength), prune everything else *)
  let prune t =
    let rec go () =
      if Heap.length t.state.strongest_ledgers > t.keep_count then begin
        let (h, _) = Heap.pop_exn t.state.strongest_ledgers in
        Ledger_hash.Table.remove t.state.hash_to_ledger h;
        go ()
      end
    in
    go ()

  let add t h ledger state =
    Ledger_hash.Table.set t.state.hash_to_ledger ~key:h ~data:(ledger, state);
    Heap.add t.state.strongest_ledgers (h, state.strength);
    prune t

  let local_get t h =
    match Ledger_hash.Table.find t.state.hash_to_ledger h with
    | None -> Or_error.errorf !"Couldn't find %{sexp:Ledger_hash.t} locally" h
    | Some x -> Or_error.return x

  let get t h : Ledger.t Deferred.Or_error.t =
    match local_get t h with
    | Error _ ->
      let%bind net = t.net in
      let open Deferred.Or_error.Let_syntax in
      let%map (ledger, state) = Net.Ledger_fetcher_io.get_ledger_at_hash net h in
      add t h ledger state;
      ledger
    | Ok (l,s) -> Deferred.Or_error.return l

  let create (config : Config.t) =
    let storage_controller =
      Store.Controller.create
        ~parent_log:config.parent_log
        { Bin_prot.Type_class.writer = State.bin_writer_t
        ; reader = State.bin_reader_t
        ; shape = State.bin_shape_t
        }
    in
    let log = Logger.child config.parent_log "ledger-fetcher" in
    let%map state =
      match%map Store.load storage_controller config.disk_location with
      | Ok state -> state
      | Error (`IO_error e) ->
        Logger.info log "Ledger failed to load from storage %s; recreating" (Error.to_string_hum e);
        State.create ()
      | Error `No_exist ->
        Logger.info log "Ledger doesn't exist in storage; recreating";
        State.create ()
      | Error `Checksum_no_match ->
        Logger.warn log "Checksum failed when loading ledger, recreating";
        State.create ()
    in
    let t =
      { state
      ; net = config.net_deferred
      ; log
      ; keep_count = config.keep_count
      ; storage_controller
      ; disk_location = config.disk_location
      }
    in
    don't_wait_for begin
      Linear_pipe.iter config.ledger_transitions ~f:(fun (h, transactions, state) ->
        let open Deferred.Let_syntax in
        (* Notice: This pipe iter blocks upstream while it's materializing ledgers from the network (potentially) AND saving to disk *)
        match%bind get t h with
        | Error e ->
          Logger.warn t.log "Failed to keep-up with transactions (can't get ledger %s)" (Error.to_string_hum e);
          return ()
        | Ok unsafe_ledger ->
          let ledger = Ledger.copy unsafe_ledger in
          List.iter transactions ~f:(fun transaction ->
            match Ledger.apply_transaction ledger transaction with
            | Error e ->
                Logger.warn t.log "Failed to apply a transaction %s" (Error.to_string_hum e)
            | Ok () -> ()
          );
          add t h ledger state;
          (* TODO: Make state saving more efficient and in appropriate places (see #180) *)
          Store.store t.storage_controller t.disk_location t.state
      )
    end;
    t
end

