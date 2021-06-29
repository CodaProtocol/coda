open Core
open Async
open Signature_lib
open Pipe_lib
module Epoch = Mina_numbers.Length
module Global_slot = Mina_numbers.Global_slot

(*Slot number within an epoch*)
module Slot = Mina_numbers.Global_slot

(* Can extract both slot numbers and epoch number*)
module Consensus_time = Consensus.Data.Consensus_time

module State = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t =
        { block_producer_keys:
            (Keypair.Stable.V1.t * Public_key.Compressed.Stable.V1.t) list }

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t =
    {block_producer_keys: (Keypair.t * Public_key.Compressed.t) list}
  [@@deriving sexp]
end

module Evaluator_status = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t = At of Global_slot.Stable.V1.t | Completed

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t = At of Global_slot.t | Completed
  [@@deriving sexp]
end

module Vrf_evaluation_result = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t =
        { slots_won: Consensus.Data.Slot_won.Stable.V1.t list
        ; evaluator_status: Evaluator_status.Stable.V1.t }

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t =
    { slots_won: Consensus.Data.Slot_won.t list
    ; evaluator_status: Evaluator_status.t }
end

module Worker_state = struct
  type init_arg =
    { constraint_constants:
        Genesis_constants.Constraint_constants.Stable.Latest.t
    ; consensus_constants: Consensus.Constants.Stable.Latest.t
    ; conf_dir: string
    ; logger: Logger.Stable.Latest.t }
  [@@deriving bin_io_unversioned]

  type t =
    { config: init_arg
    ; mutable last_checked_slot_and_epoch:
        (Epoch.t * Slot.t) Public_key.Compressed.Table.t
    ; slots_won: Consensus.Data.Slot_won.t Queue.t
          (*possibly multiple producers per slot*)
    ; mutable current_slot: Global_slot.t option
    ; mutable current_epoch: Epoch.t
    ; mutable epoch_state: State.t
    ; reset_writer:
        ( Consensus.Data.Epoch_data_for_vrf.t
        , Strict_pipe.synchronous
        , unit Deferred.t )
        Strict_pipe.Writer.t }

  let make_last_checked_slot_and_epoch_table old_table new_keys ~default =
    let module Set = Public_key.Compressed.Set in
    let module Table = Public_key.Compressed.Table in
    let last_checked_slot_and_epoch = Table.create () in
    List.iter new_keys ~f:(fun (_, pk) ->
        let data = Option.value (Table.find old_table pk) ~default in
        Table.add_exn last_checked_slot_and_epoch ~key:pk ~data ) ;
    last_checked_slot_and_epoch

  let seen_slot last_checked_slot_and_epoch epoch slot =
    let module Table = Public_key.Compressed.Table in
    let unseens =
      Table.to_alist last_checked_slot_and_epoch
      |> List.filter_map ~f:(fun (pk, last_checked_epoch_and_slot) ->
             let i =
               Tuple2.compare ~cmp1:Epoch.compare ~cmp2:Slot.compare
                 last_checked_epoch_and_slot (epoch, slot)
             in
             if i > 0 then None
             else if i = 0 then
               (*vrf evaluation was stopped at this point because it was either the end of the epoch or the key won this slot; re-check this slot when staking keys are reset so that we don't skip producing block. This will not occur in the normal flow because [slot] will be greater than the last-checked-slot*)
               Some pk
             else (
               Table.set last_checked_slot_and_epoch ~key:pk ~data:(epoch, slot) ;
               Some pk ) )
    in
    match unseens with
    | [] ->
        `All_seen
    | nel ->
        `Unseen (Public_key.Compressed.Set.of_list nel)

  let evaluate (epoch_data : Consensus.Data.Epoch_data_for_vrf.t)
      ({config; slots_won; epoch_state; _} as t) : unit Deferred.t =
    let module Slot = Mina_numbers.Global_slot in
    let logger = config.logger in
    let epoch = epoch_data.epoch in
    [%log info] "Starting VRF evaluation for epoch: $epoch"
      ~metadata:[("epoch", Epoch.to_yojson epoch)] ;
    let keypairs = epoch_state.block_producer_keys in
    let logger = config.logger in
    let start_global_slot = epoch_data.global_slot in
    let start_global_slot_since_genesis =
      epoch_data.global_slot_since_genesis
    in
    let constants = config.consensus_constants in
    let delegatee_table = epoch_data.delegatee_table in
    (*slot in the epoch*)
    let start_consensus_time =
      Consensus.Data.Consensus_time.(
        of_global_slot ~constants start_global_slot)
    in
    let total_stake = epoch_data.epoch_ledger.total_currency in
    let evaluate_vrf ~consensus_time =
      (* Try vrfs for all keypairs that are unseen within this slot until one wins or all lose *)
      (* TODO: Don't do this, and instead pick the one that has the highest chance of winning. See #2573 *)
      let slot = Consensus_time.slot consensus_time in
      let global_slot = Consensus_time.to_global_slot consensus_time in
      [%log info] "Checking VRF evaluations for epoch: $epoch, slot: $slot"
        ~metadata:
          [ ("epoch", `Int (Epoch.to_int epoch))
          ; ("slot", `Int (Slot.to_int slot)) ] ;
      Deferred.List.find_map keypairs
        ~f:(fun ((keypair : Keypair.t), public_key_compressed) ->
          let global_slot_since_genesis =
            let slot_diff =
              match Global_slot.sub global_slot start_global_slot with
              | None ->
                  failwith
                    "Checking slot-winner for a slot which is older than the \
                     slot in the latest consensus state. System time might be \
                     out-of-sync"
              | Some diff ->
                  diff
            in
            Global_slot.add start_global_slot_since_genesis slot_diff
          in
          match%map
            Consensus.Data.Vrf.check
              ~constraint_constants:config.constraint_constants ~global_slot
              ~seed:epoch_data.epoch_seed
              ~get_delegators:
                (Public_key.Compressed.Table.find delegatee_table)
              ~producer_private_key:keypair.private_key
              ~producer_public_key:public_key_compressed ~total_stake ~logger
          with
          | None ->
              None
          | Some (`Vrf_output vrf_result, `Delegator delegator) ->
              [%log info] "Won slot %d in epoch %d" (Slot.to_int slot)
                (Epoch.to_int epoch) ;
              let slot_won =
                Consensus.Data.Slot_won.
                  { delegator
                  ; producer= keypair
                  ; global_slot
                  ; global_slot_since_genesis
                  ; vrf_result }
              in
              Some slot_won )
    in
    let rec find_winning_slot (consensus_time : Consensus_time.t) =
      let global_slot = Consensus_time.to_global_slot consensus_time in
      t.current_slot <- Some global_slot ;
      let epoch' = Consensus_time.epoch consensus_time in
      if Epoch.(epoch' > epoch) then (
        t.current_slot <- None ;
        Deferred.unit )
      else
        let start = Time.now () in
        match%bind evaluate_vrf ~consensus_time with
        | None ->
            [%log info] "Did not win a slot, took $time ms"
              ~metadata:
                [("time", `Float Time.(Span.to_ms (diff (now ()) start)))] ;
            find_winning_slot (Consensus_time.succ consensus_time)
        | Some slot_won ->
            [%log info] "Won a slot, took $time ms"
              ~metadata:
                [("time", `Float Time.(Span.to_ms (diff (now ()) start)))] ;
            Queue.enqueue slots_won slot_won ;
            find_winning_slot (Consensus_time.succ consensus_time)
    in
    find_winning_slot start_consensus_time

  let create config =
    let reset_reader, reset_writer =
      Pipe_lib.Strict_pipe.create ~name:"Vrf-evaluator reset"
        Pipe_lib.Strict_pipe.Synchronous
    in
    let t =
      { config
      ; last_checked_slot_and_epoch= Public_key.Compressed.Table.create ()
      ; slots_won=
          Queue.create
            ~capacity:
              (Global_slot.to_int config.consensus_constants.slots_per_epoch)
            ()
      ; current_slot= None
      ; current_epoch= Epoch.zero
      ; epoch_state= {block_producer_keys= []}
      ; reset_writer }
    in
    don't_wait_for
      (Strict_pipe.Reader.iter_without_pushback reset_reader
         ~f:(fun epoch_data ->
           [%log' info config.logger] "Resetting epoch data for epoch %d"
             (Epoch.to_int epoch_data.epoch) ;
           Queue.clear t.slots_won ;
           t.current_slot <- None ;
           t.current_epoch <- epoch_data.epoch ;
           don't_wait_for (evaluate epoch_data t) )) ;
    return t
end

module Functions = struct
  type ('i, 'o) t =
    'i Bin_prot.Type_class.t
    * 'o Bin_prot.Type_class.t
    * (Worker_state.t -> 'i -> 'o Deferred.t)

  let create input output f : ('i, 'o) t = (input, output, f)

  let set_new_epoch_state =
    create Consensus.Data.Epoch_data_for_vrf.Stable.Latest.bin_t Unit.bin_t
      (fun w e ->
        let logger = w.config.logger in
        let update () =
          [%log info]
            "Updating epoch data for the VRF evaluation for epoch $epoch"
            ~metadata:[("epoch", Epoch.to_yojson e.epoch)] ;
          Strict_pipe.Writer.write w.reset_writer e
        in
        if Epoch.(succ e.epoch > w.current_epoch) then (
          [%log info] "Updating epoch data" ;
          update () )
        else (
          [%log info] "Received epoch data for current epoch $epoch. Skipping "
            ~metadata:[("epoch", Epoch.to_yojson e.epoch)] ;
          Deferred.unit ) )

  let slots_won_so_far =
    create Unit.bin_t Vrf_evaluation_result.Stable.Latest.bin_t (fun w () ->
        let slots_won = Queue.to_list w.slots_won in
        [%log' info w.config.logger]
          !"Slots won evaluator: %{sexp: Consensus.Data.Slot_won.t list}"
          slots_won ;
        Queue.clear w.slots_won ;
        let evaluator_status =
          match w.current_slot with
          | Some slot ->
              Evaluator_status.At slot
          | None ->
              Completed
        in
        return Vrf_evaluation_result.{slots_won; evaluator_status} )

  let update_block_producer_keys =
    create State.Stable.Latest.bin_t Unit.bin_t (fun w e ->
        let logger = w.config.logger in
        [%log info] "Updating block producer keys" ;
        w.epoch_state <- e ;
        Deferred.unit )
end

module Worker = struct
  module T = struct
    type 'worker functions =
      { set_new_epoch_state:
          ( 'worker
          , Consensus.Data.Epoch_data_for_vrf.t
          , unit )
          Rpc_parallel.Function.t
      ; slots_won_so_far:
          ('worker, unit, Vrf_evaluation_result.t) Rpc_parallel.Function.t
      ; update_block_producer_keys:
          ('worker, State.t, unit) Rpc_parallel.Function.t }

    module Worker_state = Worker_state

    module Connection_state = struct
      type init_arg = unit [@@deriving bin_io_unversioned]

      type t = unit
    end

    module Functions
        (C : Rpc_parallel.Creator
             with type worker_state := Worker_state.t
              and type connection_state := Connection_state.t) =
    struct
      let functions =
        let f (i, o, f) =
          C.create_rpc
            ~f:(fun ~worker_state ~conn_state:_ i -> f worker_state i)
            ~bin_input:i ~bin_output:o ()
        in
        let open Functions in
        { set_new_epoch_state= f set_new_epoch_state
        ; slots_won_so_far= f slots_won_so_far
        ; update_block_producer_keys= f update_block_producer_keys }

      let init_worker_state (init_arg : Worker_state.init_arg) =
        let logger = init_arg.logger in
        let max_size = 200 * 1024 * 1024 in
        let num_rotate = 1 in
        Logger.Consumer_registry.register ~id:"default"
          ~processor:(Logger.Processor.raw ())
          ~transport:
            (Logger.Transport.File_system.dumb_logrotate
               ~directory:init_arg.conf_dir
               ~log_filename:"mina-vrf-evaluator.log" ~max_size ~num_rotate) ;
        [%log info] "Vrf_evaluator started" ;
        Worker_state.create init_arg

      let init_connection_state ~connection:_ ~worker_state:_ = return
    end
  end

  include Rpc_parallel.Make (T)
end

type t = {connection: Worker.Connection.t; process: Process.t}

let update_block_producer_keys {connection; process= _} ~keypairs =
  Worker.Connection.run connection
    ~f:Worker.functions.update_block_producer_keys
    ~arg:
      State.
        {block_producer_keys= Keypair.And_compressed_pk.Set.to_list keypairs}

let create ~constraint_constants ~pids ~consensus_constants ~conf_dir ~logger
    ~keypairs =
  let on_failure err =
    [%log error] "VRF evaluator process failed with error $err"
      ~metadata:[("err", Error_json.error_to_yojson err)] ;
    Error.raise err
  in
  let%bind connection, process =
    Worker.spawn_in_foreground_exn ~on_failure ~shutdown_on:Disconnect
      ~connection_state_init_arg:()
      {constraint_constants; consensus_constants; conf_dir; logger}
  in
  [%log info]
    "Daemon started process of kind $process_kind with pid $vrf_evaluator_pid"
    ~metadata:
      [ ("vrf_evaluator_pid", `Int (Process.pid process |> Pid.to_int))
      ; ( "process_kind"
        , `String Child_processes.Termination.(show_process_kind Vrf_evaluator)
        ) ] ;
  Child_processes.Termination.register_process pids process
    Child_processes.Termination.Vrf_evaluator ;
  don't_wait_for
  @@ Pipe.iter
       (Process.stdout process |> Reader.pipe)
       ~f:(fun stdout ->
         return
         @@ [%log debug] "Vrf_evaluator stdout: $stdout"
              ~metadata:[("stdout", `String stdout)] ) ;
  don't_wait_for
  @@ Pipe.iter
       (Process.stderr process |> Reader.pipe)
       ~f:(fun stderr ->
         return
         @@ [%log error] "Vrf_evaluator stderr: $stderr"
              ~metadata:[("stderr", `String stderr)] ) ;
  let t = {connection; process} in
  let%map _ = update_block_producer_keys ~keypairs t in
  t

let set_new_epoch_state {connection; process= _} ~epoch_data_for_vrf =
  Worker.Connection.run connection ~f:Worker.functions.set_new_epoch_state
    ~arg:epoch_data_for_vrf

let slots_won_so_far {connection; process= _} =
  Worker.Connection.run connection ~f:Worker.functions.slots_won_so_far ~arg:()
