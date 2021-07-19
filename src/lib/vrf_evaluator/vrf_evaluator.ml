open Core
open Async
open Signature_lib
module Epoch = Mina_numbers.Length
module Global_slot = Mina_numbers.Global_slot

(*Slot number within an epoch*)
module Slot = Mina_numbers.Global_slot

(* Can extract both (slot, epoch) and global slot number from Consensus_time*)
module Consensus_time = Consensus.Data.Consensus_time

module Keys_input = struct
  module Poly = struct
    [%%versioned
    module Stable = struct
      [@@@no_toplevel_latest_type]

      module V1 = struct
        type 'a t =
          { block_producer_keys:
              (Keypair.Stable.V1.t * Public_key.Compressed.Stable.V1.t) list
          ; delegatee_table: 'a
          ; time: Block_time.Stable.V1.t }
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
  end

  module Init = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = unit Poly.Stable.V1.t [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]

    let create block_producer_keys time : t =
      {block_producer_keys; delegatee_table= (); time}
  end

  module Update = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          Mina_base.Account.Stable.V1.t
          Mina_base.Account.Index.Stable.V1.Table.t
          Public_key.Compressed.Stable.V1.Table.t
          Poly.Stable.V1.t
        [@@deriving sexp]

        let to_latest = Fn.id
      end
    end]
  end
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

module Vrf_evaluation = struct
  module Input = struct
    [%%versioned
    module Stable = struct
      [@@@no_toplevel_latest_type]

      module V1 = struct
        type t = Block_time.Stable.V1.t

        let to_latest = Fn.id
      end
    end]

    type t = Stable.Latest.t
  end

  module Result = struct
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
end

module Slots_won = struct
  open Doubly_linked

  type t = Consensus.Data.Slot_won.t Doubly_linked.t [@@deriving sexp]

  module Slot_won = struct
    type t = Consensus.Data.Slot_won.t Elt.t

    let is_for_slot (t : t) ~slot =
      Global_slot.(slot = (Elt.value t).global_slot)

    let global_slot (t : t) = (Elt.value t).global_slot

    let block_producer (t : t) = (Elt.value t).producer

    let value = Elt.value
  end

  let check t =
    match first t with
    | None ->
        ()
    | Some first ->
        fold t ~init:first
          ~f:(fun (prev : Consensus.Data.Slot_won.t)
             (curr : Consensus.Data.Slot_won.t)
             ->
            if Global_slot.(curr.global_slot = first.global_slot) then curr
            else if Global_slot.(prev.global_slot < curr.global_slot) then curr
            else failwith "Slots won in an incorrect order" )
        |> ignore

  (*First list contains wins for slots < [at] and seconds list for slots >= [at]*)
  let split ~at t =
    let rec go before = function
      | [] ->
          (List.rev before, [])
      | (x : Consensus.Data.Slot_won.t) :: xs
        when Global_slot.(x.global_slot >= at) ->
          (List.rev before, x :: xs)
      | x :: xs ->
          go (x :: before) xs
    in
    go [] (to_list t)

  let all ~from t = snd (split ~at:from t)

  let delete_all_before ~slot t =
    filter_inplace t ~f:(fun (s : Consensus.Data.Slot_won.t) ->
        Global_slot.(s.global_slot >= slot) )

  [%%define_locally
  Doubly_linked.
    ( first_elt
    , insert_after
    , insert_before
    , insert_first
    , insert_last
    , remove
    , remove_first
    , remove_last
    , next
    , prev
    , clear
    , create )]
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
    ; slots_won: Consensus.Data.Slot_won.t Queue.t
    ; slots_won_staged: Slots_won.t
          (*To keep track of slots won from previous keys that may still be applicable, acts a working area so that stale slots are not sent to the parent process*)
    ; mutable current_slot: Global_slot.t option
    ; mutable epoch_data:
        unit Ivar.t * Consensus.Data.Epoch_data_for_vrf.t option
    ; mutable block_producer_keys: (Keypair.t * Public_key.Compressed.t) list
    ; last_checked_slot_and_epoch:
        (Epoch.t * Slot.t) Public_key.Compressed.Table.t }

  let update_last_checked_slot_and_epoch_table t new_keys ~now =
    let global_slot =
      Consensus.Data.Consensus_time.(
        of_time_exn now ~constants:t.config.consensus_constants)
    in
    let epoch, slot =
      Consensus.Data.Consensus_time.(epoch global_slot, slot global_slot)
    in
    let module Table = Public_key.Compressed.Table in
    List.iter new_keys ~f:(fun (_, pk) ->
        Table.change t.last_checked_slot_and_epoch pk ~f:(fun _ ->
            Some (epoch, slot) ) ) ;
    [%log' info t.config.logger]
      !"last checked epoch slots %{sexp: (Epoch.t * Slot.t) \
        Public_key.Compressed.Table.t}%!"
      t.last_checked_slot_and_epoch

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
             else (
               Table.set last_checked_slot_and_epoch ~key:pk ~data:(epoch, slot) ;
               Some pk ) )
    in
    match unseens with
    | [] ->
        `All_seen
    | nel ->
        `Unseen (Public_key.Compressed.Set.of_list nel)

  let validate_start_slot (t : t) start_time =
    match snd t.epoch_data with
    | None ->
        true
    | Some e ->
        let c1 =
          Global_slot.(
            Consensus_time.to_global_slot start_time >= e.global_slot)
        in
        let start_epoch = Consensus_time.epoch start_time in
        let c2 = Epoch.(start_epoch = e.epoch) in
        c1 && c2

  let evaluate
      ( { config
        ; slots_won
        ; slots_won_staged
        ; block_producer_keys
        ; last_checked_slot_and_epoch
        ; epoch_data= interrupt_ivar, epoch_data
        ; _ } as t ) ~consensus_time_now : (unit, unit) Interruptible.t =
    match epoch_data with
    | None ->
        Interruptible.return ()
    | Some epoch_data ->
        let open Interruptible.Let_syntax in
        let%bind () =
          Interruptible.lift Deferred.unit (Ivar.read interrupt_ivar)
        in
        let module Slot = Mina_numbers.Global_slot in
        let logger = config.logger in
        let epoch = epoch_data.epoch in
        [%log info] "Starting VRF evaluation for epoch: $epoch"
          ~metadata:[("epoch", Epoch.to_yojson epoch)] ;
        let keypairs = block_producer_keys in
        let logger = config.logger in
        let start_global_slot =
          Consensus_time.to_global_slot consensus_time_now
        in
        Slots_won.delete_all_before ~slot:start_global_slot slots_won_staged ;
        let slot_offset_since_genesis =
          match
            Global_slot.sub epoch_data.global_slot_since_genesis
              epoch_data.global_slot
          with
          | None ->
              failwith "Global slot > Global slot since genesis"
          | Some diff ->
              diff
        in
        let delegatee_table = epoch_data.delegatee_table in
        let total_stake = epoch_data.epoch_ledger.total_currency in
        let evaluate_vrf ~consensus_time unseen_pks =
          (* Try vrfs for all keypairs that are unseen within this slot until one wins or all lose *)
          (* TODO: Don't do this, and instead pick the one that has the highest chance of winning. See #2573 *)
          let slot = Consensus_time.slot consensus_time in
          let global_slot = Consensus_time.to_global_slot consensus_time in
          [%log info] "Checking VRF evaluations at epoch: $epoch, slot: $slot"
            ~metadata:
              [ ("epoch", `Int (Epoch.to_int epoch))
              ; ("slot", `Int (Slot.to_int slot)) ] ;
          let rec go = function
            | [] ->
                Interruptible.return None
            | ((keypair : Keypair.t), public_key_compressed) :: keypairs -> (
                if
                  not
                  @@ Public_key.Compressed.Set.mem unseen_pks
                       public_key_compressed
                then go keypairs
                else
                  let global_slot_since_genesis =
                    Global_slot.add global_slot slot_offset_since_genesis
                  in
                  match%bind
                    Consensus.Data.Vrf.check
                      ~constraint_constants:config.constraint_constants
                      ~global_slot ~seed:epoch_data.epoch_seed
                      ~get_delegators:
                        (Public_key.Compressed.Table.find delegatee_table)
                      ~producer_private_key:keypair.private_key
                      ~producer_public_key:public_key_compressed ~total_stake
                      ~logger
                  with
                  | None ->
                      go keypairs
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
                      Interruptible.return (Some slot_won) )
          in
          go keypairs
        in
        let curr_slot_won = ref (Slots_won.first_elt slots_won_staged) in
        let rec find_winning_slot (consensus_time : Consensus_time.t) =
          let slot = Consensus_time.slot consensus_time in
          let global_slot = Consensus_time.to_global_slot consensus_time in
          t.current_slot <- Some global_slot ;
          let epoch' = Consensus_time.epoch consensus_time in
          [%log info] "Slot in the epoch: $slot"
            ~metadata:[("slot", Slot.to_yojson slot)] ;
          let find f =
            let start = Time.now () in
            let%bind () =
              match%bind
                seen_slot last_checked_slot_and_epoch epoch' slot
                |> Interruptible.return
              with
              | `All_seen ->
                  [%log info] "Did not win a slot, took $time ms"
                    ~metadata:
                      [("time", `Float Time.(Span.to_ms (diff (now ()) start)))] ;
                  Interruptible.return ()
              | `Unseen pks -> (
                  match%map evaluate_vrf pks ~consensus_time with
                  | None ->
                      [%log info] "Did not win a slot, took $time ms"
                        ~metadata:
                          [ ( "time"
                            , `Float Time.(Span.to_ms (diff (now ()) start)) )
                          ]
                  | Some slot_won ->
                      [%log info] "Won a slot, took $time ms"
                        ~metadata:
                          [ ( "time"
                            , `Float Time.(Span.to_ms (diff (now ()) start)) )
                          ] ;
                      Queue.enqueue slots_won slot_won ;
                      f slot_won |> ignore )
            in
            find_winning_slot (Consensus_time.succ consensus_time)
          in
          if Epoch.(epoch' > epoch) then (
            t.current_slot <- None ;
            Interruptible.return () )
          else
            match !curr_slot_won with
            | None ->
                find (fun a -> Slots_won.insert_last slots_won_staged a)
            | Some computed_win ->
                if
                  Global_slot.(
                    global_slot = Slots_won.Slot_won.global_slot computed_win)
                then (
                  if
                    (*Slot already won; check if the block producer is still applicable*)
                    List.mem (List.map keypairs ~f:fst)
                      (Slots_won.Slot_won.block_producer computed_win)
                      ~equal:Signature_lib.Keypair.equal
                  then (
                    [%log info] "slot won (VRF already computed)" ;
                    curr_slot_won :=
                      Slots_won.next slots_won_staged computed_win ;
                    Queue.enqueue slots_won
                      (Slots_won.Slot_won.value computed_win) ;
                    find_winning_slot (Consensus_time.succ consensus_time) )
                  else
                    (*Not applicable. remove the winning slot and recompute*)
                    match Slots_won.next slots_won_staged computed_win with
                    | None ->
                        Slots_won.remove slots_won_staged computed_win ;
                        curr_slot_won := None ;
                        find (fun a ->
                            Slots_won.insert_last slots_won_staged a |> ignore
                        )
                    | Some next ->
                        Slots_won.remove slots_won_staged computed_win ;
                        curr_slot_won := Some next ;
                        find (fun a ->
                            Slots_won.insert_before slots_won_staged next a
                            |> ignore ) )
                else if
                  Global_slot.(
                    global_slot < Slots_won.Slot_won.global_slot computed_win)
                then
                  find (fun a ->
                      Slots_won.insert_before slots_won_staged computed_win a
                  )
                else
                  find (fun a ->
                      let new_elt =
                        Slots_won.insert_after slots_won_staged computed_win a
                      in
                      curr_slot_won := Some new_elt )
        in
        find_winning_slot consensus_time_now

  let create config =
    { config
    ; last_checked_slot_and_epoch= Public_key.Compressed.Table.create ()
    ; slots_won=
        Queue.create
          ~capacity:
            (Global_slot.to_int config.consensus_constants.slots_per_epoch)
          ()
    ; slots_won_staged= Slots_won.create ()
    ; current_slot= None
    ; epoch_data= (Ivar.create (), None)
    ; block_producer_keys= [] }
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
          let interrupt_ivar, _ = w.epoch_data in
          Ivar.fill_if_empty interrupt_ivar () ;
          Slots_won.clear w.slots_won_staged ;
          Queue.clear w.slots_won ;
          w.current_slot <- None ;
          w.epoch_data <- (Ivar.create (), Some e) ;
          Interruptible.don't_wait_for
            (Worker_state.evaluate w
               ~consensus_time_now:
                 (Consensus_time.of_global_slot e.global_slot
                    ~constants:w.config.consensus_constants)) ;
          Deferred.unit
        in
        match w.epoch_data with
        | _, None ->
            update ()
        | _, Some current ->
            if Epoch.(succ e.epoch > current.epoch) then update ()
            else (
              [%log info]
                "Received epoch data for current epoch $epoch. Skipping "
                ~metadata:[("epoch", Epoch.to_yojson e.epoch)] ;
              Deferred.unit ) )

  let slots_won_so_far =
    create Vrf_evaluation.Input.Stable.Latest.bin_t
      Vrf_evaluation.Result.Stable.Latest.bin_t (fun w _now ->
        let slots_won = Queue.to_list w.slots_won in
        [%log' info w.config.logger]
          !"Slots won: %{sexp: Consensus.Data.Slot_won.t list}"
          slots_won ;
        let evaluator_status =
          match w.current_slot with
          | Some slot ->
              Evaluator_status.At slot
          | None ->
              Completed
        in
        return Vrf_evaluation.Result.{slots_won; evaluator_status} )

  let update_block_producer_keys =
    create Keys_input.Update.Stable.Latest.bin_t Unit.bin_t
      (fun w {block_producer_keys; delegatee_table; time} ->
        let logger = w.config.logger in
        [%log info] "Updating block producer keys $keys"
          ~metadata:
            [ ( "keys"
              , `List
                  (List.map block_producer_keys ~f:(fun (_, pk) ->
                       Public_key.Compressed.to_yojson pk )) ) ] ;
        let update_keys () =
          w.block_producer_keys <- block_producer_keys ;
          Worker_state.update_last_checked_slot_and_epoch_table w
            block_producer_keys ~now:time
        in
        match snd w.epoch_data with
        | None ->
            return (update_keys ())
        | Some e ->
            let consensus_time_now =
              Consensus_time.of_time_exn time
                ~constants:w.config.consensus_constants
            in
            let epoch_now = Consensus_time.epoch consensus_time_now in
            let global_slot_now =
              Consensus_time.to_global_slot consensus_time_now
            in
            let interrupt_vrf_evaluation = Epoch.(epoch_now = e.epoch) in
            if interrupt_vrf_evaluation then (
              (*Update keys after stopping the evaluation*)
              Ivar.fill_if_empty (fst w.epoch_data) () ;
              w.current_slot <- Some global_slot_now ;
              Queue.clear w.slots_won ;
              w.epoch_data <- (Ivar.create (), Some {e with delegatee_table}) ;
              update_keys () ;
              Interruptible.don't_wait_for
                (Worker_state.evaluate w ~consensus_time_now) )
            else
              (*evaluation should have stopped by now, waiting for new epoch data*)
              update_keys () ;
            Deferred.unit )

  let set_block_producer_keys =
    create Keys_input.Init.Stable.Latest.bin_t Unit.bin_t (fun w k ->
        let logger = w.config.logger in
        [%log info] "Setting block producer keys" ;
        w.block_producer_keys <- k.block_producer_keys ;
        Worker_state.update_last_checked_slot_and_epoch_table w
          k.block_producer_keys ~now:k.time ;
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
          ( 'worker
          , Vrf_evaluation.Input.t
          , Vrf_evaluation.Result.t )
          Rpc_parallel.Function.t
      ; update_block_producer_keys:
          ('worker, Keys_input.Update.t, unit) Rpc_parallel.Function.t
      ; set_block_producer_keys:
          ('worker, Keys_input.Init.t, unit) Rpc_parallel.Function.t }

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
        ; update_block_producer_keys= f update_block_producer_keys
        ; set_block_producer_keys= f set_block_producer_keys }

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
        return (Worker_state.create init_arg)

      let init_connection_state ~connection:_ ~worker_state:_ = return
    end
  end

  include Rpc_parallel.Make (T)
end

type t = {connection: Worker.Connection.t; process: Process.t}

let set_block_producer_keys {connection; process= _} ~keypairs ~now =
  Worker.Connection.run connection ~f:Worker.functions.set_block_producer_keys
    ~arg:
      (Keys_input.Init.create
         (Keypair.And_compressed_pk.Set.to_list keypairs)
         now)

let update_block_producer_keys {connection; process= _} ~keypairs
    ~delegatee_table ~now =
  Worker.Connection.run connection
    ~f:Worker.functions.update_block_producer_keys
    ~arg:
      { block_producer_keys= Keypair.And_compressed_pk.Set.to_list keypairs
      ; delegatee_table
      ; time= now }

let create ~constraint_constants ~pids ~consensus_constants ~conf_dir ~logger
    ~keypairs ~now =
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
  let%map _ = set_block_producer_keys ~keypairs ~now t in
  t

let set_new_epoch_state {connection; process= _} ~epoch_data_for_vrf =
  Worker.Connection.run connection ~f:Worker.functions.set_new_epoch_state
    ~arg:epoch_data_for_vrf

let slots_won_so_far {connection; process= _} ~now =
  Worker.Connection.run connection ~f:Worker.functions.slots_won_so_far
    ~arg:now
