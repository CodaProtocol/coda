open Core_kernel
open Async_kernel
open Protocols

module type Ledger_builder_io_intf = sig
  type t

  type net

  type ledger_builder_hash

  type ledger_hash

  type ledger_builder_aux

  type sync_ledger_query

  type sync_ledger_answer

  type state

  val create : net -> t

  val get_ledger_builder_aux_at_hash :
    t -> ledger_builder_hash -> ledger_builder_aux Deferred.Or_error.t

  val glue_sync_ledger :
       t
    -> (ledger_hash * sync_ledger_query) Linear_pipe.Reader.t
    -> (ledger_hash * sync_ledger_answer) Linear_pipe.Writer.t
    -> unit
end

module type Network_intf = sig
  type t

  type state_with_witness

  type ledger_builder

  type state

  type ledger_hash

  type ledger_builder_hash

  type parallel_scan_state

  type sync_ledger_query

  type sync_ledger_answer

  type snark_pool_diff

  type transaction_pool_diff

  val states : t -> state_with_witness Linear_pipe.Reader.t

  val snark_pool_diffs : t -> snark_pool_diff Linear_pipe.Reader.t

  val transaction_pool_diffs : t -> transaction_pool_diff Linear_pipe.Reader.t

  val broadcast_state : t -> state_with_witness -> unit

  val broadcast_snark_pool_diff : t -> snark_pool_diff -> unit

  val broadcast_transaction_pool_diff : t -> transaction_pool_diff -> unit

  module Ledger_builder_io :
    Ledger_builder_io_intf
    with type net := t
     and type ledger_builder_aux := parallel_scan_state
     and type ledger_builder_hash := ledger_builder_hash
     and type ledger_hash := ledger_hash
     and type state := state
     and type sync_ledger_query := sync_ledger_query
     and type sync_ledger_answer := sync_ledger_answer

  module Config : sig
    type t
  end

  val create :
       Config.t
    -> get_ledger_builder_aux_at_hash:(   ledger_builder_hash
                                       -> (parallel_scan_state * ledger_hash)
                                          option
                                          Deferred.t)
    -> answer_sync_ledger_query:(   ledger_hash * sync_ledger_query
                                 -> (ledger_hash * sync_ledger_answer)
                                    Deferred.t)
    -> t Deferred.t
end

module type Transaction_pool_intf = sig
  type t

  type pool_diff

  type transaction_with_valid_signature

  type transaction

  val transactions : t -> transaction_with_valid_signature Sequence.t

  val broadcasts : t -> pool_diff Linear_pipe.Reader.t

  val load :
       disk_location:string
    -> incoming_diffs:pool_diff Linear_pipe.Reader.t
    -> t Deferred.t

  val add : t -> transaction -> unit Deferred.t
end

module type Snark_pool_intf = sig
  type t

  type completed_work_statement

  type completed_work_checked

  type pool_diff

  val broadcasts : t -> pool_diff Linear_pipe.Reader.t

  val load :
       disk_location:string
    -> incoming_diffs:pool_diff Linear_pipe.Reader.t
    -> t Deferred.t

  val get_completed_work :
    t -> completed_work_statement -> completed_work_checked option
end

module type Ledger_builder_controller_intf = sig
  type ledger_builder

  type ledger_builder_hash

  type internal_transition

  type external_transition

  type ledger

  type net

  type state

  type t

  type sync_query

  type sync_answer

  type ledger_proof

  type ledger_hash

  module Config : sig
    type t =
      { parent_log: Logger.t
      ; net_deferred: net Deferred.t
      ; external_transitions: external_transition Linear_pipe.Reader.t
      ; genesis_ledger: ledger
      ; disk_location: string }
    [@@deriving make]
  end

  module Aux : sig
    type t = {root_and_proof: (ledger_hash * ledger_proof) option; state: state}
  end

  val create : Config.t -> t Deferred.t

  val local_get_ledger :
    t -> ledger_builder_hash -> (ledger_builder * state) Deferred.Or_error.t

  val strongest_ledgers :
    t -> (ledger_builder * external_transition) Linear_pipe.Reader.t

  val handle_sync_ledger_queries :
    ledger_hash * sync_query -> ledger_hash * sync_answer
end

module type Miner_intf = sig
  type t

  type ledger_hash

  type ledger_builder

  type transaction

  type external_transition

  type completed_work_statement

  type completed_work_checked

  type state

  type state_proof

  module Tip : sig
    type t =
      { state: state * state_proof
      ; ledger_builder: ledger_builder
      ; transactions: transaction Sequence.t }
  end

  type change = Tip_change of Tip.t

  val create :
       parent_log:Logger.t
    -> get_completed_work:(   completed_work_statement
                           -> completed_work_checked option)
    -> change_feeder:change Linear_pipe.Reader.t
    -> t

  val transitions : t -> external_transition Linear_pipe.Reader.t
end

module type Witness_change_intf = sig
  type t_with_witness

  type witness

  type t

  val forget_witness : t_with_witness -> t

  val add_witness_exn : t -> witness -> t_with_witness

  val add_witness : t -> witness -> t_with_witness Or_error.t
end

module type State_with_witness_intf = sig
  type state

  type ledger_hash

  type ledger_builder_transition

  type ledger_builder_transition_with_valid_signatures_and_proofs

  type t =
    { ledger_builder_transition:
        ledger_builder_transition_with_valid_signatures_and_proofs
    ; state: state }
  [@@deriving sexp]

  module Stripped : sig
    type t =
      {ledger_builder_transition: ledger_builder_transition; state: state}
    [@@deriving bin_io]
  end

  val strip : t -> Stripped.t

  val forget_witness : t -> state
end

module type Inputs_intf = sig
  include Coda_pow.Inputs_intf

  module Proof_carrying_state : sig
    type t = (State.t, State.Proof.t) Coda_pow.Proof_carrying_data.t
    [@@deriving sexp, bin_io]
  end

  module State_with_witness :
    State_with_witness_intf
    with type state := Proof_carrying_state.t
     and type ledger_hash := Ledger_hash.t
     and type ledger_builder_transition := Ledger_builder_transition.t
     and type ledger_builder_transition_with_valid_signatures_and_proofs :=
                Ledger_builder_transition.With_valid_signatures_and_proofs.t

  module Snark_pool :
    Snark_pool_intf
    with type completed_work_statement := Completed_work.Statement.t
     and type completed_work_checked := Completed_work.Checked.t

  module Transaction_pool :
    Transaction_pool_intf
    with type transaction_with_valid_signature :=
                Transaction.With_valid_signature.t
     and type transaction := Transaction.t

  module Sync_ledger : sig
    type query [@@deriving bin_io]

    type answer [@@deriving bin_io]
  end

  module Net :
    Network_intf
    with type state_with_witness := External_transition.t
     and type ledger_builder := Ledger_builder.t
     and type ledger_builder_hash := Ledger_builder_hash.t
     and type state := State.t
     and type snark_pool_diff := Snark_pool.pool_diff
     and type transaction_pool_diff := Transaction_pool.pool_diff
     and type parallel_scan_state := Ledger_builder.Aux.t
     and type ledger_hash := Ledger_hash.t
     and type sync_ledger_query := Sync_ledger.query
     and type sync_ledger_answer := Sync_ledger.answer

  module Ledger_builder_controller :
    Ledger_builder_controller_intf
    with type net := Net.t
     and type ledger := Ledger.t
     and type ledger_builder := Ledger_builder.t
     and type ledger_builder_hash := Ledger_builder_hash.t
     and type internal_transition := Internal_transition.t
     and type external_transition := External_transition.t
     and type state := State.t
     and type sync_query := Sync_ledger.query
     and type sync_answer := Sync_ledger.answer
     and type ledger_hash := Ledger_hash.t
     and type ledger_proof := Ledger_proof.t

  module Miner :
    Miner_intf
    with type ledger_hash := Ledger_hash.t
     and type ledger_builder := Ledger_builder.t
     and type transaction := Transaction.With_valid_signature.t
     and type state := State.t
     and type state_proof := State.Proof.t
     and type completed_work_statement := Completed_work.Statement.t
     and type completed_work_checked := Completed_work.Checked.t
     and type external_transition := External_transition.t

  module Genesis : sig
    val state : State.t

    val ledger : Ledger.t

    val proof : State.Proof.t
  end
end

module Make (Inputs : Inputs_intf) = struct
  open Inputs

  type t =
    { miner: Miner.t
    ; net: Net.t
    ; external_transitions:
        External_transition.t Linear_pipe.Writer.t
        (* TODO: Is this the best spot for the transaction_pool ref? *)
    ; transaction_pool: Transaction_pool.t
    ; snark_pool: Snark_pool.t
    ; ledger_builder: Ledger_builder_controller.t
    ; best_lb: Ledger_builder.t option ref
    ; log: Logger.t
    ; ledger_builder_transition_backup_capacity: int }

  let best_ledger_builder t = !(t.best_lb)
  let best_ledger t = Option.map (best_ledger_builder t) ~f:Ledger_builder.ledger

  let transaction_pool t = t.transaction_pool

  let snark_pool t = t.snark_pool

  module Config = struct
    type t =
      { log: Logger.t
      ; net_config: Net.Config.t
      ; ledger_builder_persistant_location: string
      ; transaction_pool_disk_location: string
      ; snark_pool_disk_location: string
      ; ledger_builder_transition_backup_capacity: int [@default 10] }
    [@@deriving make]
  end

  let create (config: Config.t) =
    let external_transitions_reader, external_transitions_writer =
      Linear_pipe.create ()
    in
    let net_ivar = Ivar.create () in
    let%bind ledger_builder =
      Ledger_builder_controller.create
        (Ledger_builder_controller.Config.make ~parent_log:config.log
           ~net_deferred:(Ivar.read net_ivar) ~genesis_ledger:Genesis.ledger
           ~disk_location:config.ledger_builder_persistant_location
           ~external_transitions:external_transitions_reader)
    in
    let%bind net =
      Net.create config.net_config
        ~get_ledger_builder_aux_at_hash:(fun hash ->
          (* TODO: Just make lbc do this *)
          match%map
            Ledger_builder_controller.local_get_ledger ledger_builder hash
          with
          | Ok (lb, state) ->
              Some
                ( Ledger_builder.aux lb
                , Ledger.merkle_root (Ledger_builder.ledger lb) )
          | _ -> None )
        ~answer_sync_ledger_query:(fun query ->
          return (Ledger_builder_controller.handle_sync_ledger_queries query)
          )
    in
    Ivar.fill net_ivar net ;
    don't_wait_for
      (Linear_pipe.transfer_id (Net.states net) external_transitions_writer) ;
    let%bind transaction_pool =
      Transaction_pool.load
        ~disk_location:config.transaction_pool_disk_location
        ~incoming_diffs:(Net.transaction_pool_diffs net)
    in
    don't_wait_for
      (Linear_pipe.iter (Transaction_pool.broadcasts transaction_pool) ~f:
         (fun x ->
           Net.broadcast_transaction_pool_diff net x ;
           Deferred.unit )) ;
    let%bind snark_pool =
      Snark_pool.load ~disk_location:config.snark_pool_disk_location
        ~incoming_diffs:(Net.snark_pool_diffs net)
    in
    don't_wait_for
      (Linear_pipe.iter (Snark_pool.broadcasts snark_pool) ~f:(fun x ->
           Net.broadcast_snark_pool_diff net x ;
           Deferred.unit )) ;
    let strongest_ledgers_for_miner, strongest_ledgers_for_network =
      Linear_pipe.fork2
        (Ledger_builder_controller.strongest_ledgers ledger_builder)
    in
    let best_lb : Ledger_builder.t option ref = ref None in
    Linear_pipe.iter strongest_ledgers_for_network ~f:(fun (lb, t) ->
        (* TODO: Don't just hack this here *)
        best_lb := Some lb ;
        Net.broadcast_state net t ;
        Deferred.unit )
    |> don't_wait_for ;
    let miner =
      Miner.create ~parent_log:config.log
        ~change_feeder:
          (Linear_pipe.map strongest_ledgers_for_miner ~f:
             (fun (ledger_builder, {state; state_proof; _}) ->
               Miner.Tip_change
                 { state= (state, state_proof)
                 ; ledger_builder
                 ; transactions= Transaction_pool.transactions transaction_pool
                 } ))
        ~get_completed_work:(Snark_pool.get_completed_work snark_pool)
    in
    don't_wait_for
      (Linear_pipe.transfer_id (Miner.transitions miner)
         external_transitions_writer) ;
    return
      { miner
      ; net
      ; best_lb
      ; external_transitions= external_transitions_writer
      ; transaction_pool
      ; snark_pool
      ; ledger_builder
      ; log= config.log
      ; ledger_builder_transition_backup_capacity=
          config.ledger_builder_transition_backup_capacity }

  let forget_diff_validity
      { Ledger_builder_diff.With_valid_signatures_and_proofs.prev_hash
      ; completed_works
      ; transactions
      ; creator } =
    { Ledger_builder_diff.prev_hash
    ; completed_works= List.map completed_works ~f:Completed_work.forget
    ; transactions= (transactions :> Transaction.t list)
    ; creator }

  let forget_transition_validity
      {Ledger_builder_transition.With_valid_signatures_and_proofs.old; diff} =
    {Ledger_builder_transition.old; diff= forget_diff_validity diff}
end
