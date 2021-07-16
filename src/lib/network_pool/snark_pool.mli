open Async_kernel
open Pipe_lib
open Network_peer
open Core_kernel

module type S = sig
  type transition_frontier

  module Resource_pool : sig
    include
      Intf.Snark_resource_pool_intf
        with type transition_frontier := transition_frontier

    val remove_solved_work : t -> Transaction_snark_work.Statement.t -> unit

    module Diff : Intf.Snark_pool_diff_intf with type resource_pool := t
  end

  module For_tests : sig
    val get_rebroadcastable :
         Resource_pool.t
      -> has_timed_out:(Core.Time.t -> [ `Timed_out | `Ok ])
      -> Resource_pool.Diff.t list
  end

  include
    Intf.Network_pool_base_intf
      with type resource_pool := Resource_pool.t
       and type resource_pool_diff := Resource_pool.Diff.t
       and type resource_pool_diff_verified := Resource_pool.Diff.verified
       and type transition_frontier := transition_frontier
       and type config := Resource_pool.Config.t
       and type transition_frontier_diff :=
            Resource_pool.transition_frontier_diff
       and type rejected_diff := Resource_pool.Diff.rejected

  val get_completed_work :
       t
    -> Transaction_snark_work.Statement.t
    -> Transaction_snark_work.Checked.t option

  val load :
       config:Resource_pool.Config.t
    -> logger:Logger.t
    -> constraint_constants:Genesis_constants.Constraint_constants.t
    -> consensus_constants:Consensus.Constants.t
    -> time_controller:Block_time.Controller.t
    -> incoming_diffs:
         ( Resource_pool.Diff.t Envelope.Incoming.t
         * Mina_net2.Validation_callback.t )
         Strict_pipe.Reader.t
    -> local_diffs:
         ( Resource_pool.Diff.t
         * (   (Resource_pool.Diff.t * Resource_pool.Diff.rejected) Or_error.t
            -> unit) )
         Strict_pipe.Reader.t
    -> frontier_broadcast_pipe:
         transition_frontier option Broadcast_pipe.Reader.t
    -> t Deferred.t
end

module type Transition_frontier_intf = sig
  type t

  type staged_ledger

  module Breadcrumb : sig
    type t

    val staged_ledger : t -> staged_ledger
  end

  type best_tip_diff

  val best_tip : t -> Breadcrumb.t

  val best_tip_diff_pipe : t -> best_tip_diff Broadcast_pipe.Reader.t

  val snark_pool_refcount_pipe :
       t
    -> Transition_frontier.Extensions.Snark_pool_refcount.view
       Pipe_lib.Broadcast_pipe.Reader.t
end

module Make
    (Base_ledger : Intf.Base_ledger_intf) (Staged_ledger : sig
      type t

      val ledger : t -> Base_ledger.t
    end)
    (Transition_frontier : Transition_frontier_intf
                             with type staged_ledger := Staged_ledger.t) :
  S with type transition_frontier := Transition_frontier.t

include S with type transition_frontier := Transition_frontier.t

module Diff_versioned : sig
  [%%versioned:
  module Stable : sig
    module V2 : sig
      type t = Resource_pool.Diff.t =
        | Add_solved_work of
            Transaction_snark_work.Statement.Stable.V2.t
            * Ledger_proof.Stable.V2.t One_or_two.Stable.V1.t
              Priced_proof.Stable.V1.t
        | Empty
      [@@deriving compare, sexp, hash]
    end

    module V1 : sig
      type t [@@deriving compare, sexp, hash]

      val to_latest : t -> V2.t
    end
  end]
end
