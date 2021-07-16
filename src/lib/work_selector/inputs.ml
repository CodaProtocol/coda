open Core_kernel
open Currency

module Test_inputs = struct
  module Transaction_witness = Int
  module Ledger_hash = Int
  module Sparse_ledger = Int
  module Transaction = Int
  module Ledger_proof_statement = Fee

  module Transaction_protocol_state = struct
    type 'a t = 'a
  end

  module Ledger_proof = struct
    type t = Fee.t [@@deriving hash, compare, sexp]
  end

  module Transaction_snark_work = struct
    type t = Fee.t

    let fee = Fn.id

    module Statement = struct
      type t = Transaction_snark.Statement.t One_or_two.t
    end
  end

  module Snark_pool = struct
    [%%versioned
    module Stable = struct
      [@@@no_toplevel_latest_type]

      module V2 = struct
        type t = Transaction_snark.Statement.Stable.V2.t One_or_two.Stable.V1.t
        [@@deriving hash, compare, sexp]

        let to_latest = Fn.id
      end

      module V1 = struct
        type t = Transaction_snark.Statement.Stable.V1.t One_or_two.Stable.V1.t
        [@@deriving hash, compare, sexp]

        let to_latest : t -> V2.t =
          One_or_two.map ~f:Transaction_snark.Statement.Stable.V1.to_latest
      end
    end]

    module Work = Hashable.Make_binable (Stable.Latest)

    type t = Currency.Fee.t Work.Table.t

    let get_completed_work (t : t) = Work.Table.find t

    let create () = Work.Table.create ()

    let add_snark t ~work ~fee =
      Work.Table.update t work ~f:(function
        | None ->
            fee
        | Some fee' ->
            Currency.Fee.min fee fee')
  end

  module Staged_ledger = struct
    type t =
      (int, Transaction_snark_work.t) Snark_work_lib.Work.Single.Spec.t List.t

    let work = Fn.id

    let all_work_pairs t ~get_state:_ = Ok (One_or_two.group_list t)
  end

  module Transition_frontier = struct
    type t = Staged_ledger.t

    type best_tip_view = unit

    let best_tip_pipe : t -> best_tip_view Pipe_lib.Broadcast_pipe.Reader.t =
     fun _t ->
      let reader, _writer = Pipe_lib.Broadcast_pipe.create () in
      reader

    let best_tip_staged_ledger = Fn.id

    let get_protocol_state _t _hash =
      Ok
        (Lazy.force Precomputed_values.for_unit_tests).protocol_state_with_hash
          .data
  end
end

module Implementation_inputs = struct
  open Mina_base
  module Ledger_hash = Ledger_hash
  module Sparse_ledger = Sparse_ledger
  module Transaction = Transaction
  module Transaction_witness = Transaction_witness
  module Ledger_proof = Ledger_proof
  module Transaction_snark_work = Transaction_snark_work
  module Snark_pool = Network_pool.Snark_pool
  module Staged_ledger = Staged_ledger
  module Transaction_protocol_state = Transaction_protocol_state

  module Transition_frontier = struct
    type t = Transition_frontier.t

    type best_tip_view = Extensions.Best_tip_diff.view

    let best_tip_pipe : t -> best_tip_view Pipe_lib.Broadcast_pipe.Reader.t =
     fun t ->
      let open Transition_frontier.Extensions in
      let extensions = Transition_frontier.extensions t in
      get_view_pipe extensions Best_tip_diff

    let best_tip_staged_ledger t =
      Transition_frontier.(best_tip t |> Breadcrumb.staged_ledger)

    let get_protocol_state t state_hash =
      match Transition_frontier.find_protocol_state t state_hash with
      | Some p ->
          Ok p
      | None ->
          Or_error.errorf
            !"Protocol state with hash %{sexp: State_hash.t} not found"
            state_hash
  end
end
