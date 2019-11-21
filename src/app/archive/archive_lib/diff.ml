open Coda_transition
open Signature_lib
open Core_kernel
open Coda_base
module Breadcrumb = Transition_frontier.Breadcrumb

(* TODO: We should be able to fully deserialize and serialize via json *)
module Transition_frontier = struct
  open Transition_frontier.Diff

  module Stable = struct
    module V1 = struct
      module T = struct
        type t =
          | Breadcrumb_added of
              { block:
                  ( External_transition.Stable.V1.t
                  , State_hash.Stable.V1.t )
                  With_hash.Stable.V1.t
              ; sender_receipt_chains_from_parent_ledger:
                  ( Public_key.Compressed.Stable.V1.t
                  * Receipt.Chain_hash.Stable.V1.t )
                  list }
          | Root_transitioned of Root_transition.Lite.Stable.V1.t
          | Bootstrap of {lost_blocks: State_hash.Stable.V1.t list}
        [@@deriving bin_io, version]
      end

      include T
    end

    module Latest = V1
  end

  type t = Stable.Latest.t =
    | Breadcrumb_added of
        { block:
            ( External_transition.Stable.V1.t
            , State_hash.Stable.V1.t )
            With_hash.Stable.V1.t
        ; sender_receipt_chains_from_parent_ledger:
            (Public_key.Compressed.Stable.V1.t * Receipt.Chain_hash.Stable.V1.t)
            list }
    | Root_transitioned of Root_transition.Lite.Stable.V1.t
    | Bootstrap of {lost_blocks: State_hash.Stable.V1.t list}
end

module Transaction_pool = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type t =
          { added: (User_command.Stable.V1.t * Block_time.Stable.V1.t) list
          ; removed: User_command.Stable.V1.t list }
        [@@deriving bin_io, version]
      end

      include T
    end

    module Latest = V1
  end

  type t = Stable.Latest.t =
    { added: (User_command.Stable.V1.t * Block_time.Stable.V1.t) list
    ; removed: User_command.Stable.V1.t list }
end

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
        | Transition_frontier of Transition_frontier.Stable.V1.t
        | Transaction_pool of Transaction_pool.Stable.V1.t
      [@@deriving bin_io, version]
    end

    include T
  end

  module Latest = V1
end

type t = Stable.Latest.t =
  | Transition_frontier of Transition_frontier.Stable.V1.t
  | Transaction_pool of Transaction_pool.Stable.V1.t

module Builder = struct
  let breadcrumb_added breadcrumb =
    let ((block, _) as validated_block) =
      Breadcrumb.validated_transition breadcrumb
    in
    let user_commands =
      External_transition.Validated.user_commands validated_block
    in
    let sender_receipt_chains_from_parent_ledger =
      let user_commands = User_command.Set.of_list user_commands in
      let senders =
        Public_key.Compressed.Set.map user_commands ~f:User_command.sender
      in
      let ledger =
        Staged_ledger.ledger @@ Breadcrumb.staged_ledger breadcrumb
      in
      Set.to_list senders
      |> List.map ~f:(fun sender ->
             Option.value_exn
               (let open Option.Let_syntax in
               let%bind ledger_location =
                 Ledger.location_of_key ledger sender
               in
               let%map {receipt_chain_hash; _} =
                 Ledger.get ledger ledger_location
               in
               (sender, receipt_chain_hash)) )
    in
    Transition_frontier.Breadcrumb_added
      {block; sender_receipt_chains_from_parent_ledger}

  let user_commands user_commands =
    Transaction_pool {Transaction_pool.added= user_commands; removed= []}
end
