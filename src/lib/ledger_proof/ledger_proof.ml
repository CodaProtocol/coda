open Core_kernel
open Mina_base

module type S = Ledger_proof_intf.S

module Prod : Ledger_proof_intf.S with type t = Transaction_snark.t = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Transaction_snark.Stable.V1.t
      [@@deriving compare, sexp, yojson, hash]

      let to_latest = Fn.id

      let of_latest t = Ok t
    end
  end]

  let statement (t : t) = Transaction_snark.statement t

  let sok_digest = Transaction_snark.sok_digest

  let statement_target (t : Transaction_snark.Statement.t) = t.target

  let underlying_proof = Transaction_snark.proof

  let create
      ~statement:{ Transaction_snark.Statement.source
                 ; target
                 ; supply_increase
                 ; fee_excess
                 ; next_available_token_before
                 ; next_available_token_after
                 ; pending_coinbase_stack_state
                 ; sok_digest= () } ~sok_digest ~proof =
    Transaction_snark.create ~source ~target ~pending_coinbase_stack_state
      ~supply_increase ~fee_excess ~next_available_token_before
      ~next_available_token_after ~sok_digest ~proof
end

include Prod

module For_tests = struct
  let mk_dummy_proof statement =
    create ~statement ~sok_digest:Sok_message.Digest.default
      ~proof:Proof.transaction_dummy
end
