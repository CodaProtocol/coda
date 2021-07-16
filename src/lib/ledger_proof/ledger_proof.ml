open Core_kernel
open Mina_base

module type S = Ledger_proof_intf.S

module Prod : Ledger_proof_intf.S with type t = Transaction_snark.t = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t = Transaction_snark.Stable.V2.t
      [@@deriving compare, equal, sexp, yojson, hash]

      let to_latest = Fn.id
    end

    module V1 = struct
      type t = Transaction_snark.Stable.V1.t
      [@@deriving compare, equal, sexp, yojson, hash]

      let to_latest = Transaction_snark.Stable.V1.to_latest
    end
  end]

  let statement (t : t) = Transaction_snark.statement t

  let sok_digest = Transaction_snark.sok_digest

  let statement_target (t : Transaction_snark.Statement.t) = t.target

  let underlying_proof = Transaction_snark.proof

  let create ~statement ~sok_digest ~proof =
    Transaction_snark.create ~statement:{ statement with sok_digest } ~proof
end

include Prod

module For_tests = struct
  let mk_dummy_proof statement =
    create ~statement ~sok_digest:Sok_message.Digest.default
      ~proof:Proof.transaction_dummy
end
