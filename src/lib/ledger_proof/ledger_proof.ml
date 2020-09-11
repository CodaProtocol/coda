[%%import
"../../config.mlh"]

open Core_kernel
open Coda_base

module type S = Ledger_proof_intf.S

module Prod : Ledger_proof_intf.S with type t = Transaction_snark.t = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Transaction_snark.Stable.V1.t
      [@@deriving compare, sexp, to_yojson]

      let to_latest = Fn.id

      let of_latest t = Ok t
    end
  end]

  type t = Stable.Latest.t [@@deriving compare, sexp, to_yojson]

  let id : t Type_equal.Id.t =
    Type_equal.Id.create ~name:"ledger-proof-prod" sexp_of_t

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

module Debug :
  Ledger_proof_intf.S
  with type t = Transaction_snark.Statement.t * Sok_message.Digest.Stable.V1.t =
struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        Transaction_snark.Statement.Stable.V1.t
        * Sok_message.Digest.Stable.V1.t
      [@@deriving compare, hash, sexp, yojson]

      let to_latest = Fn.id

      let of_latest t = Ok t
    end
  end]

  type t = Stable.Latest.t [@@deriving compare, sexp, yojson]

  let id : t Type_equal.Id.t =
    Type_equal.Id.create ~name:"ledger-proof-debug" sexp_of_t

  let statement ((t, _) : t) : Transaction_snark.Statement.t = t

  let underlying_proof (_ : t) = Proof.transaction_dummy

  let statement_target (t : Transaction_snark.Statement.t) = t.target

  let sok_digest (_, d) = d

  let create ~statement ~sok_digest ~proof:_ = (statement, sok_digest)
end

[%%if
proof_level = "full"]

include Prod

[%%else]

(* TODO #1698: proof_level=check *)

include Debug

[%%endif]

type _ type_witness =
  | Debug : Debug.t type_witness
  | Prod : Prod.t type_witness

type with_witness = With_witness : 't * 't type_witness -> with_witness

module For_tests = struct
  let mk_dummy_proof statement =
    create ~statement ~sok_digest:Sok_message.Digest.default
      ~proof:Proof.transaction_dummy
end
