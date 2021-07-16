open Core
open Snark_params.Tick

[%%versioned:
module Stable : sig
  module V1 : sig
    type t =
      ( Ledger_hash.Stable.V1.t
      , Account_id.Stable.V1.t
      , Account.Stable.V1.t
      , Token_id.Stable.V1.t )
      Sparse_ledger_lib.Sparse_ledger.T.Stable.V1.t
    [@@deriving sexp, to_yojson]
  end
end]

val merkle_root : t -> Ledger_hash.t

val depth : t -> int

val next_available_token : t -> Token_id.t

val get_exn : t -> int -> Account.t

val path_exn :
  t -> int -> [ `Left of Ledger_hash.t | `Right of Ledger_hash.t ] list

val find_index_exn : t -> Account_id.t -> int

val of_root : depth:int -> next_available_token:Token_id.t -> Ledger_hash.t -> t

val apply_user_command_exn :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> txn_global_slot:Mina_numbers.Global_slot.t
  -> t
  -> Signed_command.t
  -> t

val apply_transaction_exn :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> txn_state_view:Snapp_predicate.Protocol_state.View.t
  -> t
  -> Transaction.t
  -> t

val of_any_ledger : Ledger.Any_ledger.M.t -> t

val of_ledger_subset_exn : Ledger.t -> Account_id.t list -> t

val of_ledger_index_subset_exn : Ledger.Any_ledger.witness -> int list -> t

val iteri : t -> f:(Account.Index.t -> Account.t -> unit) -> unit

val handler : t -> Handler.t Staged.t

val has_locked_tokens_exn :
  global_slot:Mina_numbers.Global_slot.t -> account_id:Account_id.t -> t -> bool
