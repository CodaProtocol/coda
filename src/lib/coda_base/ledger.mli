open Core
open Signature_lib

module Location : Merkle_ledger.Location_intf.S

module Db :
  Merkle_ledger.Database_intf.S
  with module Location = Location
  with module Addr = Location.Addr
  with type root_hash := Ledger_hash.t
   and type hash := Ledger_hash.t
   and type account := Account.t
   and type key := Public_key.Compressed.t
   and type token_id := Token_id.t
   and type token_id_set := Token_id.Set.t
   and type account_id := Account_id.t
   and type account_id_set := Account_id.Set.t

module Any_ledger :
  Merkle_ledger.Any_ledger.S
  with module Location = Location
  with type account := Account.t
   and type key := Public_key.Compressed.t
   and type token_id := Token_id.t
   and type token_id_set := Token_id.Set.t
   and type account_id := Account_id.t
   and type account_id_set := Account_id.Set.t
   and type hash := Ledger_hash.t

module Mask :
  Merkle_mask.Masking_merkle_tree_intf.S
  with module Location = Location
   and module Attached.Addr = Location.Addr
  with type account := Account.t
   and type key := Public_key.Compressed.t
   and type token_id := Token_id.t
   and type token_id_set := Token_id.Set.t
   and type account_id := Account_id.t
   and type account_id_set := Account_id.Set.t
   and type hash := Ledger_hash.t
   and type location := Location.t
   and type parent := Any_ledger.M.t

module Maskable :
  Merkle_mask.Maskable_merkle_tree_intf.S
  with module Location = Location
  with module Addr = Location.Addr
  with type account := Account.t
   and type key := Public_key.Compressed.t
   and type token_id := Token_id.t
   and type token_id_set := Token_id.Set.t
   and type account_id := Account_id.t
   and type account_id_set := Account_id.Set.t
   and type hash := Ledger_hash.t
   and type root_hash := Ledger_hash.t
   and type unattached_mask := Mask.t
   and type attached_mask := Mask.Attached.t
   and type t := Any_ledger.M.t

include
  Merkle_mask.Maskable_merkle_tree_intf.S
  with module Location := Location
  with module Addr = Location.Addr
  with type root_hash := Ledger_hash.t
   and type hash := Ledger_hash.t
   and type account := Account.t
   and type key := Public_key.Compressed.t
   and type token_id := Token_id.t
   and type token_id_set := Token_id.Set.t
   and type account_id := Account_id.t
   and type account_id_set := Account_id.Set.t
   and type t = Mask.Attached.t
   and type attached_mask = Mask.Attached.t
   and type unattached_mask = Mask.t

(* We override the type of unregister_mask_exn that comes from
   Merkle_mask.Maskable_merkle_tree_intf.S because at this level callers aren't
   doing reparenting and shouldn't be able to turn off the check parameter.
*)
val unregister_mask_exn : Mask.Attached.t -> Mask.t

(* The maskable ledger is t = Mask.Attached.t because register/unregister
 * work off of this type *)
type maskable_ledger = t

val with_ledger : depth:int -> f:(t -> 'a) -> 'a

val with_ephemeral_ledger : depth:int -> f:(t -> 'a) -> 'a

val create : ?directory_name:string -> depth:int -> unit -> t

val create_ephemeral : depth:int -> unit -> t

val of_database : Db.t -> t

(** This is not _really_ copy, merely a stop-gap until we remove usages of copy in our codebase. What this actually does is creates a new empty mask on top of the current ledger *)
val copy : t -> t

val register_mask : t -> Mask.t -> Mask.Attached.t

val commit : Mask.Attached.t -> unit

module Undo : sig
  open Transaction_logic

  module User_command_undo : sig
    module Common : sig
      type t = Undo.User_command_undo.Common.t =
        { user_command: User_command.t
        ; previous_receipt_chain_hash: Receipt.Chain_hash.t
        ; fee_payer_timing: Account.Timing.t
        ; source_timing: Account.Timing.t option }
      [@@deriving sexp]
    end

    module Body : sig
      type t = Undo.User_command_undo.Body.t =
        | Payment of {previous_empty_accounts: Account_id.t list}
        | Stake_delegation of {previous_delegate: Public_key.Compressed.t}
        | Create_new_token of {created_token: Token_id.t}
        | Create_token_account
        | Mint_tokens
        | Set_token_permissions of {previous_token_locked: bool}
        | Failed
      [@@deriving sexp]
    end

    type t = Undo.User_command_undo.t = {common: Common.t; body: Body.t}
    [@@deriving sexp]
  end

  module Fee_transfer_undo : sig
    type t = Undo.Fee_transfer_undo.t =
      {fee_transfer: Fee_transfer.t; previous_empty_accounts: Account_id.t list}
    [@@deriving sexp]
  end

  module Coinbase_undo : sig
    type t = Undo.Coinbase_undo.t =
      {coinbase: Coinbase.t; previous_empty_accounts: Account_id.t list}
    [@@deriving sexp]
  end

  module Varying : sig
    type t = Undo.Varying.t =
      | User_command of User_command_undo.t
      | Fee_transfer of Fee_transfer_undo.t
      | Coinbase of Coinbase_undo.t
    [@@deriving sexp]
  end

  type t = Undo.t = {previous_hash: Ledger_hash.t; varying: Varying.t}
  [@@deriving sexp]

  val transaction : t -> Transaction.t Or_error.t
end

val create_new_account_exn : t -> Account_id.t -> Account.t -> unit

val apply_user_command :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> txn_global_slot:Coda_numbers.Global_slot.t
  -> t
  -> User_command.With_valid_signature.t
  -> Undo.User_command_undo.t Or_error.t

val apply_transaction :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> txn_global_slot:Coda_numbers.Global_slot.t
  -> t
  -> Transaction.t
  -> Undo.t Or_error.t

val undo :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> t
  -> Undo.t
  -> unit Or_error.t

val merkle_root_after_user_command_exn :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> txn_global_slot:Coda_numbers.Global_slot.t
  -> t
  -> User_command.With_valid_signature.t
  -> Ledger_hash.t * [`Next_available_token of Token_id.t]

val create_empty : t -> Account_id.t -> Path.t * Account.t

val num_accounts : t -> int

(** Generate an initial ledger state. There can't be a regular Quickcheck
    generator for this type because you need to detach a mask from it's parent
    when you're done with it - the GC doesn't take care of that. *)
val gen_initial_ledger_state :
  (Signature_lib.Keypair.t * Currency.Amount.t * Coda_numbers.Account_nonce.t)
  array
  Quickcheck.Generator.t

type init_state =
  (Signature_lib.Keypair.t * Currency.Amount.t * Coda_numbers.Account_nonce.t)
  array
[@@deriving sexp_of]

(** Apply a generated state to a blank, concrete ledger. *)
val apply_initial_ledger_state : t -> init_state -> unit
