(** The data structure underlying the transaction pool. We want to efficently
    support all the operations necessary. We also need to make sure that an
    attacker can't craft transactions or sequences thereof that take up an
    unacceptable amount of resources leading to a DoS.
*)
open Core

open Mina_base
open Mina_numbers

module Command_error : sig
  type t =
    | Invalid_nonce of
        [ `Expected of Account.Nonce.t
        | `Between of Account.Nonce.t * Account.Nonce.t ]
        * Account.Nonce.t
    | Insufficient_funds of
        [ `Balance of Currency.Amount.t ] * Currency.Amount.t
    | (* NOTE: don't punish for this, attackers can induce nodes to banlist
          each other that way! *)
        Insufficient_replace_fee of
        [ `Replace_fee of Currency.Fee.t ] * Currency.Fee.t
    | Overflow
    | Bad_token
    | Expired of
        [ `Valid_until of Mina_numbers.Global_slot.t ]
        * [ `Current_global_slot of Mina_numbers.Global_slot.t ]
    | Unwanted_fee_token of Token_id.t
    | Invalid_transaction
  [@@deriving sexp, to_yojson]

  val grounds_for_diff_rejection : t -> bool
end

val replace_fee : Currency.Fee.t

module Config : sig
  type t
end

module Sender_local_state : sig
  type t [@@deriving sexp, to_yojson]

  val sender : t -> Account_id.t

  val is_remove : t -> bool
end

(** Transaction pool. This is a purely functional data structure. *)
type t [@@deriving sexp_of]

val config : t -> Config.t

val get_sender_local_state : t -> Account_id.t -> Sender_local_state.t

val set_sender_local_state : t -> Sender_local_state.t -> t

module rec Update : sig
  val apply : Update.t -> t -> t

  type t [@@deriving to_yojson, sexp]

  val merge : t -> t -> t

  val empty : t
end

(* TODO sexp is debug only, remove *)

(** Empty pool *)
val empty :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> consensus_constants:Consensus.Constants.t
  -> time_controller:Block_time.Controller.t
  -> t

(** How many transactions are currently in the pool *)
val size : t -> int

(** What is the lowest fee transaction in the pool *)
val min_fee : t -> Currency.Fee.t option

(** Remove the lowest fee command from the pool, along with any others from the
    same account with higher nonces. *)
val remove_lowest_fee :
  t -> Transaction_hash.User_command_with_valid_signature.t Sequence.t * t

(** Remove all the user commands that are expired. (Valid-until < Current-global-slot) *)
val remove_expired :
  t -> Transaction_hash.User_command_with_valid_signature.t Sequence.t * t

(** Get the highest fee applicable command in the pool *)
val get_highest_fee :
  t -> Transaction_hash.User_command_with_valid_signature.t option

(** Call this when a transaction is added to the best tip or when generating a
    sequence of transactions to apply. This will drop any transactions at that
    nonce from the pool. May also drop queued commands for that sender if there
    was a different queued transaction from that sender at that nonce, and the
    committed one consumes more currency than the queued one. In that case it'll
    return the dropped ones in the sequence, including the one with the same
    nonce as the committed one if it's different.
*)
val handle_committed_txn :
     t
  -> Transaction_hash.User_command_with_valid_signature.t
  -> fee_payer_balance:Currency.Amount.t
  -> fee_payer_nonce:Mina_base.Account.Nonce.t
  -> ( t * Transaction_hash.User_command_with_valid_signature.t Sequence.t
     , [ `Queued_txns_by_sender of
         string
         * Transaction_hash.User_command_with_valid_signature.t Sequence.t ] )
     Result.t

(** Add a command to the pool. Pass the current nonce for the account and
    its current balance. Throws if the contents of the pool before adding the
    new command are invalid given the supplied current nonce and balance - you
    are required to keep the pool in sync with the ledger you are applying
    transactions against.
*)
val add_from_gossip_exn_async :
     config:Config.t
  -> sender_local_state:Sender_local_state.t
  -> verify:
       (   User_command.Verifiable.t
        -> User_command.Valid.t option Async.Deferred.t)
  -> [ `Unchecked of Transaction_hash.User_command.t * User_command.Verifiable.t
     | `Checked of Transaction_hash.User_command_with_valid_signature.t ]
  -> Account_nonce.t
  -> Currency.Amount.t
  -> ( ( Transaction_hash.User_command_with_valid_signature.t
       * Transaction_hash.User_command_with_valid_signature.t list )
       * Sender_local_state.t
       * Update.t
     , Command_error.t )
     Async.Deferred.Result.t
(** Returns the commands dropped as a result of adding the command, which will
    be empty unless we're replacing one. *)

(** Add a command to the pool. Pass the current nonce for the account and
    its current balance. Throws if the contents of the pool before adding the
    new command are invalid given the supplied current nonce and balance - you
    are required to keep the pool in sync with the ledger you are applying
    transactions against.
*)
val add_from_gossip_exn :
     t
  -> verify:(User_command.Verifiable.t -> User_command.Valid.t option)
  -> [ `Unchecked of Transaction_hash.User_command.t * User_command.Verifiable.t
     | `Checked of Transaction_hash.User_command_with_valid_signature.t ]
  -> Account_nonce.t
  -> Currency.Amount.t
  -> ( Transaction_hash.User_command_with_valid_signature.t
       * t
       * Transaction_hash.User_command_with_valid_signature.t Sequence.t
     , Command_error.t )
     Result.t
(** Returns the commands dropped as a result of adding the command, which will
    be empty unless we're replacing one. *)

(** Add a command to the pool that was removed from the best tip because we're
    switching chains. Must be called in reverse order i.e. newest-to-oldest.
*)
val add_from_backtrack :
     t
  -> Transaction_hash.User_command_with_valid_signature.t
  -> (t, Command_error.t) Result.t

(** Check whether a command is in the pool *)
val member : t -> Transaction_hash.User_command.t -> bool

(** Get all the user commands sent by a user with a particular account *)
val all_from_account :
  t -> Account_id.t -> Transaction_hash.User_command_with_valid_signature.t list

(** Get all user commands in the pool. *)
val get_all : t -> Transaction_hash.User_command_with_valid_signature.t list

val find_by_hash :
     t
  -> Transaction_hash.t
  -> Transaction_hash.User_command_with_valid_signature.t option

(** Check the contents of the pool are valid against the current ledger. Call
    this whenever the transition frontier is (re)created.
*)
val revalidate :
     t
  -> (Account_id.t -> Account_nonce.t * Currency.Amount.t)
     (** Lookup an account in the new ledger *)
  -> t * Transaction_hash.User_command_with_valid_signature.t Sequence.t

(** Get the current global slot according to the pool's time controller. *)
val current_global_slot : t -> Mina_numbers.Global_slot.t

module For_tests : sig
  (** Checks the invariants of the data structure. If this throws an exception
      there is a bug. *)
  val assert_invariants : t -> unit
end
