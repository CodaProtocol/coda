(* extensional.ml -- extensional representations of archive db data *)

open Core_kernel
open Mina_base
open Signature_lib

(* the tables in the archive db uses foreign keys to refer to other
   tables; the types here fills in the data from those other tables, using
   their native OCaml types to assure the validity of the data
*)

module User_command = struct
  (* for `typ` and `status`, a string is enough
     in any case, there aren't existing string conversions for the
     original OCaml types

     The versioned modules in Transaction_hash.Stable don't have yojson functions
     it would be difficult to add them, so we just use the ones for the
      top-level Transaction.t
  *)
  type t =
    { sequence_no : int
    ; typ : string
    ; fee_payer : Public_key.Compressed.Stable.Latest.t
    ; source : Public_key.Compressed.Stable.Latest.t
    ; receiver : Public_key.Compressed.Stable.Latest.t
    ; fee_token : Token_id.Stable.Latest.t
    ; token : Token_id.Stable.Latest.t
    ; nonce : Account.Nonce.Stable.Latest.t
    ; amount : Currency.Amount.Stable.Latest.t option
    ; fee : Currency.Fee.Stable.Latest.t
    ; valid_until : Mina_numbers.Global_slot.Stable.Latest.t option
    ; memo : Signed_command_memo.Stable.Latest.t
    ; hash : Transaction_hash.Stable.Latest.t
          [@to_yojson Transaction_hash.to_yojson]
          [@of_yojson Transaction_hash.of_yojson]
    ; status : string
    ; failure_reason : Transaction_status.Failure.Stable.Latest.t option
    ; source_balance : Currency.Balance.Stable.Latest.t option
    ; fee_payer_account_creation_fee_paid :
        Currency.Amount.Stable.Latest.t option
    ; fee_payer_balance : Currency.Balance.Stable.Latest.t
    ; receiver_account_creation_fee_paid :
        Currency.Amount.Stable.Latest.t option
    ; receiver_balance : Currency.Balance.Stable.Latest.t option
    ; created_token : Token_id.Stable.Latest.t option
    }
  [@@deriving yojson, equal, bin_io_unversioned]
end

module Internal_command = struct
  (* for `typ`, a string is enough
     no existing string conversion for the original OCaml type
  *)
  type t =
    { sequence_no : int
    ; secondary_sequence_no : int
    ; typ : string
    ; receiver : Public_key.Compressed.Stable.Latest.t
    ; receiver_balance : Currency.Balance.Stable.Latest.t
    ; fee : Currency.Fee.Stable.Latest.t
    ; token : Token_id.Stable.Latest.t
    ; hash : Transaction_hash.Stable.Latest.t
          [@to_yojson Transaction_hash.to_yojson]
          [@of_yojson Transaction_hash.of_yojson]
    }
  [@@deriving yojson, equal, bin_io_unversioned]
end

module Block = struct
  type t =
    { state_hash : State_hash.Stable.Latest.t
    ; parent_hash : State_hash.Stable.Latest.t
    ; creator : Public_key.Compressed.Stable.Latest.t
    ; block_winner : Public_key.Compressed.Stable.Latest.t
    ; snarked_ledger_hash : Frozen_ledger_hash.Stable.Latest.t
    ; staking_epoch_seed : Epoch_seed.Stable.Latest.t
    ; staking_epoch_ledger_hash : Frozen_ledger_hash.Stable.Latest.t
    ; next_epoch_seed : Epoch_seed.Stable.Latest.t
    ; next_epoch_ledger_hash : Frozen_ledger_hash.Stable.Latest.t
    ; ledger_hash : Ledger_hash.Stable.Latest.t
    ; height : Unsigned_extended.UInt32.Stable.Latest.t
    ; global_slot : Mina_numbers.Global_slot.Stable.Latest.t
    ; global_slot_since_genesis : Mina_numbers.Global_slot.Stable.Latest.t
    ; timestamp : Block_time.Stable.Latest.t
    ; user_cmds : User_command.t list
    ; internal_cmds : Internal_command.t list
    }
  [@@deriving yojson, equal, bin_io_unversioned]
end
