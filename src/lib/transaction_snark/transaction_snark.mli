open Core
open Mina_base
open Snark_params

(** For debugging. Logs to stderr the inputs to the top hash. *)
val with_top_hash_logging : (unit -> 'a) -> 'a

module Pending_coinbase_stack_state : sig
  module Init_stack : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t = Base of Pending_coinbase.Stack_versioned.Stable.V1.t | Merge
        [@@deriving sexp, hash, compare, equal, yojson]
      end
    end]
  end

  module Poly : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'pending_coinbase t =
          { source : 'pending_coinbase; target : 'pending_coinbase }
        [@@deriving compare, equal, fields, hash, sexp, yojson]

        val to_latest :
             ('pending_coinbase -> 'pending_coinbase')
          -> 'pending_coinbase t
          -> 'pending_coinbase' t
      end
    end]

    val typ :
         ('pending_coinbase_var, 'pending_coinbase) Tick.Typ.t
      -> ('pending_coinbase_var t, 'pending_coinbase t) Tick.Typ.t
  end

  type 'pending_coinbase poly = 'pending_coinbase Poly.t =
    { source : 'pending_coinbase; target : 'pending_coinbase }
  [@@deriving sexp, hash, compare, equal, fields, yojson]

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t = Pending_coinbase.Stack_versioned.Stable.V1.t Poly.Stable.V1.t
      [@@deriving compare, equal, hash, sexp, yojson]
    end
  end]

  type var = Pending_coinbase.Stack.var Poly.t

  open Tick

  val typ : (var, t) Typ.t

  val to_input : t -> (Field.t, bool) Random_oracle.Input.t

  val var_to_input : var -> (Field.Var.t, Boolean.var) Random_oracle.Input.t
end

module Registers : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type ('ledger, 'pending_coinbase_stack, 'token_id, 'local_state) t =
        { ledger : 'ledger
        ; pending_coinbase_stack : 'pending_coinbase_stack
        ; next_available_token : 'token_id
        ; local_state : 'local_state
        }
      [@@deriving compare, equal, hash, sexp, yojson, hlist, fields]
    end
  end]
end

module Local_state : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t =
        ( Parties.Digest.Stable.V1.t
        , Token_id.Stable.V1.t
        , Currency.Amount.Stable.V1.t
        , Ledger_hash.Stable.V1.t
        , bool
        , Parties.Transaction_commitment.Stable.V1.t )
        Parties_logic.Local_state.Stable.V1.t
      [@@deriving compare, equal, hash, sexp, yojson]
    end
  end]

  module Checked : sig
    open Pickles.Impls.Step

    type t =
      ( Field.t
      , Token_id.Checked.t
      , Currency.Amount.Checked.t
      , Ledger_hash.var
      , Boolean.var
      , Parties.Transaction_commitment.Checked.t )
      Parties_logic.Local_state.t
  end
end

module Statement : sig
  module Poly : sig
    [%%versioned:
    module Stable : sig
      module V2 : sig
        type ( 'ledger_hash
             , 'amount
             , 'pending_coinbase
             , 'fee_excess
             , 'token_id
             , 'sok_digest
             , 'local_state )
             t =
          { source :
              ( 'ledger_hash
              , 'pending_coinbase
              , 'token_id
              , 'local_state )
              Registers.Stable.V1.t
          ; target :
              ( 'ledger_hash
              , 'pending_coinbase
              , 'token_id
              , 'local_state )
              Registers.Stable.V1.t
          ; supply_increase : 'amount
          ; fee_excess : 'fee_excess
          ; sok_digest : 'sok_digest
          }
        [@@deriving compare, equal, hash, sexp, yojson, hlist]
      end

      module V1 : sig
        type ( 'ledger_hash
             , 'amount
             , 'pending_coinbase
             , 'fee_excess
             , 'token_id
             , 'sok_digest )
             t

        (* =
             { source: 'ledger_hash
             ; target: 'ledger_hash
             ; supply_increase: 'amount
             ; pending_coinbase_stack_state: 'pending_coinbase
             ; fee_excess: 'fee_excess
             ; next_available_token_before: 'token_id
             ; next_available_token_after: 'token_id
             ; sok_digest: 'sok_digest }
           [@@deriving compare, equal, hash, sexp, yojson]

           val to_latest :
                ('ledger_hash -> 'ledger_hash')
             -> ('amount -> 'amount')
             -> ('pending_coinbase -> 'pending_coinbase')
             -> ('fee_excess -> 'fee_excess')
             -> ('token_id -> 'token_id')
             -> ('sok_digest -> 'sok_digest')
             -> ( 'ledger_hash
                , 'amount
                , 'pending_coinbase
                , 'fee_excess
                , 'token_id
                , 'sok_digest )
                t
             -> ( 'ledger_hash'
                , 'amount'
                , 'pending_coinbase'
                , 'fee_excess'
                , 'token_id'
                , 'sok_digest' )
                t *)
      end
    end]
  end

  type ( 'ledger_hash
       , 'amount
       , 'pending_coinbase
       , 'fee_excess
       , 'token_id
       , 'sok_digest
       , 'local_state )
       poly =
        ( 'ledger_hash
        , 'amount
        , 'pending_coinbase
        , 'fee_excess
        , 'token_id
        , 'sok_digest
        , 'local_state )
        Poly.t =
    { source :
        ('ledger_hash, 'pending_coinbase, 'token_id, 'local_state) Registers.t
    ; target :
        ('ledger_hash, 'pending_coinbase, 'token_id, 'local_state) Registers.t
    ; supply_increase : 'amount
    ; fee_excess : 'fee_excess
    ; sok_digest : 'sok_digest
    }
  [@@deriving compare, equal, hash, sexp, yojson]

  [%%versioned:
  module Stable : sig
    module V2 : sig
      type t =
        ( Frozen_ledger_hash.Stable.V1.t
        , Currency.Amount.Stable.V1.t
        , Pending_coinbase.Stack_versioned.Stable.V1.t
        , Fee_excess.Stable.V1.t
        , Token_id.Stable.V1.t
        , unit
        , Local_state.Stable.V1.t )
        Poly.Stable.V2.t
      [@@deriving compare, equal, hash, sexp, yojson]
    end

    module V1 : sig
      type t =
        ( Frozen_ledger_hash.Stable.V1.t
        , Currency.Amount.Stable.V1.t
        , Pending_coinbase_stack_state.Stable.V1.t
        , Fee_excess.Stable.V1.t
        , Token_id.Stable.V1.t
        , unit )
        Poly.Stable.V1.t
      [@@deriving compare, equal, hash, sexp, yojson]
    end
  end]

  module With_sok : sig
    [%%versioned:
    module Stable : sig
      module V2 : sig
        type t =
          ( Frozen_ledger_hash.Stable.V1.t
          , Currency.Amount.Stable.V1.t
          , Pending_coinbase.Stack_versioned.Stable.V1.t
          , Fee_excess.Stable.V1.t
          , Token_id.Stable.V1.t
          , Sok_message.Digest.Stable.V1.t
          , Local_state.Stable.V1.t )
          Poly.Stable.V2.t
        [@@deriving compare, equal, hash, sexp, yojson]
      end

      module V1 : sig
        type t =
          ( Frozen_ledger_hash.Stable.V1.t
          , Currency.Amount.Stable.V1.t
          , Pending_coinbase_stack_state.Stable.V1.t
          , Fee_excess.Stable.V1.t
          , Token_id.Stable.V1.t
          , Sok_message.Digest.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving compare, equal, hash, sexp, yojson]
      end
    end]

    type var =
      ( Frozen_ledger_hash.var
      , Currency.Amount.var
      , Pending_coinbase.Stack.var
      , Fee_excess.var
      , Token_id.var
      , Sok_message.Digest.Checked.t
      , Local_state.Checked.t )
      Poly.t

    open Tick

    val typ : (var, t) Typ.t

    val to_input : t -> (Field.t, bool) Random_oracle.Input.t

    val to_field_elements : t -> Field.t array

    module Checked : sig
      type t = var

      val to_input :
        var -> ((Field.Var.t, Boolean.var) Random_oracle.Input.t, _) Checked.t

      (* This is actually a checked function. *)
      val to_field_elements : var -> Field.Var.t array
    end
  end

  val gen : t Quickcheck.Generator.t

  val merge : t -> t -> t Or_error.t

  include Hashable.S_binable with type t := t

  include Comparable.S with type t := t
end

[%%versioned:
module Stable : sig
  module V2 : sig
    type t [@@deriving compare, sexp, yojson]
  end

  module V1 : sig
    type t [@@deriving compare, equal, sexp, yojson, hash]
  end
end]

val create : statement:Statement.With_sok.t -> proof:Mina_base.Proof.t -> t

val proof : t -> Mina_base.Proof.t

val statement : t -> Statement.t

val sok_digest : t -> Sok_message.Digest.t

open Pickles_types

type tag =
  ( Statement.With_sok.Checked.t
  , Statement.With_sok.t
  , Nat.N2.n
  , Nat.N6.n )
  Pickles.Tag.t

val verify :
     (t * Sok_message.t) list
  -> key:Pickles.Verification_key.t
  -> bool Async.Deferred.t

module Verification : sig
  module type S = sig
    val tag : tag

    val verify : (t * Sok_message.t) list -> bool Async.Deferred.t

    val id : Pickles.Verification_key.Id.t Lazy.t

    val verification_key : Pickles.Verification_key.t Lazy.t

    val verify_against_digest : t -> bool Async.Deferred.t

    val constraint_system_digests : (string * Md5_lib.t) list Lazy.t
  end
end

val check_transaction :
     ?preeval:bool
  -> constraint_constants:Genesis_constants.Constraint_constants.t
  -> sok_message:Sok_message.t
  -> source:Frozen_ledger_hash.t
  -> target:Frozen_ledger_hash.t
  -> init_stack:Pending_coinbase.Stack.t
  -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
  -> next_available_token_before:Token_id.t
  -> next_available_token_after:Token_id.t
  -> snapp_account1:Snapp_account.t option
  -> snapp_account2:Snapp_account.t option
  -> Transaction.Valid.t Transaction_protocol_state.t
  -> Tick.Handler.t
  -> unit

val check_user_command :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> sok_message:Sok_message.t
  -> source:Frozen_ledger_hash.t
  -> target:Frozen_ledger_hash.t
  -> init_stack:Pending_coinbase.Stack.t
  -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
  -> next_available_token_before:Token_id.t
  -> next_available_token_after:Token_id.t
  -> Signed_command.With_valid_signature.t Transaction_protocol_state.t
  -> Tick.Handler.t
  -> unit

val generate_transaction_witness :
     ?preeval:bool
  -> constraint_constants:Genesis_constants.Constraint_constants.t
  -> sok_message:Sok_message.t
  -> source:Frozen_ledger_hash.t
  -> target:Frozen_ledger_hash.t
  -> init_stack:Pending_coinbase.Stack.t
  -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
  -> next_available_token_before:Token_id.t
  -> next_available_token_after:Token_id.t
  -> snapp_account1:Snapp_account.t option
  -> snapp_account2:Snapp_account.t option
  -> Transaction.Valid.t Transaction_protocol_state.t
  -> Tick.Handler.t
  -> unit

module Parties_segment : sig
  module Witness : sig
    type t
  end

  module Basic : sig
    type (_, _, _, _) t =
      (* Corresponds to payment *)
      | Opt_signed_unsigned : (unit, unit, unit, unit) t
      | Opt_signed_opt_signed : (unit, unit, unit, unit) t
      | Opt_signed : (unit, unit, unit, unit) t
      | Proved
          : ( Snapp_statement.Checked.t * unit
            , Snapp_statement.t * unit
            , Nat.N2.n * unit
            , Side_loaded_verification_key.Max_branches.n * unit )
            t
  end
end

module type S = sig
  include Verification.S

  val cache_handle : Pickles.Cache_handle.t

  val of_transaction :
       sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> snapp_account1:Snapp_account.t option
    -> snapp_account2:Snapp_account.t option
    -> Transaction.Valid.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t Async.Deferred.t

  val of_user_command :
       sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> Signed_command.With_valid_signature.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t Async.Deferred.t

  val of_fee_transfer :
       sok_digest:Sok_message.Digest.t
    -> source:Frozen_ledger_hash.t
    -> target:Frozen_ledger_hash.t
    -> init_stack:Pending_coinbase.Stack.t
    -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
    -> next_available_token_before:Token_id.t
    -> next_available_token_after:Token_id.t
    -> Fee_transfer.t Transaction_protocol_state.t
    -> Tick.Handler.t
    -> t Async.Deferred.t

  val of_parties_segment_exn :
       (_, _, _, _) Parties_segment.Basic.t
    -> statement:Statement.With_sok.t
    -> witness:Parties_segment.Witness.t
    -> t Async.Deferred.t

  val merge :
    t -> t -> sok_digest:Sok_message.Digest.t -> t Async.Deferred.Or_error.t
end

module Make (Inputs : sig
  val constraint_constants : Genesis_constants.Constraint_constants.t

  val proof_level : Genesis_constants.Proof_level.t
end) : S
[@@warning "-67"]

val constraint_system_digests :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> unit
  -> (string * Md5.t) list
