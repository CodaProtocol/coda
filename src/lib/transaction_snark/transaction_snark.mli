open Core
open Coda_base
open Snark_params

(** For debugging. Logs to stderr the inputs to the top hash. *)
val with_top_hash_logging : (unit -> 'a) -> 'a

module Pending_coinbase_stack_state : sig
  module Init_stack : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t = Base of Pending_coinbase.Stack_versioned.Stable.V1.t | Merge
        [@@deriving sexp, hash, compare, eq, yojson]
      end
    end]
  end

  module Poly : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type 'pending_coinbase t =
          {source: 'pending_coinbase; target: 'pending_coinbase}
        [@@deriving compare, eq, fields, hash, sexp, yojson]

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
    {source: 'pending_coinbase; target: 'pending_coinbase}
  [@@deriving sexp, hash, compare, eq, fields, yojson]

  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t = Pending_coinbase.Stack_versioned.Stable.V1.t Poly.Stable.V1.t
      [@@deriving compare, eq, hash, sexp, yojson]
    end
  end]

  type var = Pending_coinbase.Stack.var Poly.t

  open Tick

  val typ : (var, t) Typ.t

  val to_input : t -> (Field.t, bool) Random_oracle.Input.t

  val var_to_input : var -> (Field.Var.t, Boolean.var) Random_oracle.Input.t
end

module Statement : sig
  module Poly : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type ( 'ledger_hash
             , 'amount
             , 'pending_coinbase
             , 'fee_excess
             , 'token_id
             , 'sok_digest )
             t =
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
             t
      end
    end]
  end

  type ( 'ledger_hash
       , 'amount
       , 'pending_coinbase
       , 'fee_excess
       , 'token_id
       , 'sok_digest )
       poly =
        ( 'ledger_hash
        , 'amount
        , 'pending_coinbase
        , 'fee_excess
        , 'token_id
        , 'sok_digest )
        Poly.t =
    { source: 'ledger_hash
    ; target: 'ledger_hash
    ; supply_increase: 'amount
    ; pending_coinbase_stack_state: 'pending_coinbase
    ; fee_excess: 'fee_excess
    ; next_available_token_before: 'token_id
    ; next_available_token_after: 'token_id
    ; sok_digest: 'sok_digest }
  [@@deriving compare, equal, hash, sexp, yojson]

  [%%versioned:
  module Stable : sig
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
      module V1 : sig
        type t =
          ( Frozen_ledger_hash.Stable.V1.t
          , Currency.Amount.Stable.V1.t
          , Pending_coinbase_stack_state.Stable.V1.t
          , Fee_excess.Stable.V1.t
          , Token_id.Stable.V1.t
          , Sok_message.Digest.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving compare, equal, hash, sexp, to_yojson]
      end
    end]

    type var =
      ( Frozen_ledger_hash.var
      , Currency.Amount.var
      , Pending_coinbase_stack_state.var
      , Fee_excess.var
      , Token_id.var
      , Sok_message.Digest.Checked.t )
      Poly.Stable.V1.t

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
  module V1 : sig
    type t [@@deriving compare, sexp, to_yojson]
  end
end]

val create :
     source:Frozen_ledger_hash.t
  -> target:Frozen_ledger_hash.t
  -> supply_increase:Currency.Amount.t
  -> pending_coinbase_stack_state:Pending_coinbase_stack_state.t
  -> fee_excess:Fee_excess.t
  -> next_available_token_before:Token_id.t
  -> next_available_token_after:Token_id.t
  -> sok_digest:Sok_message.Digest.t
  -> proof:Coda_base.Proof.t
  -> t

val proof : t -> Coda_base.Proof.t

val statement : t -> Statement.t

val sok_digest : t -> Sok_message.Digest.t

open Pickles_types

type tag =
  ( Statement.With_sok.Checked.t
  , Statement.With_sok.t
  , Nat.N2.n
  , Nat.N5.n )
  Pickles.Tag.t

val verify : (t * Sok_message.t) list -> key:Pickles.Verification_key.t -> bool

module Verification : sig
  module type S = sig
    val tag : tag

    val verify : (t * Sok_message.t) list -> bool

    val id : Pickles.Verification_key.Id.t Lazy.t

    val verification_key : Pickles.Verification_key.t Lazy.t

    val verify_against_digest : t -> bool
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
    -> t

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
    -> t

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
    -> t

  val merge : t -> t -> sok_digest:Sok_message.Digest.t -> t Or_error.t
end

module Make () : S

val constraint_system_digests : unit -> (string * Md5.t) list
