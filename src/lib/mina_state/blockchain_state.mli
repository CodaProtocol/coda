open Core_kernel
open Mina_base
open Snark_params.Tick

module Poly : sig
  [%%versioned:
  module Stable : sig
    module V2 : sig
      type ( 'staged_ledger_hash
           , 'snarked_ledger_hash
           , 'token_id
           , 'local_state
           , 'time )
           t =
        { staged_ledger_hash : 'staged_ledger_hash
        ; genesis_ledger_hash : 'snarked_ledger_hash
        ; registers :
            ( 'snarked_ledger_hash
            , unit
            , 'token_id
            , 'local_state )
            Registers.Stable.V1.t
        ; timestamp : 'time
        }
      [@@deriving sexp, fields, equal, compare, hash, yojson, hlist]
    end

    module V1 : sig
      type ('staged_ledger_hash, 'snarked_ledger_hash, 'token_id, 'time) t =
        { staged_ledger_hash : 'staged_ledger_hash
        ; snarked_ledger_hash : 'snarked_ledger_hash
        ; genesis_ledger_hash : 'snarked_ledger_hash
        ; snarked_next_available_token : 'token_id
        ; timestamp : 'time
        }
      [@@deriving sexp, equal, compare, fields, yojson]
    end
  end]
end

module Value : sig
  [%%versioned:
  module Stable : sig
    module V2 : sig
      type t =
        ( Staged_ledger_hash.Stable.V1.t
        , Frozen_ledger_hash.Stable.V1.t
        , Token_id.Stable.V1.t
        , Local_state.Stable.V1.t
        , Block_time.Stable.V1.t )
        Poly.Stable.V2.t
      [@@deriving sexp, equal, compare, hash, yojson]

      val to_latest : t -> t
    end

    module V1 : sig
      type t =
        ( Staged_ledger_hash.Stable.V1.t
        , Frozen_ledger_hash.Stable.V1.t
        , Token_id.Stable.V1.t
        , Block_time.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving sexp, equal, compare, hash, yojson]

      val to_latest : t -> V2.t
    end
  end]
end

include
  Snarkable.S
    with type var =
          ( Staged_ledger_hash.var
          , Frozen_ledger_hash.var
          , Token_id.var
          , Local_state.Checked.t
          , Block_time.Unpacked.var )
          Poly.t
     and type value := Value.t

val staged_ledger_hash :
  ('staged_ledger_hash, _, _, _, _) Poly.t -> 'staged_ledger_hash

val snarked_ledger_hash :
  (_, 'snarked_ledger_hash, _, _, _) Poly.t -> 'snarked_ledger_hash

val genesis_ledger_hash :
  (_, 'snarked_ledger_hash, _, _, _) Poly.t -> 'snarked_ledger_hash

val snarked_next_available_token : (_, _, 'token_id, _, _) Poly.t -> 'token_id

val registers :
     (_, 'snarked_ledger_hash, 'token_id, 'local_state, _) Poly.t
  -> ('snarked_ledger_hash, unit, 'token_id, 'local_state) Registers.Stable.V1.t

val timestamp : (_, _, _, _, 'time) Poly.t -> 'time

val create_value :
     staged_ledger_hash:Staged_ledger_hash.t
  -> genesis_ledger_hash:Frozen_ledger_hash.t
  -> registers:
       ( Frozen_ledger_hash.t
       , unit
       , Token_id.t
       , Local_state.t )
       Registers.Stable.V1.t
  -> timestamp:Block_time.t
  -> Value.t

val negative_one :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> consensus_constants:Consensus.Constants.t
  -> genesis_ledger_hash:Ledger_hash.t
  -> snarked_next_available_token:Token_id.t
  -> Value.t

val genesis :
     constraint_constants:Genesis_constants.Constraint_constants.t
  -> consensus_constants:Consensus.Constants.t
  -> genesis_ledger_hash:Ledger_hash.t
  -> snarked_next_available_token:Token_id.t
  -> Value.t

val set_timestamp :
     ('staged_ledger_hash, 'lh, 'tok, 'ls, 'time) Poly.t
  -> 'time
  -> ('staged_ledger_hash, 'lh, 'tok, 'ls, 'time) Poly.t

val to_input : Value.t -> (Field.t, bool) Random_oracle.Input.t

val var_to_input :
  var -> ((Field.Var.t, Boolean.var) Random_oracle.Input.t, _) Checked.t

type display = (string, string, string, Local_state.display, string) Poly.t
[@@deriving yojson]

val display : Value.t -> display
