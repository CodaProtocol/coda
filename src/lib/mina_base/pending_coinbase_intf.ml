(** Pending_coinbase is to keep track of all the coinbase transactions that have been applied to the ledger but for which there is no ledger proof yet. Every ledger proof corresponds to a sequence of coinbase transactions which is part of all the transactions it proves. Each of these sequences[Stack] are stored using the merkle tree representation. The stacks are operated in a FIFO manner by keeping track of its positions in the merkle tree. Whenever a ledger proof is emitted, the oldest stack is removed from the tree and when a new coinbase is applied, the latest stack is updated with the new coinbase.
    The operations on the merkle tree of coinbase stacks include:
    1) adding a new singleton stack
    2) updating the latest stack when a new coinbase is added to it
    2) deleting the oldest stack

    A stack can be either be created or modified by pushing a coinbase on to it.

    This module also provides an interface for the checked computations required required to prove it in snark

    Stack operations are done for transaction snarks and tree operations are done for the blockchain snark*)

open Core
open Snark_params
open Snarky_backendless
open Tick
open Signature_lib
open Currency

module type S = sig
  type t [@@deriving sexp, to_yojson]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, to_yojson, version]
    end

    module Latest = V1
  end

  module Coinbase_data : sig
    module Stable : sig
      module V1 : sig
        type t = Public_key.Compressed.Stable.V1.t * Amount.Stable.V1.t
        [@@deriving sexp, bin_io, to_yojson]
      end

      module Latest = V1
    end

    type t = Stable.Latest.t [@@deriving sexp, to_yojson]

    type var = Public_key.Compressed.var * Amount.var

    val typ : (var, t) Typ.t

    val empty : t

    val of_coinbase : Coinbase.t -> t

    val genesis : t

    val var_of_t : t -> var
  end

  module type Data_hash_intf = sig
    type t = private Field.t [@@deriving sexp, compare, equal, yojson, hash]

    type var

    val var_of_t : t -> var

    val typ : (var, t) Typ.t

    val var_to_hash_packed : var -> Field.Var.t

    val equal_var : var -> var -> (Boolean.var, _) Tick.Checked.t

    val to_bytes : t -> string

    val to_bits : t -> bool list

    val gen : t Quickcheck.Generator.t
  end

  module rec Hash : sig
    include Data_hash_intf

    val merge : height:int -> t -> t -> t

    val empty_hash : t

    val of_digest : Random_oracle.Digest.t -> t
  end

  module Hash_versioned : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type nonrec t = Hash.t [@@deriving sexp, compare, equal, yojson, hash]
      end
    end]
  end

  module Stack_versioned : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type nonrec t [@@deriving sexp, compare, equal, yojson, hash]
      end
    end]
  end

  module Stack : sig
    type t = Stack_versioned.t [@@deriving sexp, compare, equal, yojson, hash]

    type var

    val data_hash : t -> Hash.t

    val var_of_t : t -> var

    val typ : (var, t) Typ.t

    val gen : t Quickcheck.Generator.t

    val to_input : t -> (Field.t, bool) Random_oracle.Input.t

    val to_bits : t -> bool list

    val to_bytes : t -> string

    val equal_var : var -> var -> (Boolean.var, _) Tick.Checked.t

    val var_to_input : var -> (Field.Var.t, Boolean.var) Random_oracle.Input.t

    val empty : t

    (** Creates a new stack with the state stack from an existing stack*)
    val create_with : t -> t

    val equal_data : t -> t -> bool

    val equal_state_hash : t -> t -> bool

    val push_coinbase : Coinbase.t -> t -> t

    val push_state : State_body_hash.t -> t -> t

    module Checked : sig
      type t = var

      val push_coinbase : Coinbase_data.var -> t -> (t, 'a) Tick.Checked.t

      val push_state : State_body_hash.var -> t -> (t, 'a) Tick.Checked.t

      val if_ : Boolean.var -> then_:t -> else_:t -> (t, _) Tick.Checked.t

      val check_merge :
           transition1:t * t
        -> transition2:t * t
        -> (Boolean.var, _) Tick.Checked.t

      val empty : t

      val create_with : t -> t
    end
  end

  module State_stack : sig
    type t
  end

  module Update : sig
    module Action : sig
      [%%versioned:
      module Stable : sig
        module V1 : sig
          type t =
            | Update_none
            | Update_one
            | Update_two_coinbase_in_first
            | Update_two_coinbase_in_second
          [@@deriving sexp, to_yojson]
        end
      end]

      type var = Boolean.var * Boolean.var

      val typ : (var, t) Typ.t

      val var_of_t : t -> var
    end

    module Poly : sig
      [%%versioned:
      module Stable : sig
        module V1 : sig
          type ('action, 'coinbase_amount) t =
            {action: 'action; coinbase_amount: 'coinbase_amount}
          [@@deriving sexp]
        end
      end]
    end

    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t = (Action.Stable.V1.t, Amount.Stable.V1.t) Poly.Stable.V1.t
        [@@deriving sexp, to_yojson]
      end
    end]

    type var = (Action.var, Amount.var) Poly.t

    val genesis : t

    val typ : (var, t) Typ.t

    val var_of_t : t -> var
  end

  val create : depth:int -> unit -> t Or_error.t

  (** Delete the oldest stack*)
  val remove_coinbase_stack : depth:int -> t -> (Stack.t * t) Or_error.t

  (** Root of the merkle tree that has stacks as leaves*)
  val merkle_root : t -> Hash.t

  val handler :
    depth:int -> t -> is_new_stack:bool -> (request -> response) Staged.t

  (** Update the current working stack or if [is_new_stack] add as the new working stack*)
  val update_coinbase_stack :
    depth:int -> t -> Stack.t -> is_new_stack:bool -> t Or_error.t

  (** Stack that is currently being updated. if [is_new_stack] then a new stack is returned*)
  val latest_stack : t -> is_new_stack:bool -> Stack.t Or_error.t

  (** The stack that corresponds to the next ledger proof that is to be generated*)
  val oldest_stack : t -> Stack.t Or_error.t

  (** Hash of the auxiliary data (everything except the merkle root (Hash.t))*)
  val hash_extra : t -> string

  module Checked : sig
    type var = Hash.var

    type path

    module Address : sig
      type value

      type var

      val typ : depth:int -> (var, value) Typ.t
    end

    type _ Request.t +=
      | Coinbase_stack_path : Address.value -> path Request.t
      | Get_coinbase_stack : Address.value -> (Stack.t * path) Request.t
      | Set_coinbase_stack : Address.value * Stack.t -> unit Request.t
      | Set_oldest_coinbase_stack : Address.value * Stack.t -> unit Request.t
      | Find_index_of_newest_stacks :
          Update.Action.t
          -> (Address.value * Address.value) Request.t
      | Find_index_of_oldest_stack : Address.value Request.t
      | Get_previous_stack : State_stack.t Request.t

    val get : depth:int -> var -> Address.var -> (Stack.var, _) Tick.Checked.t

    (**
       [update_stack t ~is_new_stack updated_stack] implements the following spec:
       - gets the address[addr] of the latest stack or a new stack
       - finds a coinbase stack in [t] at path [addr] and pushes the coinbase_data on to the stack
       - returns a root [t'] of the tree
    *)
    val add_coinbase :
         constraint_constants:Genesis_constants.Constraint_constants.t
      -> var
      -> Update.var
      -> coinbase_receiver:Public_key.Compressed.var
      -> supercharge_coinbase:Boolean.var
      -> State_body_hash.var
      -> (var, 's) Tick.Checked.t

    (**
       [pop_coinbases t pk updated_stack] implements the following spec:

       - gets the address[addr] of the oldest stack.
       - finds a coinbase stack in [t] at path [addr] and replaces it with empty stack if a [proof_emitted] is true
       - returns a root [t'] of the tree
    *)
    val pop_coinbases :
         constraint_constants:Genesis_constants.Constraint_constants.t
      -> var
      -> proof_emitted:Boolean.var
      -> (var * Stack.var, 's) Tick.Checked.t
  end
end
