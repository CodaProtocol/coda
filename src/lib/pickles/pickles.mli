open Core_kernel
open Pickles_types
open Hlist
module Tick_field_sponge = Tick_field_sponge
module Util = Util
module Step_main_inputs = Step_main_inputs
module Backend = Backend
module Sponge_inputs = Sponge_inputs
module Impls = Impls
module Inductive_rule = Inductive_rule
module Tag = Tag
module Pairing_main = Pairing_main

module type Statement_intf = sig
  type field

  type t

  val to_field_elements : t -> field array
end

module type Statement_var_intf =
  Statement_intf with type field := Impls.Step.Field.t

module type Statement_value_intf =
  Statement_intf with type field := Impls.Step.field

module Verification_key : sig
  [%%versioned:
  module Stable : sig
    module V1 : sig
      type t
    end
  end]

  val dummy : t Lazy.t

  module Id : sig
    type t [@@deriving sexp, equal]

    val dummy : unit -> t

    val to_string : t -> string
  end

  val load :
       cache:Key_cache.Spec.t list
    -> Id.t
    -> (t * [`Cache_hit | `Locally_generated]) Async.Deferred.Or_error.t
end

module type Proof_intf = sig
  type statement

  type t

  val verification_key : Verification_key.t Lazy.t

  val id : Verification_key.Id.t Lazy.t

  val verify : (statement * t) list -> bool Async.Deferred.t
end

module Proof : sig
  type ('max_width, 'mlmb) t

  val dummy : 'w Nat.t -> 'm Nat.t -> _ Nat.t -> ('w, 'm) t

  module Make (W : Nat.Intf) (MLMB : Nat.Intf) : sig
    type nonrec t = (W.n, MLMB.n) t [@@deriving sexp, compare, yojson, hash]
  end

  module Branching_2 : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t = Make(Nat.N2)(Nat.N2).t
        [@@deriving sexp, compare, equal, yojson, hash]
      end
    end]
  end
end

module Statement_with_proof : sig
  type ('s, 'max_width, _) t = 's * ('max_width, 'max_width) Proof.t
end

val verify :
     (module Nat.Intf with type n = 'n)
  -> (module Statement_value_intf with type t = 'a)
  -> Verification_key.t
  -> ('a * ('n, 'n) Proof.t) list
  -> bool Async.Deferred.t

module Prover : sig
  type ('prev_values, 'local_widths, 'local_heights, 'a_value, 'proof) t =
       ?handler:(   Snarky_backendless.Request.request
                 -> Snarky_backendless.Request.response)
    -> ( 'prev_values
       , 'local_widths
       , 'local_heights )
       H3.T(Statement_with_proof).t
    -> 'a_value
    -> 'proof
end

module Provers : module type of H3_2.T (Prover)

module Dirty : sig
  type t = [`Cache_hit | `Generated_something | `Locally_generated]

  val ( + ) : t -> t -> t
end

module Cache_handle : sig
  type t

  val generate_or_load : t -> Dirty.t
end

module Side_loaded : sig
  module Verification_key : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        type t [@@deriving sexp, equal, compare, hash, yojson]
      end
    end]

    val dummy : t

    open Impls.Step

    val to_input : t -> (Field.Constant.t, bool) Random_oracle_input.t

    module Checked : sig
      type t

      val to_input : t -> (Field.t, Boolean.var) Random_oracle_input.t
    end

    val typ : (Checked.t, t) Impls.Step.Typ.t

    module Max_branches : Nat.Add.Intf

    module Max_width : Nat.Add.Intf
  end

  module Proof : sig
    [%%versioned:
    module Stable : sig
      module V1 : sig
        (* TODO: This should really be able to be any width up to the max width... *)
        type t =
          (Verification_key.Max_width.n, Verification_key.Max_width.n) Proof.t
        [@@deriving sexp, equal, yojson, hash, compare]
      end
    end]
  end

  val create :
       name:string
    -> max_branching:(module Nat.Add.Intf with type n = 'n1)
    -> value_to_field_elements:('value -> Impls.Step.Field.Constant.t array)
    -> var_to_field_elements:('var -> Impls.Step.Field.t array)
    -> typ:('var, 'value) Impls.Step.Typ.t
    -> ('var, 'value, 'n1, Verification_key.Max_branches.n) Tag.t

  val verify :
       value_to_field_elements:('value -> Impls.Step.Field.Constant.t array)
    -> (Verification_key.t * 'value * Proof.t) list
    -> bool Async.Deferred.t

  (* Must be called in the inductive rule snarky function defining a
   rule for which this tag is used as a predecessor. *)
  val in_circuit :
    ('var, 'value, 'n1, 'n2) Tag.t -> Verification_key.Checked.t -> unit

  (* Must be called immediately before calling the prover for the inductive rule
    for which this tag is used as a predecessor. *)
  val in_prover : ('var, 'value, 'n1, 'n2) Tag.t -> Verification_key.t -> unit
end

module Make (Inputs : sig
  module A : Statement_var_intf

  module A_value : Statement_value_intf

  module Max_branching : Nat.Add.Intf

  module Branches : Nat.Intf

  val constraint_constants : Snark_keys_header.Constraint_constants.t

  val name : string

  val self :
    [`New | `Existing of (A.t, A_value.t, Max_branching.n, Branches.n) Tag.t]

  val typ : (A.t, A_value.t) Impls.Step.Typ.t

  type prev_varss

  type prev_valuess

  type widthss

  type heightss

  val choices :
       self:(A.t, A_value.t, Max_branching.n, Branches.n) Tag.t
    -> ( prev_varss
       , prev_valuess
       , widthss
       , heightss
       , A.t
       , A_value.t )
       H4_2.T(Inductive_rule).t
end) : sig
  open Inputs

  val self : (A.t, A_value.t, Max_branching.n, Branches.n) Tag.t

  type maxes_ns

  module Maxes : sig
    type ns = maxes_ns

    type length = Max_branching.n

    val length : (ns, length) Hlist.Length.t

    val maxes : ns Hlist.H1.T(Nat).t
  end

  val step_domains : (Pickles_base.Domains.t, Branches.n) Vector.t

  module type Steps = sig
    type prev_vars

    type prev_values

    type widths

    type heights

    val branch_data :
      ( A.t
      , A_value.t
      , Max_branching.n
      , Branches.n
      , prev_vars
      , prev_values
      , widths
      , heights )
      Step_branch_data.t

    val constraint_system : Backend.Tick.R1CS_constraint_system.t Lazy.t

    val constraint_system_digest : Md5.t Lazy.t

    val constraint_system_hash : string Lazy.t

    module Keys : sig
      module Proving : sig
        type t = private Backend.Tick.Proving_key.t

        val header_template : Snark_keys_header.t Lazy.t

        val cache_key : Cache.Step.Key.Proving.t Lazy.t

        val check_header : string -> Snark_keys_header.t Or_error.t

        val read_with_header : string -> (Snark_keys_header.t * t) Or_error.t

        val write_with_header : string -> t -> unit Or_error.t

        (** Set or get the [registered_key]. This is implicitly called by
            [use_key_cache]; care should be taken to ensure that this is not
            set when that will also be called.
        *)
        val registered_key : t Lazy.t Set_once.t

        (** Lazy proxy to the [registered_key] value. *)
        val registered_key_lazy : t Lazy.t

        val of_raw_key : Backend.Tick.Proving_key.t -> t
      end

      module Verification : sig
        type t = private Backend.Tick.Verification_key.t

        val header_template : Snark_keys_header.t Lazy.t

        val cache_key : Cache.Step.Key.Verification.t Lazy.t

        val check_header : string -> Snark_keys_header.t Or_error.t

        val read_with_header : string -> (Snark_keys_header.t * t) Or_error.t

        val write_with_header : string -> t -> unit Or_error.t

        (** Set or get the [registered_key]. This is implicitly called by
            [use_key_cache]; care should be taken to ensure that this is not
            set when that will also be called.
        *)
        val registered_key : t Lazy.t Set_once.t

        (** Lazy proxy to the [registered_key] value. *)
        val registered_key_lazy : t Lazy.t

        val of_raw_key : Backend.Tick.Verification_key.t -> t
      end

      val generate : unit -> Proving.t * Verification.t

      val read_or_generate_from_cache :
           Key_cache.Spec.t list
        -> (Proving.t * Dirty.t) Lazy.t * (Verification.t * Dirty.t) Lazy.t

      (** Register the key cache as the source for the keys.
          This may be called instead of setting [Proving.registered_key] and
          [Verification.registered_key].
          If either key has already been registered, this function will fail
      *)
      val use_key_cache : Key_cache.Spec.t list -> unit
    end

    val prove :
         ?handler:(   Snarky_backendless.Request.request
                   -> Snarky_backendless.Request.response)
      -> (prev_values, widths, heights) H3.T(Statement_with_proof).t
      -> A_value.t
      -> (Max_branching.n, Max_branching.n) Proof.t Async.Deferred.t
  end

  module Steps_m : sig
    type ('prev_vars, 'prev_values, 'widths, 'heights) t =
      (module Steps
         with type prev_vars = 'prev_vars
          and type prev_values = 'prev_values
          and type widths = 'widths
          and type heights = 'heights)
  end

  val steps :
    (prev_varss, prev_valuess, widthss, heightss) Hlist.H4.T(Steps_m).t

  module Wrap_keys : sig
    val requests : (Max_branching.n, maxes_ns) Requests.Wrap.t

    val main :
         ( Impls.Wrap_impl.field Snarky_backendless.Cvar.t
         , Impls.Wrap_impl.Field.t Pickles_types.Scalar_challenge.t
         , Wrap_main.Other_field.Packed.t Pickles_types.Shifted_value.t
         , Impls.Wrap_impl.Field.t (* Unused *)
         , Impls.Wrap_impl.Field.t
         , Impls.Wrap_impl.Field.t
         , Impls.Wrap.Impl.field Snarky_backendless.Cvar.t
         , ( Impls.Wrap_impl.Field.t Wrap_main.SC.t
             Import.Types.Bulletproof_challenge.t
           , Nat.z Backend.Tick.Rounds.plus_n )
           Pickles_types.Vector.t
         , Impls.Wrap_impl.field Snarky_backendless.Cvar.t )
         Composition_types.Dlog_based.Statement.In_circuit.t
      -> unit

    val constraint_system : Impls.Wrap.R1CS_constraint_system.t Lazy.t

    val constraint_system_digest : Md5.t Lazy.t

    val constraint_system_hash : string Lazy.t

    module Keys : sig
      module Proving : sig
        type t = private Backend.Tock.Proving_key.t

        val header_template : Snark_keys_header.t Lazy.t

        val cache_key : Cache.Wrap.Key.Proving.t Lazy.t

        val check_header : string -> Snark_keys_header.t Or_error.t

        val read_with_header : string -> (Snark_keys_header.t * t) Or_error.t

        val write_with_header : string -> t -> unit Or_error.t

        (** Set or get the [registered_key]. This is implicitly called by
            [use_key_cache]; care should be taken to ensure that this is not
            set when that will also be called.
        *)
        val registered_key : t Lazy.t Set_once.t

        (** Lazy proxy to the [registered_key] value. *)
        val registered_key_lazy : t Lazy.t

        val of_raw_key : Backend.Tock.Proving_key.t -> t
      end

      module Verification : sig
        type t = private Verification_key.t

        val header_template : Snark_keys_header.t Lazy.t

        val cache_key : Cache.Wrap.Key.Verification.t Lazy.t

        val check_header : string -> Snark_keys_header.t Or_error.t

        val read_with_header : string -> (Snark_keys_header.t * t) Or_error.t

        val write_with_header : string -> t -> unit Or_error.t

        (** Set or get the [registered_key]. This is implicitly called by
            [use_key_cache]; care should be taken to ensure that this is not
            set when that will also be called.
        *)
        val registered_key : t Lazy.t Set_once.t

        (** Lazy proxy to the [registered_key] value. *)
        val registered_key_lazy : t Lazy.t

        val of_raw_key : Verification_key.t -> t
      end

      val generate : unit -> Proving.t * Verification.t

      val read_or_generate_from_cache :
           Key_cache.Spec.t list
        -> (Proving.t * Dirty.t) Lazy.t * (Verification.t * Dirty.t) Lazy.t

      (** Register the key cache as the source for the keys.
          This may be called instead of setting [Proving.registered_key] and
          [Verification.registered_key].
          If either key has already been registered, this function will fail
      *)
      val use_key_cache : Key_cache.Spec.t list -> unit
    end
  end

  val verify :
    (A_value.t * (Max_branching.n, Max_branching.n) Proof.t) list -> bool

  module Proof :
    Proof_intf
    with type t = (Max_branching.n, Max_branching.n) Proof.t
     and type statement = A_value.t

  (** Register this proof system, associating it with the tag [self].
      This must be called exactly once, after the
      [Wrap_keys.Keys.Verification.registered_key] has been satisfied, but
      before evaluating any proof system that refers to the tag.
  *)
  val register : unit -> unit

  (** Load keys from the specified cache, or generate them if they are not
      found.
      This sets the [registered_key] values in [Wrap_keys] and each module in
      [steps] before calling [register]. If any of these have already been
      called, the corresponding exception will be raised.
  *)
  val use_cache : Key_cache.Spec.t list -> Cache_handle.t
end

(** This compiles a series of inductive rules defining a set into a proof
    system for proving membership in that set, with a prover corresponding
    to each inductive rule. *)
val compile :
     ?self:('a_var, 'a_value, 'max_branching, 'branches) Tag.t
  -> ?cache:Key_cache.Spec.t list
  -> (module Statement_var_intf with type t = 'a_var)
  -> (module Statement_value_intf with type t = 'a_value)
  -> typ:('a_var, 'a_value) Impls.Step.Typ.t
  -> branches:(module Nat.Intf with type n = 'branches)
  -> max_branching:(module Nat.Add.Intf with type n = 'max_branching)
  -> name:string
  -> constraint_constants:Snark_keys_header.Constraint_constants.t
  -> choices:(   self:('a_var, 'a_value, 'max_branching, 'branches) Tag.t
              -> ( 'prev_varss
                 , 'prev_valuess
                 , 'widthss
                 , 'heightss
                 , 'a_var
                 , 'a_value )
                 H4_2.T(Inductive_rule).t)
  -> ('a_var, 'a_value, 'max_branching, 'branches) Tag.t
     * Cache_handle.t
     * (module Proof_intf
          with type t = ('max_branching, 'max_branching) Proof.t
           and type statement = 'a_value)
     * ( 'prev_valuess
       , 'widthss
       , 'heightss
       , 'a_value
       , ('max_branching, 'max_branching) Proof.t Async.Deferred.t )
       H3_2.T(Prover).t
