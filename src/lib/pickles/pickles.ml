module P = Proof

module type Statement_intf = Intf.Statement

module type Statement_var_intf = Intf.Statement_var

module type Statement_value_intf = Intf.Statement_value

open Tuple_lib
module SC = Scalar_challenge
open Core_kernel
open Import
open Types
open Pickles_types
open Poly_types
open Hlist
open Common
open Backend
module Backend = Backend
module Sponge_inputs = Sponge_inputs
module Util = Util
module Tick_field_sponge = Tick_field_sponge
module Impls = Impls
module Inductive_rule = Inductive_rule
module Tag = Tag
module Dirty = Dirty
module Cache_handle = Cache_handle
module Step_main_inputs = Step_main_inputs
module Pairing_main = Pairing_main

let verify = Verify.verify

(* This file (as you can see from the mli) defines a compiler which turns an inductive
   definition of a set into an inductive SNARK system for proving using those rules.

   The two ingredients we use are two SNARKs.
   - A pairing based SNARK for a field Fp, using the group G1/Fq (whose scalar field is Fp)
   - A DLOG based SNARK for a field Fq, using the group G/Fp (whose scalar field is Fq)

   For convenience in this discussion, let's define
    (F_0, G_0) := (Fp, G1)
    (F_1, G_1) := (Fq, G)
   So ScalarField(G_i) = F_i and G_i / F_{1-i}.

   An inductive set A is defined by a sequence of inductive rules.
   An inductive rule is intuitively described by something of the form

   a1 ∈ A1, ..., an ∈ An
     f [ a0, ... a1 ] a
   ----------------------
           a ∈ A

   where f is a snarky function defined over an Impl with Field.t = Fp
   and each Ai is itself an inductive rule (possibly equal to A itself).

   We pursue the "step" then "wrap" approach for proof composition.

   The main source of complexity is that we must "wrap" proofs whose verifiers are
   slightly different.

   The main sources of complexity are twofold:
   1. Each SNARK verifier includes group operations and scalar field operations.
      This is problematic because the group operations use the base field, which is
      not equal to the scalar field.

      Schematically, from the circuit point-of-view, we can say a proof is
      - a sequence of F_0 elements xs_0
      - a sequence of F_1 elelements xs_1
      and a verifier is a pair of "snarky functions"
      - check_0 : F_0 list -> F_1 list -> unit which uses the Impl with Field.t = F_0
      - check_1 : F_0 list -> F_1 list -> unit which uses the Impl with Field.t = F_1
      - subset_00 : 'a list -> 'a list
      - subset_01 : 'a list -> 'a list
      - subset_10 : 'a list -> 'a list
      - subset_11 : 'a list -> 'a list
      and a proof verifies if
      ( check_0 (subset_00 xs_0) (subset_01 xs_1)  ;
        check_1 (subset_10 xs_0) (subset_11 xs_1) )

      When verifying a proof, we perform the parts of the verifier involving group operations
      and expose as public input the scalar-field elements we need to perform the final checks.

      In the F_0 circuit, we witness xs_0 and xs_1,
      execute `check_0 (subset_00 xs_0) (subset_01 xs_1)` and
      expose `subset_10 xs_0` and `subset_11 xs_1` as public inputs.

      So the "public inputs" contain within them an "unfinalized proof".

      Then, the next time we verify that proof within an F_1 circuit we "finalize" those
      unfinalized proofs by running `check_1 xs_0_subset xs_1_subset`.

      I didn't implement it exactly this way (although in retrospect probably I should have) but
      that's the basic idea.

      **The complexity this causes:**
      When you prove a rule that includes k recursive verifications, you expose k unfinalized
      proofs. So, the shape of a statement depends on how many "predecessor statements" it has
      or in other words, how many verifications were performed within it.

      Say we have an inductive set given by inductive rules R_1, ... R_n such that
      each rule R_i has k_i predecessor statements.

      In the "wrap" circuit, we must be able to verify a proof coming from any of the R_i.
      So, we must pad the statement for the proof we're wrapping to have `max_i k_i`
      unfinalized proof components.

   2. The verifier for each R_i looks a little different depending on the complexity of the "step"
      circuit corresponding to R_i has. Namely, it is dependent on the "domains" H and K for this
      circuit.

      So, when the "wrap" circuit proves the statement,
      "there exists some index i in 1,...,n and a proof P such that verifies(P)"
      "verifies(P)" must also take the index "i", compute the correct domain sizes correspond to rule "i"
      and use *that* in the "verifies" computation.
*)

let pad_local_max_branchings
    (type prev_varss prev_valuess env max_branching branches)
    (max_branching : max_branching Nat.t)
    (length : (prev_varss, branches) Hlist.Length.t)
    (local_max_branchings :
      (prev_varss, prev_valuess, env) H2_1.T(H2_1.T(E03(Int))).t) :
    ((int, max_branching) Vector.t, branches) Vector.t =
  let module Vec = struct
    type t = (int, max_branching) Vector.t
  end in
  let module M =
    H2_1.Map
      (H2_1.T
         (E03
            (Int)))
            (E03 (Vec))
            (struct
              module HI = H2_1.T (E03 (Int))

              let f : type a b e. (a, b, e) H2_1.T(E03(Int)).t -> Vec.t =
               fun xs ->
                let (T (branching, pi)) = HI.length xs in
                let module V = H2_1.To_vector (Int) in
                let v = V.f pi xs in
                Vector.extend_exn v max_branching 0
            end)
  in
  let module V = H2_1.To_vector (Vec) in
  V.f length (M.f local_max_branchings)

open Zexe_backend

module Me_only = struct
  module Dlog_based = Types.Dlog_based.Proof_state.Me_only
  module Pairing_based = Types.Pairing_based.Proof_state.Me_only
end

module Proof_ = P.Base
module Proof = P

module Statement_with_proof = struct
  type ('s, 'max_width, _) t =
    (* TODO: use Max local max branching instead of max_width *)
    's * ('max_width, 'max_width) Proof.t
end

let pad_pass_throughs
    (type local_max_branchings max_local_max_branchings max_branching)
    (module M : Hlist.Maxes.S
      with type ns = max_local_max_branchings
       and type length = max_branching)
    (pass_throughs : local_max_branchings H1.T(Proof_.Me_only.Dlog_based).t) =
  let dummy_chals = Dummy.Ipa.Wrap.challenges in
  let rec go : type len ms ns.
         ms H1.T(Nat).t
      -> ns H1.T(Proof_.Me_only.Dlog_based).t
      -> ms H1.T(Proof_.Me_only.Dlog_based).t =
   fun maxes me_onlys ->
    match (maxes, me_onlys) with
    | [], _ :: _ ->
        assert false
    | [], [] ->
        []
    | m :: maxes, [] ->
        { sg= Lazy.force Dummy.Ipa.Step.sg
        ; old_bulletproof_challenges= Vector.init m ~f:(fun _ -> dummy_chals)
        }
        :: go maxes []
    | m :: maxes, me_only :: me_onlys ->
        let me_only =
          { me_only with
            old_bulletproof_challenges=
              Vector.extend_exn me_only.old_bulletproof_challenges m
                dummy_chals }
        in
        me_only :: go maxes me_onlys
  in
  go M.maxes pass_throughs

module Verification_key = struct
  include Verification_key

  module Id = struct
    include Cache.Wrap.Key.Verification

    let dummy_id = Type_equal.Id.(uid (create ~name:"dummy" sexp_of_opaque))

    let dummy : unit -> t =
      let header =
        { Snark_keys_header.header_version= Snark_keys_header.header_version
        ; kind= {type_= "verification key"; identifier= "dummy"}
        ; constraint_constants=
            { sub_windows_per_window= 0
            ; ledger_depth= 0
            ; work_delay= 0
            ; block_window_duration_ms= 0
            ; transaction_capacity= Log_2 0
            ; pending_coinbase_depth= 0
            ; coinbase_amount= Unsigned.UInt64.of_int 0
            ; supercharged_coinbase_factor= 0
            ; account_creation_fee= Unsigned.UInt64.of_int 0
            ; fork= None }
        ; commits= {mina= ""; marlin= ""}
        ; length= 0
        ; commit_date= ""
        ; constraint_system_hash= ""
        ; identifying_hash= "" }
      in
      let t = lazy (dummy_id, header, Md5.digest_string "") in
      fun () -> Lazy.force t
  end

  (* TODO: Make async *)
  let load ~cache id =
    Key_cache.Sync.read cache
      (Key_cache.Sync.Disk_storable.of_binable Id.to_string
         (module Verification_key.Stable.Latest))
      id
    |> Async.return
end

module type Proof_intf = sig
  type statement

  type t

  val verification_key : Verification_key.t Lazy.t

  val id : Verification_key.Id.t Lazy.t

  val verify : (statement * t) list -> bool Async.Deferred.t
end

module Prover = struct
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

module Proof_system = struct
  type ( 'a_var
       , 'a_value
       , 'max_branching
       , 'branches
       , 'prev_valuess
       , 'widthss
       , 'heightss )
       t =
    | T :
        ('a_var, 'a_value, 'max_branching, 'branches) Tag.t
        * (module Proof_intf with type t = 'proof
                              and type statement = 'a_value)
        * ( 'prev_valuess
          , 'widthss
          , 'heightss
          , 'a_value
          , 'proof )
          H3_2.T(Prover).t
        -> ( 'a_var
           , 'a_value
           , 'max_branching
           , 'branches
           , 'prev_valuess
           , 'widthss
           , 'heightss )
           t
end

module Debug = struct
  let log_step main typ name index =
    let module Constraints = Snarky_log.Constraints (Impls.Step.Internal_Basic) in
    let log =
      let weight =
        let sys = Backend.Tick.R1CS_constraint_system.create () in
        fun (c : Impls.Step.Constraint.t) ->
          let prev = sys.next_row in
          List.iter c ~f:(fun {annotation; basic} ->
              Backend.Tick.R1CS_constraint_system.add_constraint sys
                ?label:annotation basic ) ;
          let next = sys.next_row in
          next - prev
      in
      Constraints.log ~weight
        Impls.Step.(
          make_checked (fun () ->
              ( let x = with_label __LOC__ (fun () -> exists typ) in
                main x ()
                : unit ) ))
    in
    Snarky_log.to_file
      (sprintf "step-snark-%s-%d.json" name (Index.to_int index))
      log

  let log_wrap main typ name id =
    let module Constraints = Snarky_log.Constraints (Impls.Wrap.Internal_Basic) in
    let log =
      let sys = Backend.Tock.R1CS_constraint_system.create () in
      let weight (c : Impls.Wrap.Constraint.t) =
        let prev = sys.next_row in
        List.iter c ~f:(fun {annotation; basic} ->
            Backend.Tock.R1CS_constraint_system.add_constraint sys
              ?label:annotation basic ) ;
        let next = sys.next_row in
        next - prev
      in
      let log =
        Constraints.log ~weight
          Impls.Wrap.(
            make_checked (fun () ->
                ( let x = with_label __LOC__ (fun () -> exists typ) in
                  main x ()
                  : unit ) ))
      in
      log
    in
    Snarky_log.to_file
      (sprintf
         !"wrap-%s-%{sexp:Type_equal.Id.Uid.t}.json"
         name (Type_equal.Id.uid id))
      log
end

module type Inputs = sig
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
end

module Make (Inputs : Inputs) = struct
  open Inputs

  let self =
    match self with
    | `New ->
        {Tag.id= Type_equal.Id.create ~name sexp_of_opaque; kind= Compiled}
    | `Existing self ->
        self

  module IR = Inductive_rule.T (A) (A_value)
  module HIR = H4.T (IR)

  let rec conv_irs : type v1ss v2ss wss hss.
         (v1ss, v2ss, wss, hss, A.t, A_value.t) H4_2.T(Inductive_rule).t
      -> (v1ss, v2ss, wss, hss) H4.T(IR).t = function
    | [] ->
        []
    | r :: rs ->
        r :: conv_irs rs

  let choices = conv_irs (choices ~self)

  let snark_keys_header kind constraint_system_hash =
    { Snark_keys_header.header_version= Snark_keys_header.header_version
    ; kind
    ; constraint_constants
    ; commits=
        {mina= Mina_version.commit_id; marlin= Mina_version.marlin_commit_id}
    ; length= (* This is a dummy, it gets filled in on read/write. *) 0
    ; commit_date= Mina_version.commit_date
    ; constraint_system_hash
    ; identifying_hash=
        (* TODO: Proper identifying hash. *)
        constraint_system_hash }

  let check_snark_keys_header kind (header : Snark_keys_header.t) =
    let open Or_error.Let_syntax in
    let%bind () =
      if Int.equal header.header_version Snark_keys_header.header_version then
        return ()
      else Or_error.errorf "Snark key header version mismatch"
    in
    let%bind () =
      if Snark_keys_header.Kind.equal header.kind kind then return ()
      else
        Or_error.tag_arg (Or_error.errorf "Snark key kind mismatch")
          "kind" (header.kind, kind) (fun (got, expected) ->
            Sexp.List
              [ List [Atom "got"; Snark_keys_header.Kind.sexp_of_t got]
              ; List
                  [Atom "expected"; Snark_keys_header.Kind.sexp_of_t expected]
              ] )
    in
    let%bind () =
      if
        Snark_keys_header.Constraint_constants.equal
          header.constraint_constants constraint_constants
      then return ()
      else
        Or_error.tag_arg
          (Or_error.errorf "Snark key header constraint constants do not match")
          "constraint constants"
          (header.constraint_constants, constraint_constants)
          (fun (got, expected) ->
            Sexp.List
              [ List
                  [ Atom "got"
                  ; Snark_keys_header.Constraint_constants.sexp_of_t got ]
              ; List
                  [ Atom "expected"
                  ; Snark_keys_header.Constraint_constants.sexp_of_t expected
                  ] ] )
    in
    (* TODO: Check identifying hash. *)
    return ()

  let check_constraint_system_hash ~got ~expected =
    if String.equal got expected then Or_error.return ()
    else
      Or_error.tag_arg
        (Or_error.errorf
           "Snark key header constraint system hashes do not match")
        "constraint system hash" (got, expected) (fun (got, expected) ->
          Sexp.List
            [List [Atom "got"; Atom got]; List [Atom "expected"; Atom expected]]
      )

  let max_local_max_branchings (type n)
      (module Max_branching : Nat.Intf with type n = n) branches choices =
    let module Local_max_branchings = struct
      type t = (int, Max_branching.n) Vector.t
    end in
    let module M =
      H4.Map
        (IR)
        (E04 (Local_max_branchings))
        (struct
          module V = H4.To_vector (Int)
          module HT = H4.T (Tag)

          module M =
            H4.Map
              (Tag)
              (E04 (Int))
              (struct
                let f (type a b c d) (t : (a, b, c, d) Tag.t) : int =
                  if Type_equal.Id.same t.id self.id then
                    Nat.to_int Max_branching.n
                  else
                    let (module M) = Types_map.max_branching t in
                    Nat.to_int M.n
              end)

          let f : type a b c d. (a, b, c, d) IR.t -> Local_max_branchings.t =
           fun rule ->
            let (T (_, l)) = HT.length rule.prevs in
            Vector.extend_exn (V.f l (M.f rule.prevs)) Max_branching.n 0
        end)
    in
    let module V = H4.To_vector (Local_max_branchings) in
    let padded = V.f branches (M.f choices) |> Vector.transpose in
    (padded, Maxes.m padded)

  let choices_length = HIR.length choices

  let ( (prev_varss_n : Branches.n Nat.t)
      , (prev_varss_length : (prev_varss, Branches.n) Length.t) ) =
    let (T (prev_varss_n, prev_varss_length)) = HIR.length choices in
    let T = Nat.eq_exn prev_varss_n Branches.n in
    (prev_varss_n, prev_varss_length)

  let () = Timer.start __LOC__

  (* This abstract type serves an unfortunate purpose: we know that the type
     derived for the module [Maxes] below is depedent on the values passed to
     this functor, but OCaml has no sense of value-dependent types.

     We prefer this to a generative functor because it allows our types to
     alias between different 'instances' of this functor, giving us far more
     flexibility in where and how we call it, particularly when loading keys.
  *)
  type maxes_ns

  let full_signature =
    let T = Max_branching.eq in
    let padded, (maxes : (module Maxes.S with type length = Max_branching.n)) =
      max_local_max_branchings (module Max_branching) prev_varss_length choices
    in
    (* This coercion allows the [Maxes.ns] type to escape as [maxes_ns]. *)
    let (maxes
          : (module Maxes.S
               with type length = Max_branching.n
                and type ns = maxes_ns)) =
      Obj.magic maxes
    in
    Timer.clock __LOC__ ;
    {Full_signature.padded; maxes}

  module Maxes = (val full_signature.maxes)

  let wrap_domains =
    let module M = Wrap_domains.Make (A) (A_value) in
    let rec f : type a b c d.
        (a, b, c, d) H4.T(IR).t -> (a, b, c, d) H4.T(M.I).t = function
      | [] ->
          []
      | x :: xs ->
          x :: f xs
    in
    let res =
      M.f full_signature prev_varss_n prev_varss_length ~self
        ~choices:(f choices)
        ~max_branching:(module Max_branching)
    in
    Timer.clock __LOC__ ; res

  let step_widths =
    let module M =
      H4.Map
        (IR)
        (E04 (Int))
        (struct
          module M = H4.T (Tag)

          let f : type a b c d. (a, b, c, d) IR.t -> int =
           fun r ->
            let (T (n, _)) = M.length r.prevs in
            Nat.to_int n
        end)
    in
    let module V = H4.To_vector (Int) in
    let res = V.f prev_varss_length (M.f choices) in
    Timer.clock __LOC__ ; res

  module Branch_data = struct
    type ('vars, 'vals, 'n, 'm) t =
      ( A.t
      , A_value.t
      , Max_branching.n
      , Branches.n
      , 'vars
      , 'vals
      , 'n
      , 'm )
      Step_branch_data.t
  end

  let step_data =
    let T = Max_branching.eq in
    let i = ref 0 in
    Timer.clock __LOC__ ;
    let module M =
      H4.Map (IR) (Branch_data)
        (struct
          let f : type a b c d. (a, b, c, d) IR.t -> (a, b, c, d) Branch_data.t
              =
           fun rule ->
            Timer.clock __LOC__ ;
            let res =
              Common.time "make step data" (fun () ->
                  Step_branch_data.create ~index:(Index.of_int_exn !i)
                    ~max_branching:Max_branching.n ~branches:Branches.n ~self
                    ~typ A.to_field_elements A_value.to_field_elements rule
                    ~wrap_domains ~branchings:step_widths )
            in
            Timer.clock __LOC__ ; incr i ; res
        end)
    in
    M.f choices

  let step_domains =
    let module M =
      H4.Map
        (Branch_data)
        (E04 (Domains))
        (struct
          let f (T b : _ Branch_data.t) = b.domains
        end)
    in
    let module V = H4.To_vector (Domains) in
    let res = V.f prev_varss_length (M.f step_data) in
    Timer.clock __LOC__ ; res

  module type Step_keys = sig
    type prev_vars

    type prev_values

    type widths

    type heights

    val branch_data : (prev_vars, prev_values, widths, heights) Branch_data.t

    val constraint_system : Tick.R1CS_constraint_system.t Lazy.t

    val constraint_system_digest : Md5.t Lazy.t

    val constraint_system_hash : string Lazy.t

    module Keys : sig
      module Proving : sig
        type t = private Tick.Proving_key.t

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

        val of_raw_key : Tick.Proving_key.t -> t
      end

      module Verification : sig
        type t = private Tick.Verification_key.t

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

        val of_raw_key : Tick.Verification_key.t -> t
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

  module Step_keys_m = struct
    type ('prev_vars, 'prev_values, 'widths, 'heights) t =
      (module Step_keys
         with type prev_vars = 'prev_vars
          and type prev_values = 'prev_values
          and type widths = 'widths
          and type heights = 'heights)
  end

  let steps_keys =
    let T = Max_branching.eq in
    let module M =
      H4.Map (Branch_data) (Step_keys_m)
        (struct
          let etyp =
            Impls.Step.input ~branching:Max_branching.n
              ~wrap_rounds:Tock.Rounds.n

          let f (type prev_vars prev_values widths heights)
              (T b as data :
                (prev_vars, prev_values, widths, heights) Branch_data.t) :
              (prev_vars, prev_values, widths, heights) Step_keys_m.t =
            let (T (typ, conv)) = etyp in
            let main x () : unit =
              b.main
                (Impls.Step.with_label "conv" (fun () -> conv x))
                ~step_domains
            in
            let () = if debug then Debug.log_step main typ name b.index in
            let open Impls.Step in
            ( module struct
              type nonrec prev_vars = prev_vars

              type nonrec prev_values = prev_values

              type nonrec widths = widths

              type nonrec heights = heights

              let branch_data = data

              let rule = b.rule

              let constraint_system =
                lazy (Impls.Step.constraint_system ~exposing:[typ] main)

              let constraint_system_digest =
                Lazy.map ~f:R1CS_constraint_system.digest constraint_system

              let constraint_system_hash =
                Lazy.map ~f:Md5.to_hex constraint_system_digest

              module Keys = struct
                module Proving = struct
                  type t = Tick.Proving_key.t

                  let kind =
                    { Snark_keys_header.Kind.type_= "step-proving-key"
                    ; identifier= name ^ "-" ^ b.rule.identifier }

                  let header_template =
                    Lazy.map constraint_system_hash ~f:(fun cs_hash ->
                        snark_keys_header kind cs_hash )

                  let cache_key =
                    let%map.Lazy.Let_syntax cs = constraint_system
                    and header = header_template in
                    ( Type_equal.Id.uid self.id
                    , header
                    , Index.to_int b.index
                    , cs )

                  let check_header path =
                    let open Or_error.Let_syntax in
                    let%bind header, () =
                      Snark_keys_header.read_with_header
                        ~read_data:(fun ~offset:_ _ -> ())
                        path
                    in
                    let%bind () = check_snark_keys_header kind header in
                    let%map () =
                      (* TODO: Remove this when identifying hashes are
                         implemented.
                      *)
                      check_constraint_system_hash
                        ~got:header.constraint_system_hash
                        ~expected:(Lazy.force constraint_system_hash)
                    in
                    header

                  let read_with_header path =
                    let open Or_error.Let_syntax in
                    let%bind header, key =
                      Snark_keys_header.read_with_header
                        ~read_data:(fun ~offset ->
                          Marlin_plonk_bindings.Pasta_fp_index.read ~offset
                            (Backend.Tick.Keypair.load_urs ()) )
                        path
                    in
                    let%bind () = check_snark_keys_header kind header in
                    let%map () =
                      (* TODO: Remove this when identifying hashes are
                         implemented.
                      *)
                      check_constraint_system_hash
                        ~got:header.constraint_system_hash
                        ~expected:(Lazy.force constraint_system_hash)
                    in
                    ( header
                    , { Backend.Tick.Keypair.index= key
                      ; cs= Lazy.force constraint_system } )

                  let write_with_header path t =
                    Or_error.try_with (fun () ->
                        Snark_keys_header.write_with_header
                          ~expected_max_size_log2:
                            33 (* 8 GB should be enough *)
                          ~append_data:
                            (Marlin_plonk_bindings.Pasta_fp_index.write
                               ~append:true t.Backend.Tick.Keypair.index)
                          (Lazy.force header_template)
                          path )

                  let registered_key = Set_once.create ()

                  let registered_key_lazy =
                    lazy
                      ( match Set_once.get registered_key with
                      | Some key ->
                          Lazy.force key
                      | None ->
                          failwithf
                            "Step proving key for system %s, rule %s was not \
                             registered before use"
                            name b.rule.identifier () )

                  let of_raw_key = Fn.id
                end

                module Verification = struct
                  type t = Tick.Verification_key.t

                  let kind =
                    { Snark_keys_header.Kind.type_= "step-verification-key"
                    ; identifier= name ^ "-" ^ b.rule.identifier }

                  let header_template =
                    Lazy.map constraint_system_hash ~f:(fun cs_hash ->
                        snark_keys_header kind cs_hash )

                  let cache_key : Cache.Step.Key.Verification.t Lazy.t =
                    let%map.Lazy.Let_syntax cs_hash = constraint_system_digest
                    and header = header_template in
                    ( Type_equal.Id.uid self.id
                    , header
                    , Index.to_int b.index
                    , cs_hash )

                  let check_header path =
                    let open Or_error.Let_syntax in
                    let%bind header, () =
                      Snark_keys_header.read_with_header
                        ~read_data:(fun ~offset:_ _ -> ())
                        path
                    in
                    let%bind () = check_snark_keys_header kind header in
                    let%map () =
                      (* TODO: Remove this when identifying hashes are
                         implemented.
                      *)
                      check_constraint_system_hash
                        ~got:header.constraint_system_hash
                        ~expected:(Lazy.force constraint_system_hash)
                    in
                    header

                  let read_with_header path =
                    let open Or_error.Let_syntax in
                    let%bind header, key =
                      Snark_keys_header.read_with_header
                        ~read_data:(fun ~offset path ->
                          Marlin_plonk_bindings.Pasta_fp_verifier_index.read
                            ~offset
                            (Backend.Tick.Keypair.load_urs ())
                            path )
                        path
                    in
                    let%bind () = check_snark_keys_header kind header in
                    let%map () =
                      (* TODO: Remove this when identifying hashes are
                         implemented.
                      *)
                      check_constraint_system_hash
                        ~got:header.constraint_system_hash
                        ~expected:(Lazy.force constraint_system_hash)
                    in
                    (header, key)

                  let write_with_header path x =
                    Or_error.try_with (fun () ->
                        Snark_keys_header.write_with_header
                          ~expected_max_size_log2:
                            33 (* 8 GB should be enough *)
                          ~append_data:
                            (Marlin_plonk_bindings.Pasta_fp_verifier_index
                             .write ~append:true x)
                          (Lazy.force header_template)
                          path )

                  let registered_key = Set_once.create ()

                  let registered_key_lazy =
                    lazy
                      ( match Set_once.get registered_key with
                      | Some key ->
                          Lazy.force key
                      | None ->
                          failwithf
                            "Step proving key for system %s, rule %s was not \
                             registered before use"
                            name b.rule.identifier () )

                  let of_raw_key = Fn.id
                end

                let generate () =
                  Common.time "stepkeygen" (fun () ->
                      let kp = generate_keypair ~exposing:[typ] main in
                      (Keypair.pk kp, Keypair.vk kp) )

                let read_or_generate_from_cache :
                       Key_cache.Spec.t list
                    -> (Proving.t * Dirty.t) Lazy.t
                       * (Verification.t * Dirty.t) Lazy.t =
                  Memo.of_comparable
                    (module Key_cache.Spec.List)
                    (fun cache ->
                      Common.time "step read or generate" (fun () ->
                          let kp_cache, vk_cache =
                            Cache.Step.read_or_generate cache Proving.cache_key
                              Verification.cache_key typ main
                          in
                          let pk_cache =
                            let%map.Lazy.Let_syntax kp, dirty = kp_cache in
                            (Keypair.pk kp, dirty)
                          in
                          (pk_cache, vk_cache) ) )

                let use_key_cache cache =
                  let pk_cache, vk_cache = read_or_generate_from_cache cache in
                  Set_once.set_exn Proving.registered_key [%here]
                    (Lazy.map ~f:fst pk_cache) ;
                  Set_once.set_exn Verification.registered_key [%here]
                    (Lazy.map ~f:fst vk_cache)
              end
            end )
        end)
    in
    M.f step_data

  let step_vk_commitments =
    let module Vk = struct
      type t =
        Tick.Curve.Affine.t Dlog_plonk_types.Poly_comm.Without_degree_bound.t
        Plonk_verification_key_evals.t
    end in
    let module M =
      H4.Map
        (Step_keys_m)
        (E04 (Vk))
        (struct
          let f (type prev_vars prev_values widths heights)
              (( module
                Step ) :
                (prev_vars, prev_values, widths, heights) Step_keys_m.t) =
            Tick.Keypair.vk_commitments
              (Lazy.force
                 ( Step.Keys.Verification.registered_key_lazy
                   :> Impls.Step.Verification_key.t Lazy.t ))
        end)
    in
    let module V = H4.To_vector (Vk) in
    lazy (V.f prev_varss_length (M.f steps_keys))

  module Wrap_keys = struct
    let requests, main =
      Timer.clock __LOC__ ;
      let prev_wrap_domains =
        let module M =
          H4.Map
            (IR)
            (H4.T
               (E04 (Domains)))
               (struct
                 let f : type a b c d.
                     (a, b, c, d) IR.t -> (a, b, c, d) H4.T(E04(Domains)).t =
                  fun rule ->
                   let module M =
                     H4.Map
                       (Tag)
                       (E04 (Domains))
                       (struct
                         let f (type a b c d) (t : (a, b, c, d) Tag.t) :
                             Domains.t =
                           Types_map.lookup_map t ~self:self.id
                             ~default:wrap_domains ~f:(function
                             | `Compiled d ->
                                 d.wrap_domains
                             | `Side_loaded _ ->
                                 Common.wrap_domains )
                       end)
                   in
                   M.f rule.Inductive_rule.prevs
               end)
        in
        M.f choices
      in
      Timer.clock __LOC__ ;
      Wrap_main.wrap_main full_signature prev_varss_length step_vk_commitments
        step_widths step_domains prev_wrap_domains
        (module Max_branching)

    let constraint_system =
      let open Impls.Wrap in
      lazy
        (let (T (typ, conv)) = input () in
         let main x () : unit = main (conv x) in
         let () = if debug then Debug.log_wrap main typ name self.id in
         constraint_system ~exposing:[typ] main)

    let constraint_system_digest =
      Lazy.map ~f:Impls.Wrap.R1CS_constraint_system.digest constraint_system

    let constraint_system_hash =
      Lazy.map ~f:Md5.to_hex constraint_system_digest

    module Keys = struct
      module Proving = struct
        type t = Tock.Proving_key.t

        let kind =
          {Snark_keys_header.Kind.type_= "wrap-proving-key"; identifier= name}

        let header_template =
          Lazy.map constraint_system_hash ~f:(fun cs_hash ->
              snark_keys_header kind cs_hash )

        let cache_key =
          let%map.Lazy.Let_syntax cs = constraint_system
          and header = header_template in
          (Type_equal.Id.uid self.id, header, cs)

        let check_header path =
          let open Or_error.Let_syntax in
          let%bind header, () =
            Snark_keys_header.read_with_header
              ~read_data:(fun ~offset:_ _ -> ())
              path
          in
          let%bind () = check_snark_keys_header kind header in
          let%map () =
            (* TODO: Remove this when identifying hashes are
               implemented.
            *)
            check_constraint_system_hash ~got:header.constraint_system_hash
              ~expected:(Lazy.force constraint_system_hash)
          in
          header

        let read_with_header path =
          let open Or_error.Let_syntax in
          let%bind header, key =
            Snark_keys_header.read_with_header
              ~read_data:(fun ~offset ->
                Marlin_plonk_bindings.Pasta_fq_index.read ~offset
                  (Backend.Tock.Keypair.load_urs ()) )
              path
          in
          let%bind () = check_snark_keys_header kind header in
          let%map () =
            (* TODO: Remove this when identifying hashes are
               implemented.
            *)
            check_constraint_system_hash ~got:header.constraint_system_hash
              ~expected:(Lazy.force constraint_system_hash)
          in
          ( header
          , {Backend.Tock.Keypair.index= key; cs= Lazy.force constraint_system}
          )

        let write_with_header path t =
          Or_error.try_with (fun () ->
              Snark_keys_header.write_with_header
                ~expected_max_size_log2:33 (* 8 GB should be enough *)
                ~append_data:
                  (Marlin_plonk_bindings.Pasta_fq_index.write ~append:true
                     t.Backend.Tock.Keypair.index)
                (Lazy.force header_template)
                path )

        let registered_key = Set_once.create ()

        let registered_key_lazy =
          lazy
            ( match Set_once.get registered_key with
            | Some key ->
                Lazy.force key
            | None ->
                failwithf
                  "Wrap proving key for system %s was not registered before use"
                  name () )

        let of_raw_key = Fn.id
      end

      module Verification = struct
        type t = Verification_key.t

        let kind =
          { Snark_keys_header.Kind.type_= "wrap-verification-key"
          ; identifier= name }

        let header_template =
          Lazy.map constraint_system_hash ~f:(fun cs_hash ->
              snark_keys_header kind cs_hash )

        let cache_key =
          let%map.Lazy.Let_syntax cs_hash = constraint_system_digest
          and header = header_template in
          (Type_equal.Id.uid self.id, header, cs_hash)

        let check_header path =
          let open Or_error.Let_syntax in
          let%bind header, () =
            Snark_keys_header.read_with_header
              ~read_data:(fun ~offset:_ _ -> ())
              path
          in
          let%bind () = check_snark_keys_header kind header in
          let%map () =
            (* TODO: Remove this when identifying hashes are
               implemented.
            *)
            check_constraint_system_hash ~got:header.constraint_system_hash
              ~expected:(Lazy.force constraint_system_hash)
          in
          header

        let read_with_header path =
          let open Or_error.Let_syntax in
          let%bind header, key =
            Snark_keys_header.read_with_header
              ~read_data:(fun ~offset path ->
                In_channel.read_all path
                |> Bigstring.of_string ~pos:offset
                |> Verification_key.Stable.Latest.bin_read_t ~pos_ref:(ref 0)
                )
              path
          in
          let%bind () = check_snark_keys_header kind header in
          let%map () =
            (* TODO: Remove this when identifying hashes are
               implemented.
            *)
            check_constraint_system_hash ~got:header.constraint_system_hash
              ~expected:(Lazy.force constraint_system_hash)
          in
          (header, key)

        let write_with_header path x =
          Or_error.try_with (fun () ->
              Snark_keys_header.write_with_header
                ~expected_max_size_log2:33 (* 8 GB should be enough *)
                ~append_data:(fun path ->
                  Out_channel.with_file ~append:true path ~f:(fun file ->
                      Out_channel.output_string file
                        (Binable.to_string
                           (module Verification_key.Stable.Latest)
                           x) ) )
                (Lazy.force header_template)
                path )

        let registered_key = Set_once.create ()

        let registered_key_lazy =
          lazy
            ( match Set_once.get registered_key with
            | Some key ->
                Lazy.force key
            | None ->
                failwithf
                  "Wrap verification key for system %s was not registered \
                   before use"
                  name () )

        let of_raw_key = Fn.id
      end

      let generate () =
        Common.time "wrapkeygen" (fun () ->
            let module Vk = Verification_key in
            let open Impls.Wrap in
            let (T (typ, conv)) = input () in
            let main x () : unit = main (conv x) in
            let kp = generate_keypair ~exposing:[typ] main in
            let pk = Keypair.pk kp in
            let vk = Keypair.vk kp in
            let vk : Vk.t =
              { index= vk
              ; commitments=
                  Pickles_types.Plonk_verification_key_evals.map vk.evals
                    ~f:(fun x ->
                      Array.map x.unshifted ~f:(function
                        | Infinity ->
                            failwith "Unexpected zero curve point"
                        | Finite x ->
                            x ) )
              ; step_domains= Vector.to_array step_domains
              ; data=
                  (let open Marlin_plonk_bindings.Pasta_fq_index in
                  {constraints= domain_d1_size pk.index}) }
            in
            (pk, vk) )

      let read_or_generate_from_cache :
             Key_cache.Spec.t list
          -> (Proving.t * Dirty.t) Lazy.t * (Verification.t * Dirty.t) Lazy.t =
        Memo.of_comparable
          (module Key_cache.Spec.List)
          (fun cache ->
            Common.time "wrap read or generate" (fun () ->
                let open Impls.Wrap in
                let (T (typ, conv)) = input () in
                let main x () : unit = main (conv x) in
                let kp_cache, vk_cache =
                  Cache.Wrap.read_or_generate
                    (Vector.to_array step_domains)
                    cache Proving.cache_key Verification.cache_key typ main
                in
                let pk_cache =
                  let%map.Lazy.Let_syntax kp, dirty = kp_cache in
                  (Keypair.pk kp, dirty)
                in
                (pk_cache, vk_cache) ) )

      let use_key_cache cache =
        let pk_cache, vk_cache = read_or_generate_from_cache cache in
        Set_once.set_exn Proving.registered_key [%here]
          (Lazy.map ~f:fst pk_cache) ;
        Set_once.set_exn Verification.registered_key [%here]
          (Lazy.map ~f:fst vk_cache)
    end
  end

  module type Steps = sig
    include Step_keys

    val prove :
         ?handler:(   Snarky_backendless.Request.request
                   -> Snarky_backendless.Request.response)
      -> (prev_values, widths, heights) H3.T(Statement_with_proof).t
      -> A_value.t
      -> (Max_branching.n, Max_branching.n) Proof.t Async.Deferred.t
  end

  module Steps_m = struct
    type ('prev_vars, 'prev_values, 'widths, 'heights) t =
      (module Steps
         with type prev_vars = 'prev_vars
          and type prev_values = 'prev_values
          and type widths = 'widths
          and type heights = 'heights)
  end

  let steps =
    let T = Max_branching.eq in
    let module S = Step.Make (A) (A_value) (Max_branching) in
    let module M =
      H4.Map (Step_keys_m) (Steps_m)
        (struct
          let f (type prev_vars prev_values widths heights)
              (( module
                Step ) :
                (prev_vars, prev_values, widths, heights) Step_keys_m.t) :
              (prev_vars, prev_values, widths, heights) Steps_m.t =
            ( module struct
              include Step

              let prove =
                let wrap_vk =
                  Wrap_keys.Keys.Verification.registered_key_lazy
                in
                let (T b as branch_data) = Step.branch_data in
                let step_pk = Step.Keys.Proving.registered_key_lazy in
                let step_vk = Step.Keys.Verification.registered_key_lazy in
                let step_pk = (step_pk :> Impls.Step.Proving_key.t Lazy.t) in
                let step_vk =
                  (step_vk :> Impls.Step.Verification_key.t Lazy.t)
                in
                let (module Requests) = b.requests in
                let _, prev_vars_length = b.branching in
                let wrap ?handler prevs next_state =
                  let pairing_vk = Lazy.force step_vk in
                  let wrap_vk = Lazy.force wrap_vk in
                  let wrap_pk = Wrap_keys.Keys.Proving.registered_key_lazy in
                  let prevs =
                    let module M =
                      H3.Map (Statement_with_proof) (P.With_data)
                        (struct
                          let f
                              ((app_state, T proof) : _ Statement_with_proof.t)
                              =
                            P.T
                              { proof with
                                statement=
                                  { proof.statement with
                                    pass_through=
                                      { proof.statement.pass_through with
                                        app_state } } }
                        end)
                    in
                    M.f prevs
                  in
                  let%bind.Async proof =
                    S.f ?handler branch_data next_state
                      ~prevs_length:prev_vars_length ~self ~step_domains
                      ~self_dlog_plonk_index:wrap_vk.commitments
                      ~maxes:(module Maxes)
                      (Lazy.force step_pk) wrap_vk.index prevs
                  in
                  let proof =
                    { proof with
                      statement=
                        { proof.statement with
                          pass_through=
                            pad_pass_throughs
                              (module Maxes)
                              proof.statement.pass_through } }
                  in
                  let%map.Async proof =
                    Wrap.wrap ~max_branching:Max_branching.n
                      full_signature.maxes Wrap_keys.requests
                      ~dlog_plonk_index:wrap_vk.commitments Wrap_keys.main
                      A_value.to_field_elements ~pairing_vk
                      ~step_domains:b.domains
                      ~pairing_plonk_indices:step_vk_commitments ~wrap_domains
                      (Lazy.force wrap_pk) proof
                  in
                  Proof.T
                    { proof with
                      statement=
                        { proof.statement with
                          pass_through=
                            {proof.statement.pass_through with app_state= ()}
                        } }
                in
                wrap
            end )
        end)
    in
    M.f steps_keys

  let data : _ Types_map.Compiled.t =
    let wrap_vk = Wrap_keys.Keys.Verification.registered_key_lazy in
    { branches= Branches.n
    ; branchings= step_widths
    ; max_branching= (module Max_branching)
    ; typ
    ; value_to_field_elements= A_value.to_field_elements
    ; var_to_field_elements= A.to_field_elements
    ; wrap_key= Lazy.map wrap_vk ~f:Verification_key.commitments
    ; wrap_vk= Lazy.map wrap_vk ~f:Verification_key.index
    ; wrap_domains
    ; step_domains }

  let register () = Types_map.add_exn self data

  module P = Proof

  module Proof = ( val let T = Max_branching.eq in
                       ( module struct
                         type statement = A_value.t

                         module Max_local_max_branching = Max_branching
                         module Max_branching_vec = Nvector (Max_branching)
                         include Proof.Make
                                   (Max_branching)
                                   (Max_local_max_branching)

                         let verification_key =
                           Wrap_keys.Keys.Verification.registered_key_lazy

                         let id = Wrap_keys.Keys.Verification.cache_key

                         let verify ts =
                           verify
                             (module Max_branching)
                             (module A_value)
                             (Lazy.force verification_key)
                             ts

                         let statement (T p : t) =
                           p.statement.pass_through.app_state
                       end )
                     : Proof_intf
                     with type t = (Max_branching.n, Max_branching.n) Proof.t
                      and type statement = A_value.t )

  let verify = Proof.verify

  let compile :
         cache:Key_cache.Spec.t list
      -> unit
      -> ( prev_valuess
         , widthss
         , heightss
         , A_value.t
         , (Max_branching.n, Max_branching.n) P.t Async.Deferred.t )
         H3_2.T(Prover).t
         * _
         * _
         * _ =
   fun ~cache () ->
    let T = Max_branching.eq in
    let cache_handle = ref (Lazy.return `Cache_hit) in
    let accum_dirty t = cache_handle := Cache_handle.(!cache_handle + t) in
    Timer.clock __LOC__ ;
    let () =
      let module M =
        H4.Iter
          (Step_keys_m)
          (struct
            let f (type prev_vars prev_values widths heights)
                (( module
                  Step ) :
                  (prev_vars, prev_values, widths, heights) Step_keys_m.t) =
              Step.Keys.use_key_cache cache ;
              let pk_cache, vk_cache =
                Step.Keys.read_or_generate_from_cache cache
              in
              accum_dirty (Lazy.map pk_cache ~f:snd) ;
              accum_dirty (Lazy.map vk_cache ~f:snd)
          end)
      in
      M.f steps_keys
    in
    Timer.clock __LOC__ ;
    let disk_key =
      let open Impls.Wrap in
      let (T (typ, conv)) = input () in
      let main x () : unit = Wrap_keys.main (conv x) in
      let disk_key_prover = Wrap_keys.Keys.Proving.cache_key in
      let disk_key_verifier = Wrap_keys.Keys.Verification.cache_key in
      Wrap_keys.Keys.use_key_cache cache ;
      let wrap_pk, wrap_vk =
        Common.time "wrap read or generate " (fun () ->
            Cache.Wrap.read_or_generate
              (Vector.to_array step_domains)
              cache disk_key_prover disk_key_verifier typ main )
      in
      Timer.clock __LOC__ ;
      accum_dirty (Lazy.map wrap_pk ~f:snd) ;
      accum_dirty (Lazy.map wrap_vk ~f:snd) ;
      disk_key_verifier
    in
    let module S = Step.Make (A) (A_value) (Max_branching) in
    let wrap_vk = Wrap_keys.Keys.Verification.registered_key_lazy in
    let provers =
      let rec go : type xs1 xs2 xs3 xs4.
             (xs1, xs2, xs3, xs4) H4.T(Steps_m).t
          -> ( xs2
             , xs3
             , xs4
             , A_value.t
             , (Max_branching.n, Max_branching.n) P.t Async.Deferred.t )
             H3_2.T(Prover).t =
       fun bs ->
        match bs with [] -> [] | (module Step) :: bs -> Step.prove :: go bs
      in
      go steps
    in
    Timer.clock __LOC__ ;
    register () ;
    (provers, wrap_vk, disk_key, !cache_handle)

  let use_cache cache =
    let _, _, _, cache_handle = compile ~cache () in
    cache_handle
end

module Side_loaded = struct
  module V = Verification_key

  module Verification_key = struct
    include Side_loaded_verification_key

    let of_compiled tag : t =
      let d = Types_map.lookup_compiled tag.Tag.id in
      { wrap_vk= Some (Lazy.force d.wrap_vk)
      ; wrap_index=
          Lazy.force d.wrap_key
          |> Plonk_verification_key_evals.map ~f:Array.to_list
      ; max_width= Width.of_int_exn (Nat.to_int (Nat.Add.n d.max_branching))
      ; step_data=
          At_most.of_vector
            (Vector.map2 d.branchings d.step_domains ~f:(fun width ds ->
                 ({Domains.h= ds.h}, Width.of_int_exn width) ))
            (Nat.lte_exn (Vector.length d.step_domains) Max_branches.n) }

    module Max_width = Width.Max
  end

  let in_circuit tag vk = Types_map.set_ephemeral tag {index= `In_circuit vk}

  let in_prover tag vk = Types_map.set_ephemeral tag {index= `In_prover vk}

  let create ~name ~max_branching ~value_to_field_elements
      ~var_to_field_elements ~typ =
    Types_map.add_side_loaded ~name
      { max_branching
      ; value_to_field_elements
      ; var_to_field_elements
      ; typ
      ; branches= Verification_key.Max_branches.n }

  module Proof = Proof.Branching_max

  let verify (type t) ~(value_to_field_elements : t -> _)
      (ts : (Verification_key.t * t * Proof.t) list) =
    let m =
      ( module struct
        type nonrec t = t

        let to_field_elements = value_to_field_elements
      end
      : Intf.Statement_value
        with type t = t )
    in
    (* TODO: This should be the actual max width on a per proof basis *)
    let max_branching =
      (module Verification_key.Max_width
      : Nat.Intf
        with type n = Verification_key.Max_width.n )
    in
    with_return (fun {return} ->
        List.map ts ~f:(fun (vk, x, p) ->
            let vk : V.t =
              { commitments=
                  Plonk_verification_key_evals.map ~f:Array.of_list
                    vk.wrap_index
              ; step_domains=
                  Array.map (At_most.to_array vk.step_data) ~f:(fun (d, w) ->
                      let input_size =
                        Side_loaded_verification_key.(
                          input_size ~of_int:Fn.id ~add:( + ) ~mul:( * )
                            (Width.to_int vk.max_width))
                      in
                      { Domains.x=
                          Pow_2_roots_of_unity (Int.ceil_log2 input_size)
                      ; h= d.h } )
              ; index=
                  ( match vk.wrap_vk with
                  | None ->
                      return (Async.return false)
                  | Some x ->
                      x )
              ; data=
                  (* This isn't used in verify_heterogeneous, so we can leave this dummy *)
                  {constraints= 0} }
            in
            Verify.Instance.T (max_branching, m, vk, x, p) )
        |> Verify.verify_heterogenous )
end

let compile
    : type a_var a_value prev_varss prev_valuess widthss heightss max_branching branches.
       ?self:(a_var, a_value, max_branching, branches) Tag.t
    -> ?cache:Key_cache.Spec.t list
    -> (module Statement_var_intf with type t = a_var)
    -> (module Statement_value_intf with type t = a_value)
    -> typ:(a_var, a_value) Impls.Step.Typ.t
    -> branches:(module Nat.Intf with type n = branches)
    -> max_branching:(module Nat.Add.Intf with type n = max_branching)
    -> name:string
    -> constraint_constants:Snark_keys_header.Constraint_constants.t
    -> choices:(   self:(a_var, a_value, max_branching, branches) Tag.t
                -> ( prev_varss
                   , prev_valuess
                   , widthss
                   , heightss
                   , a_var
                   , a_value )
                   H4_2.T(Inductive_rule).t)
    -> (a_var, a_value, max_branching, branches) Tag.t
       * Cache_handle.t
       * (module Proof_intf
            with type t = (max_branching, max_branching) Proof.t
             and type statement = a_value)
       * ( prev_valuess
         , widthss
         , heightss
         , a_value
         , (max_branching, max_branching) Proof.t Async.Deferred.t )
         H3_2.T(Prover).t =
 fun ?self ?(cache = []) (module A_var) (module A_value) ~typ
     ~branches:(module Branches) ~max_branching:(module Max_branching) ~name
     ~constraint_constants ~choices ->
  let module M = Make (struct
    module A = A_var
    module A_value = A_value
    module Max_branching = Max_branching
    module Branches = Branches

    let constraint_constants = constraint_constants

    let name = name

    let self = match self with None -> `New | Some self -> `Existing self

    let typ = typ

    type nonrec prev_varss = prev_varss

    type nonrec prev_valuess = prev_valuess

    type nonrec widthss = widthss

    type nonrec heightss = heightss

    let choices = choices
  end) in
  let provers, wrap_vk, wrap_disk_key, cache_handle = M.compile ~cache () in
  let T = Max_branching.eq in
  let module P = struct
    type statement = A_value.t

    module Max_local_max_branching = Max_branching
    module Max_branching_vec = Nvector (Max_branching)
    include Proof.Make (Max_branching) (Max_local_max_branching)

    let id = wrap_disk_key

    let verification_key = wrap_vk

    let verify ts =
      verify
        (module Max_branching)
        (module A_value)
        (Lazy.force verification_key)
        ts

    let statement (T p : t) = p.statement.pass_through.app_state
  end in
  (M.self, cache_handle, (module P), provers)

module Provers = H3_2.T (Prover)
module Proof0 = Proof

let%test_module "test no side-loaded" =
  ( module struct
    let () =
      Tock.Keypair.set_urs_info
        [On_disk {directory= "/tmp/"; should_write= true}]

    let () =
      Tick.Keypair.set_urs_info
        [On_disk {directory= "/tmp/"; should_write= true}]

    open Impls.Step

    let () = Snarky_backendless.Snark0.set_eval_constraints true

    module Statement = struct
      type t = Field.t

      let to_field_elements x = [|x|]

      module Constant = struct
        type t = Field.Constant.t [@@deriving bin_io]

        let to_field_elements x = [|x|]
      end
    end

    module Blockchain_snark = struct
      module Statement = Statement

      let tag, _, p, Provers.[step] =
        Common.time "compile" (fun () ->
            compile
              (module Statement)
              (module Statement.Constant)
              ~typ:Field.typ
              ~branches:(module Nat.N1)
              ~max_branching:(module Nat.N2)
              ~name:"blockchain-snark"
              ~constraint_constants:
                (* Dummy values *)
                { sub_windows_per_window= 0
                ; ledger_depth= 0
                ; work_delay= 0
                ; block_window_duration_ms= 0
                ; transaction_capacity= Log_2 0
                ; pending_coinbase_depth= 0
                ; coinbase_amount= Unsigned.UInt64.of_int 0
                ; supercharged_coinbase_factor= 0
                ; account_creation_fee= Unsigned.UInt64.of_int 0
                ; fork= None }
              ~choices:(fun ~self ->
                [ { identifier= "main"
                  ; prevs= [self; self]
                  ; main=
                      (fun [prev; _] self ->
                        let is_base_case = Field.equal Field.zero self in
                        let proof_must_verify = Boolean.not is_base_case in
                        let self_correct = Field.(equal (one + prev) self) in
                        Boolean.Assert.any [self_correct; is_base_case] ;
                        [proof_must_verify; Boolean.false_] )
                  ; main_value=
                      (fun _ self ->
                        let is_base_case = Field.Constant.(equal zero self) in
                        let proof_must_verify = not is_base_case in
                        [proof_must_verify; false] ) } ] ) )

      module Proof = (val p)
    end

    let xs =
      let s_neg_one = Field.Constant.(negate one) in
      let b_neg_one : (Nat.N2.n, Nat.N2.n) Proof0.t =
        Proof0.dummy Nat.N2.n Nat.N2.n Nat.N2.n
      in
      let b0 =
        Common.time "b0" (fun () ->
            Async.Thread_safe.block_on_async_exn (fun () ->
                Blockchain_snark.step
                  [(s_neg_one, b_neg_one); (s_neg_one, b_neg_one)]
                  Field.Constant.zero ) )
      in
      let b1 =
        Common.time "b1" (fun () ->
            Async.Thread_safe.block_on_async_exn (fun () ->
                Blockchain_snark.step
                  [(Field.Constant.zero, b0); (Field.Constant.zero, b0)]
                  Field.Constant.one ) )
      in
      [(Field.Constant.zero, b0); (Field.Constant.one, b1)]

    let%test_unit "verify" =
      assert (
        Async.Thread_safe.block_on_async_exn (fun () ->
            Blockchain_snark.Proof.verify xs ) )
  end )

(*
let%test_module "test" =
  ( module struct
    let () =
      Tock.Keypair.set_urs_info
        [On_disk {directory= "/tmp/"; should_write= true}]

    let () =
      Tick.Keypair.set_urs_info
        [On_disk {directory= "/tmp/"; should_write= true}]

    open Impls.Step

    module Txn_snark = struct
      module Statement = struct
        type t = Field.t

        let to_field_elements x = [|x|]

        module Constant = struct
          type t = Field.Constant.t [@@deriving bin_io]

          let to_field_elements x = [|x|]
        end
      end

      (* A snark proving one knows a preimage of a hash *)
      module Know_preimage = struct
        module Statement = Statement

        type _ Snarky_backendless.Request.t +=
          | Preimage : Field.Constant.t Snarky_backendless.Request.t

        let hash_checked x =
          let open Step_main_inputs in
          let s = Sponge.create sponge_params in
          Sponge.absorb s (`Field x) ;
          Sponge.squeeze_field s

        let hash x =
          let open Tick_field_sponge in
          let s = Field.create params in
          Field.absorb s x ; Field.squeeze s

      let dummy_constraints () =
        let b = exists Boolean.typ_unchecked ~compute:(fun _ -> true) in
        let g = exists
            Step_main_inputs.Inner_curve.typ ~compute:(fun _ ->
                Tick.Inner_curve.(to_affine_exn one))
        in
        let _ =
          Step_main_inputs.Ops.scale_fast g
            (`Plus_two_to_len [|b; b|])
        in
        let _ =
          Pairing_main.Scalar_challenge.endo g (Scalar_challenge [b])
        in
        ()

        let tag, _, p, Provers.[prove; _] =
          compile
            (module Statement)
            (module Statement.Constant)
            ~typ:Field.typ
            ~branches:(module Nat.N2) (* Should be able to set to 1 *)
            ~max_branching:
              (module Nat.N2) (* TODO: Should be able to set this to 0 *)
            ~name:"preimage"
            ~choices:(fun ~self ->
              (* TODO: Make it possible to have a system that doesn't use its "self" *)
              [ { prevs= []
                ; main_value= (fun [] _ -> [])
                ; main=
                    (fun [] s ->
                       dummy_constraints () ;
                      let x = exists ~request:(fun () -> Preimage) Field.typ in
                      Field.Assert.equal s (hash_checked x) ;
                      [] ) }
                (* TODO: Shouldn't have to have this dummy *)
              ; { prevs= [self; self]
                ; main_value= (fun [_; _] _ -> [true; true])
                ; main=
                    (fun [_; _] s ->
                       dummy_constraints () ;
                       (* Unsatisfiable. *)
                      Field.(Assert.equal s (s + one)) ;
                      [Boolean.true_; Boolean.true_] ) } ] )

        let prove ~preimage =
          let h = hash preimage in
          ( h
          , prove [] h ~handler:(fun (With {request; respond}) ->
                match request with
                | Preimage ->
                    respond (Provide preimage)
                | _ ->
                    unhandled ) )

        module Proof = (val p)

        let side_loaded_vk = Side_loaded.Verification_key.of_compiled tag
      end

      let side_loaded =
        Side_loaded.create
          ~max_branching:(module Nat.N2)
          ~name:"side-loaded"
          ~value_to_field_elements:Statement.to_field_elements
          ~var_to_field_elements:Statement.to_field_elements ~typ:Field.typ

      let tag, _, p, Provers.[base; preimage_base; merge] =
        compile
          (module Statement)
          (module Statement.Constant)
          ~typ:Field.typ
          ~branches:(module Nat.N3)
          ~max_branching:(module Nat.N2)
          ~name:"txn-snark"
          ~choices:(fun ~self ->
            [ { prevs= []
              ; main=
                  (fun [] x ->
                    let t = (Field.is_square x :> Field.t) in
                    for i = 0 to 10_000 do
                      assert_r1cs t t t
                    done ;
                    [] )
              ; main_value= (fun [] _ -> []) }
            ; { prevs= [side_loaded]
              ; main=
                  (fun [hash] x ->
                    Side_loaded.in_circuit side_loaded
                      (exists Side_loaded_verification_key.typ
                         ~compute:(fun () -> Know_preimage.side_loaded_vk)) ;
                    Field.Assert.equal hash x ;
                    [Boolean.true_] )
              ; main_value= (fun [_] _ -> [true]) }
            ; { prevs= [self; self]
              ; main=
                  (fun [l; r] res ->
                    assert_r1cs l r res ;
                    [Boolean.true_; Boolean.true_] )
              ; main_value= (fun _ _ -> [true; true]) } ] )

      module Proof = (val p)
    end

    let t_proof =
      let preimage = Field.Constant.of_int 10 in
(*       let base1 = preimage in *)
      let base1, preimage_proof = Txn_snark.Know_preimage.prove ~preimage in
      let base2 = Field.Constant.of_int 9 in
      let base12 = Field.Constant.(base1 * base2) in
(*       let t1 = Common.time "t1" (fun () -> Txn_snark.base [] base1) in *)
      let t1 =
        Common.time "t1" (fun () ->
            Side_loaded.in_prover Txn_snark.side_loaded
              Txn_snark.Know_preimage.side_loaded_vk ;
            Txn_snark.preimage_base [(base1, preimage_proof)] base1 )
      in
      let module M = struct
        type t = Field.Constant.t * Txn_snark.Proof.t [@@deriving bin_io]
      end in
      Common.time "verif" (fun () ->
          assert (
            Txn_snark.Proof.verify (List.init 2 ~f:(fun _ -> (base1, t1))) ) ) ;
      Common.time "verif" (fun () ->
          assert (
            Txn_snark.Proof.verify (List.init 4 ~f:(fun _ -> (base1, t1))) ) ) ;
      Common.time "verif" (fun () ->
          assert (
            Txn_snark.Proof.verify (List.init 8 ~f:(fun _ -> (base1, t1))) ) ) ;
      let t2 = Common.time "t2" (fun () -> Txn_snark.base [] base2) in
      assert (Txn_snark.Proof.verify [(base1, t1); (base2, t2)]) ;
      (* Need two separate booleans.
         Should carry around prev should verify and self should verify *)
      let t12 =
        Common.time "t12" (fun () ->
            Txn_snark.merge [(base1, t1); (base2, t2)] base12 )
      in
      assert (Txn_snark.Proof.verify [(base1, t1); (base2, t2); (base12, t12)]) ;
      Common.time "verify" (fun () ->
          assert (
            Verify.verify_heterogenous
              [ T
                  ( (module Nat.N2)
                  , (module Txn_snark.Know_preimage.Statement.Constant)
                  , Lazy.force Txn_snark.Know_preimage.Proof.verification_key
                  , base1
                  , preimage_proof )
              ; T
                  ( (module Nat.N2)
                  , (module Txn_snark.Statement.Constant)
                  , Lazy.force Txn_snark.Proof.verification_key
                  , base1
                  , t1 )
              ; T
                  ( (module Nat.N2)
                  , (module Txn_snark.Statement.Constant)
                  , Lazy.force Txn_snark.Proof.verification_key
                  , base2
                  , t2 )
              ; T
                  ( (module Nat.N2)
                  , (module Txn_snark.Statement.Constant)
                  , Lazy.force Txn_snark.Proof.verification_key
                  , base12
                  , t12 ) ] ) ) ;
      (base12, t12)

    module Blockchain_snark = struct
      module Statement = Txn_snark.Statement

      let tag, _, p, Provers.[step] =
        Common.time "compile" (fun () ->
            compile
              (module Statement)
              (module Statement.Constant)
              ~typ:Field.typ
              ~branches:(module Nat.N1)
              ~max_branching:(module Nat.N2)
              ~name:"blockchain-snark"
              ~choices:(fun ~self ->
                [ { prevs= [self; Txn_snark.tag]
                  ; main=
                      (fun [prev; txn_snark] self ->
                        let is_base_case = Field.equal Field.zero self in
                        let proof_must_verify = Boolean.not is_base_case in
                        Boolean.Assert.any
                          [Field.(equal (one + prev) self); is_base_case] ;
                        [proof_must_verify; proof_must_verify] )
                  ; main_value=
                      (fun _ self ->
                        let is_base_case = Field.Constant.(equal zero self) in
                        let proof_must_verify = not is_base_case in
                        [proof_must_verify; proof_must_verify] ) } ] ) )

      module Proof = (val p)
    end

    let xs =
      let s_neg_one = Field.Constant.(negate one) in
      let b_neg_one : (Nat.N2.n, Nat.N2.n) Proof0.t =
        Proof0.dummy Nat.N2.n Nat.N2.n Nat.N2.n
      in
      let b0 =
        Common.time "b0" (fun () ->
            Blockchain_snark.step
              [(s_neg_one, b_neg_one); t_proof]
              Field.Constant.zero )
      in
      let b1 =
        Common.time "b1" (fun () ->
            Blockchain_snark.step
              [(Field.Constant.zero, b0); t_proof]
              Field.Constant.one )
      in
      [(Field.Constant.zero, b0); (Field.Constant.one, b1)]

    let%test_unit "verify" = assert (Blockchain_snark.Proof.verify xs)
  end ) *)
