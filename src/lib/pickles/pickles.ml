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
  let rec go :
      type len ms ns.
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
        { sg = Lazy.force Dummy.Ipa.Step.sg
        ; old_bulletproof_challenges = Vector.init m ~f:(fun _ -> dummy_chals)
        }
        :: go maxes []
    | m :: maxes, me_only :: me_onlys ->
        let me_only =
          { me_only with
            old_bulletproof_challenges =
              Vector.extend_exn me_only.old_bulletproof_challenges m dummy_chals
          }
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
        { Snark_keys_header.header_version = Snark_keys_header.header_version
        ; kind = { type_ = "verification key"; identifier = "dummy" }
        ; constraint_constants =
            { sub_windows_per_window = 0
            ; ledger_depth = 0
            ; work_delay = 0
            ; block_window_duration_ms = 0
            ; transaction_capacity = Log_2 0
            ; pending_coinbase_depth = 0
            ; coinbase_amount = Unsigned.UInt64.of_int 0
            ; supercharged_coinbase_factor = 0
            ; account_creation_fee = Unsigned.UInt64.of_int 0
            ; fork = None
            }
        ; commits = { mina = ""; marlin = "" }
        ; length = 0
        ; commit_date = ""
        ; constraint_system_hash = ""
        ; identifying_hash = ""
        }
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
       ?handler:
         (   Snarky_backendless.Request.request
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
        * (module Proof_intf with type t = 'proof and type statement = 'a_value)
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

module Make (A : Statement_var_intf) (A_value : Statement_value_intf) = struct
  module IR = Inductive_rule.T (A) (A_value)
  module HIR = H4.T (IR)

  let max_local_max_branchings ~self (type n)
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
                  if Type_equal.Id.same t.id self then
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

  module Lazy_ (A : T0) = struct
    type t = A.t Lazy.t
  end

  module Lazy_keys = struct
    type t =
      (Impls.Step.Keypair.t * Dirty.t) Lazy.t
      * (Marlin_plonk_bindings.Pasta_fp_verifier_index.t * Dirty.t) Lazy.t

    (* TODO Think this is right.. *)
  end

  let log_step main typ name index =
    let module Constraints = Snarky_log.Constraints (Impls.Step.Internal_Basic) in
    let log =
      let weight =
        let sys = Backend.Tick.R1CS_constraint_system.create () in
        fun (c : Impls.Step.Constraint.t) ->
          let prev = sys.next_row in
          List.iter c ~f:(fun { annotation; basic } ->
              Backend.Tick.R1CS_constraint_system.add_constraint sys
                ?label:annotation basic) ;
          let next = sys.next_row in
          next - prev
      in
      Constraints.log ~weight
        Impls.Step.(
          make_checked (fun () : unit ->
              let x = with_label __LOC__ (fun () -> exists typ) in
              main x ()))
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
        List.iter c ~f:(fun { annotation; basic } ->
            Backend.Tock.R1CS_constraint_system.add_constraint sys
              ?label:annotation basic) ;
        let next = sys.next_row in
        next - prev
      in
      let log =
        Constraints.log ~weight
          Impls.Wrap.(
            make_checked (fun () : unit ->
                let x = with_label __LOC__ (fun () -> exists typ) in
                main x ()))
      in
      log
    in
    Snarky_log.to_file
      (sprintf
         !"wrap-%s-%{sexp:Type_equal.Id.Uid.t}.json"
         name (Type_equal.Id.uid id))
      log

  let compile :
      type prev_varss prev_valuess widthss heightss max_branching branches.
         self:(A.t, A_value.t, max_branching, branches) Tag.t
      -> cache:Key_cache.Spec.t list
      -> ?disk_keys:
           (Cache.Step.Key.Verification.t, branches) Vector.t
           * Cache.Wrap.Key.Verification.t
      -> branches:(module Nat.Intf with type n = branches)
      -> max_branching:(module Nat.Add.Intf with type n = max_branching)
      -> name:string
      -> constraint_constants:Snark_keys_header.Constraint_constants.t
      -> typ:(A.t, A_value.t) Impls.Step.Typ.t
      -> choices:
           (   self:(A.t, A_value.t, max_branching, branches) Tag.t
            -> (prev_varss, prev_valuess, widthss, heightss) H4.T(IR).t)
      -> ( prev_valuess
         , widthss
         , heightss
         , A_value.t
         , (max_branching, max_branching) Proof.t Async.Deferred.t )
         H3_2.T(Prover).t
         * _
         * _
         * _ =
   fun ~self ~cache ?disk_keys ~branches:(module Branches)
       ~max_branching:(module Max_branching) ~name ~constraint_constants ~typ
       ~choices ->
    let snark_keys_header kind constraint_system_hash =
      { Snark_keys_header.header_version = Snark_keys_header.header_version
      ; kind
      ; constraint_constants
      ; commits =
          { mina = Mina_version.commit_id
          ; marlin = Mina_version.marlin_commit_id
          }
      ; length = (* This is a dummy, it gets filled in on read/write. *) 0
      ; commit_date = Mina_version.commit_date
      ; constraint_system_hash
      ; identifying_hash =
          (* TODO: Proper identifying hash. *)
          constraint_system_hash
      }
    in
    Timer.start __LOC__ ;
    let T = Max_branching.eq in
    let choices = choices ~self in
    let (T (prev_varss_n, prev_varss_length)) = HIR.length choices in
    let T = Nat.eq_exn prev_varss_n Branches.n in
    let padded, (module Maxes) =
      max_local_max_branchings
        (module Max_branching)
        prev_varss_length choices ~self:self.id
    in
    let full_signature = { Full_signature.padded; maxes = (module Maxes) } in
    Timer.clock __LOC__ ;
    let wrap_domains =
      let module M = Wrap_domains.Make (A) (A_value) in
      let rec f :
          type a b c d. (a, b, c, d) H4.T(IR).t -> (a, b, c, d) H4.T(M.I).t =
        function
        | [] ->
            []
        | x :: xs ->
            x :: f xs
      in
      M.f full_signature prev_varss_n prev_varss_length ~self
        ~choices:(f choices)
        ~max_branching:(module Max_branching)
    in
    Timer.clock __LOC__ ;
    let module Branch_data = struct
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
    end in
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
      V.f prev_varss_length (M.f choices)
    in
    let step_data =
      let i = ref 0 in
      Timer.clock __LOC__ ;
      let module M =
        H4.Map (IR) (Branch_data)
          (struct
            let f :
                type a b c d. (a, b, c, d) IR.t -> (a, b, c, d) Branch_data.t =
             fun rule ->
              Timer.clock __LOC__ ;
              let res =
                Common.time "make step data" (fun () ->
                    Step_branch_data.create ~index:(Index.of_int_exn !i)
                      ~max_branching:Max_branching.n ~branches:Branches.n ~self
                      ~typ A.to_field_elements A_value.to_field_elements rule
                      ~wrap_domains ~branchings:step_widths)
              in
              Timer.clock __LOC__ ; incr i ; res
          end)
      in
      M.f choices
    in
    Timer.clock __LOC__ ;
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
      V.f prev_varss_length (M.f step_data)
    in
    let cache_handle = ref (Lazy.return `Cache_hit) in
    let accum_dirty t = cache_handle := Cache_handle.(!cache_handle + t) in
    Timer.clock __LOC__ ;
    let step_keypairs =
      let disk_keys =
        Option.map disk_keys ~f:(fun (xs, _) -> Vector.to_array xs)
      in
      let module M =
        H4.Map
          (Branch_data)
          (E04 (Lazy_keys))
          (struct
            let etyp =
              Impls.Step.input ~branching:Max_branching.n
                ~wrap_rounds:Tock.Rounds.n

            let f (T b : _ Branch_data.t) =
              let (T (typ, conv)) = etyp in
              let main x () : unit =
                b.main
                  (Impls.Step.with_label "conv" (fun () -> conv x))
                  ~step_domains
              in
              let () = if debug then log_step main typ name b.index in
              let open Impls.Step in
              let k_p =
                lazy
                  (let cs = constraint_system ~exposing:[ typ ] main in
                   let cs_hash =
                     Md5.to_hex (R1CS_constraint_system.digest cs)
                   in
                   ( Type_equal.Id.uid self.id
                   , snark_keys_header
                       { type_ = "step-proving-key"
                       ; identifier = name ^ "-" ^ b.rule.identifier
                       }
                       cs_hash
                   , Index.to_int b.index
                   , cs ))
              in
              let k_v =
                match disk_keys with
                | Some ks ->
                    Lazy.return ks.(Index.to_int b.index)
                | None ->
                    lazy
                      (let id, _header, index, cs = Lazy.force k_p in
                       let digest = R1CS_constraint_system.digest cs in
                       ( id
                       , snark_keys_header
                           { type_ = "step-verification-key"
                           ; identifier = name ^ "-" ^ b.rule.identifier
                           }
                           (Md5.to_hex digest)
                       , index
                       , digest ))
              in
              let ((pk, vk) as res) =
                Common.time "step read or generate" (fun () ->
                    Cache.Step.read_or_generate cache k_p k_v typ main)
              in
              accum_dirty (Lazy.map pk ~f:snd) ;
              accum_dirty (Lazy.map vk ~f:snd) ;
              res
          end)
      in
      M.f step_data
    in
    Timer.clock __LOC__ ;
    let step_vks =
      let module V = H4.To_vector (Lazy_keys) in
      lazy
        (Vector.map (V.f prev_varss_length step_keypairs) ~f:(fun (_, vk) ->
             Tick.Keypair.vk_commitments (fst (Lazy.force vk))))
    in
    Timer.clock __LOC__ ;
    let wrap_requests, wrap_main =
      Timer.clock __LOC__ ;
      let prev_wrap_domains =
        let module M =
          H4.Map
            (IR)
            (H4.T
               (E04 (Domains)))
               (struct
                 let f :
                     type a b c d.
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
                                 Common.wrap_domains)
                       end)
                   in
                   M.f rule.Inductive_rule.prevs
               end)
        in
        M.f choices
      in
      Timer.clock __LOC__ ;
      Wrap_main.wrap_main full_signature prev_varss_length step_vks step_widths
        step_domains prev_wrap_domains
        (module Max_branching)
    in
    Timer.clock __LOC__ ;
    let (wrap_pk, wrap_vk), disk_key =
      let open Impls.Wrap in
      let (T (typ, conv)) = input () in
      let main x () : unit = wrap_main (conv x) in
      let () = if debug then log_wrap main typ name self.id in
      let self_id = Type_equal.Id.uid self.id in
      let disk_key_prover =
        lazy
          (let cs = constraint_system ~exposing:[ typ ] main in
           let cs_hash = Md5.to_hex (R1CS_constraint_system.digest cs) in
           ( self_id
           , snark_keys_header
               { type_ = "wrap-proving-key"; identifier = name }
               cs_hash
           , cs ))
      in
      let disk_key_verifier =
        match disk_keys with
        | None ->
            lazy
              (let id, _header, cs = Lazy.force disk_key_prover in
               let digest = R1CS_constraint_system.digest cs in
               ( id
               , snark_keys_header
                   { type_ = "wrap-verification-key"; identifier = name }
                   (Md5.to_hex digest)
               , digest ))
        | Some (_, (_id, header, digest)) ->
            Lazy.return (self_id, header, digest)
      in
      let r =
        Common.time "wrap read or generate " (fun () ->
            Cache.Wrap.read_or_generate
              (Vector.to_array step_domains)
              cache disk_key_prover disk_key_verifier typ main)
      in
      (r, disk_key_verifier)
    in
    Timer.clock __LOC__ ;
    accum_dirty (Lazy.map wrap_pk ~f:snd) ;
    accum_dirty (Lazy.map wrap_vk ~f:snd) ;
    let wrap_vk = Lazy.map wrap_vk ~f:fst in
    let module S = Step.Make (A) (A_value) (Max_branching) in
    let provers =
      let module Z = H4.Zip (Branch_data) (E04 (Impls.Step.Keypair)) in
      let f :
          type prev_vars prev_values local_widths local_heights.
             (prev_vars, prev_values, local_widths, local_heights) Branch_data.t
          -> Lazy_keys.t
          -> ?handler:
               (   Snarky_backendless.Request.request
                -> Snarky_backendless.Request.response)
          -> ( prev_values
             , local_widths
             , local_heights )
             H3.T(Statement_with_proof).t
          -> A_value.t
          -> (Max_branching.n, Max_branching.n) Proof.t Async.Deferred.t =
       fun (T b as branch_data) (step_pk, step_vk) ->
        let (module Requests) = b.requests in
        let _, prev_vars_length = b.branching in
        let step handler prevs next_state =
          let wrap_vk = Lazy.force wrap_vk in
          S.f ?handler branch_data next_state ~prevs_length:prev_vars_length
            ~self ~step_domains ~self_dlog_plonk_index:wrap_vk.commitments
            (Impls.Step.Keypair.pk (fst (Lazy.force step_pk)))
            wrap_vk.index prevs
        in
        let pairing_vk = fst (Lazy.force step_vk) in
        let wrap ?handler prevs next_state =
          let wrap_vk = Lazy.force wrap_vk in
          let prevs =
            let module M =
              H3.Map (Statement_with_proof) (P.With_data)
                (struct
                  let f ((app_state, T proof) : _ Statement_with_proof.t) =
                    P.T
                      { proof with
                        statement =
                          { proof.statement with
                            pass_through =
                              { proof.statement.pass_through with app_state }
                          }
                      }
                end)
            in
            M.f prevs
          in
          let%bind.Async.Deferred proof =
            step handler ~maxes:(module Maxes) prevs next_state
          in
          let proof =
            { proof with
              statement =
                { proof.statement with
                  pass_through =
                    pad_pass_throughs
                      (module Maxes)
                      proof.statement.pass_through
                }
            }
          in
          let%map.Async.Deferred proof =
            Wrap.wrap ~max_branching:Max_branching.n full_signature.maxes
              wrap_requests ~dlog_plonk_index:wrap_vk.commitments wrap_main
              A_value.to_field_elements ~pairing_vk ~step_domains:b.domains
              ~pairing_plonk_indices:(Lazy.force step_vks) ~wrap_domains
              (Impls.Wrap.Keypair.pk (fst (Lazy.force wrap_pk)))
              proof
          in
          Proof.T
            { proof with
              statement =
                { proof.statement with
                  pass_through =
                    { proof.statement.pass_through with app_state = () }
                }
            }
        in
        wrap
      in
      let rec go :
          type xs1 xs2 xs3 xs4.
             (xs1, xs2, xs3, xs4) H4.T(Branch_data).t
          -> (xs1, xs2, xs3, xs4) H4.T(E04(Lazy_keys)).t
          -> ( xs2
             , xs3
             , xs4
             , A_value.t
             , (max_branching, max_branching) Proof.t Async.Deferred.t )
             H3_2.T(Prover).t =
       fun bs ks ->
        match (bs, ks) with
        | [], [] ->
            []
        | b :: bs, k :: ks ->
            f b k :: go bs ks
      in
      go step_data step_keypairs
    in
    Timer.clock __LOC__ ;
    let data : _ Types_map.Compiled.t =
      { branches = Branches.n
      ; branchings = step_widths
      ; max_branching = (module Max_branching)
      ; typ
      ; value_to_field_elements = A_value.to_field_elements
      ; var_to_field_elements = A.to_field_elements
      ; wrap_key = Lazy.map wrap_vk ~f:Verification_key.commitments
      ; wrap_vk = Lazy.map wrap_vk ~f:Verification_key.index
      ; wrap_domains
      ; step_domains
      }
    in
    Timer.clock __LOC__ ;
    Types_map.add_exn self data ;
    (provers, wrap_vk, disk_key, !cache_handle)
end

module Side_loaded = struct
  module V = Verification_key

  module Verification_key = struct
    include Side_loaded_verification_key

    let of_compiled tag : t =
      let d = Types_map.lookup_compiled tag.Tag.id in
      { wrap_vk = Some (Lazy.force d.wrap_vk)
      ; wrap_index =
          Lazy.force d.wrap_key
          |> Plonk_verification_key_evals.map ~f:Array.to_list
      ; max_width = Width.of_int_exn (Nat.to_int (Nat.Add.n d.max_branching))
      ; step_data =
          At_most.of_vector
            (Vector.map2 d.branchings d.step_domains ~f:(fun width ds ->
                 ({ Domains.h = ds.h }, Width.of_int_exn width)))
            (Nat.lte_exn (Vector.length d.step_domains) Max_branches.n)
      }

    module Max_width = Width.Max
  end

  let in_circuit tag vk = Types_map.set_ephemeral tag { index = `In_circuit vk }

  let in_prover tag vk = Types_map.set_ephemeral tag { index = `In_prover vk }

  let create ~name ~max_branching ~value_to_field_elements
      ~var_to_field_elements ~typ =
    Types_map.add_side_loaded ~name
      { max_branching
      ; value_to_field_elements
      ; var_to_field_elements
      ; typ
      ; branches = Verification_key.Max_branches.n
      }

  module Proof = Proof.Branching_max

  let verify (type t) ~(value_to_field_elements : t -> _)
      (ts : (Verification_key.t * t * Proof.t) list) =
    let m =
      ( module struct
        type nonrec t = t

        let to_field_elements = value_to_field_elements
      end : Intf.Statement_value
        with type t = t )
    in
    (* TODO: This should be the actual max width on a per proof basis *)
    let max_branching =
      (module Verification_key.Max_width : Nat.Intf
        with type n = Verification_key.Max_width.n )
    in
    with_return (fun { return } ->
        List.map ts ~f:(fun (vk, x, p) ->
            let vk : V.t =
              { commitments =
                  Plonk_verification_key_evals.map ~f:Array.of_list
                    vk.wrap_index
              ; step_domains =
                  Array.map (At_most.to_array vk.step_data) ~f:(fun (d, w) ->
                      let input_size =
                        Side_loaded_verification_key.(
                          input_size ~of_int:Fn.id ~add:( + ) ~mul:( * )
                            (Width.to_int vk.max_width))
                      in
                      { Domains.x =
                          Pow_2_roots_of_unity (Int.ceil_log2 input_size)
                      ; h = d.h
                      })
              ; index =
                  ( match vk.wrap_vk with
                  | None ->
                      return (Async.return false)
                  | Some x ->
                      x )
              ; data =
                  (* This isn't used in verify_heterogeneous, so we can leave this dummy *)
                  { constraints = 0 }
              }
            in
            Verify.Instance.T (max_branching, m, vk, x, p))
        |> Verify.verify_heterogenous)
end

let compile :
    type a_var a_value prev_varss prev_valuess widthss heightss max_branching branches.
       ?self:(a_var, a_value, max_branching, branches) Tag.t
    -> ?cache:Key_cache.Spec.t list
    -> ?disk_keys:
         (Cache.Step.Key.Verification.t, branches) Vector.t
         * Cache.Wrap.Key.Verification.t
    -> (module Statement_var_intf with type t = a_var)
    -> (module Statement_value_intf with type t = a_value)
    -> typ:(a_var, a_value) Impls.Step.Typ.t
    -> branches:(module Nat.Intf with type n = branches)
    -> max_branching:(module Nat.Add.Intf with type n = max_branching)
    -> name:string
    -> constraint_constants:Snark_keys_header.Constraint_constants.t
    -> choices:
         (   self:(a_var, a_value, max_branching, branches) Tag.t
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
 fun ?self ?(cache = []) ?disk_keys (module A_var) (module A_value) ~typ
     ~branches ~max_branching ~name ~constraint_constants ~choices ->
  let self =
    match self with
    | None ->
        { Tag.id = Type_equal.Id.create ~name sexp_of_opaque; kind = Compiled }
    | Some self ->
        self
  in
  let module M = Make (A_var) (A_value) in
  let rec conv_irs :
      type v1ss v2ss wss hss.
         (v1ss, v2ss, wss, hss, a_var, a_value) H4_2.T(Inductive_rule).t
      -> (v1ss, v2ss, wss, hss) H4.T(M.IR).t = function
    | [] ->
        []
    | r :: rs ->
        r :: conv_irs rs
  in
  let provers, wrap_vk, wrap_disk_key, cache_handle =
    M.compile ~self ~cache ?disk_keys ~branches ~max_branching ~name ~typ
      ~constraint_constants ~choices:(fun ~self -> conv_irs (choices ~self))
  in
  let (module Max_branching) = max_branching in
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
  (self, cache_handle, (module P), provers)

module Provers = H3_2.T (Prover)
module Proof0 = Proof

let%test_module "test no side-loaded" =
  ( module struct
    let () = Backtrace.elide := false

    let () =
      Tock.Keypair.set_urs_info
        [ On_disk { directory = "/tmp/"; should_write = true } ]

    let () =
      Tick.Keypair.set_urs_info
        [ On_disk { directory = "/tmp/"; should_write = true } ]

    open Impls.Step

    let () = Snarky_backendless.Snark0.set_eval_constraints true

    module Statement = struct
      type t = Field.t

      let to_field_elements x = [| x |]

      module Constant = struct
        type t = Field.Constant.t [@@deriving bin_io]

        let to_field_elements x = [| x |]
      end
    end

    module Blockchain_snark = struct
      module Statement = Statement

      let tag, _, p, Provers.[ step ] =
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
                { sub_windows_per_window = 0
                ; ledger_depth = 0
                ; work_delay = 0
                ; block_window_duration_ms = 0
                ; transaction_capacity = Log_2 0
                ; pending_coinbase_depth = 0
                ; coinbase_amount = Unsigned.UInt64.of_int 0
                ; supercharged_coinbase_factor = 0
                ; account_creation_fee = Unsigned.UInt64.of_int 0
                ; fork = None
                }
              ~choices:(fun ~self ->
                [ { identifier = "main"
                  ; prevs = [ self; self ]
                  ; main =
                      (fun [ prev; _ ] self ->
                        let is_base_case = Field.equal Field.zero self in
                        let proof_must_verify = Boolean.not is_base_case in
                        let self_correct = Field.(equal (one + prev) self) in
                        Boolean.Assert.any [ self_correct; is_base_case ] ;
                        [ proof_must_verify; Boolean.false_ ])
                  ; main_value =
                      (fun _ self ->
                        let is_base_case = Field.Constant.(equal zero self) in
                        let proof_must_verify = not is_base_case in
                        [ proof_must_verify; false ])
                  }
                ]))

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
                  [ (s_neg_one, b_neg_one); (s_neg_one, b_neg_one) ]
                  Field.Constant.zero))
      in
      let b1 =
        Common.time "b1" (fun () ->
            Async.Thread_safe.block_on_async_exn (fun () ->
                Blockchain_snark.step
                  [ (Field.Constant.zero, b0); (Field.Constant.zero, b0) ]
                  Field.Constant.one))
      in
      [ (Field.Constant.zero, b0); (Field.Constant.one, b1) ]

    let%test_unit "verify" =
      assert (
        Async.Thread_safe.block_on_async_exn (fun () ->
            Blockchain_snark.Proof.verify xs) )
  end )

(*
let%test_module "test" =
  ( module struct
    let () = Backtrace.elide := false

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
