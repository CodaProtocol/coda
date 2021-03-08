module S = Sponge
open Core
open Pickles_types
open Common
open Poly_types
open Higher_kinded_poly
open Hlist
open Import
open Impls.Step
open Step_main_inputs
module B = Inductive_rule.B

module Proof_system = struct
  module Step = struct
    module Types = struct
      module Per_proof_witness_constant = P3.T (Per_proof_witness.Constant)
      module Per_proof_witness = P3.T (Per_proof_witness)
      module Unfinalized = Unfinalized
      module Unfinalized_constant = Unfinalized.Constant
    end

    let per_proof_witness_typ
        (type a_var a_value max_num_input_proofs num_rules)
        (basic :
          ( a_var
          , a_value
          , max_num_input_proofs
          , num_rules )
          Types_map.Compiled.basic)
        (tag : (a_var, a_value, max_num_input_proofs, num_rules) Tag.tag) prevs
        prev_num_input_proofss prev_num_ruless prevs_length
        prev_num_input_proofss_length prev_num_ruless_length =
      let module Typ_with_max_num_input_proofs = struct
        type ( 'var
             , 'value
             , 'local_max_num_input_proofs
             , 'local_num_rules
             , 'poly_var
             , 'poly_value )
             t =
          ( ( 'var
            , 'local_max_num_input_proofs
            , 'local_num_rules
            , 'poly_var )
            P3.t
          , ( 'value
            , 'local_max_num_input_proofs
            , 'local_num_rules
            , 'poly_value )
            P3.t )
          Typ.t
      end in
      let module PPW = P3.T (Per_proof_witness) in
      let module PPWC = P3.T (Per_proof_witness.Constant) in
      let rec join : type e pvars pvals ns1 ns2 br.
             (pvars, pvals, ns1, ns2) H4.T(Tag).t
          -> ns1 H1.T(Nat).t
          -> ns2 H1.T(Nat).t
          -> (pvars, br) Length.t
          -> (ns1, br) Length.t
          -> (ns2, br) Length.t
          -> ( pvars
             , pvals
             , ns1
             , ns2
             , PPW.witness
             , PPWC.witness )
             H4_2.T(Typ_with_max_num_input_proofs).t =
       fun ds ns1 ns2 ld ln1 ln2 ->
        match (ds, ns1, ns2, ld, ln1, ln2) with
        | [], [], [], Z, Z, Z ->
            []
        | d :: ds, n1 :: ns1, n2 :: ns2, S ld, S ln1, S ln2 ->
            let step_domains, typ =
              (fun (type var value n m) (d : (var, value, n, m) Tag.t) ->
                let typ : (_, m) Vector.t * (var, value) Typ.t =
                  match Type_equal.Id.same_witness tag d.id with
                  | Some T ->
                      (basic.step_domains, basic.typ)
                  | None -> (
                    (* TODO: Abstract this into a function in Types_map *)
                    match d.kind with
                    | Compiled ->
                        let d = Types_map.lookup_compiled d.id in
                        (d.step_domains, d.typ)
                    | Side_loaded ->
                        let d = Types_map.lookup_side_loaded d.id in
                        (* TODO: This replication to please the type checker is
                       pointless... *)
                        ( Vector.init d.permanent.num_rules ~f:(fun _ ->
                              Side_loaded_verification_key.max_domains_with_x
                          )
                        , d.permanent.typ ) )
                in
                typ )
                d
            in
            let transport_poly (type a b c d e f t1 t2)
                (t :
                  ( (a, b, c) Per_proof_witness.t
                  , (d, e, f) Per_proof_witness.Constant.t )
                  Typ.t)
                (eq_1 : (t1, (a, b, c) Per_proof_witness.t) Type_equal.t)
                (eq_2 :
                  (t2, (d, e, f) Per_proof_witness.Constant.t) Type_equal.t) :
                (t1, t2) Typ.t =
              let T = eq_1 in
              let T = eq_2 in
              t
            in
            let t = Per_proof_witness.typ ~step_domains typ n1 n2 in
            transport_poly t (PPW.mk_eq ()) (PPWC.mk_eq ())
            :: join ds ns1 ns2 ld ln1 ln2
        | [], _, _, _, _, _ ->
            .
        | _ :: _, _, _, _, _, _ ->
            .
      in
      let typs =
        join prevs prev_num_input_proofss prev_num_ruless prevs_length
          prev_num_input_proofss_length prev_num_ruless_length
      in
      let module Prev_typ =
        H4_2.Typ_split (Impls.Step) (Typ_with_max_num_input_proofs) (P3) (P3)
          (struct
            let f = Fn.id
          end)
      in
      Prev_typ.f typs

    let get_opening_sg (ppw : _ Types.Per_proof_witness.poly) : Inner_curve.t =
      let _, _, _, _, _, (opening, _) = Types.Per_proof_witness.of_poly ppw in
      opening.sg

    let get_step_data
        (type self_var self_value self_max_num_input_proofs self_num_rules var
        value max_num_input_proofs num_rules)
        (self_data :
          ( self_var
          , self_value
          , self_max_num_input_proofs
          , self_num_rules )
          Types_map.For_step.t)
        (self_tag :
          ( self_var
          , self_value
          , self_max_num_input_proofs
          , self_num_rules )
          Tag.tag) (tag : (var, value, max_num_input_proofs, num_rules) Tag.t)
        : (var, value, max_num_input_proofs, num_rules) Types_map.For_step.t =
      match Type_equal.Id.same_witness self_tag tag.id with
      | Some T ->
          self_data
      | None -> (
        match tag.kind with
        | Compiled ->
            Types_map.For_step.of_compiled (Types_map.lookup_compiled tag.id)
        | Side_loaded ->
            Types_map.For_step.of_side_loaded
              (Types_map.lookup_side_loaded tag.id) )

    let finalize_and_verify
        (type self_var self_value self_max_num_input_proofs self_num_rules var
        value max_num_input_proofs num_rules)
        (self_data :
          ( self_var
          , self_value
          , self_max_num_input_proofs
          , self_num_rules )
          Types_map.For_step.t)
        (self_tag :
          ( self_var
          , self_value
          , self_max_num_input_proofs
          , self_num_rules )
          Tag.tag)
        (proof :
          (var, max_num_input_proofs, num_rules) Types.Per_proof_witness.poly)
        (tag : (var, value, max_num_input_proofs, num_rules) Tag.t)
        (pass_through : Digest.t) (unfinalized : Unfinalized.t)
        (should_verify : Boolean.var) =
      let open Pairing_main in
      Boolean.Assert.( = ) unfinalized.should_finalize should_verify ;
      let ( app_state
          , state
          , prev_evals
          , sg_old
          , old_bulletproof_challenges
          , (opening, messages) ) =
        Types.Per_proof_witness.of_poly proof
      in
      let d = get_step_data self_data self_tag tag in
      let finalized, chals =
        with_label __LOC__ (fun () ->
            let sponge_digest = state.sponge_digest_before_evaluations in
            let sponge =
              let open Step_main_inputs in
              let sponge = Sponge.create sponge_params in
              Sponge.absorb sponge (`Field sponge_digest) ;
              sponge |> Opt_sponge.Underlying.of_sponge |> S.Bit_sponge.make
            in
            finalize_other_proof d.max_num_input_proofs
              ~num_input_proofs:d.num_input_proofs
              ~rules_num_input_proofs:d.rules_num_input_proofs
              ~step_domains:d.step_domains ~sponge ~old_bulletproof_challenges
              state.deferred_values prev_evals )
      in
      let which_rule = state.deferred_values.which_rule in
      let state =
        with_label __LOC__ (fun () ->
            { state with
              deferred_values=
                { state.deferred_values with
                  which_rule=
                    Pseudo.choose
                      ( state.deferred_values.which_rule
                      , Vector.init d.num_rules ~f:Field.of_int )
                      ~f:Fn.id
                    |> Import.Types.Index.of_field (module Impl) } } )
      in
      let statement =
        let prev_me_only =
          with_label __LOC__ (fun () ->
              let hash =
                (* TODO: Don't rehash when it's not necessary *)
                unstage
                  (hash_me_only_opt ~index:d.wrap_key d.var_to_field_elements)
              in
              hash ~num_input_proofs:d.rules_num_input_proofs
                ~max_num_input_proofs:(Nat.Add.n d.max_num_input_proofs)
                ~which_rule
                (* Use opt sponge for cutting off the bulletproof challenges early *)
                { app_state
                ; dlog_plonk_index= d.wrap_key
                ; sg= sg_old
                ; old_bulletproof_challenges } )
        in
        { Import.Types.Dlog_based.Statement.pass_through= prev_me_only
        ; proof_state= {state with me_only= pass_through} }
      in
      let verified =
        with_label __LOC__ (fun () ->
            verify ~num_input_proofs:d.max_num_input_proofs
              ~wrap_domain:d.wrap_domains.h ~is_base_case:should_verify ~sg_old
              ~opening ~messages ~wrap_verification_key:d.wrap_key statement
              unfinalized )
      in
      if debug then
        as_prover
          As_prover.(
            fun () ->
              let finalized = read Boolean.typ finalized in
              let verified = read Boolean.typ verified in
              let should_verify = read Boolean.typ should_verify in
              printf "finalized: %b\n%!" finalized ;
              printf "verified: %b\n%!" verified ;
              printf "should_verify: %b\n\n%!" should_verify) ;
      (chals, Boolean.(verified &&& finalized ||| not should_verify))

    let per_proof_spec ~wrap_rounds :
        ( Types.Unfinalized_constant.t
        , Types.Unfinalized.t
        , _ )
        Composition_types.Spec.t =
      let open Composition_types.Spec in
      let open Composition_types.Pairing_based.Proof_state.Per_proof.In_circuit in
      Map_var (Map_value (spec wrap_rounds, to_data, of_data), to_data, of_data)

    let per_proof_witness_statement :
        ('a, _, _, Types.Per_proof_witness.witness) P3.t -> 'a =
     fun witness ->
      let x, _, _, _, _, _ = Types.Per_proof_witness.of_poly witness in
      x
  end
end

module type Proof_system = sig
  module Step : sig
    module Types : sig
      module Per_proof_witness : P3.S

      module Per_proof_witness_constant : P3.S

      module Unfinalized : T0

      module Unfinalized_constant : T0
    end

    val per_proof_witness_typ :
         ( 'self_var
         , 'self_value
         , 'self_max_num_input_proofs
         , 'self_num_rules )
         Types_map.Compiled.basic
      -> ( 'self_var
         , 'self_value
         , 'self_max_num_input_proofs
         , 'self_num_rules )
         Tag.tag
      -> ('vars, 'values, 'max_num_input_proofss, 'num_ruless) H4.T(Tag).t
      -> 'max_num_input_proofss H1.T(Pickles_types.Nat).t
      -> 'num_ruless H1.T(Pickles_types.Nat).t
      -> ('vars, 'length) Pickles_types.Hlist.Length.t
      -> ('max_num_input_proofss, 'length) Pickles_types.Hlist.Length.t
      -> ('num_ruless, 'length) Pickles_types.Hlist.Length.t
      -> ( ( 'vars
           , 'max_num_input_proofss
           , 'num_ruless
           , Types.Per_proof_witness.witness )
           H3_1.T(P3).t
         , ( 'values
           , 'max_num_input_proofss
           , 'num_ruless
           , Types.Per_proof_witness_constant.witness )
           H3_1.T(P3).t )
         Typ.t

    val get_opening_sg :
      ('a, 'b, 'c, Types.Per_proof_witness.witness) P3.t -> Inner_curve.t

    val finalize_and_verify :
         ( 'self_var
         , 'self_value
         , 'self_max_num_input_proofs
         , 'self_num_rules )
         Types_map.For_step.t
      -> ( 'self_var
         , 'self_value
         , 'self_max_num_input_proofs
         , 'self_num_rules )
         Tag.tag
      -> ( 'var
         , 'max_num_input_proofs
         , 'num_rules
         , Types.Per_proof_witness.witness )
         P3.t
      -> ('var, 'value, 'max_num_input_proofs, 'num_rules) Tag.t
      -> Digest.t
      -> Types.Unfinalized.t
      -> Boolean.var
      -> (Field.t, Backend.Tick.Rounds.n) Vector.t * Boolean.var

    val per_proof_spec :
         wrap_rounds:Backend.Tock.Rounds.n Pickles_types.Nat.t
      -> ( Types.Unfinalized_constant.t
         , Types.Unfinalized.t
         , < bool1: bool
           ; bool2: Boolean.var
           ; bulletproof_challenge1:
               Challenge.Constant.t Scalar_challenge.t Bulletproof_challenge.t
           ; bulletproof_challenge2:
               Field.t Scalar_challenge.t Bulletproof_challenge.t
           ; challenge1: Challenge.Constant.t
           ; challenge2: Field.t
           ; digest1: Digest.Constant.t
           ; digest2: Field.t
           ; field1: Step_main_inputs.Other_field.t Shifted_value.t
           ; field2: Impls.Step.Other_field.t Shifted_value.t
           ; .. > )
         Composition_types.Spec.t

    val per_proof_witness_statement :
      ('a, _, _, Types.Per_proof_witness.witness) P3.t -> 'a
  end
end

type ( 'per_proof_witness
     , 'per_proof_witness_constant
     , 'unfinalized
     , 'unfinalized_constant )
     proof_system =
  (module Proof_system
     with type Step.Types.Per_proof_witness.witness = 'per_proof_witness
      and type Step.Types.Per_proof_witness_constant.witness = 'per_proof_witness_constant
      and type Step.Types.Unfinalized.t = 'unfinalized
      and type Step.Types.Unfinalized_constant.t = 'unfinalized_constant)

module PS = struct
  type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) proof_system
end

module Bulletproof_challenges = struct
  type t = (Field.t, Backend.Tick.Rounds.n) Vector.t
end

module Proof_system_ : Proof_system = Proof_system

module Typ_function = struct
  type ( 'vars
       , 'values
       , 'local_max_num_input_proofss
       , 'local_num_ruless
       , 'br
       , 'poly_vars
       , 'poly_values )
       t =
       ( 'vars
       , 'values
       , 'local_max_num_input_proofss
       , 'local_num_ruless )
       H4.T(Tag).t
    -> 'local_max_num_input_proofss H1.T(Nat).t
    -> 'local_num_ruless H1.T(Nat).t
    -> ('vars, 'br) Length.t
    -> ('local_max_num_input_proofss, 'br) Length.t
    -> ('local_num_ruless, 'br) Length.t
    -> ( ( 'vars
         , 'local_max_num_input_proofss
         , 'local_num_ruless
         , 'poly_vars )
         H3_1.T(P3).t
       , ( 'values
         , 'local_max_num_input_proofss
         , 'local_num_ruless
         , 'poly_values )
         H3_1.T(P3).t )
       Typ.t
end

let build_combined_typ basic self_id proof_systems tagss ns1 ns2 ld ln1 ln2 =
  let module Typ_with_max_num_input_proofs = struct
    type ( 'var
         , 'value
         , 'local_max_num_input_proofs
         , 'local_num_rules
         , 'poly_var
         , 'poly_value )
         t =
      ( ( 'var
        , 'local_max_num_input_proofs
        , 'local_num_rules
        , 'poly_var )
        H3_1.T(P3).t
      , ( 'value
        , 'local_max_num_input_proofs
        , 'local_num_rules
        , 'poly_value )
        H3_1.T(P3).t )
      Typ.t
  end in
  let rec join
      : type e pvars pvals ns1 ns2 per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants br.
         ( per_proof_witnesses
         , per_proof_witness_constants
         , unfinalizeds
         , unfinalized_constants )
         H4.T(PS).t
      -> (pvars, pvals, ns1, ns2) H4.T(H4.T(Tag)).t
      -> ns1 H1.T(H1.T(Nat)).t
      -> ns2 H1.T(H1.T(Nat)).t
      -> (pvars, br) H2.T(Length).t
      -> (ns1, br) H2.T(Length).t
      -> (ns2, br) H2.T(Length).t
      -> ( pvars
         , pvals
         , ns1
         , ns2
         , per_proof_witnesses
         , per_proof_witness_constants )
         H6.T(Typ_with_max_num_input_proofs).t =
   fun proof_systems tagss ns1 ns2 ld ln1 ln2 ->
    match (proof_systems, tagss, ns1, ns2, ld, ln1, ln2) with
    | [], [], [], [], [], [], [] ->
        []
    | ( (module PS_) :: proof_systems
      , tag :: tags
      , n1 :: ns1
      , n2 :: ns2
      , l :: ld
      , l1 :: ln1
      , l2 :: ln2 ) ->
        let typ =
          PS_.Step.per_proof_witness_typ basic self_id tag n1 n2 l l1 l2
        in
        let typs = join proof_systems tags ns1 ns2 ld ln1 ln2 in
        typ :: typs
    | (module PS_) :: _, [], [], [], [], [], [] ->
        failwith "build_combined_typ"
    | [], _ :: _, _, _, _, _, _ ->
        failwith "build_combined_typ"
  in
  let module M =
    H6.Typ_split (Impls.Step) (Typ_with_max_num_input_proofs)
      (H3_1.T (P3))
      (H3_1.T (P3))
      (struct
        let f = Fn.id
      end)
  in
  M.f (join proof_systems tagss ns1 ns2 ld ln1 ln2)

let rec h2_vec_to_h2_h1_1 : type length params actual_length carrying.
       (carrying, length) H2.T(Vector).t
    -> (params, actual_length) H2.T(Length).t
    -> (actual_length, length) H2.T(Nat.Lte).t
    -> (params, carrying) H2.T(H1_1.T(H2.Snd)).t =
 fun vectors lengths ltes ->
  match (vectors, lengths, ltes) with
  | [], [], [] ->
      []
  | vector :: vectors, length :: lengths, lte :: ltes ->
      let hlist = H1_1.Of_vector.f length (Vector.trim vector lte) in
      let hlists = h2_vec_to_h2_h1_1 vectors lengths ltes in
      hlist :: hlists
  | _ :: _, [], _ ->
      .
  | [], _ :: _, _ ->
      .

(* The SNARK function corresponding to the input inductive rule. *)
let step_main
    : type num_input_proofs total_num_input_proofs num_rules prev_vars prev_values a_var a_value max_num_input_proofs max_total_num_input_proofs prev_num_ruless prev_num_input_proofss per_proof_witness per_proof_witness_constant unfinalized unfinalized_constant.
       (module Requests.Step.S
          with type prev_num_input_proofss = prev_num_input_proofss
           and type prev_num_ruless = prev_num_ruless
           and type statement = a_value
           and type prev_values = prev_values
           and type max_num_input_proofs = max_num_input_proofs
           and type per_proof_witnesses = per_proof_witness_constant)
    -> (module Nat.Add.Intf with type n = max_total_num_input_proofs)
    -> num_rules:num_rules Nat.t
    -> prev_num_input_proofss:prev_num_input_proofss H1.T(H1.T(Nat)).t
    -> prev_num_input_proofss_length:( prev_num_input_proofss
                                     , num_input_proofs )
                                     H2.T(Length).t
    -> prev_num_ruless:(* For each inner proof of type T , the number of rules that type T has. *)
       prev_num_ruless H1.T(H1.T(Nat)).t
    -> prev_num_ruless_length:( prev_num_ruless
                              , num_input_proofs )
                              H2.T(Length).t
    -> prevs_lengths:(prev_vars, num_input_proofs) H2.T(Length).t
    -> prevs_length:(num_input_proofs, total_num_input_proofs) Nat.Sum.t
    -> ltes:(num_input_proofs, max_num_input_proofs) H2.T(Nat.Lte).t
    -> max_lengths:(max_num_input_proofs, max_total_num_input_proofs) Nat.Sum.t
    -> proof_systems:( per_proof_witness
                     , per_proof_witness_constant
                     , unfinalized
                     , unfinalized_constant )
                     H4.T(PS).t
    -> basic:( a_var
             , a_value
             , max_total_num_input_proofs
             , num_rules )
             Types_map.Compiled.basic
    -> self:(a_var, a_value, max_total_num_input_proofs, num_rules) Tag.t
    -> ( prev_vars
       , prev_values
       , prev_num_input_proofss
       , prev_num_ruless
       , a_var
       , a_value )
       Inductive_rule.t
    -> (   ( (unfinalized, max_num_input_proofs) H2.T(Vector).t
           , Field.t
           , max_num_input_proofs H1.T(Vector.Carrying(Digest)).t )
           Types.Pairing_based.Statement.t
        -> unit)
       Staged.t =
 fun (module Req) (module Max_num_input_proofs) ~num_rules
     ~prev_num_input_proofss ~prev_num_input_proofss_length ~prev_num_ruless
     ~prev_num_ruless_length ~prevs_lengths ~prevs_length ~ltes ~max_lengths
     ~proof_systems ~basic ~self rule ->
  let module T (F : T4) = struct
    type ('a, 'b, 'n, 'm) t =
      | Other of ('a, 'b, 'n, 'm) F.t
      | Self : (a_var, a_value, max_num_input_proofs, num_rules) t
  end in
  let prev_typ =
    build_combined_typ basic self.id proof_systems rule.prevs
      prev_num_input_proofss prev_num_ruless prevs_lengths
      prev_num_input_proofss_length prev_num_ruless_length
  in
  let main
      (stmt :
        ( (unfinalized, max_num_input_proofs) H2.T(Vector).t
        , Field.t
        , max_num_input_proofs H1.T(Vector.Carrying(Digest)).t )
        Types.Pairing_based.Statement.t) =
    let open Requests.Step in
    let open Impls.Step in
    with_label "step_main" (fun () ->
        let T = Max_num_input_proofs.eq in
        let dlog_plonk_index =
          exists
            ~request:(fun () -> Req.Wrap_index)
            (Plonk_verification_key_evals.typ
               (Typ.array Inner_curve.typ
                  ~length:
                    (index_commitment_length ~max_degree:Max_degree.wrap
                       basic.wrap_domains.h)))
        in
        let app_state = exists basic.typ ~request:(fun () -> Req.App_state) in
        let prevs =
          exists prev_typ ~request:(fun () -> Req.Proof_with_datas)
        in
        let prev_statements =
          let rec f
              : type prev_varss prev_num_input_proofss prev_num_ruless per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants.
                 ( prev_varss
                 , prev_num_input_proofss
                 , prev_num_ruless
                 , per_proof_witnesses )
                 H4.T(H3_1.T(P3)).t
              -> ( per_proof_witnesses
                 , per_proof_witness_constants
                 , unfinalizeds
                 , unfinalized_constants )
                 H4.T(PS).t
              -> prev_varss H1.T(H1.T(Id)).t =
           fun proofss proof_systems ->
            match (proofss, proof_systems) with
            | [], [] ->
                []
            | proofs :: proofss, (module PS) :: proof_systems ->
                let module PPW = struct
                  type t = PS.Step.Types.Per_proof_witness.witness
                end in
                let module M =
                  H3_1.Map1_to_H1 (P3) (Id) (PPW)
                    (struct
                      let f = PS.Step.per_proof_witness_statement
                    end)
                in
                let proofs = M.f proofs in
                let proofss = f proofss proof_systems in
                proofs :: proofss
          in
          f prevs proof_systems
        in
        let proofs_should_verify =
          with_label "rule_main" (fun () -> rule.main prev_statements app_state)
        in
        let unfinalized_proofs =
          h2_vec_to_h2_h1_1 stmt.proof_state.unfinalized_proofs prevs_lengths
            ltes
        in
        let module Packed_digest = Field in
        let module Proof = struct
          type t = Wrap_proof.var
        end in
        let open Pairing_main in
        let pass_throughs =
          with_label "pass_throughs" (fun () ->
              let rec f : type length params actual_length carrying.
                     length H1.T(Vector.Carrying(Impls.Step.Digest)).t
                  -> (params, actual_length) H2.T(Length).t
                  -> (actual_length, length) H2.T(Nat.Lte).t
                  -> params H1.T(H1.T(E01(Digest))).t =
               fun vectors lengths ltes ->
                match (vectors, lengths, ltes) with
                | [], [], [] ->
                    []
                | vector :: vectors, length :: lengths, lte :: ltes ->
                    let module V = H1.Of_vector (Digest) in
                    let hlist = V.f length (Vector.trim vector lte) in
                    let hlists = f vectors lengths ltes in
                    hlist :: hlists
                | _ :: _, [], _ ->
                    .
                | [], _ :: _, _ ->
                    .
              in
              f stmt.pass_through prevs_lengths ltes )
        in
        let self_data :
            ( a_var
            , a_value
            , max_total_num_input_proofs
            , num_rules )
            Types_map.For_step.t =
          { num_rules
          ; rules_num_input_proofs=
              Vector.map basic.rules_num_input_proofs ~f:Field.of_int
          ; max_num_input_proofs= (module Max_num_input_proofs)
          ; num_input_proofs= None
          ; typ= basic.typ
          ; var_to_field_elements= basic.var_to_field_elements
          ; value_to_field_elements= basic.value_to_field_elements
          ; wrap_domains= basic.wrap_domains
          ; step_domains= `Known basic.step_domains
          ; wrap_key= dlog_plonk_index }
        in
        let sgs =
          let rec f
              : type prev_varss prev_num_input_proofss prev_num_ruless num_input_proofss total_num_input_proofs per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants.
                 ( prev_varss
                 , prev_num_input_proofss
                 , prev_num_ruless
                 , per_proof_witnesses )
                 H4.T(H3_1.T(P3)).t
              -> (prev_varss, num_input_proofss) H2.T(Length).t
              -> (num_input_proofss, total_num_input_proofs) Nat.Sum.t
              -> ( per_proof_witnesses
                 , per_proof_witness_constants
                 , unfinalizeds
                 , unfinalized_constants )
                 H4.T(PS).t
              -> (Inner_curve.t, total_num_input_proofs) Vector.t =
           fun proofss lengths sum proof_systems ->
            match (proofss, lengths, sum, proof_systems) with
            | [], [], [], [] ->
                []
            | ( proofs :: proofss
              , length :: lengths
              , add_length :: sum
              , (module PS) :: proof_systems ) ->
                let module M =
                  H3_1.Map1_to_H1
                    (P3)
                    (E01 (Inner_curve))
                    (struct
                      type t = PS.Step.Types.Per_proof_witness.witness
                    end)
                    (struct
                      let f = PS.Step.get_opening_sg
                    end)
                in
                let module V = H1.To_vector (Inner_curve) in
                let sgs = V.f length (M.f proofs) in
                let sgss = f proofss lengths sum proof_systems in
                Vector.append sgs sgss add_length
          in
          let module V = H3.To_vector (Inner_curve) in
          f prevs prevs_lengths prevs_length proof_systems
        in
        let bulletproof_challenges =
          with_label "prevs_verified" (fun () ->
              let rec go
                  : type prev_varss prev_valuess prev_num_input_proofss prev_num_ruless per_proof_witnesses per_proof_witness_constants unfinalizeds unfinalized_constants lengths total_length.
                     ( prev_varss
                     , prev_num_input_proofss
                     , prev_num_ruless
                     , per_proof_witnesses )
                     H4.T(H3_1.T(P3)).t
                  -> ( prev_varss
                     , prev_valuess
                     , prev_num_input_proofss
                     , prev_num_ruless )
                     H4.T(H4.T(Tag)).t
                  -> prev_varss H1.T(H1.T(E01(Digest))).t
                  -> (prev_varss, unfinalizeds) H2.T(H1_1.T(H2.Snd)).t
                  -> prev_varss H1.T(H1.T(E01(B))).t
                  -> (prev_varss, lengths) H2.T(Length).t
                  -> (lengths, total_length) Nat.Sum.t
                  -> ( per_proof_witnesses
                     , per_proof_witness_constants
                     , unfinalizeds
                     , unfinalized_constants )
                     H4.T(PS).t
                  -> ( (Field.t, Backend.Tick.Rounds.n) Vector.t
                     , total_length )
                     Vector.t
                     * B.t list =
               fun proofss tagss pass_throughss unfinalizedss should_verifyss
                   pis sum proof_systems ->
                match
                  ( proofss
                  , tagss
                  , pass_throughss
                  , unfinalizedss
                  , should_verifyss
                  , pis
                  , sum
                  , proof_systems )
                with
                | [], [], [], [], [], [], [], [] ->
                    ([], [])
                | ( [] :: proofss
                  , [] :: tagss
                  , [] :: pass_throughss
                  , [] :: unfinalizedss
                  , [] :: should_verifyss
                  , Z :: pis
                  , Z :: sum
                  , _ :: proof_systems ) ->
                    let chalss, vs =
                      go proofss tagss pass_throughss unfinalizedss
                        should_verifyss pis sum proof_systems
                    in
                    (chalss, vs)
                | ( (p :: proofs) :: proofss
                  , (tag :: tags) :: tagss
                  , (pass_through :: pass_throughs) :: pass_throughss
                  , (unfinalized :: unfinalizeds) :: unfinalizedss
                  , (should_verify :: should_verifys) :: should_verifyss
                  , S pi :: pis
                  , S add_length :: sum
                  , (module PS) :: _ ) ->
                    let chals, verified =
                      PS.Step.finalize_and_verify self_data self.id p tag
                        pass_through unfinalized should_verify
                    in
                    let chalss, vs =
                      go (proofs :: proofss) (tags :: tagss)
                        (pass_throughs :: pass_throughss)
                        (unfinalizeds :: unfinalizedss)
                        (should_verifys :: should_verifyss)
                        (pi :: pis) (add_length :: sum) proof_systems
                    in
                    (chals :: chalss, verified :: vs)
              in
              let chalss, vs =
                go prevs rule.prevs pass_throughs unfinalized_proofs
                  proofs_should_verify prevs_lengths prevs_length proof_systems
              in
              Boolean.Assert.all vs ; chalss )
        in
        let () =
          with_label "hash_me_only" (fun () ->
              let hash_me_only =
                unstage
                  (hash_me_only ~index:dlog_plonk_index
                     basic.var_to_field_elements)
              in
              Field.Assert.equal stmt.proof_state.me_only
                (hash_me_only
                   { app_state
                   ; dlog_plonk_index
                   ; sg= sgs
                   ; old_bulletproof_challenges=
                       (* Note: the bulletproof_challenges here are unpadded! *)
                       bulletproof_challenges }) )
        in
        () )
  in
  stage main
