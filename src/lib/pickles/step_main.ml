module S = Sponge
open Core
open Pickles_types
open Common
open Hlist
open Import
open Impls.Step
open Step_main_inputs
module B = Inductive_rule.B

(* The SNARK function corresponding to the input inductive rule. *)
let step_main
    : type num_input_proofs num_rules prev_vars prev_values a_var a_value max_num_input_proofs prev_num_ruless prev_num_input_proofss.
       (module Requests.Step.S
          with type prev_num_input_proofss = prev_num_input_proofss
           and type prev_num_ruless = prev_num_ruless
           and type statement = a_value
           and type prev_values = prev_values
           and type max_num_input_proofs = max_num_input_proofs)
    -> (module Nat.Add.Intf with type n = max_num_input_proofs)
    -> num_rules:num_rules Nat.t
    -> prev_num_input_proofss:prev_num_input_proofss H1.T(Nat).t
    -> prev_num_input_proofss_length:( prev_num_input_proofss
                                     , num_input_proofs )
                                     Hlist.Length.t
    -> prev_num_ruless:(* For each inner proof of type T , the number of rules that type T has. *)
       prev_num_ruless H1.T(Nat).t
    -> prev_num_ruless_length:( prev_num_ruless
                              , num_input_proofs )
                              Hlist.Length.t
    -> prevs_length:(prev_vars, num_input_proofs) Hlist.Length.t
    -> lte:(num_input_proofs, max_num_input_proofs) Nat.Lte.t
    -> basic:( a_var
             , a_value
             , max_num_input_proofs
             , num_rules )
             Types_map.Compiled.basic
    -> self:(a_var, a_value, max_num_input_proofs, num_rules) Tag.t
    -> ( prev_vars
       , prev_values
       , prev_num_input_proofss
       , prev_num_ruless
       , a_var
       , a_value )
       Inductive_rule.t
    -> (   ( (Unfinalized.t, max_num_input_proofs) Vector.t
           , Field.t
           , (Field.t, max_num_input_proofs) Vector.t )
           Types.Pairing_based.Statement.t
        -> unit)
       Staged.t =
 fun (module Req) (module Max_num_input_proofs) ~num_rules
     ~prev_num_input_proofss ~prev_num_input_proofss_length ~prev_num_ruless
     ~prev_num_ruless_length ~prevs_length ~lte ~basic ~self rule ->
  let module T (F : T4) = struct
    type ('a, 'b, 'n, 'm) t =
      | Other of ('a, 'b, 'n, 'm) F.t
      | Self : (a_var, a_value, max_num_input_proofs, num_rules) t
  end in
  let module Typ_with_max_num_input_proofs = struct
    type ('var, 'value, 'local_max_num_input_proofs, 'local_num_rules) t =
      ( ( 'var
        , 'local_max_num_input_proofs
        , 'local_num_rules )
        Per_proof_witness.t
      , ( 'value
        , 'local_max_num_input_proofs
        , 'local_num_rules )
        Per_proof_witness.Constant.t )
      Typ.t
  end in
  let prev_typs =
    let rec join : type e pvars pvals ns1 ns2 br.
           (pvars, pvals, ns1, ns2) H4.T(Tag).t
        -> ns1 H1.T(Nat).t
        -> ns2 H1.T(Nat).t
        -> (pvars, br) Length.t
        -> (ns1, br) Length.t
        -> (ns2, br) Length.t
        -> (pvars, pvals, ns1, ns2) H4.T(Typ_with_max_num_input_proofs).t =
     fun ds ns1 ns2 ld ln1 ln2 ->
      match (ds, ns1, ns2, ld, ln1, ln2) with
      | [], [], [], Z, Z, Z ->
          []
      | d :: ds, n1 :: ns1, n2 :: ns2, S ld, S ln1, S ln2 ->
          let step_domains, typ =
            (fun (type var value n m) (d : (var, value, n, m) Tag.t) ->
              let typ : (_, m) Vector.t * (var, value) Typ.t =
                match Type_equal.Id.same_witness self.id d.id with
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
                            Side_loaded_verification_key.max_domains_with_x )
                      , d.permanent.typ ) )
              in
              typ )
              d
          in
          let t = Per_proof_witness.typ ~step_domains typ n1 n2 in
          t :: join ds ns1 ns2 ld ln1 ln2
      | [], _, _, _, _, _ ->
          .
      | _ :: _, _, _, _, _, _ ->
          .
    in
    join rule.prevs prev_num_input_proofss prev_num_ruless prevs_length
      prev_num_input_proofss_length prev_num_ruless_length
  in
  let module Prev_typ =
    H4.Typ (Impls.Step) (Typ_with_max_num_input_proofs) (Per_proof_witness)
      (Per_proof_witness.Constant)
      (struct
        let f = Fn.id
      end)
  in
  let main (stmt : _ Types.Pairing_based.Statement.t) =
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
          exists (Prev_typ.f prev_typs) ~request:(fun () ->
              Req.Proof_with_datas )
        in
        let prev_statements =
          let module M =
            H3.Map1_to_H1 (Per_proof_witness) (Id)
              (struct
                let f : type a b c. (a, b, c) Per_proof_witness.t -> a =
                 fun (x, _, _, _, _, _) -> x
              end)
          in
          M.f prevs
        in
        let proofs_should_verify =
          with_label "rule_main" (fun () -> rule.main prev_statements app_state)
        in
        let datas =
          let self_data :
              ( a_var
              , a_value
              , max_num_input_proofs
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
          let module M =
            H4.Map (Tag) (Types_map.For_step)
              (struct
                let f : type a b n m.
                    (a, b, n, m) Tag.t -> (a, b, n, m) Types_map.For_step.t =
                 fun tag ->
                  match Type_equal.Id.same_witness self.id tag.id with
                  | Some T ->
                      self_data
                  | None -> (
                    match tag.kind with
                    | Compiled ->
                        Types_map.For_step.of_compiled
                          (Types_map.lookup_compiled tag.id)
                    | Side_loaded ->
                        Types_map.For_step.of_side_loaded
                          (Types_map.lookup_side_loaded tag.id) )
              end)
          in
          M.f rule.prevs
        in
        let unfinalized_proofs =
          let module H = H1.Of_vector (Unfinalized) in
          H.f prevs_length
            (Vector.trim stmt.proof_state.unfinalized_proofs lte)
        in
        let module Packed_digest = Field in
        let module Proof = struct
          type t = Wrap_proof.var
        end in
        let open Pairing_main in
        let pass_throughs =
          with_label "pass_throughs" (fun () ->
              let module V = H1.Of_vector (Digest) in
              V.f prevs_length (Vector.trim stmt.pass_through lte) )
        in
        let sgs =
          let module M =
            H3.Map
              (Per_proof_witness)
              (E03 (Inner_curve))
              (struct
                let f : type a b c.
                    (a, b, c) Per_proof_witness.t -> Inner_curve.t =
                 fun (_, _, _, _, _, (opening, _)) -> opening.sg
              end)
          in
          let module V = H3.To_vector (Inner_curve) in
          V.f prevs_length (M.f prevs)
        in
        let bulletproof_challenges =
          with_label "prevs_verified" (fun () ->
              let rec go : type vars vals ns1 ns2 n.
                     (vars, ns1, ns2) H3.T(Per_proof_witness).t
                  -> (vars, vals, ns1, ns2) H4.T(Types_map.For_step).t
                  -> vars H1.T(E01(Digest)).t
                  -> vars H1.T(E01(Unfinalized)).t
                  -> vars H1.T(E01(B)).t
                  -> (vars, n) Length.t
                  -> (_, n) Vector.t * B.t list =
               fun proofs datas pass_throughs unfinalizeds should_verifys pi ->
                match
                  ( proofs
                  , datas
                  , pass_throughs
                  , unfinalizeds
                  , should_verifys
                  , pi )
                with
                | [], [], [], [], [], Z ->
                    ([], [])
                | ( p :: proofs
                  , d :: datas
                  , pass_through :: pass_throughs
                  , unfinalized :: unfinalizeds
                  , should_verify :: should_verifys
                  , S pi ) ->
                    Boolean.Assert.( = ) unfinalized.should_finalize
                      should_verify ;
                    let ( app_state
                        , state
                        , prev_evals
                        , sg_old
                        , old_bulletproof_challenges
                        , (opening, messages) ) =
                      p
                    in
                    let finalized, chals =
                      with_label __LOC__ (fun () ->
                          let sponge_digest =
                            state.sponge_digest_before_evaluations
                          in
                          let sponge =
                            let open Step_main_inputs in
                            let sponge = Sponge.create sponge_params in
                            Sponge.absorb sponge (`Field sponge_digest) ;
                            sponge |> Opt_sponge.Underlying.of_sponge
                            |> S.Bit_sponge.make
                          in
                          finalize_other_proof d.max_num_input_proofs
                            ~num_input_proofs:d.num_input_proofs
                            ~rules_num_input_proofs:d.rules_num_input_proofs
                            ~step_domains:d.step_domains ~sponge
                            ~old_bulletproof_challenges state.deferred_values
                            prev_evals )
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
                                    , Vector.init d.num_rules ~f:Field.of_int
                                    )
                                    ~f:Fn.id
                                  |> Types.Index.of_field (module Impl) } } )
                    in
                    let statement =
                      let prev_me_only =
                        with_label __LOC__ (fun () ->
                            let hash =
                              (* TODO: Don't rehash when it's not necessary *)
                              unstage
                                (hash_me_only_opt ~index:d.wrap_key
                                   d.var_to_field_elements)
                            in
                            hash ~num_input_proofs:d.rules_num_input_proofs
                              ~max_num_input_proofs:
                                (Nat.Add.n d.max_num_input_proofs)
                              ~which_rule
                              (* Use opt sponge for cutting off the bulletproof challenges early *)
                              { app_state
                              ; dlog_plonk_index= d.wrap_key
                              ; sg= sg_old
                              ; old_bulletproof_challenges } )
                      in
                      { Types.Dlog_based.Statement.pass_through= prev_me_only
                      ; proof_state= {state with me_only= pass_through} }
                    in
                    let verified =
                      with_label __LOC__ (fun () ->
                          verify ~num_input_proofs:d.max_num_input_proofs
                            ~wrap_domain:d.wrap_domains.h
                            ~is_base_case:should_verify ~sg_old ~opening
                            ~messages ~wrap_verification_key:d.wrap_key
                            statement unfinalized )
                    in
                    if debug then
                      as_prover
                        As_prover.(
                          fun () ->
                            let finalized = read Boolean.typ finalized in
                            let verified = read Boolean.typ verified in
                            let should_verify =
                              read Boolean.typ should_verify
                            in
                            printf "finalized: %b\n%!" finalized ;
                            printf "verified: %b\n%!" verified ;
                            printf "should_verify: %b\n\n%!" should_verify) ;
                    let chalss, vs =
                      go proofs datas pass_throughs unfinalizeds should_verifys
                        pi
                    in
                    ( chals :: chalss
                    , Boolean.(verified &&& finalized ||| not should_verify)
                      :: vs )
              in
              let chalss, vs =
                go prevs datas pass_throughs unfinalized_proofs
                  proofs_should_verify prevs_length
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
