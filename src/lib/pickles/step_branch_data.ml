open Core
open Pickles_types
open Hlist
open Common
open Import

(* The data obtained from "compiling" an inductive rule into a circuit. *)
type ( 'a_var
     , 'a_value
     , 'max_num_input_proofs
     , 'num_rules
     , 'prev_vars
     , 'prev_values
     , 'prev_num_input_proofss
     , 'prev_num_ruless )
     t =
  | T :
      { num_input_proofs:
          'num_input_proofs Nat.t
          * ('prev_vars, 'num_input_proofs) Hlist.Length.t
      ; index: Types.Index.t
      ; lte: ('num_input_proofs, 'max_num_input_proofs) Nat.Lte.t
      ; domains: Domains.t
      ; rule:
          ( 'prev_vars
          , 'prev_values
          , 'prev_num_input_proofss
          , 'prev_num_ruless
          , 'a_avar
          , 'a_value )
          Inductive_rule.t
      ; main:
             step_domains:(Domains.t, 'num_rules) Vector.t
          -> ( (Unfinalized.t, 'max_num_input_proofs) Vector.t
             , Impls.Step.Field.t
             , (Impls.Step.Field.t, 'max_num_input_proofs) Vector.t )
             Types.Pairing_based.Statement.t
          -> unit
      ; requests:
          (module Requests.Step.S
             with type statement = 'a_value
              and type max_num_input_proofs = 'max_num_input_proofs
              and type prev_values = 'prev_values
              and type prev_num_input_proofss = 'prev_num_input_proofss
              and type prev_num_ruless = 'prev_num_ruless) }
      -> ( 'a_var
         , 'a_value
         , 'max_num_input_proofs
         , 'num_rules
         , 'prev_vars
         , 'prev_values
         , 'prev_num_input_proofss
         , 'prev_num_ruless )
         t

(* Compile an inductive rule. *)
let create
    (type num_rules max_num_input_proofs prev_num_input_proofss prev_num_ruless
    a_var a_value prev_vars prev_values) ~index
    ~(self : (a_var, a_value, max_num_input_proofs, num_rules) Tag.t)
    ~wrap_domains ~(max_num_input_proofs : max_num_input_proofs Nat.t)
    ~(rules_num_input_proofs : (int, num_rules) Vector.t)
    ~(num_rules : num_rules Nat.t) ~typ var_to_field_elements
    value_to_field_elements (rule : _ Inductive_rule.t) =
  Timer.clock __LOC__ ;
  let module HT = H4.T (Tag) in
  let (T (num_input_proofs, prevs_length)) = HT.length rule.prevs in
  let rec extract_lengths : type a b n m k.
         (a, b, n, m) HT.t
      -> (a, k) Length.t
      -> n H1.T(Nat).t * m H1.T(Nat).t * (n, k) Length.t * (m, k) Length.t =
   fun ts len ->
    match (ts, len) with
    | [], Z ->
        ([], [], Z, Z)
    | t :: ts, S len -> (
        let ns, ms, len_ns, len_ms = extract_lengths ts len in
        match Type_equal.Id.same_witness self.id t.id with
        | Some T ->
            (max_num_input_proofs :: ns, num_rules :: ms, S len_ns, S len_ms)
        | None ->
            let (module M), num_rules =
              match t.kind with
              | Compiled ->
                  let d = Types_map.lookup_compiled t.id in
                  (d.max_num_input_proofs, d.num_rules)
              | Side_loaded ->
                  let d = Types_map.lookup_side_loaded t.id in
                  (d.permanent.max_num_input_proofs, d.permanent.num_rules)
            in
            let T = M.eq in
            (M.n :: ns, num_rules :: ms, S len_ns, S len_ms) )
  in
  Timer.clock __LOC__ ;
  let ( prev_num_input_proofss
      , prev_num_ruless
      , prev_num_input_proofss_length
      , prev_num_ruless_length ) =
    extract_lengths rule.prevs prevs_length
  in
  let lte = Nat.lte_exn num_input_proofs max_num_input_proofs in
  let requests = Requests.Step.create () in
  Timer.clock __LOC__ ;
  let step ~step_domains =
    Step_main.step_main requests
      (Nat.Add.create max_num_input_proofs)
      rule
      ~basic:
        { typ
        ; rules_num_input_proofs
        ; var_to_field_elements
        ; value_to_field_elements
        ; wrap_domains
        ; step_domains }
      ~num_rules ~prevs_length ~prev_num_input_proofss
      ~prev_num_input_proofss_length ~prev_num_ruless ~prev_num_ruless_length
      ~lte ~self
    |> unstage
  in
  Timer.clock __LOC__ ;
  let own_domains =
    let main =
      step
        ~step_domains:
          (Vector.init num_rules ~f:(fun _ -> Fix_domains.rough_domains))
    in
    let etyp =
      Impls.Step.input ~num_input_proofs:max_num_input_proofs
        ~wrap_rounds:Backend.Tock.Rounds.n
    in
    Fix_domains.domains (module Impls.Step) etyp main
  in
  Timer.clock __LOC__ ;
  T
    { num_input_proofs= (num_input_proofs, prevs_length)
    ; index
    ; lte
    ; rule
    ; domains= own_domains
    ; main= step
    ; requests }
