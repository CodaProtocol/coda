open Core
open Pickles_types
open Higher_kinded_poly
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
          * ('prev_vars, 'num_input_proofss) H2.T(Length).t
          * ('num_input_proofss, 'num_input_proofs) Nat.Sum.t
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
          -> ( ( Unfinalized.t * unit
               , 'max_num_input_proofs * unit )
               H2.T(Vector).t
             , Impls.Step.Field.t
             , ('max_num_input_proofs * unit)
               H1.T(Vector.Carrying(Impls.Step.Digest)).t )
             Types.Pairing_based.Statement.t
          -> unit
      ; requests:
          (module Requests.Step.S
             with type statement = 'a_value
              and type max_num_input_proofs = 'max_num_input_proofs * unit
              and type prev_values = 'prev_values
              and type prev_num_input_proofss = 'prev_num_input_proofss
              and type prev_num_ruless = 'prev_num_ruless
              and type per_proof_witnesses = P3.W(Per_proof_witness.Constant).t
                                             * unit) }
      -> ( 'a_var
         , 'a_value
         , 'max_num_input_proofs
         , 'num_rules
         , 'prev_vars
         , 'prev_values
         , 'prev_num_input_proofss
         , 'prev_num_ruless )
         t

let rec sum_ltes_exn : type terms1 terms2 total1 total2.
       (terms1, total1) Nat.Sum.t
    -> (terms2, total2) Nat.Sum.t
    -> (terms1, terms2) H2.T(Nat.Lte).t =
 fun xs ys ->
  match (xs, ys) with
  | [], [] ->
      []
  | Z :: xs, _ :: ys ->
      Z :: sum_ltes_exn xs ys
  | S x :: xs, S y :: ys ->
      let (lte :: ltes) = sum_ltes_exn (x :: xs) (y :: ys) in
      S lte :: ltes
  | S _ :: _, Z :: _ ->
      failwith "sum_ltes_exn"
  | _ :: _, [] ->
      failwith "sum_ltes_exn"
  | [], _ :: _ ->
      failwith "sum_ltes_exn"

(* Compile an inductive rule. *)
let create
    (type num_rules max_num_input_proofs prev_num_input_proofss prev_num_ruless
    a_var a_value prev_vars prev_values) ~index
    ~(self : (a_var, a_value, max_num_input_proofs, num_rules) Tag.t)
    ~wrap_domains ~(max_num_input_proofs : max_num_input_proofs Nat.t)
    ~(max_num_input_proofss :
       (max_num_input_proofs * unit, max_num_input_proofs) Nat.Sum.t)
    ~(rules_num_input_proofs : (int, num_rules) Vector.t)
    ~(num_rules : num_rules Nat.t) ~typ var_to_field_elements
    value_to_field_elements (rule : _ Inductive_rule.t) =
  Timer.clock __LOC__ ;
  let module HT = H4.T (Tag) in
  let module HHT = H4.Sum_length (H4.T (Tag)) (HT) in
  let (T (total_num_input_proofs, prevs_lengths, prevs_length)) =
    HHT.length rule.prevs
  in
  let rec extract_lengths : type a b n m k.
         (a, b, n, m) H4.T(H4.T(Tag)).t
      -> (a, k) H2.T(Length).t
      -> n H1.T(H1.T(Nat)).t
         * m H1.T(H1.T(Nat)).t
         * (n, k) H2.T(Length).t
         * (m, k) H2.T(Length).t =
   fun tss ls ->
    match (tss, ls) with
    | [], [] ->
        ([], [], [], [])
    | [] :: tss, Z :: ls ->
        let ns, ms, len_ns, len_ms = extract_lengths tss ls in
        ([] :: ns, [] :: ms, Z :: len_ns, Z :: len_ms)
    | (t :: ts) :: tss, S len :: ls -> (
        let ns :: nss, ms :: mss, len_ns :: len_nss, len_ms :: len_mss =
          extract_lengths (ts :: tss) (len :: ls)
        in
        match Type_equal.Id.same_witness self.id t.id with
        | Some T ->
            ( (max_num_input_proofs :: ns) :: nss
            , (num_rules :: ms) :: mss
            , S len_ns :: len_nss
            , S len_ms :: len_mss )
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
            ( (M.n :: ns) :: nss
            , (num_rules :: ms) :: mss
            , S len_ns :: len_nss
            , S len_ms :: len_mss ) )
  in
  Timer.clock __LOC__ ;
  let [_] = prevs_lengths in
  let ( prev_num_input_proofss
      , prev_num_ruless
      , prev_num_input_proofss_length
      , prev_num_ruless_length ) =
    extract_lengths rule.prevs prevs_lengths
  in
  let lte = Nat.lte_exn total_num_input_proofs max_num_input_proofs in
  let ltes = sum_ltes_exn prevs_length max_num_input_proofss in
  let max_lengths = Nat.Sum.[Nat.Adds.add_zr max_num_input_proofs] in
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
      ~proof_systems:[(module Step_main.Proof_system)]
      ~num_rules ~prevs_lengths ~prevs_length ~prev_num_input_proofss
      ~max_lengths ~prev_num_input_proofss_length ~prev_num_ruless
      ~prev_num_ruless_length ~ltes ~self
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
      Impls.Step.input_of_hlist ~num_input_proofss:[max_num_input_proofs]
        ~per_proof_specs:
          [ Step_main.Proof_system.Step.per_proof_spec
              ~wrap_rounds:Backend.Tock.Rounds.n ]
    in
    Fix_domains.domains (module Impls.Step) etyp main
  in
  Timer.clock __LOC__ ;
  T
    { num_input_proofs= (total_num_input_proofs, prevs_lengths, prevs_length)
    ; index
    ; lte
    ; rule
    ; domains= own_domains
    ; main= step
    ; requests }
