open Core_kernel
open Pickles_types
open Import
open Backend

(* We maintain a global hash table which stores for each inductive proof system some
   data.
*)
type inner_curve_var =
  Tick.Field.t Snarky_backendless.Cvar.t
  * Tick.Field.t Snarky_backendless.Cvar.t

module Basic = struct
  type ('var, 'value, 'max_num_input_proofs, 'num_rules) t =
    { max_num_input_proofs:
        (module Nat.Add.Intf with type n = 'max_num_input_proofs)
    ; value_to_field_elements: 'value -> Impls.Step.Field.Constant.t array
    ; var_to_field_elements: 'var -> Impls.Step.Field.t array
    ; typ: ('var, 'value) Impls.Step.Typ.t
    ; num_rules: 'num_rules Nat.t
    ; wrap_domains: Domains.t
    ; wrap_key:
        Tick.Inner_curve.Affine.t
        Dlog_plonk_types.Poly_comm.Without_degree_bound.t
        Plonk_verification_key_evals.t
    ; wrap_vk: Impls.Wrap.Verification_key.t }
end

module Side_loaded = struct
  module Ephemeral = struct
    type t =
      { index:
          [ `In_circuit of Side_loaded_verification_key.Checked.t
          | `In_prover of Side_loaded_verification_key.t ] }
  end

  module Permanent = struct
    type ('var, 'value, 'max_num_input_proofs, 'num_rules) t =
      { max_num_input_proofs:
          (module Nat.Add.Intf with type n = 'max_num_input_proofs)
      ; value_to_field_elements: 'value -> Impls.Step.Field.Constant.t array
      ; var_to_field_elements: 'var -> Impls.Step.Field.t array
      ; typ: ('var, 'value) Impls.Step.Typ.t
      ; num_rules: 'num_rules Nat.t }
  end

  type ('var, 'value, 'n1, 'n2) t =
    { ephemeral: Ephemeral.t option
    ; permanent: ('var, 'value, 'n1, 'n2) Permanent.t }

  type packed =
    | T :
        ('var, 'value, 'n1, 'n2) Tag.tag * ('var, 'value, 'n1, 'n2) t
        -> packed

  let to_basic
      { permanent=
          { max_num_input_proofs
          ; value_to_field_elements
          ; var_to_field_elements
          ; typ
          ; num_rules }
      ; ephemeral } =
    let wrap_key, wrap_vk =
      match ephemeral with
      | Some {index= `In_prover i} ->
          (i.wrap_index, i.wrap_vk)
      | _ ->
          failwithf "Side_loaded.to_basic: Expected `In_prover (%s)" __LOC__ ()
    in
    let wrap_vk = Option.value_exn ~here:[%here] wrap_vk in
    { Basic.max_num_input_proofs
    ; wrap_vk
    ; value_to_field_elements
    ; var_to_field_elements
    ; typ
    ; num_rules
    ; wrap_domains= Common.wrap_domains
    ; wrap_key= Plonk_verification_key_evals.map ~f:Array.of_list wrap_key }
end

module Compiled = struct
  type f = Impls.Wrap.field

  type ('a_var, 'a_value, 'max_num_input_proofs, 'num_rules) basic =
    { typ: ('a_var, 'a_value) Impls.Step.Typ.t
    ; rules_num_input_proofs: (int, 'num_rules) Vector.t
          (* For each rule, how many predecessor proofs does it have? *)
    ; var_to_field_elements: 'a_var -> Impls.Step.Field.t array
    ; value_to_field_elements: 'a_value -> Tick.Field.t array
    ; wrap_domains: Domains.t
    ; step_domains: (Domains.t, 'num_rules) Vector.t }

  (* This is the data associated to an inductive proof system with statement type
   ['a_var], which has ['num_rules] many "variants" each of which depends on at most
   ['max_num_input_proofs] many previous statements. *)
  type ('a_var, 'a_value, 'max_num_input_proofs, 'num_rules) t =
    { num_rules: 'num_rules Nat.t
    ; max_num_input_proofs:
        (module Nat.Add.Intf with type n = 'max_num_input_proofs)
    ; rules_num_input_proofs: (int, 'num_rules) Vector.t
          (* For each rule, how many predecessor proofs does it have? *)
    ; typ: ('a_var, 'a_value) Impls.Step.Typ.t
    ; value_to_field_elements: 'a_value -> Tick.Field.t array
    ; var_to_field_elements: 'a_var -> Impls.Step.Field.t array
    ; wrap_key:
        Tick.Inner_curve.Affine.t
        Dlog_plonk_types.Poly_comm.Without_degree_bound.t
        Plonk_verification_key_evals.t
        Lazy.t
    ; wrap_vk: Impls.Wrap.Verification_key.t Lazy.t
    ; wrap_domains: Domains.t
    ; step_domains: (Domains.t, 'num_rules) Vector.t }

  type packed =
    | T :
        ('var, 'value, 'n1, 'n2) Tag.tag * ('var, 'value, 'n1, 'n2) t
        -> packed

  let to_basic
      { num_rules
      ; max_num_input_proofs
      ; rules_num_input_proofs
      ; typ
      ; value_to_field_elements
      ; var_to_field_elements
      ; wrap_vk
      ; wrap_domains
      ; step_domains
      ; wrap_key } =
    { Basic.max_num_input_proofs
    ; wrap_domains
    ; value_to_field_elements
    ; var_to_field_elements
    ; typ
    ; num_rules= Vector.length step_domains
    ; wrap_key= Lazy.force wrap_key
    ; wrap_vk= Lazy.force wrap_vk }
end

module For_step = struct
  type ('a_var, 'a_value, 'max_num_input_proofs, 'num_rules) t =
    { num_rules: 'num_rules Nat.t
    ; max_num_input_proofs:
        (module Nat.Add.Intf with type n = 'max_num_input_proofs)
    ; rules_num_input_proofs: (Impls.Step.Field.t, 'num_rules) Vector.t
    ; typ: ('a_var, 'a_value) Impls.Step.Typ.t
    ; value_to_field_elements: 'a_value -> Tick.Field.t array
    ; var_to_field_elements: 'a_var -> Impls.Step.Field.t array
    ; wrap_key:
        inner_curve_var Dlog_plonk_types.Poly_comm.Without_degree_bound.t
        Plonk_verification_key_evals.t
    ; wrap_domains: Domains.t
    ; step_domains:
        [ `Known of (Domains.t, 'num_rules) Vector.t
        | `Side_loaded of
          ( Impls.Step.Field.t Side_loaded_verification_key.Domain.t
            Side_loaded_verification_key.Domains.t
          , 'num_rules )
          Vector.t ]
    ; num_input_proofs:
        Side_loaded_verification_key.Num_input_proofs.Checked.t option }

  let of_side_loaded (type a b c d)
      ({ ephemeral
       ; permanent=
           { num_rules
           ; max_num_input_proofs
           ; typ
           ; value_to_field_elements
           ; var_to_field_elements } } :
        (a, b, c, d) Side_loaded.t) : (a, b, c, d) t =
    let index =
      match ephemeral with
      | Some {index= `In_circuit i} ->
          i
      | _ ->
          failwithf "For_step.side_loaded: Expected `In_circuit (%s)" __LOC__
            ()
    in
    let T =
      Nat.eq_exn num_rules Side_loaded_verification_key.Max_num_rules.n
    in
    { num_rules
    ; max_num_input_proofs
    ; rules_num_input_proofs=
        Vector.map index.rules_num_input_proofs
          ~f:Side_loaded_verification_key.Num_input_proofs.Checked.to_field
    ; typ
    ; value_to_field_elements
    ; var_to_field_elements
    ; wrap_key= index.wrap_index
    ; wrap_domains= Common.wrap_domains
    ; step_domains= `Side_loaded index.step_domains
    ; num_input_proofs= Some index.num_input_proofs }

  let of_compiled
      ({ num_rules
       ; max_num_input_proofs
       ; rules_num_input_proofs
       ; typ
       ; value_to_field_elements
       ; var_to_field_elements
       ; wrap_key
       ; wrap_domains
       ; step_domains } :
        _ Compiled.t) =
    { num_rules
    ; num_input_proofs= None
    ; max_num_input_proofs
    ; rules_num_input_proofs=
        Vector.map rules_num_input_proofs ~f:Impls.Step.Field.of_int
    ; typ
    ; value_to_field_elements
    ; var_to_field_elements
    ; wrap_key=
        Plonk_verification_key_evals.map (Lazy.force wrap_key)
          ~f:(Array.map ~f:Step_main_inputs.Inner_curve.constant)
    ; wrap_domains
    ; step_domains= `Known step_domains }
end

type t =
  { compiled: Compiled.packed Type_equal.Id.Uid.Table.t
  ; side_loaded: Side_loaded.packed Type_equal.Id.Uid.Table.t }

let univ : t =
  { compiled= Type_equal.Id.Uid.Table.create ()
  ; side_loaded= Type_equal.Id.Uid.Table.create () }

let find t k =
  match Hashtbl.find t k with None -> failwith "key not found" | Some x -> x

let lookup_compiled : type a b n m.
    (a, b, n, m) Tag.tag -> (a, b, n, m) Compiled.t =
 fun t ->
  let (T (other_id, d)) = find univ.compiled (Type_equal.Id.uid t) in
  let T = Type_equal.Id.same_witness_exn t other_id in
  d

let lookup_side_loaded : type a b n m.
    (a, b, n, m) Tag.tag -> (a, b, n, m) Side_loaded.t =
 fun t ->
  let (T (other_id, d)) = find univ.side_loaded (Type_equal.Id.uid t) in
  let T = Type_equal.Id.same_witness_exn t other_id in
  d

let lookup_basic : type a b n m. (a, b, n, m) Tag.t -> (a, b, n, m) Basic.t =
 fun t ->
  match t.kind with
  | Compiled ->
      Compiled.to_basic (lookup_compiled t.id)
  | Side_loaded ->
      Side_loaded.to_basic (lookup_side_loaded t.id)

let lookup_step_domains : type a b n m.
    (a, b, n, m) Tag.t -> (Domain.t, m) Vector.t =
 fun t ->
  let f = Vector.map ~f:Domains.h in
  match t.kind with
  | Compiled ->
      f (lookup_compiled t.id).step_domains
  | Side_loaded -> (
      let t = lookup_side_loaded t.id in
      match t.ephemeral with
      | Some {index= `In_circuit _} | None ->
          failwith __LOC__
      | Some {index= `In_prover k} ->
          let a =
            At_most.to_array (At_most.map k.step_data ~f:(fun (ds, _) -> ds.h))
          in
          Vector.init t.permanent.num_rules ~f:(fun i ->
              try a.(i) with _ -> Domain.Pow_2_roots_of_unity 0 ) )

let max_num_input_proofs : type n1.
    (_, _, n1, _) Tag.t -> (module Nat.Add.Intf with type n = n1) =
 fun tag ->
  match tag.kind with
  | Compiled ->
      (lookup_compiled tag.id).max_num_input_proofs
  | Side_loaded ->
      (lookup_side_loaded tag.id).permanent.max_num_input_proofs

let typ : type var value.
    (var, value, _, _) Tag.t -> (var, value) Impls.Step.Typ.t =
 fun tag ->
  match tag.kind with
  | Compiled ->
      (lookup_compiled tag.id).typ
  | Side_loaded ->
      (lookup_side_loaded tag.id).permanent.typ

let value_to_field_elements : type a.
    (_, a, _, _) Tag.t -> a -> Tick.Field.t array =
 fun t ->
  match t.kind with
  | Compiled ->
      (lookup_compiled t.id).value_to_field_elements
  | Side_loaded ->
      (lookup_side_loaded t.id).permanent.value_to_field_elements

let lookup_map (type a b c d) (t : (a, b, c, d) Tag.t) ~self ~default
    ~(f :
          [ `Compiled of (a, b, c, d) Compiled.t
          | `Side_loaded of (a, b, c, d) Side_loaded.t ]
       -> _) =
  match Type_equal.Id.same_witness t.id self with
  | Some _ ->
      default
  | None -> (
    match t.kind with
    | Compiled ->
        let (T (other_id, d)) = find univ.compiled (Type_equal.Id.uid t.id) in
        let T = Type_equal.Id.same_witness_exn t.id other_id in
        f (`Compiled d)
    | Side_loaded ->
        let (T (other_id, d)) =
          find univ.side_loaded (Type_equal.Id.uid t.id)
        in
        let T = Type_equal.Id.same_witness_exn t.id other_id in
        f (`Side_loaded d) )

let add_side_loaded ~name permanent =
  let id = Type_equal.Id.create ~name sexp_of_opaque in
  Hashtbl.add_exn univ.side_loaded ~key:(Type_equal.Id.uid id)
    ~data:(T (id, {ephemeral= None; permanent})) ;
  {Tag.kind= Side_loaded; id}

let set_ephemeral {Tag.kind; id} eph =
  (match kind with Side_loaded -> () | _ -> failwith "Expected Side_loaded") ;
  Hashtbl.update univ.side_loaded (Type_equal.Id.uid id) ~f:(function
    | None ->
        assert false
    | Some (T (id, d)) ->
        T (id, {d with ephemeral= Some eph}) )

let add_exn (type a b c d) (tag : (a, b, c, d) Tag.t)
    (data : (a, b, c, d) Compiled.t) =
  Hashtbl.add_exn univ.compiled ~key:(Type_equal.Id.uid tag.id)
    ~data:(Compiled.T (tag.id, data))
