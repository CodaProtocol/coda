[%%import "/src/config.mlh"]

open Core_kernel

[%%ifdef consensus_mechanism]

open Snark_params.Tick
module Mina_numbers = Mina_numbers

[%%else]

module Mina_numbers = Mina_numbers_nonconsensus.Mina_numbers
module Currency = Currency_nonconsensus.Currency
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module Frozen_ledger_hash = Frozen_ledger_hash0
module Ledger_hash = Ledger_hash0

(* Semantically this type represents a function
     { has_valid_signature: bool; has_valid_proof: bool } -> bool

     These are all of them:
     00 01 10 11 | intuitive definition       | Make sense
     0  0  0  0  | Impossible                 | yes
     0  0  0  1  | Both                       | yes
     0  0  1  0  | Proof and not signature    | no
     0  0  1  1  | Proof                      | yes
     0  1  0  0  | Signature and not proof    | no
     0  1  0  1  | Signature                  | yes
     0  1  1  0  | Exactly one                | no
     0  1  1  1  | Either                     | yes
     1  0  0  0  | Neither                    | no
     1  0  0  1  | Neither or both            | no
     1  0  1  0  | Neither or proof, not both | no
     ...
     1  1  1  1  | None                       | yes

     The ones marked as "not making sense" don't make sense because it is pointless
     to demand a signature failed to verify since you can always make a failing signature
     or proof.

     The ones that make sense are
     0  0  0  0  | Impossible                 | yes
     0  0  0  1  | Both                       | yes
     0  0  1  1  | Proof                      | yes
     0  1  0  1  | Signature                  | yes
     0  1  1  1  | Either                     | yes
     1  1  1  1  | None                       | yes

     "Making sense" can be captured by the idea that these are the *increasing*
     boolean functions on the type { has_valid_signature: bool; has_valid_proof: bool }.
  *)
module Auth_required = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        | None
        | Either
        | Proof
        | Signature
        | Both
        | Impossible (* Both and either can both be subsumed in verification key.
                        It is good to have "Either" as a separate thing to spare the owner from
                        having to make a proof instead of a signature. Both, I'm not sure if there's
                        a good justification for. *)
      [@@deriving sexp, equal, compare, hash, yojson, enum]

      let to_latest = Fn.id
    end
  end]

  (* The encoding is chosen so that it is easy to write this function

      let spec_eval t ~signature_verifies =
        let impossible = (constant t && not (signature_sufficient t)) in
        let result =
          not impossible &&
          ( (signature_verifies && signature_sufficient t)
            || not (signature_necessary t) )
        in
        { result; proof_must_verify= not (signature_sufficient t) } *)

  (* Here is the mapping between our type and the bits
       { constant: bool
       ; signature_necessary: bool
       ; signature_sufficient: bool
       }

       Not constant
        Signature not necessary
          Signature not sufficient
            Proof
          Signature sufficient
            Either
        Signature necessary
          Signature not sufficient
            Both
          Signature sufficient
            Signature

       Constant
        Signature not sufficient
          Impossible
        Signature sufficient
          None
  *)
  module Encoding = struct
    type 'bool t =
      { constant : 'bool
      ; signature_necessary : 'bool
      ; signature_sufficient : 'bool
      }
    [@@deriving hlist, fields]

    let to_input t =
      let [ x; y; z ] = to_hlist t in
      Random_oracle.Input.bitstring [ x; y; z ]

    let map t ~f =
      { constant = f t.constant
      ; signature_necessary = f t.signature_necessary
      ; signature_sufficient = f t.signature_sufficient
      }

    let _ = map

    [%%ifdef consensus_mechanism]

    let if_ b ~then_:t ~else_:e =
      let open Pickles.Impls.Step in
      { constant = Boolean.if_ b ~then_:t.constant ~else_:e.constant
      ; signature_necessary =
          Boolean.if_ b ~then_:t.signature_necessary
            ~else_:e.signature_necessary
      ; signature_sufficient =
          Boolean.if_ b ~then_:t.signature_sufficient
            ~else_:e.signature_sufficient
      }

    [%%endif]
  end

  let encode : t -> bool Encoding.t = function
    | Impossible ->
        { constant = true
        ; signature_necessary = true
        ; signature_sufficient = false
        }
    | None ->
        { constant = true
        ; signature_necessary = false
        ; signature_sufficient = true
        }
    | Proof ->
        { constant = false
        ; signature_necessary = false
        ; signature_sufficient = false
        }
    | Signature ->
        { constant = false
        ; signature_necessary = true
        ; signature_sufficient = true
        }
    | Either ->
        { constant = false
        ; signature_necessary = false
        ; signature_sufficient = true
        }
    | Both ->
        { constant = false
        ; signature_necessary = true
        ; signature_sufficient = false
        }

  let decode : bool Encoding.t -> t = function
    | { constant = true; signature_necessary = _; signature_sufficient = false }
      ->
        Impossible
    | { constant = true; signature_necessary = _; signature_sufficient = true }
      ->
        None
    | { constant = false
      ; signature_necessary = false
      ; signature_sufficient = false
      } ->
        Proof
    | { constant = false
      ; signature_necessary = true
      ; signature_sufficient = true
      } ->
        Signature
    | { constant = false
      ; signature_necessary = false
      ; signature_sufficient = true
      } ->
        Either
    | { constant = false
      ; signature_necessary = true
      ; signature_sufficient = false
      } ->
        Both

  let%test_unit "decode encode" =
    List.iter [ Impossible; Proof; Signature; Either; Both ] ~f:(fun t ->
        [%test_eq: t] t (decode (encode t)))

  [%%ifdef consensus_mechanism]

  module Checked = struct
    type t = Boolean.var Encoding.t

    let if_ = Encoding.if_

    let to_input : t -> _ = Encoding.to_input

    let constant t = Encoding.map (encode t) ~f:Boolean.var_of_value

    let eval_no_proof
        ({ constant; signature_necessary = _; signature_sufficient } : t)
        ~signature_verifies =
      (* ways authorization can succeed when no proof is present:
         - None
           {constant= true; signature_necessary= _; signature_sufficient= true}
         - Either && signature_verifies
           {constant= false; signature_necessary= false; signature_sufficient= true}
         - Signature && signature_verifies
           {constant= false; signature_necessary= true; signature_sufficient= true}
      *)
      let open Pickles.Impls.Step.Boolean in
      signature_sufficient
      &&& (constant ||| ((not constant) &&& signature_verifies))

    let spec_eval ({ constant; signature_necessary; signature_sufficient } : t)
        ~signature_verifies =
      let open Pickles.Impls.Step.Boolean in
      let impossible = constant &&& not signature_sufficient in
      let result =
        (not impossible)
        &&& ( signature_verifies &&& signature_sufficient
            ||| not signature_necessary )
      in
      let didn't_fail_yet = result in
      (* If the transaction already failed to verify, we don't need to assert
         that the proof should verify. *)
      (result, `proof_must_verify (didn't_fail_yet &&& not signature_sufficient))
  end

  let typ =
    let t =
      let open Encoding in
      Typ.of_hlistable
        [ Boolean.typ; Boolean.typ; Boolean.typ ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist
    in
    Typ.transport t ~there:encode ~back:decode

  [%%endif]

  let to_input x = Encoding.to_input (encode x)

  let check (t : t) (c : Control.Tag.t) =
    match (t, c) with
    | Impossible, _ ->
        false
    | None, _ ->
        true
    | Both, (Proof | Signature) ->
        false
    | Proof, Proof ->
        true
    | Signature, Signature ->
        true
    (* The signatures and proofs have already been checked by this point. *)
    | Either, (Proof | Signature) ->
        true
    | Signature, Proof ->
        false
    | Proof, Signature ->
        false
    | (Both | Proof | Signature | Either), None_given ->
        false
end

module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('bool, 'controller) t =
        { stake : 'bool
        ; edit_state : 'controller
        ; send : 'controller
        ; receive : 'controller
        ; set_delegate : 'controller
        ; set_permissions : 'controller
        ; set_verification_key : 'controller
        }
      [@@deriving sexp, equal, compare, hash, yojson, hlist, fields]
    end
  end]

  let to_input controller t =
    let f mk acc field = mk (Core_kernel.Field.get field t) :: acc in
    Stable.Latest.Fields.fold ~init:[]
      ~stake:(f (fun x -> Random_oracle.Input.bitstring [ x ]))
      ~edit_state:(f controller) ~send:(f controller)
      ~set_delegate:(f controller) ~set_permissions:(f controller)
      ~set_verification_key:(f controller) ~receive:(f controller)
    |> List.reduce_exn ~f:Random_oracle.Input.append
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t = (bool, Auth_required.Stable.V1.t) Poly.Stable.V1.t
    [@@deriving sexp, equal, compare, hash, yojson]

    let to_latest = Fn.id
  end
end]

[%%ifdef consensus_mechanism]

module Checked = struct
  type t = (Boolean.var, Auth_required.Checked.t) Poly.Stable.Latest.t

  let to_input x = Poly.to_input Auth_required.Checked.to_input x

  open Pickles.Impls.Step

  let if_ b ~then_ ~else_ =
    let g cond f =
      cond b
        ~then_:(Core_kernel.Field.get f then_)
        ~else_:(Core_kernel.Field.get f else_)
    in
    let c = g Auth_required.Checked.if_ in
    Poly.Fields.map ~stake:(g Boolean.if_) ~edit_state:c ~send:c ~receive:c
      ~set_delegate:c ~set_permissions:c ~set_verification_key:c

  let constant (t : Stable.Latest.t) : t =
    let open Core_kernel.Field in
    let a f = Auth_required.Checked.constant (get f t) in
    Poly.Fields.map
      ~stake:(fun f -> Boolean.var_of_value (get f t))
      ~edit_state:a ~send:a ~receive:a ~set_delegate:a ~set_permissions:a
      ~set_verification_key:a
end

let typ =
  let open Poly.Stable.Latest in
  Typ.of_hlistable
    [ Boolean.typ
    ; Auth_required.typ
    ; Auth_required.typ
    ; Auth_required.typ
    ; Auth_required.typ
    ; Auth_required.typ
    ; Auth_required.typ
    ]
    ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
    ~value_of_hlist:of_hlist

[%%endif]

let to_input x = Poly.to_input Auth_required.to_input x

let user_default : t =
  { stake = true
  ; edit_state = Signature
  ; send = Signature
  ; receive = None
  ; set_delegate = Signature
  ; set_permissions = Signature
  ; set_verification_key = Signature
  }

let empty : t =
  { stake = false
  ; edit_state = None
  ; send = None
  ; receive = None
  ; set_delegate = None
  ; set_permissions = None
  ; set_verification_key = None
  }
