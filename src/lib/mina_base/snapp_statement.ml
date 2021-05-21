[%%import
"/src/config.mlh"]

open Core_kernel

[%%ifdef
consensus_mechanism]

open Snark_params.Tick
module Mina_numbers = Mina_numbers

[%%else]

module Mina_numbers = Mina_numbers_nonconsensus.Mina_numbers
module Random_oracle = Random_oracle_nonconsensus.Random_oracle

[%%endif]

module Predicate = Snapp_predicate

(*
   For each of the two account's states:
   - before predicate
   - after predicate?
    - set, keep, or ignore
*)

(* This is the statement against which snapp proofs are created. *)
module Poly = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('predicate, 'body) t =
        {predicate: 'predicate; body1: 'body; body2: 'body}
      [@@deriving hlist, sexp, yojson]

      let to_latest = Fn.id
    end
  end]

  let typ spec =
    let open Stable.Latest in
    Typ.of_hlistable spec ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
      ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Predicate.Stable.V1.t
      , Snapp_command.Party.Body.Stable.V1.t )
      Poly.Stable.V1.t
    [@@deriving sexp, yojson]

    let to_latest = Fn.id
  end
end]

module Checked = struct
  open Pickles.Impls.Step

  type t =
    ( (Predicate.Checked.t, Field.t Set_once.t) With_hash.t
    , (Snapp_command.Party.Body.Checked.t, Field.t Set_once.t) With_hash.t )
    Poly.Stable.Latest.t

  let to_field_elements ({predicate; body1; body2} : t) : Field.t array =
    let f hash x =
      let s = With_hash.hash x in
      match Set_once.get s with
      | None ->
          let h = hash (With_hash.data x) in
          Set_once.set_exn s [%here] h ;
          h
      | Some h ->
          h
    in
    let predicate = f Snapp_predicate.Checked.digest predicate in
    let body1 = f Snapp_command.Party.Body.Checked.digest body1 in
    let body2 = f Snapp_command.Party.Body.Checked.digest body2 in
    [|predicate; body1; body2|]
end

let to_field_elements ({predicate; body1; body2} : t) : Field.t array =
  let predicate = Snapp_predicate.digest predicate in
  let body1 = Snapp_command.Party.Body.digest body1 in
  let body2 = Snapp_command.Party.Body.digest body2 in
  [|predicate; body1; body2|]

let typ : (Checked.t, t) Typ.t =
  Poly.typ
    [ Predicate.typ ()
    ; Snapp_command.Party.Body.typ ()
    ; Snapp_command.Party.Body.typ () ]
  |> Typ.transport_var
       ~there:(fun ({predicate; body1; body2} : Checked.t) ->
         { Poly.predicate= With_hash.data predicate
         ; body1= With_hash.data body1
         ; body2= With_hash.data body2 } )
       ~back:(fun ({predicate; body1; body2} : _ Poly.t) ->
         let f = With_hash.of_data ~hash_data:(fun _ -> Set_once.create ()) in
         {Poly.predicate= f predicate; body1= f body1; body2= f body2} )

open Snapp_basic

module Complement = struct
  module One_proved = struct
    module Poly = struct
      type ('bool, 'token_id, 'fee_payer_opt, 'nonce) t =
        { second_starts_empty: 'bool
        ; second_ends_empty: 'bool
        ; token_id: 'token_id
        ; account2_nonce: 'nonce
        ; other_fee_payer_opt: 'fee_payer_opt }
      [@@deriving hlist, sexp, eq, yojson, hash, compare]
    end

    open Mina_numbers

    module Checked = struct
      type t =
        ( Boolean.var
        , Token_id.Checked.t
        , (Boolean.var, Other_fee_payer.Payload.Checked.t) Flagged_option.t
        , Account_nonce.Checked.t )
        Poly.t

      let complete
          ({ second_starts_empty
           ; second_ends_empty
           ; token_id
           ; account2_nonce
           ; other_fee_payer_opt } :
            t) ~one:({predicate; body1; body2} as one : Checked.t) :
          Snapp_command.Payload.One_proved.Digested.Checked.t =
        let _ = Checked.to_field_elements one in
        let ( ! ) x = Set_once.get_exn (With_hash.hash x) [%here] in
        { Snapp_command.Payload.Inner.second_starts_empty
        ; second_ends_empty
        ; token_id
        ; other_fee_payer_opt
        ; one= {predicate= !predicate; body= !body1}
        ; two= {predicate= account2_nonce; body= !body2} }
    end

    type t =
      ( bool
      , Token_id.t
      , Other_fee_payer.Payload.t option
      , Account_nonce.t )
      Poly.t

    let typ : (Checked.t, t) Typ.t =
      let open Poly in
      Typ.of_hlistable
        [ Boolean.typ
        ; Boolean.typ
        ; Token_id.typ
        ; Account_nonce.typ
        ; Flagged_option.typ Other_fee_payer.Payload.typ
          |> Typ.transport
               ~there:
                 (Flagged_option.of_option
                    ~default:Other_fee_payer.Payload.dummy)
               ~back:Flagged_option.to_option ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    let create
        ({ second_starts_empty
         ; second_ends_empty
         ; token_id
         ; other_fee_payer_opt
         ; one= _
         ; two } :
          Snapp_command.Payload.One_proved.t) : t =
      { second_starts_empty
      ; second_ends_empty
      ; token_id
      ; account2_nonce= two.predicate
      ; other_fee_payer_opt }

    let complete
        ({ second_starts_empty
         ; second_ends_empty
         ; token_id
         ; account2_nonce
         ; other_fee_payer_opt } :
          t) ~one:({predicate; body1; body2} : Stable.Latest.t) :
        Snapp_command.Payload.One_proved.t =
      { Snapp_command.Payload.Inner.second_starts_empty
      ; second_ends_empty
      ; token_id
      ; other_fee_payer_opt
      ; one= {predicate; body= body1}
      ; two= {predicate= account2_nonce; body= body2} }
  end

  module Two_proved = struct
    module Poly = struct
      type ('token_id, 'fee_payer_opt) t =
        {token_id: 'token_id; other_fee_payer_opt: 'fee_payer_opt}
      [@@deriving hlist, sexp, eq, yojson, hash, compare]
    end

    type t = (Token_id.t, Other_fee_payer.Payload.t option) Poly.t

    let create
        ({ second_starts_empty= _
         ; second_ends_empty= _
         ; token_id
         ; other_fee_payer_opt
         ; one= _
         ; two= _ } :
          Snapp_command.Payload.Two_proved.t) : t =
      {token_id; other_fee_payer_opt}

    module Checked = struct
      type t =
        ( Token_id.Checked.t
        , (Boolean.var, Other_fee_payer.Payload.Checked.t) Flagged_option.t )
        Poly.t

      let complete ({token_id; other_fee_payer_opt} : t) ~(one : Checked.t)
          ~(two : Checked.t) :
          Snapp_command.Payload.Two_proved.Digested.Checked.t =
        let _ = Checked.to_field_elements one in
        let _ = Checked.to_field_elements two in
        let ( ! ) x = Set_once.get_exn (With_hash.hash x) [%here] in
        { Snapp_command.Payload.Inner.second_starts_empty= Boolean.false_
        ; second_ends_empty= Boolean.false_
        ; token_id
        ; other_fee_payer_opt
            (* one.body2 = two.body1
    two.body2 = one.body1 *)
        ; one= {predicate= !(one.predicate); body= !(one.body1)}
        ; two= {predicate= !(two.predicate); body= !(one.body2)} }
    end

    let typ : (Checked.t, t) Typ.t =
      let open Poly in
      Typ.of_hlistable
        [ Token_id.typ
        ; Flagged_option.typ Other_fee_payer.Payload.typ
          |> Typ.transport
               ~there:
                 (Flagged_option.of_option
                    ~default:Other_fee_payer.Payload.dummy)
               ~back:Flagged_option.to_option ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    let complete ({token_id; other_fee_payer_opt} : t) ~(one : Stable.Latest.t)
        ~(two : Stable.Latest.t) : Snapp_command.Payload.Two_proved.t =
      { Snapp_command.Payload.Inner.second_starts_empty= false
      ; second_ends_empty= false
      ; token_id
      ; other_fee_payer_opt
          (* one.body2 = two.body1
   two.body2 = one.body1 *)
      ; one= {predicate= one.predicate; body= one.body1}
      ; two= {predicate= one.predicate; body= two.body1} }
  end
end
