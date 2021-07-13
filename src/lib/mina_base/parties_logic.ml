module type Iffable = sig
  type bool

  type t

  val if_ : bool -> then_:t -> else_:t -> t
end

module type Bool_intf = sig
  type t

  include Iffable with type t := t and type bool := t

  val true_ : t

  val false_ : t

  val equal : t -> t -> t

  val not : t -> t

  val ( ||| ) : t -> t -> t

  val ( &&& ) : t -> t -> t

  val assert_ : t -> unit
end

module type Amount_intf = sig
  include Iffable

  module Signed : sig
    type t

    val is_pos : t -> bool
  end

  val zero : t

  val ( - ) : t -> t -> Signed.t

  val ( + ) : t -> t -> t

  val add_signed : t -> Signed.t -> t
end

module type Token_id_intf = sig
  include Iffable

  val equal : t -> t -> bool

  val invalid : t

  val default : t
end

module Local_state = struct
  open Core_kernel

  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('parties, 'token_id, 'excess, 'ledger, 'bool, 'comm) t =
        { parties: 'parties
              (* Commitment to all parts of the transaction EXCEPT for the fee payer *)
        ; transaction_commitment: 'comm
        ; token_id: 'token_id
        ; excess: 'excess
        ; ledger: 'ledger
        ; success: 'bool
        ; will_succeed: 'bool }
      [@@deriving compare, equal, hash, sexp, yojson, fields, hlist]
    end
  end]

  let typ parties token_id excess ledger bool comm =
    Pickles.Impls.Step.Typ.of_hlistable
      [parties; comm; token_id; excess; ledger; bool; bool]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist
end

module type Parties_intf = sig
  include Iffable

  type party

  val empty : t

  val is_empty : t -> bool

  val pop : t -> party * t
end

module type Ledger_intf = sig
  include Iffable
end

module Eff = struct
  type (_, _) t =
    | Get_account :
        'party * 'ledger
        -> ( 'account * 'inclusion_proof
           , < party: 'party
             ; account: 'account
             ; inclusion_proof: 'inclusion_proof
             ; ledger: 'ledger
             ; .. > )
           t
    | Check_inclusion :
        'ledger * 'account * 'inclusion_proof
        -> ( unit
           , < ledger: 'ledger
             ; inclusion_proof: 'inclusion_proof
             ; account: 'account
             ; .. > )
           t
    | Check_predicate :
        'bool * 'party * 'account * 'global_state
        -> ( 'bool
           , < bool: 'bool
             ; party: 'party
             ; account: 'account
             ; global_state: 'global_state
             ; .. > )
           t
    | Check_protocol_state_predicate :
        'protocol_state_pred * 'global_state
        -> ( 'bool
           , < bool: 'bool
             ; global_state: 'global_state
             ; protocol_state_predicate: 'protocol_state_pred
             ; .. > )
           t
    | Set_account_if :
        'bool * 'ledger * 'account * 'inclusion_proof
        -> ( 'ledger
           , < bool: 'bool
             ; ledger: 'ledger
             ; inclusion_proof: 'inclusion_proof
             ; account: 'account
             ; .. > )
           t
    | Get_global_ledger :
        'global_state
        -> ('ledger, < global_state: 'global_state ; ledger: 'ledger ; .. >) t
    | Modify_global_excess :
        'global_state * ('amount -> 'amount)
        -> ( 'global_state
           , < global_state: 'global_state ; amount: 'amount ; .. > )
           t
    | Modify_global_ledger :
        'global_state * ('ledger -> 'ledger)
        -> ( 'global_state
           , < global_state: 'global_state ; ledger: 'ledger ; .. > )
           t
    | Party_token_id :
        'party
        -> ('token_id, < party: 'party ; token_id: 'token_id ; .. >) t
    | Check_auth_and_update_account :
        { is_start: 'bool
        ; party: 'party
        ; account: 'account
        ; transaction_commitment: 'transaction_commitment
        ; at_party: 'parties
        ; global_state: 'global_state
        ; inclusion_proof: 'ip }
        -> ( 'account * 'bool
           , < inclusion_proof: 'ip
             ; bool: 'bool
             ; party: 'party
             ; parties: 'parties
             ; transaction_commitment: 'transaction_commitment
             ; account: 'account
             ; global_state: 'global_state
             ; .. > )
           t
    | Balance :
        'account
        -> ('amount, < account: 'account ; amount: 'amount ; .. >) t
    | Finalize_local_state :
        'bool * 'local_state
        -> (unit, < local_state: 'local_state ; bool: 'bool ; .. >) t
    | Transaction_commitment_on_start :
        { start_party: 'party
        ; protocol_state_predicate: 'protocol_state_pred
        ; other_parties: 'parties }
        -> ( 'transaction_commitment
           , < party: 'party
             ; parties: 'parties
             ; bool: 'bool
             ; protocol_state_predicate: 'protocol_state_pred
             ; transaction_commitment: 'transaction_commitment
             ; .. > )
           t
end

type 'e handler = {perform: 'r. ('r, 'e) Eff.t -> 'r}

module type Inputs_intf = sig
  module Bool : Bool_intf

  module Ledger : Ledger_intf with type bool := Bool.t

  module Account : sig
    type t
  end

  module Amount : Amount_intf with type bool := Bool.t

  module Token_id : Token_id_intf with type bool := Bool.t

  module Parties : Parties_intf with type bool := Bool.t

  module Transaction_commitment : sig
    include Iffable with type bool := Bool.t

    val empty : t
  end
end

module Start_data = struct
  open Core_kernel

  [%%versioned
  module Stable = struct
    module V1 = struct
      type ('parties, 'protocol_state_pred, 'bool) t =
        { parties: 'parties
        ; protocol_state_predicate: 'protocol_state_pred
        ; will_succeed: 'bool }
    end
  end]
end

module Make (Inputs : Inputs_intf) = struct
  open Inputs
  module Ps = Inputs.Parties

  let apply (type global_state)
      ~(is_start : [`Yes of _ Start_data.t | `No | `Compute of _ Start_data.t])
      (h :
        (< global_state: global_state
         ; transaction_commitment: Transaction_commitment.t
         ; amount: Amount.t
         ; bool: Bool.t
         ; .. >
         as
         'env)
        handler)
      ((global_state : global_state), (local_state : _ Local_state.t)) =
    let open Inputs in
    let is_start' =
      let is_start' = Ps.is_empty local_state.parties in
      Printf.printf "reached line %s\n%!" __LOC__
      |> fun () ->
      ( match is_start with
      | `Compute _ ->
          ()
      | `Yes _ ->
          Bool.assert_ is_start'
      | `No ->
          Bool.assert_ (Bool.not is_start') ) ;
      Printf.printf "reached line %s\n%!" __LOC__
      |> fun () ->
      match is_start with
      | `Yes _ ->
          Bool.true_
      | `No ->
          Bool.false_
      | `Compute _ ->
          is_start'
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let local_state =
      { local_state with
        ledger=
          Inputs.Ledger.if_ is_start'
            ~then_:(h.perform (Get_global_ledger global_state))
            ~else_:local_state.ledger }
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let protocol_state_predicate_satisfied =
      match is_start with
      | `Yes start_data | `Compute start_data ->
          h.perform
            (Check_protocol_state_predicate
               (start_data.protocol_state_predicate, global_state))
      | `No ->
          Bool.true_
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let (party, remaining), at_party, local_state =
      let to_pop =
        match is_start with
        | `Compute start_data ->
            Ps.if_ is_start' ~then_:start_data.parties
              ~else_:local_state.parties
        | `Yes start_data ->
            start_data.parties
        | `No ->
            local_state.parties
      in
      Printf.printf "reached line %s\n%!" __LOC__
      |> fun () ->
      let party, remaining = Ps.pop to_pop in
      Printf.printf "reached line %s\n%!" __LOC__
      |> fun () ->
      let transaction_commitment =
        match is_start with
        | `No ->
            local_state.transaction_commitment
        | `Yes start_data | `Compute start_data ->
            let on_start =
              h.perform
                (Transaction_commitment_on_start
                   { start_party= party
                   ; protocol_state_predicate=
                       start_data.protocol_state_predicate
                   ; other_parties= remaining })
            in
            Transaction_commitment.if_ is_start' ~then_:on_start
              ~else_:local_state.transaction_commitment
      in
      Printf.printf "reached line %s\n%!" __LOC__
      |> fun () ->
      let local_state =
        { local_state with
          transaction_commitment
        ; token_id=
            Token_id.if_ is_start' ~then_:Token_id.default
              ~else_:local_state.token_id }
      in
      Printf.printf "reached line %s\n%!" __LOC__
      |> fun () -> ((party, remaining), to_pop, local_state)
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let local_state =
      { local_state with
        parties= remaining
      ; will_succeed=
          ( match is_start with
          | `Yes start_data ->
              start_data.will_succeed
          | `No ->
              local_state.will_succeed
          | `Compute start_data ->
              Bool.if_ is_start' ~then_:start_data.will_succeed
                ~else_:local_state.will_succeed ) }
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let a, inclusion_proof =
      h.perform (Get_account (party, local_state.ledger))
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    h.perform (Check_inclusion (local_state.ledger, a, inclusion_proof)) ;
    let predicate_satisfied : Bool.t =
      h.perform (Check_predicate (is_start', party, a, global_state))
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let a', update_permitted =
      h.perform
        (Check_auth_and_update_account
           { is_start= is_start'
           ; at_party
           ; global_state
           ; party
           ; account= a
           ; transaction_commitment= local_state.transaction_commitment
           ; inclusion_proof })
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let party_succeeded =
      Bool.(
        protocol_state_predicate_satisfied &&& predicate_satisfied
        &&& update_permitted)
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    (* The first party must succeed. *)
    (*
    Bool.(assert_ ((not is_start') ||| party_succeeded)) ;
    *)
    let bb = Bool.((not is_start') ||| party_succeeded) in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    (* Printf.printf "%s\n%!"
    |> fun () -> *)
    Bool.(if_ (not bb) ~then_:update_permitted ~else_:true_ |> assert_)
    |> fun () ->
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    Bool.(assert_ ((not is_start') ||| party_succeeded))
    |> fun () ->
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let local_state =
      { local_state with
        success= Bool.( &&& ) local_state.success party_succeeded }
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let local_delta =
      (* TODO: This is wasteful as it repeats a computation performed inside
         the account update. *)
      Amount.(h.perform (Balance a) - h.perform (Balance a'))
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let party_token = h.perform (Party_token_id party) in
    Bool.(assert_ (not (Token_id.(equal invalid) party_token))) ;
    let fee_excess_change0, new_local_fee_excess =
      let curr_token : Token_id.t = local_state.token_id in
      let same_token = Token_id.equal curr_token party_token in
      let curr_is_default = Token_id.(equal default) curr_token in
      Bool.(
        assert_
          ( (not is_start')
          ||| ( curr_is_default &&& same_token
              &&& Amount.Signed.is_pos local_delta ) )) ;
      let should_merge = Bool.( &&& ) (Bool.not same_token) curr_is_default in
      let to_merge_amount =
        Amount.if_ should_merge ~then_:local_state.excess ~else_:Amount.zero
      in
      let new_local =
        let base =
          Amount.if_ same_token ~then_:local_state.excess ~else_:Amount.zero
        in
        Amount.add_signed base local_delta
      in
      (to_merge_amount, new_local)
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let local_state = {local_state with excess= new_local_fee_excess} in
    let global_state =
      (* TODO: Maybe overflows should be possible and cause a transaction failure? *)
      h.perform
        (Modify_global_excess (global_state, Amount.( + ) fee_excess_change0))
    in
    (* If a's token ID differs from that in the local state, then
      the local state excess gets moved into the execution state's fee excess.

      If there are more parties to execute after this one, then the local delta gets
      accumulated in the local state.

      If there are no more parties to execute, then we do the same as if we switch tokens.
      The local state excess (plus the local delta) gets moved to the fee excess if it is default token.
    *)
    let new_ledger =
      let should_apply = Bool.( ||| ) is_start' local_state.will_succeed in
      h.perform
        (Set_account_if (should_apply, local_state.ledger, a', inclusion_proof))
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let is_last_party = Ps.is_empty remaining in
    let local_state =
      { local_state with
        ledger= new_ledger
      ; transaction_commitment=
          Transaction_commitment.if_ is_last_party
            ~then_:Transaction_commitment.empty
            ~else_:local_state.transaction_commitment }
    in
    h.perform (Finalize_local_state (is_last_party, local_state)) ;
    (*
       In the SNARK, this will be
    Bool.(assert_
            (not is_last_party |||
            equal local_state.will_succeed local_state.success)) ;
       *)
    let global_state, local_state =
      let curr_is_default = Token_id.(equal default) local_state.token_id in
      let should_perform_second_excess_merge =
        (* See: https://github.com/MinaProtocol/mina/issues/8921 *)
        Bool.(curr_is_default &&& is_last_party)
      in
      ( h.perform
          (Modify_global_excess
             ( global_state
             , fun amt ->
                 Amount.if_ should_perform_second_excess_merge
                   ~then_:Amount.(amt + local_state.excess)
                   ~else_:amt ))
      , { local_state with
          excess=
            Amount.if_ is_last_party ~then_:Amount.zero
              ~else_:local_state.excess } )
    in
    Printf.printf "reached line %s\n%!" __LOC__
    |> fun () ->
    let global_state =
      h.perform
        (Modify_global_ledger
           ( global_state
           , fun curr ->
               Inputs.Ledger.if_ is_last_party ~then_:local_state.ledger
                 ~else_:curr ))
    in
    (global_state, local_state)

  let step h state = apply ~is_start:`No h state

  let start start_data h state = apply ~is_start:(`Yes start_data) h state
end
