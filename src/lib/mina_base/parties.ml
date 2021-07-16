open Core

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { fee_payer : Party.Signed.Stable.V1.t
      ; other_parties : Party.Stable.V1.t list
      ; protocol_state : Snapp_predicate.Protocol_state.Stable.V1.t
      }
    [@@deriving sexp, compare, equal, hash, yojson]

    let to_latest = Fn.id

    let version_byte = Base58_check.Version_bytes.snapp_command

    let description = "Parties"
  end
end]

include Codable.Make_base58_check (Stable.Latest)

module Valid = struct
  module Stable = Stable

  type t = Stable.Latest.t
end

let check (t : t) =
  let p = t.fee_payer in
  Token_id.(equal default) p.data.body.token_id
  && Sgn.(equal Neg) p.data.body.delta.sgn

let parties (t : t) : Party.t list =
  let p = t.fee_payer in
  { authorization = Control.Signature p.authorization
  ; data = { p.data with predicate = Party.Predicate.Nonce p.data.predicate }
  }
  :: t.other_parties

(** [fee_lower_bound_exn t] may raise if [check t = false] *)
let fee_lower_bound_exn (t : t) : Currency.Fee.t =
  let x = t.fee_payer.data.body.delta in
  match x.sgn with
  | Neg ->
      (* See what happens if all the parties that use up balance succeed,
         and all the non-fee-payer parties that contribute balance fail.

         In the future we could have this function take in the current view of the
         world to get a more accurate lower bound.
      *)
      List.fold_until t.other_parties ~init:x.magnitude ~finish:Fn.id
        ~f:(fun acc p ->
          if Token_id.(p.data.body.token_id <> default) then Continue acc
          else
            let y = p.data.body.delta in
            match y.sgn with
            | Neg ->
                Continue acc
            | Pos -> (
                match Currency.Amount.sub acc y.magnitude with
                | None ->
                    Stop Currency.Amount.zero
                | Some acc' ->
                    Continue acc' ))
      |> Currency.Amount.to_fee
  | Pos ->
      assert false

let fee_payer_party ({ fee_payer; _ } : t) = fee_payer

let fee_payer (t : t) = Party.Signed.account_id (fee_payer_party t)

let nonce (t : t) : Account.Nonce.t = (fee_payer_party t).data.predicate

let fee_token (t : t) = (fee_payer_party t).data.body.token_id

let accounts_accessed (t : t) =
  List.map (parties t) ~f:(fun p ->
      Account_id.create p.data.body.pk p.data.body.token_id)
  |> List.dedup_and_sort ~compare:Account_id.compare

let fee_payer_pk (t : t) = t.fee_payer.data.body.pk

let value_if b ~then_ ~else_ = if b then then_ else else_

module Virtual = struct
  module First_party = Party

  module Bool = struct
    type t = bool

    let true_ = true

    let assert_ _ = ()

    let equal = Bool.equal

    let not = not

    let ( || ) = ( || )

    let ( && ) = ( && )
  end

  module Unit = struct
    type t = unit

    let if_ = value_if
  end

  module Ledger = Unit
  module Account = Unit

  module Amount = struct
    open Currency.Amount

    type nonrec t = t

    let if_ = value_if

    module Signed = Signed

    let zero = zero

    let ( - ) (x1 : t) (x2 : t) : Signed.t =
      Option.value_exn Signed.(of_unsigned x1 + negate (of_unsigned x2))

    let ( + ) (x1 : t) (x2 : t) : t = Option.value_exn (add x1 x2)

    let add_signed (x1 : t) (x2 : Signed.t) : t =
      let y = Option.value_exn Signed.(of_unsigned x1 + x2) in
      match y.sgn with Pos -> y.magnitude | Neg -> failwith "add_signed"
  end

  module Token_id = struct
    include Token_id

    let if_ = value_if
  end

  module Parties = struct
    type t = Party.t list

    let if_ = value_if

    type party = Party.t

    let empty = []

    let is_empty = List.is_empty

    let pop (t : t) = match t with [] -> failwith "pop" | p :: t -> (p, t)
  end
end

module Digest = Zexe_backend.Pasta.Fp

module With_hashes = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type 'a t = ('a * Digest.Stable.V1.t) list
      [@@deriving sexp, compare, equal, hash, yojson]
    end
  end]

  let empty = Outside_hash_image.t

  let cons_hash hash h_tl =
    Random_oracle.hash ~init:Hash_prefix_states.party_cons [| hash; h_tl |]

  let cons ({ hash; data } : ('a, 'h) With_hash.t) (t : 'a t) : 'a t =
    let h_tl = match t with [] -> empty | (_, h_tl) :: _ -> h_tl in
    (data, cons_hash hash h_tl) :: t

  let hash (t : _ t) : Random_oracle.Digest.t =
    match t with [] -> empty | (_, h) :: _ -> h

  let create ps ~hash ~data =
    List.fold ~init:[] (List.rev ps) ~f:(fun acc p ->
        cons { hash = hash p; data = data p } acc)

  let create_all_parties t =
    create ~data:Fn.id
      ~hash:(fun p -> Party.Predicated.digest p.data)
      (Party.of_signed t.fee_payer :: t.other_parties)

  let other_parties_hash t =
    List.fold (List.rev t.other_parties) ~init:empty ~f:(fun acc p ->
        let hash = Party.Predicated.digest p.data in
        cons_hash hash acc)

  let digest (t : _ t) : Random_oracle.Digest.t =
    match t with [] -> empty | (_, h) :: _ -> h
end

module Verifiable = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { fee_payer : Party.Signed.Stable.V1.t
        ; other_parties :
            ( Party.Stable.V1.t
            * Pickles.Side_loaded.Verification_key.Stable.V1.t option )
            With_hashes.Stable.V1.t
        ; protocol_state : Snapp_predicate.Protocol_state.Stable.V1.t
        }
      [@@deriving sexp, compare, equal, hash, yojson]

      let to_latest = Fn.id
    end
  end]
end

let of_verifiable (t : Verifiable.t) : t =
  { fee_payer = t.fee_payer
  ; other_parties = List.map t.other_parties ~f:(fun ((x, _), _) -> x)
  ; protocol_state = t.protocol_state
  }

let valid_interval (t : t) =
  let open Snapp_predicate.Closed_interval in
  match t.protocol_state.curr_global_slot with
  | Ignore ->
      Mina_numbers.Global_slot.{ lower = zero; upper = max_value }
  | Check i ->
      i

module Transaction_commitment = struct
  module Stable = Zexe_backend.Pasta.Fp.Stable

  type t = Stable.Latest.t

  let empty = Outside_hash_image.t

  let typ = Snark_params.Tick.Field.typ

  let create ~other_parties_hash ~protocol_state_predicate_hash : t =
    Random_oracle.hash ~init:Hash_prefix.party_with_protocol_state_predicate
      [| protocol_state_predicate_hash; other_parties_hash |]

  let with_fee_payer (t : t) ~fee_payer_hash =
    Random_oracle.hash ~init:Hash_prefix.party_cons [| fee_payer_hash; t |]

  module Checked = struct
    type t = Pickles.Impls.Step.Field.t

    let create ~other_parties_hash ~protocol_state_predicate_hash =
      Random_oracle.Checked.hash
        ~init:Hash_prefix.party_with_protocol_state_predicate
        [| protocol_state_predicate_hash; other_parties_hash |]

    let with_fee_payer (t : t) ~fee_payer_hash =
      Random_oracle.Checked.hash ~init:Hash_prefix.party_cons
        [| fee_payer_hash; t |]
  end
end

let commitment (t : t) : Transaction_commitment.t =
  Transaction_commitment.create
    ~other_parties_hash:(With_hashes.other_parties_hash t)
    ~protocol_state_predicate_hash:
      (Snapp_predicate.Protocol_state.digest t.protocol_state)
