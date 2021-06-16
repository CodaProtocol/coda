open Core_kernel
open Mina_numbers
open Mina_base

module Authorization = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        | Proved of Snapp_predicate.Stable.V1.t * Control.Stable.V1.t
        | Signed of Signature.Stable.V1.t (* TODO: Sign choice, like in
                                             User_command_input *)
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]
end

module Party = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Snapp_command.Party.Body.Stable.V1.t
        , Authorization.Stable.V1.t )
        Snapp_command.Party.Predicated.Poly.Stable.V1.t
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]
end

module Second_party = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        | Existing of Party.Stable.V1.t
        | New of Snapp_command.Party.Body.Stable.V1.t
        | Not_given
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Party.Stable.V1.t
      , Second_party.Stable.V1.t )
      Snapp_command.Inner.Stable.V1.t
    [@@deriving sexp]

    let to_latest = Fn.id
  end
end]

(* TODO: Deduplicate with User_command_input *)
let inferred_nonce ~get_current_nonce ~(account_id : Account_id.t) ~nonce_map =
  let open Result.Let_syntax in
  let update_map = Map.set nonce_map ~key:account_id in
  match Map.find nonce_map account_id with
  | Some nonce ->
      (* Multiple commands from the same fee-payer. *)
      let next_nonce = Account_nonce.succ nonce in
      let updated_map = update_map ~data:next_nonce in
      Ok (next_nonce, updated_map)
  | None ->
      let%map txn_pool_or_account_nonce = get_current_nonce account_id in
      let updated_map = update_map ~data:txn_pool_or_account_nonce in
      (txn_pool_or_account_nonce, updated_map)

let to_snapp_command ?(nonce_map = Account_id.Map.empty) ~get_current_nonce
    ({token_id; fee_payment; one; two} : t) :
    (Snapp_command.t * Account_nonce.t Account_id.Map.t, _) Result.t =
  let open Result.Let_syntax in
  let empty body : Snapp_command.Party.Authorized.Empty.t =
    {data= {body; predicate= ()}; authorization= ()}
  in
  let proved body predicate control : Snapp_command.Party.Authorized.Proved.t =
    {data= {body; predicate}; authorization= control}
  in
  let signed ?(nonce_map = nonce_map) (body : Snapp_command.Party.Body.t)
      signature =
    let%map nonce, nonce_map =
      inferred_nonce ~get_current_nonce ~nonce_map
        ~account_id:(Account_id.create body.pk token_id)
    in
    ( ( {data= {body; predicate= nonce}; authorization= signature}
        : Snapp_command.Party.Authorized.Signed.t )
    , nonce_map )
  in
  match (one, two) with
  | {body; predicate= Proved (predicate, control)}, Not_given ->
      return
        ( Snapp_command.Proved_empty
            { token_id
            ; fee_payment
            ; one= proved body predicate control
            ; two= None }
        , nonce_map )
  | {body= body1; predicate= Proved (predicate, control)}, New body2 ->
      return
        ( Snapp_command.Proved_empty
            { token_id
            ; fee_payment
            ; one= proved body1 predicate control
            ; two= Some (empty body2) }
        , nonce_map )
  | ( {body= body1; predicate= Proved (predicate1, control1)}
    , Existing {body= body2; predicate= Proved (predicate2, control2)} ) ->
      return
        ( Snapp_command.Proved_proved
            { token_id
            ; fee_payment
            ; one= proved body1 predicate1 control1
            ; two= proved body2 predicate2 control2 }
        , nonce_map )
  | ( {body= body1; predicate= Proved (predicate, control)}
    , Existing {body= body2; predicate= Signed signature} )
  | ( {body= body2; predicate= Signed signature}
    , Existing {body= body1; predicate= Proved (predicate, control)} ) ->
      let%map two, nonce_map = signed body2 signature in
      ( Snapp_command.Proved_signed
          {token_id; fee_payment; one= proved body1 predicate control; two}
      , nonce_map )
  | {body; predicate= Signed signature}, Not_given ->
      let%map one, nonce_map = signed body signature in
      ( Snapp_command.Signed_empty {token_id; fee_payment; one; two= None}
      , nonce_map )
  | {body= body1; predicate= Signed signature}, New body2 ->
      let%map one, nonce_map = signed body1 signature in
      ( Snapp_command.Signed_empty
          {token_id; fee_payment; one; two= Some (empty body2)}
      , nonce_map )
  | ( {body= body1; predicate= Signed signature1}
    , Existing {body= body2; predicate= Signed signature2} ) ->
      let%bind one, nonce_map = signed body1 signature1 in
      let%map two, nonce_map = signed ~nonce_map body2 signature2 in
      (Snapp_command.Signed_signed {token_id; fee_payment; one; two}, nonce_map)
