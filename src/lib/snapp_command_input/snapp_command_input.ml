open Core_kernel
open Async_kernel
open Mina_numbers
open Mina_base
open Signature_lib

module Sign_choice = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Signature of Signature.Stable.V1.t | Other
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]
end

module Authorization = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        | Proved of Snapp_predicate.Stable.V1.t * Control.Stable.V1.t
        | Signed of Sign_choice.Stable.V1.t
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

module Other_fee_payer = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { payload:
            ( Public_key.Compressed.Stable.V1.t
            , Token_id.Stable.V1.t
            , Account_nonce.Stable.V1.t option
            , Currency.Fee.Stable.V1.t )
            Other_fee_payer.Payload.Poly.Stable.V1.t
        ; sign_choice: Sign_choice.Stable.V1.t }
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { token_id: Token_id.Stable.V1.t
      ; fee_payment: Other_fee_payer.Stable.V1.t option
      ; one: Party.Stable.V1.t
      ; two: Second_party.Stable.V1.t }
    [@@deriving sexp, to_yojson]

    let to_latest = Fn.id
  end
end]

[%%define_locally
Stable.Latest.(to_yojson)]

let sign ~find_identity ~signer ~(snapp_command : Snapp_command.t) = function
  | Sign_choice.Signature signature ->
      Deferred.Result.return signature
  | Other -> (
    match find_identity ~needle:signer with
    | None ->
        Deferred.Result.fail
          "Couldn't find an unlocked key for specified `sender`. Did you \
           unlock the account you're making a transaction from?"
    | Some (`Keypair {Keypair.private_key; _}) ->
        Deferred.Result.return (Snapp_command.sign snapp_command private_key)
    | Some (`Hd_index hd_index) ->
        Secrets.Hardware_wallets.sign_snapp ~hd_index
          ~public_key:(Public_key.decompress_exn signer)
          ~snapp_payload_digest:(Snapp_command.payload_digest snapp_command) )

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
      (* TODO: Allow user provided nonce and use min nonce *)
      let%map `Min _min_nonce, txn_pool_or_account_nonce =
        get_current_nonce account_id
      in
      let updated_map = update_map ~data:txn_pool_or_account_nonce in
      (txn_pool_or_account_nonce, updated_map)

(* TODO: [nonce_map] may need to be updated regardless of whether we actually
   use it or not. *)
let to_snapp_command ?(nonce_map = Account_id.Map.empty) ~get_current_nonce
    ~find_identity ({token_id; fee_payment; one; two} as snapp_input : t) :
    (Snapp_command.t * Account_nonce.t Account_id.Map.t, _) Deferred.Result.t =
  Deferred.Result.map_error ~f:(fun str ->
      Error.createf "Error creating snapp command: %s Error: %s"
        (Yojson.Safe.to_string (to_yojson snapp_input))
        str )
  @@
  let open Deferred.Result.Let_syntax in
  let empty body : Snapp_command.Party.Authorized.Empty.t =
    {data= {body; predicate= ()}; authorization= ()}
  in
  let proved body predicate control : Snapp_command.Party.Authorized.Proved.t =
    {data= {body; predicate}; authorization= control}
  in
  let signed ?(nonce_map = nonce_map) ~sign (body : Snapp_command.Party.Body.t)
      sign_choice =
    let%bind authorization = sign ~signer:body.pk sign_choice in
    Deferred.return
    @@ let%map.Result.Let_syntax nonce, nonce_map =
         inferred_nonce ~get_current_nonce ~nonce_map
           ~account_id:(Account_id.create body.pk token_id)
       in
       ( ( {data= {body; predicate= nonce}; authorization}
           : Snapp_command.Party.Authorized.Signed.t )
       , nonce_map )
  in
  (* TODO: A fee_payment from one of the snapp accounts will mess up our nonce
     calculation and result in an invalid transaction.
  *)
  let fee_payment ?(nonce_map = nonce_map) ~sign () =
    match fee_payment with
    | Some {payload= {pk; token_id; nonce; fee}; sign_choice} ->
        let%bind nonce, nonce_map =
          match nonce with
          | Some nonce ->
              let next_nonce = Account_nonce.succ nonce in
              return
                ( nonce
                , Map.set nonce_map
                    ~key:(Account_id.create pk token_id)
                    ~data:next_nonce )
          | None ->
              Deferred.return
              @@ inferred_nonce ~get_current_nonce ~nonce_map
                   ~account_id:(Account_id.create pk token_id)
        in
        let%map signature = sign ~signer:pk sign_choice in
        ( ( Some {payload= {pk; token_id; nonce; fee}; signature}
            : Mina_base.Other_fee_payer.t option )
        , nonce_map )
    | None ->
        return (None, nonce_map)
  in
  (* TODO: Make this less bad. *)
  let sign_payload ~f =
    let%bind snapp_command, _ =
      f ~sign:(fun ~signer:_ _ -> Deferred.Result.return Signature.dummy)
    in
    f ~sign:(sign ~find_identity ~snapp_command)
  in
  match (one, two) with
  | {body; predicate= Proved (predicate, control)}, Not_given ->
      sign_payload ~f:(fun ~sign ->
          let%map fee_payment, nonce_map = fee_payment ~sign () in
          ( Snapp_command.Proved_empty
              { token_id
              ; fee_payment
              ; one= proved body predicate control
              ; two= None }
          , nonce_map ) )
  | {body= body1; predicate= Proved (predicate, control)}, New body2 ->
      sign_payload ~f:(fun ~sign ->
          let%map fee_payment, nonce_map = fee_payment ~sign () in
          ( Snapp_command.Proved_empty
              { token_id
              ; fee_payment
              ; one= proved body1 predicate control
              ; two= Some (empty body2) }
          , nonce_map ) )
  | ( {body= body1; predicate= Proved (predicate1, control1)}
    , Existing {body= body2; predicate= Proved (predicate2, control2)} ) ->
      sign_payload ~f:(fun ~sign ->
          let%map fee_payment, nonce_map = fee_payment ~sign () in
          ( Snapp_command.Proved_proved
              { token_id
              ; fee_payment
              ; one= proved body1 predicate1 control1
              ; two= proved body2 predicate2 control2 }
          , nonce_map ) )
  | ( {body= body1; predicate= Proved (predicate, control)}
    , Existing {body= body2; predicate= Signed sign_choice} )
  | ( {body= body2; predicate= Signed sign_choice}
    , Existing {body= body1; predicate= Proved (predicate, control)} ) ->
      sign_payload ~f:(fun ~sign ->
          let%bind two, nonce_map = signed ~sign body2 sign_choice in
          let%map fee_payment, nonce_map = fee_payment ~nonce_map ~sign () in
          ( Snapp_command.Proved_signed
              {token_id; fee_payment; one= proved body1 predicate control; two}
          , nonce_map ) )
  | {body; predicate= Signed sign_choice}, Not_given ->
      sign_payload ~f:(fun ~sign ->
          let%bind one, nonce_map = signed ~sign body sign_choice in
          let%map fee_payment, nonce_map = fee_payment ~nonce_map ~sign () in
          ( Snapp_command.Signed_empty {token_id; fee_payment; one; two= None}
          , nonce_map ) )
  | {body= body1; predicate= Signed sign_choice}, New body2 ->
      sign_payload ~f:(fun ~sign ->
          let%bind one, nonce_map = signed ~sign body1 sign_choice in
          let%map fee_payment, nonce_map = fee_payment ~nonce_map ~sign () in
          ( Snapp_command.Signed_empty
              {token_id; fee_payment; one; two= Some (empty body2)}
          , nonce_map ) )
  | ( {body= body1; predicate= Signed sign_choice1}
    , Existing {body= body2; predicate= Signed sign_choice2} ) ->
      sign_payload ~f:(fun ~sign ->
          let%bind one, nonce_map = signed ~sign body1 sign_choice1 in
          let%bind two, nonce_map =
            signed ~nonce_map ~sign body2 sign_choice2
          in
          let%map fee_payment, nonce_map = fee_payment ~sign ~nonce_map () in
          ( Snapp_command.Signed_signed {token_id; fee_payment; one; two}
          , nonce_map ) )

let to_snapp_commands ?(nonce_map = Account_id.Map.empty) ~get_current_nonce
    ~find_identity uc_inputs : Snapp_command.t list Deferred.Or_error.t =
  (* When batching multiple user commands, keep track of the nonces and send
      all the user commands if they are valid or none if there is an error in
      one of them.
  *)
  let open Deferred.Or_error.Let_syntax in
  let%map snapp_commands, _ =
    Deferred.Or_error.List.fold ~init:([], nonce_map) uc_inputs
      ~f:(fun (valid_snapp_commands, nonce_map) uc_input ->
        let%map res, updated_nonce_map =
          to_snapp_command ~nonce_map ~get_current_nonce ~find_identity
            uc_input
        in
        (res :: valid_snapp_commands, updated_nonce_map) )
  in
  List.rev snapp_commands
