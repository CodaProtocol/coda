open Mina_base
open Signature_lib
open Mina_numbers
open Core_kernel
open Async_kernel

module Payload = struct
  module Common = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          ( Currency.Fee.Stable.V1.t
          , Public_key.Compressed.Stable.V1.t
          , Token_id.Stable.V1.t
          , Account_nonce.Stable.V1.t option
          , Global_slot.Stable.V1.t
          , Signed_command_memo.Stable.V1.t )
          Signed_command_payload.Common.Poly.Stable.V1.t
        [@@deriving sexp, to_yojson]

        let to_latest = Fn.id
      end
    end]

    let create ~fee ~fee_token ~fee_payer_pk ?nonce ~valid_until ~memo : t =
      { fee; fee_token; fee_payer_pk; nonce; valid_until; memo }

    let to_user_command_common (t : t) ~minimum_nonce ~inferred_nonce :
        (Signed_command_payload.Common.t, string) Result.t =
      let open Result.Let_syntax in
      let%map nonce =
        match t.nonce with
        | None ->
            (*User did not provide a nonce, use inferred*)
            Ok inferred_nonce
        | Some nonce ->
            (* NB: A lower, explicitly given nonce can be used to cancel
               transactions or to re-issue them with a higher fee.
            *)
            if Account_nonce.(minimum_nonce <= nonce && nonce <= inferred_nonce)
            then Ok nonce
            else
              Error
                (sprintf
                   !"Input nonce %s either different from inferred nonce %s or \
                     below minimum_nonce %s"
                   (Account_nonce.to_string nonce)
                   (Account_nonce.to_string inferred_nonce)
                   (Account_nonce.to_string minimum_nonce))
      in
      { Signed_command_payload.Common.Poly.fee = t.fee
      ; fee_token = t.fee_token
      ; fee_payer_pk = t.fee_payer_pk
      ; nonce
      ; valid_until = t.valid_until
      ; memo = t.memo
      }

    let fee_payer ({ fee_token; fee_payer_pk; _ } : t) =
      Account_id.create fee_payer_pk fee_token
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        ( Common.Stable.V1.t
        , Signed_command_payload.Body.Stable.V1.t )
        Signed_command_payload.Poly.Stable.V1.t
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]

  let create ~fee ~fee_token ~fee_payer_pk ?nonce ~valid_until ~memo ~body : t =
    { common =
        Common.create ~fee ~fee_token ~fee_payer_pk ?nonce ~valid_until ~memo
    ; body
    }

  let to_user_command_payload (t : t) ~minimum_nonce ~inferred_nonce :
      (Signed_command_payload.t, string) Result.t =
    let open Result.Let_syntax in
    let%map common =
      Common.to_user_command_common t.common ~minimum_nonce ~inferred_nonce
    in
    { Signed_command_payload.Poly.common; body = t.body }

  let fee_payer ({ common; _ } : t) = Common.fee_payer common
end

module Sign_choice = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        | Signature of Signature.Stable.V1.t
        | Hd_index of Unsigned_extended.UInt32.Stable.V1.t
        | Keypair of Keypair.Stable.V1.t
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      ( Payload.Stable.V1.t
      , Public_key.Compressed.Stable.V1.t
      , Sign_choice.Stable.V1.t )
      Signed_command.Poly.Stable.V1.t
    [@@deriving sexp, to_yojson]

    let to_latest = Fn.id
  end
end]

[%%define_locally Stable.Latest.(to_yojson)]

let fee_payer ({ payload; _ } : t) = Payload.fee_payer payload

let create ?nonce ~fee ~fee_token ~fee_payer_pk ~valid_until ~memo ~body ~signer
    ~sign_choice () : t =
  let valid_until = Option.value valid_until ~default:Global_slot.max_value in
  let payload =
    Payload.create ~fee ~fee_token ~fee_payer_pk ?nonce ~valid_until ~memo ~body
  in
  { payload; signer; signature = sign_choice }

let sign ~(network_id : int) ~signer
    ~(user_command_payload : Signed_command_payload.t) = function
  | Sign_choice.Signature signature ->
      Option.value_map
        ~default:(Deferred.return (Error "Invalid_signature"))
        (Signed_command.create_with_signature_checked ~network_id signature
           signer user_command_payload)
        ~f:Deferred.Result.return
  | Keypair signer_kp ->
      Deferred.Result.return
        (Signed_command.sign ~network_id signer_kp user_command_payload)
  | Hd_index hd_index ->
      Secrets.Hardware_wallets.sign ~network_id ~hd_index
        ~public_key:(Public_key.decompress_exn signer)
        ~user_command_payload

let inferred_nonce ~get_current_nonce ~(fee_payer : Account_id.t) ~nonce_map =
  let open Result.Let_syntax in
  let update_map = Map.set nonce_map ~key:fee_payer in
  match Map.find nonce_map fee_payer with
  | Some (min_nonce, nonce) ->
      (* Multiple user commands from the same fee-payer. *)
      let next_nonce = Account_nonce.succ nonce in
      let updated_map = update_map ~data:(min_nonce, next_nonce) in
      Ok (min_nonce, next_nonce, updated_map)
  | None ->
      let%map `Min min_nonce, txn_pool_or_account_nonce =
        get_current_nonce fee_payer
      in
      let updated_map =
        update_map ~data:(min_nonce, txn_pool_or_account_nonce)
      in
      (min_nonce, txn_pool_or_account_nonce, updated_map)

(* If the receiver account doesn't exist yet (as far as we can tell) *and* the
 * user command isn't sufficient to cover the account creation fee, log a
 * warning. *)
let warn_if_unable_to_pay_account_creation_fee ~get_account
    ~(constraint_constants : Genesis_constants.Constraint_constants.t) ~logger
    user_command_payload =
  let receiver_pk = Signed_command_payload.receiver_pk user_command_payload in
  let token = Signed_command_payload.token user_command_payload in
  let receiver = Account_id.create receiver_pk token in
  let receiver_account = get_account receiver in
  let amount = Signed_command_payload.amount user_command_payload in
  match (receiver_account, amount) with
  | `Bootstrapping, _ | `Active (Some _), _ | _, None ->
      ()
  | `Active None, Some amount ->
      let open Currency.Amount in
      let account_creation_fee =
        of_fee constraint_constants.account_creation_fee
      in
      if amount < account_creation_fee then
        [%log warn]
          "A transaction was submitted that is likely to fail because the \
           receiver account doesn't appear to have been created already and \
           the transaction amount of %s is smaller than the account creation \
           fee of %s."
          (to_formatted_string amount)
          (to_formatted_string account_creation_fee) ;
      ()

let to_user_command ?(nonce_map = Account_id.Map.empty) ~get_current_nonce
    ~get_account ~constraint_constants ~logger (client_input : t) =
  Deferred.map
    ~f:
      (Result.map_error ~f:(fun str ->
           Error.createf "Error creating user command: %s Error: %s"
             (Yojson.Safe.to_string (to_yojson client_input))
             str))
  @@
  let open Deferred.Result.Let_syntax in
  let fee_payer = fee_payer client_input in
  let%bind minimum_nonce, inferred_nonce, updated_nonce_map =
    inferred_nonce ~get_current_nonce ~fee_payer ~nonce_map |> Deferred.return
  in
  let%bind user_command_payload =
    Payload.to_user_command_payload client_input.payload ~minimum_nonce
      ~inferred_nonce
    |> Deferred.return
  in
  let () =
    warn_if_unable_to_pay_account_creation_fee ~get_account
      ~constraint_constants ~logger user_command_payload
  in
  let%map signed_user_command =
    sign ~network_id:constraint_constants.network_id ~signer:client_input.signer
      ~user_command_payload client_input.signature
  in
  (Signed_command.forget_check signed_user_command, updated_nonce_map)

let to_user_commands ?(nonce_map = Account_id.Map.empty) ~get_current_nonce
    ~get_account ~constraint_constants ~logger uc_inputs :
    Signed_command.t list Deferred.Or_error.t =
  (* When batching multiple user commands, keep track of the nonces and send
      all the user commands if they are valid or none if there is an error in
      one of them.
  *)
  let open Deferred.Or_error.Let_syntax in
  let%map user_commands, _ =
    Deferred.Or_error.List.fold ~init:([], nonce_map) uc_inputs
      ~f:(fun (valid_user_commands, nonce_map) uc_input ->
        let%map res, updated_nonce_map =
          to_user_command ~nonce_map ~get_current_nonce ~get_account
            ~constraint_constants ~logger uc_input
        in
        (res :: valid_user_commands, updated_nonce_map))
  in
  List.rev user_commands
