open Core_kernel
open Async
open Rosetta_lib

(* Rosetta_models.Currency shadows our Currency so we "save" it as MinaCurrency first *)
module MinaCurrency = Currency
open Rosetta_models
module Signature = Mina_base.Signature
module Transaction = Rosetta_lib.Transaction
module Public_key = Signature_lib.Public_key
module Signed_command_payload = Mina_base.Signed_command_payload
module User_command = Mina_base.User_command
module Signed_command = Mina_base.Signed_command
module Transaction_hash = Mina_base.Transaction_hash

module Get_nonce =
[%graphql
{|
    query get_nonce($public_key: PublicKey!, $token_id: TokenId) {
      account(publicKey: $public_key, token: $token_id) {
        balance {
          blockHeight @bsDecoder(fn: "Decoders.uint32")
          stateHash
        }
        nonce
      }
      daemonStatus {
        chainId
      }
      initialPeers
     }
|}]

module Validate_payment =
[%graphql
{|
  query validate($from: PublicKey!, $to_: PublicKey!, $token: UInt64, $amount: UInt64, $fee: UInt64, $validUntil: UInt64, $memo: String, $nonce: UInt32!, $signature: String!) {
    validatePayment(signature: {rawSignature: $signature}, input: {from: $from, to:$to_, token:$token, amount:$amount, fee:$fee, validUntil: $validUntil, memo: $memo, nonce:$nonce}) }
  |}]

module Send_payment =
[%graphql
{|
  mutation send($from: PublicKey!, $to_: PublicKey!, $token: UInt64, $amount: UInt64, $fee: UInt64, $validUntil: UInt64, $memo: String, $nonce: UInt32!, $signature: String!) {
    sendPayment(signature: {rawSignature: $signature}, input: {from: $from, to:$to_, token:$token, amount:$amount, fee:$fee, validUntil: $validUntil, memo: $memo, nonce:$nonce}) {
      payment {
        hash
      }
  }}
  |}]

module Send_delegation =
[%graphql
{|
mutation ($sender: PublicKey!,
          $receiver: PublicKey!,
          $fee: UInt64!,
          $nonce: UInt32!,
          $memo: String,
          $signature: String!) {
  sendDelegation(signature: {rawSignature: $signature}, input:
    {from: $sender, to: $receiver, fee: $fee, memo: $memo, nonce: $nonce}) {
    delegation {
      hash
    }
  }
}
|}]

module Send_create_token =
[%graphql
{|
mutation ($sender: PublicKey,
          $receiver: PublicKey!,
          $fee: UInt64!,
          $nonce: UInt32,
          $memo: String,
          $signature: String!) {
  createToken(signature: {rawSignature: $signature}, input:
    {feePayer: $sender, tokenOwner: $receiver, fee: $fee, nonce: $nonce, memo: $memo}) {
    createNewToken {
      hash
    }
  }
}
|}]

module Send_create_token_account =
[%graphql
{|
mutation ($sender: PublicKey,
          $tokenOwner: PublicKey!,
          $receiver: PublicKey!,
          $token: TokenId!,
          $fee: UInt64!,
          $nonce: UInt32,
          $memo: String,
          $signature: String!) {
  createTokenAccount(signature: {rawSignature: $signature}, input:
    {feePayer: $sender, tokenOwner: $tokenOwner, receiver: $receiver, token: $token, fee: $fee, nonce: $nonce, memo: $memo}) {
    createNewTokenAccount {
      hash
    }
  }
}
|}]

module Send_mint_tokens =
[%graphql
{|
mutation ($sender: PublicKey!,
          $receiver: PublicKey,
          $token: TokenId!,
          $amount: UInt64!,
          $fee: UInt64!,
          $nonce: UInt32,
          $memo: String,
          $signature: String!) {
  mintTokens(signature: {rawSignature: $signature}, input:
    {tokenOwner: $sender, receiver: $receiver, token: $token, amount: $amount, fee: $fee, nonce: $nonce, memo: $memo}) {
    mintTokens {
      hash
    }
  }
}
|}]

module Options = struct
  type t = {sender: Public_key.Compressed.t; token_id: Unsigned.UInt64.t}

  module Raw = struct
    type t = {sender: string; token_id: string} [@@deriving yojson]
  end

  let to_json t =
    { Raw.sender= Public_key.Compressed.to_base58_check t.sender
    ; token_id= Unsigned.UInt64.to_string t.token_id }
    |> Raw.to_yojson

  let of_json r =
    Raw.of_yojson r
    |> Result.map_error ~f:(fun e ->
           Errors.create ~context:"Options of_json" (`Json_parse (Some e)) )
    |> Result.bind ~f:(fun (r : Raw.t) ->
           let open Result.Let_syntax in
           let%map sender =
             Public_key.Compressed.of_base58_check r.sender
             |> Result.map_error ~f:(fun e ->
                    Errors.create ~context:"Options of_json bad public key"
                      (`Json_parse (Some (Core_kernel.Error.to_string_hum e)))
                )
           in
           {sender; token_id= Unsigned.UInt64.of_string r.token_id} )
end

(* TODO: unify handling of json between this and Options (above) and everything else in rosetta *)
module Metadata_data = struct
  type t =
    { sender: string
    ; nonce: Unsigned_extended.UInt32.t
    ; token_id: Unsigned_extended.UInt64.t }
  [@@deriving yojson]

  let create ~nonce ~sender ~token_id =
    {sender= Public_key.Compressed.to_base58_check sender; nonce; token_id}

  let of_json r =
    of_yojson r
    |> Result.map_error ~f:(fun e ->
           Errors.create ~context:"Options of_json" (`Json_parse (Some e)) )
end

module Derive = struct
  module Env = struct
    module T (M : Monad_fail.S) = struct
      type t = {lift: 'a 'e. ('a, 'e) Result.t -> ('a, 'e) M.t}
    end

    module Real = T (Deferred.Result)
    module Mock = T (Result)

    let real : Real.t = {lift= Deferred.return}

    let mock : Mock.t = {lift= Fn.id}
  end

  module Impl (M : Monad_fail.S) = struct
    module Token_id_decode = Amount_of.Token_id.T (M)

    let handle ~(env : Env.T(M).t) (req : Construction_derive_request.t) =
      let open M.Let_syntax in
      let%bind pk =
        let pk_or_error =
          try Ok (Rosetta_coding.Coding.to_public_key req.public_key.hex_bytes)
          with exn -> Error (Core_kernel.Error.of_exn exn)
        in
        env.lift
        @@ Result.map_error
             ~f:(fun _ -> Errors.create `Malformed_public_key)
             pk_or_error
      in
      let%map token_id = Token_id_decode.decode req.metadata in
      { Construction_derive_response.address= None
      ; account_identifier=
          Some
            (User_command_info.account_id
               (`Pk Public_key.(compress pk |> Compressed.to_base58_check))
               (Option.value ~default:Amount_of.Token_id.default token_id))
      ; metadata= None }
  end

  module Real = Impl (Deferred.Result)
  module Mock = Impl (Result)
end

module Metadata = struct
  module Env = struct
    module T (M : Monad_fail.S) = struct
      type 'gql t =
        { gql:
               ?token_id:Unsigned.UInt64.t
            -> address:Public_key.Compressed.t
            -> unit
            -> ('gql, Errors.t) M.t
        ; validate_network_choice: 'gql Network.Validate_choice.Impl(M).t
        ; lift: 'a 'e. ('a, 'e) Result.t -> ('a, 'e) M.t }
    end

    module Real = T (Deferred.Result)
    module Mock = T (Result)

    let real : graphql_uri:Uri.t -> 'gql Real.t =
     fun ~graphql_uri ->
      { gql=
          (fun ?token_id:_ ~address () ->
            Graphql.query
              (Get_nonce.make
                 ~public_key:
                   (`String (Public_key.Compressed.to_base58_check address))
                   (* for now, nonce is based on the fee payer's account using the default token,
                    per @mrmr1993
                 *)
                 ~token_id:(`String Mina_base.Token_id.(default |> to_string))
                 (* WAS:
                   ( match token_id with
                   | Some x ->
                       `String (Unsigned.UInt64.to_string x)
                   | None ->
                       `Null )
                 *)
                 ())
              graphql_uri )
      ; validate_network_choice= Network.Validate_choice.Real.validate
      ; lift= Deferred.return }
  end

  module Impl (M : Monad_fail.S) = struct
    let handle ~(env : 'gql Env.T(M).t) (req : Construction_metadata_request.t)
        =
      let open M.Let_syntax in
      let%bind req_options =
        match req.options with
        | Some options ->
            M.return options
        | None ->
            M.fail (Errors.create `No_options_provided)
      in
      let%bind options = Options.of_json req_options |> env.lift in
      let%bind res =
        env.gql ~token_id:options.token_id ~address:options.sender ()
      in
      let%bind () =
        env.validate_network_choice ~network_identifier:req.network_identifier
          ~gql_response:res
      in
      let%map account =
        match res#account with
        | None ->
            M.fail
              (Errors.create
                 (`Account_not_found
                   (Public_key.Compressed.to_base58_check options.sender)))
        | Some account ->
            M.return account
      in
      let nonce =
        Option.map
          ~f:(fun nonce -> Unsigned.UInt32.of_string nonce)
          account#nonce
        |> Option.value ~default:Unsigned.UInt32.zero
      in
      let suggested_fee =
        Amount_of.coda
          (MinaCurrency.Fee.to_uint64
             Mina_compile_config.default_transaction_fee)
      in
      let amount_metadata =
        `Assoc
          [ ( "minimum_fee"
            , Amount.to_yojson
                (Amount_of.coda
                   (MinaCurrency.Fee.to_uint64
                      Mina_compile_config.minimum_user_command_fee)) ) ]
      in
      { Construction_metadata_response.metadata=
          Metadata_data.create ~sender:options.Options.sender
            ~token_id:options.Options.token_id ~nonce
          |> Metadata_data.to_yojson
      ; suggested_fee= [{suggested_fee with metadata= Some amount_metadata}] }
  end

  module Real = Impl (Deferred.Result)
  module Mock = Impl (Result)
end

module Preprocess = struct
  module Env = struct
    module T (M : Monad_fail.S) = struct
      type t = {lift: 'a 'e. ('a, 'e) Result.t -> ('a, 'e) M.t}
    end

    module Real = T (Deferred.Result)
    module Mock = T (Result)

    let real : Real.t = {lift= Deferred.return}

    let mock : Mock.t = {lift= Fn.id}
  end

  module Impl (M : Monad_fail.S) = struct
    let lift_reason_validation_to_errors ~(env : Env.T(M).t) t =
      Result.map_error t ~f:(fun reasons ->
          Errors.create (`Operations_not_valid reasons) )
      |> env.lift

    let handle ~(env : Env.T(M).t) (req : Construction_preprocess_request.t) =
      let open M.Let_syntax in
      let%bind partial_user_command =
        User_command_info.of_operations req.operations
        |> lift_reason_validation_to_errors ~env
      in
      let%map pk =
        let (`Pk pk) = partial_user_command.User_command_info.Partial.source in
        Public_key.Compressed.of_base58_check pk
        |> Result.map_error ~f:(fun _ ->
               Errors.create `Public_key_format_not_valid )
        |> env.lift
      in
      { Construction_preprocess_response.options=
          Some
            (Options.to_json
               { Options.sender= pk
               ; token_id= partial_user_command.User_command_info.Partial.token
               })
      ; required_public_keys= [] }
  end

  module Real = Impl (Deferred.Result)
  module Mock = Impl (Result)
end

module Payloads = struct
  module Env = struct
    module T (M : Monad_fail.S) = struct
      type t = {lift: 'a 'e. ('a, 'e) Result.t -> ('a, 'e) M.t}
    end

    module Real = T (Deferred.Result)
    module Mock = T (Result)

    let real : Real.t = {lift= Deferred.return}

    let mock : Mock.t = {lift= Fn.id}
  end

  module Impl (M : Monad_fail.S) = struct
    let lift_reason_validation_to_errors ~(env : Env.T(M).t) t =
      Result.map_error t ~f:(fun reasons ->
          Errors.create (`Operations_not_valid reasons) )
      |> env.lift

    let handle ~(env : Env.T(M).t) (req : Construction_payloads_request.t) =
      let open M.Let_syntax in
      let%bind metadata =
        match req.metadata with
        | Some json ->
            Metadata_data.of_json json |> env.lift
        | None ->
            M.fail
              (Errors.create
                 ~context:"Metadata is required for payloads request"
                 (`Json_parse None))
      in
      let%bind partial_user_command =
        User_command_info.of_operations req.operations
        |> lift_reason_validation_to_errors ~env
      in
      let%bind pk =
        let (`Pk pk) = partial_user_command.User_command_info.Partial.source in
        Public_key.Compressed.of_base58_check pk
        |> Result.map_error ~f:(fun _ ->
               Errors.create ~context:"compression"
                 `Public_key_format_not_valid )
        |> Result.bind ~f:(fun pk ->
               Result.of_option (Public_key.decompress pk)
                 ~error:
                   (Errors.create ~context:"decompression"
                      `Public_key_format_not_valid) )
        |> Result.map ~f:Rosetta_coding.Coding.of_public_key
        |> env.lift
      in
      let%bind user_command_payload =
        User_command_info.Partial.to_user_command_payload ~nonce:metadata.nonce
          partial_user_command
        |> env.lift
      in
      let random_oracle_input = Signed_command.to_input user_command_payload in
      let%map unsigned_transaction_string =
        { Transaction.Unsigned.random_oracle_input
        ; command= partial_user_command
        ; nonce= metadata.nonce }
        |> Transaction.Unsigned.render
        |> Result.map ~f:Transaction.Unsigned.Rendered.to_yojson
        |> Result.map ~f:Yojson.Safe.to_string
        |> env.lift
      in
      { Construction_payloads_response.unsigned_transaction=
          unsigned_transaction_string
      ; payloads=
          [ { Signing_payload.address= None
            ; account_identifier=
                Some
                  (User_command_info.account_id
                     partial_user_command.User_command_info.Partial.source
                     partial_user_command.User_command_info.Partial.token)
            ; hex_bytes= pk
            ; signature_type= Some "schnorr_poseidon" } ] }
  end

  module Real = Impl (Deferred.Result)
  module Mock = Impl (Result)
end

module Combine = struct
  module Env = struct
    module T (M : Monad_fail.S) = struct
      type t = {lift: 'a 'e. ('a, 'e) Result.t -> ('a, 'e) M.t}
    end

    module Real = T (Deferred.Result)
    module Mock = T (Result)

    let real : Real.t = {lift= Deferred.return}

    let mock : Mock.t = {lift= Fn.id}
  end

  module Impl (M : Monad_fail.S) = struct
    let handle ~(env : Env.T(M).t) (req : Construction_combine_request.t) =
      let open M.Let_syntax in
      let%bind json =
        try M.return (Yojson.Safe.from_string req.unsigned_transaction)
        with _ -> M.fail (Errors.create (`Json_parse None))
      in
      let%bind unsigned_transaction =
        Transaction.Unsigned.Rendered.of_yojson json
        |> Result.map_error ~f:(fun e -> Errors.create (`Json_parse (Some e)))
        |> Result.bind ~f:Transaction.Unsigned.of_rendered
        |> env.lift
      in
      (* TODO: validate that public key is correct w.r.t. signature for this transaction *)
      let%bind signature =
        match req.signatures with
        | s :: _ ->
            M.return @@ s.hex_bytes
        | _ ->
            M.fail (Errors.create `Signature_missing)
      in
      let signed_transaction_full =
        { Transaction.Signed.signature
        ; nonce= unsigned_transaction.nonce
        ; command= unsigned_transaction.command }
      in
      let%map rendered =
        Transaction.Signed.render signed_transaction_full |> env.lift
      in
      let signed_transaction =
        Transaction.Signed.Rendered.to_yojson rendered |> Yojson.Safe.to_string
      in
      {Construction_combine_response.signed_transaction}
  end

  module Real = Impl (Deferred.Result)
  module Mock = Impl (Result)
end

module Parse = struct
  module Env = struct
    module T (M : Monad_fail.S) = struct
      type 'gql t =
        { send_validation_request:
               payment:Transaction.Unsigned.Rendered.Payment.t
            -> signature:string
            -> unit
            -> ('gql, Errors.t) M.t
        ; lift: 'a 'e. ('a, 'e) Result.t -> ('a, 'e) M.t }
    end

    module Real = T (Deferred.Result)
    module Mock = T (Result)

    let real : graphql_uri:Uri.t -> 'gql_payment Real.t =
      let uint64 x = `String (Unsigned.UInt64.to_string x) in
      let uint32 x = `String (Unsigned.UInt32.to_string x) in
      fun ~graphql_uri ->
        { send_validation_request=
            (fun ~payment ~signature () ->
              Graphql.query
                (Validate_payment.make ~from:(`String payment.from)
                   ~to_:(`String payment.to_) ~token:(uint64 payment.token)
                   ~amount:(uint64 payment.amount) ~fee:(uint64 payment.fee)
                   ?validUntil:(Option.map ~f:uint32 payment.valid_until)
                   ?memo:payment.memo ~nonce:(uint32 payment.nonce) ~signature
                   ())
                graphql_uri )
        ; lift= Deferred.return }
  end

  module Impl (M : Monad_fail.S) = struct
    let handle ~(env : 'graphql_txn Env.T(M).t)
        (req : Construction_parse_request.t) =
      let open M.Let_syntax in
      let%bind json =
        try M.return (Yojson.Safe.from_string req.transaction)
        with _ -> M.fail (Errors.create (`Json_parse None))
      in
      let%map operations, account_identifier_signers =
        match req.signed with
        | true ->
            let%bind signed_rendered_transaction =
              Transaction.Signed.Rendered.of_yojson json
              |> Result.map_error ~f:(fun e ->
                     Errors.create (`Json_parse (Some e)) )
              |> env.lift
            in
            let%bind signed_transaction =
              Transaction.Signed.of_rendered signed_rendered_transaction
              |> env.lift
            in
            let%map () =
              match signed_rendered_transaction.payment with
              | Some payment ->
                  (* Only perform validation on payments. *)
                  let%bind res =
                    env.send_validation_request ~payment
                      ~signature:signed_transaction.signature ()
                  in
                  if res#validatePayment then M.return ()
                  else M.fail (Errors.create `Signature_invalid)
              | None ->
                  M.return ()
            in
            ( User_command_info.to_operations ~failure_status:None
                signed_transaction.command
            , [ User_command_info.account_id signed_transaction.command.source
                  signed_transaction.command.token ] )
        | false ->
            let%map unsigned_transaction =
              Transaction.Unsigned.Rendered.of_yojson json
              |> Result.map_error ~f:(fun e ->
                     Errors.create (`Json_parse (Some e)) )
              |> Result.bind ~f:Transaction.Unsigned.of_rendered
              |> env.lift
            in
            ( User_command_info.to_operations ~failure_status:None
                unsigned_transaction.command
            , [] )
      in
      { Construction_parse_response.operations
      ; signers= []
      ; account_identifier_signers
      ; metadata= None }
  end

  module Real = Impl (Deferred.Result)
  module Mock = Impl (Result)
end

module Hash = struct
  module Env = struct
    module T (M : Monad_fail.S) = struct
      type t = {lift: 'a 'e. ('a, 'e) Result.t -> ('a, 'e) M.t}
    end

    module Real = T (Deferred.Result)
    module Mock = T (Result)

    let real : Real.t = {lift= Deferred.return}

    let mock : Mock.t = {lift= Fn.id}
  end

  module Impl (M : Monad_fail.S) = struct
    let handle ~(env : Env.T(M).t) (req : Construction_hash_request.t) =
      let open M.Let_syntax in
      let%bind json =
        try M.return (Yojson.Safe.from_string req.signed_transaction)
        with _ -> M.fail (Errors.create (`Json_parse None))
      in
      let%bind signed_transaction =
        Transaction.Signed.Rendered.of_yojson json
        |> Result.map_error ~f:(fun e -> Errors.create (`Json_parse (Some e)))
        |> Result.bind ~f:Transaction.Signed.of_rendered
        |> env.lift
      in
      let%bind signer =
        let (`Pk pk) = signed_transaction.command.source in
        Public_key.Compressed.of_base58_check pk
        |> Result.map_error ~f:(fun _ ->
               Errors.create ~context:"compression"
                 `Public_key_format_not_valid )
        |> Result.bind ~f:(fun pk ->
               Result.of_option (Public_key.decompress pk)
                 ~error:
                   (Errors.create ~context:"decompression"
                      `Public_key_format_not_valid) )
        |> Result.map_error ~f:(fun _ -> Errors.create `Malformed_public_key)
        |> env.lift
      in
      let%bind payload =
        User_command_info.Partial.to_user_command_payload
          ~nonce:signed_transaction.nonce signed_transaction.command
        |> env.lift
      in
      (* TODO: Implement signature coding *)
      let%map signature =
        Result.of_option
          (Signature.Raw.decode signed_transaction.signature)
          ~error:(Errors.create `Signature_missing)
        |> env.lift
      in
      let full_command = {Signed_command.Poly.payload; signature; signer} in
      let hash =
        Transaction_hash.hash_command
          (User_command.Signed_command full_command)
      in
      Construction_hash_response.create (Transaction_hash.to_base58_check hash)
  end

  module Real = Impl (Deferred.Result)
  module Mock = Impl (Result)
end

module Submit = struct
  module Env = struct
    module T (M : Monad_fail.S) = struct
      type ( 'gql_payment
           , 'gql_delegation
           , 'gql_create_token
           , 'gql_create_token_account
           , 'gql_mint_tokens )
           t =
        { gql_payment:
               payment:Transaction.Unsigned.Rendered.Payment.t
            -> signature:string
            -> unit
            -> ('gql_payment, Errors.t) M.t
              (* TODO: Validate network choice with separate query *)
        ; gql_delegation:
               delegation:Transaction.Unsigned.Rendered.Delegation.t
            -> signature:string
            -> unit
            -> ('gql_delegation, Errors.t) M.t
        ; gql_create_token:
               create_token:Transaction.Unsigned.Rendered.Create_token.t
            -> signature:string
            -> unit
            -> ('gql_create_token, Errors.t) M.t
        ; gql_create_token_account:
               create_token_account:Transaction.Unsigned.Rendered
                                    .Create_token_account
                                    .t
            -> signature:string
            -> unit
            -> ('gql_create_token_account, Errors.t) M.t
        ; gql_mint_tokens:
               mint_tokens:Transaction.Unsigned.Rendered.Mint_tokens.t
            -> signature:string
            -> unit
            -> ('gql_mint_tokens, Errors.t) M.t
        ; lift: 'a 'e. ('a, 'e) Result.t -> ('a, 'e) M.t }
    end

    module Real = T (Deferred.Result)
    module Mock = T (Result)

    let real :
           graphql_uri:Uri.t
        -> ( 'gql_payment
           , 'gql_delegation
           , 'gql_create_token
           , 'gql_create_token_account
           , 'gql_mint_tokens )
           Real.t =
      let uint64 x = `String (Unsigned.UInt64.to_string x) in
      let uint32 x = `String (Unsigned.UInt32.to_string x) in
      let token_id x = `String (Mina_base.Token_id.to_string x) in
      fun ~graphql_uri ->
        { gql_payment=
            (fun ~payment ~signature () ->
              Graphql.query
                (Send_payment.make ~from:(`String payment.from)
                   ~to_:(`String payment.to_) ~token:(uint64 payment.token)
                   ~amount:(uint64 payment.amount) ~fee:(uint64 payment.fee)
                   ?validUntil:(Option.map ~f:uint32 payment.valid_until)
                   ?memo:payment.memo ~nonce:(uint32 payment.nonce) ~signature
                   ())
                graphql_uri )
        ; gql_delegation=
            (fun ~delegation ~signature () ->
              Graphql.query
                (Send_delegation.make ~sender:(`String delegation.delegator)
                   ~receiver:(`String delegation.new_delegate)
                   ~fee:
                     (uint64 delegation.fee)
                     (*                   ?validUntil:(Option.map ~f:uint32 delegation.valid_until) *)
                   ?memo:delegation.memo ~nonce:(uint32 delegation.nonce)
                   ~signature ())
                graphql_uri )
        ; gql_create_token=
            (fun ~create_token ~signature () ->
              Graphql.query
                (Send_create_token.make
                   ~receiver:(`String create_token.receiver)
                   ~fee:(uint64 create_token.fee) ?memo:create_token.memo
                   ~nonce:(uint32 create_token.nonce)
                   ~signature ())
                graphql_uri )
        ; gql_create_token_account=
            (fun ~create_token_account ~signature () ->
              Graphql.query
                (Send_create_token_account.make
                   ~tokenOwner:(`String create_token_account.token_owner)
                   ~receiver:(`String create_token_account.receiver)
                   ~token:(token_id create_token_account.token)
                   ~fee:(uint64 create_token_account.fee)
                   ?memo:create_token_account.memo
                   ~nonce:(uint32 create_token_account.nonce)
                   ~signature ())
                graphql_uri )
        ; gql_mint_tokens=
            (fun ~mint_tokens ~signature () ->
              Graphql.query
                (Send_mint_tokens.make
                   ~sender:(`String mint_tokens.token_owner)
                   ~receiver:(`String mint_tokens.receiver)
                   ~token:(token_id mint_tokens.token)
                   ~amount:(uint64 mint_tokens.amount)
                   ~fee:(uint64 mint_tokens.fee) ?memo:mint_tokens.memo
                   ~nonce:(uint32 mint_tokens.nonce) ~signature ())
                graphql_uri )
        ; lift= Deferred.return }
  end

  module Impl (M : Monad_fail.S) = struct
    let handle
        ~(env :
           ( 'gql_payment
           , 'gql_delegation
           , 'gql_create_token
           , 'gql_create_token_account
           , 'gql_mint_tokens )
           Env.T(M).t) (req : Construction_submit_request.t) =
      let open M.Let_syntax in
      let%bind json =
        try M.return (Yojson.Safe.from_string req.signed_transaction)
        with _ -> M.fail (Errors.create (`Json_parse None))
      in
      let%bind signed_transaction =
        Transaction.Signed.Rendered.of_yojson json
        |> Result.map_error ~f:(fun e -> Errors.create (`Json_parse (Some e)))
        |> env.lift
      in
      let open M.Let_syntax in
      let%map hash =
        match
          ( signed_transaction.payment
          , signed_transaction.stake_delegation
          , signed_transaction.create_token
          , signed_transaction.create_token_account
          , signed_transaction.mint_tokens )
        with
        | Some payment, None, None, None, None ->
            let%map res =
              env.gql_payment ~payment ~signature:signed_transaction.signature
                ()
            in
            let (`UserCommand payment) = (res#sendPayment)#payment in
            payment#hash
        | None, Some delegation, None, None, None ->
            let%map res =
              env.gql_delegation ~delegation
                ~signature:signed_transaction.signature ()
            in
            let (`UserCommand delegation) = (res#sendDelegation)#delegation in
            delegation#hash
        | None, None, Some create_token, None, None ->
            let%map res =
              env.gql_create_token ~create_token
                ~signature:signed_transaction.signature ()
            in
            ((res#createToken)#createNewToken)#hash
        | None, None, None, Some create_token_account, None ->
            let%map res =
              env.gql_create_token_account ~create_token_account
                ~signature:signed_transaction.signature ()
            in
            ((res#createTokenAccount)#createNewTokenAccount)#hash
        | None, None, None, None, Some mint_tokens ->
            let%map res =
              env.gql_mint_tokens ~mint_tokens
                ~signature:signed_transaction.signature ()
            in
            ((res#mintTokens)#mintTokens)#hash
        | _ ->
            M.fail
              (Errors.create
                 ~context:
                   "Must have one of payment, stakeDelegation, createToken, \
                    createTokenAccount, or mintTokens"
                 (`Json_parse None))
      in
      { Construction_submit_response.transaction_identifier=
          Transaction_identifier.create hash
      ; metadata= None }
  end

  module Real = Impl (Deferred.Result)
  module Mock = Impl (Result)
end

let router ~get_graphql_uri_or_error ~logger (route : string list) body =
  [%log debug] "Handling /construction/ $route"
    ~metadata:[("route", `List (List.map route ~f:(fun s -> `String s)))] ;
  let open Deferred.Result.Let_syntax in
  match route with
  | ["derive"] ->
      let%bind req =
        Errors.Lift.parse ~context:"Request"
        @@ Construction_derive_request.of_yojson body
        |> Errors.Lift.wrap
      in
      let%map res =
        Derive.Real.handle ~env:Derive.Env.real req |> Errors.Lift.wrap
      in
      Construction_derive_response.to_yojson res
  | ["preprocess"] ->
      let%bind req =
        Errors.Lift.parse ~context:"Request"
        @@ Construction_preprocess_request.of_yojson body
        |> Errors.Lift.wrap
      in
      let%map res =
        Preprocess.Real.handle ~env:Preprocess.Env.real req |> Errors.Lift.wrap
      in
      Construction_preprocess_response.to_yojson res
  | ["metadata"] ->
      let%bind req =
        Errors.Lift.parse ~context:"Request"
        @@ Construction_metadata_request.of_yojson body
        |> Errors.Lift.wrap
      in
      let%bind graphql_uri = get_graphql_uri_or_error () in
      let%map res =
        Metadata.Real.handle ~env:(Metadata.Env.real ~graphql_uri) req
        |> Errors.Lift.wrap
      in
      Construction_metadata_response.to_yojson res
  | ["payloads"] ->
      let%bind req =
        Errors.Lift.parse ~context:"Request"
        @@ Construction_payloads_request.of_yojson body
        |> Errors.Lift.wrap
      in
      let%map res =
        Payloads.Real.handle ~env:Payloads.Env.real req |> Errors.Lift.wrap
      in
      Construction_payloads_response.to_yojson res
  | ["combine"] ->
      let%bind req =
        Errors.Lift.parse ~context:"Request"
        @@ Construction_combine_request.of_yojson body
        |> Errors.Lift.wrap
      in
      let%map res =
        Combine.Real.handle ~env:Combine.Env.real req |> Errors.Lift.wrap
      in
      Construction_combine_response.to_yojson res
  | ["parse"] ->
      let%bind req =
        Errors.Lift.parse ~context:"Request"
        @@ Construction_parse_request.of_yojson body
        |> Errors.Lift.wrap
      in
      let%bind graphql_uri = get_graphql_uri_or_error () in
      let%map res =
        Parse.Real.handle ~env:(Parse.Env.real ~graphql_uri) req
        |> Errors.Lift.wrap
      in
      Construction_parse_response.to_yojson res
  | ["hash"] ->
      let%bind req =
        Errors.Lift.parse ~context:"Request"
        @@ Construction_hash_request.of_yojson body
        |> Errors.Lift.wrap
      in
      let%map res =
        Hash.Real.handle ~env:Hash.Env.real req |> Errors.Lift.wrap
      in
      Construction_hash_response.to_yojson res
  | ["submit"] ->
      let%bind req =
        Errors.Lift.parse ~context:"Request"
        @@ Construction_submit_request.of_yojson body
        |> Errors.Lift.wrap
      in
      let%bind graphql_uri = get_graphql_uri_or_error () in
      let%map res =
        Submit.Real.handle ~env:(Submit.Env.real ~graphql_uri) req
        |> Errors.Lift.wrap
      in
      Construction_submit_response.to_yojson res
  | _ ->
      Deferred.Result.fail `Page_not_found
