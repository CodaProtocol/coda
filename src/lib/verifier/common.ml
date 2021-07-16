open Core_kernel
open Mina_base

let check :
       User_command.Verifiable.t
    -> [ `Valid of User_command.Valid.t
       | `Invalid
       | `Valid_assuming of User_command.Valid.t * _ list ] = function
  | User_command.Signed_command c -> (
      match Signed_command.check c with
      | None ->
          `Invalid
      | Some c ->
          `Valid (User_command.Signed_command c) )
  | Parties { fee_payer; other_parties; protocol_state } ->
      with_return (fun { return } ->
          let commitment =
            let other_parties_hash =
              match other_parties with
              | [] ->
                  Parties.With_hashes.empty
              | (_, h) :: _ ->
                  h
            in
            Parties.Transaction_commitment.create ~other_parties_hash
              ~protocol_state_predicate_hash:
                (Snapp_predicate.Protocol_state.digest protocol_state)
          in
          let check_signature s pk msg =
            match Signature_lib.Public_key.decompress pk with
            | None ->
                return `Invalid
            | Some pk ->
                if
                  not
                    (Signature_lib.Schnorr.verify s
                       (Backend.Tick.Inner_curve.of_affine pk)
                       (Random_oracle_input.field msg))
                then return `Invalid
                else ()
          in
          check_signature fee_payer.authorization fee_payer.data.body.pk
            (Parties.Transaction_commitment.with_fee_payer commitment
               ~fee_payer_hash:
                 (Party.Predicated.digest
                    (Party.Predicated.of_signed fee_payer.data))) ;
          let valid_assuming =
            List.filter_map other_parties ~f:(fun ((p, vk_opt), at_party) ->
                match p.authorization with
                | Signature s ->
                    check_signature s p.data.body.pk commitment ;
                    None
                | None_given ->
                    None
                | Proof pi -> (
                    match vk_opt with
                    | None ->
                        return `Invalid
                    | Some vk ->
                        Some
                          ( vk
                          , { Snapp_statement.Poly.transaction = commitment
                            ; at_party
                            }
                          , pi ) ))
          in
          let v =
            User_command.Poly.Parties
              { Parties.fee_payer
              ; other_parties = List.map other_parties ~f:(fun ((p, _), _) -> p)
              ; protocol_state
              }
          in
          match valid_assuming with
          | [] ->
              `Valid v
          | _ :: _ ->
              `Valid_assuming (v, valid_assuming))
