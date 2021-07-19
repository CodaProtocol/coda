open Core_kernel
open Mina_base

let check ~(network_id : int) :
       User_command.Verifiable.t
    -> [ `Valid of User_command.Valid.t
       | `Invalid
       | `Valid_assuming of User_command.Valid.t * _ list ] = function
  | User_command.Signed_command c -> (
      match Signed_command.check ~network_id c with
      | None ->
          `Invalid
      | Some c ->
          `Valid (User_command.Signed_command c) )
  | Snapp_command (cmd, vks) ->
      with_return (fun { return } ->
          let payload =
            lazy
              Snapp_command.(
                Payload.(Digested.digest (digested (to_payload cmd))))
          in
          let check_signature s pk =
            if
              not
                (Signature_lib.Schnorr.verify ~network_id s
                   (Backend.Tick.Inner_curve.of_affine
                      (Signature_lib.Public_key.decompress_exn pk))
                   (Random_oracle_input.field (Lazy.force payload)))
            then return `Invalid
            else ()
          in
          (* TODO: Unify this computation of statement with the code Snapp_statement.of_payload. *)
          let statement_to_check
              ( vk
              , (p : Snapp_command.Party.Authorized.Proved.t)
              , (other : Snapp_command.Party.Body.t) ) =
            let statement : Snapp_statement.t =
              { predicate = p.data.predicate
              ; body1 = p.data.body
              ; body2 = other
              }
            in
            match p.authorization with
            | Signature s ->
                check_signature s p.data.body.pk ;
                None
            | Both { signature; proof } ->
                check_signature signature p.data.body.pk ;
                Some (vk, statement, proof)
            | Proof p ->
                Some (vk, statement, p)
            | None_given ->
                (* TODO: This should probably be an error. *)
                None
          in
          let statements_to_check : _ list =
            List.filter_map ~f:statement_to_check
              ( match (cmd, vks) with
              | Proved_proved r, `Two (vk1, vk2) ->
                  [ (vk1, r.one, r.two.data.body)
                  ; (vk2, r.two, r.one.data.body)
                  ]
              | Proved_signed r, `One vk1 ->
                  check_signature r.two.authorization r.two.data.body.pk ;
                  [ (vk1, r.one, r.two.data.body) ]
              | Proved_empty r, `One vk1 ->
                  let two =
                    Option.value_map r.two
                      ~default:Snapp_command.Party.Body.dummy ~f:(fun two ->
                        two.data.body)
                  in
                  [ (vk1, r.one, two) ]
              | Signed_signed r, `Zero ->
                  check_signature r.one.authorization r.one.data.body.pk ;
                  check_signature r.two.authorization r.two.data.body.pk ;
                  []
              | Signed_empty r, `Zero ->
                  check_signature r.one.authorization r.one.data.body.pk ;
                  []
              | Proved_proved _, (`Zero | `One _)
              | (Proved_signed _ | Proved_empty _), (`Zero | `Two _)
              | (Signed_signed _ | Signed_empty _), (`One _ | `Two _) ->
                  failwith "Wrong number of vks" )
          in
          let v = User_command.Snapp_command cmd in
          match statements_to_check with
          | [] ->
              `Valid v
          | _ :: _ ->
              `Valid_assuming (v, statements_to_check))
