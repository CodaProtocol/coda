open Core
open Pickles

type _ Snarky_backendless.Request.t +=
  | Get_score :
      Pickles.Impls.Step.Internal_Basic.Field.t Snarky_backendless.Request.t

let target_score = 700

let dummy_constraints () =
  let module Impl = Pickles.Impls.Step in
  let module Inner_curve = Pickles.Step_main_inputs.Inner_curve in
  let open Impl in
  make_checked (fun () ->
      let b = exists Boolean.typ_unchecked ~compute:(fun _ -> true) in
      let g =
        exists Inner_curve.typ ~compute:(fun _ -> Inner_curve.Params.one)
      in
      let _ =
        Pickles.Step_main_inputs.Ops.scale_fast g (`Plus_two_to_len [|b; b|])
      in
      let _ =
        Pickles.Pairing_main.Scalar_challenge.endo g (Scalar_challenge [b])
      in
      () )

let main (_ : Mina_base.Snapp_statement.Checked.t) =
  let open Pickles.Impls.Step.Internal_Basic in
  let open Checked.Let_syntax in
  let%bind () = dummy_constraints () in
  let%bind score = exists Field.typ ~request:As_prover.(return Get_score) in
  (* 10 bits because maximum score is 850. *)
  let%bind () =
    as_prover
      As_prover.(
        let%map score = read Field.typ score in
        if Field.(compare score (of_int target_score)) < 0 then
          Format.eprintf
            "The score %s is less than the target score %i.@ Unable to \
             generate a proof.@."
            (Field.to_string score) target_score)
  in
  Field.Checked.Assert.gte ~bit_length:10 score
    Field.(Var.constant (of_int 700))

include Snapp_runner_functor.Make_with_commands (struct
  module Public_input = struct
    module Value = struct
      include Mina_base.Snapp_statement

      let if_not_given = `Raise

      let args : t option Command.Spec.param =
        let open Command in
        let open Command.Let_syntax in
        let%map snapp_pk =
          Command.Param.flag "--snapp-public-key"
            ~doc:"PK Public key of the snapp account"
            (Flag.optional Cli_lib.Arg_type.public_key_compressed)
        and receiver_pk =
          Command.Param.flag "--receiver-public-key"
            ~doc:"PK Public key of the receiver account"
            (Flag.optional Cli_lib.Arg_type.public_key_compressed)
        and fee =
          Command.Param.flag "--fee"
            ~doc:
              "NUM The fee amount for the snapp account to pay the block \
               producer"
            (Flag.optional
               (Arg_type.map ~f:Unsigned.UInt64.of_string Command.Param.string))
        and amount =
          Command.Param.flag "--amount"
            ~doc:
              "NUM The amount to transfer from the snapp account to the \
               receiver"
            (Flag.optional
               (Arg_type.map ~f:Unsigned.UInt64.of_string Command.Param.string))
        and account_creation_fee =
          Command.Param.flag "--account-creation-fee"
            ~doc:
              "NUM The account creation fee, set by the network, to be paid \
               by the snapp account (default: 100000)"
            (Flag.optional_with_default
               (Unsigned.UInt64.of_string "100000")
               (Arg_type.map ~f:Unsigned.UInt64.of_string Command.Param.string))
        in
        let open Mina_base in
        let open Option.Let_syntax in
        let%map snapp_pk = snapp_pk
        and receiver_pk = receiver_pk
        and fee = fee
        and amount = amount in
        let fee = Currency.Amount.of_uint64 fee in
        let amount = Currency.Amount.of_uint64 amount in
        let account_creation_fee =
          Currency.Amount.of_uint64 account_creation_fee
        in
        let snapp_amount =
          match
            Currency.Amount.(fee + amount >>= ( + ) account_creation_fee)
          with
          | Some snapp_amount ->
              snapp_amount
          | None ->
              eprintf
                "Error computing snapp account delta: fee + amount + \
                 account_creation_fee overflowed." ;
              exit 1
        in
        ( { predicate= Snapp_predicate.accept
          ; body1=
              { pk= snapp_pk
              ; update= Snapp_command.Party.Body.dummy.update
              ; delta=
                  Currency.Amount.Signed.(negate (of_unsigned snapp_amount)) }
          ; body2=
              { pk= snapp_pk
              ; update= Snapp_command.Party.Body.dummy.update
              ; delta= Currency.Amount.Signed.of_unsigned amount } }
          : Snapp_statement.t )
    end

    module Var = Mina_base.Snapp_statement.Checked

    let typ = Mina_base.Snapp_statement.typ
  end

  module Request_data = struct
    type t = int

    let handler x (Snarky_backendless.Request.With {request; respond}) =
      match request with
      | Get_score ->
          respond (Provide Pickles.Impls.Step.Field.(Constant.of_int x))
      | _ ->
          respond Unhandled

    let args =
      Command.Param.flag "--score" ~doc:"NUM Credit score to build a proof for"
        (Command.Flag.required Command.Param.int)
  end

  module Branches = Pickles_types.Nat.N1

  let name = "credit-score-demo"

  let default_cache_location =
    Some Filename.(temp_dir_name ^/ "snapp_credit_score_demo")

  let rule =
    { Inductive_rule.prevs= []
    ; identifier= "demo-base"
    ; main=
        (fun [] i ->
          Pickles.Impls.Step.run_checked (main i) ;
          [] )
    ; main_value= (fun [] _ -> []) }
end)

let verify =
  let open Command in
  let open Command.Let_syntax in
  basic ~summary:"Verify a proof"
    (let%map cache = cache_flag
     and public_input =
       Spec.choose_one ~if_nothing_chosen:Input.Public_input.Value.if_not_given
         [ Input.Public_input.Value.args
         ; Spec.flag "--public-input-sexp"
             ~doc:
               "s-expression Enter the public input in the form of an \
                s-expression"
             (Flag.optional
                (Arg_type.Export.sexp_conv Input.Public_input.Value.t_of_sexp))
         ; Spec.flag "--public-input-json"
             ~doc:"json Enter the public input in the json format"
             (Flag.optional
                (Arg_type.create (fun str ->
                     Yojson.Safe.from_string str
                     |> Input.Public_input.Value.of_yojson
                     |> Result.map_error ~f:(fun msg ->
                            Error.createf
                              "Could read the public input from the given \
                               JSON: %s"
                              msg )
                     |> Or_error.ok_exn ))) ]
     and proof =
       Spec.flag "--proof" ~doc:"PROOF The proof to verify"
         (Flag.required Arg_type.Export.string)
     in
     fun () ->
       let _, _, (module Proof), _ = compile ?cache () in
       let proof =
         Base64.decode_exn ~alphabet:Base64.uri_safe_alphabet proof
         |> Binable.of_string (module Side_loaded.Proof.Stable.Latest)
       in
       Format.printf "Proof verified? %b@."
         (Proof.verify [(public_input, proof)]))

let () = run_commands ~additional_commands:[("verify", verify)] ()
