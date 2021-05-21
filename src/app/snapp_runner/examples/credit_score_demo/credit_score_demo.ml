open Core
open Pickles

type _ Snarky_backendless.Request.t +=
  | Get_score :
      Pickles.Impls.Step.Internal_Basic.Field.t Snarky_backendless.Request.t

let target_score = 700

module Eth_address : sig
  type t

  val of_string : string -> t

  val to_string : t -> string

  val to_field : t -> Impls.Step.Field.Constant.t

  val of_field : Impls.Step.Field.Constant.t -> t
end = struct
  type t = string

  let of_string s =
    let s = String.chop_prefix_exn ~prefix:"0x" s in
    assert (String.length s = 40) ;
    Hex.decode ~init:String.init s

  let to_string t = "0x" ^ Hex.encode t

  let to_bits t =
    List.init
      (String.length t * 8)
      ~f:(fun i ->
        let j = i mod 8 in
        (Char.to_int t.[i / 8] lsr j) land 1 = 1 )

  let of_bits bs : t =
    List.map (List.chunks_of bs ~length:8) ~f:(fun byte ->
        List.foldi byte ~init:0 ~f:(fun i acc bit ->
            if bit then acc lor (1 lsl i) else acc )
        |> Char.of_int_exn )
    |> String.of_char_list

  let to_field = Fn.compose Snark_params.Tick.Field.project to_bits

  let of_field = Fn.compose of_bits Snark_params.Tick.Field.unpack
end

let dummy_constraints () =
  let module Impl = Pickles.Impls.Step in
  let module Inner_curve = Pickles.Step_main_inputs.Inner_curve in
  let open Impl in
  let b = exists Boolean.typ_unchecked ~compute:(fun _ -> true) in
  let g = exists Inner_curve.typ ~compute:(fun _ -> Inner_curve.Params.one) in
  let _ =
    Pickles.Step_main_inputs.Ops.scale_fast g (`Plus_two_to_len [|b; b|])
  in
  let _ =
    Pickles.Pairing_main.Scalar_challenge.endo g (Scalar_challenge [b])
  in
  ()

let main (stmt : Mina_base.Snapp_statement.Checked.t) =
  let open Mina_base.Snapp_basic in
  let open Pickles.Impls.Step in
  dummy_constraints () ;
  (let (flag :: _eth_addr :: _) = stmt.body1.data.update.app_state in
   Boolean.Assert.is_true (Set_or_keep.Checked.is_set flag) ;
   Field.Assert.equal (Set_or_keep.Checked.data flag) Field.one) ;
  let score = exists Field.typ ~request:As_prover.(fun () -> Get_score) in
  (* 10 bits because maximum score is 850. *)
  as_prover
    As_prover.(
      fun () ->
        let score = read Field.typ score in
        if Field.Constant.(compare score (of_int target_score)) < 0 then
          Format.eprintf
            "The score %s is less than the target score %i.@ Unable to \
             generate a proof.@."
            (Field.Constant.to_string score)
            target_score) ;
  Field.Assert.gte ~bit_length:10 score (Field.of_int target_score)

module Cli_input = struct
  open Signature_lib

  type t =
    { snapp_pk: Public_key.Compressed.t
    ; receiver_pk: Public_key.Compressed.t
    ; fee: Currency.Amount.t
    ; amount: Currency.Amount.t
    ; account_creation_fee: Currency.Amount.t
    ; eth_address: Eth_address.t }

  let args : t option Command.Spec.param =
    let open Command in
    let open Let_syntax in
    let open Param in
    let amount_of_string =
      Fn.compose Currency.Amount.of_uint64 Unsigned.UInt64.of_string
    in
    let open Currency in
    let%map snapp_pk =
      flag "--snapp-public-key" ~doc:"PK Public key of the snapp account"
        (Flag.optional Cli_lib.Arg_type.public_key_compressed)
    and receiver_pk =
      flag "--receiver-public-key" ~doc:"PK Public key of the receiver account"
        (Flag.optional Cli_lib.Arg_type.public_key_compressed)
    and fee =
      flag "--fee"
        ~doc:
          "NUM The fee amount for the snapp account to pay the block producer"
        (Flag.optional (Arg_type.map ~f:amount_of_string string))
    and amount =
      flag "--amount"
        ~doc:
          "NUM The amount to transfer from the snapp account to the receiver"
        (Flag.optional (Arg_type.map ~f:amount_of_string string))
    and account_creation_fee =
      flag "--account-creation-fee"
        ~doc:
          "NUM The account creation fee, set by the network, to be paid by \
           the snapp account (default: 100000)"
        (Flag.optional_with_default
           (amount_of_string "100000")
           (Arg_type.map ~f:amount_of_string string))
    and eth_address =
      flag "--eth-address"
        ~doc:
          "ETH-ADDRESS The ETH address you want to store in the snapp account."
        (Flag.required (Arg_type.map ~f:Eth_address.of_string string))
    in
    let open Option.Let_syntax in
    let%map snapp_pk = snapp_pk
    and receiver_pk = receiver_pk
    and fee = fee
    and amount = amount in
    {snapp_pk; receiver_pk; fee; amount; account_creation_fee; eth_address}
end

include Snapp_runner_functor.Make_with_commands (struct
  module Public_input = struct
    module Value = struct
      include Mina_base.Snapp_statement

      let if_not_given = `Raise

      let args : t option Command.Spec.param =
        let open Command in
        let open Command.Let_syntax in
        let%map input = Cli_input.args in
        let open Mina_base in
        let open Option.Let_syntax in
        let%map { snapp_pk
                ; receiver_pk
                ; fee
                ; amount
                ; account_creation_fee
                ; eth_address } =
          input
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
              ; update=
                  { Snapp_command.Party.Body.dummy.update with
                    app_state=
                      [ Set Impls.Step.Field.Constant.one
                      ; Set (Eth_address.to_field eth_address)
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep ] }
              ; delta=
                  Currency.Amount.Signed.(negate (of_unsigned snapp_amount)) }
          ; body2=
              { pk= receiver_pk
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

  let prove_main =
    Some
      (fun ~(input : Public_input.Value.t) ~proof ->
        ( let open Signature_lib in
          Core.printf
            {graphql|mutation SendSnappCommand {
  sendSnappCommand(input:
    { snappAccount:
        { balanceChange: "-20100000"
        , publicKey: "%s"
        , predicate: { }
        , changes: {
            snappState: {
              x_0: "1",
              x_1: "%s",
            }
          }
        , proof: "%s" }
    , otherAccount:
        { balanceChange: "10000000"
        , publicKey: "%s" }
    , token: "1" })
}
|graphql}
            (Public_key.Compressed.to_base58_check input.body1.pk)
            (let (_flag :: eth_addr :: _) = input.body1.update.app_state in
             match eth_addr with
             | Keep ->
                 failwith "expected to set eth addr"
             | Set x ->
                 Pickles.Impls.Step.Field.Constant.to_string x)
            proof
            (Public_key.Compressed.to_base58_check input.body2.pk)
          : unit ) )

  let default_cache_location =
    Some Filename.(temp_dir_name ^/ "snapp_credit_score_demo")

  let rule =
    { Inductive_rule.prevs= []
    ; identifier= "demo-base"
    ; main= (fun [] i -> main i ; [])
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
       Core.printf
         !"demo statements %{sexp: Input.Public_input.Value.t list}\n%!"
         [public_input] ;
       Format.printf "Proof verified? %b@."
         (Proof.verify [(public_input, proof)]))

let () = run_commands ~additional_commands:[("verify", verify)] ()
