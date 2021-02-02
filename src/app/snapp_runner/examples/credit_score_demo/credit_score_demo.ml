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

let main () =
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
      type t = unit

      let of_yojson _ = Ok ()

      let t_of_sexp _ = ()

      let to_field_elements () = [||]

      let if_not_given = `Default_to ()
    end

    module Var = Value

    let typ = Snarky_backendless.Typ.unit ()
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

let () = run_commands ()
