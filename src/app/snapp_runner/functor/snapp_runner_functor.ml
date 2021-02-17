open Core_kernel
open Async
open Pickles

module Intf = struct
  module type Input = sig
    module Public_input : sig
      module Value : sig
        type t [@@deriving of_sexp, of_yojson]

        val if_not_given : [`Default_to of t | `Raise]

        val args : t option Command.Spec.param

        include
          Pickles.Statement_intf
          with type field := Impls.Step.Field.Constant.t
           and type t := t
      end

      module Var : Pickles.Statement_intf with type field := Impls.Step.Field.t

      val typ : (Var.t, Value.t) Impls.Step.Typ.t
    end

    module Request_data : sig
      type t

      val handler :
           t
        -> Snarky_backendless.Request.request
        -> Snarky_backendless.Request.response

      val args : t Command.Spec.param
    end

    (*module Branches : Pickles_types.Nat.Intf*)

    val name : string

    val rule :
      ( unit
      , unit
      , unit
      , unit
      , Public_input.Var.t
      , Public_input.Value.t )
      Inductive_rule.t

    val default_cache_location : string option
  end

  module type S = sig
    module Input : Input

    val compile :
         ?cache:Key_cache.Spec.t list
      -> ?disk_keys:( Pickles__Cache.Step.Key.Verification.t
                    , Pickles_types.Nat.N2.n (*Input.Branches.n*) )
                    Pickles_types.Vector.t
                    * Pickles__Cache.Wrap.Key.Verification.t
      -> unit
      -> ( Input.Public_input.Var.t
         , Input.Public_input.Value.t
         , Side_loaded.Verification_key.Max_width.n
         , Pickles_types.Nat.N2.n (*Input.Branches.n*) )
         Tag.t
         * Cache_handle.t
         * (module Proof_intf
              with type t = Side_loaded.Proof.t
               and type statement = Input.Public_input.Value.t)
         * ( unit
           , unit
           , unit
           , Input.Public_input.Value.t
           , Side_loaded.Proof.t Deferred.t )
           Prover.t
  end

  module type Commands = sig
    val cache_flag : Key_cache.Spec.t list Option.t Command.Param.t

    val mode_flag : [> `Binary | `Json] Command.Spec.param

    val commands : (string * Command.t) list

    val run_commands :
      ?additional_commands:(string * Command.t) list -> unit -> unit
  end

  module type S_with_commands = sig
    include S

    include Commands
  end
end

module Helpers = struct end

module Make (X : Intf.Input) : Intf.S with module Input = X = struct
  module Input = X

  let compile ?cache ?disk_keys () =
    let dummy_constraints x =
      let open Zexe_backend_common.Plonk_constraint_system.Plonk_constraint in
      let xx = (x, x) in
      let assert_all xs =
        Pickles.Impls.Step.assert_all
          (List.map xs ~f:(fun x ->
               [{Snarky_backendless.Constraint.basic= T x; annotation= None}]
           ))
      in
      assert_all
        [ EC_add {p1= xx; p2= xx; p3= xx}
        ; EC_scale
            { state=
                Array.init 3 ~f:(fun _ ->
                    { Zexe_backend_common.Scale_round.xt= x
                    ; b= x
                    ; yt= x
                    ; xp= x
                    ; l1= x
                    ; yp= x
                    ; xs= x
                    ; ys= x } ) }
        ; EC_endoscale
            { state=
                Array.init 3 ~f:(fun _ ->
                    { Zexe_backend_common.Endoscale_round.b2i1= x
                    ; xt= x
                    ; b2i= x
                    ; xq= x
                    ; yt= x
                    ; xp= x
                    ; l1= x
                    ; yp= x
                    ; xs= x
                    ; ys= x } ) } ]
    in
    let tag, cache_handle, proof, Provers.[prover; _dummy] =
      Pickles.compile ?cache ?disk_keys
        (module X.Public_input.Var)
        (module X.Public_input.Value)
        ~constraint_constants:
          { sub_windows_per_window= 0
          ; ledger_depth= 0
          ; work_delay= 0
          ; block_window_duration_ms= 0
          ; transaction_capacity= Log_2 0
          ; pending_coinbase_depth= 0
          ; coinbase_amount= Unsigned.UInt64.of_int 0
          ; supercharged_coinbase_factor= 0
          ; account_creation_fee= Unsigned.UInt64.of_int 0
          ; fork= None }
        ~typ:X.Public_input.typ
        ~branches:(module Pickles_types.Nat.N2)
        ~max_branching:(module Side_loaded.Verification_key.Max_width)
        ~name:X.name
        ~choices:(fun ~self ->
          [ X.rule
          ; { prevs= [self; self]
            ; main_value= (fun [_; _] _ -> [true; true])
            ; identifier= "demo"
            ; main=
                (fun [_; _] _ ->
                  let open Pickles.Impls.Step in
                  let x =
                    exists Field.typ ~compute:(fun _ -> failwith "dummy rule")
                  in
                  dummy_constraints x ;
                  Boolean.Assert.is_true (Boolean.not (Field.equal x x)) ;
                  [Boolean.true_; Boolean.true_] ) } ] )
    in
    (tag, cache_handle, proof, prover)
end

module Make_commands (X : Intf.S) : Intf.Commands = struct
  let cache_flag =
    let open Command in
    Spec.flag "--cache-location"
      ~doc:"path The directory to cache proving and verification keys in"
      (Flag.optional Spec.string)
    |> Param.map ~f:(function
         | Some d ->
             Some [Key_cache.Spec.On_disk {directory= d; should_write= true}]
         | None ->
             Option.map X.Input.default_cache_location ~f:(fun d ->
                 [Key_cache.Spec.On_disk {directory= d; should_write= true}] ) )

  let mode_flag =
    let open Command in
    Spec.flag "--mode"
      ~doc:"json|binary The mode to output the verification key in"
      (Flag.optional_with_default `Binary
         (Arg_type.of_alist_exn [("binary", `Binary); ("json", `Json)]))

  let verification_key =
    let open Command in
    let open Command.Let_syntax in
    basic ~summary:"Generate and print the verification key for the circuit"
      (let%map mode = mode_flag and cache = cache_flag in
       fun () ->
         let tag, _, _, _ = X.compile ?cache () in
         let verification_key =
           Pickles.Side_loaded.Verification_key.of_compiled tag
         in
         let string =
           match mode with
           | `Json ->
               Side_loaded.Verification_key.to_yojson verification_key
               |> Yojson.Safe.pretty_to_string
           | `Binary ->
               Binable.to_string
                 (module Side_loaded.Verification_key.Stable.Latest)
                 verification_key
               |> Base64.encode ~alphabet:Base64.uri_safe_alphabet
               |> Result.map_error ~f:(function `Msg msg ->
                      Error.createf
                        "Could not convert verification key to base64: %s" msg )
               |> Or_error.ok_exn
         in
         Format.printf "%s@." string)

  let prove =
    let open Command in
    let open Command.Let_syntax in
    async ~summary:"Generate and print a proof of the circuit"
      (let%map mode = mode_flag
       and cache = cache_flag
       and request_data = X.Input.Request_data.args
       and public_input =
         Spec.choose_one
           ~if_nothing_chosen:X.Input.Public_input.Value.if_not_given
           [ X.Input.Public_input.Value.args
           ; Spec.flag "--public-input-sexp"
               ~doc:
                 "s-expression Enter the public input in the form of an \
                  s-expression"
               (Flag.optional
                  (Arg_type.Export.sexp_conv
                     X.Input.Public_input.Value.t_of_sexp))
           ; Spec.flag "--public-input-json"
               ~doc:"json Enter the public input in the json format"
               (Flag.optional
                  (Arg_type.create (fun str ->
                       Yojson.Safe.from_string str
                       |> X.Input.Public_input.Value.of_yojson
                       |> Result.map_error ~f:(fun msg ->
                              Error.createf
                                "Could read the public input from the given \
                                 JSON: %s"
                                msg )
                       |> Or_error.ok_exn ))) ]
       in
       fun () ->
         let _, _, _, prove = X.compile ?cache () in
         let open Async in
         let%map proof =
           prove
             ~handler:(X.Input.Request_data.handler request_data)
             [] public_input
         in
         let string =
           match mode with
           | `Json ->
               Side_loaded.Proof.to_yojson proof
               |> Yojson.Safe.pretty_to_string
           | `Binary ->
               Binable.to_string (module Side_loaded.Proof.Stable.Latest) proof
               |> Base64.encode ~alphabet:Base64.uri_safe_alphabet
               |> Result.map_error ~f:(function `Msg msg ->
                      Error.createf
                        "Could not convert verification key to base64: %s" msg )
               |> Or_error.ok_exn
         in
         Format.printf "%s@." string)

  let commands = [("verification-key", verification_key); ("prove", prove)]

  let run_commands ?additional_commands () =
    let commands =
      match additional_commands with
      | None ->
          commands
      | Some additional_commands ->
          commands @ additional_commands
    in
    Command.run
      (Command.group ~summary:"Coda snapp transaction runner"
         ~preserve_subcommand_order:() commands)
end

module Make_with_commands (X : Intf.Input) : Intf.S_with_commands = struct
  module T = Make (X)
  module Commands = Make_commands (T)
  include T
  include Commands
end
