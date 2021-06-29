[%%import
"/src/config.mlh"]

open Ppxlib
open Asttypes
open Parsetree
open Longident
open Core
open Async
open Mina_state

(* TODO: refactor to do compile time selection *)
[%%if
proof_level = "full"]

let use_dummy_values = false

[%%else]

let use_dummy_values = true

[%%endif]

[%%inject
"generate_genesis_proof", generate_genesis_proof]

module type S = sig
  val compiled_values : Genesis_proof.t Async.Deferred.t option
end

let hashes =
  lazy
    (let constraint_constants =
       Genesis_constants.Constraint_constants.compiled
     in
     let proof_level = Genesis_constants.Proof_level.compiled in
     let ts =
       Transaction_snark.constraint_system_digests ~constraint_constants ()
     in
     let bs =
       Blockchain_snark.Blockchain_snark_state.constraint_system_digests
         ~proof_level ~constraint_constants ()
     in
     ts @ bs)

let hashes_to_expr ~loc hashes =
  let open Ppxlib.Ast_builder.Default in
  elist ~loc
  @@ List.map hashes ~f:(fun (x, y) ->
         [%expr
           [%e estring ~loc x]
           , Core.Md5.of_hex_exn [%e estring ~loc (Core.Md5.to_hex y)]] )

let vk_id_to_expr ~loc vk_id =
  let open Ppxlib.Ast_builder.Default in
  [%expr
    let t =
      lazy
        (Core.Sexp.of_string_conv_exn
           [%e
             estring ~loc
               (Core.Sexp.to_string
                  (Pickles.Verification_key.Id.sexp_of_t vk_id))]
           Pickles.Verification_key.Id.t_of_sexp)
    in
    fun () -> Lazy.force t]

module Inputs = struct
  let proof_level = Genesis_constants.Proof_level.compiled

  let constraint_constants = Genesis_constants.Constraint_constants.compiled

  let genesis_constants = Genesis_constants.compiled

  let genesis_epoch_data = Consensus.Genesis_epoch_data.compiled

  let consensus_constants =
    Consensus.Constants.create ~constraint_constants
      ~protocol_constants:genesis_constants.protocol

  let protocol_state_with_hash =
    Genesis_protocol_state.t ~genesis_ledger:Test_genesis_ledger.t
      ~genesis_epoch_data ~constraint_constants ~consensus_constants
end

module Dummy = struct
  let loc = Ppxlib.Location.none

  let base_proof_expr =
    if generate_genesis_proof then
      Some (Async.return [%expr Mina_base.Proof.blockchain_dummy])
    else None

  let compiled_values =
    let open Inputs in
    if generate_genesis_proof then
      Some
        (Async.return
           { Genesis_proof.runtime_config= Runtime_config.default
           ; constraint_constants
           ; proof_level
           ; genesis_constants
           ; genesis_ledger= (module Test_genesis_ledger)
           ; genesis_epoch_data
           ; consensus_constants
           ; protocol_state_with_hash
           ; constraint_system_digests= hashes
           ; proof_data= None })
    else None
end

module Make_real () = struct
  let loc = Ppxlib.Location.none

  let compiled_values =
    let open Inputs in
    if generate_genesis_proof then
      Some
        (let%bind () = return () in
         let module T = Transaction_snark.Make (Inputs) in
         let module B = Blockchain_snark.Blockchain_snark_state.Make (struct
           let tag = T.tag

           include Inputs
         end) in
         let%map values =
           Genesis_proof.create_values
             (module T)
             (module B)
             { runtime_config= Runtime_config.default
             ; constraint_constants
             ; proof_level= Full
             ; genesis_constants
             ; genesis_ledger= (module Test_genesis_ledger)
             ; genesis_epoch_data
             ; consensus_constants
             ; protocol_state_with_hash
             ; constraint_system_digests= None
             ; blockchain_proof_system_id= None }
         in
         values)
    else None
end

let main () =
  let open Ppxlib.Ast_builder.Default in
  let target = Sys.argv.(1) in
  let fmt = Format.formatter_of_out_channel (Out_channel.create target) in
  let loc = Ppxlib.Location.none in
  let (module M) =
    if use_dummy_values then (module Dummy : S) else (module Make_real () : S)
  in
  let%bind compiled_values =
    match M.compiled_values with
    | Some expr ->
        let%map expr = expr in
        Some expr
    | None ->
        Deferred.return None
  in
  let structure =
    [%str
      module T = Genesis_proof.T
      include T

      let for_unit_tests =
        lazy
          (let protocol_state_with_hash =
             Mina_state.Genesis_protocol_state.t
               ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
               ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
               ~constraint_constants:
                 Genesis_constants.Constraint_constants.for_unit_tests
               ~consensus_constants:
                 (Lazy.force Consensus.Constants.for_unit_tests)
           in
           { runtime_config= Runtime_config.default
           ; constraint_constants=
               Genesis_constants.Constraint_constants.for_unit_tests
           ; proof_level= Genesis_constants.Proof_level.for_unit_tests
           ; genesis_constants= Genesis_constants.for_unit_tests
           ; genesis_ledger= Genesis_ledger.for_unit_tests
           ; genesis_epoch_data= Consensus.Genesis_epoch_data.for_unit_tests
           ; consensus_constants= Lazy.force Consensus.Constants.for_unit_tests
           ; protocol_state_with_hash
           ; constraint_system_digests=
               lazy [%e hashes_to_expr ~loc (Lazy.force hashes)]
           ; proof_data= None })

      let compiled_inputs =
        lazy
          (let constraint_constants =
             Genesis_constants.Constraint_constants.compiled
           in
           let genesis_constants = Genesis_constants.compiled in
           let genesis_epoch_data = Consensus.Genesis_epoch_data.compiled in
           let consensus_constants =
             Consensus.Constants.create ~constraint_constants
               ~protocol_constants:genesis_constants.protocol
           in
           let protocol_state_with_hash =
             Mina_state.Genesis_protocol_state.t
               ~genesis_ledger:Test_genesis_ledger.t ~genesis_epoch_data
               ~constraint_constants ~consensus_constants
           in
           { Genesis_proof.Inputs.runtime_config= Runtime_config.default
           ; constraint_constants
           ; proof_level= Genesis_constants.Proof_level.compiled
           ; genesis_constants
           ; genesis_ledger= (module Test_genesis_ledger)
           ; genesis_epoch_data
           ; consensus_constants
           ; protocol_state_with_hash
           ; constraint_system_digests=
               [%e
                 match compiled_values with
                 | Some {constraint_system_digests= hashes; _} ->
                     [%expr Some [%e hashes_to_expr ~loc (Lazy.force hashes)]]
                 | None ->
                     [%expr None]]
           ; blockchain_proof_system_id=
               [%e
                 match compiled_values with
                 | Some
                     {proof_data= Some {blockchain_proof_system_id= id; _}; _}
                   ->
                     [%expr Some [%e vk_id_to_expr ~loc id]]
                 | _ ->
                     [%expr None]] })

      let compiled =
        [%e
          match compiled_values with
          | Some compiled_values ->
              [%expr
                Some
                  ( lazy
                    (let inputs = Lazy.force compiled_inputs in
                     { runtime_config= inputs.runtime_config
                     ; constraint_constants= inputs.constraint_constants
                     ; proof_level= inputs.proof_level
                     ; genesis_constants= inputs.genesis_constants
                     ; genesis_ledger= inputs.genesis_ledger
                     ; genesis_epoch_data= inputs.genesis_epoch_data
                     ; consensus_constants= inputs.consensus_constants
                     ; protocol_state_with_hash=
                         inputs.protocol_state_with_hash
                     ; constraint_system_digests=
                         lazy [%e hashes_to_expr ~loc (Lazy.force hashes)]
                     ; proof_data=
                         [%e
                           match compiled_values.proof_data with
                           | Some proof_data ->
                               [%expr
                                 Some
                                   { blockchain_proof_system_id=
                                       [%expr
                                         vk_id_to_expr ~loc
                                           proof_data
                                             .blockchain_proof_system_id]
                                   ; genesis_proof=
                                       Core.Binable.of_string
                                         (module Mina_base.Proof.Stable.Latest)
                                         [%e
                                           estring ~loc
                                             (Binable.to_string
                                                ( module Mina_base.Proof.Stable
                                                         .Latest )
                                                proof_data.genesis_proof)] }]
                           | None ->
                               [%expr None]] }) )]
          | None ->
              [%expr None]]]
  in
  Pprintast.top_phrase fmt (Ptop_def structure) ;
  exit 0

let () =
  don't_wait_for (main ()) ;
  never_returns (Scheduler.go ())
