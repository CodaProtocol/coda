open Core_kernel
open Async_kernel
open Mina_base

type t = unit

type ledger_proof = Ledger_proof.t

let create ~logger:_ ~proof_level ~constraint_constants:_ ~pids:_ ~conf_dir:_ =
  match proof_level with
  | Genesis_constants.Proof_level.Full ->
      failwith "Unable to handle proof-level=Full"
  | Check | None ->
      Deferred.return ()

let verify_blockchain_snarks _ _ = Deferred.Or_error.return true

let verify_commands _ (cs : User_command.Verifiable.t list) :
    [ `Valid of Mina_base.User_command.Valid.t
    | `Invalid
    | `Valid_assuming of
      ( Pickles.Side_loaded.Verification_key.t
      * Mina_base.Snapp_statement.t
      * Pickles.Side_loaded.Proof.t )
      list ]
    list
    Deferred.Or_error.t =
  List.map cs ~f:(fun c ->
      match Common.check c with
      | `Valid c ->
          `Valid c
      | `Invalid ->
          `Invalid
      | `Valid_assuming (c, _) ->
          `Valid c )
  |> Deferred.Or_error.return

let verify_transaction_snarks _ ts =
  (* Don't check if the proof has default sok, becasue they were probably not
     intended to be checked. If it has some value then check that against the
     message passed.
     This is particularly used to test that invalid proofs are not added to the
     snark pool
  *)
  List.for_all ts ~f:(fun (proof, message) ->
      let msg_digest = Sok_message.digest message in
      let sok_digest = Transaction_snark.sok_digest proof in
      Sok_message.Digest.(equal sok_digest default)
      || Mina_base.Sok_message.Digest.equal sok_digest msg_digest )
  |> Deferred.Or_error.return
