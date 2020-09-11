open Core_kernel
open Async_kernel
open Coda_base

type t = unit

type ledger_proof = Ledger_proof.Debug.t

let create ~logger:_ ~proof_level ~pids:_ ~conf_dir:_ =
  match proof_level with
  | Genesis_constants.Proof_level.Full ->
      failwith "Unable to handle proof-level=Full"
  | Check | None ->
      Deferred.return ()

let verify_blockchain_snark _ _ = Deferred.Or_error.return true

let verify_transaction_snarks _ ts =
  (*Don't check if the proof has default sok becasue they were probably not
  intended to be checked. If it has something value then check that against the
  message passed. This is particularly used to test that invalid proofs are not
  added to the snark pool*)
  List.for_all ts ~f:(fun (proof, message) ->
      if Sok_message.Digest.(equal (snd proof) default) then true
      else
        let msg_digest = Sok_message.digest message in
        Coda_base.Sok_message.Digest.equal (snd proof) msg_digest )
  |> Deferred.Or_error.return

let of_generic _ : t = ()
