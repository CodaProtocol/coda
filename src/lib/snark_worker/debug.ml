open Core
open Async
open Mina_base

module Inputs = struct
  module Worker_state = struct
    include Unit

    let create ~constraint_constants:_ ~proof_level () =
      match proof_level with
      | Genesis_constants.Proof_level.Full ->
          failwith "Unable to handle proof-level=Full"
      | Check | None ->
          Deferred.unit

    let worker_wait_time = 0.5
  end

  let perform_single () ~message s :
      (Ledger_proof.t * Time.Span.t) Deferred.Or_error.t =
    (* Use a dummy proof. *)
    let stmt =
      match s with
      | Snark_work_lib.Work.Single.Spec.Transition (stmt, _) ->
          stmt
      | Merge (stmt, _, _) ->
          stmt
    in
    let sok_digest = Sok_message.digest message in
    Deferred.Or_error.return
    @@ ( Transaction_snark.create ~statement:{ stmt with sok_digest }
           ~proof:Proof.transaction_dummy
       , Time.Span.zero )
end
