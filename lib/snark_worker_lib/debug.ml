open Core
open Async

module Inputs = struct
  module Worker_state = struct
    include Unit

    let create () = Deferred.unit

    let worker_wait_time = 1.
  end

  module Proof = Transaction_snark.Statement
  module Statement = Transaction_snark.Statement

  module Public_key = struct
    include Signature_lib.Public_key.Compressed

    let arg_type = Cli_lib.public_key_compressed
  end

  module Super_transaction = Nanobit_base.Super_transaction
  module Sparse_ledger = Nanobit_base.Sparse_ledger

  let perform_single () ~message:_ s =
    Ok (Snark_work_lib.Work.Single.Spec.statement s)
end

module Worker = Worker.Make (Inputs)

let command_name = "snark-worker-debug"
