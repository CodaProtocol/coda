module Intf = Intf
module Inputs = Prod.Inputs

module Worker = struct
  include Functor.Make (Inputs)

  module Rpcs_versioned = struct
    open Core_kernel
    open Mina_base
    open Signature_lib

    module Work = struct
      type ledger_proof = Inputs.Ledger_proof.t

      include Work
    end

    module Get_work = struct
      module V1 = struct
        module T = struct
          type query = unit [@@deriving bin_io, version {rpc}]

          type response =
            ( ( Transaction.Stable.V1.t
              , Transaction_witness.Stable.V1.t
              , Inputs.Ledger_proof.Stable.V1.t )
              Snark_work_lib.Work.Single.Spec.Stable.V1.t
              Snark_work_lib.Work.Spec.Stable.V1.t
            * Public_key.Compressed.Stable.V1.t )
            option
          [@@deriving bin_io, version {rpc}]

          let query_of_caller_model = Fn.id

          let callee_model_of_query = Fn.id

          let response_of_callee_model = Fn.id

          let caller_model_of_response = Fn.id
        end

        include T
        include Rpcs.Get_work.Register (T)
      end

      module Latest = V1
    end

    module Submit_work = struct
      module V1 = struct
        module T = struct
          type query =
            ( ( Transaction.Stable.V1.t
              , Transaction_witness.Stable.V1.t
              , Ledger_proof.Stable.V1.t )
              Snark_work_lib.Work.Single.Spec.Stable.V1.t
              Snark_work_lib.Work.Spec.Stable.V1.t
            , Ledger_proof.Stable.V1.t )
            Snark_work_lib.Work.Result.Stable.V1.t
          [@@deriving bin_io, version {rpc}]

          type response = unit [@@deriving bin_io, version {rpc}]

          let query_of_caller_model = Fn.id

          let callee_model_of_query = Fn.id

          let response_of_callee_model = Fn.id

          let caller_model_of_response = Fn.id
        end

        include T
        include Rpcs.Submit_work.Register (T)
      end

      module Latest = V1
    end
  end

  let command = command_from_rpcs (module Rpcs_versioned)
end

include Worker
