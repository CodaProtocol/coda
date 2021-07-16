open Async
open Signature_lib

(* for versioning of the types here, see

   RFC 0012, and

   https://ocaml.janestreet.com/ocaml-core/latest/doc/async_rpc_kernel/Async_rpc_kernel/Versioned_rpc/

*)

(* for each RPC, return the Master module only, and not the versioned modules, because the functor should not
   return types with bin_io; the versioned modules are defined in snark_worker.ml
*)

module Make (Inputs : Intf.Inputs_intf) = struct
  open Inputs
  open Snark_work_lib

  module Get_work = struct
    module Master = struct
      let name = "get_work"

      module T = struct
        type query = unit

        type response =
          ( (Transaction_witness.t, Ledger_proof.t) Work.Single.Spec.t
            Work.Spec.t
          * Public_key.Compressed.t )
          option
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    include Versioned_rpc.Both_convert.Plain.Make (Master)
  end

  module Submit_work = struct
    module Master = struct
      let name = "submit_work"

      module T = struct
        type query =
          ( (Transaction_witness.t, Ledger_proof.t) Work.Single.Spec.t
            Work.Spec.t
          , Ledger_proof.t )
          Work.Result.t

        type response = unit
      end

      module Caller = T
      module Callee = T
    end

    include Master.T
    include Versioned_rpc.Both_convert.Plain.Make (Master)
  end
end
