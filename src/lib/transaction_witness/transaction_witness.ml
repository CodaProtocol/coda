open Core_kernel

module Parties_segment_witness = struct
  open Mina_base
  open Currency

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { global_ledger : Sparse_ledger.Stable.V1.t
        ; local_state_init :
            ( Party.Stable.V1.t Parties.With_hashes.Stable.V1.t
            , Token_id.Stable.V1.t
            , Amount.Stable.V1.t
            , Sparse_ledger.Stable.V1.t
            , bool
            , Zexe_backend.Pasta.Fp.Stable.V1.t )
            Parties_logic.Local_state.Stable.V1.t
        ; start_parties :
            ( Parties.Stable.V1.t
            , Snapp_predicate.Protocol_state.Stable.V1.t
            , bool )
            Parties_logic.Start_data.Stable.V1.t
            list
        ; state_body : Mina_state.Protocol_state.Body.Value.Stable.V2.t
        }
      [@@deriving sexp, to_yojson]

      let to_latest = Fn.id
    end
  end]
end

[%%versioned
module Stable = struct
  module V2 = struct
    type t =
      | Non_parties of
          { transaction : Mina_base.Transaction.Stable.V2.t
          ; ledger : Mina_base.Sparse_ledger.Stable.V1.t
          ; protocol_state_body :
              Mina_state.Protocol_state.Body.Value.Stable.V2.t
          ; init_stack : Mina_base.Pending_coinbase.Stack_versioned.Stable.V1.t
          ; status : Mina_base.Transaction_status.Stable.V1.t
          }
      | Parties_segment of Parties_segment_witness.Stable.V1.t
    [@@deriving sexp, to_yojson]

    let to_latest = Fn.id
  end

  module V1 = struct
    type t =
      { ledger : Mina_base.Sparse_ledger.Stable.V1.t
      ; protocol_state_body : Mina_state.Protocol_state.Body.Value.Stable.V1.t
      ; init_stack : Mina_base.Pending_coinbase.Stack_versioned.Stable.V1.t
      ; status : Mina_base.Transaction_status.Stable.V1.t
      }
    [@@deriving sexp, to_yojson]

    let to_latest (_ : t) : V2.t =
      failwith "Cannot convert transaction witness to latest"
  end
end]
