[%%import
"/src/config.mlh"]

[%%ifdef
consensus_mechanism]

include Pickles.Side_loaded.Verification_key

[%%else]

open Core_kernel

module G = struct
  open Snark_params_nonconsensus

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Field.Stable.V1.t * Field.Stable.V1.t
      [@@deriving sexp, eq, compare, hash, yojson]

      let to_latest = Fn.id
    end
  end]
end

module R = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        G.Stable.V1.t
        Pickles_base.Side_loaded_verification_key.Repr.Stable.V1.t

      let to_latest = Fn.id
    end
  end]
end

[%%versioned_binable
module Stable = struct
  module V1 = struct
    type t =
      ( G.Stable.V1.t
      , unit )
      Pickles_base.Side_loaded_verification_key.Poly.Stable.V1.t
    [@@deriving sexp, compare, equal, hash, yojson]

    open Pickles_base.Side_loaded_verification_key

    let to_latest = Fn.id

    include Binable.Of_binable
              (R.Stable.V1)
              (struct
                type nonrec t = t

                let to_binable
                    {Poly.step_data; num_input_proofs; wrap_index; wrap_vk= _}
                    =
                  {Repr.Stable.V1.step_data; num_input_proofs; wrap_index}

                let of_binable
                    {Repr.Stable.V1.step_data; num_input_proofs; wrap_index= c}
                    =
                  { Poly.step_data
                  ; num_input_proofs
                  ; wrap_index= c
                  ; wrap_vk= Some () }
              end)
  end
end]

let to_input = Pickles_base.Side_loaded_verification_key.to_input

let dummy : t =
  let open Pickles_types in
  { step_data= At_most.[]
  ; num_input_proofs=
      Pickles_base.Side_loaded_verification_key.Num_input_proofs.zero
  ; wrap_index=
      (let g = [Snarkette.Pasta.Pallas.(to_affine_exn one)] in
       { sigma_comm_0= g
       ; sigma_comm_1= g
       ; sigma_comm_2= g
       ; ql_comm= g
       ; qr_comm= g
       ; qo_comm= g
       ; qm_comm= g
       ; qc_comm= g
       ; rcm_comm_0= g
       ; rcm_comm_1= g
       ; rcm_comm_2= g
       ; psm_comm= g
       ; add_comm= g
       ; mul1_comm= g
       ; mul2_comm= g
       ; emul1_comm= g
       ; emul2_comm= g
       ; emul3_comm= g })
  ; wrap_vk= None }

[%%endif]
