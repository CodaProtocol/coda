open Core_kernel
open Mina_base
open Mina_state

(* do not expose refer to types in here directly; use allocation functor version instead *)
module Raw_versioned__ = struct
  let id = "blockchain_snark"

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { state : Protocol_state.Value.Stable.V2.t; proof : Proof.Stable.V1.t }
      [@@deriving fields, sexp, yojson]

      let to_latest = Fn.id

      type 'a creator = state:Protocol_state.Value.t -> proof:Proof.t -> 'a

      let map_creator c ~f ~state ~proof = f (c ~state ~proof)

      let create ~state ~proof = { state; proof }
    end
  end]
end

include Allocation_functor.Make.Versioned_v1.Full (Raw_versioned__)

[%%define_locally Raw_versioned__.(state, proof)]

include (
  Stable.Latest :
    sig
      type t [@@deriving sexp, yojson]
    end
    with type t := t )

[%%define_locally Stable.Latest.(create)]
