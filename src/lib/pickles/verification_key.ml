open Core
open Pickles_types
open Import
open Zexe_backend.Pasta

module Data = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = {constraints: int} [@@deriving sexp]

      let to_latest = Fn.id
    end
  end]
end

module Repr = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { commitments:
            Backend.Tock.Curve.Affine.Stable.V1.t array
            Plonk_verification_key_evals.Stable.V1.t
        ; step_domains: Domains.Stable.V1.t array
        ; data: Data.Stable.V1.t }

      let to_latest = Fn.id
    end
  end]
end

[%%versioned_binable
module Stable = struct
  module V1 = struct
    type t =
      { commitments:
          Backend.Tock.Curve.Affine.t array Plonk_verification_key_evals.t
      ; step_domains: Domains.t array
      ; index: Impls.Wrap.Verification_key.t sexp_opaque
      ; data: Data.t }
    [@@deriving fields, sexp]

    let to_latest = Fn.id

    let of_repr urs {Repr.commitments= c; step_domains; data= d} =
      let t : Impls.Wrap.Verification_key.t =
        let log2_size = Int.ceil_log2 d.constraints in
        let d = Domain.Pow_2_roots_of_unity log2_size in
        let max_quot_size = Common.max_quot_size_int (Domain.size d) in
        { domain=
            { log_size_of_group= log2_size
            ; group_gen= Backend.Tock.Field.domain_generator log2_size }
        ; max_poly_size= 1 lsl Nat.to_int Rounds.Wrap.n
        ; max_quot_size
        ; urs
        ; evals=
            Plonk_verification_key_evals.map c ~f:(fun unshifted ->
                { Marlin_plonk_bindings.Types.Poly_comm.shifted= None
                ; unshifted=
                    Array.map unshifted ~f:(fun x -> Or_infinity.Finite x) } )
        ; shifts= Common.tock_shifts ~log2_size }
      in
      {commitments= c; step_domains; data= d; index= t}

    include Binable.Of_binable
              (Repr.Stable.V1)
              (struct
                type nonrec t = t

                let to_binable {commitments; step_domains; data; index= _} =
                  {Repr.commitments; data; step_domains}

                let of_binable r = of_repr (Backend.Tock.Keypair.load_urs ()) r
              end)
  end
end]

let dummy_commitments g =
  { Plonk_verification_key_evals.sigma_comm_0= g
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
  ; emul3_comm= g }

let dummy =
  lazy
    (let rows = Domain.size Common.wrap_domains.h in
     let g =
       let len =
         let max_degree = Common.Max_degree.wrap in
         Int.round_up rows ~to_multiple_of:max_degree / max_degree
       in
       Array.create ~len Backend.Tock.Curve.(to_affine_exn one)
     in
     { Repr.commitments= dummy_commitments g
     ; step_domains= [||]
     ; data= {constraints= rows} }
     |> Stable.Latest.of_repr (Marlin_plonk_bindings.Pasta_fq_urs.create 1))
