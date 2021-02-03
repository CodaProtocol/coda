module Snarky = Snarky_backendless
open Core
open Snarky
open Sponge
open Zexe_backend_common.Plonk_5_wires_constraint_system

module Constraints
    (Intf : Snark_intf.Run with type prover_state = unit) (Params : sig
        val params : Intf.field Params.t
    end) =
struct
  open Intf

  let permute (start : Field.t array) rounds : Field.t array =
    let length = Array.length start in
    let state =
      exists
        (Snarky.Typ.array
           ~length:Int.(rounds + 1)
           (Snarky.Typ.array ~length Field.typ))
        ~compute:
          As_prover.(
            fun () ->
              let state =
                Array.create
                  ~len:Int.(rounds + 1)
                  (Array.create ~len:length zero)
              in
              state.(0) <- Array.map ~f:(fun x -> read_var x) start ;
              for i = 1 to rounds do
                state.(i)
                <- Array.map
                     ~f:(fun x ->
                       let sq = square x in
                       square sq * sq * x )
                     state.(Int.(i - 1)) ;
                state.(i)
                <- Array.map
                     ~f:(fun p ->
                       Array.fold2_exn p state.(i) ~init:Field.Constant.zero
                         ~f:(fun c a b -> (a * b) + c) )
                     Params.params.mds ;
                for j = 0 to Int.(length - 1) do
                  state.(i).(j)
                  <- state.(i).(j)
                     + Params.params.round_constants.(Int.(i - 1)).(j)
                done
              done ;
              state)
    in
    state.(0) <- start ;
    Intf.assert_
      [{basic= Plonk_constraint.T (Poseidon {state}); annotation= None}] ;
    state.(rounds)
end
