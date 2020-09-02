open Core
open Snarky
open Sponge

module Constraints
    (Intf : Snark_intf.Run with type prover_state = unit)
    (Params : sig val params : Intf.field Params.t end)
= struct
  open Intf

  let permute (start : Field.t array) rounds : Field.t array =
    let length = Array.length start in
    let state = exists 
        (Snarky.Typ.array ~length:rounds (Snarky.Typ.array length Field.typ)) 
        ~compute:As_prover.(fun () ->
            (
              let state = Array.create rounds (Array.create length zero) in
              for i = 0 to rounds do
                let prev = if i = 0 then Array.map ~f:(fun x -> read_var x) start else state.(Int.(i-1)) in
                state.(i) <- Array.map ~f:(fun x -> (square (square x)) * x) prev;
                state.(i) <- Array.map
                    ~f:(fun p -> Array.fold2_exn p state.(i) ~init:Field.Constant.zero ~f:(fun c a b -> a * b + c))
                    Params.params.mds;
                for j = 0 to Int.(length - 1) do
                  state.(i).(j) <- state.(i).(j) + Params.params.round_constants.(i).(j)
                done
              done;
              state
            ))
    in
    Intf.assert_
      [{
        basic= Zexe_backend.R1CS_constraint_system.Plonk_constraint.T (Poseidon { start; state }) ;
        annotation= None
      }];
    state.(rounds - 1)

end
