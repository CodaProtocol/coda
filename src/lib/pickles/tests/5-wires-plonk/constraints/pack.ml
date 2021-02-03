module Snarky = Snarky_backendless
open Core
open Snarky
open Zexe_backend_common.Plonk_5_wires_constraint_system

module Constraints (Intf : Snark_intf.Run with type prover_state = unit) =
struct
  open Intf

  let unpack (scalar : Field.t) : Field.t array =
    let padlen = 4 - (Field.size_in_bits % 4) in
    let len = Field.size_in_bits + padlen in
    let rounds = len / 4 in
    let state =
      exists
        (Snarky.Typ.array
           ~length:Int.(rounds + 1)
           (Snarky.Typ.array ~length:5 Field.typ))
        ~compute:
          As_prover.(
            fun () ->
              let bits =
                Field.Constant.unpack (read_var scalar)
                |> List.rev |> Array.of_list
              in
              let bits =
                Array.init len ~f:(fun i ->
                    if i < padlen then false else bits.(Int.(i - padlen)) )
                |> Array.map ~f:(fun x ->
                       if x = true then Field.Constant.one
                       else Field.Constant.zero )
              in
              let state =
                Array.create ~len:Int.(rounds + 1) (Array.create ~len:5 zero)
              in
              state.(0) <- [|zero; zero; zero; zero; zero|] ;
              for i = 1 to rounds do
                let ind = Int.((i * 4) - 4) in
                let s = state.(Int.(i - 1)).(4) in
                let s0 = bits.(ind) in
                let s1 = bits.(Int.(ind + 1)) in
                let s2 = bits.(Int.(ind + 2)) in
                let s3 = bits.(Int.(ind + 3)) in
                let open Field.Constant in
                state.(i)
                <- [| s0
                    ; s1
                    ; s2
                    ; s3
                    ; (s * of_int 16)
                      + (s0 * of_int 8)
                      + (s1 * of_int 4)
                      + (s2 * of_int 2)
                      + s3 |]
              done ;
              state)
    in
    Intf.assert_ [{basic= Plonk_constraint.T (Pack {state}); annotation= None}] ;
    let ret = ref [] in
    for i = 0 to len - 1 do
      let elm = state.((i / 4) + 1).(i % 4) in
      if elm <> Field.zero || List.length !ret > 0 then
        ret := List.append !ret [elm]
    done ;
    Array.of_list !ret

  let pack (bits : Field.t array) : Field.t =
    let padlen = 4 - (Array.length bits % 4) in
    let len = Array.length bits + padlen in
    let bits =
      Array.init len ~f:(fun i ->
          if i < padlen then Field.zero else bits.(i - padlen) )
    in
    let rounds = len / 4 in
    let state =
      exists
        (Snarky.Typ.array
           ~length:Int.(rounds + 1)
           (Snarky.Typ.array ~length:5 Field.typ))
        ~compute:
          As_prover.(
            fun () ->
              let state =
                Array.create ~len:Int.(rounds + 1) (Array.create ~len:5 zero)
              in
              state.(0) <- [|zero; zero; zero; zero; zero|] ;
              for i = 1 to rounds do
                let ind = Int.((i * 4) - 4) in
                let s = state.(Int.(i - 1)).(4) in
                let s0 = read_var bits.(ind) in
                let s1 = read_var bits.(Int.(ind + 1)) in
                let s2 = read_var bits.(Int.(ind + 2)) in
                let s3 = read_var bits.(Int.(ind + 3)) in
                let open Field.Constant in
                state.(i)
                <- [| s0
                    ; s1
                    ; s2
                    ; s3
                    ; (s * of_int 16)
                      + (s0 * of_int 8)
                      + (s1 * of_int 4)
                      + (s2 * of_int 2)
                      + s3 |]
              done ;
              state)
    in
    Array.iteri bits ~f:(fun i b -> state.((i / 4) + 1).(i % 4) <- b) ;
    Intf.assert_ [{basic= Plonk_constraint.T (Pack {state}); annotation= None}] ;
    state.(rounds).(4)
end
