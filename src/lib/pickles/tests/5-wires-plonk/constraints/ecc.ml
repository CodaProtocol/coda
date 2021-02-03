module Snarky = Snarky_backendless
open Core
open Snarky
open Zexe_backend_common
open Zexe_backend_common.Plonk_5_wires_constraint_system
open Marlin_plonk_bindings

module Constraints
    (Intf : Snark_intf.Run
            with type prover_state = unit
             and type field = Pasta_fp.t) =
struct
  open Intf
  open Field

  module Basic = struct
    open Constant

    let threehalfs = of_int 3 / of_int 2

    let add ((x1, y1) : field * field) ((x2, y2) : field * field) :
        (field * field) * field =
      let r = one / (x2 - x1) in
      let s = (y2 - y1) * r in
      let x3 = square s - x1 - x2 in
      let y3 = ((x1 - x3) * s) - y1 in
      ((x3, y3), r)

    let double ((x, y) : field * field) : (field * field) * field =
      let r = one / y in
      let s = square x * threehalfs * r in
      let x1 = square s - (x * of_int 2) in
      let y1 = (s * (x - x1)) - y in
      ((x1, y1), r)
  end

  let add (p1 : t * t) (p2 : t * t) : t * t =
    let p3, r =
      exists
        Typ.(typ * typ * typ)
        ~compute:
          As_prover.(
            fun () ->
              Basic.add
                (read_var (fst p1), read_var (snd p1))
                (read_var (fst p2), read_var (snd p2)))
    in
    Intf.assert_
      [{basic= Plonk_constraint.T (EC_add {p1; p2; p3; r}); annotation= None}] ;
    p3

  let double (p1 : t * t) : t * t =
    let p2, r =
      exists
        Typ.(typ * typ * typ)
        ~compute:
          As_prover.(
            fun () -> Basic.double (read_var (fst p1), read_var (snd p1)))
    in
    Intf.assert_
      [{basic= Plonk_constraint.T (EC_double {p1; p2; r}); annotation= None}] ;
    p2

  (* this function constrains computation of [2^n + k]T *)
  let scale ((xt, yt) : t * t) (scalar : t array) : t * t =
    (*
      Acc := [2] T + T
      for i from n-2 down to 0
          Q := ki+1 ? T : −T
          Acc := (Acc + Q) + Acc
      return (k0 = 0) ? (Acc - T) : Acc
    *)
    let n = Array.length scalar in
    if n = 0 then (xt, yt)
    else
      let xp, yp = add (double (xt, yt)) (xt, yt) in
      let xp, yp =
        if n < 2 then (xp, yp)
        else
          let state =
            exists
              (Snarky.Typ.array
                 ~length:Int.(n - 1)
                 (Scale_round_5_wires.typ typ))
              ~compute:
                As_prover.(
                  fun () ->
                    let state = ref [] in
                    let xpl, ypl = (ref (read_var xp), ref (read_var yp)) in
                    let xtl, ytl = (read_var xt, read_var yt) in
                    for i = Int.(n - 2) downto 0 do
                      let bit = read_var scalar.(Int.(i + 1)) in
                      let (xtmp, ytmp), _ =
                        Basic.add (!xpl, !ypl) (xtl, ytl * (bit + bit - one))
                      in
                      let (xsl, ysl), _ =
                        Basic.add (!xpl, !ypl) (xtmp, ytmp)
                      in
                      let round =
                        Scale_round_5_wires.
                          { xt= xtl
                          ; b= bit
                          ; yt= ytl
                          ; xp= !xpl
                          ; l1=
                              (!ypl - (ytl * (bit + bit - one))) / (!xpl - xtl)
                          ; l2= (!ypl + ysl) / (!xpl - xsl)
                          ; yp= !ypl
                          ; xs= xsl
                          ; ys= ysl }
                      in
                      state := !state @ [round] ;
                      xpl := xsl ;
                      ypl := ysl
                    done ;
                    Array.of_list !state)
          in
          let state =
            Array.mapi state ~f:(fun i s ->
                let xp, yp =
                  if i > 0 then
                    let s = state.(Int.(i - 1)) in
                    (s.xs, s.ys)
                  else (xp, yp)
                in
                {s with xt; yt; b= scalar.(Int.(n - i - 1)); xp; yp} )
          in
          Intf.assert_
            [{basic= Plonk_constraint.T (EC_scale {state}); annotation= None}] ;
          let finish = state.(Int.(n - 2)) in
          (finish.xs, finish.ys)
      in
      let xtp, ytp = add (xp, yp) (xt, negate yt) in
      let b = Intf.Boolean.of_field scalar.(0) in
      (if_ b ~then_:xp ~else_:xtp, if_ b ~then_:yp ~else_:ytp)

  (* this function constrains computation of [2^n + k]T with unpacking *)
  let scale_pack ((xt, yt) : t * t) (scalar : t) : t * t =
    (*
      Acc := [2] T + T
      for i from n-2 down to 0
          Q := ki+1 ? T : −T
          Acc := (Acc + Q) + Acc
      return (k0 = 0) ? (Acc - T) : Acc
    *)
    let n = Int.(Field.size_in_bits + 1) in
    let xp, yp = add (double (xt, yt)) (xt, yt) in
    let xp, yp, bit =
      let state =
        exists
          (Snarky.Typ.array
             ~length:Int.(n - 1)
             (Scale_pack_round_5_wires.typ typ))
          ~compute:
            As_prover.(
              fun () ->
                let half = Field.Constant.inv (Field.Constant.of_int 2) in
                let bits =
                  Field.Constant.unpack (read_var scalar)
                  |> List.rev |> Array.of_list
                in
                let padlen = Int.(n - Array.length bits) in
                let bits =
                  Array.init n ~f:(fun i ->
                      if i < padlen then false else bits.(Int.(i - padlen)) )
                  |> Array.map ~f:(fun x ->
                         if x = true then Field.Constant.one
                         else Field.Constant.zero )
                in
                let state = ref [] in
                let xpl, ypl = (ref (read_var xp), ref (read_var yp)) in
                let xtl, ytl = (read_var xt, read_var yt) in
                let n1l = ref (read_var scalar) in
                for i = Int.(n - 2) downto 0 do
                  let bit = bits.(Int.(i + 1)) in
                  let (xtmp, ytmp), _ =
                    Basic.add (!xpl, !ypl) (xtl, ytl * (bit + bit - one))
                  in
                  let (xsl, ysl), _ = Basic.add (!xpl, !ypl) (xtmp, ytmp) in
                  let n2l = (!n1l - bit) * half in
                  let round =
                    Scale_pack_round_5_wires.
                      { xt= xtl
                      ; b= bit
                      ; yt= ytl
                      ; xp= !xpl
                      ; l1= (!ypl - (ytl * (bit + bit - one))) / (!xpl - xtl)
                      ; yp= !ypl
                      ; xs= xsl
                      ; ys= ysl
                      ; n1= !n1l
                      ; n2= n2l }
                  in
                  state := !state @ [round] ;
                  xpl := xsl ;
                  ypl := ysl ;
                  n1l := n2l
                done ;
                Array.of_list !state)
      in
      let state =
        Array.mapi state ~f:(fun i s ->
            let open Int in
            if i > 0 then
              { s with
                n1= state.(i - 1).n2
              ; xp= state.(i - 1).xs
              ; yp= state.(i - 1).ys }
            else {s with xp; yp} )
      in
      Intf.assert_
        [{basic= Plonk_constraint.T (EC_scale_pack {state}); annotation= None}] ;
      let finish = state.(Int.(n - 2)) in
      (finish.xs, finish.ys, finish.b)
    in
    let xtp, ytp = add (xp, yp) (xt, negate yt) in
    let b = Intf.Boolean.of_field bit in
    (if_ b ~then_:xp ~else_:xtp, if_ b ~then_:yp ~else_:ytp)

  let endoscale ((xt, yt) : t * t) (scalar : t array) : t * t =
    (*
      Acc := [2](endo(P) + P)
      for i from n/2-1 down to 0:
        let S[i] =
          (
            [2r[2i] - 1]P; if r[2i+1] = 0
            endo[2r[2i] - 1]P; otherwise
          )
        Acc := (Acc + S[i]) + Acc
      return Acc
    *)
    let n = Array.length scalar in
    let n = Int.(if n % 2 = 0 then n / 2 else (n + 1) / 2) in
    let endo = Pasta_pallas.endo_base () in
    let xp, yp = double (add (Field.scale xt endo, yt) (xt, yt)) in
    let state =
      exists
        (Snarky.Typ.array ~length:n (Endoscale_round_5_wires.typ typ))
        ~compute:
          As_prover.(
            fun () ->
              let state = ref [] in
              let xpl, ypl = (ref (read_var xp), ref (read_var yp)) in
              let xtl, ytl = (read_var xt, read_var yt) in
              for i = Int.(n - 1) downto 0 do
                let b1l = read_var scalar.(Int.(2 * i)) in
                let b2l =
                  Int.(
                    if (2 * i) + 1 < Array.length scalar then
                      read_var scalar.((2 * i) + 1)
                    else Field.Constant.zero)
                in
                let xql = (one + ((endo - one) * b2l)) * xtl in
                let (xtmp, ytmp), _ =
                  Basic.add (!xpl, !ypl) (xql, ytl * (b1l + b1l - one))
                in
                let (xsl, ysl), _ = Basic.add (!xpl, !ypl) (xtmp, ytmp) in
                let round =
                  Endoscale_round_5_wires.
                    { b2= b2l
                    ; xt= xtl
                    ; b1= b1l
                    ; xq= xql
                    ; yt= ytl
                    ; xp= !xpl
                    ; l1= (!ypl - ((b1l + b1l - one) * ytl)) / (!xpl - xql)
                    ; l2= (!ypl + ysl) / (!xpl - xsl)
                    ; yp= !ypl
                    ; xs= xsl
                    ; ys= ysl }
                in
                state := !state @ [round] ;
                xpl := xsl ;
                ypl := ysl
              done ;
              Array.of_list !state)
    in
    let state =
      Array.mapi state ~f:(fun i s ->
          let xp, yp =
            if i > 0 then
              let s = state.(Int.(i - 1)) in
              (s.xs, s.ys)
            else (xp, yp)
          in
          { s with
            xt
          ; yt
          ; b1= scalar.(Int.(2 * (n - i - 1)))
          ; b2=
              Int.(
                if (2 * (n - i - 1)) + 1 < Array.length scalar then
                  scalar.((2 * (n - i - 1)) + 1)
                else Field.zero)
          ; xp
          ; yp } )
    in
    Intf.assert_
      [{basic= Plonk_constraint.T (EC_endoscale {state}); annotation= None}] ;
    let finish = state.(Int.(n - 1)) in
    (finish.xs, finish.ys)
end
