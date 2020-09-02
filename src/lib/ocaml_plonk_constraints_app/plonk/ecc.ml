open Core
open Snarky
open Zexe_backend_common.R1cs_constraint_system
open Snarky_bn382.Tweedle

module Constraints (Intf : Snark_intf.Run with type prover_state = unit and type field = Dee.Field.t ) = struct
  open Intf
  open Field

  module Basic = struct
    open Constant

    let add ((x1, y1) : field * field) ((x2, y2) : field * field) : field * field  =
      let s = (y2 - y1) / (x2 - x1) in
      let x3 = square s - x1 - x2 in
      let y3 = (x1 - x3) * s - y1 in
      (x3, y3)

    let double ((x, y) : field * field) : field * field  =
      let s = square x * of_int 3 / of_int 2 / y in
      let x1 = square s - x * of_int 2 in
      let y1 = s * (x - x1) - y in
      (x1, y1)
  end

  let add (p1 : t * t) (p2 : t * t) : t * t  =
    let p3 = exists Typ.(typ * typ) ~compute:As_prover.(fun () ->
        (Basic.add (read_var (fst p1), read_var (snd p1)) (read_var (fst p2), read_var (snd p2))))
    in
    Intf.assert_
      [{
        basic= Plonk_constraint.T (EC_add { p1; p2; p3 }) ;
        annotation= None
      }];
    p3

  let double ((x, y) : t * t) : t * t  =
    let s = square x * of_int 3 / of_int 2 / y in
    let x1 = square s - x * of_int 2 in
    let y1 = s * (x - x1) - y in
    (x1, y1)

  let scale ((x, y) : t * t) (scalar : t array) : t * t =
    (*
      Acc := [2] T + T
      for i from n-2 down to 0
          Q := ki+1 ? T : −T
          Acc := (Acc + Q) + Acc
      return (k0 = 0) ? (Acc - T) : Acc
    *)

    let n = Array.length scalar in
    let (xp, yp) = add (double (x, y)) (x, y) in

    let state = exists (Snarky.Typ.array ~length:n (Scale_round.typ typ)) ~compute:As_prover.(fun () ->
        (
          let state = ref [] in
          let xpl, ypl = read_var xp, read_var yp in
          let xtl, ytl = read_var x, read_var y in

          for i = Int.(n - 2) to 0 do
            let bit = read_var scalar.(Int.(i + 1)) in
            let yq = if bit = one then ytl else negate ytl in
            let xsl, ysl = Basic.add (xpl, ypl) (Basic.add (xpl, ypl) (xtl, yq)) in
            let round = Scale_round.{ xt=xtl; b=bit; yt=ytl; xp=xpl; l1=(ypl+yq)/(xpl-xtl); yp=ypl; xs=xsl; ys=ysl } in
            state := !state @ [round];
          done;
          Array.of_list !state
        ))
    in
    Intf.assert_
      [{
        basic= Plonk_constraint.T (EC_scale { state }) ;
        annotation= None
      }];
    let finish = state.(Int.(n - 1)) in

    let if_pair b (tx, ty) (ex, ey) =
      if_ b ~then_:tx ~else_:ty, if_ b ~then_:ty ~else_:tx
    in
    if_pair
      (Intf.Boolean.of_field scalar.(0))
      (add (finish.xs, finish.ys) (x, negate y))
      (finish.xs, finish.ys)

  let endoscale ((x, y) : t * t) (scalar : t array) : t * t =
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

    let endo = Endo.Dee.scalar () in
    let n = Int.(2 * (Array.length scalar) - 1) in
    let (xp, yp) = double (add (Field.scale x endo, y) (x, y)) in

    let state = exists (Snarky.Typ.array ~length:n (Endoscale_round.typ typ)) ~compute:As_prover.(fun () ->
        (
          let state = ref [] in
          let xpl, ypl = read_var xp, read_var yp in
          let xtl, ytl = read_var x, read_var y in

          for i = Int.(n/2 - 1) to 0 do
            let b2il = read_var scalar.(Int.(2 * i)) in
            let b2i1l = read_var scalar.(Int.(2 * i + 1)) in
            let xql = if b2i1l = one then xtl * endo else xtl in
            let yql = if b2il = one then ytl else negate ytl in
            let xsl, ysl = Basic.add (xpl, ypl) (Basic.add (xpl, ypl) (xtl, ytl)) in
            let round = Endoscale_round.{ b2i1= b2i1l; xt=xtl; b2i= b2il; xq= xql; yt=ytl; xp=xpl; l1=(ypl-yql)/(xpl-xql); yp=ypl; xs=xsl; ys=ysl } in
            state := !state @ [round];
          done;
          Array.of_list !state
        ))
    in
    Intf.assert_
      [{
        basic= Plonk_constraint.T (EC_endoscale { state }) ;
        annotation= None
      }];
    let finish = state.(Int.(n - 1)) in
    (finish.xs, finish.ys)

end
