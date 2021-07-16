let range = (7, 10)

module Aux_data = struct
  type _ t = unit
end

type 'a checks = 'a * 'a * 'a

type 'a check_evals = 'a

let checks (type t) (module F : Intf.Field_intf with type t = t) ~e0 ~e1
    (() : t Aux_data.t) =
  let open F in
  let get = Five_wires_evals.get in
  let x1 = get L e0 in
  let y1 = get R e0 in
  let x2 = get O e0 in
  let y2 = get Q e0 in
  let x3 = get L e1 in
  let y3 = get R e1 in
  let r = get P e1 in
  (* Cache some additions *)
  let y31 = y3 + y1 in
  let x13 = x1 - x3 in
  let x21 = x2 - x1 in
  ( (x21 * y31) - ((y2 - y1) * x13)
  , ((x1 + x2 + x3) * (x13 * x13)) - (y31 * y31)
  , (x21 * r) - one )

let check_evals (type t) (module F : Intf.Field_intf with type t = t) alphas
    ~e0 ~e1 aux =
  let open F in
  let check0, check1, check2 = checks (module F) ~e0 ~e1 aux in
  (check0 * alphas range 0)
  + (check1 * alphas range 1)
  + (check2 * alphas range 2)

let fold_check_evals ~init x ~f = f init x
