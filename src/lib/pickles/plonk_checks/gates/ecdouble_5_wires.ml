let range = (10, 13)

module Aux_data = struct
  type _ t = unit
end

type 'a checks = 'a * 'a * 'a

type 'a check_evals = 'a

let checks (type t) (module F : Intf.Field_intf with type t = t) ~e0 ~e1:_
    (() : t Aux_data.t) =
  let open F in
  let get = Five_wires_evals.get in
  let x1 = get L e0 in
  let y1 = get R e0 in
  let x2 = get O e0 in
  let y2 = get Q e0 in
  let y1_inv = get P e0 in
  let pow x i =
    (* Keep the code tidy, but do powers efficiently. *)
    if i = 2 then x * x
    else if i = 4 then
      let x2 = x * x in
      x2 * x2
    else invalid_arg "pow"
  in
  ( (of_int 4 * pow y1 2 * (x2 + (of_int 2 * x1))) - (of_int 9 * pow x1 4)
  , (of_int 2 * y1 * (y2 + y1)) - ((x1 - x2) * of_int 3 * pow x1 2)
  , (y1 * y1_inv) - one )

let check_evals (type t) (module F : Intf.Field_intf with type t = t) alphas
    ~e0 ~e1 aux =
  let open F in
  let check0, check1, check2 = checks (module F) ~e0 ~e1 aux in
  (check0 * alphas range 0)
  + (check1 * alphas range 1)
  + (check2 * alphas range 2)

let fold_check_evals ~init x ~f = f init x
